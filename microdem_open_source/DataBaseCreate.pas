unit DataBaseCreate;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordCreateDBProblems}
   //{$Define RecordAddFieldsToDB}
   //{$Define RecordMyDataCreation}
   //{$Define RecordTimeDBConversions}
   //{$Define RecordRecordProgress}
   //{$Define RecordDBFtoClientDataSet}
   //{$Define FullRecordDBFtoClientDataSet}   //major slowdown
   //{$Define RecordCreateDBProblemsFull}
   //{$Define RecordAddRecNo}
{$EndIf}

interface

uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,

   {$IfDef UseFireDacSQLlite}
      FireDAC.Stan.ExprFuncs,
      FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
      FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
      FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
      FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Comp.Client, FireDAC.Comp.DataSet,
      FireDAC.Phys.SQLite, FireDAC.Comp.UI,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end


   {$IfDef VCL}
      Graphics, Controls,  Dialogs,StdCtrls, Mask, DBCtrls, ExtCtrls, Grids, DBGrids, ComCtrls,Buttons,Forms,
   {$EndIf}

   {$IfDef FMX}
      FMX.Graphics,
   {$EndIf}

   SysUtils, Classes,
   Petmar_types,PETMAR;


(*
**************************************************************************
The data file header structure for dBASE III PLUS table file.
**************************************************************************

The table file header:
======================

Byte  Contents    Description
----- --------    --------------------------------------------------------
0     1 byte      Valid dBASE III PLUS table file (03h without a memo
                  (.DBT file; 83h with a memo).

1-3   3 bytes     Date of last update; in YYMMDD format.
4-7   32-bit      Number of records in the table.
      number
8-9   16-bit      Number of bytes in the header.
      number
10-11 16-bit      Number of bytes in the record.
      number
12-14 3 bytes     Reserved bytes.
15-27 13 bytes    Reserved for dBASE III PLUS on a LAN.
28-31 4 bytes     Reserved bytes.
32-n  32 bytes    Field descriptor array (the structure of this array is shown below)
      each
n+1   1 byte      0Dh stored as the field terminator.

n above is the last byte in the field descriptor array. The size of the
array depends on the number of fields in the table file.



Certain DBF files (with markers 0x8B and 0xCB) provides its encoding.
Byte at offset 0x1D (decimal 29) can have the following values:
0   - unknown
1   - 437 -- MS-DOS, U.S.A.
2   - 850 -- MS-DOS, International
3   - 1252 -- Windows, U.S.A & West European
4   - 10000 -- Macintosh, U.S.A.
100 - 852 MS-DOS, East European
101 - 866 MS-DOS, Russian
102 - 865 MS-DOS, Nordic
103 - 861 MS-DOS, Iceland
104 - 895 MS-DOS, Kamenicky (Czech)
105 - 620 MS-DOS, Mazovia (Polish)
106 - 737 MS-DOS, Greek
107 - 857 MS-DOS, Turkey
108 - 863 MS-DOS, Canada
120 - 950 Windows, Traditional Chinese
121 - 949 Windows, Korean (Hangul)
122 - 936 Windows, Simplified Chinese
123 - 932 Windows, Japanese (Shift-jis)
124 - 874 Windows, Thai
150 - 10007 Macintosh, Russian
151 - 10029 Macintosh, East European
152 - 10006 Macintosh, Greek
200 - 1250 Windows, East European
201 - 1251 Windows, Russian
203 - 1253 Windows, Greek
202 - 1254 Windows, Turkish
125 - 1255 Windows, Hebrew (Only supported on Hebrew OS)
126 - 1256 Windows, Arabic (Only supported on Arabic OS)
204 - 1257 Windows, Baltic

Table Field Descriptor Bytes
============================

Byte  Contents    Description
----- --------    --------------------------------------------------------
0-10  11 bytes    Field name in ASCII (zero-filled).
11    1 byte      Field type in ASCII (C, D, L, M, or N).
12-15 4 bytes     Field data address (address is set in memory; not useful on disk).
16    1 byte      Field length in binary.
17    1 byte      Field decimal count in binary.
18-19 2 bytes     Reserved for dBASE III PLUS on a LAN.
20    1 byte      Work area ID.
21-22 2 bytes     Reserved for dBASE III PLUS on a LAN.
23    1 byte      SET FIELDS flag.
24-31 1 byte      Reserved bytes.

Table Records
=============

The records follow the header in the table file. Data records are preceded
by one byte, that is, a space (20h) if the record is not deleted, an
asterisk (2Ah) if the record is deleted. Fields are packed into records
without field separators orrecord terminators. The end of the file is
marked by a single byte, with the end-of-file marker, an OEM code page
character value of 26 (1Ah). You can input OEM code page data as indicated
below.

Allowable Input for dBASE Data Types
====================================

Data Type      Data Input
-------------- -----------------------------------------------------------
C (Character)  All OEM code page characters.
D (Date)       Numbers and a character to separate month, day, and year
               (stored internally as 8 digits in YYYYMMDD format).
N (Numeric)    - . 0 1 2 3 4 5 6 7 8 9
L (Logical)    ? Y y N n T t F f (? when not initialized).
M (Memo)       All OEM code page characters (stored internally as 10
               digits representing a .DBT block number).

Binary, Memo, and OLE Fields And .DBT Files
===========================================

Memo fields store data in .DBT files consisting of blocks numbered
sequentially (0, 1, 2, and so on). The size of these blocks are internally
set to 512 bytes. The first block in the .DBT file, block 0, is the .DBT
file header.

Memo field of each record in the .DBF file contains the number of the
block (in OEM code page values) where the field's data actually begins. If
a field contains no data, the .DBF file contains blanks (20h) rather than
a number.

When data is changed in a field, the block numbers may also change and the
number in the .DBF may be changed to reflect the new location.

This information is from the Using dBASE III PLUS manual, Appendix C.
*)


const
   MaxFieldsToAdd = 250;

type
   tSingleFieldToAdd = record
      FieldName : ShortString;
      ft : TFieldType;
      Length,Decimals : int32;
   end;
   tFieldsToAdd = array[1..MaxFieldsToAdd] of tSingleFieldToAdd;

type
   tCreateDataBase = class
    private
       {$IfDef UseFDMemTable}
          fdMemTable : tFDMemTable;
       {$EndIf}

       {$IfDef UseTCLientDataSet}
          ClientDataSet : TClientDataSet;
       {$EndIf}

       FieldsToAdd : tFieldsToAdd;
       RecordsInDB,
       FieldsInDB : integer;
       OutputFormat : ExtStr;
       inf : file;

       DBaseIIITableFileHeader : tDBaseIIITableFileHeader;
       TableFieldDescriptor    : tTableFieldDescriptor;

       DataRec : array[1..64000] of byte;
       procedure CloseDBFFile;
    protected
    public
       FileName : PathStr;
       UsingNewDBF : boolean;
       NewDBF : tMyData;
       AddRecID : boolean;
       constructor Create(fName : PathStr);
       destructor Destroy;
       procedure ReadDBFHeader(FileName : PathStr);
       procedure WriteCorrectHeader(KeepOpen : boolean = false);
       procedure AddCorrectRecordFromStringList(TheData : tStringList);
       procedure AddAField(TheName : ShortString; ft : TFieldType; Length : integer; Decimals : integer = 0);
       procedure RemoveAField(TheName: ShortString);
       procedure ChangeFieldLength(theField : shortstring; newLength : integer);
       procedure CopyDataFromOldDBFToNew(OldName : PathStr; ShowProgress : boolean);
       procedure AddBoundingBoxToTable;
       procedure AddXYZToTable;
       procedure AddLatLongToTable(HighAcc : boolean = false);
       procedure AddLat2Long2ToTable;
       procedure AddUTMtoTable;
       procedure AddRecNoToTable;
       procedure AddFontDefToTable(Font2 : boolean = false);
       procedure AddPointSymbolDefToTable;
       procedure AddLineDefToTable;
       procedure AddRedGreenBlueFieldsToTable;
       procedure AddSatBandInfo(NumBands : integer);
   end;


function ConvertDBFtoSQLite(dbfName : PathStr) : boolean;

{$IfDef UseTCLientDataSet}
   procedure ConvertDBFtoCDSFile(fName : PathStr);
   procedure LoadDBFtoClientDataSet(fName : PathStr; var CDS : tClientDataSet);
{$EndIf}

function ftFromString(fts : ShortString) : TFieldType;
function FieldTypeStr(ft : tFieldType) : ShortString;
function MakeFieldNameFromString(Input : shortString) : string10;

procedure ScreenDBFFileDump(FileName : PathStr);


implementation


uses
   DEMDefs,DEMdef_routines;



procedure ScreenDBFFileDump(FileName : PathStr);
var
   i,Offset,FieldsInDB, RecordsInDB : integer;
   inf : file;
   DBaseIIITableFileHeader : tDBaseIIITableFileHeader;
   TableFieldDescriptor : tTableFieldDescriptor;
   Findings : tStringList;
begin
   Findings := tStringList.Create;
   assignFile(inf,FileName);
   reset(inf,1);
   BlockRead(inf, DBaseIIITableFileHeader, SizeOf(tDBaseIIITableFileHeader));
   Findings.Add('Last update: ' + IntToStr(DBaseIIITableFileHeader.LastUpdate[1])+ '/' + IntToStr(DBaseIIITableFileHeader.LastUpdate[2])+ '/' +IntToStr(DBaseIIITableFileHeader.LastUpdate[3]));
   FieldsInDB := (DBaseIIITableFileHeader.BytesInHeader-33) div 32;
   RecordsInDB := DBaseIIITableFileHeader.RecordsInTable;
   Findings.Add('Fields in DB=' + IntToStr(FieldsInDB));
   Findings.Add('Records in DB=' + IntToStr(RecordsInDB));
   Offset := SizeOf(tDBaseIIITableFileHeader);
   seek(inf,Offset);
   for i := 1 to FieldsInDB do begin
      BlockRead(inf, TableFieldDescriptor, SizeOf(tTableFieldDescriptor));
      Findings.Add(IntToStr(i) + '  ' + TableFieldDescriptor.FieldName + '  ' + TableFieldDescriptor.FieldType  + '  ' + IntToStr(TableFieldDescriptor.FieldLength) +
          '   ' + IntToStr(TableFieldDescriptor.FieldDecimalCount) + '   ' + IntToStr(TableFieldDescriptor.FieldDataAddress));
   end;
   CloseFile(inf);
   {$IfDef VCL} Petmar.DisplayAndPurgeStringList(Findings,FileName); {$EndIf}
end;



function ftFromString(fts : ShortString) : TFieldType;
begin
   fts := UpperCase(fts);
   if (fts = 'FLOAT') then Result := ftFloat
   else if (fts = 'INTEGER') then Result := ftInteger
   else  Result := ftString;
end;


function FieldTypeStr(ft : tFieldType) : ShortString;
begin
   case ft of
      ftFloat : Result := 'Float';
      ftInteger : Result := 'Integer';
      ftString : Result := 'String';
      ftSmallInt : Result := 'SmallInteger';
      ftLargeInt : Result := 'LargeInteger';
      ftDate : Result := 'date';
      ftBlob : Result := 'blob';
      ftMemo : Result := 'memo';
      else begin
          Result := 'Other';
      end;
   end;
end;


function MakeFieldNameFromString(Input : shortString) : string10;
var
   j : integer;
begin
   Result := UpperCase(Copy(ptTrim(Input),1,10));
   for j := 1 to length(Result) do
      if  not (Result[j] in DBaseFieldNameChars) then Result[j] := '_';
end;

{$IfDef UseTCLientDataSet}
procedure ConvertDBFtoCDSFile(fName : PathStr);
var
   CDS : tClientDataSet;
   NewName : PathStr;
begin
   {$IfDef RecordTimeDBConversions} WriteLineToDebugFile('ConvertDBFtoCDSFile in ' + fName + '  File size= ' + SmartMemorySize(GetFileSize(fname))); {$EndIf}
   NewName := ChangeFileExt(fName,'.cds');
   LoadDBFtoClientDataSet(fName,CDS);
   CDS.SaveToFile(NewName,dfBinary);
   {$IfDef RecordTimeDBConversions} WriteLineToDebugFile('ConvertDBFtoCDSFile saved to ' + NewName); {$EndIf}
   CDS.Destroy;
   {$IfDef RecordTimeDBConversions} WriteLineToDebugFile('ConvertDBFtoCDSFile out'); {$EndIf}
end;


procedure LoadDBFtoClientDataSet(fName : PathStr; var CDS : tClientDataSet);  //uses no external code for dBase file
var
   CreateDataBase : tCreateDataBase;
   i,j,fLen : integer;
   ShowProgress : boolean;
   ftAdd : tFieldType;
   Table : tMyData;
begin
   {$IfDef RecordDBFtoClientDataSet} WriteLineToDebugFile('LoadDBFtoClientDataSet in: ' + fName);   {$EndIf}
   CreateDataBase := tCreateDataBase.Create(fName);
   CreateDataBase.OutputFormat := '.CDS';
   with CreateDataBase do begin
      CreateDataBase.ReadDBFHeader(fName);
      CDS := TClientDataSet.Create(Nil);
      for i := 1 to FieldsInDB do begin
          {$IfDef RecordDBFtoClientDataSet} WriteLineToDebugFile(IntToStr(i) + '  ' + FieldsToAdd[i].FieldName + '  ' + FieldTypeStr(FieldsToAdd[i].ft) + '  length=' + IntToStr(FieldsToAdd[i].length)); {$EndIf}
           ftAdd := FieldsToAdd[i].ft;
           if (FieldsToAdd[i].ft in [ftDate, ftUnknown]) then ftAdd := ftString;
           if (FieldsToAdd[i].ft = ftInteger) and (FieldsToAdd[i].length > 6) then ftAdd := ftFloat;
           if (ftAdd = ftString) then fLen := FieldsToAdd[i].length else fLen := 0;
          {$IfDef RecordDBFtoClientDataSet} WriteLineToDebugFile(IntToStr(i) + '  ' + FieldsToAdd[i].FieldName + '  ' + FieldTypeStr(FieldsToAdd[i].ft) + '  length=' + IntToStr(fLen)); {$EndIf}
           CDS.FieldDefs.Add(FieldsToAdd[i].FieldName,ftAdd, fLen, False);
       end;

       CDS.CreateDataset;
       CDS.Open;

       Table := tMyData.Create(fName);
       I := 0;
       ShowProgress := (Table.TotRecsInDB > MDDef.RecNumToShowDBProgress);
       {$IfDef VCL} if ShowProgress then StartProgress(ExtractFilenameNoExt(fName) + ' to CDS'); {$EndIf}
       while not Table.eof do begin
          {$IfDef VCL} if ShowProgress and (i mod 5000 = 0) then UpdateProgressBar(i/Table.TotRecsInDB); {$EndIf}
          inc(i);
          CDS.Append;
          for j := 1 to FieldsInDB do begin
             CDS.FieldByName(FieldsToAdd[j].FieldName).AsString := Table.GetFieldByNameAsString(FieldsToAdd[j].FieldName);
          end;
          CDS.Post;
          Table.Next;
       end;
       Table.Destroy;

      {$IfDef VCL} if ShowProgress then EndProgress;  {$EndIf}
      CDS.First;
      {$IfDef RecordDBFtoClientDataSet} WriteLineToDebugFile('Complete copy, fields=' + IntToStr(FieldsInDB) + '  recs=' + IntToStr(i)); {$EndIf}
   end;
   CreateDataBase.Destroy;
end;
{$EndIf}


function ConvertDBFtoSQLite(dbfName : PathStr) : boolean;
var
   CreateDataBase : tCreateDataBase;
   i,j,k,l,StartPos,RecsRead : integer;
   AFieldValue : ANSIString;
   FullRecord : tStringList;
   NewName : PathStr;
begin
   {$IfDef RecordTimeDBConversions} WriteLineToDebugFile('ConvertDBFtoSQLite in ' + dbfName + ''); {$EndIf}
   if not FileExists(dbfName) then begin
      Result := false;
      exit;
   end;
   Result := true;
   NewName := ChangeFileExt(dbfName,'.db');
   CreateDataBase := tCreateDataBase.Create(NewName);
   CreateDataBase.ReadDBFHeader(DbfName);
   CreateDataBase.AddRecNoToTable;
   CreateDataBase.WriteCorrectHeader(true);

   {$IfDef RecordTimeDBConversions} WriteLineToDebugFile('ConvertDBFtoSQLite header written'); {$EndIf}

    {$IfDef UseFireDacSQLlite}
       CreateDataBase.NewDBF := tMyData.Create(NewName);
    {$EndIf}

    assignFile(CreateDataBase.inf,dbfName);
    reset(CreateDataBase.inf,1);
    seek(CreateDataBase.inf,CreateDataBase.DBaseIIITableFileHeader.BytesInHeader);
    if (CreateDataBase.RecordsInDB > 5000) then StartProgress('Create SQLite');
    i := 1;
    while i <= CreateDataBase.RecordsInDB do begin
       if (i mod 1000 = 0) and (CreateDataBase.RecordsInDB > 5000) then UpdateProgressBar(i/CreateDataBase.RecordsInDB);
       BlockRead(CreateDataBase.inf,CreateDataBase.DataRec,CreateDataBase.DBaseIIITableFileHeader.BytesInRecord,RecsRead);

       RecsRead := RecsRead div CreateDataBase.DBaseIIITableFileHeader.BytesInRecord;
       for l := 1 to RecsRead do begin
          StartPos := 2 + pred(l) * CreateDataBase.DBaseIIITableFileHeader.BytesInRecord;
          FullRecord := tStringList.Create;
          for j := 1 to pred(CreateDataBase.FieldsInDB) do begin
             AFieldValue := '';
             for k := 1 to CreateDataBase.FieldsToAdd[j].Length do AFieldValue := AFieldValue + chr(CreateDataBase.DataRec[StartPos + pred(k)]);
             AFieldValue := ptTrim(AFieldValue);
             FullRecord.Add(AFieldValue);
             StartPos := StartPos + CreateDataBase.FieldsToAdd[j].Length;
          end;
          {$IfDef RecordTimeDBConversions} WriteLineToDebugFile('Insert rec=' + intToStr(i)); {$EndIf}
          FullRecord.Add(IntToStr(i));
          CreateDataBase.AddCorrectRecordFromStringList(FullRecord);
          FullRecord.Free;
          inc(i);
       end;
    end;
    if (CreateDataBase.RecordsInDB > 5000) then EndProgress;
    CreateDataBase.Destroy;
    ShowDefaultCursor;
   {$IfDef RecordTimeDBConversions} WriteLineToDebugFile('ConvertDBFtoSQLite out'); {$EndIf}
end;


{ tCreateDataBase }



procedure tCreateDataBase.RemoveAField(TheName: ShortString);
var
   i,j : integer;
begin
   if (TheName <> '') then begin
      for I := 1 to FieldsInDB do begin
         if (FieldsToAdd[i].FieldName = TheName) then begin
            dec(FieldsInDB);
            for j := i to FieldsInDB do begin
               FieldsToAdd[j] := FieldsToAdd[succ(j)];
            end;
         end;
      end;
   end;
end;


procedure tCreateDataBase.ChangeFieldLength(theField : shortstring; newLength : integer);
var
   i : integer;
begin
   for I := 1 to FieldsInDB do begin
      if (UpperCase(FieldsToAdd[i].FieldName) = TheField) then begin
         FieldsToAdd[i].Length := NewLength;
      end;
   end;
end;

procedure tCreateDataBase.AddAField(TheName: ShortString; ft : TFieldType; Length,Decimals: integer);
var
   i : integer;
begin
   for I := 1 to FieldsInDB do if (FieldsToAdd[i].FieldName = TheName) then exit;
   inc(FieldsInDB);
   FieldsToAdd[FieldsInDB].FieldName := UpperCase(TheName);
   FieldsToAdd[FieldsInDB].ft := ft;
   FieldsToAdd[FieldsInDB].Length := length;
   FieldsToAdd[FieldsInDB].Decimals := decimals;
   {$IfDef RecordCreateDBProblemsFull} WriteLineToDebugFile('tCreateDataBase.AddAField  ' + IntToStr(FieldsInDB) + '  ' + TheName + '  ' + FieldTypeStr(ft) + '  ' + IntToStr(Length)); {$EndIf}
end;


constructor tCreateDataBase.Create(fName: PathStr);
begin
   {$IfDef RecordCreateDBProblems} WriteLineToDebugFile('tCreateDataBase.Create, new file=' + fName); {$EndIf}
   FileName := fname;
   FieldsInDB := 0;

   {$IfDef UseTCLientDataSet}
      ClientDataSet := Nil;
   {$EndIf}

   {$IfDef UseFDMemTable}
      fdMemTable := Nil;
   {$EndIf}

   UsingNewDBF := true;  //false;
   OutPutFormat := UpperCase(ExtractFileExt(fName));
   {$IfDef RecordCreateDBProblems} WriteLineToDebugFile('Output format=' + OutPutFormat); {$EndIf}
end;


destructor tCreateDataBase.Destroy;
begin
   {$IfDef RecordCreateDBProblems} WriteLineToDebugFile('tCreateDataBase.Destroy in'); {$EndIf}
   if UsingNewDBF {or (NewDBF <> Nil)} then begin
      {$IfDef RecordCreateDBProblems} WriteLineToDebugFile('call NewDBF.Destroy'); {$EndIf}
      if (NewDBF <> Nil) then NewDBF.Destroy;
   end
   else CloseDBFFile;

   {$IfDef UseTCLientDataSet}
      if (ClientDataSet <> Nil) and ExtEquals(OutPutFormat,'.cds') then begin
         ClientDataSet.SaveToFile(FileName,dfBinary);
         ClientDataset.Free;
      end;
   {$EndIf}
   {$IfDef RecordCreateDBProblems} WriteLineToDebugFile('tCreateDataBase.Destroy out'); {$EndIf}
end;


procedure tCreateDataBase.CloseDBFFile;
var
   Val : byte;
begin
   {$IfDef RecordCreateDBProblems} WriteLineToDebugFile('tCreateDataBase.CloseDBFFile in'); {$EndIf}
   if ExtEquals(OutPutFormat,'.dbf') {and (not UsingNewDBF)} then  begin
      Val := 26;
      BlockWrite(inf,val,1);
      if (RecordsInDB > 0) then  begin
        seek(inf,0);
        DBaseIIITableFileHeader.RecordsInTable := RecordsInDB;
        BlockWrite(inf, DBaseIIITableFileHeader, SizeOf(tDBaseIIITableFileHeader));
      end;
      CloseFile(Inf);
   end;
   {$IfDef RecordCreateDBProblems} WriteLineToDebugFile('tCreateDataBase.CloseDBFFile out'); {$EndIf}
end;


procedure tCreateDataBase.ReadDBFHeader(FileName : PathStr);
var
   i,Offset : integer;
   ft : TFieldType;
   FieldsInOldDB : integer;
begin
   {$IfDef RecordCreateDBProblems} WriteLineToDebugFile('tCreateDataBase.ReadDBFHeader in: ' + FileName);{$EndIf}
   assignFile(inf,FileName);
   reset(inf,1);
   BlockRead(inf, DBaseIIITableFileHeader, SizeOf(tDBaseIIITableFileHeader));

   FieldsInOldDB := (DBaseIIITableFileHeader.BytesInHeader-33) div 32;
   {$IfDef RecordCreateDBProblems} WriteLineToDebugFile('Fields in DB=' + IntToStr(FieldsInOldDB) + '  Records in DB=' + IntToStr(DBaseIIITableFileHeader.RecordsInTable)); {$EndIf}
   Offset := SizeOf(tDBaseIIITableFileHeader);
   seek(inf,Offset);
   for i := 1 to FieldsInOldDB do begin
      BlockRead(inf, TableFieldDescriptor, SizeOf(tTableFieldDescriptor));
      case TableFieldDescriptor.FieldType of
         'C' : ft := ftString;
         'D' : ft := ftDate;
         'N' : if (TableFieldDescriptor.FieldDecimalCount = 0) then ft := ftInteger
               else ft := ftFloat;
      end;
      {$IfDef RecordCreateDBProblemsFull} WriteLineToDebugFile('tCreateDataBase.ReadDBFHeader  ' + TableFieldDescriptor.FieldName + '  ' + TableFieldDescriptor.FieldType); {$EndIf}
      AddAField(TableFieldDescriptor.FieldName,ft,TableFieldDescriptor.FieldLength,TableFieldDescriptor.FieldDecimalCount);
   end;
   CloseFile(inf);
end;



procedure tCreateDataBase.WriteCorrectHeader;


      {$IfDef UseTCLientDataSet}
      procedure WriteCDSHeaderInMemory;
      var
         i,fLen : integer;
      begin
         ClientDataSet := TClientDataSet.Create(Nil);
         for i := 1 to FieldsInDB  do begin
             if (FieldsToAdd[i].ft = ftString) then fLen := FieldsToAdd[i].Length
             else fLen := 0;
             ClientDataSet.FieldDefs.Add(FieldsToAdd[i].FieldName,FieldsToAdd[i].ft, fLen, False);
         end;
         ClientDataSet.CreateDataset;
         ClientDataSet.Open;
      end;
      {$EndIf}

      procedure WriteDBASEHeader;
      var
         i,j : integer;
         b : byte;
         TimeNow : TDateTime;
         Year,Month,Day : Word;
      begin
         {$IfDef RecordCreateDBProblemsFull} WriteLineToDebugFile('tCreateDataBase.WriteDBASEHeader in'); {$EndIf}
         assignFile(inf,FileName);
         rewrite(inf,1);

         FillChar(DBaseIIITableFileHeader,SizeOf(tDBaseIIITableFileHeader),0);

         for I := 1 to FieldsInDB do begin
            if (FieldsToAdd[i].ft in [ftSmallInt,ftInteger,ftLargeInt]) and (FieldsToAdd[i].Length < MDdef.DbMinIntFieldSize) then begin
               FieldsToAdd[i].Length := MDdef.DbMinIntFieldSize;
            end;
         end;

         with DBaseIIITableFileHeader do begin
            ValidDesig := 3;
            TimeNow := System.SysUtils.GetTime;
            DecodeDate(TimeNow,Year,Month,Day);
            if (Year < 2000) then Year := 2000;

            LastUpdate[1] := Year mod 2000;
            LastUpdate[2] := Month;
            LastUpdate[3] := Day;
            RecordsInTable := RecordsInDB;
            BytesInHeader := FieldsInDB * 32 + 33;
            BytesInRecord := 1;   //status byte at start of the record
            for I := 1 to FieldsInDB do BytesInRecord := BytesInRecord + FieldsToAdd[i].Length;
         end;
         {$IfDef RecordCreateDBProblemsFull} WriteLineToDebugFile('BytesInRecord= ' + IntToStr(DBaseIIITableFileHeader.BytesInRecord)); {$EndIf}

         BlockWrite(inf, DBaseIIITableFileHeader, SizeOf(tDBaseIIITableFileHeader));
         for I := 1 to FieldsInDB do begin
            {$IfDef RecordCreateDBProblemsFull} WriteLineToDebugFile('tCreateDataBase.WriteDBASEHeader ' + FieldTypeStr(FieldsToAdd[i].ft) + '=' + FieldsToAdd[i].FieldName + ' ' + IntToStr(FieldsToAdd[i].Length)); {$EndIf}
            FillChar(TableFieldDescriptor,SizeOf(tTableFieldDescriptor),0);

            for j := 0 to 10 do TableFieldDescriptor.FieldName[j] := #0;
            for j := 1 to length(FieldsToAdd[i].FieldName) do begin
               TableFieldDescriptor.FieldName[pred(j)] := FieldsToAdd[i].FieldName[j];
            end;

            TableFieldDescriptor.FieldLength := FieldsToAdd[i].Length;
            TableFieldDescriptor.FieldDecimalCount := FieldsToAdd[i].Decimals;
            if ftIsNumeric(FieldsToAdd[i].ft) then TableFieldDescriptor.FieldType := 'N'
            else if FieldsToAdd[i].ft = ftDate then TableFieldDescriptor.FieldType := 'D'
            else if FieldsToAdd[i].ft = ftString then TableFieldDescriptor.FieldType := 'C'
            else begin
               TableFieldDescriptor.FieldType := 'C';
            end;
            for j := 1 to 2 do TableFieldDescriptor.ReservedForLAN[j] := 0;
            for j := 1 to 2 do TableFieldDescriptor.ReservedForLAN2[j] := 0;
            for j := 24 to 31 do TableFieldDescriptor.Reserved[j] := 0;
            TableFieldDescriptor.WorkAreaID := 0;
            TableFieldDescriptor.SetFieldsFlag := 0;
            BlockWrite(inf, TableFieldDescriptor, SizeOf(tTableFieldDescriptor));
         end;
         b := 13;
         BlockWrite(inf, b, 1);
      end;


      procedure WriteSQLiteHeader;
      var
         aLine : ANSIstring;
         TStr : shortString;
         i : integer;
         {$IfDef UseFireDacSQLlite}
            dbMain : TFDConnection;
            fdTable : tFDTable;
         {$EndIf}
      begin
         {$IfDef RecordMyDataCreation} WriteLineToDebugFile('WriteSQLiteHeader in'); {$EndIf}
         aLine := '';
         for I := 1 to FieldsInDB do begin
            TStr := SQLTypeDefString(FieldsToAdd[i].ft,FieldsToAdd[i].length);
            aline := aline + FieldsToAdd[i].FieldName + ' ' + TStr + ', ';
         end;
         System.Delete(aLine,pred(length(aLine)),2);

         aLine := 'create table ' + ExtractFileNameNoExt(FileName) + '(' + aline + ');';
         {$IfDef UseFireDacSQLlite}
            {$IfDef RecordMyDataCreation} WriteLineToDebugFile('doing FireDac, aline=' + aline); {$EndIf}
            fdTable := nil;
            dbMain := Nil;
            OpenSQLLiteFiles(FileName,dbMain,fdTable,aline);
            if (fdTable <> Nil) then fdTable.Destroy;
            if (DBMain <> Nil) then dbMain.Destroy;
            NewDBF := tMyData.Create(FileName);
         {$EndIf}
      end;

      {$IfDef UseFDMemTable}
      procedure WriteFDMemHeader;
         var
            i : integer;
         begin
            {$IfDef RecordMyDataCreation} WriteLineToDebugFile('WriteFDMemHeader in'); {$EndIf}
            fdMemTable := tFDMemTable.Create(Nil);
            for I := 1 to FieldsInDB do begin
               {$IfDef RecordMyDataCreation} WriteLineToDebugFile(FieldsToAdd[i].FieldName + '  ' + IntToStr(FieldsToAdd[i].length));            {$EndIf}
               if (FieldsToAdd[i].ft in [ftInteger,ftFloat]) then FieldsToAdd[i].length := 0;
               fdMemTable.FieldDefs.Add(FieldsToAdd[i].FieldName,FieldsToAdd[i].ft,FieldsToAdd[i].length,false);
            end;
            {$IfDef RecordMyDataCreation} WriteLineToDebugFile('all fields defined'); {$EndIf}
            FDMemTable.CreateDataSet;
            {$IfDef RecordMyDataCreation} WriteLineToDebugFile('created'); {$EndIf}
            FDMemTable.Open;
            {$IfDef RecordMyDataCreation} WriteLineToDebugFile('opened'); {$EndIf}
            FDMemTable.SaveToFile(FileName,sfBinary);
            {$IfDef RecordMyDataCreation} WriteLineToDebugFile('saved'); {$EndIf}
            FDMemTable.Destroy;
            {$IfDef RecordMyDataCreation} WriteLineToDebugFile('destroyed'); {$EndIf}
         end;
      {$EndIf}

begin
   AddRecNoToTable;
   if ExtEquals(OutPutFormat,'.db') then begin
      WriteSQLiteHeader;
   end;
   if ExtEquals(OutPutFormat,'.dbf') then begin
      WriteDBASEHeader;
   end;
   {$IfDef UseFDMemTable}
      if ExtEquals(OutPutFormat,'.adb') then begin
         WriteFDMemHeader;
      end;
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      if ExtEquals(OutPutFormat,'.cds') then begin
         WriteCDSHeaderInMemory;
      end;
   {$EndIf}

   if (not KeepOpen) then Destroy;
end;


procedure tCreateDataBase.CopyDataFromOldDBFToNew(OldName : PathStr; ShowProgress : boolean);
var
   OldHeader : tDBaseIIITableFileHeader;
   oldF : file;
   i : integer;
begin
   AssignFile(OldF,OldName);
   reset(Oldf,1);
   BlockRead(OldF,OldHeader,SizeOf(OldHeader));
   seek(OldF,OldHeader.BytesInHeader);
   RecordsInDB := 0;
   for i := 1 to OldHeader.RecordsInTable do begin
      FillChar(DataRec,SizeOf(DataRec),32);
      if ShowProgress and (i mod 5000 = 0) then UpdateProgressBar(i/OldHeader.RecordsInTable);
      BlockRead(OldF,DataRec,OldHeader.BytesInRecord);
      BlockWrite(inf,DataRec,DBaseIIITableFileHeader.BytesInRecord);
      inc(RecordsInDB);
   end;
   CloseFile(OldF);
end;


procedure tCreateDataBase.AddBoundingBoxToTable;
begin
   AddAField('LAT_HI',ftFloat,11,7);
   AddAField('LAT_LOW',ftFloat,11,7);
   AddAField('LONG_HI',ftFloat,12,7);
   AddAField('LONG_LOW',ftFloat,12,7);
end;


procedure tCreateDataBase.AddSatBandInfo(NumBands : integer);
var
   i : integer;
begin
   AddAField('SAT_COL',ftInteger,6);
   AddAField('SAT_ROW',ftInteger,6);
   AddAField('COLOR',ftInteger,9);
   AddAField('CLASS',ftString,25);
   for i := 1 to NumBands do begin
      AddAField('BAND_' + IntToStr(I),ftInteger,6);
   end;
end;


procedure tCreateDataBase.AddCorrectRecordFromStringList(TheData: tStringList);


      procedure FixFieldValue(ft : tFieldType; Decimals : integer; var AFieldValue : string);
      var
        fv : float64;
      begin
         if (Uppercase(AfieldValue) = 'NAN') then AFieldValue := ''
         else if (AfieldValue = '9999') then AFieldValue := '';
         if (ft = ftString) then begin
            if (AfieldValue <> '') and (AFieldValue[1] = '"') then Delete(AFieldValue,1,1);
            if (AfieldValue <> '') and (AFieldValue[length(AFieldValue)] = '"') then Delete(AFieldValue,length(AFieldValue),1);
         end;
         if (ft = ftFloat) and (AFieldValue <> '') and (Decimals > 0) then begin
            try
               fv := StrToFloat(AFieldValue);
            except
               on Exception do AFieldValue := '';
            end;
            if (AfieldValue <> '') then AFieldValue := RealToString(fv,-18,Decimals);
         end;
      end;


      procedure AddRecordToDBFFromStringList(TheData: tStringList);
      var
         i,j,StartPos : integer;
         AFieldValue : string;
      begin
         {$If Defined(RecordCreateDBProblemsFull) or Defined(RecordRecordProgress)} WriteLineToDebugFile('tCreateDataBase.AddRecordToDBFFromStringList'); WriteStringListToDebugFile(TheData); {$EndIf}
         if UsingNewDBF then NewDBF.Insert
         else begin
            FillChar(DataRec,SizeOf(DataRec),32);
            DataRec[1] := 13;
            StartPos := 2;
         end;
         for i := 1 to TheData.Count do begin
            AFieldValue := TheData.Strings[pred(i)];
            FixFieldValue(FieldsToAdd[i].ft,FieldsToAdd[i].decimals,AFieldValue);

            if UsingNewDBF then begin
               {$IfDef RecordCreateDBProblemsFull} WriteLineToDebugFile(FieldsToAdd[i].FieldName + ' = ' + FieldTypeStr(FieldsToAdd[i].ft) + '=' + AFieldValue); {$EndIf}
               NewDBF.SetFieldByNameAsString(FieldsToAdd[i].FieldName,trim(AFieldValue));
            end
            else begin
               while (length(AFieldValue) < FieldsToAdd[i].Length) do begin
                  if (FieldsToAdd[i].ft = ftString) then AFieldValue := AFieldValue + ' '
                  else AFieldValue := ' ' + AFieldValue;
               end;
               for j := 1 to FieldsToAdd[i].Length do DataRec[StartPos + pred(j)] := ord(AFieldValue[j]);
               inc(StartPos,FieldsToAdd[i].Length);
            end;
         end;
         if UsingNewDBF then NewDBF.Post
         else BlockWrite(inf,DataRec,DBaseIIITableFileHeader.BytesInRecord);

         inc(RecordsInDB);

         {$IfDef RecordCreateDBProblemsFull}
            AFieldValue := '';
            for i := 1 to DBaseIIITableFileHeader.BytesInRecord do AFieldValue := AFieldValue + chr(DataRec[i]);
            WriteLineToDebugFile(aFieldValue);
         {$EndIf}
      end;

      {$IfDef UseFireDacSQLlite}
      procedure FireDacSQLiteAddRecordFromStringList(TheData : tStringList);
      var
         i : integer;
         TStr : String;
      begin
          {$IfDef RecordCreateDBProblemsFull} WriteStringListToDebugFile(TheData); {$EndIf}

          NewDBF.Insert;
          for i := 0 to pred(TheData.Count) do begin
             TStr := ptTrim(TheData.Strings[i]);
             FixFieldValue(ftstring, -99,TStr);
             {$IfDef RecordCreateDBProblemsFull} WriteLineToDebugFile(NewDB.GetFieldName(i) + '=' + TStr); {$EndIf}
             NewDBF.SetFieldByNameAsString(NewDBF.GetFieldName(i),TStr);
          end;
          NewDBF.Post;
      end;
     {$EndIf}

begin
   {$IfDef UseFireDacSQLlite}
      if ExtEquals(OutPutFormat,'.db') then begin
         FireDacSQLiteAddRecordFromStringList(TheData);
      end;
   {$EndIf}
   if ExtEquals(OutPutFormat,'.dbf') then AddRecordToDBFFromStringList(TheData);
end;


procedure tCreateDataBase.AddRecNoToTable;
begin
   AddAField(RecNoFName,ftInteger,9);
end;


procedure tCreateDataBase.AddXYZToTable;
begin
   AddAField('X',ftFloat,18,6);
   AddAField('Y',ftFloat,18,6);
   AddAField('Z',ftFloat,18,6);
end;


procedure tCreateDataBase.AddLatLongToTable(HighAcc : boolean = false);
begin
   if HighAcc then begin
      AddAField('LAT',ftFloat,12,8);
      AddAField('LONG',ftFloat,13,8);
   end
   else begin
      AddAField('LAT',ftFloat,10,6);
      AddAField('LONG',ftFloat,11,6);
   end;
end;


procedure tCreateDataBase.AddLat2Long2ToTable;
begin
   AddAField('LAT2',ftFloat,12,8);
   AddAField('LONG2',ftFloat,13,8);
end;


procedure tCreateDataBase.AddFontDefToTable(Font2 : boolean = false);
var
   n2 : ShortString;
begin
   if Font2 then n2 := '2' else n2 := '';
   AddAField('FONT' + n2 + '_NAME',ftString,24,0);
   AddAField('FONT' + n2 + '_SIZE',ftInteger,2,0);
   AddAField('FONT' + n2 + '_COLOR',ftInteger,8,0);
   AddAField('FONT' + n2 + '_BOLD',ftString,1,0);
   AddAField('FONT' + n2 + '_ITAL',ftString,1,0);
   AddAField('FONT' + n2 + '_UNDER',ftString,1,0);
   AddAField('ROT'  + n2 + '_ANGLE',ftInteger,4,0);
end;

procedure tCreateDataBase.AddPointSymbolDefToTable;
begin
   AddAField('SYM_TYPE',ftInteger,2);
   AddAField('SYM_SIZE',ftInteger,2);
   AddAField('SYM_COLOR',ftInteger,8);
end;

procedure tCreateDataBase.AddLineDefToTable;
begin
   AddAField('LINE_WIDTH',ftInteger,2,0);
   AddAField('LINE_COLOR',ftInteger,8,0);
end;


procedure tCreateDataBase.AddRedGreenBlueFieldsToTable;
begin
   AddAField('RED',ftInteger,3,0);
   AddAField('GREEN',ftInteger,3,0);
   AddAField('BLUE',ftInteger,3,0);
end;


procedure tCreateDataBase.AddUTMtoTable;
begin
   AddAField('X_UTM',ftFloat,12,2);
   AddAField('Y_UTM',ftFloat,12,2);
end;


initialization
finalization
   {$IfDef RecordCreateDBProblems} WriteLineToDebugFile('RecordCreateDBProblems active in databasecreate'); {$EndIf}
   {$IfDef RecordAddFieldsToDB} WriteLineToDebugFile('RecordAddFieldsToDB active in databasecreate'); {$EndIf}
   {$IfDef RecordCreateDBProblemsFull} WriteLineToDebugFile('RecordCreateDBProblemsFull active in databasecreate--major slowdown'); {$EndIf}
   {$IfDef RecordDBFtoClientDataSet} WriteLineToDebugFile('RecordDBFtoClientDataSet active in databasecreate'); {$EndIf}
   {$IfDef FullRecordDBFtoClientDataSet} WriteLineToDebugFile('FullRecordDBFtoClientDataSet active in databasecreate--major slowdown for big databases'); {$EndIf}
   {$IfDef RecordTimeDBConversions} WriteLineToDebugFile('RecordTimeDBConversions active in databasecreate'); {$EndIf}
   {$IfDef RecordMyDataCreation} WriteLineToDebugFile('RecordMyDataCreation active in databasecreate'); {$EndIf}
end.














