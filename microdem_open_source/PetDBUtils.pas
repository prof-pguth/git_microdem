unit petdbutils;


{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      //{$Define RecordDataBaseFilter}
      //{$Define RecordCSVMerge}
      //{$Define RecordCSV}
      //{$Define RecordDataBaseImage}
      //{$Define RecordOpenDB}
      //{$Define RecordCSVimport}
      //{$Define RecordRange}
      //{$Define RecordGetField}
      //{$Define RecordFieldPresent}
      //{$Define RecordDBF}
      //{$Define RecordHTML}
      //{$Define ListOpenDB}
      //{$Define RecordUnique}
      //{$Define RecordStringFromTable}
      //{$Define RecordGAZ}
      //{$Define RecordShortCSV}
      //{$Define RecordKML}
      {$Define RecordGPX}
      //{$Define RecordCSVParse}
      //{$Define RecordFullGPX}      //major slowdown, but shows the line that is causing an error
      //{$Define RecordProcessLine}  //major slowdown
      //{$Define RecordFullCSV}      //major slowdown
   {$EndIf}

{$EndIf}


interface

uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end DB declarations

   {$IfDef VCL}
      Graphics, Grids, DBGrids,Forms,StdCtrls,
   {$EndIf}

   {$IfDef MSWindows}
      Windows, Messages, ClipBrd,
   {$EndIf}

   DEMMapf,
   SysUtils,StrUtils, Classes,System.UITypes,
   DataBaseCreate,DEMDefs, PETMath,Petmar_types,petmar;

type
   tZeroPadLen = array[0..250] of integer;
   tCSVImport = (csvNormal,csvNGAGaz,csvUSGSgaz);

function PointKMLtoStringList(fName : PathStr) : tStringList;
procedure LineKMLtoStringList(fName : PathStr);
function FitBitTCXtoStringList(fName : PathStr; var ID : shortstring) : tStringList;


function DBtableFieldToDropDown(Table : tMyData; fName,WantName : ShortString) : AnsiString;
function SingleRecordToHTMLTable(Table : tMyData; VisCols : Array100Boolean; ThumbNailDir : PathStr = '') : AnsiString;
function HTMLTableCell(Contents : AnsiString) : AnsiString;
procedure SingleRecordHTMLReport(All : boolean; Table : tMyData; VisCols : Array100Boolean);

procedure DisplayWWWFromDataBase(Table : tMyData; WWWFieldName : ShortString = '');

procedure GetFields(Table : tMyData; VisCols : Array100Boolean; TypesAllowed : tSetFieldType; var FieldsInDB : tStringList; AllFields : boolean = false; SortFields : boolean = false);
procedure GetFieldsLinkPossible(Table,LinkTable : tMyData; VisCols : Array100Boolean; TypesAllowed : tSetFieldType; var FieldsInDB : tStringList; AllFields : boolean = false; SortFields : boolean = false);

procedure CopyDBTable(FromDBF,ToDBF : PathStr);
function GetStringFromFieldInDataBase(Prompt : shortString; Table : tMyData; FieldName : ShortString) : ShortString;

function AddAndIfNeeded(Filter : Ansistring) : AnsiString;
function AddOrIfNeeded(Filter : Ansistring) : AnsiString;

procedure GPXtoDBF(inName : PathStr; var OutName : PathStr);

function PointInBoxGeoFilter(Lat,Long : float64) : AnsiString;
function MakeGeoFilterFromCorners(HiLat,LowLong,LowLat,HighLong : float64) : AnsiString;  //overload;
function MakeGeoFilterFromBoundingBox(bBox : sfBoundBox) : AnsiString;  //overload;
function MakePointGeoFilter(LatFieldName,LongFieldName : string16; HiLat,LowLong,LowLat,HighLong : float64) : AnsiString;
function PointVeryCloseGeoFilter(LatFieldName,LongFieldName : string16; Lat,Long : float64; Bit : float64 = 0.0001) : AnsiString;

procedure FindPointFileGeoLimits(Table : tMyData; var HiLat,LowLong,LowLat,HighLong : float64);

function PointBoundBoxGeo(Lat,Long : float64) : sfBoundBox;

procedure SafeAdjustGeoBoundBoxForPixelIsArea(var bb : sfBoundBox);
function IntersectionTwoGeoBoundBoxes(bb1,bb2 : sfBoundBox) : sfBoundBox;
function UnionTwoGeoBoundBoxes(bb1,bb2 : sfBoundBox) : sfBoundBox;


procedure ZeroTable(fName : PathStr);  overload;
procedure ZeroTable(var TheTable : tMyData); overload;

procedure FindUniqueEntries(Table : tMyData; FieldName : ShortString; var DataThere : tStringList; Sort : boolean = true); overload;
function NumberUniqueEntries(Table : tMyData; FieldName : ShortString) : integer; overload;

procedure FindUniqueEntriesLinkPossible(Table,LinkData : tMyData; LinkFieldThisDB,LinkFieldOtherDB,FieldName : ShortString; var DataThere : tStringList; Sort : boolean = true); overload;

function DefineColorTableValues(Palette : shortstring; Min,Max : float64; var ZColorTable : tColorTableDefinitions; Reverse : boolean = false) : boolean;

procedure GetFieldValuesInArray(Table : tMyData; FieldDesired : ShortString; var zs : bfarray32; var Npts,Missing : int64; var Min,Max : float64);
procedure GetFieldValuesInArrayLinkPossible(Table,LinkData : tMyData; LinkFieldThisDB,LinkFieldOtherDB,FieldDesired : ShortString; var zs : bfarray32; var Npts,Missing : int64; var Min,Max : float64);

function LinkedField(var FieldDesired : ShortString) : boolean;
function NoStatsField(fName : ShortString) : boolean;
procedure DataBaseCorrelations(DB : integer; UseFields : tStringList; var VarCovar,Correlations  : tTrendMatrix; var NumPoints : integer);

function ExpandIconName(Table : tMyData; FieldName : ShortString; var fName : PathStr) : boolean;

procedure InsureMGRSInDataBase(fName : PathStr; var TheData : tMyData);
function ProgressIncrement(FiltRecsInDB : integer) : integer;

function PointInBoundingBox(Lat,Long : float64; BoundBox : sfBoundBox) : boolean; inline;
function CornersFromBoundBoxGeo(bb : sfBoundBox) : shortString;

procedure QuickGraphFromStringList(var sl : tStringList; xf,yf,Capt : shortstring);


{$IfDef NoDBFManipulation}
{$Else}
   procedure CheckDBaseIndexes(fName : PathStr);
   procedure RepairDBaseFieldNames(fName : PathStr);
   procedure RenameDBaseField(fName : PathStr; OldName,NewName : ShortString);
   procedure ChangeDBaseFieldType(fName : PathStr; OldName : ShortString; NewType : ANSIchar);
   procedure ChangeDBaseFieldDecimals(fName : PathStr; OldName : ShortString; NewDecimals : byte);
{$EndIf}

{$IfDef VCL}
   function OrigPickField(Table : tMyData; Mess : ShortString; TypesAllowed : tSetFieldType) : ShortString;
   procedure SendStringGridToDataBase(StringGrid1 : tStringGrid; CreateDataBase : tCreateDataBase; ZeroPadLen : tZeroPadLen);
   function DBtableToHTML(Table : tMyData; Source : tDataSource; VisCols : Array100Boolean) : AnsiString;
   procedure HTMLReport(Table : tMyData; Source : tDataSource; VisCols : Array100Boolean);  overload;
   procedure HTMLReport(Title : shortString; StringGrid : tStringGrid; fName : PathStr = '');  overload;
   function StringGridToHTMLTable(StringGrid : tStringGrid) : AnsiString;
   procedure StringGridToCSVFile(fName : PathStr; StringGrid : tStringGrid; Results : tStringList = Nil);
{$EndIf}


   function CSVFileImportToDB(fName : PathStr  = ''; SpecialGaz : tCSVImport = csvNormal) : PathStr;
   function StringList2CSVtoDB(Results : tstringList; fName : Pathstr = ''; CloseFile : boolean = false; SaveCSVfile : boolean = false; OpenTable : boolean = true) : integer;
   procedure MergeCSVFiles(var Fnames : tstringList; OutName : PathStr);

procedure MergeMultipleCSVorTextFiles(BaseMap : tMapForm = nil);


{$IfDef ExDBImages}
{$Else}
   procedure DisplayImageFromDataBase(DataTable : tMyData; FullDBName : PathStr);
   procedure ShowOneImageFromDataBase(DataTable : tMyData; FullDBName : PathStr; fieldName : ShortString);
{$EndIf}


var
   OutsideCSVImport,
   WeKnowTheHeader,
   ForceAllInStringGrid : boolean;

implementation

uses
{$IfDef VCL}
   {$IfDef ExImages}
   {$Else}
      PetImage_form,
   {$EndIf}

   Toggle_DB_use,
   BaseGraf,
   BaseMap,
   DEMesrishapefile,
   text_report_options,
   Nevadia_Main,
{$EndIf}

   DEMDataBase,
   PETImage;


{$I petdbutils_import.inc}


procedure MergeMultipleCSVorTextFiles(BaseMap : tMapForm = nil);
var
   FileNames : tStringList;
   i,j : integer;
   DefFil : byte;
   fName : PathStr;
   s11,slt : tStringList;
   TStr : shortString;
begin
   FileNames := tStringList.Create;
   FileNames.Add(LastDataBase);
   DefFil := 1;
   if GetMultipleFiles('CSV/TXT files to merge','files|*.txt;*.csv' ,FileNames,DefFil) then begin
      s11 := tStringList.Create;
      fName := FileNames.Strings[0];
      s11.LoadFromFile(fName);
      //fName := ChangeFileExt(FileNames.Strings[0],'.dbf');
      for i := 1 to pred(FileNames.Count) do begin
          wmDEM.SetPanelText(0,TimeToStr(now) + '  ' + IntToStr(succ(i)) + '/' + IntToStr(FileNames.Count) + '  ' + ExtractFileName(FileNames.Strings[i]));
          slt := tStringList.Create;
          slt.LoadFromFile(FileNames.Strings[i]);
          for j := 1 to pred(slt.Count) do begin
             TStr := trim(slt.Strings[j]);
             if (TStr <> '') then s11.Add(TStr);
          end;
          slt.Free;
      end;
      fName := ExtractFilePath(fname) + 'merge_' + ExtractFileNameNoExt(fName) + '.dbf';
      if GetFileNameDefaultExt('Merged CSV files','*.dbf',FName) then begin
         if (BaseMap = Nil) then StringList2CSVtoDB(s11,fName,true)
         else BaseMap.StringListToLoadedDatabase(s11,fName);
      end;
      LastDataBase := fName;
      wmDEM.SetPanelText(0,'');
   end;
   FileNames.Free;
   EndProgress;
end;


function IntersectionTwoGeoBoundBoxes(bb1,bb2 : sfBoundBox) : sfBoundBox;
begin
   Result.XMin := Petmath.MaxFloat(bb1.XMin,bb2.xmin);
   Result.XMax := Petmath.MinFloat(bb1.XMax,bb2.xMaX);
   Result.YMin := Petmath.MaxFloat(bb1.YMin,bb2.Ymin);
   Result.yMax := Petmath.MinFloat(bb1.YMax,bb2.YMaX);
end;

function UnionTwoGeoBoundBoxes(bb1,bb2 : sfBoundBox) : sfBoundBox;
begin
   Result.XMin := Petmath.MinFloat(bb1.XMin,bb2.xmin);
   Result.XMax := Petmath.MaxFloat(bb1.XMax,bb2.xMaX);
   Result.YMin := Petmath.MinFloat(bb1.YMin,bb2.Ymin);
   Result.yMax := Petmath.MaxFloat(bb1.YMax,bb2.YMaX);
end;


procedure MergeCSVFiles(var Fnames : tstringList; OutName : PathStr);
var
   Tstrl,OutPut : tStringList;
   Header1,Header : shortstring;
   fName : PathStr;
   i,j : integer;
begin
   {$IfDef RecordCSVMerge} writelinetoDebugFile(''); {$Endif}
   Output := tStringList.Create;
   if (OutName = '') then GetFileNameDefaultExt('Merged CSV file','*.csv',fName);
   {$IfDef RecordCSVMerge} writelinetoDebugFile('MergeCSVFiles, files=' + IntToStr(fNames.Count)); {$Endif}
   for I := 0 to pred(fNames.Count) do begin
      fName := fNames.Strings[i];
      if  FileExtEquals(fName,'.csv') or FileExtEquals(fName,'.txt') then begin
         Tstrl := tStringList.Create;
         TStrl.LoadFromFile(fName);
         if TStrl.Count > 0 then begin
            {$IfDef RecordCSVMerge} writelinetoDebugFile(IntegerToString(pred(Tstrl.count),12) + ' lines in  ' + fname); {$Endif}
            if (i = 0) then begin
               Header1 := tstrl.strings[0];
               for j := 0 to pred(TStrl.Count) do Output.Add(TStrl.Strings[j]);
            end
            else begin
               Header := tstrl.strings[0];
               if (Uppercase(Header) <> UpperCase(Header1)) then begin
                  {$IfDef RecordCSVMerge} writelinetoDebugFile('Header mismatch, ' + Header + ' and '+ Header1); {$Endif}
               end
               else begin
                  for j := 1 to pred(TStrl.Count) do Output.Add(TStrl.Strings[j]);
               end;
            end;
         end;
         TStrl.Free;
      end;
   end;
   Output.SaveToFile(OutName);
   {$IfDef RecordCSVMerge}  writelinetoDebugFile(IntegerToString(pred(Output.count),8) + ' lines in merge ' + OutName); writelinetoDebugFile(''); {$Endif}
   Output.Free;
end;


procedure QuickGraphFromStringList(var sl : tStringList; xf,yf,Capt : shortstring);
var
    fName : PathStr;
    db : integer;
begin
    fName := NextFileNumber(MDTempDir,'qg_','.csv');
    sl.SaveToFile(fName);
    if OpenNumberedGISDataBase(db,fName) then begin
        GISdb[db].CreateScatterGram('Test',xf,yf,clRed,false,Capt);
    end;
   CloseAndNilNumberedDB(db);
   sl.Destroy;
end;


function PointInBoundingBox(Lat,Long : float64; BoundBox : sfBoundBox) : boolean;
begin
   Result := (Lat <= BoundBox.YMax) and (Lat >= BoundBox.YMin) and (Long <= BoundBox.XMax) and (Long >= BoundBox.XMin);
end;


function ProgressIncrement(FiltRecsInDB : integer) : integer;
begin
   if FiltRecsInDB < 10 then Result := 1
   else if FiltRecsInDB < 50 then Result := 2
   else if FiltRecsInDB < 250 then Result := 5
   else if FiltRecsInDB < 1000 then Result := 10
   else Result := FiltRecsInDB div 100;
end;

function DefineColorTableValues(Palette : shortstring; Min,Max : float64; var ZColorTable : tColorTableDefinitions; Reverse : boolean = false) : boolean;
var
   dz : float64;
   i : integer;
begin
   {$IfDef RecordColorPalette} WriteLineToDebugFile('Petmar.DefineColorTableValues palette=' + Palette); {$EndIf}
   ZColorTable.ZTableEntries := 0;
   if FileExists(ColorBrewerName) then begin
      if DefineColorArray(Palette,ZColorTable.ZTableEntries,ZColorTable.zTableColors,Reverse) then begin
         dz := (Max - Min) / succ(ZColorTable.ZTableEntries);
         for I := 1 to ZColorTable.ZTableEntries do begin
            ZColorTable.zTableValue[i] := Min + i * dz;
         end;
      end;
   end;
   Result := (ZColorTable.ZTableEntries > 0);
   {$IfDef RecordColorPalette} WriteLineToDebugFile('Min=' + RealToString(Min,-18,2) + ' Max=' + RealToString(Max,-18,2) + ' dz=' + RealToString(dz,-18,2) + ' entries=' + IntToStr(ZColorTable.ZTableEntries)); {$EndIf}
end;



function FieldRequiresLeadingZeros(FieldName : ShortString) : boolean;
begin
   Result := false;
             (*
             StrUtils.AnsiContainsText(FieldName,'ZIP') or
             StrUtils.AnsiContainsText(FieldName,'FIPS') or
             StrUtils.AnsiContainsText(FieldName,'GEOID') or
             StrUtils.AnsiContainsText(FieldName,'GEOID2')
             *)
end;



   function StringList2CSVtoDB(Results : tstringList; fName : Pathstr = ''; CloseFile : boolean = false; SaveCSVfile : boolean = false; OpenTable : boolean = true) : integer;
   var
      fName2 : PathStr;
   begin
      if (Results.Count > 1) then begin
         {$If Defined(RecordOpenDB) or Defined(RecordCSVimport)} WriteLineToDebugFile('StringList2CSVtoDB in fname=' + fName + '   lines in file=' + IntToStr(Results.Count)); {$EndIf}
         if (fName = '') then fName := Petmar.NextFileNumber(MDTempDir, 'Temp_db_','.csv');
         if FileExists(fname) then SysUtils.DeleteFile(fName);
         fName := ChangeFileExt(fName,'.csv');
         fName2 := fName;
         Results.SaveToFile(fName);
         Results.Free;
         OpenNumberedGISDataBase(Result,fName,OpenTable);
         if CloseFile then begin
            CloseAndNilNumberedDB(Result);
            Result := 0;
         end
         else begin
            if OpenTable then GISdb[Result].dbTablef.HideHouseKeepingColumns;
         end;
         {$IfDef RecordOpenDB} WriteLineToDebugFile('StringList2CSVtoDB out, fname=' + fName); {$EndIf}
      end
      else begin
         Result := 0;
      end;
      if (not SaveCSVFile) then File2Trash(fName2);
   end;



function NoStatsField(fName : ShortString) : boolean;
begin
   Result := (fName = 'LAT') or (fName = 'LONG') or (fName = RecNoFName) or (fName = 'COLOR') or (fName = 'CLUSTER')
      or (fName = 'X_UTM') or (fName = 'Y_UTM') or (fName = 'UTM_ZONE') or (fName = 'NPTS') or (fName = 'MASK');
end;


procedure InsureMGRSInDataBase(fName : PathStr; var TheData : tMyData);
begin
   TheData.InsureFieldPresentAndAdded(ftString,'MGRS',18,0);
end;


function ExpandIconName(Table : tMyData; FieldName : ShortString; var fName : PathStr) : boolean;
begin
   fName := Table.GetFieldByNameAsString(FieldName);
   if not FileExists(fName) then begin
      if FileExists(ExtractFilePath(Table.TableName) + fName) then fName := ExtractFilePath(Table.TableName) + fName
      else fName := MainMapData + 'Icons\' + fName;
   end;
   Result := FileExists(fName);
end;


function FindValidJoin(LinkTable : tMyData; LinkField : ShortString; TheFilter : string) : boolean;
begin
   if LinkTable.IsStringField(LinkField) then LinkTable.ApplyFilter(LinkField + '=' + QuotedStr(TheFilter))
   else LinkTable.ApplyFilter(LinkField + '=' + TheFilter);
   Result := (LinkTable.RecordCount = 1) or ((LinkTable.RecordCount > 1) and MDDef.AllowFirstOfMultipleJoins);
end;


procedure DataBaseCorrelations(DB : integer; UseFields : tStringList; var VarCovar,Correlations  : tTrendMatrix; var NumPoints : integer);
label
   HasMissingData;
var
   i,j,Done,rc : integer;
   Data,Sum,MaxVal,StdDev : ^tTrendVector;
   SP : ^tTrendMatrix;
begin
   New(Data);
   New(Sum);
   New(MaxVal);
   New(StdDev);
   New(SP);
   NumPoints := 0;
   for i := 1 to Petmar_types.MaxMatrixSize do begin
      Sum^[i] := 0;
      for j := 1 to Petmar_types.MaxMatrixSize do SP^[i,j] := 0;
   end;

   StartProgress('Correlation statistics');
   GISDB[DB].MyData.First;
   Done := 0;
   rc := GISDB[DB].MyData.RecordCount;
   repeat
     inc(Done);
     {$IfDef VCL}
        if Done mod 25 = 0 then begin
           UpdateProgressBar(Done/rc);
           GISDB[DB].EmpSource.Enabled := false;
        end;
     {$EndIf}
     for i := 1 to UseFields.Count do begin
        if (GISDB[DB].MyData.GetFieldByNameAsString(ptTrim(UseFields[pred(i)])) = '') then goto HasMissingData;
        Data^[i] := GISDB[DB].MyData.GetFieldByNameAsFloat(ptTrim(UseFields[pred(i)]));
     end;
     for i := 1 to UseFields.Count do begin
         Sum^[i] := Sum^[i] + Data^[i];
         for j := 1 to UseFields.Count do SP^[i,j] := SP^[i,j] + 1.0 * Data^[i] * Data^[j];
     end {for i};
     inc(NumPoints);
     HasMissingData:;
     GISDB[DB].MyData.Next;
   until GISDB[DB].MyData.EOF;
   EndProgress;

   if (NumPoints > 1) then  begin
     for i := 1 to UseFields.Count do StdDev^[i] := sqrt((SP^[i,i] * NumPoints - 1.0 * Sum^[i] * Sum^[i]) / (1.0 * NumPoints * pred(NumPoints)));
     for i := 1 to UseFields.Count do begin
        for j := 1 to UseFields.Count do begin
            if (StdDev^[j] > 0.001) and (StdDev^[i] > 0.001) then begin
               Correlations[i,j] := (SP^[i,j] - (Sum^[j] * Sum^[i] / NumPoints) ) / pred(NumPoints) / StdDev^[j] / StdDev^[i];
               Correlations[j,i] := Correlations[i,j];
               VarCovar[i,j] := (SP^[i,j] - (Sum^[i] * Sum^[j] / NumPoints ) ) / pred(NumPoints);
               VarCovar[j,i] := VarCoVar[i,j];
            end;
        end;
     end {for i};
   end {if};
   Dispose(Data);
   Dispose(Sum);
   Dispose(MaxVal);
   Dispose(StdDev);
   Dispose(SP);
end;


procedure FindPointFileGeoLimits(Table : tMyData; var HiLat,LowLong,LowLat,HighLong : float64);
var
   LatFieldName,LongFieldName : string16;
begin
   LatFieldName := 'LAT';
   LongFieldName := 'LONG';
   Table.FindFieldRange(LatFieldName,LowLat,HiLat);
   Table.FindFieldRange(LongFieldName,LowLong,HighLong);
end;


procedure GetFieldValuesInArray(Table : tMyData; FieldDesired : ShortString; var zs : bfarray32; var Npts,Missing : int64; var Min,Max : float64);
begin
   GetFieldValuesInArrayLinkPossible(Table,Nil,'','',FieldDesired,zs,Npts,Missing,Min,Max);
end;


procedure GetFieldValuesInArrayLinkPossible(Table,LinkData : tMyData; LinkFieldThisDB,LinkFieldOtherDB,FieldDesired : ShortString; var zs : bfarray32; var Npts,Missing : int64; var Min,Max : float64);
var
   z : float64;
   TStr : string;
   Skip,rc,i : integer;
   Linking : boolean;
begin
   Linking := LinkedField(FieldDesired);
   NPts := 0;
   Table.First;
   Missing := 0;
   Min := 99e39;
   Max := -99e39;
   Skip := 1;
   rc := Table.RecordCount;
   while (rc div Skip > bfArrayMaxSize) do inc(Skip);
   StartProgress('Get values ' + FieldDesired);

   repeat
      if (NPts mod 1000 = 0) then UpdateProgressBar(Npts/rc);

      if Linking then begin
         if FindValidJoin(LinkData,LinkFieldOtherDB,Table.GetFieldByNameAsString(LinkFieldThisDB)) then TStr := LinkData.GetFieldByNameAsString(FieldDesired)
         else TStr := '';
      end
      else TStr := Table.GetFieldByNameAsString(FieldDesired);
      TStr := ptTrim(TStr);
      if (TStr <> '') then begin
         z := StrToFloat(TStr);
         Petmath.CompareValueToExtremes(z,Min,Max);
         zs[Npts] := z;
         inc(NPts);
      end
      else inc(Missing);
      for i := 1 to Skip do Table.Next;
   until Table.eof;
   EndProgress;
end;


function LinkedField(var FieldDesired : ShortString) : boolean;
begin
   Result := (Length(FieldDesired) > 5) and (UpperCase(Copy(FieldDesired,1,5)) = 'LINK-');
   if Result then Delete(FieldDesired,1,5);
end;


procedure DisplayWWWFromDataBase(Table : tMyData; WWWFieldName : ShortString = '');
{$IfDef VCL}
var
   URL : shortString;
   Ext : ExtStr;
begin
   if (WWWFieldName = '') then begin
      if Table.FieldExists('WWW') then WWWFieldName := 'WWW'
      else if Table.FieldExists('URL') then WWWFieldName := 'URL'
      else exit;
   end;
   URL := ptTrim(Table.GetFieldByNameAsString(WWWFieldName));
   if FileExists(ExtractFilePath(Table.TableName) + URL) then begin
      URL := ExtractFilePath(Table.TableName) + URL;
   end
   else begin
       Ext := UpperCase(ExtractFileExt(URL));
       if (UpperCase(Copy(URL,1,4)) <> 'HTTP') then begin
          if (Ext <> '.HTML') and (Ext <> '.HTM') and (Ext <> '.COM') and (Ext <> '.ASP') and (Ext <> '.GOV') and (Ext <> '.ORG') then begin
             URL := ExtractFilePath(Table.TableName) + URL;
             if not FileExists(URL) then URL := URL + '.doc';
          end
          else if (UpperCase(Copy(URL,1,4)) <> 'HTTP') then URL := 'https://' + URL;
       end;
   end;
   ExecuteFile(URL);
{$Else}
begin
{$EndIf}
end;


{$IfDef NoDBFManipulation}
{$Else}

      procedure CheckDBaseIndexes(fName : PathStr);
      var
         dbf : file;
         DBaseIIITableFileHeader : tDBaseIIITableFileHeader;
      begin
         if (fName <> '') then begin
            assignFile(dbf,fName);
            reset(dbf,1);
            BlockRead(dbf, DBaseIIITableFileHeader, SizeOf(tDBaseIIITableFileHeader));
            CloseFile(dbf);
            if (DBaseIIITableFileHeader.Reserved[17] = 1) and (not FileExists(ChangeFileExt(fName,'*.MDX'))) then begin
               DBaseIIITableFileHeader.Reserved[17] := 0;
               DBaseIIITableFileHeader.Reserved[18] := 0;
               reset(dbf,1);
               BlockWrite(dbf, DBaseIIITableFileHeader, SizeOf(tDBaseIIITableFileHeader));
               CloseFile(dbf);
            end;
         end;
      end;


      procedure RepairDBaseFieldNames(fName : PathStr);
      begin
         RenameDBaseField(fName,'','');
         ApplicationProcessMessages;
      end;


      procedure RenameDBaseField(fName : PathStr; OldName,NewName : ShortString);
      var
         i,j,k,Offset,NumFields : integer;
         inf : file;
         DBaseIIITableFileHeader : tDBaseIIITableFileHeader;
         TableFieldDescriptor : tTableFieldDescriptor;
         TStr : ShortString;
      begin
         if FileExists(fName) then begin
            {$IfDef RecordDBF} WriteLineToDebugFile('RepairDBaseFieldNames' + fname); {$EndIf}
            InsureFileIsNotReadOnly(fName);
            assignFile(inf,fName);
            reset(inf,1);
            BlockRead(inf, DBaseIIITableFileHeader, SizeOf(tDBaseIIITableFileHeader));
            {$IfDef RecordDBF} WriteLineToDebugFile('Rec size 1=' +IntToStr(DBaseIIITableFileHeader.BytesInRecord)); {$EndIf}

            DBaseIIITableFileHeader.LastUpdate[1] := 0;
            DBaseIIITableFileHeader.LastUpdate[2] := 1;
            DBaseIIITableFileHeader.LastUpdate[3] := 17;

            seek(inf,0);
            BlockWrite(inf, DBaseIIITableFileHeader, SizeOf(tDBaseIIITableFileHeader));

            {$IfDef RecordDBF} WriteLineToDebugFile('Rec size 2=' +IntToStr(DBaseIIITableFileHeader.BytesInRecord)); {$EndIf}

            if (OldName = '') and (NewName = '') then begin
               OldName := 'LON';
               NewName := 'LONG';
            end;

            NumFields := (DBaseIIITableFileHeader.BytesInHeader-33) div 32;
            for i := 1 to NumFields do begin
               Offset := SizeOf(tDBaseIIITableFileHeader) + pred(I) * SizeOf(tTableFieldDescriptor);
               seek(inf,Offset);

               BlockRead(inf, TableFieldDescriptor, SizeOf(tTableFieldDescriptor));
               {$IfDef RecordDBF}
                  with TableFieldDescriptor do begin
                     WriteLineToDebugFile(FieldName +  '  ' +  FieldType +
                        IntegerToString(FieldLength,8) +  IntegerToString(FieldDecimalCount,8));
               end;
               {$EndIf}
               TStr := Uppercase(ptTrim(TableFieldDescriptor.FieldName));
               if (TStr = OldName) then TStr := NewName;
               for j := Low(TStr) to High(TStr) do if (TStr[j] = ' ') then TStr[j] := '_';
               TStr := Copy(TStr,1,10);
               while (length(TStr) < 12) do TStr := TStr + #0;
               for j := 0 to 10 do TableFieldDescriptor.FieldName[j] := TStr[succ(j)];
               with TableFieldDescriptor do if (FieldType = 'N') and (FieldLength > 18) then FieldType := 'C';
               TableFieldDescriptor.FieldDataAddress := 0;
               for k := 1 to 2 do TableFieldDescriptor.ReservedForLAN[k] := 0;
               TableFieldDescriptor.WorkAreaID := 0;
               for k := 1 to 2 do TableFieldDescriptor.ReservedForLAN2[k] := 0;
               TableFieldDescriptor.SetFieldsFlag := 0;
               for k := 24 to 31 do TableFieldDescriptor.Reserved[k] := 0;
               seek(inf,Offset);
               BlockWrite(inf, TableFieldDescriptor, SizeOf(tTableFieldDescriptor));
               {$IfDef RecordDBF}
                  WriteLineToDebugFile('Revised: ' + TableFieldDescriptor.FieldName +  ' ' +  TableFieldDescriptor.FieldType + ' ' + IntToStr(TableFieldDescriptor.FieldLength) + ' ' + IntToStr(TableFieldDescriptor.FieldDecimalCount));
               {$EndIf}
            end;
            {$IfDef RecordDBF} WriteLineToDebugFile('Rec size 3=' +IntToStr(DBaseIIITableFileHeader.BytesInRecord)); {$EndIf}
            CloseFile(inf);
         end;
      end;

      procedure ChangeDBaseFieldType(fName : PathStr; OldName : ShortString; NewType : ANSIChar);
      var
         i,Offset,NumFields : integer;
         inf : file;
         DBaseIIITableFileHeader : tDBaseIIITableFileHeader;
         TableFieldDescriptor : tTableFieldDescriptor;
      begin
         if FileExists(fName) then begin
            {$IfDef RecordDBF} WriteLineToDebugFile('ChangeDBaseFieldType' + fname); {$EndIf}
            InsureFileIsNotReadOnly(fName);
            assignFile(inf,fName);
            reset(inf,1);
            BlockRead(inf, DBaseIIITableFileHeader, SizeOf(tDBaseIIITableFileHeader));

            DBaseIIITableFileHeader.LastUpdate[1] := 0;
            DBaseIIITableFileHeader.LastUpdate[2] := 1;
            DBaseIIITableFileHeader.LastUpdate[3] := 17;

            seek(inf,0);
            BlockWrite(inf, DBaseIIITableFileHeader, SizeOf(tDBaseIIITableFileHeader));

            NumFields := (DBaseIIITableFileHeader.BytesInHeader-33) div 32;
            for i := 1 to NumFields do begin
               Offset := SizeOf(tDBaseIIITableFileHeader) + pred(I) * SizeOf(tTableFieldDescriptor);
               seek(inf,Offset);

               BlockRead(inf, TableFieldDescriptor, SizeOf(tTableFieldDescriptor));
               if (Uppercase(ptTrim(TableFieldDescriptor.FieldName)) = OldName) then begin
                  TableFieldDescriptor.FieldType := NewType;
               end;

               seek(inf,Offset);
               BlockWrite(inf, TableFieldDescriptor, SizeOf(tTableFieldDescriptor));
            end;
            CloseFile(inf);
         end;
      end;

      procedure ChangeDBaseFieldDecimals(fName : PathStr; OldName : ShortString; NewDecimals : byte);
      var
         i,Offset,NumFields : integer;
         inf : file;
         DBaseIIITableFileHeader : tDBaseIIITableFileHeader;
         TableFieldDescriptor : tTableFieldDescriptor;
      begin
         if FileExists(fName) then begin
            {$IfDef RecordDBF} WriteLineToDebugFile('ChangeDBaseFieldType' + fname); {$EndIf}
            InsureFileIsNotReadOnly(fName);
            assignFile(inf,fName);
            reset(inf,1);
            BlockRead(inf, DBaseIIITableFileHeader, SizeOf(tDBaseIIITableFileHeader));

            DBaseIIITableFileHeader.LastUpdate[1] := 0;
            DBaseIIITableFileHeader.LastUpdate[2] := 1;
            DBaseIIITableFileHeader.LastUpdate[3] := 17;

            seek(inf,0);
            BlockWrite(inf, DBaseIIITableFileHeader, SizeOf(tDBaseIIITableFileHeader));

            NumFields := (DBaseIIITableFileHeader.BytesInHeader-33) div 32;
            for i := 1 to NumFields do begin
               Offset := SizeOf(tDBaseIIITableFileHeader) + pred(I) * SizeOf(tTableFieldDescriptor);
               seek(inf,Offset);

               BlockRead(inf, TableFieldDescriptor, SizeOf(tTableFieldDescriptor));
               if (Uppercase(ptTrim(TableFieldDescriptor.FieldName)) = OldName) then begin
                  TableFieldDescriptor.FieldDecimalCount := NewDecimals;
               end;

               seek(inf,Offset);
               BlockWrite(inf, TableFieldDescriptor, SizeOf(tTableFieldDescriptor));
            end;
            CloseFile(inf);
         end;
      end;



{$EndIf}


procedure ZeroTable(fName : PathStr);
var
   TheTable : tMyData;
begin
   if FileExists(fName) then begin
      TheTable := tMyData.Create(fName);
      ZeroTable(TheTable);
      TheTable.Destroy;
   end;
end;

procedure ZeroTable(var TheTable : tMyData);
var
   Total,count : integer;
begin
   if (TheTable <> nil) then begin
      Total := TheTable.RecordCount;
      if (Total > 0) then begin
         Count := 0;
        {$IfDef VCL} if WantShowProgress then StartProgress('Zero table'); {$EndIf}
         while (TheTable.RecordCount > 0) do begin
            inc(Count);
            {$IfDef VCL} if WantShowProgress and (Count mod 1000 = 0) then UpdateProgressBar(Count/Total); {$EndIf}
            TheTable.Edit;
            TheTable.Delete;
         end;
         {$IfDef VCL} if WantShowProgress then EndProgress; {$EndIf}
      end;
   end;
end;


{$IfDef ExDBImages}
{$Else}

procedure ShowOneImageFromDataBase(DataTable : tMyData; FullDBName : PathStr; fieldName : ShortString);
var
   fName : PathStr;
   Ext  : ExtStr;
   BlowUp : integer;
   LoadedImage : PETImage_form.TImageDisplayForm;
begin
   with DataTable do begin
      if not DataTable.FieldExists(fieldName) then exit;
      fName := GetFieldByNameAsString(fieldName);
      {$IfDef RecordDataBaseImage} WriteLineToDebugFile('Image name: ' + GetFieldByNameAsString(fieldName)); {$EndIf}
      if not FileExists(FName) then fName := ExtractFilePath(FullDBName) + fName;
      if FileExists(FName) then begin
         {$IfDef RecordDataBaseImage}
         WriteLineToDebugFile('File name: ' + fName);
         {$EndIf}
         Ext := UpperCase(ExtractFileExt(fName));
         if ValidImageFileExt(Ext) then begin
            LoadedImage := TImageDisplayForm.Create(Application);
            LoadedImage.LoadImage(FName);
            BlowUp := 100;
            repeat
               Dec(BlowUp);
            until (LoadedImage.Width * BlowUp div 100 < Screen.Width div 2) and (LoadedImage.Height* BlowUp div 100 < Screen.Height div 2);
            if (BlowUp < 100) then LoadedImage.ResizeImage(LoadedImage.Width * BlowUp div 100, LoadedImage.Height * BlowUp div 100);
         end
         else begin
            ExecuteFile(fName);
         end;
      end;
   end;
end;


procedure DisplayImageFromDataBase(DataTable : tMyData; FullDBName : PathStr);
var
   i : integer;
begin
   ShowOneImageFromDataBase(DataTable,FullDBName,'IMAGE');
   for i := 1 to 9 do ShowOneImageFromDataBase(DataTable,FullDBName,'IMAGE' + IntToStr(i));
end;
{$EndIf}


function NumberUniqueEntries(Table : tMyData; FieldName : ShortString) : integer;
var
   DataThere : tStringList;
begin
   DataThere := Table.ListUniqueEntriesInDB(FieldName);
   Result := DataThere.Count;
   DataThere.Free;
end;


function GetStringFromFieldInDataBase(Prompt : shortString; Table : tMyData; FieldName : ShortString) : ShortString;
var
   DataThere : tStringList;
   i : integer;
begin
   Result := '';
   {$IfDef VCL}
      DataThere := Table.ListUniqueEntriesInDB(FieldName);
      i := 0;
      if MultiSelectSingleColumnStringList(Prompt,i,DataThere,true) then begin
         Result := DataThere.Strings[i];
         {$IfDef RecordStringFromTable} WriteLineToDebugFile('i=' + IntToStr(i) + '   ' + Result); {$EndIf}
      end;
      DataThere.Free;
   {$EndIf}
end;


function AddAndIfNeeded(Filter : AnsiString) : AnsiString;
begin
   if (Filter = '') then Result := ''
   else Result := Filter + ' AND ';
end;

function AddOrIfNeeded(Filter : AnsiString) : AnsiString;
begin
   if (Filter = '') then Result := ''
   else Result := Filter + ' OR ';
end;


procedure FindUniqueEntriesLinkPossible(Table,LinkData : tMyData; LinkFieldThisDB,LinkFieldOtherDB,FieldName : ShortString; var DataThere : tStringList; Sort : boolean = true);
var
   Count,rc : integer;
   BaseTable : tMyData;
   TStr : ShortString;
begin
   {$If Defined(RecordRangeProblems) or Defined(RecordUniqueProblems)} WriteLineToDebugFile('FindUniqueEntriesLinkPossible in ' + Table.TableName + '  ' + FieldName); {$EndIf}
   try
      BaseTable := Table;
      if (LinkData <> Nil) and LinkedField(FieldName) then BaseTable := LinkData;
      DataThere := tStringList.Create;
      BaseTable.First;
      Count := 0;
      if (BaseTable.RecordCount > 0) then begin
         if Sort then begin
            DataThere.Sorted := true;
            DataThere.Duplicates := dupIgnore;
         end;
         {$IfDef VCL} if WantShowProgress then StartProgressAbortOption('Find field values: ' + FieldName); {$EndIf}
         rc := BaseTable.RecordCount;
         while not BaseTable.EOF do begin
           TStr := ptTrim(BaseTable.GetFieldByNameAsString(FieldName));
           if (TStr <> '') then DataThere.Add(TStr);
           BaseTable.Next;
           inc(Count);
           {$IfDef VCL} if WantShowProgress and ((Count mod 500) = 0) then UpdateProgressBar(Count/rc); {$EndIf}
           if WantOut then break;
         end;
      end;
   finally
     {$IfDef VCL} if WantShowProgress then EndProgress; {$EndIf}
   end;
   {$IfDef RecordRange} WriteLineToDebugFile('FindUniqueEntriesLinkPossible out'); {$EndIf}
end;


procedure FindUniqueEntries(Table : tMyData; FieldName : ShortString; var DataThere : tStringList; Sort : boolean = true);
begin
   FindUniqueEntriesLinkPossible(Table,Nil,'','',FieldName,DataThere,Sort);
end;


function FilterDecBasedOnMapSize(MapPixelSize : float64)  : integer;
begin
   if MapPixelSize < 0 then Result := -6
   else begin
      if MapPixelSize > 2500 then Result := -2
      else if MapPixelSize > 1200 then Result := -3
      else if MapPixelSize > 750 then Result := -4
      else Result := -6;
   end;
end;


function PointBoundBoxGeo(Lat,Long : float64) : sfBoundBox;
begin
   Result.xMax := Long + 0.001;
   Result.xMin := Long -0.001;
   Result.yMax := Lat + 0.001;
   Result.yMin := Lat - 0.001;
end;


procedure SafeAdjustGeoBoundBoxForPixelIsArea(var bb : sfBoundBox);
const
   QuarterSec = 1 / (4 * 3600);
begin
   bb.XMin := bb.XMin + QuarterSec;
   bb.XMax := bb.XMax - QuarterSec;
   bb.YMin := bb.YMin + QuarterSec;
   bb.YMax := bb.YMax - QuarterSec;
end;



function MakePointGeoFilter(LatFieldName,LongFieldName : string16; HiLat,LowLong,LowLat,HighLong : float64) : AnsiString;
begin
   if (LatFieldName = '') then LatFieldName := 'LAT';
   if (LongFieldName = '') then LongFieldName := 'LONG';
   if (LowLong > 0) and (HighLong < 0) then begin
      Result := '(' + LatFieldName + '>=' + RealToString(LowLat,-18,6) + ') AND (' + LatFieldName + '<=' + RealToString(HiLat,-18,6) +
        ') AND ((' + LongFieldName + '>=' + RealToString(LowLong,-18,6) + ') OR (' + LongFieldName + '<=' + RealToString(HighLong,-18,6) + '))';
   end
   else Result := '(' + LatFieldName + '>=' + RealToString(LowLat,-18,6) + ') AND (' + LatFieldName + '<=' + RealToString(HiLat,-18,6) +
      ') AND (' + LongFieldName + '>=' + RealToString(LowLong,-18,6) + ') AND (' + LongFieldName + '<=' + RealToString(HighLong,-18,6) + ')';
end;


function PointVeryCloseGeoFilter(LatFieldName,LongFieldName : string16; Lat,Long : float64; Bit : float64 = 0.0001) : AnsiString;
begin
   Result := MakePointGeoFilter(LatFieldName,LongFieldName,Lat+Bit,Long-Bit,Lat-Bit,Long+Bit);
end;


function PointInBoxGeoFilter(Lat,Long : float64) : AnsiString;
begin
   Result := '(LAT_LOW<=' + RealToString(Lat,-18,6) + ' AND ' + 'LAT_HI>=' + RealToString(Lat,-18,6) + ' AND ' + 'LONG_LOW<=' + RealToString(Long,-18,6) + ' AND LONG_HI>=' + RealToString(Long,-18,6) + ')'
end;


function CornersFromBoundBoxGeo(bb : sfBoundBox) : shortString;
begin
   Result := 'NW corner: ' + LatLongDegreeToString(bb.ymax,bb.xmin) + ' SE corner: ' + LatLongDegreeToString(bb.ymin,bb.xmax);
end;


function MakeGeoFilterFromBoundingBox(bBox : sfBoundBox) : AnsiString;
var
   TStr : shortstring;
begin
   LongitudeAngleInRange(bbox.xmin);
   LongitudeAngleInRange(bbox.xmax);
   if (bbox.xmin < bbox.xmax) then TStr := 'LONG_LOW<=' + RealToString(bbox.xmax,-18,6) + ' AND LONG_HI>=' + RealToString(bbox.xmin,-18,6)
   else TStr := '((LONG_LOW<=' + RealToString(bbox.xmax,-18,6) + ' AND LONG_HI>=' + RealToString(bbox.xmin,-18,6) + ') OR ' +
                 '(LONG_LOW<=' + RealToString(bbox.xmin,-18,6) + ' AND LONG_HI<=' + RealToString(bbox.xmax,-18,6) + ')';
   Result := '(LAT_LOW<=' + RealToString(bbox.ymax,-18,6) + ' AND LAT_HI>=' + RealToString(bbox.ymin,-18,6) + ' AND ' + TStr + ')';

end;


function MakeGeoFilterFromCorners(HiLat,LowLong,LowLat,HighLong : float64) : AnsiString;
var
   TStr : shortstring;
begin
  {$IfDef RecordDataBaseFilter} WriteLineToDebugFile('MakeCornersGeoFilter:  NW corner: ' + LatLongDegreeToString(HiLat,LowLong)) + '   SE corner: ' + LatLongDegreeToString(LowLat,HighLong)); {$EndIf}
   LongitudeAngleInRange(LowLong);
   LongitudeAngleInRange(HighLong);
   if (LowLong < HighLong) then TStr := 'LONG_LOW<=' + RealToString(HighLong,-18,6) + ' AND LONG_HI>=' + RealToString(LowLong,-18,6)
   else TStr := '((LONG_LOW<=' + RealToString(HighLong,-18,6) + ' AND LONG_HI>=' + RealToString(LowLong,-18,6) + ') OR ' +
                 '(LONG_LOW<=' + RealToString(LowLong,-18,6) + ' AND LONG_HI<=' + RealToString(HighLong,-18,6) + ')';
  Result := '(LAT_LOW<=' + RealToString(HiLat,-18,6) + ' AND LAT_HI>=' + RealToString(LowLat,-18,6) + ' AND ' + TStr + ')';
  {$IfDef RecordDataBaseFilter} WritelineToDebugFile(' Filter= ' + Result); {$EndIf}
end;


procedure CopyDBTable(FromDBF,ToDBF : PathStr);
{copies all fields from one table to another.  The second table could have new fields, precluding a file copy}
var
   FromTable,ToTable : tMyData;
   i : integer;
begin
   FromTable := TMyData.Create(FromDBF);
   ToTable := TMyData.Create(ToDBF);
   while not FromTable.Eof do begin
      ToTable.Insert;
      for i := 0 to pred(FromTable.FieldCount) do begin
         ToTable.SetFieldByNameAsString(FromTable.GetFieldName(i),ptTrim(FromTable.GetFieldByNameAsString(FromTable.GetFieldName(I))));
      end;
      ToTable.Post;
      FromTable.Next;
   end;
   FromTable.Destroy;
   ToTable.Destroy;
end;


procedure GetFieldsLinkPossible(Table,LinkTable : tMyData; VisCols : Array100Boolean; TypesAllowed : tSetFieldType; var FieldsInDB : tStringList;
     AllFields : boolean = false; SortFields : boolean = false);
var
   i : integer;
   FieldsInLinkDB : tStringList;
begin
   GetFields(Table,VisCols,TypesAllowed,FieldsInDB);
   if (LinkTable <> Nil) then begin
      GetFields(LinkTable,AllVis,TypesAllowed,FieldsInLinkDB);
      for I := 0 to pred(FieldsInLinkDB.Count) do FieldsInDB.Add('LINK-' + FieldsInLinkDB.Strings[i]);
      FieldsInLinkDB.Free;
   end;
end;


procedure GetFields(Table : tMyData; VisCols : Array100Boolean; TypesAllowed : tSetFieldType; var FieldsInDB : tStringList; AllFields : boolean = false; SortFields : boolean = false);
var
   i : integer;
begin
   {$IfDef RecordGetField} WriteLineToDebugFile('GetFields'); {$EndIf}
   FieldsInDB := tStringList.Create;
   if SortFields then FieldsInDB.Sorted := true;
   if (Table <> Nil) then with Table do begin
      for i := 0 to pred(Table.FieldCount) do begin
         if (GetFieldType(i) in TypesAllowed) and ( VisCols[i] or AllFields or (i > MaxFieldsInDB)) then begin
            FieldsInDB.Add(GetFieldName(i));
            {$IfDef RecordGetField} WriteLineToDebugFile('Add: ' + IntToStr(i) + '  ' + GetFieldName(i) + '  ' + FieldTypeStr(GetFieldType(i))); {$Endif}
         end
         else begin
            {$IfDef RecordGetField} WriteLineToDebugFile('No match: ' + IntToStr(i) + '  ' + GetFieldName(i) + '  ' + FieldTypeStr(GetFieldType(i))); {$Endif}
         end;
      end;
   end;
end;


function HTMLTableCell(Contents : AnsiString) : AnsiString;
begin
   Result :=  StartColumnString + Contents + EndColumnString;
end;


function SingleRecordToHTMLTable(Table : tMyData; VisCols : Array100Boolean; ThumbNailDir : PathStr = '') : AnsiString;
var
   i : integer;
   skip : boolean;
   TStr,TStr2 : ShortString;
begin
  Result := StartTableString + MessLineBreak;
  for i := 0 to pred(Table.FieldCount) do begin
      skip := false;
      TStr2 := Table.GetFieldByNameAsString(Table.GetFieldName(i));
      if VisCols[i] and (TStr2 <> '') then begin
         TStr := ptTrim(Table.GetFieldByNameAsString(Table.GetFieldName(i)));
         if ValidImageFileName(TStr) then begin
            if (Table.GetFieldName(i) = 'ICON') and (Uppercase(Copy(Tstr,1,3)) = 'TN-') then Skip := true;
            TStr := Table.MakeImageTag(ThumbNailDir,Table.GetFieldName(i));
         end
         else if (TStr = '') then TStr := '&nbsp';
         if not Skip then Result := Result + StartRowString + HTMLTableCell(Table.GetFieldName(i)) + HTMLTableCell(TStr) + EndRowString;
      end;
   end;
  Result := Result +  EndTableString + MessLineBreak;
end;


procedure SingleRecordHTMLReport(All : boolean; Table : tMyData; VisCols : Array100Boolean);
var
   Report : tStringList;
   fName : PathStr;
   OneLine,LineClipboard : string;
begin
   {$IfDef RecordHTML} WriteLineToDebugFile('SingleRecordHTMLReport in'); {$EndIf}
   fName := Petmar.NextFileNumber(MDTempDir, ExtractFileNameNoExt(Table.TableName) + '_','.htm');
   Report := tStringList.Create;
   Report.Add(StartHTMLString);
   LineClipboard := '';
   if All then begin
      Table.First;
      while not Table.eof do begin
        OneLine := SingleRecordToHTMLTable(Table,VisCols) + '<hr>';
        Report.Add(OneLine);
        LineClipboard := LineClipboard + OneLine;
        Table.Next;
      end;
   end
   else begin
      OneLine := SingleRecordToHTMLTable(Table,VisCols);
      Report.Add(OneLine);
      LineClipboard := OneLine;
   end;
   Report.Add(EndHTMLString);
   Report.SaveToFile(fName);
   Report.Free;

   {$IfDef RecordHTML} WriteLineToDebugFile('');  WriteLineToDebugFile(LineClipboard);  WriteLineToDebugFile(''); {$EndIf}
   ClipBrd.ClipBoard.AsText := LineClipboard;
   {$IfDef VCL}
      ExecuteFile(fname);
   {$EndIf}
   {$IfDef RecordHTML} WriteLineToDebugFile('SingleRecordHTMLReport out'); {$EndIf}
end;



function DBtableFieldToDropDown(Table : tMyData; fName,WantName : ShortString) : AnsiString;
var
   TStr : ShortString;
begin
   Result := '<label>' + WantName + '<select name = "' + WantName + '" size="1">';
   Table.First;
   while not Table.Eof do begin
      TStr := Table.GetFieldByNameAsString(fName);
      Result := Result + '<option value = "' + TStr + '">' + TStr + '</option>';
      Table.Next;
   end;
   Result := Result + ' </select></label>';
end;


{$IfDef VCL}

function CSVFileImportToDB(fName : PathStr  = ''; SpecialGaz : tCSVImport = csvNormal) : PathStr;
var
   CreateDataBase : tCreateDataBase;
   ZeroPadLen : tZeroPadLen;
   Duplicates,
   FileInMemory,
   RecordValues : tStringList;
   j : int64;
   LinesToMoveToStringGrid,NumDupesIgnored,
   NumRules,fs,skip,
   OnLine,i,k,n,Len,Decs,BadFieldNames : integer;
   LongV : float64;
   SepChar : Char;
   AskAboutBadFieldNames,RecIDExists,SaveToDebugLog,
   IsString,IsFloat,IsLeadingZero : boolean;
   TStr,Str,MenuStr,LastLine : String;
   t2,aStr : shortstring;
   tFile : System.Text;
   NeedTrimWarning,
   ProcessInMemory,AllInStringGrid : boolean;
   Fieldname : shortstring;
   LocalStringGrid : tStringGrid;
   OldNames,NewNames : array[1..250] of string35;


         procedure CleanLine(var MenuStr : String);
         var
            j : integer;
         begin
              if (SepChar = ' ') then begin
                 for j := length(MenuStr) downto 2 do begin
                    if (MenuStr[j] = ' ') and (MenuStr[pred(j)] = ' ') then begin
                       System.Delete(MenuStr,j,1);
                    end;
                 end;
              end;
              if (MenuStr[1] = SepChar) then System.Insert('9999',MenuStr,1);
              for j := pred(Length(MenuStr)) downto 2 do begin
                 if (MenuStr[j] = SepChar) and (MenuStr[pred(j)] = SepChar) then begin
                    System.Insert('9999',MenuStr,j);
                 end;
              end;
         end;

        procedure ProcessLine(MenuStr : string; j : integer);
        {$IfDef RecordProcessLine}
        const
           RecsDone : integer = 0;
        {$EndIf}
         var
            i : integer;
         begin
           {$IfDef RecordProcessLine} inc(RecsDone); WriteLineToDebugFile('Recs=' + IntToStr(RecsDone)); if (RecsDone > 100) then MDdef.MDRecordDebugLog := false; {$EndIf}
           CleanLine(MenuStr);
            RecordValues := tStringList.Create;
            for i := 0 to pred(LocalStringGrid.ColCount) do begin
               fName := ptTrim(LocalStringGrid.Cells[i,0]);
               TStr := Petmar_types.BeforeSpecifiedCharacterUnicode(MenuStr,SepChar,true,true);
               {$IfDef RecordProcessLine} WriteLineToDebugFile('fName=' + fName + '   TStr=' + TStr); {$EndIf}
               if (fName <> 'SKIP') then begin
                  if FieldRequiresLeadingZeros(fName) then begin
                     while (Length(TStr) < ZeroPadLen[i]) do TStr := '0' + TStr;
                  end;
                  RecordValues.Add(ptTrim(TStr));
                  {$IfDef RecordProcessLine} WriteLineToDebugFile('added TStr=' + TStr); {$EndIf}
               end;
            end;
            if (not RecIdExists) then  RecordValues.Add(IntToStr(j));
            CreateDataBase.AddCorrectRecordFromStringList(RecordValues);
            RecordValues.Free;
         end;


         procedure GetGazReplacements;
         var
            Table : tMyData;
         begin
            Table := tMyData.Create(CSVImportRulesFName);
            if (SpecialGaz = csvNGAgaz) then Table.ApplyFilter('GAZ_TYPE=' + QuotedStr('NGA'))
            else if (SpecialGaz = csvUSGSgaz) then Table.ApplyFilter('GAZ_TYPE=' + QuotedStr('USGS'));
            while not Table.eof do begin
               inc(NumRules);
               OldNames[NumRules] := Table.GetFieldByNameAsString('IN_NAME');
               NewNames[NumRules] := Table.GetFieldByNameAsString('OUT_NAME');
               Table.Next;
            end;
            Table.Destroy;
         end;


begin
  {$If Defined(RecordShortCSV) or Defined(RecordCSVProblems)} WriteLineToDebugFile('DoCSVFileImport enter for ' + fName); {$EndIf}
  {$IfDef RecordProcessLine} SaveToDebugLog := MDdef.MDRecordDebugLog; {$EndIf}
   if (FName = '') then begin
      fName := ProgramRootDir;
      if not GetFileFromDirectory('CSV file','*.txt;*.csv',fName) then exit;
   end;
   wmdem.SetPanelText(0,'Make ' + ExtractFileName(fName));

   if OutsideCSVImport then begin
      RemoveASCII0FromFile(fName);
   end;

     LocalStringGrid := tStringGrid.Create(Application);
     ShowHourglassCursor;
     fs := Petmar.GetFileSize(fName);
     ProcessInMemory := (fs < (InMemoryStringSizeLimit));

     {$If Defined(RecordShortCSV) or Defined(RecordCSVProblems)} if ProcessInMemory then WriteLineToDebugFile('Process in memory') else WriteLineToDebugFile('Process by line'); {$EndIf}
     FileInMemory := tStringList.Create;
     if ProcessInMemory then begin
        FileInMemory.LoadFromFile(fName);
     end
     else begin
        fs := fs div 1024 div 1024;
        if fs < 50 then Skip := 0
        else if fs < 200 then Skip := 10
        else Skip := 25;

        AssignFile(tfile,fName);
        reset(tfile);
        repeat
           if not eof(tfile) then begin
              readln(tfile,LastLine);
              if (LastLine <> '') then FileInMemory.Add(LastLine);
              for j := 1 to skip do readln(tfile);
           end;
        until eof(tfile);
     end;

     {$IfDef RecordCSV}
        WriteLineToDebugFile('CSV file import: ' + fname + '  Records: ' + IntToStr(FileInMemory.Count));
        k := 10;
        if (FileInMemory.Count < 10) then k := pred(FileInMemory.Count);
        for j := 0 to pred(k) do WriteLineToDebugFile('---' +  FileInMemory.Strings[j]);
     {$EndIf}

     Str := FileInMemory.Strings[0];
     for k := Length(Str) downto 1 do
        if Str[k] in ['0'..'9','-','.',' ',',',#9] then Delete(Str,k,1);
     if (Length(Str) = 0) then begin
        if (not WeKnowTheHeader) and AnswerIsYes('Enter field names') then begin
           aStr := FileInMemory.Strings[0];
           Petmar.GetString('Headers for ' + aStr,MDDef.CSVfileHeaders,true,DBaseFieldNameChars + [' ',',']);
        end;
        FileInMemory.Insert(0,MDDef.CSVfileHeaders);
     end;

     LocalStringGrid.RowCount := FileInMemory.Count;
     MenuStr := ptTrim(FileInMemory.Strings[0]);
     for k := 1 to 2 do if (length(MenuStr) > 0) and (MenuStr[1] = '/') then Delete(MenuStr,1,1);
     GetSeparationCharacterUnicode(MenuStr,SepChar);

     {$IfDef RecordCSV} WriteLineToDebugFile('SepChar= "' + SepChar + '"' + '  ord=' + IntToStr(ord(SepChar))); {$EndIf}

     if ForceAllInStringGrid then begin
        AllInStringGrid := true;
        ForceAllInStringGrid := false;
     end
     else begin
         if ProcessInMemory then begin
            AllInStringGrid := (FileInMemory.Count < InMemoryStringSizeLimit);
         end
         else AllInStringGrid := false;
     end;

     if AllInStringGrid then LinesToMoveToStringGrid := pred(FileInMemory.Count)
     else begin
         if ProcessInMemory then LinesToMoveToStringGrid := InMemoryStringSizeLimit
         else LinesToMoveToStringGrid := pred(FileInMemory.Count);
     end;
     NumDupesIgnored := 0;
     if (SepChar in [#9, ',', ' ', '|', ';']) then begin
        ptTrim(MenuStr);
        if (SepChar = ' ') then begin
           for j := length(MenuStr) downto 2 do
              if (MenuStr[j] = ' ') and (MenuStr[pred(j)] = ' ') then begin
                 System.Delete(MenuStr,j,1);
              end;
        end;
        i := 1;
        for j := 1 to length(MenuStr) do if (MenuStr[j] = SepChar) then inc(i);
        LocalStringGrid.ColCount := i;
        Duplicates := tStringList.Create;
        LastLine := '';
        OnLine := 0;
        StartProgress('Parse');
        for i := 0 to LinesToMoveToStringGrid do begin
           if (i mod 2000 = 0) then UpdateProgressBar(i/LinesToMoveToStringGrid);
           MenuStr := ptTrim(FileInMemory.Strings[i]);
           ReplaceStr(MenuStr,#0,'');
           if (SepChar <> ' ') then ReplaceStr(MenuStr,' ','');
           {$IfDef RecordCSV} if (i mod 25000 = 0) or (i < 10) then WriteLineToDebugFile(IntToStr(i) + '/' + IntToStr(FileInMemory.Count) + '  line=' + MenuStr); {$EndIf}
           if (MenuStr <> LastLine) or MDDef.DupeImportsAllowed then begin
              LastLine := MenuStr;
              CleanLine(MenuStr);
             {$IfDef RecordCSV} if (i mod 25000 = 0) or (i < 10) then WriteLineToDebugFile('clean=' + MenuStr); {$EndIf}
              for j := 0 to LocalStringGrid.ColCount-2 do begin
                 T2 := ptTrim(BeforeSpecifiedCharacterUnicode(MenuStr,SepChar,false,true));
                 if (length(t2) > 2) and (t2[1] = '"') and  (t2[length(t2)] = '"') then begin
                    Delete(t2,1,1);
                    Delete(t2,length(t2),1);
                 end;
                 LocalStringGrid.Cells[j,OnLine] := t2;
                 {$IfDef RecordCSV} if (i mod 25000 = 0) or (i < 25) then WriteLineToDebugFile(IntToStr(j) + '  ' + LocalStringGrid.Cells[j,0] + ' = ' + LocalStringGrid.Cells[j,OnLine] +  '   len=' + IntToStr(length(LocalStringGrid.Cells[j,OnLine])) ); {$EndIf}
              end;
              LocalStringGrid.Cells[pred(LocalStringGrid.ColCount),OnLine] := ptTrim(MenuStr);
              inc(OnLine);
           end
           else begin
              inc(NumDupesIgnored);
              Duplicates.Add(LastLine);
           end;
        end;
        LocalStringGrid.RowCount := OnLine;
     end;
     {$IfDef RecordCSV} if (NumDupesIgnored > 0) then WriteLineToDebugFile(' Duplicate lines ignored: ' + IntToStr(NumDupesIgnored)); {$EndIf}

     Duplicates.Destroy;

     {$IfDef RecordCSV} WriteLineToDebugFile(' Records: ' + IntToStr(LocalStringGrid.RowCount) + '   Fields: ' + IntToStr(LocalStringGrid.ColCount)); {$EndIf}

     Result := ExtractFilePath(fName) + SpacesToUnderScores(ExtractFileNameNoExt(fName)) + DefaultDBExt;
     DeleteFileIfExists(Result);

     NumRules := 0;
     if (SpecialGaz = csvNGAgaz) or (SpecialGaz = csvUSGSgaz) then GetGazReplacements;

     RecIDExists := false;
     for i := 0 to pred(LocalStringGrid.ColCount) do begin
         FieldName := UpperCase(ptTrim(LocalStringGrid.Cells[i,0]));
         if StrUtils.AnsiContainsText(FieldName,'Roughness (') then FieldName := 'ROUGHNESS';
         if StrUtils.AnsiContainsText(FieldName,'Gaussian curvature (') then FieldName := 'CURV_GAUSS';
         for j := length(FieldName) downto 1 do if (FieldName[j] in [' ']) then Delete(FieldName,j,1);
         {$IfDef RecordCSV} WriteLineToDebugFile(FieldName); {$EndIf}
         if (FieldName = '')  then  begin
            FieldName := 'F_' + IntToStr(i);
         end
         else begin
            if (FieldName = 'SEGMENT_ID_BEG') then FieldName := 'SEG_BEGIN';
            if (FieldName = 'SEGMENT_ID_END') then FieldName := 'SEG_END';
            if (FieldName = 'EASTING') then FieldName := 'X';
            if (FieldName = 'NORTHING') then FieldName := 'Y';
            if (FieldName = 'ELEVATION') then FieldName := 'Z';
            if (FieldName = 'LATITUDE') then FieldName := 'LAT';
            if (FieldName = 'LATITUDEN/S') then FieldName := 'LAT';
            if (FieldName = 'LONGITUDE') then FieldName := 'LONG';
            if (FieldName = 'LONGITUDEE/W') then FieldName := 'LONG';
            if (FieldName = 'LON') then FieldName := 'LONG';
            for j := length(FieldName) downto 1 do if not (FieldName[j] in DBaseFieldNameChars) then Delete(FieldName,j,1);
            if (FieldName = '')  then FieldName := 'F_' + IntToStr(i);
            for j := 1 to NumRules do if FieldName = OldNames[j] then FieldName := NewNames[j];
           if (FieldName[1] in ['0'..'9']) then FieldName := 'N_' + FieldName;
         end;
         LocalStringGrid.Cells[i,0] := FieldName;
         if (FieldName = RecNoFName) then RecIdExists := true;
     end;

     {$IfDef RecordCSV} WriteLineToDebugFile('Field names done'); {$EndIf}

     {$IfDef MICRODEM}
     if (SpecialGaz = csvNGAgaz) then begin
        {$IfDef RecordGAZ}
        WriteLineToDebugFile('NGA GAZ=' + fName +'output='+ OutputName);
        OutputName := DEMDefs.GazetteerDir + ExtractFileName(OutputName);
        {$EndIf}
     end;

     if (SpecialGaz = csvUSGSgaz) then begin
        {$IfDef RecordGAZ} WriteLineToDebugFile('USGS GAZ='+ fName + ' output='+ OutputName); {$EndIf}
     end;
     {$EndIf}

      CreateDataBase := tCreateDataBase.Create(Result);
      CreateDataBase.AddRecId := not RecIdExists;
        {$IfDef RecordCSV}
            WriteLineToDebugFile('Fields in Input file ' + fName  + 'NumFields= ' + IntToStr(LocalStringGrid.ColCount));
            for i := 0 to pred(LocalStringGrid.ColCount) do WriteLineToDebugFile(UpperCase(LocalStringGrid.Cells[i,0]));
        {$EndIf}

         BadFieldNames := 0;
         for i := 0 to pred(LocalStringGrid.ColCount) do begin
            TStr := UpperCase(LocalStringGrid.Cells[i,0]);
            if (Length(TStr) > 10) or (ptTrim(TStr) ='') then inc(BadFieldNames);
         end;

         if MDDef.AutoDBFieldNameFixes then AskAboutBadFieldNames := false
         else AskAboutBadFieldNames := (BadFieldNames > 0) and AnswerIsYes('Ask about bad field names');

         for i := 0 to 250 do ZeroPadLen[i] := 0;

          for i := 0 to pred(LocalStringGrid.ColCount) do begin
             {$IfDef RecordCSVParse} WriteLineToDebugFile(IntToStr(i) + '='+ UpperCase(LocalStringGrid.Cells[i,0])); {$EndIf}
             Len := 0;
             Decs := 0;
             IsString := false;
             IsFloat := false;
             IsLeadingZero := false;
             FieldName := LocalStringGrid.Cells[i,0];
             NeedTrimWarning := false;
             if (FieldName <> 'SKIP') then begin
                {$IfDef RecordCSVParse} WriteLineToDebugFile('Name check'); {$EndIf}
                while (Length(FieldName) > 10) or (ptTrim(FieldName) = '') do begin
                   if (Length(FieldName) > 10) then FieldName := Copy(FieldName,1,10);
                   if AskAboutBadFieldNames then GetString('Field name (< 10 chars)',FieldName,true,DBaseFieldNameChars);
                end;
                LocalStringGrid.Cells[i,0] := FieldName;

                if (FieldName = 'IP') then IsString := true;
                StartProgress('Field check ' + FieldName);
                for j := 1 to pred(LocalStringGrid.RowCount) do begin
                   if ((pred(j) mod 2000) = 0) then UpdateProgressBar(j/LocalStringGrid.RowCount);

                   TStr := ptTrim(LocalStringGrid.Cells[i,j]);
                   if (TStr <> '') then begin
                      if (TStr[1] = '"') then Delete(TStr,1,1);
                      if (Length(Tstr)> 0) and (TStr[Length(Tstr)] = '"') then Delete(TStr,length(TStr),1);
                   end;

                   if (FieldName = 'LAT') then begin
                       while System.Copy(TStr,1,2) = '00' do Delete(Tstr,1,1);
                       if (TStr <> '') and (TStr[1] = 'N') then Delete(TStr,1,1);
                       if (TStr <> '') and (TStr[1] = 'S') then TStr[1] := '-';
                       if (TStr <> '') then begin
                          if TStr[Length(Tstr)] = 'N' then Delete(TStr,length(TStr),1);
                          if TStr[Length(Tstr)] = 'S' then begin
                             Delete(TStr,length(TStr),1);
                             TStr := '-' + TStr;
                          end;
                       end;
                       LocalStringGrid.Cells[i,j] := TStr;
                       IsFloat := true;
                       IsString := false;
                       Len := 11;
                       Decs := 7;
                    end
                    else if (FieldName = 'LONG') then begin
                       while System.Copy(TStr,1,2) = '00' do Delete(Tstr,1,1);
                       if (TStr <> '') and (TStr[1] = 'E') then Delete(TStr,1,1);
                       if (TStr <> '') and (TStr[1] = 'W') then TStr[1] := '-';
                       if (TStr <> '') and (TStr[Length(Tstr)] = 'E') then Delete(TStr,length(TStr),1);
                       if (TStr <> '') and (TStr[Length(Tstr)] = 'W') then begin
                          Delete(TStr,length(TStr),1);
                          TStr := '-' + TStr;
                       end;
                       if (TStr <> '') then begin
                         LongV := StrToFloat(TStr);
                         PetMath.LongitudeAngleInRange(LongV);
                         LocalStringGrid.Cells[i,j] := RealToString(LongV,-12,-7);
                         IsFloat := true;
                         IsString := false;
                         Len := 12;
                         Decs := 7;
                       end;
                    end
                    else begin
                       if (Length(TStr) > 1) and (TStr[1] = '0') then IsLeadingZero := true;

                       if (TStr = '#DIV/0!') or (UpperCase(TStr) = 'NAN') or (UpperCase(TStr) = 'NO DATA') or (UpperCase(TStr) = 'NA') then begin
                          TStr := '';
                          LocalStringGrid.Cells[i,j] := '';
                       end
                       else if (TStr <> '') then begin
                          k := length(TStr);
                          if (k > 254) then begin
                             if NeedTrimWarning then MessageToContinue(FieldName + MessLineBreak + TStr + MessLineBreak + ' is too long (' + IntToStr(k) + ' chars; problems likely and will be truncated');
                             k := 254;
                             NeedTrimWarning := false;
                          end;
                          t2 := '-';
                          if (k > len) then len := k;
                          if IsNumeric(TStr) then begin
                              if MissingNumericString(TStr) then LocalStringGrid.Cells[i,j] := ''
                              else begin
                                 for n := 1 to k do begin
                                    if (TStr[n] = '.') then begin
                                       IsFloat := true;
                                       if (k-n > Decs) then Decs := k - n;
                                    end;
                                 end;
                              end;
                          end
                          else begin
                             IsString := true;
                          end;
                          if IsString then Decs := 0;
                       end;
                    end;
                 end;

                {$IfDef RecordCSVParse} WriteLineToDebugFile('Done field check'); {$EndIf}

               if FieldRequiresLeadingZeros(FieldName) or IsString or (IsLeadingZero and (not IsFloat)) then begin
                  if FieldRequiresLeadingZeros(FieldName) or IsLeadingZero then begin
                     ZeroPadLen[i] := len;
                  end;
                  if (FieldName = 'BIN_NAME') and (Len < 35) then Len := 35;
                  CreateDataBase.AddAField(FieldName,ftString,Len);
                  {$IfDef RecordCSVParse} WriteLineToDebugFile(FieldName + '  type = string,  len=' + IntToStr(len) + '  zeropad=' + IntToStr(ZeroPadLen[i])); {$EndIf}
               end
               else if IsFloat then begin
                  if (FieldName = 'LAT') or (FieldName = 'LONG') then begin
                     if (Len < 11) then Len := 11;
                     if (Decs < 7) then Decs := 7;
                  end
                  else if (FieldName = 'X_UTM') or (FieldName = 'Y_UTM') then begin
                     if (Len < 12) then Len := 12;
                     if (Decs < 2) then Decs := 2;
                  end
                  else Len := Len + 1;
                  CreateDataBase.AddAField(FieldName,ftFloat,Len,Decs);
                  {$IfDef RecordCSVParse} WriteLineToDebugFile(FieldName + '  type = float64'); {$EndIf}
               end
               else begin
                  if (Len >= 10) then begin
                     CreateDataBase.AddAField(FieldName,ftString,Len);
                     ZeroPadLen[i] := len;
                     {$IfDef RecordCSVParse} WriteLineToDebugFile(FieldName + '  type = string (because too long for int or field=zip)'); {$EndIf}
                  end
                  else begin
                     CreateDataBase.AddAField(FieldName,ftInteger,succ(Len));
                     {$IfDef RecordCSVParse} WriteLineToDebugFile(FieldName + '  type = integer, length=' + IntToStr(Len)); {$EndIf}
                  end;
               end;
            end;
         end;
         if not RecIdExists then CreateDataBase.AddAField(RecNoFName,ftInteger,9);

         CreateDataBase.WriteCorrectHeader(true);

      ShowHourglassCursor;
      wmdem.SetPanelText(0,'');

      {$IfDef RecordCSV} WriteLineToDebugFile('Batch mode point 1'); {$EndIf}

      if CreateDataBase.UsingNewDBF then CreateDataBase.NewDBF := tMyData.Create(CreateDataBase.FileName);

      if AllinStringGrid then begin
         {$IfDef RecordCSV} WriteLineToDebugFile('All in string grid'); {$EndIf}
         SendStringGridToDataBase(LocalStringGrid,CreateDataBase,ZeroPadLen);
      end
      else begin
         if ProcessInMemory then begin
            StartProgress('Convert');
            for j := 1 to pred(FileInMemory.Count) do begin
               if (j mod 100 = 0) then UpdateProgressBar(j/FileInMemory.Count);
               MenuStr := FileInMemory.Strings[j];
               processLine(MenuStr,j);
            end;
         end
         else begin
            ShowHourglassCursor;
            j := 0;
            Reset(tfile);
            readln(tfile);
            while not eof(tfile) do begin
               readln(tfile,MenuStr);
               inc(j);
               processLine(MenuStr,j);
            end;
            CloseFile(tFile);
         end;
         EndProgress;
      end;
      ShowHourglassCursor;
      LocalStringGrid.Free;
      CreateDataBase.Destroy;
      EndProgress;
      {$IfDef RecordProcessLine} MDdef.MDRecordDebugLog := SaveToDebugLog; {$EndIf}
end;


procedure SendStringGridToDataBase(StringGrid1 : tStringGrid; CreateDataBase : tCreateDataBase; ZeroPadLen : tZeroPadLen);
var
   i,j : integer;
   fv : shortString;
   TStr : Shortstring;
   RecordValues : tStringList;
begin
   {$IfDef RecordCSV} WriteLineToDebugFile('TCSVFileImportForm.BitBtn1Click import underway'); {$EndIf}
   StartProgress('Create DB ' + ExtractFileName(CreateDataBase.FileName));
   for j := 1 to pred(StringGrid1.RowCount) do begin
      if (j mod 250 = 0) then UpdateProgressBar(j / StringGrid1.RowCount);
      {$IfDef RecordCSV} if (J mod 500 = 0) then WriteLineToDebugFile('Record ' + IntToStr(j)); {$EndIf}
      RecordValues := tStringList.Create;
      for i := 0 to pred(StringGrid1.ColCount) do begin
         TStr := StringGrid1.Cells[i,0];
         if (TStr <> 'SKIP') then begin
            {$IfDef RecordFullCSV} WriteLineToDebugFile(TStr + ': '  + StringGrid1.Cells[i,j]); {$EndIf}
            if FieldRequiresLeadingZeros(TStr) then begin
               fv := ptTrim(StringGrid1.Cells[i,j]);
               while (length(fv) < ZeroPadLen[i]) do fv := '0' + fv;
               StringGrid1.Cells[i,j] := fv;
            end;
            RecordValues.Add(ptTrim(StringGrid1.Cells[i,j]));
         end;
      end;
      if (CreateDataBase.AddRecID) then RecordValues.Add(IntToStr(j));
      CreateDataBase.AddCorrectRecordFromStringList(RecordValues);
      RecordValues.Free;
      {$IfDef RecordFullCSV} WriteLineToDebugFile(''); {$EndIf}
   end;
   EndProgress;
   {$IfDef RecordCSV} WriteLineToDebugFile('TCSVFileImportForm.BitBtn1Click import done'); {$EndIf}
end;


function OrigPickField(Table : tMyData; Mess: ShortString; TypesAllowed : tSetFieldType) : ShortString;
var
  FieldsInDB : tStringList;
  WantField,i  : integer;
begin
   with Table do begin
      GetFields(Table,AllVis,TypesAllowed,FieldsInDB,false);
      if (FieldsInDB.Count = 0) then Result := ''
      else begin
         for i := pred(FieldsInDB.Count) downto 0 do
             if (UpperCase(FieldsInDB[i]) = 'USE') then FieldsInDB.Delete(i);
         if (FieldsInDB.Count = 1) then Result := FieldsInDB.Strings[0]
         else begin
            WantField := 0;
            if MultiSelectSingleColumnStringList('Database Field for ' + Mess,WantField,FieldsInDB,true) then begin
               Result := FieldsInDB.Strings[WantField];
            end
            else Result := '';
         end;
      end;
   end;
   FieldsInDB.Free;
end;


function StringGridToHTMLTable(StringGrid : tStringGrid) : AnsiString;
var
   i,J : integer;
   TStr : shortString;
begin
   {$IfDef RecordHTML} WriteLineToDebugFile('StringGridToHTMLTable in'); {$EndIf}
   Result := StartTableString + MessLineBreak;
   for i := 0 to pred(StringGrid.RowCount) do begin
      Result := Result + StartRowString;
      for j := 0 to pred(StringGrid.ColCount) do begin
         TStr := RemoveUnderScores(ptTrim(StringGrid.Cells[j,i]));
         if (TStr = '') then TStr := '&nbsp';
         Result := Result + HTMLTableCell(TStr);
      end;
      Result := Result + EndRowString  + MessLineBreak;
   end;
   Result := Result + EndTableString + MessLineBreak;
   ClipBrd.ClipBoard.AsText := Result;
end;


procedure StringGridToCSVFile(fName : PathStr; StringGrid : tStringGrid; Results : tStringList = Nil);
var
   i,J : integer;
   Line : string;
   ch : AnsiChar;
begin
   if (fName = '') then begin
      fName := MainMapData;
      Petmar.GetFileNameDefaultExt('CSV export','CSV file|*.CSV',fName);
      if (fName = '') then exit;
   end;

   ch := ',';
   if (Results = Nil) then Results := tStringList.Create;
   for i := 0 to pred(StringGrid.RowCount) do begin
      Line := '';
      for j := 0 to pred(StringGrid.ColCount) do Line := Line + StringGrid.Cells[j,i] + ch;
      Delete(Line,Length(Line),1);
      Results.Add(Line);
   end;
   Results.SaveToFile(fName);
   Results.Free;
end;


procedure HTMLReport(Title : shortString; StringGrid : tStringGrid; fName : PathStr = '');
var
   ReportSL : tStringList;
   Report : string;
begin
   {$IfDef RecordHTML} WriteLineToDebugFile('');  WriteLineToDebugFile('HTMLReport'); {$EndIf}
   if (fName = '') then begin
      fName := Petmar.NextFileNumber(MDTempDir, 'StringGrid_','.htm');
   end;
   Report := StartHTMLString + '<b>' + Title + '</b>' + StringGridToHTMLTable(StringGrid) + EndHTMLString;
   ClipBrd.ClipBoard.AsText := Report;
   {$IfDef RecordHTML} WriteLineToDebugFile(Report);  WriteLineToDebugFile(''); {$EndIf}
   ReportSL := tStringList.Create;
   ReportSL.Add(Report);
   ReportSL.SaveToFile(fName);
   ReportSL.Free;
   ExecuteFile(fname);
end;


procedure HTMLReport(Table : tMyData; Source : tDataSource; VisCols : Array100Boolean);
var
   ReportSL : tStringList;
   Report : string;
   fName : PathStr;
begin
   fName := Petmar.NextFileNumber(MDTempDir, ExtractFileNameNoExt(Table.TableName) + '_','.htm');
   Report := StartHTMLString + DBtableToHTML(Table,Source,VisCols) + EndHTMLString;
   ClipBrd.ClipBoard.AsText := Report;
   {$IfDef RecordHTML} WriteLineToDebugFile(Report);  WriteLineToDebugFile(''); {$EndIf}
   ReportSL := tStringList.Create;
   ReportSL.Add(Report);
   ReportSL.SaveToFile(fName);
   ReportSL.Free;
   ExecuteFile(fname);
end;


function DBtableToHTML(Table : tMyData; Source : tDataSource; VisCols : Array100Boolean) : AnsiString;
var
   j : integer;
   TStr : ShortString;
begin
   Result := StartTableString + StartRowString;
   for j := 0 to pred(Table.FieldCount) do begin
      if (VisCols[j]) then Result := Result + StartColumnString + Table.GetFieldName(j) + EndColumnString;
   end;
   Result := Result + EndRowString;
   Table.First;
   Source.Enabled := true;   //needed to get columns.visible
   repeat
      Result := Result + StartRowString;
      for j := 0 to pred(Table.FieldCount) do begin
         if (VisCols[j])  then begin
            if (Table.GetFieldName(j) = 'COLOR') then begin
               Result := Result + '<td style="background-color: ' + ColorToHtml(Table.GetFieldByNameAsInteger(Table.GetFieldName(j))) +'">' +  EndColumnString;
            end
            else begin
                TStr := ptTrim(Table.GetFieldByNameAsString(Table.GetFieldName(j)));
                if  (not ValidImageFileName(TStr)) and ValidImageFileName(ExtractFilePath(Table.FullTableName) + TStr) then TStr := ExtractFilePath(Table.FullTableName) + TStr;
                if ValidImageFileName(TStr) then begin
                   TStr := '<img border="0" width="400" height="300" src="' +TStr + '">';
                end
                else if (TStr = '') then TStr := '&nbsp';
                Result := Result + StartColumnString + TStr + EndColumnString;
            end;
         end {if};
      end {for j};
      Result := Result + EndRowString;
      Table.Next;
   until Table.EOF;
   Result := Result + EndTableString;
end;

{$EndIf}


var
   i : integer;
initialization
   {$IfDef MessageStartupUnit} MessageToContinue('Startup petdbutils'); {$EndIf}
   for i := 0 to MaxFieldsInDB do AllVis[i] := true;
   OutsideCSVImport := false;
   ForceAllInStringGrid := false;
   WeKnowTheHeader := false;
finalization
end.



















