
procedure tMyData.ApplyFilter(TheFilter: ANSIstring);
begin
   Filtered := (TheFilter <> '');
   //if Filtered then begin
      Filter := TheFilter;
      if (TheData <> Nil) then begin
         if MDDef.DBfilterCaseInSensitive then TheData.FilterOptions := [foCaseInsensitive]
         else TheData.FilterOptions := [];
         TheData.Filter := TheFilter;
         {$IfDef UseTDBF}
         {$Else}
         TheData.Filtered := true;
         {$EndIf}
      end
      else if (TheClientDataSet <> Nil) then begin
         if MDDef.DBfilterCaseInSensitive then TheClientDataSet.FilterOptions := [foCaseInsensitive]
         else TheClientDataSet.FilterOptions := [];
         TheClientDataSet.Filter := TheFilter;
         TheClientDataSet.Filtered := true;
      end;
   //end;
end;


function tMyData.ColorFromRGBinTable : tColor;
begin
   if FieldExists('COLOR') then  Result := GetFieldByNameAsInteger('COLOR')
   else Result := RGB(GetFieldByNameAsInteger('RED'),GetFieldByNameAsInteger('GREEN'),GetFieldByNameAsInteger('BLUE'));
end;


procedure tMyData.First;
begin
   if (TheData <> Nil) then TheData.First
   else if (TheClientDataSet <> Nil) then TheClientDataSet.First;
end;

function tMyData.GetFieldName(i : integer) : string12;
begin
   if (TheData <> Nil) then Result := TheData.Fields[i].FieldName
   else if (TheClientDataSet <> Nil) then Result := TheClientDataSet.Fields[i].FieldName;
end;


function tMyData.GetFieldDataSize(I : integer) : integer;
var
   f : file;
   DBaseIIITableFileHeader : tDBaseIIITableFileHeader;
   TableFieldDescriptor : tTableFieldDescriptor;
   NumFields,i2 : integer;
begin
   if (TheData <> Nil) then begin
      Result := 0;
      assignFile(f,TableName);
      reset(f,1);
      BlockRead(f, DBaseIIITableFileHeader, SizeOf(tDBaseIIITableFileHeader));
      NumFields := (DBaseIIITableFileHeader.BytesInHeader-33) div 32;
      for i2 := 0 to i do BlockRead(f, TableFieldDescriptor, SizeOf(tTableFieldDescriptor));
      Result := TableFieldDescriptor.FieldLength;
      //writelineToDebugFile(IntToStr(i) + '=' + IntToStr(Result));
      CloseFile(f);
   end
   else if (TheClientDataSet <> Nil) then Result := TheClientDataSet.Fields[i].DataSize;
end;


constructor tMyData.Create(fName: PathStr);
var
   Ext : ExtStr;
begin
    {$IfDef RecordMYDataCreation}
    WriteLineToDebugFile('tMyData.Create ' + fName);
    {$EndIf}
    Ext := UpperCase(ExtractFileExt(fName));
    TableName := fName;
    if (Ext = '.XML') then begin
        TheClientDataSet := tClientDataSet.Create(Application);
        TheClientDataSet.LoadFromFile(FName);
        TheClientDataSet.LogChanges := false;
    end
    else if (Ext = '.DBF') or (Ext = '.SHP') then begin
       CreateAndOpenTable(Self.TheData,ChangeFileExt(fName,'.dbf'));
       //TheData.Open;
    end;
    ApplyFilter('');
    //Filtered := false;
end;


destructor tMyData.Destroy;
begin
   {$IfDef RecordMYDataCreation}
   WriteLineToDebugFile('tMyData.Destroy ' + TableName);
   {$EndIf}
   if (TheData <> Nil) then TheData.Destroy;
   if (TheClientDataSet <> Nil) then TheClientDataSet.Destroy;
end;


procedure tMyData.DeleteCurrentRecord;
begin
   if (TheData <> Nil) then TheData.Delete
   else if (TheClientDataSet <> Nil) then TheClientDataSet.Delete;
end;


procedure tMyData.Edit;
begin
   if (TheData <> Nil) then TheData.Edit
   else if (TheClientDataSet <> Nil) then TheClientDataSet.Edit;
end;

procedure tMyData.Post;
begin
   if (TheData <> Nil) then TheData.Post
   else if (TheClientDataSet <> Nil) then TheClientDataSet.Post;
end;

procedure tMyData.Insert;
begin
   if (TheData <> Nil) then TheData.Insert
   else if (TheClientDataSet <> Nil) then TheClientDataSet.Insert;
end;

procedure tMyData.Delete;
begin
   if (TheData <> Nil) then TheData.Delete
   else if (TheClientDataSet <> Nil) then TheClientDataSet.Delete;
end;

procedure tMyData.Prior;
begin
   if (TheData <> Nil) then TheData.Prior
   else if (TheClientDataSet <> Nil) then TheClientDataSet.Prior;
end;


procedure tMyData.Next;
begin
   if (TheData <> Nil) then TheData.Next
   else if (TheClientDataSet <> Nil) then TheClientDataSet.Next;
end;

procedure tMyData.Last;
begin
   if (TheData <> Nil) then TheData.Last
   else if (TheClientDataSet <> Nil) then TheClientDataSet.Last;
end;

function tMyData.EOF;
begin
   if (TheData <> Nil) then Result := TheData.Eof
   else if (TheClientDataSet <> Nil) then Result := TheClientDataSet.Eof;
end;

function tMyData.BOF;
begin
   if (TheData <> Nil) then Result := TheData.Bof
   else if (TheClientDataSet <> Nil) then Result := TheClientDataSet.Bof;
end;


function tMyData.RecNo : integer;
begin
   if (TheData <> Nil) then Result := TheData.RecNo
   else if (TheClientDataSet <> Nil) then Result := TheClientDataSet.RecNo;
end;


function tMyData.GetFieldByNameAsFloat(FieldName : string12) : float;
begin
   if (TheData <> Nil) then Result := TheData.FieldByName(FieldName).AsFloat
   else if (TheClientDataSet <> Nil) then Result := TheClientDataSet.FieldByName(FieldName).AsFloat;
end;


function tMyData.CarefullyGetFieldByNameAsFloat(FieldName : string12; var Value : float) : boolean;
var
   TStr : ShortString;
begin
   Tstr := GetFieldByNameAsString(FieldName);
   Result := (TStr <> '');
   if Result then Value := StrToFloat(TStr);
end;


function tMyData.CarefullyGetFieldByNameAsInteger(FieldName : string12; var Value : integer) : boolean;
var
   TStr : ShortString;
begin
   Tstr := GetFieldByNameAsString(FieldName);
   Result := (TStr <> '');
   if Result then Value := StrToInt(TStr);
end;


function tMyData.GetFieldByNameAsInteger(FieldName : string12) : integer;
begin
   if (TheData <> Nil) then Result := TheData.FieldByName(FieldName).AsInteger
   else if (TheClientDataSet <> Nil) then Result := TheClientDataSet.FieldByName(FieldName).AsInteger;
end;


function tMyData.GetFieldByNameAsString(FieldName : string12) : ShortString;
begin
   if (TheData <> Nil) then Result := TheData.FieldByName(FieldName).AsString
   else if (TheClientDataSet <> Nil) then Result := TheClientDataSet.FieldByName(FieldName).AsString;
end;

procedure tMyData.SetFieldByNameAsFloat(FieldName : string12; value : float);
begin
   if (TheData <> Nil) then TheData.FieldByName(FieldName).AsFloat := value
   else if (TheClientDataSet <> Nil) then TheClientDataSet.FieldByName(FieldName).AsFloat := value;
end;

procedure tMyData.CarefullySetFloat(FieldName : string12; var Value : float; Precision : float);  //inline;
begin
   if (abs(Value) < Precision) then Value := 0;
   SetFieldByNameAsFloat(FieldName,value);
end;


procedure tMyData.SetFieldByNameAsString(FieldName : string12; value : shortString);
begin
   if (TheData <> Nil) then TheData.FieldByName(FieldName).AsString := value
   else if (TheClientDataSet <> Nil) then TheClientDataSet.FieldByName(FieldName).AsString := value;
end;


procedure tMyData.SetFieldByNameAsInteger(FieldName : string12; value : integer);
begin
   if (TheData <> Nil) then TheData.FieldByName(FieldName).AsInteger := value
   else if (TheClientDataSet <> Nil) then TheClientDataSet.FieldByName(FieldName).AsInteger := value;
end;


function tMyData.UniqueEntriesInDB(FieldName : string12) : tStringList;
begin
   if (TheData <> Nil) or (TheClientDataSet <> Nil) then FindUniqueEntries(Self,FieldName,Result)
   else Result := TStringList.Create;
end;

function tMyData.RecordCount : integer;
begin
   Result := 0;
   if (TheData <> Nil) then Result := TheData.RecordCount
   else if (TheClientDataSet <> Nil) then Result := TheClientDataSet.RecordCount;
end;

procedure tMyData.AssignEmpSource(EmpSource : TDataSource);
begin
   if (TheData <> Nil) then EmpSource.DataSet := TheData
   else if (TheClientDataSet <> Nil) then EmpSource.DataSet := TheClientDataSet;
end;


function tMyData.GetFieldPrecision(fName : string35) : integer;
var
   f : file;
   DBaseIIITableFileHeader : tDBaseIIITableFileHeader;
   TableFieldDescriptor : tTableFieldDescriptor;
   NumFields,i,j : integer;
   theName : string12;
begin
   if (TheData <> Nil) then with TheData do begin
      Result := 0;
      assignFile(f,TableName);
      reset(f,1);
      BlockRead(f, DBaseIIITableFileHeader, SizeOf(tDBaseIIITableFileHeader));
      NumFields := (DBaseIIITableFileHeader.BytesInHeader-33) div 32;
      for i := 0 to pred(NumFields) do begin
        BlockRead(f, TableFieldDescriptor, SizeOf(tTableFieldDescriptor));
        theName := '';
        for j := 0 to 10 do if TableFieldDescriptor.FieldName[j] <> #0 then theName := theName +  TableFieldDescriptor.FieldName[j];

        if (theName = fName) then  begin
           Result := TableFieldDescriptor.FieldDecimalCount;
           Break;
        end;
     end;
     CloseFile(f);
   end
   else Result := 0;
end;


function tMyData.GetFieldPrecision(i : integer) : integer;
var
   f : file;
   DBaseIIITableFileHeader : tDBaseIIITableFileHeader;
   TableFieldDescriptor : tTableFieldDescriptor;
   NumFields,i2,j : integer;
   theName : string12;
begin
   if (TheData <> Nil) then with TheData do begin
      Result := 0;
      assignFile(f,TableName);
      reset(f,1);
      BlockRead(f, DBaseIIITableFileHeader, SizeOf(tDBaseIIITableFileHeader));
      NumFields := (DBaseIIITableFileHeader.BytesInHeader-33) div 32;
      for i2 := 0 to i do begin
        BlockRead(f, TableFieldDescriptor, SizeOf(tTableFieldDescriptor));
      end;
      Result := TableFieldDescriptor.FieldDecimalCount;
     CloseFile(f);
   end
   else Result := 0;
end;


function tMyData.GetFieldType(fName : string35) : tFieldType;
var
   i : integer;
begin
   if (TheData <> Nil) then with TheData do begin
      for i := 0 to pred(FieldCount) do begin
         if (Fields[i].FieldName = fName) then begin
            Result := Fields[i].DataType;
            exit;
         end;
      end;
   end
   else if (TheClientDataSet <> Nil) then with TheClientDataSet do begin
      for i := 0 to pred(FieldCount) do begin
         if (Fields[i].FieldName = fName) then begin
            Result := Fields[i].DataType;
            exit;
         end;
      end;
   end;
end;


function tMyData.GetFieldType(i : integer) : tFieldType;
begin
   if (TheData <> Nil) then with TheData do begin
      Result := Fields[i].DataType;
   end
   else if (TheClientDataSet <> Nil) then with TheClientDataSet do begin
      Result := Fields[i].DataType;
   end;
end;


function tMyData.FieldCount : integer;
begin
   if (TheData <> Nil) then with TheData do begin
     Result := FieldCount;
   end
   else if (TheClientDataSet <> Nil) then with TheClientDataSet do begin
     Result := FieldCount;
   end;
end;

function tMyData.FieldsInDataBase : tStringList;
var
   i : integer;
begin
   Result := tStringList.Create;
   if (TheData <> Nil) then with TheData do begin
      for i := 0 to pred(FieldCount) do Result.Add(Fields[i].FieldName);
   end
   else if (TheClientDataSet <> Nil) then with TheClientDataSet do begin
      for i := 0 to pred(FieldCount) do Result.Add(Fields[i].FieldName);
   end;
end;


function tMyData.GetFieldLength(WantFieldName : string12) : integer;
var
     i : integer;
begin
   Result := -1;
   if (TheData <> Nil) then with TheData do begin
         for i := 0 to pred(FieldCount) do begin
            if (Fields[i].FieldName = WantFieldName) then begin
               Result := Fields[i].DataSize;
               exit;
            end;
         end;
   end
   else if (TheClientDataSet <> Nil) then with TheClientDataSet do begin
         for i := 0 to pred(FieldCount) do begin
            if (Fields[i].FieldName = WantFieldName) then begin
               Result := Fields[i].DataSize;
               exit;
            end;
         end;
   end;
end;


function tMyData.FieldTypeAndLength(WantFieldName : string12) : shortstring;
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
      ftFloat : Result := 'Float';
      else Result := 'Other';
   end;
   Result := Result + ' (' + IntToStr(i) + ')';
end;


function tMyData.FieldExists(WantFieldName : string12; NeedAType : boolean = false; NeedType : tFieldType = ftFloat) : boolean;
var
   i : integer;
begin
   Result := false;
   if (TheData <> Nil) then with TheData do begin
      for i := 0 to pred(FieldCount) do if (Fields[i].FieldName = WantFieldName) then begin
         if NeedAType then begin
            if (Fields[i].DataType = NeedType) then begin
               Result := true;
               exit;
            end;
         end
         else begin
            Result := true;
            exit;
         end;
      end;
   end
   else if (TheClientDataSet <> Nil) then with TheClientDataSet do begin
      for i := 0 to pred(FieldCount) do if (Fields[i].FieldName = WantFieldName) then begin
         if NeedAType then begin
            if (Fields[i].DataType = NeedType) then begin
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


function tMyData.FieldSum(FieldDesired : string35) : float;
var
   z : float;
begin
   First;
   repeat
      if CarefullyGetFieldByNameAsFloat(FieldDesired,z) then Result := Result + z;
      Next;
   until EOF;
end;


function tMyData.IsNumericField(WantFieldName : string12) : boolean;
begin
    Result := GetFieldType(WantFieldName) in [ftFloat, ftInteger, ftSmallInt];
end;

function tMyData.IsFloatField(WantFieldName : string12) : boolean;
begin
    Result := GetFieldType(WantFieldName) in [ftFloat];
end;

function tMyData.IsIntegerField(WantFieldName : string12) : boolean;
begin
    Result := GetFieldType(WantFieldName) in [ftInteger,ftSmallInt];
end;

function tMyData.IsStringField(WantFieldName : string12) : boolean;
begin
    Result := GetFieldType(WantFieldName) in [ftString];
end;

procedure tMyData.DefineFontFromTable(Font : tFont);
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

procedure tMyData.PostFont(aFont : tFont);
var
   ch : AnsiChar;
begin
   Edit;
   SetFieldByNameAsString('FONT_NAME',aFont.Name);
   SetFieldByNameAsInteger('FONT_SIZE',aFont.Size);
   SetFieldByNameAsInteger('FONT_COLOR',aFont.Color);
   if fsBold in aFont.Style then ch := 'B' else ch := ' ';
   SetFieldByNameAsString('FONT_BOLD',ch);
   if fsItalic in aFont.Style then ch := 'I' else ch := ' ';
   SetFieldByNameAsString('FONT_ITAL',ch);
   if fsUnderline in aFont.Style then ch := 'U' else ch := ' ';
   SetFieldByNameAsString('FONT_UNDER',ch);
end;

procedure tMyData.DefinePointSymbol(var Symbol : tDrawingSymbol; var SymbolSize : byte; var SymbolColor : tColor);
begin
   if GetFieldByNameAsString('SYM_TYPE') <> '' then begin
      Symbol := tDrawingSymbol(GetFieldByNameAsInteger('SYM_TYPE'));
      SymbolSize := GetFieldByNameAsInteger('SYM_SIZE');
      SymbolColor := GetFieldByNameAsInteger('SYM_COLOR');
   end;
end;


procedure tMyData.PostPointSymbol(Symbol : tDrawingSymbol; SymbolSize : byte; SymbolColor : tColor);
begin
    SetFieldByNameAsInteger('SYM_TYPE',ord(Symbol));
    SetFieldByNameAsInteger('SYM_SIZE',SymbolSize);
    SetFieldByNameAsInteger('SYM_COLOR',SymbolColor);
end;



function tMyData.InsureFieldPresentAndAdded(ft : TFieldType; FieldName : string12; Length : integer; Decimals : integer = 0) : boolean;
{$IfDef ExDBCreate}
begin
{$Else}
var
   fName,OldName,WorkingName : PathStr;
   aName : string12;
   i,fLen : integer;
   DbaForm : TDbaForm;
   NewClientDataSet :  tClientDataSet;
begin
   {$IfDef RecordFieldPresentProblems}
   WriteLineToDebugFile('InsureFieldPresentAndAdded, checking: ' + FieldName);
   {$EndIf}
   FieldName := UpperCase(FieldName);
   if FieldExists(FieldName) then Result := false
   else begin
      fName := ChangeFileExt(TableName,'.dbf');
      if (TheData <> Nil) then begin
         {$IfDef RecordFieldPresentProblems}
         WriteLineToDebugFile('have to add field ' + FieldName + ' to ' + fName);
         {$EndIf}
         Result := true;

         WorkingName := Petmar.NextFileNumber(ExtractFilePath(fname) +'new_working_','.dbf');
         {$IfDef RecordFieldPresentProblems}
         WriteLineToDebugFile('wname= ' + WorkingName);
         {$EndIf}
         CreateNewDBaseFile(WorkingName, DbaForm);
         for i := 0 to pred(FieldCount) do begin
            AddFieldToTable(dbaForm.Table1, GetFieldName(i), GetFieldType(i), GetFieldDataSize(I), GetFieldPrecision(i) );
         end;
         AddFieldToTable(dbaForm.Table1, FieldName, ft, Length, Decimals );
         CloseNewDBaseFile(DbaForm);
         TheData.Destroy;
         TheData := Nil;
         {$IfDef RecordFieldPresentProblems}
         WriteLineToDebugFile('added');
         {$EndIf}

         CopyDBTable(fName,WorkingName);
         Application.ProcessMessages;
         OldName := Petmar.NextFileNumber(ExtractFilePath(fname) +'old_file_','.dbf');
         {$IfDef RecordFieldPresentProblems}
         WriteLineToDebugFile('oldname= ' + WorkingName);
         {$EndIf}
         SysUtils.RenameFile(fName,OldName);
         Application.ProcessMessages;

         SysUtils.RenameFile(WorkingName,fName);
         Application.ProcessMessages;
         {$IfDef RecordFieldPresentProblems}
         WriteLineToDebugFile('ready to reopen');
         {$EndIf}
         CreateAndOpenTable(TheData,fName);
      end
      else if (TheClientDataSet <> Nil) then begin
          NewClientDataSet := tClientDataSet.Create(application);
          for i := 0 to pred(FieldCount) do begin
             ft := GetFieldType(i);
             if (ft in [ftString]) then fLen := GetFieldDataSize(i)
             else fLen := 0;
             NewClientDataSet.FieldDefs.Add(GetFieldName(i), ft, fLen,False );
          end;
          NewClientDataSet.FieldDefs.Add(FieldName,ft,Length, False);
          NewClientDataSet.CreateDataset;
          NewClientDataSet.Open;

          TheClientDataSet.First;
          while not TheClientDataSet.eof do begin
             NewClientDataSet.Insert;
             for i := 0 to pred(FieldCount) do begin
                aName := GetFieldName(i);
                NewClientDataSet.FieldByName(aName).AsString := theClientDataSet.FieldByName(aName).AsString;
             end;
             TheClientDataSet.Next;
          end;
          theClientDataset.Free;
          NewClientDataSet.SaveToFile(TableName);
          NewClientDataSet.Free;
//          ClientDataSet.SaveToFile(fName);
          TheClientDataSet := tClientDataSet.Create(Application);
          TheClientDataSet.LoadFromFile(TableName);
          TheClientDataSet.LogChanges := false;
       end;
   end;

   {$IfDef RecordFieldPresentProblems}
   WriteLineToDebugFile('InsureFieldPresentAndAdded out');
   {$EndIf}
{$EndIf}
end;


function tMyData.GetTableStructure : tStringList;
var
   i : integer;
   TStr,TStr2 : ShortString;
begin
   Result := tStringList.Create;
   //if (TheData <> Nil) then with TheData do begin
      for i := 0 to pred(FieldCount) do begin
         case GetFieldType(i) of
            ftString : TStr := 'string';
            ftFloat  : TStr := 'float';
            ftInteger : TStr := 'integer';
            ftDate : TStr := 'date';
            ftSmallInt : TStr := 'small integer';
            ftBlob : TStr := 'blob';
            ftMemo : TStr := 'memo';
            else begin
               TStr := 'other';
            end;
         end;
         if (GetFieldType(i) = ftFloat) then begin
            TStr2 := '  ' + IntToStr(GetFieldPrecision(i)) + ' decimals';
         end
         else TStr2 := '';

         Result.Add('   ' + GetFieldName(i) + '   ' + TStr + '  ' + IntToStr(GetFieldDataSize(i)) + TStr2);
      end;
   (*
   end;
   if (TheClientDataSet <> Nil) then begin
      Result.Add('Currently not implemented');
   end;
   *)
end;


