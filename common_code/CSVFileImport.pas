unit CSVFileImport;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordGAZProblems}
   //{$Define RecordCSVProblems}
   //{$Define RecordKMLProblems}
   //{$Define RecordCSVParseProblems}
   //{$Define RecordProcessLineProblems}  //major slowdown
   //{$Define RecordFullCSVProblems}      //major slowdown
{$EndIf}


interface

uses
//needed for inline of the core DB functions
   Data.DB,

   {$IfDef UseFireDacSQLlite}
   FireDAC.Stan.ExprFuncs,
   FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
   FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
   FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
   FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Comp.Client, FireDAC.Comp.DataSet,
   FireDAC.Phys.SQLite, FireDAC.Comp.UI,   //FireDAC.VCLUI.Wait,
   {$EndIf}

   {$IfDef UseBDETables}
   dbTables,
   {$EndIf}

   {$IfDef UseTDBF}
   dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
   DBClient,
   {$EndIf}
//end core DB functions definitions

  Vcl.ExtCtrls, Vcl.ComCtrls,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Grids,StrUtils,  StdCtrls, Buttons,

  DataBaseCreate,Petmar_types,PETMAR;

type
    tZeroPadLen = array[1..250] of integer;


type
  TCSVFileImportForm = class(TForm)
    StringGrid1: TStringGrid;
    ProgressBar1: TProgressBar;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    CheckBox2: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
  private
    { Private declarations }
    CreateDataBase : tCreateDataBase;
  public
    { Public declarations }
    OutPutName : PathStr;
    ZeroPadLen : tZeroPadLen;
  end;

type
   tCSVImport = (csvNormal,csvNGAGaz,csvUSGSgaz);

function DoCSVFileImport(fName : PathStr  = ''; SpecialGaz : tCSVImport = csvNormal) : PathStr;
function PointKMLtoStringList(fName : PathStr) : tStringList;
procedure SendStringGridToDataBase(StringGrid1 : tStringGrid; CreateDataBase : tCreateDataBase; ZeroPadLen : tZeroPadLen);


implementation

{$R *.dfm}

uses
   demdef_routines, demdefs,
   PetDBUtils,
   Nevadia_main, PETMath;

const
   BaseInputFile : PathStr = '';
//var
   //CSVFileImportForm : TCSVFileImportForm;


Function IsNumeric(s: AnsiString) : Boolean;
//from http://www.efg2.com/Lab/Library/Delphi/MathFunctions/General.htm
var
   Code : Integer;
   Value : float;
BEGIN
   val(s, Value, Code);
   Result := (Code = 0);
END;


function PointKMLtoStringList(fName : PathStr) : tStringList;
var
   FileInMemory : tStringList;
   NumIcons,
   i,j : integer;
   Str : String;
   NameStr,LatStr,LongStr,IconStr : string;
   Adding,DoingIcon : boolean;
   IconNames : array[1..2,1..25] of string35;
begin
   ShowHourglassCursor;
   FileInMemory := tStringList.Create;
   FileInMemory.LoadFromFile(fName);

   Result := tStringList.Create;
   Result.Add('LAT,LONG,NAME,ICON');

    Adding := false;
    DoingIcon := false;
    NumIcons := 0;
    for I := 0 to pred(FileInMemory.count) do begin
       Str := UpperCase(ptTrim(UpperCase(FileInMemory.Strings[i])));
       if copy(str,1,9) = '<STYLE ID' then begin
           inc(NumIcons);
           DoingIcon := true;
           Petmar_Types.BeforeSpecifiedCharacter(Str,'"',true,true);
           Str := Petmar_Types.BeforeSpecifiedCharacter(Str,'"');
           {$IfDef RecordKMLProblems}
           WriteLineToDebugFile('Icon=' + IntToStr(NumIcons) + '  ' + Str);
           {$EndIf}
           IconNames[1,NumIcons] := Str;
       end;
       if DoingIcon then begin
           if copy(str,1,6) = '<HREF>' then begin
              Delete(Str,1,6);
              Str := Petmar_Types.BeforeSpecifiedCharacter(Str,'<');
              {$IfDef RecordKMLProblems}
              WriteLineToDebugFile('    ' + Str);
              {$EndIf}
              IconNames[2,NumIcons] := Str;
           end;
           if copy(str,1,7) = '</STYLE' then DoingIcon := false;
       end;
       if copy(str,1,10) = '<PLACEMARK' then begin
           Adding := true;
           DoingIcon := false;
           Delete(Str,1,10);
       end;
       if Adding then begin
           if Copy(Str,1,6) = '<NAME>' then begin
              Delete(Str,1,6);
              NameStr := Petmar_Types.BeforeSpecifiedCharacter(Str,'<');
              if (NameStr = 'UNTITLED PLACEMARK') then NameStr := 'Placemark';
           end;
           if Copy(Str,1,10) = '<STYLEURL>' then begin
              Petmar_Types.BeforeSpecifiedCharacter(Str,'#',true,true);
              str := Petmar_Types.BeforeSpecifiedCharacter(Str,'<',true,true);
              for j := 1 to 25 do begin
                 if str = IconNames[1,j] then begin
                    IconStr := IconNames[2,j];
                    break;
                 end;
              end;
           end;
           if (Copy(Str,1,13)) = '<COORDINATES>' then begin
              Delete(Str,1,13);
              LongStr := Petmar_Types.BeforeSpecifiedCharacter(Str,',',true,true);
              LatStr := Petmar_Types.BeforeSpecifiedCharacter(Str,',',true,true);
           end;
           if str = '</PLACEMARK>' then begin
              Adding := false;
              Result.Add(LatStr + ',' + LongStr + ',' + NameStr + ',' + IconStr);
           end;
        end;
      end;
   FileInMemory.Destroy;
end;


function FieldRequiresLeadingZeros(FieldName : ShortString) : boolean;
begin
   Result := StrUtils.AnsiContainsText(FieldName,'ZIP') or
             StrUtils.AnsiContainsText(FieldName,'FIPS') or
             StrUtils.AnsiContainsText(FieldName,'GEOID')
end;

function DoCSVFileImport(fName : PathStr  = ''; SpecialGaz : tCSVImport = csvNormal) : PathStr;
var
   CreateDataBase : tCreateDataBase;
   OutPutName : PathStr;
   ZeroPadLen : tZeroPadLen;
   FileInMemory : tStringList;
   RecordValues : tStringList;
   LinesToMoveToStringGrid,NumDupesIgnored,
   NumRules,
   OnLine,i,j,k,n,Len,Decs,BadFieldNames : integer;
   LongV : float;
   SepChar : char;
   SepCharANSI : ANSIChar;
   AskAboutBadFieldNames,
   IsString,IsFloat,IsLeadingZero,BatchMode : boolean;
   TStr,Str,MenuStr,LastLine : ANSIString;
   t2,aStr : shortstring;
   tFile : System.Text;
   NeedTrimWarning,
   ProcessInMemory,AllInStringGrid : boolean;
   Fieldname : shortstring;
   LocalStringGrid : tStringGrid;
   OldNames,NewNames : array[1..250] of string35;


            procedure ProcessLine(MenuStr : ANSIstring);
           {$IfDef RecordProcessLineProblems}
           const
              RecsDone : integer = 0;
           {$EndIf}
            var
               i : integer;
            begin
              {$IfDef RecordProcessLineProblems}
              inc(RecsDone);
              WriteLineToDebugFile('Recs=' + IntToStr(RecsDone));
              if RecsDone > 100 then MDdef.MDRecordDebugLog := false;
              {$EndIf}
               RecordValues := tStringList.Create;
               for i := 0 to pred(LocalStringGrid.ColCount) do begin
                  fName := ptTrim(LocalStringGrid.Cells[i,0]);
                  TStr := Petmar_types.BeforeSpecifiedCharacterANSI(MenuStr,SepCharANSI,true,true);
                  {$IfDef RecordProcessLineProblems}
                  WriteLineToDebugFile('fName=' + fName + '   TStr=' + TStr);
                  {$EndIf}
                  if (fName <> 'SKIP') then begin
                     if FieldRequiresLeadingZeros(fName) then begin
                        while (Length(TStr) < ZeroPadLen[i]) do TStr := '0' + TStr;
                     end;
                     RecordValues.Add(ptTrim(TStr));
                     {$IfDef RecordProcessLineProblems}
                     WriteLineToDebugFile('added TStr=' + TStr);
                     {$EndIf}
                  end;
               end;
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
  {$IfDef RecordCSVProblems}
  WriteLineToDebugFile('DoCSVFileImport enter');
  {$EndIf}
   if (FName = '') then begin
      if BaseInputFile  = '' then BaseInputFile := ProgramRootDir;
      fName := BaseInputFile;
      if not GetFileFromDirectory('CSV file','*.txt;*.csv;*.KML',fName) then exit;
      BaseInputFile := fName;
      BatchMode := false;
   end
   else BatchMode := true;

   wmdem.SetPanelText(0,'Make ' + ExtractFileName(fName));
     if BatchMode then begin
        LocalStringGrid := tStringGrid.Create(Application);
     end
     else begin
        (*
        CSVFileImportForm := TCSVFileImportForm.Create(Application);
        CSVFileImportForm.Caption := 'CSV import ' + ExtractFileName(FName);
        CSVFileImportForm.Show;
        LocalStringGrid := CSVFileImportForm.StringGrid1;
        *)
     end;

     //ShowModal;

     ShowHourglassCursor;
     ProcessInMemory := (Petmar.GetFileSize(fName) < (InMemoryStringSizeLimit * 80));

     FileInMemory := tStringList.Create;
     if ProcessInMemory then begin
        FileInMemory.LoadFromFile(fName);
     end
     else begin
        AssignFile(tfile,fName);
        reset(tfile);
        for I := 0 to 25000 do begin
           readln(tfile,LastLine);
           if (LastLine <> '') then FileInMemory.Add(LastLine);
        end;
     end;

     {$IfDef RecordCSVProblems}
     WriteLineToDebugFile('CSV file import: ' + fname,true);
     WriteLineToDebugFile(' Records: ' + IntToStr(FileInMemory.Count));
     k := 10;
     if (FileInMemory.Count < 10) then k := pred(FileInMemory.Count);
     for j := 0 to pred(k) do WriteLineToDebugFile('---' +  FileInMemory.Strings[j]);
     {$EndIf}

     Str := FileInMemory.Strings[0];
     for k := Length(Str) downto 1 do
        if Str[k] in ['0'..'9','-','.',' ',',',#9] then Delete(Str,k,1);
     if (Length(Str) = 0) and AnswerIsYes('Enter field names') then begin
        aStr := FileInMemory.Strings[0];
        Petmar.GetString('Headers for ' + aStr,MDDef.CSVfileHeaders,true,DBaseFieldNameChars + [' ',',']);
        FileInMemory.Insert(0,MDDef.CSVfileHeaders);
     end;

     LocalStringGrid.RowCount := FileInMemory.Count;
     MenuStr := ptTrim(FileInMemory.Strings[0]);
     GetSeparationCharacter(MenuStr,SepCharANSI);
     //SepChar := SepCharANSI;

     {$IfDef RecordCSVProblems}
     WriteLineToDebugFile('SepChar=' + SepChar);
     {$EndIf}

     if ProcessInMemory then begin
        AllInStringGrid := (FileInMemory.Count < 250000);
        if not AllInStringGrid then begin
           AllInStringGrid := AnswerIsYes('Use string grid (lines=' + IntToStr(FileInMemory.Count) + ')');
        end;
     end
     else AllInStringGrid := false;

     if AllInStringGrid then LinesToMoveToStringGrid := pred(FileInMemory.Count)
     else begin
         if ProcessInMemory then LinesToMoveToStringGrid := 250000
         else LinesToMoveToStringGrid := pred(FileInMemory.Count);
     end;
     NumDupesIgnored := 0;

     if (MenuStr[1] = '"') then begin
        i := 0;
        for j := 1 to length(MenuStr) do if MenuStr[j] = '"' then inc(i);
        i := i div 2;
        LocalStringGrid.ColCount := i;
        for i := 0 to pred(FileInMemory.Count) do begin
           //CSVFileImportForm.ProgressBar1.Position := round(100 * i/FileInMemory.Count);
           MenuStr := ptTrim(FileInMemory.Strings[i]);
           for j := 0 to pred(LocalStringGrid.ColCount) do begin
              Delete(MenuStr,1,1);  //leading "
              LocalStringGrid.Cells[j,i] := ptTrim(BeforeSpecifiedCharacterANSI(MenuStr,'"',false,true));
              if length(MenuStr) > 0 then Delete(MenuStr,1,1);  // comma
           end;
        end;
     end
     else if (SepChar in [#9,',',' ','|',';']) then begin
        ptTrim(MenuStr);
        if (SepChar = ' ') then begin
           for j := length(MenuStr) downto 2 do
              if (MenuStr[j] = ' ') and (MenuStr[pred(j)] = ' ') then begin
                 System.Delete(MenuStr,j,1);
              end;
        end;
        i := 1;
        for j := 1 to length(MenuStr) do if MenuStr[j] = SepCharANSI then inc(i);
        LocalStringGrid.ColCount := i;
        LastLine := '';
        OnLine := 0;
        for i := 0 to LinesToMoveToStringGrid do begin
           //if (i mod 10 = 0) then CSVFileImportForm.ProgressBar1.Position := round(100 * i/FileInMemory.Count);
           {$IfDef RecordCSVProblems}
           if (i mod 1000 = 0) then WriteLineToDebugFile(IntToStr(i) + '/' + IntToStr(FileInMemory.Count));
           {$EndIf}

           MenuStr := ptTrim(FileInMemory.Strings[i]);
           if (SepChar = ' ') then begin
              for j := length(MenuStr) downto 2 do
                 if (MenuStr[j] = ' ') and (MenuStr[pred(j)] = ' ') then begin
                    System.Delete(MenuStr,j,1);
                 end;
           end;

           if (MenuStr <> LastLine) or MDDef.DupeImportsAllowed then begin
              TStr := ' ';
              for j := pred(Length(MenuStr)) downto 1 do begin
                 if (MenuStr[j] = SepCharANSI) and (MenuStr[pred(j)] = SepCharANSI) then begin
                    System.Insert('9999',MenuStr,j);
                 end;
              end;
              for j := 0 to LocalStringGrid.ColCount-2 do
                 LocalStringGrid.Cells[j,OnLine] := ptTrim(BeforeSpecifiedCharacterANSI(MenuStr,SepCharANSI,false,true));
              MenuStr := ptTrim(MenuStr);
              if (length(MenuStr) > 0) and (MenuStr[length(MenuStr)] = SepCharANSI) then Delete(MenuStr,Length(MenuStr),1);

              LocalStringGrid.Cells[pred(LocalStringGrid.ColCount),OnLine] := ptTrim(MenuStr);
              inc(OnLine);
           end
           else inc(NumDupesIgnored);
           LastLine := ptTrim(FileInMemory.Strings[i]);
        end;
        LocalStringGrid.RowCount := OnLine;
     end;

     if (NumDupesIgnored > 0) then begin
        MessageToContinue('Duplicate lines ignored: ' + IntToStr(NumDupesIgnored));
     end;

     {$IfDef RecordCSVProblems}
     WriteLineToDebugFile(' Records: ' + IntToStr(LocalStringGrid.RowCount),true);
     WriteLineToDebugFile(' Fields: ' + IntToStr(LocalStringGrid.ColCount));
     {$EndIf}

     Result := ExtractFilePath(fName) + SpacesToUnderScores(ExtractFileNameNoExt(fName)) + DefaultDBExt;
     DeleteFileIfExists(Result);
     //Result := CSVFileImportForm.OutPutName;

      NumRules := 0;
      if (SpecialGaz = csvNGAgaz) or (SpecialGaz = csvUSGSgaz) then GetGazReplacements;

     for i := 0 to pred(LocalStringGrid.ColCount) do begin
         FieldName := UpperCase(ptTrim(LocalStringGrid.Cells[i,0]));
         for j := length(FieldName) downto 1 do if (FieldName[j] in [' ']) then Delete(FieldName,j,1);
         {$IfDef RecordCSVProblems}
         WriteLineToDebugFile(FieldName);
         {$EndIf}
         if (FieldName = 'EASTING') then FieldName := 'X';
         if (FieldName = 'NORTHING') then FieldName := 'Y';
         if (FieldName = 'ELEVATION') then FieldName := 'Z';
         if (FieldName = 'LATITUDE') then FieldName := 'LAT';
         if (FieldName = 'LATITUDE N/S') then FieldName := 'LAT';
         if (FieldName = 'LONGITUDE') then FieldName := 'LONG';
         if (FieldName = 'LONGITUDE E/W') then FieldName := 'LONG';
         if (FieldName = 'LON') then FieldName := 'LONG';
         for j := length(FieldName) downto 1 do if not (FieldName[j] in ['A'..'Z','0'..'9','_']) then Delete(FieldName,j,1);
         for j := 1 to NumRules do if FieldName = OldNames[j] then FieldName := NewNames[j];
         LocalStringGrid.Cells[i,0] := FieldName;
     end;

     {$IfDef RecordCSVProblems}
     WriteLineToDebugFile('Field names done');
     {$EndIf}

     {$IfDef MICRODEM}
     if (SpecialGaz = csvNGAgaz) then begin
        {$IfDef RecordGAZProblems}
        WriteLineToDebugFile('NGA GAZ='+ fName,true);
        WriteLineToDebugFile('output='+ OutputName);
        OutputName := DEMDefs.GazetteerDir + ExtractFileName(OutputName);
        {$EndIf}
     end;

     if (SpecialGaz = csvUSGSgaz) then begin
        {$IfDef RecordGAZProblems}
        WriteLineToDebugFile('USGS GAZ='+ fName,true);
        WriteLineToDebugFile('output='+ OutputName);
        {$EndIf}
     end;
     {$EndIf}

      CreateDataBase := tCreateDataBase.Create(Result);
      with CreateDataBase do begin
        {$IfDef RecordCSVProblems}
         WriteLineToDebugFile('Fields in Input file ' + fName,true);
         for i := 0 to pred(LocalStringGrid.ColCount) do
            WriteLineToDebugFile(UpperCase(LocalStringGrid.Cells[i,0]));
         WriteLineToDebugFile('NumFields= ' + IntToStr(LocalStringGrid.ColCount));
         {$EndIf}

         BadFieldNames := 0;
         for i := 0 to pred(LocalStringGrid.ColCount) do begin
            TStr := UpperCase(LocalStringGrid.Cells[i,0]);
            if (Length(TStr) > 10) or (ptTrim(TStr) ='') then inc(BadFieldNames);
         end;

         AskAboutBadFieldNames := (BadFieldNames > 0) and AnswerIsYes('Ask about bad field names');

          for i := 0 to pred(LocalStringGrid.ColCount) do begin
             ZeroPadLen[i] := 0;
             {$IfDef RecordCSVParseProblems}
             WriteLineToDebugFile(IntToStr(i) + '='+ UpperCase(StringGrid1.Cells[i,0]),true);
             {$EndIf}
             Len := 0;
             Decs := 0;
             IsString := false;
             IsFloat := false;
             IsLeadingZero := false;
             FieldName := LocalStringGrid.Cells[i,0];
             NeedTrimWarning := true;
             if (FieldName <> 'SKIP') then begin
               {$IfDef RecordCSVParseProblems}
               WriteLineToDebugFile('Name check');
               {$EndIf}
               while (Length(FieldName) > 10) or (ptTrim(FieldName) = '') do begin
                  if AskAboutBadFieldNames then GetString('Field name (< 10 chars)',FieldName,true,['A'..'Z','0'..'9','_'])
                  else FieldName := 'FIELD' + IntToStr(i);
               end;
               LocalStringGrid.Cells[i,0] := FieldName;

                if (FieldName = 'IP') then IsString := true;

                for j := 1 to pred(LocalStringGrid.RowCount) do begin
                   TStr := ptTrim(LocalStringGrid.Cells[i,j]);
                   if (FieldName = 'LAT') then begin
                       while System.Copy(TStr,1,2) = '00' do Delete(Tstr,1,1);
                       if TStr[Length(Tstr)] = 'N' then Delete(TStr,length(TStr),1);
                       if TStr[Length(Tstr)] = 'S' then begin
                          Delete(TStr,length(TStr),1);
                          TStr := '-' + TStr;
                       end;
                       LocalStringGrid.Cells[i,j] := TStr;
                       IsFloat := true;
                       IsString := false;
                       Len := 11;
                       Decs := 7;
                    end
                    else if (FieldName = 'LONG') then begin
                       while System.Copy(TStr,1,2) = '00' do Delete(Tstr,1,1);
                       if TStr[Length(Tstr)] = 'E' then Delete(TStr,length(TStr),1);
                       if TStr[Length(Tstr)] = 'W' then begin
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

                       if (TStr = '#DIV/0!') or (UpperCase(TStr) = 'NAN') or (UpperCase(TStr) = 'NO DATA') then begin
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
                          if (not IsString) then begin
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
                          end;
                          if IsString then Decs := 0;
                       end;
                    end;
                 end;

                {$IfDef RecordCSVParseProblems}
                WriteLineToDebugFile('Done with field check');
                {$EndIf}

               if IsString or (IsLeadingZero and (not IsFloat)) then begin
                  if (FieldName = 'BIN_NAME') and (Len < 35) then Len := 35;
                  AddAField(FieldName,ftString,Len);
                  if IsLeadingZero then ZeroPadLen[i] := len;
                  {$IfDef RecordCSVParseProblems}
                  WriteLineToDebugFile(FieldName + '  type = string,  lne=' + IntToStr(len) + '  zeropad=' + IntToStr(ZeroPadLen[i]));
                  {$EndIf}
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
                  AddAField(FieldName,ftFloat,Len,Decs);
                  {$IfDef RecordCSVParseProblems}
                  WriteLineToDebugFile(FieldName + '  type = float');
                  {$EndIf}
               end
               else begin
                  if (Len >= 10) or FieldRequiresLeadingZeros(FieldName) then begin
                     AddAField(FieldName,ftString,Len);
                     ZeroPadLen[i] := len;
                     {$IfDef RecordCSVParseProblems}
                     WriteLineToDebugFile(FieldName + '  type = string (because too long for int or field=zip)');
                     {$EndIf}
                  end
                  else begin
                     AddAField(FieldName,ftInteger,succ(Len));
                     {$IfDef RecordCSVParseProblems}
                     WriteLineToDebugFile(FieldName + '  type = integer, length=' + IntToStr(Len));
                     {$EndIf}
                  end;
               end;
            end;
         end;
         CreateDataBase.WriteCorrectHeader(true);
      end;

      ShowHourglassCursor;
      wmdem.SetPanelText(0,'');

      if BatchMode then begin
         {$IfDef RecordCSVProblems}
         WriteLineToDebugFile('Batch mode point 1',true);
         {$EndIf}

         if AllinStringGrid then begin
            SendStringGridToDataBase(LocalStringGrid,CreateDataBase,ZeroPadLen);


            //CSVFileImportForm.BitBtn1Click(Nil);  //actually import data
         end
         else begin
            StartProgress('Convert');
            for j := 1 to pred(FileInMemory.Count) do begin
               if (j mod 1000 = 0) then UpdateProgressBar(j/FileInMemory.Count);
               MenuStr := FileInMemory.Strings[j];
               processLine(MenuStr);
            end;
            if (not ProcessInMemory) then begin
               while not eof(tfile) do begin
                  readln(tfile,MenuStr);
                  processLine(MenuStr);
               end;
               CloseFile(tFile);
            end;
            EndProgress;
         end;
      end;
      FileInMemory.Free;
      //CSVFileImportForm.CreateDataBase.Destroy;
      if BatchMode then LocalStringGrid.Free
      else //CSVFileImportForm.Close;
end;


procedure TCSVFileImportForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   {$IfDef RecordCSVProblems}
   WriteLineToDebugFileWithTime('TCSVFileImportForm.FormClose start');
   {$EndIf}
   Action := caFree;
   Application.ProcessMessages;
   {$IfDef RecordCSVProblems}
   WriteLineToDebugFileWithTime('TCSVFileImportForm.FormClose done');
   {$EndIf}
end;


procedure TCSVFileImportForm.BitBtn2Click(Sender: TObject);
begin
   Close;
end;


procedure TCSVFileImportForm.BitBtn3Click(Sender: TObject);
begin
   DisplayHTMLTopic('html/csv_file_imports.htm');
end;


procedure SendStringGridToDataBase(StringGrid1 : tStringGrid; CreateDataBase : tCreateDataBase; ZeroPadLen : tZeroPadLen);
var
   i,j{,MissFieldNum} : integer;
   v : float;
   fv : shortString;
   //MissField,
   TStr : Shortstring;
   RecordValues : tStringList;
begin
   {$IfDef RecordCSVProblems}
   WriteLineToDebugFileWithTime('TCSVFileImportForm.BitBtn1Click import underway');
   {$EndIf}
   ShowHourglassCursor;
   for j := 1 to pred(StringGrid1.RowCount) do begin
      //ProgressBar1.Position := round(100 * j/pred(StringGrid1.RowCount));
      {$IfDef RecordCSVProblems}
      if (J mod 500 = 0) then WriteLineToDebugFileWithTime('Record ' + IntToStr(j));
      {$EndIf}
         RecordValues := tStringList.Create;
         for i := 0 to pred(StringGrid1.ColCount) do begin
            TStr := StringGrid1.Cells[i,0];
            if (TStr <> 'SKIP') then begin
               {$IfDef RecordFullCSVProblems}
               WriteLineToDebugFile(TStr + ': '  + StringGrid1.Cells[i,j]);
               {$EndIf}
               if FieldRequiresLeadingZeros(TStr) then begin
                  fv := ptTrim(StringGrid1.Cells[i,j]);
                  while (length(fv) < ZeroPadLen[i]) do fv := '0' + fv;
                  StringGrid1.Cells[i,j] := fv;
               end;
               RecordValues.Add(ptTrim(StringGrid1.Cells[i,j]));
            end;
         end;
         CreateDataBase.AddCorrectRecordFromStringList(RecordValues);
         RecordValues.Free;
         {$IfDef RecordFullCSVProblems}
         WriteLineToDebugFile('');
         {$EndIf}
   end;
   ShowDefaultCursor;
   {$IfDef RecordCSVProblems}
   WriteLineToDebugFileWithTime('TCSVFileImportForm.BitBtn1Click import done');
   {$EndIf}
end;

procedure TCSVFileImportForm.BitBtn1Click(Sender: TObject);
begin
   SendStringGridToDataBase(StringGrid1,CreateDataBase,ZeroPadLen);
   BitBtn1.Enabled := false;
end;


procedure TCSVFileImportForm.FormCreate(Sender: TObject);
begin
   {$IfDef RecordCSVProblems}
   WriteLineToDebugFileWithTime('TCSVFileImportForm.FormCreate start');
   {$EndIf}
   Petmar.PlaceFormAtMousePosition(Self);
   {$IfDef RecordCSVProblems}
   WriteLineToDebugFileWithTime('TCSVFileImportForm.FormCreate done');
   {$EndIf}
end;


initialization
finalization
   {$IfDef RecordCSVProblems}
   WriteLineToDebugFile('RecordCSVProblems active in csvfileimport');
   {$EndIf}
   {$IfDef RecordKMLProblems}
   WriteLineToDebugFile('RecordKMLProblems active in csvfileimport');
   {$EndIf}
   {$IfDef RecordFullCSVProblems}
   WriteLineToDebugFile('RecordFullCSVProblems active in csvfileimport (major slowdown)');
   {$EndIf}
   {$IfDef RecordGAZProblems}
   WriteLineToDebugFile('RecordGAZProblems active in csvfileimport');
   {$EndIf}
   {$IfDef RecordCSVParseProblems}
   WriteLineToDebugFile('RecordCSVParseProblems active in csvfileimport');
   {$EndIf}
end.



