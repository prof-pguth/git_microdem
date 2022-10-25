unit toggle_db_use;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define ShowToggle}
   //{$Define FullShowToggle}
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

   {$IfDef UseBDETables}
      dbTables,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}


   Windows, Classes, Graphics, Forms, Controls, Buttons, StdCtrls, ExtCtrls,SysUtils,
   DEMDefs,
   PetDBUtils,Petmar_types,PetMAR;

type
  TToggleDBfieldsForm = class(TForm)
    ListBox1: TListBox;
    ListBox2: TListBox;
    Panel1: TPanel;
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Button1: TButton;
    Panel2: TPanel;
    Label2: TLabel;
    Label1: TLabel;
    Button2: TButton;
    ComboBox1: TComboBox;
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ComboBox1Change(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateStatus;
  public
    { Public declarations }
    TheTable : tMyData;
    SecondField,LimField,
    FieldDisplayed,ToggleField :  ShortString;
    HappyEnding,WasFiltered : boolean;
    OldFilter : string;
  end;

function VerifyRecordsToUse(var Table : tMyData; FieldShownUser : ShortString; aCaption : shortstring = ''; FieldToToggle : ShortString = 'USE'; SecondaryField : ShortString = ''; LimitField : ShortString = '') : boolean; overload;

function VerifyRecordsToUse(fName : PathStr; FieldShownUser : ShortString; aCaption : shortstring = ''; FieldToToggle : ShortString = 'USE'; SecondaryField : ShortString = ''; LimitField : ShortString = '') : boolean; overload;
procedure PickSomeFromStringList(var SL : tStringList; aCaption : ShortString);
procedure PickDEMsToUse(var DEMs : tDEMbooleanArray; aCaption : ShortString);


implementation

{$R *.DFM}

uses
   Make_Tables,
   DEMCoord;


procedure PickDEMsToUse(var DEMs : tDEMbooleanArray; aCaption : ShortString);
var
   fName : PathStr;
   table : tMyData;
   i : integer;
   ch : char;
begin
   {$IfDef ShowToggle} WriteLineToDebugFile('PickSomeFromStringList in'); {$EndIf}
   fName := MDTempDir + 'merge.dbf';
   MakePickUseTable(fName);
   Table := tMyData.Create(fName);
   for i := 1 to MaxDEMDataSets do begin
     if ValidDEM(i) then begin
        Table.Insert;
        Table.SetFieldByNameAsString('MENU_OPTS',IntToStr(i) + '-' + DEMGlb[i].AreaName);
        if DEMs[i] then ch := 'Y' else ch := 'N';
        Table.SetFieldByNameAsString('USE',ch);
     end;
   end;
   VerifyRecordsToUse(Table,'MENU_OPTS',aCaption);
   {$IfDef ShowToggle} WriteLineToDebugFile('VerifyRecordsToUse success'); {$EndIf}
   Table.First;
   while not Table.eof do begin
      i := StrToInt(Petmar_Types.BeforeSpecifiedString(Table.GetFieldByNameAsString('MENU_OPTS'),'-'));
      DEMs[i] := Table.GetFieldByNameAsString('USE') = 'Y';
      Table.Next;
   end;
   Table.Destroy;
   {$IfDef ShowToggle} WriteLineToDebugFile('PickSomeFromStringList out'); {$EndIf}
end;




procedure PickSomeFromStringList(var SL : tStringList; aCaption : ShortString);
var
   fName : PathStr;
   table : tMyData;
   i : integer;
begin
   {$IfDef ShowToggle} WriteLineToDebugFile('PickSomeFromStringList in'); {$EndIf}
   fName := MDTempDir + 'merge.dbf';
   MakePickUseTable(fName);
   Table := tMyData.Create(fName);
   for I := 0 to pred(sl.Count) do begin
      Table.Insert;
      Table.SetFieldByNameAsString('MENU_OPTS',sl.Strings[i]);
      Table.SetFieldByNameAsString('USE','Y');
      Table.Next;
   end;
   VerifyRecordsToUse(Table,'MENU_OPTS',aCaption);
   {$IfDef ShowToggle} WriteLineToDebugFile('VerifyRecordsToUse success'); {$EndIf}
   sl.clear;
   Table.ApplyFilter('USE=' + QuotedStr('Y'));
   Table.First;
   while not Table.eof do begin
      sl.add(Table.GetFieldByNameAsString('MENU_OPTS'));
      Table.Next;
   end;
   Table.Destroy;
   {$IfDef ShowToggle} WriteLineToDebugFile('PickSomeFromStringList out'); {$EndIf}
end;


function VerifyRecordsToUse(fName : PathStr; FieldShownUser : ShortString; aCaption : shortstring = ''; FieldToToggle : ShortString = 'USE'; SecondaryField : ShortString = ''; LimitField : ShortString = '') : boolean; overload;
var
  Table : tMyData;
begin
   Table := tMyData.Create(fName);
   Result := VerifyRecordsToUse(Table,FieldShownUser,aCaption,FieldToToggle,SecondaryField,LimitField);
   Table.Destroy;
end;


function VerifyRecordsToUse(var Table : tMyData; FieldShownUser : ShortString; aCaption : shortstring = ''; FieldToToggle : ShortString = 'USE'; SecondaryField : ShortString = ''; LimitField : ShortString = '') : boolean; overload;
var
   ToggleDBfieldsForm : TToggleDBfieldsForm;
   DataThere : tStringList;
   i : integer;
begin
   {$IfDef ShowToggle} WriteLineToDebugFile('VerifyRecordsToUse, FieldShownUser=' + FieldShownUser +  '  FieldToToggle=' + FieldToToggle); {$EndIf}
   if (Table.RecordCount = 0) then begin
      Result := false;
      exit;
   end;
   ToggleDBfieldsForm := TToggleDBfieldsForm.Create(Application);
   with ToggleDBfieldsForm do begin
      TheTable := Table;
      FieldDisplayed := FieldShownUser;
      ToggleField := FieldToToggle;
      SecondField := SecondaryField;
      LimField := LimitField;
      OldFilter := TheTable.Filter;
      WasFiltered := (OldFilter <> '');
      if (LimField <> '') then begin
         DataThere := Nil;
         DataThere := TheTable.UniqueEntriesInDB(LimField);
         for i := 0 to pred(DataThere.Count) do ComboBox1.Items.Add(DataThere.Strings[i]);
         DataThere.Free;
         ComboBox1.Visible := true;
      end
      else ComboBox1.Visible := false;

      UpdateStatus;
      if (aCaption <> '') then Caption := aCaption;

      ToggleDBfieldsForm.ShowModal;
      Result := HappyEnding;
   end;
   {$IfDef ShowToggle} WriteLineToDebugFile('VerifyRecordsToUse out'); {$EndIf}
end;


procedure TToggleDBfieldsForm.Button1Click(Sender: TObject);
var
   ch : AnsiChar;
begin
   TheTable.First;
   while not TheTable.Eof do begin
      TheTable.Edit;
      if (Sender = Button1) then ch := 'Y' else ch := 'N';
      TheTable.SetFieldByNameAsString(ToggleField,ch);
      TheTable.Next;
   end;
   UpdateStatus;
end;


procedure TToggleDBfieldsForm.CancelBtnClick(Sender: TObject);
begin
   HappyEnding := false;
end;

procedure TToggleDBfieldsForm.ComboBox1Change(Sender: TObject);
begin
   UpdateStatus;
end;

procedure TToggleDBfieldsForm.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TToggleDBfieldsForm.FormResize(Sender: TObject);
begin
   ListBox1.Width := ClientWidth div 2 - 5;
   ListBox2.Width := ListBox1.Width;
end;


procedure TToggleDBfieldsForm.UpdateStatus;
var
   TStr,fName : ShortString;
begin
   {$IfDef FulllShowToggle} WriteLineToDebugFile('TToggleDBfieldsForm.UpdateStatus in'); {$EndIf}
   ListBox1.Items.Clear;
   ListBox2.Items.Clear;
   TheTable.ApplyFilter(OldFilter);
   if ComboBox1.Visible and (ComboBox1.ItemIndex <> -1) then begin
      if TheTable.Filtered then TStr := TheTable.Filter + ' AND ' else TStr := '';
      TheTable.ApplyFilter(TStr + LimField + '=' + QuotedStr(ComboBox1.Items[ComboBox1.ItemIndex]));
   end;

   TheTable.First;
   while not TheTable.EOF do begin
      fName := TheTable.GetFieldByNameAsString(FieldDisplayed);
      if (fName <> '') then begin
         if (SecondField <> '') then fName := fName + '  (' + TheTable.GetFieldByNameAsString(SecondField) + ')';
         if Uppercase(TheTable.GetFieldByNameAsString(ToggleField)) = 'Y' then begin
            ListBox2.Items.Add(fName);
            {$IfDef FullShowToggle} WriteLineToDebugFile(fName + '=Y'); {$EndIf}
         end
         else begin
            ListBox1.Items.Add(fName);
            {$IfDef FullShowToggle} WriteLineToDebugFile(fName + '=N'); {$EndIf}
         end;
      end;
      TheTable.Next;
   end;
   {$IfDef FulllShowToggle} WriteLineToDebugFile('TToggleDBfieldsForm.UpdateStatus while over');  {$EndIf}
   Label1.Caption := 'Enabled (' + IntToStr(ListBox2.Count) + ')';
   Label2.Caption := 'Disabled (' + IntToStr(ListBox1.Count) + ')';
   {$IfDef FulllShowToggle} WriteLineToDebugFile('TToggleDBfieldsForm.UpdateStatus out'); {$EndIf}
end;


procedure TToggleDBfieldsForm.ListBox1Click(Sender: TObject);
var
   New : AnsiChar;
   TStr : ANSIString;
   SaveItem : integer;
begin
   {$IfDef ShowToggle} WriteLineToDebugFile('TToggleDBfieldsForm.ListBox1Click'); {$EndIf}
   if (Sender = ListBox1) then begin
      {$IfDef ShowToggle} WriteLineToDebugFile('Toggle list box 1'); {$EndIf}
      TStr := ListBox1.Items[ListBox1.ItemIndex];
   end;
   if (Sender = ListBox2) then begin
      {$IfDef ShowToggle} WriteLineToDebugFile('Toggle list box 2'); {$EndIf}
      TStr := ListBox2.Items[ListBox2.ItemIndex];
   end;
   if (SecondField <> '') then begin
      TStr := trim(Petmar_types.BeforeSpecifiedCharacterANSI(TStr,'(',false,false));
   end;
   if (Sender = ListBox1) then SaveItem := ListBox1.ItemIndex;
   if (Sender = ListBox2) then SaveItem := ListBox2.ItemIndex;

   TStr := FieldDisplayed + '=' + QuotedStr(TStr);
   if WasFiltered then TheTable.ApplyFilter(TStr + ' AND (' + OldFilter + ')')
   else TheTable.ApplyFilter(TStr);

   {$IfDef ShowToggle} WriteLineToDebugFile('Filter:  ' + TheTable.Filter + '  Recs=' + IntToStr(TheTable.RecordCount)); {$EndIf}

   if (TheTable.RecordCount >= 1) then begin
      {$IfDef ShowToggle} WriteLineToDebugFile('Number to toggle=' + IntToStr(TheTable.RecordCount) + '   ToggleField=' + ToggleField);   {$EndIf}
      while not TheTable.eof do begin
          TheTable.Edit;
          if (Sender = ListBox1) then New := 'Y' else New := 'N';
          TheTable.SetFieldByNameAsString(ToggleField,New);
          TheTable.Next;
      end;
   end;
   TheTable.ApplyFilter('');
   UpdateStatus;
   if (Sender = ListBox1) then ListBox1.ItemIndex := SaveItem;
   if (Sender = ListBox2) then ListBox2.ItemIndex := SaveItem;
   {$IfDef ShowToggle} WriteLineToDebugFile('TToggleDBfieldsForm.ListBox1Click out'); {$EndIf}
end;


procedure TToggleDBfieldsForm.ListBox2Click(Sender: TObject);
begin
   ListBox1Click(Sender);
end;


procedure TToggleDBfieldsForm.OKBtnClick(Sender: TObject);
begin
   {$IfDef ShowToggle} WriteLineToDebugFile('TToggleDBfieldsForm.OKBtnClick in'); {$EndIf}
   HappyEnding := (ListBox2.Count > 0);
   {$IfDef ShowToggle} WriteLineToDebugFile('TToggleDBfieldsForm.OKBtnClick out'); {$EndIf}
end;

initialization
finalization
   {$IfDef ShowToggle} WriteLineToDebugFile('FullShowtoggle active in toggle_db_use'); {$EndIf}
   {$IfDef FullShowToggle} WriteLineToDebugFile('Showtoggle active in toggle_db_use'); {$EndIf}
end.
