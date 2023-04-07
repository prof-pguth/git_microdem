unit db_join;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordDBjoin}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,  Dialogs, StdCtrls, Buttons,DB,

  DEMDataBase;

type
  Tdbjoinform2 = class(TForm)
    ComboBox1: TComboBox;
    Label2: TLabel;
    Memo1: TMemo;
    BitBtn1: TBitBtn;
    Label3: TLabel;
    Label4: TLabel;
    ComboBox2: TComboBox;
    Memo2: TMemo;
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Label1: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    CheckBox1: TCheckBox;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    CheckBox2: TCheckBox;
    procedure HelpBtnClick(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    TheGIS : TGISdataBaseModule;
  end;


procedure SetUpJoin(var GIS : TGISdataBaseModule; ShowOnly : boolean = false);


implementation

{$R *.dfm}

uses
   Petmar_types,Petmar,Petmar_db,PetDBUtils,DEMDefs;


procedure CheckForDefaultJoins(var LinkName : ShortString; Table : tMyData);
begin
  if (LinkName = '') and (Table <> Nil) then begin
     if Table.FieldExists('FIPS') then LinkName := 'FIPS'
     else if Table.FieldExists('STATE_FIPS') then LinkName := 'STATE_FIPS'
     else if Table.FieldExists('STFID') then LinkName := 'STFID'
     else if Table.FieldExists('GEO_ID2') then LinkName := 'GEO_ID2'
  end;
end;


procedure SetUpJoin(var GIS : TGISdataBaseModule; ShowOnly : boolean = false);
var
  Form2 : TDBJoinForm2;
  FieldsInDB : tStringList;
begin
  {$IfDef RecordDBjoin} WriteLineToDebugFile('SetUpJoin in'); {$EndIf}
   Form2 := TDBJoinForm2.Create(Application);
   with Form2 do begin
      Caption := 'Join for ' + GIS.dbName;
      TheGIS := GIS;
      TheGIS.ClearGISFilter;
      Label1.Caption := IntToStr(TheGIS.MyData.RecordCount) + ' records';
      TheGIS.ClearGISFilter;
      FieldsInDB := Nil;
      GetFieldsLinkPossible(GIS.MyData,Nil,TheGIS.dbOpts.VisCols,[ftString,ftInteger,ftSmallInt],FieldsInDB);
      ComboBox1.Items := FieldsInDB;
      CheckForDefaultJoins(TheGIS.dbOpts.LinkFieldThisDB,TheGIS.MyData);
      if (TheGIS.dbOpts.LinkFieldThisDB <> '') then begin
         ComboBox1.Text := TheGIS.dbOpts.LinkFieldThisDB;
         ComboBox1Change(Nil);
      end;
      if TheGIS.dbOpts.LinkTableName <> '' then begin
         BitBtn1Click(Nil);
      end;
      if (TheGIS.dbOpts.LinkFieldOtherDB <> '') and (TheGIS.dbOpts.LinkTableName <> '') then begin
         ComboBox2.Text := TheGIS.dbOpts.LinkFieldOtherDB;
         ComboBox2Change(Nil);
      end;
   end;
   if ShowOnly then with Form2 do begin
      ComboBox1.Enabled := false;
      ComboBox2.Enabled := false;
      BitBtn1.Enabled := false;
      CancelBtn.Enabled := false;
   end;
   Form2.ShowModal;
end;


procedure Tdbjoinform2.BitBtn1Click(Sender: TObject);
var
  FieldsInDB : tStringList;
  Table : tMyData;
begin
   Label3.Caption := '';
   Label5.Caption := '';
   if (Sender <> Nil) then with TheGIS,MyData,dbOpts do begin
      LinkTableName := ExtractFilePath(LastDataBase);
      if not GetFileFromDirectory('linked data base',DefaultDBMask,LinkTableName) then exit;
      if UpperCase(ExtractFileExt(LinkTableName)) <> UpperCase(DefaultDBExt) then begin
         MessageToContinue('linked file must be ' + DefaultDBExt);
         LinkTableName := '';
         exit;
      end;
   end;

   Table := tMyData.Create(TheGIS.dbOpts.LinkTableName);
   CheckForDefaultJoins(TheGIS.dbOpts.LinkFieldOtherDB,Table);
   if TheGIS.dbOpts.LinkFieldOtherDB = '' then begin
      if Table.FieldExists(TheGIS.dbOpts.LinkFieldThisDB) then
         TheGIS.dbOpts.LinkFieldOtherDB := TheGIS.dbOpts.LinkFieldThisDB;
   end;
   Label3.Caption := TheGIS.dbOpts.LinkTableName;

   Label5.Caption := IntToStr(Table.RecordCount) + ' records';
   FieldsInDB := Nil;
   GetFieldsLinkPossible(Table,Nil,AllVis,[ftString,ftInteger,ftSmallInt],FieldsInDB,true);
   ComboBox2.Items := FieldsInDB;
   if Table.FieldExists(ComboBox1.Text) then begin
      ComboBox2.Text := ComboBox1.Text;
      ComboBox2Change(Sender);
   end;

   Table.Destroy;
end;

procedure Tdbjoinform2.CancelBtnClick(Sender: TObject);
begin
  {$IfDef RecordDBjoin} WriteLineToDebugFile('Tdbjoinform2.CancelBtnClick'); {$EndIf}
end;

procedure Tdbjoinform2.CheckBox1Click(Sender: TObject);
begin
   MDdef.AllowFirstOfMultipleJoins := CheckBox1.Checked;
end;

procedure Tdbjoinform2.CheckBox2Click(Sender: TObject);
begin
    MDdef.JoinRemoveLeadingZeros := CheckBox2.Checked;
end;

procedure CheckOnFieldTrims(Table : tMyData; fName : shortString);
var
   i : integer;
   TStr : shortString;
begin
   for i := 0 to 9 do begin
     TStr := Table.GetFieldByNameAsString(fName);
     if (TStr <> '') and (TStr[1] = ' ') then begin
        Table.TrimAllStringFields;
        break;
     end;
     Table.Next;
     if Table.eof then break;
   end;
   Table.First;
end;


procedure Tdbjoinform2.ComboBox1Change(Sender: TObject);
var
  i,j,Skipr : integer;
  TStr : shortString;
begin
   with TheGIS,MyData do begin
      if FieldExists(UpperCase(ComboBox1.Text)) then begin
        dbOpts.LinkFieldThisDB := UpperCase(ComboBox1.Text);
        First;
        Memo1.Clear;
        Label6.Caption := 'Field ' + MyData.FieldTypeAndLength(dbOpts.LinkFieldThisDB);
        CheckOnFieldTrims(MyData,dbOpts.LinkFieldThisDB);
        if MDdef.JoinRemoveLeadingZeros and TheGIS.MyData.IsStringField(TheGIS.dbOpts.LinkFieldThisDB) then begin
           RemoveLeadingZerosInField(TheGIS.MyData,TheGIS.dbOpts.LinkFieldThisDB);
        end;
        Skipr := MyData.TotRecsInDB div 12;
        TheGIS.EmpSource.Enabled := false;
        for i := 0 to 9 do begin
           TStr := GetFieldByNameAsString(dbOpts.LinkFieldThisDB);
           Memo1.Lines.Add(TStr);
           for j := 1 to Skipr do MyData.Next;
           if eof then break;
        end;
        TheGIS.EmpSource.Enabled := true;
        Label9.Caption := 'Actual record: ' + IntToStr(length(TStr));
      end;
   end;
end;


procedure Tdbjoinform2.ComboBox2Change(Sender: TObject);
var
  i,j,Skipr : integer;
  Table : tMyData;
  TStr : shortString;
begin
   with TheGIS,MyData do begin
      Label8.Visible := false;
      if (dbOpts.LinkTableName <> '') and (ComboBox2.Text <> '') then begin
         dbOpts.LinkFieldOtherDB := UpperCase(ComboBox2.Text);
         Table := tMyData.Create(dbOpts.LinkTableName);
         Label7.Caption := 'Field length: ' + Table.FieldTypeAndLength(dbOpts.LinkFieldOtherDB);
         Memo2.Clear;
         if Table.FieldExists(dbOpts.LinkFieldOtherDB) then begin
            CheckOnFieldTrims(Table,dbOpts.LinkFieldOtherDB);
            if MDdef.JoinRemoveLeadingZeros and Table.IsStringField(TheGIS.dbOpts.LinkFieldOtherDB) then begin
               RemoveLeadingZerosInField(Table,TheGIS.dbOpts.LinkFieldOtherDB);
            end;

            Skipr := Table.TotRecsInDB div 12;
            for i := 0 to 9 do begin
               TStr := Table.GetFieldByNameAsString(dbOpts.LinkFieldOtherDB);
               Memo2.Lines.Add(TStr);
               for j := 1 to Skipr do Table.Next;
               if Table.eof then break;
            end;
            Label10.Caption := 'Actual record: ' + IntToStr(length(TStr));
            if NumberUniqueEntries(Table,dbOpts.LinkFieldOtherDB) < Table.RecordCount then Label8.Visible := true;
         end
         else begin
            dbOpts.LinkFieldOtherDB := '';
         end;
         Table.Destroy;
      end;
   end;
end;

procedure Tdbjoinform2.FormCreate(Sender: TObject);
begin
   CheckBox1.Checked := MDdef.AllowFirstOfMultipleJoins;
   CheckBox2.Checked := MDdef.JoinRemoveLeadingZeros;
end;


procedure Tdbjoinform2.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\db_join.htm');
end;


procedure Tdbjoinform2.OKBtnClick(Sender: TObject);
begin
  {$IfDef RecordDBjoin} WriteLineToDebugFile('Tdbjoinform2.OKBtnClick (done)'); {$EndIf}
end;

initialization
finalization
  {$IfDef RecordDBjoin} WriteLineToDebugFile('RecordDBjoin active in db_join'); {$EndIf}
end.
