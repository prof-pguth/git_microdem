unit demdbfilter;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}
   //{$Define RecordFilterProblems}
{$EndIf}


interface

uses
   Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Controls, Vcl.Buttons,
   System.Classes,
   Windows, SysUtils, Graphics, Forms,DB,
   demdatabase,Petmar_Types;

type
  TdbFilterCreation = class(TForm)
    Panel1: TPanel;
    BitBtn8: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn3: TBitBtn;
    HelpBtn: TBitBtn;
    BitBtn10: TBitBtn;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Panel2: TPanel;
    Label2: TLabel;
    Label1: TLabel;
    Edit1: TEdit;
    BitBtn2: TBitBtn;
    ComboBox3: TComboBox;
    ComboBox1: TComboBox;
    Button1: TButton;
    ComboBox2: TComboBox;
    Edit2: TEdit;
    BitBtn5: TBitBtn;
    Memo1: TMemo;
    CheckBox1: TCheckBox;
    BitBtn4: TBitBtn;
    TabSheet2: TTabSheet;
    Memo2: TMemo;
    BitBtn6: TBitBtn;
    TabSheet3: TTabSheet;
    GroupBox1: TGroupBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    CheckBox20: TCheckBox;
    CheckBox21: TCheckBox;
    CheckBox22: TCheckBox;
    CheckBox23: TCheckBox;
    Memo3: TMemo;
    BitBtn11: TBitBtn;
    CheckBox8: TCheckBox;
    BitBtn12: TBitBtn;
    GroupBox2: TGroupBox;
    Edit4: TEdit;
    ComboBox5: TComboBox;
    Label3: TLabel;
    ComboBox4: TComboBox;
    Edit3: TEdit;
    BitBtn13: TBitBtn;
    BitBtn7: TBitBtn;
    CheckBox9: TCheckBox;
    procedure CheckBox23Click(Sender: TObject);
    procedure CheckBox20Click(Sender: TObject);
    procedure CheckBox21Click(Sender: TObject);
    procedure CheckBox22Click(Sender: TObject);
    procedure CheckBox15Click(Sender: TObject);
    procedure CheckBox12Click(Sender: TObject);
    procedure CheckBox13Click(Sender: TObject);
    procedure CheckBox14Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
  private
    { Private declarations }
    procedure CreateTimeFilter;
  public
   { Public declarations }
     QuickFilter : boolean;
     WantedField : integer;
     WantedFieldName : ShortString;
     //GISDataBase : TGISdataBaseModule;
     db : integer;
  end;

var
  dbFilterCreation: TdbFilterCreation;


procedure GetFilterString(inDB : integer; {GISDataBase : TGISdataBaseModule;} var TheFilter : AnsiString; var ChangeUse : boolean; Quick : boolean = false; StartFieldName : Ansistring = '');


implementation

{$R *.DFM}


uses
   PETMAR,PETdbUtils,toggle_db_use,
   DEMDefs,demdbtable, PETImage;

var
   WantedFilter : string;


procedure GetFilterString(inDB : integer; {GISDataBase : TGISdataBaseModule;} var TheFilter : AnsiString; var ChangeUse : boolean; Quick : boolean = false; StartFieldName : Ansistring = '');
begin
   with GISdb[indb] {GISDataBase} do begin
      {$IfDef RecordFilterProblems}
         WriteLineToDebugFile('GetFilterString in, MainFilter= "' + GISDataBase.dbOpts.MainFilter + '"');
         WriteLineToDebugFile('GetFilterString in, Geofilter= "' + GISDataBase.dbOpts.GeoFilter + '"');
         WriteLineToDebugFile('GetFilterString in, Timefilter= "' + GISDataBase.dbOpts.TimeFilter + '"');
      {$EndIf}
      dbFilterCreation := TdbFilterCreation.Create(Application);
      dbFilterCreation.QuickFilter := Quick;
      dbFilterCreation.Caption := 'DB filter ' + GISdb[indb].dbName;
      //dbFilterCreation.GISDataBase := GISDataBase;
      dbFilterCreation.db := inDB;
      dbFilterCreation.TabSheet3.Enabled := GISdb[indb].MyData.FieldExists('YEAR') or GISdb[indb].MyData.FieldExists(GISdb[indb].MonthFieldName);
      dbFilterCreation.GroupBox1.Enabled := GISdb[indb].MyData.FieldExists(GISdb[indb].MonthFieldName);
      dbFilterCreation.GroupBox2.Enabled := GISdb[indb].MyData.FieldExists('YEAR');
      dbFilterCreation.TabSheet1.Visible := true;
      dbFilterCreation.PageControl1.ActivePage := dbFilterCreation.TabSheet1;
      dbFilterCreation.BitBtn8.Enabled := GISdb[indb].TheMapOwner <> Nil;

      with dbFilterCreation do begin
         CheckBox1.Checked := DEMDefs.MDDef.DBfilterCaseInSensitive;
         CheckBox8.Enabled := MyData.FieldExists('USE');
         BitBtn11.Enabled := CheckBox8.Enabled;
         BitBtn10.Enabled := LayerTable <> Nil;
         if (dbOpts.TimeFilter <> '') then System.Delete(dbOpts.MainFilter,1,Length(dbOpts.TimeFilter) + 5);

         if (dbOpts.MainFilter <> '') then begin
            if (dbOpts.MainFilter[1] = '(') and (dbOpts.MainFilter[length(dbOpts.MainFilter)] = ')') then begin
               System.Delete(dbOpts.MainFilter,1,1);
               System.Delete(dbOpts.MainFilter,Length(dbOpts.MainFilter),1);
            end;
            Memo1.Lines.Add(dbOpts.MainFilter);
         end;
         Memo2.Lines.Clear;
         if (dbOpts.GeoFilter <> '') then Memo2.Lines.Add(dbOpts.GeoFilter);
         Memo3.Lines.Clear;
         if (dbOpts.TimeFilter <> '') then Memo3.Lines.Add(dbOpts.TimeFilter);

         Button1.Visible := false;
         ComboBox1.Items := GISdb[indb].MyData.FieldsInDataBase;
         Button1.Visible := (ComboBox1.Items.Count < MyData.FieldCount);

         WantedField := -99;
         WantedFilter := TheFilter;
         if Quick then begin
            ComboBox1.Text := StartFieldName;
            ComboBox1Change(Nil);
            if not GISdb[indb].MyData.IsNumericField(StartFieldName) then begin
               BitBtn5Click(Nil);
               BitBtn2Click(Nil);
               BitBtn1Click(Nil);
               TheFilter := dbOpts.MainFilter;
               dbFilterCreation.Destroy;
               exit;
            end;
         end;
         ShowModal;

         MDDef.DBfilterCaseInSensitive := CheckBox1.Checked;
         ChangeUse := CheckBox8.Checked;

         if (dbOpts.TimeFilter <> '') then begin
            if (WantedFilter = '') then WantedFilter := dbOpts.TimeFilter
            else WantedFilter := dbOpts.TimeFilter + ' AND ' + WantedFilter;
         end;

         dbOpts.MainFilter := WantedFilter;
         if (Memo2.Lines[0] = '') then dbOpts.GeoFilter := '';

         if (WantedFilter <> '') and (dbOpts.GeoFilter <> '') then TheFilter := WantedFilter + ' AND ' + dbOpts.GeoFilter
         else if (WantedFilter <> '') then TheFilter := dbOpts.MainFilter
         else if (dbOpts.GeoFilter <> '') then TheFilter := dbOpts.GeoFilter
         else TheFilter := '';
         dbFilterCreation.Destroy;

         {$IfDef RecordFilterProblems}
            WriteLineToDebugFile('GetFilterString out, MainFilter= "' + GISDataBase.dbOpts.MainFilter + '"');
            WriteLineToDebugFile('GetFilterString out, Geofilter= "' + GISDataBase.dbOpts.GeoFilter + '"');
            WriteLineToDebugFile('GetFilterString out, Timefilter= "' + GISDataBase.dbOpts.TimeFilter + '"');
         {$EndIf}
      end;
   end;
end;


procedure TdbFilterCreation.BitBtn5Click(Sender: TObject);
var
   DataThere : tStringList;
   i         : integer;
begin
   if GISdb[db].MyData.GetFieldType(GISdb[db].MyData.GetFieldName(WantedField)) in [ftString,ftInteger,ftSmallInt,ftLargeInt] then begin
      GISdb[db].EmpSource.Enabled := false;
      DataThere := GISdb[db].MyData.UniqueEntriesInDB(WantedFieldName);
      GISdb[db].EmpSource.Enabled := true;
      i := 0;
      MultiSelectSingleColumnStringList('Desired value',i,DataThere);
      Edit2.Text := DataThere.Strings[i];
      DataThere.Free;
   end;
end;


procedure TdbFilterCreation.CheckBox12Click(Sender: TObject);
begin
   CreateTimeFilter;
end;

procedure TdbFilterCreation.CheckBox13Click(Sender: TObject);
begin
   CreateTimeFilter;
end;

procedure TdbFilterCreation.CheckBox14Click(Sender: TObject);
begin
   CreateTimeFilter;
end;

procedure TdbFilterCreation.CheckBox15Click(Sender: TObject);
begin
   CreateTimeFilter;
end;

procedure TdbFilterCreation.CheckBox20Click(Sender: TObject);
begin
   CreateTimeFilter;
end;

procedure TdbFilterCreation.CheckBox21Click(Sender: TObject);
begin
   CreateTimeFilter;
end;

procedure TdbFilterCreation.CheckBox22Click(Sender: TObject);
begin
   CreateTimeFilter;
end;

procedure TdbFilterCreation.CheckBox23Click(Sender: TObject);
begin
   CreateTimeFilter;
end;

procedure TdbFilterCreation.CheckBox2Click(Sender: TObject);
begin
   MDDef.DBfilterClearLayer := CheckBox2.Checked;
end;

procedure TdbFilterCreation.CheckBox3Click(Sender: TObject);
begin
   MDDef.DBfilterRedrawOnClose := CheckBox3.Checked;
end;

procedure TdbFilterCreation.CheckBox4Click(Sender: TObject);
begin
   CreateTimeFilter;
end;

procedure TdbFilterCreation.CheckBox5Click(Sender: TObject);
begin
   CreateTimeFilter;
end;

procedure TdbFilterCreation.CheckBox6Click(Sender: TObject);
begin
   CreateTimeFilter;
end;

procedure TdbFilterCreation.CheckBox7Click(Sender: TObject);
begin
   CreateTimeFilter;
end;


procedure TdbFilterCreation.CheckBox9Click(Sender: TObject);
begin
   MDDef.ApplyFilterToAllDBs := CheckBox9.Checked;
end;

procedure TdbFilterCreation.CreateTimeFilter;

   procedure AddMonth(cb : tCheckBox; Month : integer);
   begin
       {with GISdb[db].MyData do } if cb.Checked then begin
         if GISdb[db].dbOpts.TimeFilter = '' then GISdb[db].dbOpts.TimeFilter := '(' else GISdb[db].dbOpts.TimeFilter := GISdb[db].dbOpts.TimeFilter + ' OR ';
         GISdb[db].dbOpts.TimeFilter := GISdb[db].dbOpts.TimeFilter + 'MONTH=' + IntToStr(Month);
      end;
   end;

begin
   //with GISDataBase,MyData do begin
      GISdb[db].dbOpts.TimeFilter := '';
      AddMonth(CheckBox4,1);
      AddMonth(CheckBox5,2);
      AddMonth(CheckBox6,3);
      AddMonth(CheckBox7,4);
      AddMonth(CheckBox14,5);
      AddMonth(CheckBox13,6);
      AddMonth(CheckBox12,7);
      AddMonth(CheckBox15,8);
      AddMonth(CheckBox22,9);
      AddMonth(CheckBox21,10);
      AddMonth(CheckBox20,11);
      AddMonth(CheckBox23,12);
      if (GISdb[db].dbOpts.TimeFilter <> '') then begin
         GISdb[db].dbOpts.TimeFilter := GISdb[db].dbOpts.TimeFilter + ')';
         BitBtn1.Enabled := true;
      end;
      Memo3.Lines.Clear;
      Memo3.Lines.Add(GISdb[db].dbOpts.TimeFilter);
   //end;
end;

procedure TdbFilterCreation.ComboBox1Change(Sender: TObject);
var
   i : integer;
begin
   //with GISDataBase.MyData do begin
      for i := 0 to pred(GISdb[db].MyData.FieldCount) do begin
         if (GISdb[db].MyData.GetFieldName(i) = ComboBox1.Text) then begin
            WantedField := i;
            WantedFieldName := ComboBox1.Text;
            Edit1.Text := '';
            Edit2.Text := '';
            if GISdb[db].MyData.GetFieldType(WantedField) in [ftString] then
               if (ComboBox2.Text = '<') or (ComboBox2.Text = '<=') then ComboBox2.Text := '=';
            BitBtn5.Enabled := GISdb[db].MyData.GetFieldType(WantedField) in [ftString,ftInteger,ftSmallInt,ftLargeInt];
            BitBtn7.Enabled := GISdb[db].MyData.GetFieldType(WantedField) in [ftString,ftInteger,ftSmallInt,ftLargeInt];
            Edit1.Enabled := GISdb[db].MyData.GetFieldType(WantedField) in [ftFloat,ftInteger,ftSmallInt,ftLargeInt,ftDate];
            ComboBox3.Enabled := Edit1.Enabled;
            Edit2.Enabled := true;
            exit;
         end;
      end;
   //end;
end;


procedure TdbFilterCreation.BitBtn2Click(Sender: TObject);
var
   f1,f2,f3,f4 : shortString;
   TStr,TStr2 : ShortString;
begin
   if (WantedField >= 0) then begin
      f4 := '';
      if (GISdb[db].MyData.GetFieldType(WantedField) in [ftFloat,ftInteger,ftSmallInt,ftLargeInt,ftDate]) then begin
         f1 := '';
         f2 := '';
         if Length(Edit1.Text) > 0 then begin
            if (ComboBox3.Text = '<') then TStr := '>' else TStr := '>=';
            if GISdb[db].MyData.GetFieldType(WantedField) in [ftDate] then TStr2 := QuotedStr(ptTrim(Edit1.Text))
            else TStr2 := Edit1.Text;
            f1 := GISdb[db].MyData.GetFieldName(WantedField) + TStr + TStr2;
         end;
         if (Length(Edit2.Text) > 0) then begin
            if (GISdb[db].MyData.GetFieldType(WantedField) in [ftDate]) then TStr2 := QuotedStr(ptTrim(Edit2.Text))
            else TStr2 := Edit2.Text;
            f2 := GISdb[db].MyData.GetFieldName(WantedField) + ComboBox2.Text + TStr2;
         end;
         if (f1 <> '') and (f2 <> '') then f3 := ' AND ' else f3 := '';
         f4 := f1 + f3 + f2;
         if (f1 <> '') and (f2 <> '') then f4 := '(' + f4 + ')';
      end
      else if (GISdb[db].MyData.GetFieldType(WantedField) in [ftString,ftDate]) then begin
         f4 := GISdb[db].MyData.GetFieldName(WantedField) + ComboBox2.Text + QuotedStr(ptTrim(Edit2.Text));
      end;
      if (f4 <> '') then begin
         Memo1.Lines.Add(f4);
      end;
   end;
   WantedField := -99;
   Edit1.Enabled := false;
   Edit2.Enabled := false;
   Edit1.Text := '';
   Edit2.Text := '';
   ComboBox1.Text := '';
   BitBtn5.Enabled := false;
   if QuickFilter then BitBtn1Click(Sender);
end;


procedure TdbFilterCreation.BitBtn10Click(Sender: TObject);
begin
   BitBtn1Click(Sender);
   GISdb[db].AddSymbolizationToLayerTable('');
end;

procedure TdbFilterCreation.BitBtn11Click(Sender: TObject);
begin
   Memo1.Lines.Add('USE=' + QuotedStr('Y'));
   BitBtn1Click(Sender);
end;

procedure TdbFilterCreation.BitBtn12Click(Sender: TObject);
begin
   ShowHourglassCursor;
   BitBtn2Click(Sender);
   BitBtn1Click(Sender);
end;

procedure TdbFilterCreation.BitBtn13Click(Sender: TObject);
begin
   GISdb[db].dbOpts.TimeFilter := '';
   Memo3.Lines.Clear;
end;

procedure TdbFilterCreation.BitBtn1Click(Sender: TObject);

   procedure BuildFilter;
   var
      i : integer;
      TStr : shortString;
   begin
      WantedFilter := '';
      for i := pred(Memo1.Lines.Count) downto 0 do begin
         TStr := ptTrim(Memo1.Lines[i]);
         if (TStr = '') then Memo1.Lines.Delete(i);
      end;

      for i := 0 to pred(Memo1.Lines.Count) do begin
         if (i = 0) then WantedFilter := Memo1.Lines[i]
         else WantedFilter := WantedFilter + ' AND ' + Memo1.Lines[i];
      end;
      if (WantedFilter <> '') then WantedFilter := '(' + WantedFilter + ')'
   end;

var
   i : integer;
begin
   BuildFilter;
   if (WantedFilter = '') and (GISdb[db].dbOpts.TimeFilter = '') and (GISdb[db].dbOpts.GeoFilter = '') and (Sender = BitBtn1) then if AnswerIsYes('No filter conditions selected; add condition being built') then begin
      BitBtn2Click(Sender);
      BuildFilter;
   end;
   GISdb[db].dbOpts.MainFilter := WantedFilter;
   GISdb[db].EmpSource.Enabled := false;
   GISdb[db].AssembleGISFilter;
   {$IfDef RecordFilterProblems}
      WriteLineToDebugFile('TdbFilterCreation.BitBtn1Click, MainFilter= "' + GISdb[db].dbOpts.MainFilter + '"');
      WriteLineToDebugFile('TdbFilterCreation.BitBtn1Click, Geofilter= "' + GISdb[db].dbOpts.GeoFilter + '"');
      WriteLineToDebugFile('TdbFilterCreation.BitBtn1Click, Timefilter= "' + GISdb[db].dbOpts.TimeFilter + '"');
   {$EndIf}
   if (Sender = BitBtn1) or (Sender = BitBtn11) or (Sender = BitBtn12) or QuickFilter then begin
      if MDDef.ApplyFilterToAllDBs then begin
          for i := 1 to MaxDataBase do begin
            if ValidDB(i) then begin
                GISdb[i].ApplyGISFilter(GISdb[db].MyData.Filter);
                //if (GISdb[i].theMapOwner <> Nil) then begin
                   GISdb[i].RedrawLayerOnMap;
               //end;
            end;
          end;
      end
      else begin
         //if (GISdb[db].theMapOwner <> Nil) then begin
           GISdb[db].RedrawLayerOnMap;
        //end;
      end;


      Close;
   end;
   GISdb[db].EmpSource.Enabled := true;
end;


procedure TdbFilterCreation.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caHide;
end;


procedure TdbFilterCreation.FormCreate(Sender: TObject);
begin
   BitBtn1.Enabled := false;
   CheckBox2.Checked := MDDef.DBfilterClearLayer;
   CheckBox3.Checked := MDDef.DBfilterRedrawOnClose;
   CheckBox9.Checked := MDDef.ApplyFilterToAllDBs;
   PlaceFormAtMousePosition(Self);
end;


procedure TdbFilterCreation.BitBtn3Click(Sender: TObject);
begin
   WantedFilter := '';
   Close;
end;


procedure TdbFilterCreation.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\db_filters.htm');
end;


procedure TdbFilterCreation.BitBtn4Click(Sender: TObject);
begin
   Memo1.Lines.Clear;
   BitBtn1.Enabled := false;
   BitBtn1Click(Sender);
end;


procedure TdbFilterCreation.Memo1Change(Sender: TObject);
begin
   BitBtn1.Enabled := true;
end;


procedure TdbFilterCreation.BitBtn6Click(Sender: TObject);
begin
   Memo2.Lines.Clear;
   GISdb[db].dbOpts.GeoFilter := '';
   BitBtn1Click(Sender);
end;

procedure TdbFilterCreation.BitBtn7Click(Sender: TObject);
var
   SL : tStringList;
   aFilter : shortstring;
   i : integer;
begin
   if (WantedFieldName = '') then begin
      MessageToContinue('Pick DB field first');
   end
   else if GISdb[db].MyData.GetFieldType(GISdb[db].MyData.GetFieldName(WantedField)) in [ftString,ftInteger,ftSmallInt,ftLargeInt] then begin
      GISdb[db].EmpSource.Enabled := false;
      sl := GISdb[db].MyData.UniqueEntriesInDB(WantedFieldName);
      GISdb[db].EmpSource.Enabled := true;
      PickSomeFromStringList(SL,'fields to add with OR');
      if (sl.Count > 1) then begin
         aFilter := '';
         for i := 0 to pred(sl.Count) do begin
            if GISdb[db].MyData.GetFieldType(GISdb[db].MyData.GetFieldName(WantedField)) in [ftString] then begin
               aFilter := AddOrIfNeeded(aFilter) + WantedFieldName + '=' + QuotedStr(sl.Strings[i]);
            end
            else begin
               aFilter := AddOrIfNeeded(aFilter) + WantedFieldName+ '=' + sl.Strings[i];
            end;
         end;
         Memo1.Lines.Add('(' + aFilter + ')');
      end;
      sl.Free;
   end;
end;


procedure TdbFilterCreation.ComboBox2Change(Sender: TObject);
begin
   Edit1.Enabled := (ComboBox2.Text = '<') or  (ComboBox2.Text = '<=');
   ComboBox3.Enabled := Edit1.Enabled;
end;

procedure TdbFilterCreation.Button1Click(Sender: TObject);
var
   i : integer;
begin
   ComboBox1.Items.Clear;
   with GISdb[db].MyData do for i := 0 to pred(FieldCount) do ComboBox1.Items.Add(GetFieldName(i));
   Button1.Visible := false;
end;

procedure TdbFilterCreation.BitBtn8Click(Sender: TObject);
begin
   BitBtn1Click(Sender);
end;



procedure TdbFilterCreation.FormResize(Sender: TObject);
begin
   Memo1.Width := ClientWidth;
   Memo2.Width := ClientWidth;
end;


initialization
finalization
{$IfDef RecordFilterProblems} WriteLineToDebugFile('RecordFilterProblems active in demdbfilter'); {$EndIf}
end.
