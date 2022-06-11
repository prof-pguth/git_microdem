unit quick_filter;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 1/25/2014       }
{_________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordQuickFilter}
{$EndIf}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.ComCtrls,
  db;

type
  TQuickFilterForm = class(TForm)
    Panel6: TPanel;
    qfLabel7: TLabel;
    qfLabel8: TLabel;
    qfCheckBox2: TCheckBox;
    qfCheckBox3: TCheckBox;
    BitBtn24: TBitBtn;
    BitBtn25: TBitBtn;
    qfRadioGroup1: TRadioGroup;
    CheckBox4: TCheckBox;
    qf1ComboBox10: TComboBox;
    qf2ComboBox9: TComboBox;
    GroupBox1: TGroupBox;
    BitBtn27: TBitBtn;
    BitBtn26: TBitBtn;
    qfBitBtn1: TBitBtn;
    qfBitBtn2: TBitBtn;
    qfBitBtn3: TBitBtn;
    qfBitBtn4: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure qf1ComboBox10Change(Sender: TObject);
    procedure qf2ComboBox9Change(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure BitBtn25Click(Sender: TObject);
    procedure BitBtn26Click(Sender: TObject);
    procedure BitBtn27Click(Sender: TObject);
    procedure BitBtn24Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure qfBitBtn1Click(Sender: TObject);
    procedure qfBitBtn2Click(Sender: TObject);
    procedure qfBitBtn3Click(Sender: TObject);
    procedure qfBitBtn4Click(Sender: TObject);
  private
    procedure Refilter;
    { Private declarations }
  public
    { Public declarations }
     DBonTable : integer;
     Refiltering : boolean;
  end;


procedure StartQuickFilter(DB : integer; TwoFields : boolean = true);


implementation

{$R *.dfm}


uses
   Petmar,Petmar_types, PetDButils,PetImage,
   DEMMapf,DEMDefs,DEMDataBase,
   Nevadia_Main;


procedure TQuickFilterForm.Refilter;
var
   ff,f1,f2,tstr : shortstring;
   i : integer;
begin
   if Refiltering then exit;
   {$IfDef RecordQuickFilter}
   WriteLineToDebugFile('TQuickFilterForm.Refilter in');
   {$EndIf}
   Refiltering := true;
   with GISdb[DBonTable] do begin
      if (qf1ComboBox10.Text <> '') then begin
         if MyData.GetFieldType(dbOpts.QFField1) = ftString then TStr := QuotedStr(ptTrim(qf1ComboBox10.Text))
         else TStr := ptTrim(qf1ComboBox10.Text);
         f1 := dbOpts.QFfield1 + '=' + TStr;
      end
      else f1 := '';

      if (qf2ComboBox9.Text <> '') then begin
         if MyData.GetFieldType(dbOpts.QFField2) = ftString then TStr :=  QuotedStr(ptTrim(qf2ComboBox9.Text))
         else TStr := ptTrim(qf2ComboBox9.Text);
         f2 := dbOpts.QFfield2 + '=' +  TStr;
      end
      else f2 := '';

      if (f1 = '') and (f2 = '') then begin
         dbOpts.MainFilter := '';
      end
      else begin
         ff := f1;
         if (f2 <> '') then begin
            if (qfRadioGroup1.ItemIndex = 0) then ff := PetDBUtils.AddAndIfNeeded(ff) + f2
            else ff := PetDBUtils.AddOrIfNeeded(ff) + f2;
         end;
         dbOpts.MainFilter := ff;
      end;
      GISdb[DBonTable].EmpSource.Enabled := false;
      AssembleGISFilter;
      {$IfDef RecordQuickFilter}
      WriteLineToDebugFile('Filter applied');
      {$EndIf}
      if MDDef.ApplySameFilterAllDBs then ShowOtherDBsWithSameFilter(qfCheckBox2.Checked,qfCheckBox3.Checked,true)
      else GISdb[DBonTable].dbTableF.ShowFilteredDB(qfCheckBox2.Checked,qfCheckBox3.Checked);

      {$IfDef NoDBGraphs}
      {$Else}
      if (GISdb[DBonTable].LastGraph <> Nil) then begin
         //GISdb[DBonTable].LastGraph.Close;
         GISdb[DBonTable].ActuallyDrawGraph(GISdb[DBonTable].LastGraphtype);
      end;
      {$EndIf}
   end;
   Refiltering := false;

   {$IfDef RecordQuickFilter}
   WriteLineToDebugFile('TQuickFilterForm.Refilter out');
   {$EndIf}
end;


procedure StartQuickFilter(DB : integer; TwoFields : boolean = true);
var
  QuickFilterForm : TQuickFilterForm;
  i : integer;
begin
   GISdb[DB].dbOpts.QFField1 := PickField(GISdb[DB].MyData,GISdb[DB].LinkTable,GISdb[DB].dbOpts.VisCols,'First field',[ftString,ftInteger,ftSmallInt]);
   GISdb[DB].dbOpts.QFField2 := '';
   if (GISdb[DB].dbOpts.QFField1 <> '') then begin
      with QuickFilterForm do begin
         QuickFilterForm := tQuickFilterForm.Create(Application);
         QuickFilterForm.DBonTable := DB;
         QuickFilterForm.Caption := 'Quick filter ' + GISdb[DB].dbName;
         wmDEM.FormPlacementInCorner(QuickFilterForm,lpNEMap);
         qfLabel7.Caption := GISdb[DB].dbOpts.QFField1;
         GISdb[DB].FillComboBoxFromField(qf1ComboBox10,GISdb[DB].dbOpts.QFField1);
         qf1ComboBox10.Enabled := true;
         if TwoFields then GISdb[DB].dbOpts.QFField2 := PickField(GISdb[DB].MyData,GISdb[DB].LinkTable,GISdb[DB].dbOpts.VisCols,'Second field',[ftString,ftInteger,ftSmallInt]);
         if (GISdb[DB].dbOpts.QFField2 = '') then begin
            qfLabel8.Visible := false;
            qf2ComboBox9.Visible := false;
            qfBitBtn3.Visible := false;
            qfBitBtn4.Visible := false;
         end
         else begin
            qfLabel8.Caption := GISdb[DB].dbOpts.QFField2;
            GISdb[DB].FillComboBoxFromField(qf2ComboBox9,GISdb[DB].dbOpts.QFField2);
            qf2ComboBox9.Enabled := true;
        end;
         {$IfDef NoDBGrafs}
         {$Else}
         if (GISdb[DB].theGraphOwner <> Nil) then CopyImageToBitmap(GISdb[DB].TheGraphOwner.Image1,GISdb[DB].dbTablef.GraphOwnerBitmap);
         {$EndIf}
         {$IfDef RecordQuickFilterProblems}
         WriteLineToDebugFile('Tdbtablef.Quickfiltering1Click' + ' ComboBox8.Items.Count=' + IntToStr(ComboBox10.Items.Count) + ' qf2ComboBox9.Items.Count=' + IntToStr(qf2ComboBox9.Items.Count));
         {$EndIf}
      end;
      QuickFilterForm.Show;
   end;
end;


procedure TQuickFilterForm.qfBitBtn1Click(Sender: TObject);
begin
   if qf1ComboBox10.ItemIndex < qf1ComboBox10.Items.Count -1 then qf1ComboBox10.ItemIndex := qf1ComboBox10.ItemIndex + 1
   else qf1ComboBox10.ItemIndex := 0;
   Refilter;
end;


procedure TQuickFilterForm.BitBtn24Click(Sender: TObject);
begin
   GISdb[DBonTable].dbTableF.ShowFilteredDB(qfCheckBox2.Checked,qfCheckBox3.Checked);
end;

procedure TQuickFilterForm.BitBtn25Click(Sender: TObject);
begin
   EditMyFont(GISdb[DBonTable].dbOpts.GisLabelFont1);
end;

procedure TQuickFilterForm.BitBtn26Click(Sender: TObject);
begin
   GISdb[DBonTable].MyData.Prior;
   GISdb[DBonTable].dbtablef.Highlightrecordonmap1Click(Nil);
end;

procedure TQuickFilterForm.BitBtn27Click(Sender: TObject);
begin
   GISdb[DBonTable].MyData.Next;
   GISdb[DBonTable].dbtablef.Highlightrecordonmap1Click(Nil);
end;

procedure TQuickFilterForm.qfBitBtn2Click(Sender: TObject);
begin
   if (qf1ComboBox10.ItemIndex > 0) then qf1ComboBox10.ItemIndex := qf1ComboBox10.ItemIndex - 1
   else qf1ComboBox10.ItemIndex := qf1ComboBox10.Items.Count - 1;
   Refilter;
end;

procedure TQuickFilterForm.qfBitBtn3Click(Sender: TObject);
begin
   if qf2ComboBox9.ItemIndex < qf2ComboBox9.Items.Count -1 then qf2ComboBox9.ItemIndex := qf2ComboBox9.ItemIndex + 1
   else qf2ComboBox9.ItemIndex := 0;
   Refilter;
end;

procedure TQuickFilterForm.qfBitBtn4Click(Sender: TObject);
begin
   if qf2ComboBox9.ItemIndex > 0  then qf2ComboBox9.ItemIndex := qf2ComboBox9.ItemIndex - 1
   else qf2ComboBox9.ItemIndex := qf2ComboBox9.Items.Count - 1;
   Refilter;
end;

procedure TQuickFilterForm.CheckBox4Click(Sender: TObject);
begin
   MDDef.ApplySameFilterAllDBs := CheckBox4.Checked;
end;

procedure TQuickFilterForm.qf1ComboBox10Change(Sender: TObject);
begin
   Refilter;
end;

procedure TQuickFilterForm.qf2ComboBox9Change(Sender: TObject);
begin
   Refilter;
end;

procedure TQuickFilterForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TQuickFilterForm.FormCreate(Sender: TObject);
begin
   CheckBox4.Checked := MDDef.ApplySameFilterAllDBs;
   Refiltering := false;
end;

initialization
finalization
   {$IfDef RecordQuickFilter}
   WriteLineToDebugFile('RecordQuickFilter on in quick_filter');
   {$EndIf}
end.
