unit demix_evals_scores_graphs;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}


//{$Define ConvertDBFtoDB}


{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      {$Define RecordDEMIX}
      //{$Define RecordChangeDB}
      {$Define RecordDEMIXByLandCover
      {$Define RecordGraphCaption}
   {$Else}
   {$EndIf}
{$EndIf}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics,StrUtils,Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  Petmar_types,BaseGraf;


type
  Teval_scores_graph_form = class(TForm)
    RadioGroup2: TRadioGroup;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    BitBtn3: TBitBtn;
    Label1: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Edit3: TEdit;
    BitBtn5: TBitBtn;
    RadioGroup4: TRadioGroup;
    BitBtn6: TBitBtn;
    RadioGroup5: TRadioGroup;
    BitBtn12: TBitBtn;
    CheckBox4: TCheckBox;
    //BitBtn15: TBitBtn;
    BitBtn16: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn17: TBitBtn;
    BitBtn23: TBitBtn;
    Edit4: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Edit5: TEdit;
    BitBtn27: TBitBtn;
    RadioGroup7: TRadioGroup;
    BitBtn28: TBitBtn;
    BitBtn29: TBitBtn;
    BitBtn30: TBitBtn;
    GroupBox1: TGroupBox;
    Memo1: TMemo;
    BitBtn21: TBitBtn;
    BitBtn20: TBitBtn;
    BitBtn19: TBitBtn;
    BitBtn18: TBitBtn;
    GroupBox2: TGroupBox;
    Memo3: TMemo;
    BitBtn33: TBitBtn;
    BitBtn34: TBitBtn;
    BitBtn35: TBitBtn;
    Memo4: TMemo;
    ComboBox1: TComboBox;
    Edit6: TEdit;
    Label6: TLabel;
    RadioGroup8: TRadioGroup;
    RadioGroup9: TRadioGroup;
    BitBtn22: TBitBtn;
    RadioGroup1: TRadioGroup;
    GroupBox3: TGroupBox;
    Memo2: TMemo;
    GroupBox4: TGroupBox;
    BitBtn4: TBitBtn;
    BitBtn10: TBitBtn;
    GroupBox5: TGroupBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    BitBtn1: TBitBtn;
    RadioGroup3: TRadioGroup;
    RadioGroup6: TRadioGroup;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn38: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtn11: TBitBtn;
    procedure RadioGroup3Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure RadioGroup4Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure RadioGroup5Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    //procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure BitBtn19Click(Sender: TObject);
    procedure BitBtn21Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure BitBtn23Click(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure RadioGroup6Click(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure BitBtn20Click(Sender: TObject);
    procedure BitBtn27Click(Sender: TObject);
    procedure RadioGroup7Click(Sender: TObject);
    procedure BitBtn28Click(Sender: TObject);
    procedure BitBtn29Click(Sender: TObject);
    procedure BitBtn30Click(Sender: TObject);
    procedure BitBtn33Click(Sender: TObject);
    procedure BitBtn34Click(Sender: TObject);
    procedure BitBtn35Click(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
    procedure RadioGroup8Click(Sender: TObject);
    procedure RadioGroup9Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn22Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn38Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
  private
    { Private declarations }
    procedure ChangeDBonForm(Newdb : integer);
    function AssembleDEMlist : tStringList;
    function MakeCriteriaList : tStringList;
    function MakeGeomorphFilters: tStringList;
    function MakeCriteriaFilter(UseCriteria : tStringList) : ShortString;
    procedure LoadDEMsInMemo;
  public
    { Public declarations }
     db_U10,db_u80,db_u120,db_Full,
     db : integer;
     UseDEMs : tStringList;
  end;

var
  eval_scores_graph_form : Teval_scores_graph_form;


procedure StartDEMIXgraphs(DB : integer);

implementation

{$R *.dfm}

uses
   Nevadia_main,
   Petmar,Petmar_db,Petimage_form,PetImage,petdbutils,
   DEMdataBase,
   Toggle_db_use,
   DEMdefs,
   DEMdef_routines,
   DEMIX_graphs,
   DEMIX_control,
   DEMIX_definitions;


procedure StartDEMIXgraphs(DB : integer);
var
  eval_scores_graph_form : Teval_scores_graph_form;
  i : integer;
   FieldsInDB : tStringList;
begin
   GetDEMIXpaths(false,db);
   //GISdb. := ID_DEMIX_DB_type(db);
   eval_scores_graph_form := Teval_scores_graph_form.Create(Application);
   if (db = -4) then begin
      eval_scores_graph_form.BitBtn4Click(nil);
      db := eval_scores_graph_form.db;
   end;

   RecognizeDEMIXVersion(DB);
   //SetParamsForDEMIXmode;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('StartDEMIXgraphs after RecognizeDEMIXVersion, NumDEMIXtestDEM=' + IntToStr(NumDEMIXtestDEM)); {$EndIf}
   eval_scores_graph_form.LoadDEMsInMemo;
   GISdb[db].dbtablef.OpenDEMIXgraphs1.Enabled := false;

   eval_scores_graph_form.ChangeDBonForm(db);
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('StartDEMIXgraphs after change DB, NumDEMIXtestDEM=' + IntToStr(NumDEMIXtestDEM)); {$EndIf}

   LoadDEMIXnames;


   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('StartDEMIXgraphs after load, NumDEMIXtestDEM=' + IntToStr(NumDEMIXtestDEM)); {$EndIf}
   eval_scores_graph_form.LoadDEMsInMemo;
   eval_scores_graph_form.BitBtn9Click(nil);

   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('StartDEMIXgraphs after LoadDEMsInMemo, NumDEMIXtestDEM=' + IntToStr(NumDEMIXtestDEM)); {$EndIf}

   GetFields(GISdb[DB].MyData,GISdb[DB].dbOpts.VisCols,NumericFieldTypes,FieldsInDB);
   if (FieldsInDB <> Nil) and (FieldsInDB.Count > 0) then begin
      eval_scores_graph_form.ComboBox2.Items := FieldsInDB;
      eval_scores_graph_form.ComboBox3.Items := FieldsInDB;
      eval_scores_graph_form.ComboBox4.Items := FieldsInDB;
      FieldsInDB.Destroy;
   end;

   eval_scores_graph_form.Show;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('StartDEMIXgraph out, NumDEMIXtestDEM=' + IntToStr(NumDEMIXtestDEM)  + '  show user DEMs=' + IntToStr(eval_scores_graph_form.Memo3.Lines.Count)); {$EndIf}
end;


procedure Teval_scores_graph_form.LoadDEMsInMemo;
var
   i : integer;
   theDEMs : tStringList;
begin
   if ValidDB(db) then begin
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('LoadDEMsInMem in, NumDEMIXtestDEM=' + IntToStr(NumDEMIXtestDEM)); {$EndIf}
      Memo3.Clear;
      theDEMs := GetListOfTestDEMsinUse;
      for I := 0 to pred(theDEMs.Count) do begin
         Memo3.Lines.Add(theDEMs.Strings[i]);
      end;
      theDEMs.Destroy;
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Teval_scores_graph_form.LoadDEMsInMem, show user DEMs=' + IntToStr(Memo3.Lines.Count)); {$EndIf}
   end;
end;


procedure Teval_scores_graph_form.ChangeDBonForm(Newdb : integer);
var
   i : integer;
   //fName : shortstring;
begin
   if ValidDB(NewDB) then begin
      {$IfDef RecordChangeDB} WriteLineToDebugFile('ChangeDBonForm in OldDB=' + IntToStr(db) + '  NewDB=' + IntToStr(NewDB)); {$EndIf}
      RecognizeDEMIXVersion(NewDB);
      {$IfDef RecordChangeDB} WriteLineToDebugFile('ChangeDBonForm recognized ' + GISdb[Newdb].dbName + ' ' + DEMIXModeName + '  ' + CriteriaFamily); {$EndIf}

      Caption := 'DEMIX graphs: ' + GISdb[Newdb].dbName;
      GISdb[Newdb].EmpSource.Enabled := false;
      if GISdb[Newdb].MyData.FieldExists('CRITERION') then begin
         Memo1.Lines := GISdb[Newdb].MyData.ListUniqueEntriesInDB('CRITERION');
      end;
      db := NewDB;
      RadioGroup4.Enabled := GISdb[DB].MyData.FieldExists('ELEV_SSIM');
      {$IfDef RecordChangeDB} WriteLineToDebugFile('ChangeDBonForm out ' + GISdb[db].dbName + ' ' + DEMIXModeName + '  ' + CriteriaFamily); {$EndIf}
   end;
end;


function Teval_scores_graph_form.AssembleDEMlist : tStringList;
var
   j : integer;
begin
   Result := tStringList.Create;
   for j := 0 to pred(Memo3.Lines.Count) do begin
      Result.Add(Memo3.Lines[j]);
   end;
end;

function Teval_scores_graph_form.MakeCriteriaList : tStringList;
var
   j : integer;
begin
   Result := tStringList.Create;
   for j := 0 to pred(Memo1.Lines.Count) do begin
      Result.Add(Memo1.Lines[j]);
   end;
end;

function Teval_scores_graph_form.MakeCriteriaFilter(UseCriteria : tStringList) : shortString;
var
   j : integer;
begin
   Result := '';
   if (UseCriteria.Count > 0) then begin
     Result := 'CRITERION=' + QuotedStr(UseCriteria.Strings[0]);
     if (UseCriteria.Count > 1) then begin
        for j := 1 to pred(UseCriteria.Count) do begin
           Result := Result + ' OR CRITERION=' + QuotedStr(UseCriteria.Strings[j]);
        end;
     end;
     Result := '(' + Result + ')';
   end;
end;


procedure Teval_scores_graph_form.BitBtn10Click(Sender: TObject);
//graphs for each of the elevation range DBs, win/loss versus basecompareDEM
var
   i,j,k,ColBigBitmap : integer;
   HL : shortstring;
   aName : PathStr;
   Legend,BigBitmap : tMyBitmap;
   GeomorphFilters,DEMList: tStringList;


         procedure DoElevationRange(adb : integer; BaseCompareDEM,aFilter : shortstring);
         var
            gr : tThisBaseGraph;
         begin
            ChangeDBonForm(adb);
            {$IfDef RecordDEMIX} WriteLineToDebugFile('DoElevationRange DEM ' + BaseCompareDEM +  ' ' + DEMIXModeName + ' DEMs=' + IntToStr(DEMList.Count)); {$EndIf}
            GISdb[db].ApplyGISFilter(aFilter);
            if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
               if GISdb[db].MyData.FieldExists(BaseCompareDEM) then begin
                  HL := DEMIXModeName + ' ' + BaseCompareDEM + ' ' + GISdb[DB].MyData.Filter + ' (tiles=' + IntToStr(GISdb[DB].NumUniqueEntriesInDB('DEMIX_TILE')) + ')';
                  {$IfDef RecordDEMIX} WriteLineToDebugFile(HL); {$EndIf}
                  inc(ColBigBitmap);
                  gr := WinningPercentagesComparedToCOP(db,BaseCompareDEM,MakeCriteriaList,DEMList,HL);
                  AddGraphToBigBitmap(ColBigBitmap,pred(DEMList.Count),1,gr,BigBitmap);
                  {$IfDef RecordDEMIX} WriteLineToDebugFile('Added '+ ' DEMs=' + IntToStr(DEMList.Count)); {$EndIf}
                  gr.Destroy;
                  {$IfDef RecordDEMIX} WriteLineToDebugFile('DoElevationRange out, DEMs=' + IntToStr(DEMList.Count)); {$EndIf}
               end
               else begin
                  {$IfDef RecordDEMIX} WriteLineToDebugFile('DEM ' + BaseCompareDEM +  ' not in ' + DEMIXModeName); {$EndIf}
               end;
            end
            else begin
               {$IfDef RecordDEMIX} WriteLineToDebugFile('DoElevationRange, no filter matches for ' + aFilter); {$EndIf}
            end;
         end;


begin
  {$IfDef RecordDEMIX} HighlightLineToDebugFile('Teval_scores_graph_form.BitBtn10Click in'); {$EndIf}
   if GISdb[db].MyData.FieldExists('AVG_SLOPE') then begin
      BitBtn4Click(Sender);   //If needed loads db_U10,db_U80,db_U120,db_Full))
      GeomorphFilters := MakeGeomorphFilters;
      DEMList := AssembleDEMlist;
      for i := 0 to pred(GeomorphFilters.Count) do begin
        {$IfDef RecordDEMIX} WriteLineToDebugFile('DEMs=' + IntToStr(DEMList.Count)); {$EndIf}
         for j := 0 to pred(DEMList.Count) do begin
           {$IfDef RecordDEMIX} WriteLineToDebugFile('j=' + IntToStr(j) + '  ' + DEMList[j] +'  DEMs=' + IntToStr(DEMList.Count)); {$EndIf}
           ColBigBitmap := 0;
           CreateBitmap(BigBitmap,1200,4500);
           if ValidDB(db_U10) then DoElevationRange(db_U10,DEMlist[j],GeomorphFilters[i]);
           if ValidDB(db_U80) then DoElevationRange(db_U80,DEMlist[j],GeomorphFilters[i]);
           if ValidDB(db_U120) then DoElevationRange(db_U120,DEMlist[j],GeomorphFilters[i]);
           if ValidDB(db_Full) then DoElevationRange(db_Full,DEMlist[j],GeomorphFilters[i]);
           {$IfDef RecordDEMIX} WriteLineToDebugFile('DEMs=' + IntToStr(DEMList.Count)); {$EndIf}
           if (ColBigBitmap > 0) then begin
              Legend := DEMIXTestDEMLegend(true, DEMList);
              aName := MDTempDir + 'win_lose_tie_' + DEMlist[j] + '.png';
              FinishBigMap(BigBitmap,Legend,aname);
           end
           else BigBitmap.Destroy;
           {$IfDef RecordDEMIX} WriteLineToDebugFile('j=' + IntToStr(j) + '  Saved, ' + aName + '  DEMs=' + IntToStr(DEMList.Count)); {$EndIf}
         end;
         {$IfDef RecordDEMIX} WriteLineToDebugFile('Done DEMs loop'); {$EndIf}
      end;
      GISdb[db].ClearGISFilter;
      GISdb[db].ShowStatus;
      GeomorphFilters.Destroy;
      DEMList.Destroy;
   end
   else MessageToContinue('Add tile characters to DB');
end;



procedure Teval_scores_graph_form.BitBtn11Click(Sender: TObject);
begin
   VerifyRecordsToUse(DemixSettingsDir + 'demix_dems.dbf','SHORT_NAME');
   LoadDEMsInMemo;
end;

procedure Teval_scores_graph_form.BitBtn12Click(Sender: TObject);
begin
   WhiskerPlotsByCluster(DB);
end;

procedure Teval_scores_graph_form.BitBtn16Click(Sender: TObject);
begin
   DEMIX_evaluations_graph(DB,yasBestEvalColoredBySlope,AssembleDEMList,MakeCriteriaList,true);
end;

function Teval_scores_graph_form.MakeGeomorphFilters : tStringList;
var
   i : integer;
begin
   Result := tStringList.Create;
   for i := 0 to pred(Memo2.Lines.Count) do begin
      if Memo2.Lines[i] = '(None)' then Result.Add('')
      else Result.Add(Memo2.Lines[i]);
   end;
end;


procedure Teval_scores_graph_form.BitBtn17Click(Sender: TObject);
begin
   FilterJustOneGraph(DB,MakeCriteriaList,MakeGeomorphFilters,true);
end;

procedure Teval_scores_graph_form.BitBtn18Click(Sender: TObject);
var
   pn : integer;
   Crits : tStringList;
begin
    GISdb[DB].EmpSource.Enabled := false;
    Crits := GISdb[DB].MyData.ListUniqueEntriesInDB('CRITERION');
    if (Sender = BitBtn21) or (Crits.Count = 1) or MultiSelectSingleColumnStringList('Criteria to graph',pn,Crits,true,true) then begin
       Memo1.Lines.Clear;
       Memo1.Lines := Crits;
    end;
end;

procedure Teval_scores_graph_form.BitBtn19Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := DEMIXSettingsDir;
   if GetExistingFileName('DEMIX tiles','tile*.txt',fName) then begin
      Memo1.Lines.LoadFromFile(fName);
   end;
end;

procedure Teval_scores_graph_form.BitBtn1Click(Sender: TObject);
begin
   BestBySlopeRough(db,MakeCriteriaList,True,ComboBox2.Text,ComboBox3.Text,ComboBox4.Text);
end;

procedure Teval_scores_graph_form.BitBtn20Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := DEMIXSettingsDir;
   if GetFileNameDefaultExt('list of criteria','*.txt',fName) then Memo1.Lines.SaveToFile(fName);
end;

procedure Teval_scores_graph_form.BitBtn21Click(Sender: TObject);
begin
   BitBtn18Click(Sender);
end;

procedure Teval_scores_graph_form.BitBtn22Click(Sender: TObject);
begin
   DisplayBitmap(DEMIXTestDEMLegend(true, AssembleDEMList,MDDef.DEMIX_xsize));
end;

procedure Teval_scores_graph_form.BitBtn23Click(Sender: TObject);
begin
   AllGraphsOneImage(MDDef.NumGraphCols);
end;

procedure Teval_scores_graph_form.BitBtn27Click(Sender: TObject);
begin
   LastDataBase := DEMIX_final_DB_dir;
   db := OpenMultipleDataBases('New DB for DEMIX graphs');
   ChangeDBonForm(db);
end;


procedure Teval_scores_graph_form.BitBtn28Click(Sender: TObject);
begin
   BestEvalGraphPerCriterionMultipleFilters(db,MakeGeomorphFilters,MakeCriteriaList,CriteriaFamily);
end;


procedure Teval_scores_graph_form.BitBtn29Click(Sender: TObject);
begin
   WinningComparedToBaseDEM(db,ComboBox1.Text,MakeGeomorphFilters,MakeCriteriaList,AssembleDEMList);
end;


procedure Teval_scores_graph_form.BitBtn2Click(Sender: TObject);
begin
   CombineAllPanelGraphs;
end;

procedure Teval_scores_graph_form.BitBtn30Click(Sender: TObject);
begin
   GISdb[db].dbtablef.BringToFront;
end;

procedure Teval_scores_graph_form.BitBtn33Click(Sender: TObject);
begin
  TileCharateristicsWhiskerPlotsByCluster(DB,false,MakeGeomorphFilters);
end;


procedure Teval_scores_graph_form.BitBtn34Click(Sender: TObject);
var
   Filters : tStringList;
   aFilter : shortstring;
   i : integer;
begin
   aFilter := GISdb[DB].MyData.Filter;
   if GISdb[DB].MyData.FieldExists('CRITERION') then GISdb[DB].ApplyGISFilter('CRITERION=' + QuotedStr('ELEV_FUV'));
   Filters := tStringList.Create;
   for i := 1 to 15 do begin  //loop through clusters, quantiles
      Filters.Add('CLUSTER=' + IntToStr(i));
   end;
   TileCharateristicsWhiskerPlotsByCluster(DB,false,Filters);
   GISdb[DB].MyData.Filter := aFilter;
end;


procedure Teval_scores_graph_form.BitBtn35Click(Sender: TObject);
var
   TheFilters : tStringList;
   i,j : integer;
begin
   TheFilters := tStringList.Create;
   for I := 0 to pred(Memo3.Lines.Count) do
      for j := 0 to pred(Memo4.Lines.Count) do
         TheFilters.Add('DEM=' + QuotedStr(Memo3.Lines[i]) + ' AND ' + Memo4.Lines[j]);
   ClusterMapLocation(DB, TheFilters);
   TheFilters.Destroy;
end;

procedure Teval_scores_graph_form.BitBtn38Click(Sender: TObject);
begin
   SaveMDDefaults;
end;

procedure Teval_scores_graph_form.BitBtn3Click(Sender: TObject);
begin
   wmdem.Closeallgraphs1Click(Sender);
   wmdem.Closeallpictureviewwindows1Click(Sender);
end;


procedure Teval_scores_graph_form.BitBtn4Click(Sender: TObject);

   procedure TryOne(Which : shortString; var db_num : integer; var fName : PathStr);
   begin
      if not ValidDB(db_num) then begin
         if FileExists(fName) then begin
            OpenNumberedGISDataBase(db_num,fName,True);
         end
         else begin
            db_num := OpenMultipleDataBases(Which + ' DB for DEMIX graphs');
            fName := GISdb[db_num].dbFullName;
         end;
      end;
   end;

begin
   LastDataBase := DEMIX_final_DB_dir;
   if MDdef.DEMIX_AllowCoastal then begin
     TryOne('U10',db_U10,MDDef.DEMIX_U10DBfName);
     TryOne('U80',db_U80,MDDef.DEMIX_U80DBfName);
     TryOne('U120',db_U120,MDDef.DEMIX_U120DBfName);
   end;
   TryOne('FULL',db_Full,MDDef.DEMIX_FullDBfName);
   db := db_Full;
   ChangeDBonForm(db_Full);
end;

procedure Teval_scores_graph_form.BitBtn5Click(Sender: TObject);
begin
   SSIM_FUV_scatterplot(db,RadioGroup4.Items[RadioGroup4.ItemIndex]);
end;

procedure Teval_scores_graph_form.BitBtn6Click(Sender: TObject);
begin
   HistogramsAllCriteria(db);
end;


procedure Teval_scores_graph_form.BitBtn7Click(Sender: TObject);
begin
   Memo2.Lines.Clear;
   Memo2.Lines.Add('(None)');
end;

procedure Teval_scores_graph_form.BitBtn8Click(Sender: TObject);
var
   i,j,k,ColBigBitmap : integer;
   HL : shortstring;
   aName : PathStr;
   Legend,BigBitmap : tMyBitmap;
begin
     {$IfDef RecordDEMIX} HighlightLineToDebugFile('Teval_scores_graph_form.BitBtn8Click in'); {$EndIf}
      BitBtn4Click(Sender);   //If needed loads db_U10,db_U80,db_U120,db_Full))
      if ValidDB(db_Full) then BestEvalGraphPerCriterionMultipleFilters(db_Full,MakeGeomorphFilters,MakeCriteriaList,'Full');
      if MDdef.DEMIX_AllowCoastal then begin
        if ValidDB(db_U120) then BestEvalGraphPerCriterionMultipleFilters(db_U120,MakeGeomorphFilters,MakeCriteriaList,'U120');
        if ValidDB(db_U80) then BestEvalGraphPerCriterionMultipleFilters(db_U80,MakeGeomorphFilters,MakeCriteriaList,'U80');
        if ValidDB(db_U10) then BestEvalGraphPerCriterionMultipleFilters(db_U10,MakeGeomorphFilters,MakeCriteriaList,'U10');
      end;
end;

procedure Teval_scores_graph_form.BitBtn9Click(Sender: TObject);
var
   sl,sl2 : tStringList;
   i : integer;
   fName : shortstring;
begin
   Memo1.Clear;
   sl := OpenFUVOrderedParams;
   GISdb[db].DBFieldUniqueEntries('CRITERION',sl2);
   for I := 0 to pred(Sl.count) do begin
      fName := sl[i] + '_FUV';
      if sl2.IndexOf(fName) <> -1 then Memo1.Lines.Add(fName);
   end;
   sl.Destroy;
   sl2.Destroy;
end;

procedure Teval_scores_graph_form.CheckBox1Click(Sender: TObject);
begin
   MDDef.DEMIX_combined_graph := CheckBox1.Checked;
end;

procedure Teval_scores_graph_form.CheckBox2Click(Sender: TObject);
begin
   MDDef.PanelsByTestDEM := CheckBox2.Checked;
end;

procedure Teval_scores_graph_form.CheckBox3Click(Sender: TObject);
begin
   MovieByTestDEM := CheckBox3.Checked;
end;

procedure Teval_scores_graph_form.CheckBox4Click(Sender: TObject);

begin
   MDDef.DEMIX_graph_Retired_DEMs := CheckBox4.Checked;
   LoadDEMsInMemo;
end;

procedure Teval_scores_graph_form.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text, MDDef.DEMIXlegendFontSize);
end;

procedure Teval_scores_graph_form.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text, MDDef.DEMIX_ysize);
end;

procedure Teval_scores_graph_form.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text, MDDef.DEMIX_xsize);
end;

procedure Teval_scores_graph_form.Edit4Change(Sender: TObject);
begin
   CheckEditString(Edit4.Text,MDDef.DEMIXUseBins);
end;

procedure Teval_scores_graph_form.Edit5Change(Sender: TObject);
begin
   CheckEditString(Edit5.Text,MDDef.NumGraphCols);
end;

procedure Teval_scores_graph_form.Edit6Change(Sender: TObject);
begin
   CheckEditString(Edit6.Text,MDDef.DefaultGraphFont.Size);
end;

procedure Teval_scores_graph_form.FormActivate(Sender: TObject);
begin
   RecognizeDEMIXVersion(DB);
end;

procedure Teval_scores_graph_form.FormCreate(Sender: TObject);
begin
   CheckBox1.Checked := MDDef.DEMIX_combined_graph;
   CheckBox2.Checked := MDDef.PanelsByTestDEM;
   CheckBox3.Checked := MovieByTestDEM;
   CheckBox4.Checked := MDDef.DEMIX_graph_Retired_DEMs;
   Edit1.Text := IntToStr(MDDef.DEMIXlegendFontSize);
   Edit2.Text := IntToStr(MDDef.DEMIX_ysize);
   Edit3.Text := IntToStr(MDDef.DEMIX_xsize);
   Edit4.Text := IntToStr(MDDef.DEMIXUseBins);
   Edit5.Text := IntToStr(MDDef.NumGraphCols);
   Edit6.Text := IntToStr(MDDef.DefaultGraphFont.Size);

   RadioGroup5.ItemIndex := MDDef.DemixSymSize - 1;

   RadioGroup3.ItemIndex := MDDef.SummarySymbol;
   RadioGroup6.ItemIndex := MDDef.TwoParameterVisualization;
   RadioGroup7.ItemIndex := MDDef.DEMIX_groupWonLost;

   db_U10 := 0;
   db_u80 := 0;
   db_u120 := 0;
   db_Full := 0;
   Width := 1000;
end;

procedure Teval_scores_graph_form.RadioGroup1Click(Sender: TObject);
begin
  SetColorForProcessing;
  Self.Visible := false;
  Case RadioGroup1.ItemIndex of
     0 : DEMIX_evaluations_graph(DB,yasBestEval,AssembleDEMList,MakeCriteriaList,true);
     1 : DEMIX_evaluations_graph(DB,yasSlope,AssembleDEMList,MakeCriteriaList,true);
     2 : DEMIX_evaluations_graph(DB,yasRuff,AssembleDEMList,MakeCriteriaList,true);
     3 : DEMIX_evaluations_graph(DB,yasRelief,AssembleDEMList,MakeCriteriaList,true);
     4 : DEMIX_evaluations_graph(DB,yasForest,AssembleDEMList,MakeCriteriaList,true);
     5 : DEMIX_evaluations_graph(DB,yasBarren,AssembleDEMList,MakeCriteriaList,true);
  End;
  RadioGroup1.ItemIndex := -1;
  SetColorForWaiting;
  Self.Visible := true;
end;


procedure Teval_scores_graph_form.RadioGroup2Click(Sender: TObject);
begin
  if (not ContinueExperimentalDEMIX) then exit;

  SetColorForProcessing;
  Self.Visible := false;
  Case RadioGroup2.ItemIndex of
     0 : DEMIX_AreaAverageScores_graph(DB,AssembleDEMList);
     1 : GraphAverageScoresByTile(DB,AssembleDEMList,Nil,Nil);
     2 : DEMIX_AreaAverageScores_graph(DB,AssembleDEMList,false);
     3 : DEMIX_Area_ind_criteria_graph(DB,AssembleDEMList);
  End;
   RadioGroup2.ItemIndex := -1;
   SetColorForWaiting;
   Self.Visible := true;
end;

procedure Teval_scores_graph_form.RadioGroup3Click(Sender: TObject);
begin
   MDDef.SummarySymbol := RadioGroup3.ItemIndex;
end;


procedure Teval_scores_graph_form.RadioGroup4Click(Sender: TObject);
begin
   SSIM_FUV_scatterplot(db,RadioGroup4.Items[RadioGroup4.ItemIndex]);
end;

procedure Teval_scores_graph_form.RadioGroup5Click(Sender: TObject);
begin
   MDDef.DemixSymSize := RadioGroup5.ItemIndex + 1;
end;

procedure Teval_scores_graph_form.RadioGroup6Click(Sender: TObject);
begin
   MDDef.TwoParameterVisualization := RadioGroup6.ItemIndex;
end;

procedure Teval_scores_graph_form.RadioGroup7Click(Sender: TObject);
begin
   MDDef.DEMIX_groupWonLost := RadioGroup7.ItemIndex;
end;

procedure Teval_scores_graph_form.RadioGroup8Click(Sender: TObject);
begin
   RadioGroup9Click(Sender);
end;

procedure Teval_scores_graph_form.RadioGroup9Click(Sender: TObject);
var
   DoingRanks,FUVExpandScales : boolean;
   Criteria,TheDEMs : tStringList;

      function AverageWithFilters(DB : integer; LandParam : shortstring; CriteriaInFilter : tStringList; AddLegend : boolean) : tThisBaseGraph;
      var
         MinHoriz,MaxHoriz : float64;
         NewDB,i : integer;
         Suff,HL,fName,UseFilter : ShortString;
         Value : integer;
         aMinVal,aMaxVal : float64;
         GeomorphFilters,Labels : tStringList;
      begin {function AverageWithFilter}
         {$IfDef RecordDEMIXByLandCover} WriteLineToDebugFile('AverageWithFilters in, ' + LandParam + 'average criteria=' + IntToStr(CriteriaInFilter.Count)); {$EndIf}
         wmDEM.SetPanelText(1,LandParam);
         MakeLandParamFilters(LandParam,GeomorphFilters,Labels,Memo2);
         if DoingRanks then Suff := '_SCR' else Suff := '';
         UseFilter :=  MakeCriteriaFilter(CriteriaInFilter);
         {$IfDef RecordDEMIXByLandcover} WriteLineToDebugFile('Making DB ' + 'DEMs=' + IntToStr(TheDEMs.Count) + ' GeomorphFilters=' + IntToStr(GeomorphFilters.Count) +
            ' Labels=' + IntToStr(Labels.Count) + '  ' + UseFilter); {$EndIf}
         NewDB := AverageScoresOfDEMs(DB,TheDEMs,UseFilter,Suff,GeomorphFilters,Labels);
         if ValidDB(NewDB) then begin
             if DoingRanks then begin
                HL := 'Average Ranks';
                MinHoriz := 1;
                MaxHoriz := TheDEMs.Count;
             end
             else begin
                if (CriteriaInFilter.Count = 1) then  HL := 'Criterion: ' + CriteriaInFilter.Strings[0]
                else begin
                  HL := 'Criteria Avg: ' + CriteriaInFilter.Strings[0];
                  for i := 1 to pred(CriteriaInFilter.Count) do begin
                     HL := HL + '-' + CriteriaInFilter.Strings[i];
                  end;
                end;

                HL := StringReplace(HL, '_FUV', '',[rfReplaceAll, rfIgnoreCase]);

                if (GISdb[db].DEMIXdbtype in [ddbDiffDist]) then begin
                   MinHoriz := -9999;
                   MaxHoriz := -9999;
                end
                else if FUVExpandScales then begin
                  MinHoriz := 999;
                  MaxHoriz := -999;
                  for I := 0 to pred(TheDEMs.Count) do begin
                     fName := TheDEMs[i];
                     if GISdb[NewDB].MyData.FieldExists(fName) then begin
                         GISdb[NewDB].MyData.FindFieldRange(fName,aMinVal,aMaxVal);
                         if (aMinVal < MinHoriz) then MinHoriz := aMinVal;
                         if (aMaxVal > MaxHoriz) then MaxHoriz := aMaxVal;
                     end;
                  end;
                end
                else begin
                   MinHoriz := 0;
                   MaxHoriz := 1;
                end;
             end;
             {$IfDef RecordDEMIXByLandCover} WriteLineToDebugFile('Calling AverageScoresGraph ' + HL + ' DEMs=' + IntToStr(TheDEMs.Count)); {$EndIf}
             Result := AverageScoresGraph(NewDB,TheDEMs,HL,AddLegend,MinHoriz,MaxHoriz);
             {$IfDef RecordGraphCaption} ('AverageWithFilter, GraphCaption=' + Result.Caption); {$EndIf}
         end
         else begin
            MessageToContinue(LandParam + ' Failure to create AverageScoresOfDEMs');
            Result := Nil;
         end;
      end {function AverageWithFilter};

      procedure MultipleAverageRanksOrEvaluations(CriteriaInFilter : tStringList);
      var
         BigBitmap,bmp : tMyBitmap;
         y : integer;
         Title : shortstring;
         //gr : array[1..5] of tThisBaseGraph;

         function DrawOnPanel(i : integer; Terrain : shortstring) : tThisBaseGraph;
         begin
            Result := AverageWithFilters(DB,Terrain,CriteriaInFilter,i=5);
            if (Result <> Nil) then begin
                if (i=1) then begin
                   BigBitmap.Canvas.TextOut(Result.GraphDraw.LeftMargin + 5,2, DEMIXModeName + ', ' + CriteriaFamily + ' Criteria');
                   Title := Result.GraphDraw.HorizLabel;
                end;
                CopyImageToBitmap(Result.Image1,bmp);
                BigBitmap.Canvas.Draw(2,y,bmp);
                y := y + 10 + bmp.Height;
                bmp.Destroy;
            end;
         end;

      begin
         {$IfDef RecordDEMIXByLandcover} WriteLineToDebugFile('MultipleAverageRanksOrEvaluations'); {$EndIf}
         CreateBitmap(BigBitmap,1200,4500);
         BigBitmap.Canvas.Font.Size := 20;
         BigBitmap.Canvas.Font.Style := [fsBold];
         y := 15 + BigBitmap.Canvas.TextHeight(CriteriaFamily + ' Criteria');
         DrawOnPanel(1,'BARREN_PC');
         DrawOnPanel(2,'FOREST_PC');
         DrawOnPanel(3,'URBAN_PC');
         DrawOnPanel(4,'AVG_ROUGH');
         DrawOnPanel(5,'AVG_SLOPE');
         GetImagePartOfBitmap(BigBitmap);
         DisplayBitmap(BigBitmap,Title);
         {$IfDef RecordDEMIXByLandCover} WriteLineToDebugFile('Teval_scores_graph_form.BitBtn40Click out'); {$EndIf}
      end;

      procedure MultipleByEachCriterion;
      var
         i : integer;
         CriteriaInFilter : tStringList;
      begin
         {$IfDef RecordDEMIX} WriteLineToDebugFile('In MultipleAverageRanksOrEvaluations, Criteria=' + IntToStr(Criteria.Count)); {$EndIf}
         for i := 0 to pred(Criteria.Count) do begin
            CriteriaInFilter := tStringList.Create;
            CriteriaInFilter.Add(Criteria.Strings[i]);
            {$IfDef RecordDEMIX} WriteLineToDebugFile(Criteria.Strings[i]); {$EndIf}
            wmDEM.SetPanelText(2,'Criterion ' + IntToStr(succ(i)) + '/' + IntToStr(Criteria.Count) + ' ' + Criteria.Strings[i]);
            MultipleAverageRanksOrEvaluations(CriteriaInFilter);
            CriteriaInFilter.Destroy;
         end;
         {$IfDef RecordDEMIX} WriteLineToDebugFile('Out MultipleAverageRanksOrEvaluations, Criteria=' + IntToStr(Criteria.Count)); {$EndIf}
      end;


var
   Choice : integer;
begin {Teval_scores_graph_form.RadioGroup9Click}
    DoingRanks := (Sender = RadioGroup8);
    if (Sender = RadioGroup8) then Choice := RadioGroup8.ItemIndex
    else Choice := RadioGroup9.ItemIndex;

    FUVExpandScales := AnswerIsYes('Expand FUV scale of values used instead of 0-1');
    TheDEMs := AssembleDEMList;
    Criteria := MakeCriteriaList;

    SetColorForProcessing;
    Self.Visible := false;
    {$IfDef RecordDEMIX} HighlightLineToDebugFile('Enter Teval_scores_graph_form.RadioGroup9Click, choice=' + IntToStr(Choice) + ' DEMs=' + IntToStr(TheDEMs.Count)); {$EndIf}
    case Choice of
       0 : AverageWithFilters(DB,'Users',Criteria,true);
       1 : AverageWithFilters(DB,'AVG_SLOPE',Criteria,true);
       2 : AverageWithFilters(DB,'AVG_ROUGH',Criteria,true);
       3 : AverageWithFilters(DB,'FOREST_PC',Criteria,true);
       4 : AverageWithFilters(DB,'BARREN_PC',Criteria,true);
       5 : AverageWithFilters(DB,'URBAN_PC',Criteria,true);
       6 : MultipleAverageRanksOrEvaluations(Criteria);
       7 : MultipleByEachCriterion;
    end;
    RadioGroup8.ItemIndex := -1;
    RadioGroup9.ItemIndex := -1;
    {$IfDef RecordDEMIX} HighlightLineToDebugFile('Exit Teval_scores_graph_form.RadioGroup9Click, choice=' + IntToStr(Choice) + ' DEMs=' + IntToStr(TheDEMs.Count)); {$EndIf}
    TheDEMs.Destroy;
    Criteria.Destroy;

    SetColorForWaiting;
    Self.Visible := true;
end {Teval_scores_graph_form.RadioGroup9Click};


initialization
finalization
end.
