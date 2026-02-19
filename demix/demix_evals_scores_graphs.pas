unit demix_evals_scores_graphs;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}


//{$Define ConvertDBFtoDB}


{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      {$Define RecordDEMIX}
      //{$Define RecordBoxPlots}
      //{$Define RecordChangeDB}
      //{$Define RecordDEMIXByLandCover
      //{$Define RecordGraphCaption}
      //{$Define TrackCriteriaList}
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
    BitBtn2: TBitBtn;
    BitBtn23: TBitBtn;
    Edit4: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Edit5: TEdit;
    BitBtn27: TBitBtn;
    RadioGroup7: TRadioGroup;
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
    Edit6: TEdit;
    Label6: TLabel;
    //RadioGroup8: TRadioGroup;
    RadioGroup9: TRadioGroup;
    BitBtn22: TBitBtn;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    BitBtn4: TBitBtn;
    BitBtn10: TBitBtn;
    GroupBox5: TGroupBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Label7: TLabel;
    Label8: TLabel;
    BitBtn1: TBitBtn;
    RadioGroup3: TRadioGroup;
    BitBtn8: TBitBtn;
    BitBtn38: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtn11: TBitBtn;
    BitBtn13: TBitBtn;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    BitBtn31: TBitBtn;
    GroupBox6: TGroupBox;
    CheckBox9: TCheckBox;
    CheckBox7: TCheckBox;
    BitBtn36: TBitBtn;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    CheckBox15: TCheckBox;
    RadioGroup6: TRadioGroup;
    GroupBox7: TGroupBox;
    BitBtn29: TBitBtn;
    ComboBox1: TComboBox;
    BitBtn37: TBitBtn;
    BitBtn40: TBitBtn;
    GroupBox8: TGroupBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox8: TCheckBox;
    BitBtn41: TBitBtn;
    RadioGroup12: TRadioGroup;
    GroupBox9: TGroupBox;
    GroupBox10: TGroupBox;
    BitBtn39: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn25: TBitBtn;
    BitBtn26: TBitBtn;
    BitBtn24: TBitBtn;
    BitBtn15: TBitBtn;
    BitBtn14: TBitBtn;
    GroupBox11: TGroupBox;
    ComboBox8: TComboBox;
    BitBtn32: TBitBtn;
    GroupBox12: TGroupBox;
    BitBtn17: TBitBtn;
    BitBtn28: TBitBtn;
    BitBtn16: TBitBtn;
    BitBtn42: TBitBtn;
    BitBtn43: TBitBtn;
    BitBtn44: TBitBtn;
    CheckBox4: TCheckBox;
    Edit7: TEdit;
    Label11: TLabel;
    BitBtn45: TBitBtn;
    BitBtn46: TBitBtn;
    CheckBox14: TCheckBox;
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
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure BitBtn19Click(Sender: TObject);
    procedure BitBtn21Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure BitBtn23Click(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
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
    //procedure RadioGroup8Click(Sender: TObject);
    procedure RadioGroup9Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn22Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn38Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn24Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure BitBtn26Click(Sender: TObject);
    procedure BitBtn31Click(Sender: TObject);
    //procedure BitBtn32Click(Sender: TObject);
    procedure BitBtn36Click(Sender: TObject);
    procedure BitBtn37Click(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure ComboBox7Change(Sender: TObject);
    procedure CheckBox14Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure CheckBox10Click(Sender: TObject);
    procedure CheckBox11Click(Sender: TObject);
    procedure CheckBox12Click(Sender: TObject);
    procedure CheckBox13Click(Sender: TObject);
    procedure CheckBox15Click(Sender: TObject);
    procedure RadioGroup6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn25Click(Sender: TObject);
    procedure BitBtn39Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure BitBtn40Click(Sender: TObject);
    procedure BitBtn41Click(Sender: TObject);
    procedure RadioGroup12Click(Sender: TObject);
    procedure BitBtn32Click(Sender: TObject);
    procedure BitBtn42Click(Sender: TObject);
    procedure BitBtn43Click(Sender: TObject);
    procedure BitBtn44Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure Edit7Change(Sender: TObject);
    procedure BitBtn45Click(Sender: TObject);
    procedure BitBtn46Click(Sender: TObject);
  private
    { Private declarations }
    procedure ChangeDBonForm(Newdb : integer);
    function MakeDEMlist : tStringList;
    function MakeCriteriaList(All : boolean = false) : tStringList;
    function GeomorphFiltersFromMixed1(var Labels : tStringList) : tStringList;
    function MakeCriteriaFilter(UseCriteria : tStringList) : ShortString;
    procedure LoadDEMsInMemo;
    procedure BoxCarFilters(NumFilter: integer);
  public
    { Public declarations }
     db_U10,db_u80,db_u120,db_Full,
     db : integer;
     UseDEMs : tStringList;
  end;

//var
  //eval_scores_graph_form : Teval_scores_graph_form;


procedure StartDEMIXgraphs(DB : integer);

implementation

{$R *.dfm}

uses
   Nevadia_main,
   Petmar,Petmar_db,Petimage_form,PetImage,petdbutils, PetMath,
   DEMdataBase,
   Toggle_db_use,
   MD_use_tools,
   DEMdefs,
   DEMdef_routines,
   DEMCoord,
   DEMstat,
   DEMmapf,
   DEM_db_ops,
   DEM_manager,
   DEMIX_graphs,
   DEMIX_control,
   DEMIX_definitions;


procedure GetGraphHorizontalLimits(db : integer; Criterion : shortstring; theDEMs : tStringList; var MinHoriz,MaxHoriz : float64; var CriteriaNameAdd : shortstring);
var
   i : integer;
   MomentVar : tMomentVar;
begin
   if StrUtils.AnsiContainsText(GISdb[db].dbName,'diff_dist') then begin
      GISdb[db].ApplyGISFilter('CRITERION=' + QuotedStr(Criterion));
      GISdb[db].EmpSource.Enabled := false;
      MinHoriz := 9999;
      MaxHoriz := -9999;
      for i := 0 to pred(TheDEMs.Count) do begin
         MomentVar := GISdb[db].MyData.GetFieldStatistics(TheDEMs.Strings[i]);
         if MomentVar.PC2 < MinHoriz then MinHoriz := MomentVar.PC2;
         if MomentVar.PC98 > MaxHoriz then MaxHoriz := MomentVar.PC98;
      end;
      MinHoriz := MinHoriz - 1;
      MaxHoriz := MaxHoriz + 1;
      CriteriaNameAdd := '';
   end
   else begin
      CriteriaNameAdd := ' FUV';
      if MDDef.FUVExpandScales then begin
         MinHoriz := 1;
         MaxHoriz := -1;
      end
      else begin
         MinHoriz := 0;
         MaxHoriz := 1;
      end;
   end;
end;


 procedure StartDEMIXgraphs(DB : integer);
var
  eval_scores_graph_form : Teval_scores_graph_form;
  i : integer;
  FieldsInDB : tStringList;
begin
   GetDEMIXpaths(false,db);
   eval_scores_graph_form := Teval_scores_graph_form.Create(Application);
   if (db = -4) then begin
      eval_scores_graph_form.BitBtn4Click(nil);
      db := eval_scores_graph_form.db;
   end;

   //RecognizeDEMIXVersion(DB);
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('StartDEMIXgraphs after RecognizeDEMIXVersion, NumDEMIXtestDEM=' + IntToStr(NumDEMIXtestDEM)); {$EndIf}
   GISdb[db].dbtablef.OpenDEMIXgraphs1.Enabled := false;

   eval_scores_graph_form.ChangeDBonForm(db);
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('StartDEMIXgraphs after change DB, NumDEMIXtestDEM=' + IntToStr(NumDEMIXtestDEM)); {$EndIf}

   LoadDEMIXnames;

   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('StartDEMIXgraphs after load, NumDEMIXtestDEM=' + IntToStr(NumDEMIXtestDEM)); {$EndIf}
   eval_scores_graph_form.LoadDEMsInMemo;
   eval_scores_graph_form.CheckBox14.Checked := MDDef.DEMIX_IgnoreTies;
   eval_scores_graph_form.BitBtn9Click(nil);

   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('StartDEMIXgraphs after LoadDEMsInMemo, NumDEMIXtestDEM=' + IntToStr(NumDEMIXtestDEM)); {$EndIf}

   GetFields(GISdb[DB].MyData,GISdb[DB].dbOpts.VisCols,NumericFieldTypes,FieldsInDB);
   if (FieldsInDB <> Nil) and (FieldsInDB.Count > 0) then begin
      eval_scores_graph_form.ComboBox2.Items := FieldsInDB;
      eval_scores_graph_form.ComboBox3.Items := FieldsInDB;
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
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('LoadDEMsInMem0 in, NumDEMIXtestDEM=' + IntToStr(NumDEMIXtestDEM)); {$EndIf}
      Memo3.Clear;
      theDEMs := GetListOfTestDEMsinUse;
      for I := 0 to pred(theDEMs.Count) do begin
         if GISdb[db].MyData.FieldExists(theDEMs.Strings[i]) then begin
            Memo3.Lines.Add(theDEMs.Strings[i]);
         end;
      end;
      theDEMs.Destroy;
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Teval_scores_graph_form.LoadDEMsInMem, show user DEMs=' + IntToStr(Memo3.Lines.Count)); {$EndIf}
   end;
end;


procedure Teval_scores_graph_form.ChangeDBonForm(Newdb : integer);
var
   i : integer;
begin
   if ValidDB(NewDB) then begin
      Get_DEMIX_CriteriaToleranceFName(NewDB);
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
      RadioGroup9.Enabled := TileCharacteristicsInDB(NewDB);
      {$IfDef RecordChangeDB} WriteLineToDebugFile('ChangeDBonForm out ' + GISdb[db].dbName + ' ' + DEMIXModeName + '  ' + CriteriaFamily + '  ' + DEMIX_criteria_tolerance_fName); {$EndIf}
   end;
end;


function Teval_scores_graph_form.MakeDEMlist : tStringList;
var
   j : integer;
begin
   Result := tStringList.Create;
   for j := 0 to pred(Memo3.Lines.Count) do begin
      if (Memo3.Lines[j] <> '') then Result.Add(Memo3.Lines[j]);
   end;
end;

function Teval_scores_graph_form.MakeCriteriaList(All : boolean = false) : tStringList;
var
   j : integer;
   Criterion : shortstring;
   sl2 : tStringList;
begin
   Result := tStringList.Create;
   GISdb[db].DBFieldUniqueEntries('CRITERION',sl2);
   for j := 0 to pred(Memo1.Lines.Count) do begin
      Criterion := UpperCase(Memo1.Lines[j]);
      if (Criterion <> '') and (All or (sl2.IndexOf(Criterion) <> -1)) then begin
         Result.Add(NoSuffixCriterion(Criterion));
      end;
   end;
   sl2.Destroy;
   {$IfDef TrackCriteriaList}TrackCriteriaList(Result,'MakeCriteriaList'); {$EndIf}
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
   GeomorphFilters,DEMList,Labels : tStringList;


         procedure DoElevationRange(adb : integer; BaseCompareDEM,aFilter : shortstring);
         var
            gr : tThisBaseGraph;
         begin
            ChangeDBonForm(adb);
            {$IfDef RecordDEMIX} WriteLineToDebugFile('DoElevationRange DEM ' + BaseCompareDEM +  ' ' + DEMIXModeName + ' DEMs=' + IntToStr(DEMList.Count)); {$EndIf}
            GISdb[db].ApplyGISFilter(aFilter);
            if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
               if GISdb[db].MyData.FieldExists(BaseCompareDEM) then begin
                  HL := DEMIXModeName + ' ' + BaseCompareDEM + ' ' + GISdb[DB].MyData.Filter + NumTilesString(DB);
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
   if TileCharacteristicsInDB(DB,true) then begin
      BitBtn4Click(Sender);   //If needed loads db_U10,db_U80,db_U120,db_Full))
      GeomorphFilters := GeomorphFiltersFromMixed1(Labels);
      DEMList := MakeDEMlist;
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
              Legend := DEMIXTestDEMLegend(DEMList);
              aName := MDTempDir + 'win_lose_tie_' + DEMlist[j] + '.png';
              FinishBigBitMapWithLegend(BigBitmap,Legend,aname);
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
   end;
end;


procedure Teval_scores_graph_form.BitBtn11Click(Sender: TObject);
begin
   VerifyRecordsToUse(DemixSettingsDir + 'demix_dems.dbf','SHORT_NAME');
   LoadDEMsInMemo;
end;


procedure Teval_scores_graph_form.BitBtn12Click(Sender: TObject);
begin
   WhiskerPlotsByCluster(DB,MakeCriteriaList);
end;



procedure Teval_scores_graph_form.BitBtn13Click(Sender: TObject);
var
   TheDEMs,Criteria,Findings,Filters1,DEMs2,GraphList,Labels1 : tStringList;
   aLine,DEM,ThisWinners,BaseFilter : shortstring;
   fName : PathStr;
   db2,i,j,k,This,TotalWinners,Slope : integer;
   Winners : array[0..50] of integer;
   Tolerance : float32;


    function OneGraph(SlopeFilter,SlopeLabel : shortstring; TheDEMs : tStringList; WhichDEMs : shortstring) : tThisBaseGraph;
    var
       i,j,k : integer;
       TStr : shortstring;
    begin
       Findings := tStringList.Create;
       aline := 'CRITERION,TOLERANCE,TILES,WINNERS';
       for j := 0 to pred(TheDEMs.Count) do aline := aline + ',' + theDEMs.Strings[j];
       Findings.Add(aline);
       for i := 0 to pred(Criteria.Count) do begin
           TStr := SlopeFilter + ' AND CRITERION=' + QuotedStr(Criteria.Strings[i]);
           if BaseFilter <> '' then TStr := TStr + ' AND ' + BaseFilter;
           {$IfDef RecordDEMIX} WriteLineToDebugFile('OneGraph, filter=' + TStr); {$EndIf}
           GISdb[db].ApplyGISFilter(TStr);
           GISdb[db].EmpSource.Enabled := false;
           if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
               Tolerance := CriterionTieTolerance(Criteria.Strings[i]);
               aline := Criteria.Strings[i] + ',' + RealToString(Tolerance,-8,-6) + ',' + IntToStr(GISdb[db].MyData.FiltRecsInDB);
               for k := 0 to 50 do Winners[k] := 0;
               TotalWinners := 0;
               {$IfDef RecordDEMIX} WriteLineToDebugFile('Matches=' + IntToStr(GISdb[db].MyData.FiltRecsInDB)); {$EndIf}
               while not GISdb[db].MyData.eof do begin
                  ThisWinners := WinnerAndTies(DB,theDEMs,Tolerance);
                  for j := 0 to pred(TheDEMs.Count) do begin
                     DEM := theDEMs.Strings[j];
                     if StrUtils.AnsiContainsText(ThisWinners,DEM) then begin
                        inc(Winners[j]);
                        inc(TotalWinners);
                     end;
                  end;
                  GISdb[db].MyData.Next;
               end;
               aline := aline + ',' + IntToStr(TotalWinners);
               for j := 0 to pred(TheDEMs.Count) do aline := aline + ',' + IntToStr(Winners[j]);
               Findings.Add(aline);
           end;
        end;
        TStr := SlopeFilter;
        StripInvalidPathNameChars(TStr);
        if (Findings.Count > 1) then begin
          fName := NextFileNumber(MDtempDir,'wins_by_' + TStr + '_','.dbf');
          db2 := StringList2CSVtoDB(Findings,fName);
          Result := FractionOfWinnersGraph(db2,TheDEMs,Criteria);
          Result.GraphDraw.LLCornerText := SlopeLabel + '  ' + WhichDEMs + '  ' + BaseFilter;
          Result.GraphDraw.LeftMargin := 100;
          Result.Height := 100 + 25 * succ(Criteria.Count);
          Result.GraphDraw.MarginsGood := true;
          Result.RedrawDiagram11Click(Nil);
          FinishGraph(Result,GraphList);
        end;
    end;


var
   gr : tGraphArray;
   Panels : tStringList;
   BigBitmap,Bitmap,Legend : tMyBitmap;
   StartY : integer;
   ListName : PathStr;
begin {Teval_scores_graph_form.BitBtn13Click}
    {$IfDef RecordDEMIX} WriteLineToDebugFile('Teval_scores_graph_form.BitBtn13Click in'); {$EndIf}
    ListName := DEMIXSettingsDir + 'DEMs_2.txt';
    if FileExists(ListName) then begin

        BaseFilter := GISdb[db].MyData.Filter;
        Self.Visible := false;
        GetDEMIXpaths(True);
        TheDEMs := MakeDEMlist;
        Criteria := MakeCriteriaList;

        GISdb[DB].EmpSource.Enabled := false;

        ImportLandParamFilters(MDDef.DEMIX_filter1_fName,Filters1,Labels1);

        DEMs2 := tStringList.Create;
        DEMs2.LoadFromFile(ListName);

        GraphList := tStringList.Create;
        Findings := tStringList.Create;
        Panels := tStringList.Create;
        for i := 0 to pred(Filters1.Count) do begin
           {$IfDef RecordDEMIX} WriteLineToDebugFile('Filter=' + Filters1[i]); {$EndIf}
           wmDEM.SetPanelText(1,IntToStr(succ(i)) + '/' + IntToStr(Filters1.Count) + '  ' + Filters1[i] ,true);
           j := 0;
           gr[j] := OneGraph(Filters1.Strings[i],Labels1.Strings[i],TheDEMs,'All test DEMs');
           inc(j);
           gr[j] := OneGraph(Filters1.Strings[i],Labels1.Strings[i],DEMs2,'Unrestricted test DEMs');
           inc(j);
           fName := MergeGraphPanelsHorizontal(j,gr,false,'');
           Panels.Add(fName);
        end;

         BigBitmap := LoadBitmapFromFile(Panels[0]);
         for i := 1 to pred(Panels.Count) do begin
            Bitmap := LoadBitmapFromFile(Panels[i]);
            StartY := BigBitmap.Height + 15;
            BigBitmap.Height := StartY + Bitmap.Height;
            Bigbitmap.Canvas.Draw(0,StartY,Bitmap);
            BitMap.Destroy;
         end;
         BigBitmap.Height := BigBitmap.Height + 15;
         Legend := DEMIXtestDEMLegend(theDEMs);
         Fname := NextFileNumber(MDtempDir,'Winner_','.png');
         FinishBigBitMapWithLegend(BigBitmap,Legend,fName);
        TheDEMs.Destroy;
        Criteria.Destroy;
        Filters1.Destroy;
        DEMs2.Destroy;
        Panels.Destroy;

        EndDEMIXProcessing;
        Self.Visible := true;
        GISdb[db].ApplyGISFilter(BaseFilter);
    end
    else begin
       MessageToContinue('Option requires ' + ListName);
    end;
end {Teval_scores_graph_form.BitBtn13Click};


procedure Teval_scores_graph_form.BitBtn14Click(Sender: TObject);
var
   Filters,Labels : tStringList;
begin
   Self.Visible := false;
   ImportLandParamFilters(MDDef.DEMIX_filter1_fName, Filters,Labels);
   MainGraphOptions(DB,2,MakeDEMlist,MakeCriteriaList,Filters,Labels);
   Self.Visible := true;
end;

procedure Teval_scores_graph_form.BitBtn15Click(Sender: TObject);
var
   Filters,Labels : tStringList;
begin
   Self.Visible := false;
   ImportLandParamFilters(MDDef.DEMIX_filter1_fName, Filters,Labels);
   MainGraphOptions(DB,1,MakeDEMlist,MakeCriteriaList,Filters,Labels);
   Self.Visible := true;
end;


procedure Teval_scores_graph_form.BitBtn16Click(Sender: TObject);
var
   DEMs,GeoFilters,Labels : tStringList;
   i,NewDB : Integer;
begin
   Self.Visible := false;
   DEMs := MakeDEMlist;
   GeoFilters := GeomorphFiltersFromMixed1(Labels);
   for i := 0 to pred(DEMs.Count) do begin
      {$IfDef RecordDEMIX} WriteLineToDebugFile('Start sort: ' + DEMs.Strings[i]); {$EndIf}
      MDcreatedCSV := true;
      NewDB := SortDataBase(db,true,false,DEMs.Strings[i]);
      {$IfDef RecordDEMIX} WriteLineToDebugFile('End sort: ' + DEMs.Strings[i]); {$EndIf}
      BestEvalGraphPerCriterionMultipleFilters(NewDB,0,DEMs.Strings[i],GeoFilters,Labels,MakeCriteriaList,CriteriaFamily);
      {$IfDef RecordDEMIX} WriteLineToDebugFile('Graph over: ' + DEMs.Strings[i]); {$EndIf}
   end;
   Self.Visible := true;
end;


function Teval_scores_graph_form.GeomorphFiltersFromMixed1(var Labels : tStringList) : tStringList;
begin
   ImportLandParamFilters(MDDef.DEMIX_filter1_fName, Result,Labels);
end;


procedure Teval_scores_graph_form.BitBtn17Click(Sender: TObject);
var
   GeoFilters,Labels : tStringList;
   NewDB : integer;
begin
   Self.Visible := false;
   GeoFilters := GeomorphFiltersFromMixed1(Labels);
   MDcreatedCSV := true;
   NewDB := SortDataBase(db,true,false,'BEST_EVAL');
   FilterJustOneGraph(NewDB,MakeCriteriaList,GeoFilters,Labels,true);
   Self.Visible := true;
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
   if GetExistingFileName('DEMIX criteria','criteria*.txt',fName) then begin
      Memo1.Lines.LoadFromFile(fName);
   end;
end;


procedure Teval_scores_graph_form.BitBtn1Click(Sender: TObject);
begin
   try
      Self.Visible := false;
      PiesBestByTwoLandTypes(db,True,MakeCriteriaList,MakeDEMlist,ComboBox2.Text,ComboBox3.Text);
   finally
      Self.Visible := true;
   end;
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
var
   bmp : tMyBitmap;
begin
   bmp := DEMIXTestDEMLegend(MakeDEMlist,true,1200);
   AssignBitmapToClipBoard(bmp);
   DisplayBitmap(bmp);
end;

procedure Teval_scores_graph_form.BitBtn23Click(Sender: TObject);
begin
   AllGraphsOneImage(MDDef.NumGraphCols);
end;

procedure Teval_scores_graph_form.BitBtn24Click(Sender: TObject);
var
   Filters,Labels : tStringList;
begin
   Self.Visible := false;
   ImportLandParamFilters(MDDef.DEMIX_filter1_fName, Filters,Labels);
   MainGraphOptions(DB,3,MakeDEMlist,MakeCriteriaList,Filters,Labels);
   Self.Visible := true;
end;

procedure Teval_scores_graph_form.BitBtn25Click(Sender: TObject);
begin
  RadioGroup9.ItemIndex := 1;
end;

procedure Teval_scores_graph_form.BitBtn26Click(Sender: TObject);
var
   Filters,Labels : tStringList;
   bf : shortstring;
begin
   Self.Visible := false;
   bf := GISdb[db].MyData.Filter;
   ImportLandParamFilters(MDDef.DEMIX_filter1_fName, Filters,Labels);
   MainGraphOptions(DB,4,MakeDEMlist,MakeCriteriaList,Filters,Labels);
   Self.Visible := true;
   GISdb[db].ApplyGISfilter(bf);
end;

procedure Teval_scores_graph_form.BitBtn27Click(Sender: TObject);
begin
   LastDataBase := DEMIX_final_DB_dir;
   db := OpenMultipleDataBases('New DB for DEMIX graphs');
   ChangeDBonForm(db);
end;


procedure Teval_scores_graph_form.BitBtn28Click(Sender: TObject);
var
   GeoFilters,Labels : tStringList;
   NewDB : integer;
begin
   Self.Visible := false;
   GeoFilters := GeomorphFiltersFromMixed1(Labels);
   MDcreatedCSV := true;
   NewDB := SortDataBase(db,true,false,'BEST_EVAL');
   BestEvalGraphPerCriterionMultipleFilters(Newdb,0,'BEST_EVAL',GeoFilters,Labels,MakeCriteriaList,CriteriaFamily);
   Self.Visible := true;
end;


procedure Teval_scores_graph_form.BitBtn29Click(Sender: TObject);
var
   GeoFilters,Labels : tStringList;
begin
   GeoFilters := GeomorphFiltersFromMixed1(Labels);
   WinningComparedToBaseDEM(db,ComboBox1.Text, GeoFilters,Labels,MakeCriteriaList,MakeDEMlist);
end;


procedure Teval_scores_graph_form.BitBtn2Click(Sender: TObject);
begin
   CombineAllPanelGraphs;
end;

procedure Teval_scores_graph_form.BitBtn30Click(Sender: TObject);
begin
   GISdb[db].dbtablef.BringToFront;
end;


procedure Teval_scores_graph_form.BitBtn31Click(Sender: TObject);
var
   DEMs,Criteria : tStringList;
   i,j,k,ref,rg,tg : integer;
   r : float32;
   AreaName,Criterion,DEMIX_TILE : shortstring;
   fName,BaseDir : PathStr;
   OpenMaps : boolean;
   tests : array[0..15] of integer;
   gr : tGraphArray;

      procedure RedrawGraph(k : integer);
      begin
         if (Criterion = 'ELEV') then begin
           gr[k].GraphDraw.MinHorizAxis := 800;
           gr[k].GraphDraw.MaxHorizAxis := 1800;
           gr[k].GraphDraw.MinVertAxis := 800;
           gr[k].GraphDraw.MaxVertAxis := 1800;
         end;
         if (Criterion = 'SLOPE') then begin
            gr[k].GraphDraw.MinHorizAxis := 0;
            gr[k].GraphDraw.MaxHorizAxis := 150;
            gr[k].GraphDraw.MinVertAxis := 0;
            gr[k].GraphDraw.MaxVertAxis := 150;
         end;
         if (Criterion = 'TPI') then begin
            gr[k].GraphDraw.MinHorizAxis := -50;
            gr[k].GraphDraw.MaxHorizAxis := 50;
            gr[k].GraphDraw.MinVertAxis := -50;
            gr[k].GraphDraw.MaxVertAxis := 50;
         end;
         if (Criterion = 'OPENU') then begin
            gr[k].GraphDraw.MinHorizAxis := 50;
            gr[k].GraphDraw.MaxHorizAxis := 120;
            gr[k].GraphDraw.MinVertAxis := 50;
            gr[k].GraphDraw.MaxVertAxis := 120;
         end;
         if (Criterion = 'TANGC') then begin
            gr[k].GraphDraw.MinHorizAxis := -0.04;
            gr[k].GraphDraw.MaxHorizAxis := 0.04;
            gr[k].GraphDraw.MinVertAxis := -0.04;
            gr[k].GraphDraw.MaxVertAxis := 0.04;
         end;
         if (Criterion = 'PLANC') then begin
            gr[k].GraphDraw.MinHorizAxis := -0.4;
            gr[k].GraphDraw.MaxHorizAxis := 0.4;
            gr[k].GraphDraw.MinVertAxis := -0.4;
            gr[k].GraphDraw.MaxVertAxis := 0.4;
         end;
         gr[k].GraphDraw.LeftMargin := 90;
         gr[k].GraphDraw.BottomMargin := 50;
         gr[k].GraphDraw.XWindowSize := 500;
         gr[k].GraphDraw.YWindowSize := 400;
         gr[k].MainSymbol.Size := 2;
         gr[k].RedrawDiagram11Click(Nil);
      end;

begin
   Self.Visible := false;
   GetDEMIXpaths(True);
   AreaName := 'state_line';
   DEMIX_TILE := 'UTM_11N_x63y395';
   DEMs := MakeDEMlist;
   Criteria := MakeCriteriaList;
   BaseDir := MDDef.DEMIX_BaseDir + AreaName + '\' + DEMIX_Tile + '_ref_test_dem\';
   k := -1;
   MDdef.AddFUVtoR2 := true;
   OpenMaps := false;
   fName := BaseDir + 'ref_dtm_srtm.tif';
   {$IfDef RecordDEMIX} HighlightLineToDebugFile('Load ref dtm=' + fname); {$EndIf}
   LoadNewDEM(ref,fName,OpenMaps);
   DEMglb[ref].AreaName := 'Reference DTM ELEV';
   for i := 0 to pred(DEMs.Count) do begin
      fName := BaseDir + DEMs[i] + '.tif';
      LoadNewDEM(tests[i],fName,OpenMaps);
      DEMglb[Tests[i]].AreaName := DEMs.Strings[i] + ' ELEV';
   end;

   for j := 0 to pred(DEMs.Count) do begin
     wmDEM.SetPanelText(1,DEMs[j],true);
     for i := 0 to pred(Criteria.Count) do begin
        Criterion := NoSuffixCriterion(Criteria.Strings[i]);
        wmDEM.SetPanelText(2,Criterion ,true);
        if (Criterion = 'ELEV') then begin
           inc(k);
           gr[k] := GridScatterGram(DEMglb[ref].FullDEMGridLimits,r,ref,Tests[j]);
           RedrawGraph(k);
        end
        else begin
            rg := CreateSingleLSPGrid(OpenMaps,ref,Criterion);
            DEMglb[rg].AreaName := 'Reference DTM ' + Criterion;
            tg := CreateSingleLSPGrid(OpenMaps,tests[j],Criterion);
            DEMglb[tg].AreaName := DEMs.Strings[j] + ' ' + Criterion;
            inc(k);
            gr[k] := GridScatterGram(DEMglb[rg].FullDEMgridLimits,r,rg,Tg);
            RedrawGraph(k);
        end;
      end;
   end;
   AllGraphsOneImage(Criteria.Count);
   EndDEMIXProcessing;
   Self.Visible := true;
   MDdef.AddFUVtoR2 := false;
   CloseAllDEMs;
end;


procedure Teval_scores_graph_form.BitBtn32Click(Sender: TObject);
var
   NewDB : integer;
begin
  {$IfDef RecordDEMIX} HighlightLineToDebugFile('Enter Teval_scores_graph_form.RadioGroup1Click, choice=' + IntToStr(ComboBox8.ItemIndex)); {$EndIf}
  SetColorForProcessing;
  Self.Visible := false;
  if (ComboBox8.ItemIndex = 0) then begin
     MDcreatedCSV := true;
     NewDB := SortDataBase(db,true,false,'BEST_EVAL');
     DEMIX_evaluations_graph(NewDB,ComboBox8.ItemIndex,MakeDEMlist,MakeCriteriaList,true);
  end
  else DEMIX_evaluations_graph(DB,ComboBox8.ItemIndex,MakeDEMlist,MakeCriteriaList,true);
  SetColorForWaiting;
  Self.Visible := true;
  {$IfDef RecordDEMIX} HighlightLineToDebugFile('Exit Teval_scores_graph_form.RadioGroup1Click, choice=' + IntToStr(ComboBox8.ItemIndex)); {$EndIf}
end;

procedure Teval_scores_graph_form.BitBtn33Click(Sender: TObject);
var
   Labels : tStringList;
begin
  TileCharateristicsWhiskerPlotsByCluster(DB,false,GeomorphFiltersFromMixed1(Labels));
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


procedure Teval_scores_graph_form.BitBtn36Click(Sender: TObject);
var
   DEMs2 : tStringList;
   ListName : PathStr;
begin
   ListName := DEMIXSettingsDir + 'DEMs_2.txt';
   if FileExists(ListName) then begin
       Self.Visible := false;
       GetDEMIXpaths(True);
       if CheckBox7.Checked then begin
           GISdb[db].ApplyGISFilter('COUNTRY=' + QuotedStr('USA'));
           WinningPiesByCriteria(db, MakeCriteriaList, MakeDEMlist,'USA tiles');
           GISdb[db].ApplyGISFilter('COUNTRY<>' + QuotedStr('USA'));
           WinningPiesByCriteria(db, MakeCriteriaList, MakeDEMlist,'Non-US tiles');
       end
       else if CheckBox9.Checked then begin
           WinningPiesByCriteria(db, MakeCriteriaList, MakeDEMlist,'Unrestricted DEM license');
           if CheckBox2.Checked then begin
              DEMs2 := tStringList.Create;
              DEMs2.LoadFromFile(ListName);
              WinningPiesByCriteria(db, MakeCriteriaList,DEMs2,'Restricted DEM license');
           end;
       end
       else WinningPiesByCriteria(db, MakeCriteriaList, MakeDEMlist,'');
       EndDEMIXProcessing;
       Self.Visible := true;
   end
   else begin
      MessageToContinue('Option requires ' + ListName);
   end;
end;


procedure Teval_scores_graph_form.BitBtn37Click(Sender: TObject);
begin
   BoxCarFilters(1);
end;


procedure Teval_scores_graph_form.BoxCarFilters(NumFilter : integer);
var
   Filters1,Labels1,Filters2,Labels2,Stats,TheCriteria,theDEMs,GeomorphFilters : tStringList;
   MomentVar : tMomentVar;
   fName,aName : PathStr;
   i,j,k,n,ndb : integer;
   BigBitmap,
   Bitmap : tMyBitmap;
   CriteriaNameAdd,DEM,BaseFilter : shortstring;
   Color : tColor;
   MinHoriz,MaxHoriz : float64;
   gr : array[0..10] of tThisBaseGraph;
begin
   {$IfDef RecordBoxPlots} WriteLineToDebugFile('Teval_scores_graph_form.BitBtn37Click in (BoxPlots)'); {$EndIf}
   if TileCharacteristicsInDB(DB,true) then begin
       SetColorForProcessing;
       Self.Visible := false;
       BaseFilter := GISdb[db].MyData.Filter;
       theDEMs := MakeDEMlist;
       ImportLandParamFilters(MDDef.DEMIX_filter1_fName,Filters1,Labels1);
       ImportLandParamFilters(MDDef.DEMIX_filter2_fName,Filters2,Labels2);
       theCriteria := MakeCriteriaList;
       if (NumFilter = 2) then begin
           for j := 0 to pred(theCriteria.Count) do begin
              GetGraphHorizontalLimits(db,theCriteria[j],theDEMs,MinHoriz,MaxHoriz,CriteriaNameAdd);
              for n := 0 to pred(Filters2.Count) do begin
                for k := 0 to pred(Filters1.Count) do begin
                    Stats := tStringList.Create;
                    Stats.Add(MomentStr + ',COLOR');
                    GISdb[db].ApplyGISFilter('CRITERION=' + QuotedStr(theCriteria[j]) + ' AND ' + Filters1.Strings[k] + ' AND ' + Filters2.Strings[n]);
                    {$IfDef RecordBoxPlots} WriteLineToDebugFile(GISdb[db].MyData.Filter); {$EndIf}

                     for i := 0 to pred(TheDEMs.Count) do begin
                        GISdb[db].EmpSource.Enabled := false;
                        DEM := TheDEMs.Strings[i];
                        MomentVar := GISdb[DB].GetFieldStatistics(DEM);
                        Stats.Add(DEM + MomentResultsToString(MomentVar) + ',' + IntToStr(ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEM))));
                     end;
                     fName := NextFileNumber(MDTempDir,'box_plot_' + theCriteria[j],'.dbf');
                     ndb := StringList2CSVtoDB(Stats,fName,false,false,false);
                     gr[k] := StartBoxPlot(ndb,Labels1[k] + '  ' + Labels2[n] + ' (n=' + IntToStr(GISdb[db].MyData.FiltRecsInDB) + ')',theCriteria[j] + CriteriaNameAdd);
                     gr[k].GraphDraw.MinHorizAxis := MinHoriz;
                     gr[k].GraphDraw.MaxHorizAxis := MaxHoriz;
                     gr[k].GraphDraw.LLcornerTextAtEdge := false;
                     gr[k].Width := 700;
                     gr[k].RedrawDiagram11Click(Nil);
                     AddGraphToBigBitmap(succ(n) + k * Filters2.Count,Filters2.Count,Filters1.Count,gr[k],BigBitmap);
                 end;
              end;
              aName := NextFileNumber(MDTempDir,'box_plot_' + ExtractFileNameNoExt(MDDef.DEMIX_filter1_fName) + '_' + ExtractFileNameNoExt(MDDef.DEMIX_filter2_fName),'.png');
              Bitmap := DEMIXTestDEMLegend(theDEMs);
              FinishBigBitMapWithLegend(BigBitmap,bitmap,aname);
              {$IfDef RecordBoxPlots} WriteLineToDebugFile(GISdb[db].MyData.Filter + ' done'); {$EndIf}
           end;
       end
       else if (NumFilter = 1) then begin
           for j := 0 to pred(theCriteria.Count) do begin
              GetGraphHorizontalLimits(db,theCriteria[j],theDEMs,MinHoriz,MaxHoriz,CriteriaNameAdd);
              for k := 0 to pred(Filters1.Count) do begin
                  Stats := tStringList.Create;
                  Stats.Add(MomentStr + ',COLOR');
                  GISdb[db].ApplyGISFilter('CRITERION=' + QuotedStr(theCriteria[j]) + ' AND ' + Filters1.Strings[k]);
                  {$IfDef RecordBoxPlots} WriteLineToDebugFile(GISdb[db].MyData.Filter); {$EndIf}

                   for i := 0 to pred(TheDEMs.Count) do begin
                      GISdb[db].EmpSource.Enabled := false;
                      DEM := TheDEMs.Strings[i];
                      MomentVar := GISdb[DB].GetFieldStatistics(DEM);
                      Stats.Add(DEM + MomentResultsToString(MomentVar) + ',' + IntToStr(ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEM))));
                   end;
                   fName := NextFileNumber(MDTempDir,'box_plot_' + theCriteria[j],'.dbf');
                   ndb := StringList2CSVtoDB(Stats,fName,false,false,false);
                   gr[k] := StartBoxPlot(ndb,Labels1[k] + ' (n=' + IntToStr(GISdb[db].MyData.FiltRecsInDB) + ')',theCriteria[j] + CriteriaNameAdd);
                   gr[k].GraphDraw.MinHorizAxis := MinHoriz;
                   gr[k].GraphDraw.MaxHorizAxis := MaxHoriz;
                   gr[k].GraphDraw.LLcornerTextAtEdge := false;
                   gr[k].Width := 700;
                   gr[k].RedrawDiagram11Click(Nil);
                   AddGraphToBigBitmap(succ(j) + k * theCriteria.Count,theCriteria.Count,Filters1.Count,gr[k],BigBitmap);
               end;
               {$IfDef RecordBoxPlots} WriteLineToDebugFile(GISdb[db].MyData.Filter + ' done'); {$EndIf}
              aName := NextFileNumber(MDTempDir,'box_plot_' + ExtractFileNameNoExt(MDDef.DEMIX_filter1_fName),'.png');
              Bitmap := DEMIXTestDEMLegend(theDEMs);
              FinishBigBitMapWithLegend(BigBitmap,bitmap,aname);
           end;
       end;

       theDEMs.Destroy;
       Labels1.Destroy;
       Filters1.Destroy;
       theCriteria.Destroy;
       SetColorForWaiting;
       Self.Visible := true;
       GISdb[db].ApplyGISFilter(BaseFilter);
       {$IfDef RecordBoxPlots} WriteLineToDebugFile('Teval_scores_graph_form.BitBtn37Click out BoxPlots)'); {$EndIf}
   end;
end;


procedure Teval_scores_graph_form.BitBtn38Click(Sender: TObject);
begin
   SaveMDDefaults;
end;

procedure Teval_scores_graph_form.BitBtn39Click(Sender: TObject);
begin
  RadioGroup9.ItemIndex := 2;
end;

procedure Teval_scores_graph_form.BitBtn3Click(Sender: TObject);
begin
   wmdem.Closeallgraphs1Click(Sender);
   wmdem.Closeallpictureviewwindows1Click(Sender);
end;


procedure Teval_scores_graph_form.BitBtn40Click(Sender: TObject);
begin
   BoxCarFilters(2);
end;

procedure Teval_scores_graph_form.BitBtn41Click(Sender: TObject);
var
   DEMs,Criteria,Results : tStringList;
   aFilter,aLine,bf,Best,Worst: shortstring;
   j,k : integer;
   fName : PathStr;
   AverageFUV,BestAvg,WorstAvg : float32;
begin
   DEMs := MakeDEMlist;
   Criteria := MakeCriteriaList;
   bf := GISdb[DB].MyData.Filter;

   Results := tStringList.Create;
   aLine := 'CRITERION,N';
   for k := 0 to pred(DEMs.Count) do aLine := aline + ',' + DEMs.Strings[k];
   Results.Add(aline + ',' + 'BEST_DEM,WORST_DEM');

    for j := 0 to pred(Criteria.Count) do begin
       if (bf = '') then aFilter := 'CRITERION=' + QuotedStr(Criteria.Strings[j])
       else aFilter := bf + ' AND CRITERION=' + QuotedStr(Criteria.Strings[j]);
       GISdb[DB].ApplyGISFilter(aFilter);
       GISdb[DB].EmpSource.Enabled := false;
       {$If Defined(RecordDEMIXGraphFull)} WriteLineToDebugFile(aFilter + '  ' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
       if (GISdb[DB].MyData.FiltRecsInDB > 0) then begin
          aLine := Criteria.Strings[j] + ',' + IntToStr(GISdb[DB].MyData.FiltRecsInDB);
          BestAvg := 9999;
          WorstAvg := -9999;
          for k := 0 to pred(DEMs.Count) do begin
             if (GISdb[DB].MyData.FiltRecsInDB = 1) then
                 AverageFUV := GISdb[DB].MyData.GetFieldByNameAsFloat(DEMs.strings[k])
             else begin
                if MDDef.DEMIX_UseMedian then AverageFUV := GISdb[DB].MyData.FieldMedian(DEMs.strings[k])
                else AverageFUV := GISdb[DB].MyData.FieldAverage(DEMs.strings[k]);
             end;
             if AverageFUV < BestAvg then begin
                Best := DEMs.Strings[k];
                BestAvg := AverageFUV;
             end;
             if AverageFUV > WorstAvg then begin
                Worst := DEMs.Strings[k];
                WorstAvg := AverageFUV;
             end;
             GISdb[DB].EmpSource.Enabled := false;
             aline := aline + ',' + RealToString(AverageFUV,-12,-8);
          end;
          Results.Add(aline + ',' + Best + ',' + Worst);
       end;
    end;
    fName := NextFileNumber(MDtempDir,'data','.dbf');
    PetDBUtils.StringList2CSVtoDB(Results,fName);
    GISdb[DB].ApplyGISFilter(bf);
end;

procedure Teval_scores_graph_form.BitBtn42Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := DEMIXSettingsDir;
   if GetExistingFileName('DEMIX DEMs','dem*.txt',fName) then begin
      Memo3.Lines.LoadFromFile(fName);
   end;
end;

procedure Teval_scores_graph_form.BitBtn43Click(Sender: TObject);
var
   GeoFilters,Labels : tStringList;
   DB2,NewDB,NewDB2 : integer;
   fName : PathStr;
begin
   fName := ExtractFilePath(GISdb[db].dbFullName);
   if GetExistingFileName('Second FUV data base','Database files|*.dbf',fName) and OpenNumberedGISDataBase(DB2,fName,true) then begin
       {$IfDef RecordDEMIX} WriteLineToDebugFile('Teval_scores_graph_form.BitBtn43Click in, 2nd DB=' + ExtractFileNameNoExt(fName)); {$EndIf}
       Self.Visible := false;
       ExpandLegend := true;
       GeoFilters := GeomorphFiltersFromMixed1(Labels);
       MDcreatedCSV := true;
       {$IfDef RecordDEMIX} WriteLineToDebugFile('Sort first'); {$EndIf}
       NewDB := SortDataBase(db,true,false,'BEST_EVAL');
       {$IfDef RecordDEMIX} WriteLineToDebugFile('Sort second'); {$EndIf}
       MDcreatedCSV := true;
       NewDB2 := SortDataBase(db2,true,false,'BEST_EVAL');
       BestEvalGraphPerCriterionMultipleFilters(Newdb,NewDB2,'BEST_EVAL',GeoFilters,Labels,MakeCriteriaList(true),CriteriaFamily);
       Self.Visible := true;
       ExpandLegend := false;
       {$IfDef RecordDEMIX} WriteLineToDebugFile('Teval_scores_graph_form.BitBtn43Click out'); {$EndIf}
   end;
end;


procedure Teval_scores_graph_form.BitBtn44Click(Sender: TObject);
begin
(*
var
   DEMs,Criteria,Results : tStringList;
   i,j,k,ref,rg,tg,filtered : integer;
   AreaName,Criterion,DEMIX_TILE,aLine : shortstring;
   fName,BaseDir : PathStr;
   OpenMaps : boolean;
   tests : array[0..15] of integer;
   gl : tGridLimits;
begin
   Self.Visible := false;
   GetDEMIXpaths(True);

   AreaName := 'state_line';
   DEMIX_TILE := 'UTM_11N_x63y395';

   AreaName := 'es_granada';
   DEMIX_TILE := 'ES_HU30-0992-3';

   DEMs := AssembleDEMlist;
   Criteria := MakeCriteriaList;
   BaseDir := MDDef.DEMIX_BaseDir + AreaName + '\' + DEMIX_Tile + '_ref_test_dem\';

   OpenMaps := false;
   filtered := 0;

   fName := BaseDir + 'ref_dtm_srtm.tif';
   {$IfDef RecordDEMIX} HighlightLineToDebugFile('Load ref dtm=' + fname); {$EndIf}
   LoadNewDEM(ref,fName,OpenMaps);

   DEMglb[ref].AreaName := 'Reference_DTM_ELEV';
   for i := 0 to pred(DEMs.Count) do begin
      fName := BaseDir + DEMs[i] + '.tif';
      LoadNewDEM(tests[i],fName,OpenMaps);
   end;

   for i := 0 to pred(DEMs.Count) do begin
      if DEMs.Strings[i] = 'GEDTMV1_2' then begin
         filtered := WBT_Gaussian(false,fName,0.75,MDTEMPdir + 'Gaussian_' + DEMs.Strings[i] + '.tif');
      end;
   end;
   if ValidDEM(filtered) then begin
      tests[DEMs.Count] := filtered;
      DEMs.Add('GEDTM_filt');
   end;

   Results := tStringList.Create;
   aLine := 'AREA,DEMIX_TILE,CRITERION';
   for i := 0 to pred(DEMs.Count) do begin
      Aline := aLine + ',' + DEMs.Strings[i];
   end;
   Results.Add(aline);

   gl := DEMglb[Ref].FullDEMGridLimits;
   for i := 0 to pred(Criteria.Count) do begin
      Criterion := NoSuffixCriterion(Criteria.Strings[i]);
      {$IfDef RecordDEMIX} WriteLineToDebugFile(Criterion); {$EndIf}
      wmDEM.SetPanelText(2,Criterion ,true);
      aline := AreaName + ',' + DEMIX_TILE + ',' + Criterion;
      if (Criterion <> 'ELEV') then rg := CreateSingleLSPGrid(OpenMaps,ref,Criterion);
      for j := 0 to pred(DEMs.Count) do begin
        {$IfDef RecordDEMIX} WriteLineToDebugFile(DEMs[j]); {$EndIf}
        wmDEM.SetPanelText(1,DEMs[j],true);
        if (Criterion = 'ELEV') then begin
           aline := aLine + ',' + RealToString(GetFUVForPairGrids(gl,Ref,Tests[j]),-12,-8);
        end
        else begin
           tg := CreateSingleLSPGrid(OpenMaps,tests[j],Criterion);
           aline := aLine + ',' + RealToString(GetFUVForPairGrids(gl,rg,tg),-12,-8);
           CloseSingleDEM(tg);
        end;
     end;
     {$IfDef RecordDEMIX} WriteLineToDebugFile(aline); {$EndIf}
     Results.Add(aline);
     CloseSingleDEM(rg);
   end;
   fName := NextFileNumber(MDtempDir,'GEDTM_filtering','.dbf');
   PetDBUtils.StringList2CSVtoDB(Results,fName);
   CloseAllDEMs;
   EndDEMIXProcessing;
   Self.Visible := true;
*)
end;



procedure Teval_scores_graph_form.BitBtn45Click(Sender: TObject);
begin
   BestDEMonGraphTwoParameters(DB,MakeCriteriaList,MakeDEMlist);
end;

procedure Teval_scores_graph_form.BitBtn46Click(Sender: TObject);
begin
   try
      Self.Visible := false;
      PiesBestByTwoLandTypes(db,False,MakeCriteriaList,MakeDEMlist,ComboBox2.Text,ComboBox3.Text);
   finally
      Self.Visible := true;
   end;
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
   {$IfDef IncludeCoastalDEMs}
       if MDdef.DEMIX_AllowCoastal then begin
         TryOne('U10',db_U10,MDDef.DEMIX_U10DBfName);
         TryOne('U80',db_U80,MDDef.DEMIX_U80DBfName);
         TryOne('U120',db_U120,MDDef.DEMIX_U120DBfName);
       end;
   {$EndIf}
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
  RadioGroup9.ItemIndex := 0;
end;

procedure Teval_scores_graph_form.BitBtn8Click(Sender: TObject);
var
  GeoFilters,Labels : tStringList;
begin
   {$IfDef RecordDEMIX} HighlightLineToDebugFile('Teval_scores_graph_form.BitBtn8Click in'); {$EndIf}
    BitBtn4Click(Sender);   //If needed loads db_U10,db_U80,db_U120,db_Full))
    Self.Visible := false;
    GeoFilters := GeomorphFiltersFromMixed1(Labels);
    if ValidDB(db_Full) then BestEvalGraphPerCriterionMultipleFilters(db_Full,0,'BEST_EVAL',GeoFilters,Labels,MakeCriteriaList,'Full');
    if MDdef.DEMIX_AllowCoastal then begin
      if ValidDB(db_U120) then BestEvalGraphPerCriterionMultipleFilters(db_U120,0,'BEST_EVAL',GeoFilters,Labels,MakeCriteriaList,'U120');
      if ValidDB(db_U80) then BestEvalGraphPerCriterionMultipleFilters(db_U80,0,'BEST_EVAL',GeoFilters,Labels,MakeCriteriaList,'U80');
      if ValidDB(db_U10) then BestEvalGraphPerCriterionMultipleFilters(db_U10,0,'BEST_EVAL',GeoFilters,Labels,MakeCriteriaList,'U10');
    end;
    Self.Visible := true;
end;


procedure Teval_scores_graph_form.BitBtn9Click(Sender: TObject);
//deals with case when the database does not have all the expected criteria
var
   sl,sl2 : tStringList;
   i : integer;
   fName : shortstring;
begin
   Memo1.Clear;
   sl := GetListDEMIXOrderedCriteria(DEMIX_criteria_tolerance_fName);
   GISdb[db].DBFieldUniqueEntries('CRITERION',sl2);
   for I := 0 to pred(Sl.count) do begin
      fName := sl[i];
      if sl2.IndexOf(fName) <> -1 then begin
         Memo1.Lines.Add(fName);
      end;
   end;
   sl.Destroy;
   sl2.Destroy;
end;

procedure Teval_scores_graph_form.CheckBox10Click(Sender: TObject);
begin
   MDdef.DEMIX_ruff_filters := CheckBox10.Checked;
end;

procedure Teval_scores_graph_form.CheckBox11Click(Sender: TObject);
begin
   MDdef.DEMIX_barren_filters := CheckBox11.Checked;
end;

procedure Teval_scores_graph_form.CheckBox12Click(Sender: TObject);
begin
   MDdef.DEMIX_forest_filters := CheckBox12.Checked;
end;

procedure Teval_scores_graph_form.CheckBox13Click(Sender: TObject);
begin
   MDdef.DEMIX_urban_filters := CheckBox13.Checked;
end;

procedure Teval_scores_graph_form.CheckBox14Click(Sender: TObject);
begin
   MDdef.DEMIX_IgnoreTies := CheckBox14.Checked;
end;

procedure Teval_scores_graph_form.CheckBox15Click(Sender: TObject);
begin
   MDDef.DEMIX_average_criteria := CheckBox15.Checked;
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
   //MovieByTestDEM := CheckBox3.Checked;
end;

procedure Teval_scores_graph_form.CheckBox4Click(Sender: TObject);
begin
   MDdef.ShowPieN := CheckBox4.Checked;
end;

procedure Teval_scores_graph_form.CheckBox5Click(Sender: TObject);
begin
   MDDef.FUVExpandScales := CheckBox5.Checked;
end;

procedure Teval_scores_graph_form.CheckBox6Click(Sender: TObject);
begin
   MDdef.DEMIX_UseMedian := CheckBox6.Checked;
end;

procedure Teval_scores_graph_form.CheckBox8Click(Sender: TObject);
begin
   MDdef.DEMIX_slope_filters := CheckBox8.Checked;
end;

procedure Teval_scores_graph_form.ComboBox6Change(Sender: TObject);
begin
   MDDef.DEMIX_filter1_fName := DEMIXsettingsDir + ComboBox6.Text + '.dbf';
end;

procedure Teval_scores_graph_form.ComboBox7Change(Sender: TObject);
begin
   MDDef.DEMIX_filter2_fName := DEMIXsettingsDir + ComboBox7.Text + '.dbf';
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

procedure Teval_scores_graph_form.Edit7Change(Sender: TObject);
begin
   CheckEditString(Edit7.Text,MDDef.LSPsForVertLabels);

end;

procedure Teval_scores_graph_form.FormActivate(Sender: TObject);
begin
   RecognizeDEMIXVersion(DB);
end;



procedure Teval_scores_graph_form.FormCreate(Sender: TObject);
var
   DEMIXfilters : tStringList;
   i : integer;
begin
   CheckBox1.Checked := MDDef.DEMIX_combined_graph;
   CheckBox2.Checked := MDDef.PanelsByTestDEM;
   //CheckBox3.Checked := MovieByTestDEM;
   CheckBox4.Checked := MDdef.ShowPieN;
   CheckBox5.Checked := MDDef.FUVExpandScales;
   CheckBox6.Checked := MDdef.DEMIX_UseMedian;

   CheckBox8.Checked := MDdef.DEMIX_slope_filters;
   CheckBox10.Checked := MDdef.DEMIX_ruff_filters;
   CheckBox11.Checked := MDdef.DEMIX_barren_filters;
   CheckBox12.Checked := MDdef.DEMIX_forest_filters;
   CheckBox13.Checked := MDdef.DEMIX_urban_filters;

   CheckBox15.Checked := MDDef.DEMIX_average_criteria;

   Edit1.Text := IntToStr(MDDef.DEMIXlegendFontSize);
   Edit2.Text := IntToStr(MDDef.DEMIX_ysize);
   Edit3.Text := IntToStr(MDDef.DEMIX_xsize);
   Edit4.Text := IntToStr(MDDef.DEMIXUseBins);
   Edit5.Text := IntToStr(MDDef.NumGraphCols);
   Edit6.Text := IntToStr(MDDef.DefaultGraphFont.Size);
   Edit7.Text := IntToStr(MDDef.LSPsForVertLabels);

   RadioGroup5.ItemIndex := pred(MDDef.DemixSymSize);

   RadioGroup3.ItemIndex := MDDef.SummarySymbol;
   RadioGroup6.ItemIndex := MDdef.DEMIX_series_symbol;

   RadioGroup7.ItemIndex := MDDef.DEMIX_groupWonLost;
   RadioGroup12.ItemIndex := pred(MDdef.DEMIX_Line_Width);


   DEMIXfilters := tStringList.Create;
   FindMatchingFiles(DEMIXsettingsDir,'filters_*.dbf',DEMIXFilters);
   if (DEMIXfilters.Count > 0) then begin
       for i := 0 to pred(DEMIXfilters.Count) do begin
          ComboBox6.Items.Add(ExtractFileNameNoExt(DEMIXfilters.strings[i]));
          ComboBox7.Items.Add(ExtractFileNameNoExt(DEMIXfilters.strings[i]));
       end;

       if FileExists(MDDef.DEMIX_filter1_fName) then ComboBox6.Text := ExtractFileNameNoExt(MDDef.DEMIX_filter1_fName)
       else begin
          ComboBox6.Text := ComboBox6.Items[0];
          MDDef.DEMIX_filter1_fName := DEMIXsettingsDir + ComboBox6.Text + '.dbf';
       end;
       if FileExists(MDDef.DEMIX_filter2_fName) then ComboBox7.Text := ExtractFileNameNoExt(MDDef.DEMIX_filter2_fName)
       else begin
          ComboBox7.Text := ComboBox7.Items[pred(DEMIXfilters.Count)];
          MDDef.DEMIX_filter2_fName := DEMIXsettingsDir + ComboBox7.Text + '.dbf';
       end;
   end
   else begin
      MessageToContinue('filter files missing');
   end;

   db_U10 := 0;
   db_u80 := 0;
   db_u120 := 0;
   db_Full := 0;
   Width := 1064;
end;


procedure Teval_scores_graph_form.RadioGroup12Click(Sender: TObject);
begin
   MDDef.DEMIX_Line_Width := succ(RadioGroup12.ItemIndex);
end;

procedure Teval_scores_graph_form.RadioGroup2Click(Sender: TObject);
begin
  if (not ContinueExperimentalDEMIX) then exit;
  SetColorForProcessing;
  Self.Visible := false;
  Case RadioGroup2.ItemIndex of
     0 : DEMIX_AreaAverageScores_graph(DB,MakeDEMlist);
     1 : GraphAverageScoresByTile(DB,MakeDEMlist,Nil,Nil);
     2 : DEMIX_AreaAverageScores_graph(DB,MakeDEMlist,false);
     3 : DEMIX_Area_ind_criteria_graph(DB,MakeDEMlist);
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
   MDdef.DEMIX_series_symbol := RadioGroup6.ItemIndex;
end;

procedure Teval_scores_graph_form.RadioGroup7Click(Sender: TObject);
begin
   MDDef.DEMIX_groupWonLost := RadioGroup7.ItemIndex;
end;


procedure Teval_scores_graph_form.RadioGroup9Click(Sender: TObject);
var
   DoingRanks : boolean;
   BaseFilter : shortstring;
   Criteria,TheDEMs : tStringList;

      function AverageWithFilters(DB : integer; LandParam : shortstring; CriteriaInFilter : tStringList; AddLegend : boolean) : tThisBaseGraph;
      var
         MinHoriz,MaxHoriz : float64;
         NewDB,i : integer;
         Suff,HL,fName,UseFilter,tstr : ShortString;
         Value : integer;
         aMinVal,aMaxVal : float64;
         GeomorphFilters,Labels : tStringList;
      begin {function AverageWithFilter}
         {$IfDef RecordDEMIXByLandCover} WriteLineToDebugFile('AverageWithFilters in, ' + LandParam + 'average criteria=' + IntToStr(CriteriaInFilter.Count)); {$EndIf}
         wmDEM.SetPanelText(1,LandParam,true);
         if (LandParam = '') then ImportLandParamFilters(MDDef.DEMIX_filter1_fName, GeomorphFilters,Labels)
         else MakeLandParamFilters(LandParam,GeomorphFilters,Labels);
         if DoingRanks then Suff := '_SCR' else Suff := '';

         UseFilter := MakeCriteriaFilter(CriteriaInFilter);
         if (UseFilter = '') then UseFilter := BaseFilter
         else if (BaseFilter <> '') then UseFilter := UseFilter + ' AND ' + BaseFilter;

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
                if MDDef.DEMIX_UseMedian then tStr := 'median' else TStr := 'mean';
                if (CriteriaInFilter.Count = 1) then HL := TStr + ' FUV:' + CriteriaInFilter.Strings[0]
                else begin
                  HL := 'FUV ' + TStr + ': '  + CriteriaInFilter.Strings[0];
                  for i := 1 to pred(CriteriaInFilter.Count) do begin
                     HL := HL + '-' + CriteriaInFilter.Strings[i];
                  end;
                end;

                HL := StringReplace(HL, '_FUV', '',[rfReplaceAll, rfIgnoreCase]);

                if (GISdb[db].DEMIXdbtype in [ddbDiffDist]) then begin
                   MinHoriz := -9999;
                   MaxHoriz := -9999;
                end
                else if MDDef.FUVExpandScales then begin
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
                  MinHoriz := MinHoriz - 0.05 * (MaxHoriz-MinHoriz);
                  MaxHoriz := MaxHoriz + 0.05 * (MaxHoriz-MinHoriz);
                end
                else begin
                   MinHoriz := -0.05;
                   MaxHoriz := 1.05;
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


      procedure MultipleAverageRanksOrEvaluations(LeftMarginLabels : boolean; CriteriaInFilter : tStringList; Findings : tStringList = Nil); //var gr : tGraphArray);
      var
         BigBitmap,bmp : tMyBitmap;
         y,i,MaxLeft,MaxWide,NumGr : integer;
         Title : shortstring;
         gr : tGraphArray;
         fName : PathStr;
      begin
         {$IfDef RecordDEMIXByLandcover} WriteLineToDebugFile('MultipleAverageRanksOrEvaluations'); {$EndIf}
         NumGr := 0;
         if MDDef.DEMIX_slope_filters then begin gr[NumGr] := AverageWithFilters(DB,'AVG_SLOPE',CriteriaInFilter,false); inc(NumGr); end;
         if MDDef.DEMIX_ruff_filters then begin gr[NumGr] := AverageWithFilters(DB,'AVG_ROUGH',CriteriaInFilter,false); inc(NumGr); end;
         if MDDef.DEMIX_forest_filters then begin gr[NumGr] := AverageWithFilters(DB,'FOREST_PC',CriteriaInFilter,false); inc(NumGr); end;
         if MDDef.DEMIX_barren_filters then begin gr[NumGr] := AverageWithFilters(DB,'BARREN_PC',CriteriaInFilter,false); inc(NumGr); end;
         if MDDef.DEMIX_urban_filters then begin gr[NumGr] := AverageWithFilters(DB,'URBAN_PC',CriteriaInFilter,false); inc(NumGr); end;
         MaxLeft := 0;
         MaxWide := 0;
         for i := 0 to pred(NumGR) do begin
            if (gr[i].GraphDraw.LeftMargin > MaxLeft) then MaxLeft := gr[i].GraphDraw.LeftMargin;
            if (gr[i].Width > MaxWide) then MaxWide := gr[i].Width;
         end;
         for i := 0 to pred(NumGR) do begin
            gr[i].GraphDraw.LeftMargin := MaxLeft;
            gr[i].Width := MaxWide;
         end;
         for i := 0 to pred(NumGR) do begin
            gr[i].RedrawDiagram11Click(Nil);
         end;
         fName := MergeGraphPanelsVertical(NumGr,gr,(Not LeftMarginLabels),nil);
         if Findings = Nil then DisplayBitmap(fName,'DEMIX')
         else Findings.Add(fName);
         {$IfDef RecordDEMIXByLandCover} WriteLineToDebugFile('Teval_scores_graph_form.BitBtn40Click out'); {$EndIf}
      end;


      procedure MultipleByEachCriterion(Choice : integer);
      var
         i : integer;
         CriteriaInFilter,Findings : tStringList;
         Legend : tMyBitmap;
         gr : tThisBaseGraph;
      begin
         {$IfDef RecordDEMIX} WriteLineToDebugFile('In MultipleByEachCriterion, Criteria=' + IntToStr(Criteria.Count)); {$EndIf}
         Findings := tStringList.Create;
         for i := 0 to pred(Criteria.Count) do begin
            CriteriaInFilter := tStringList.Create;
            CriteriaInFilter.Add(Criteria.Strings[i]);
            {$IfDef RecordDEMIX} WriteLineToDebugFile('MultipleByEachCriterion: ' + Criteria.Strings[i]); {$EndIf}
            wmDEM.SetPanelText(2,'Criterion ' + IntToStr(succ(i)) + '/' + IntToStr(Criteria.Count) + ' ' + Criteria.Strings[i],true);
            if (Choice = 0) then begin
               gr := AverageWithFilters(DB,'',CriteriaInFilter,false);
               Findings.Add(gr.SaveGraphName);
            end
            else if (Choice = 2) then MultipleAverageRanksOrEvaluations((i=0),CriteriaInFilter,Findings);
            CriteriaInFilter.Destroy;
         end;
         Legend := DEMIXTestDEMLegend(MakeDEMlist);
         MergeVerticalPanels(Findings,Legend,'DEMIX');
         {$IfDef RecordDEMIX} WriteLineToDebugFile('Out MultipleAverageRanksOrEvaluations, Criteria=' + IntToStr(Criteria.Count)); {$EndIf}
      end;


var
   Choice : integer;
begin {Teval_scores_graph_form.RadioGroup9Click}
   Choice := RadioGroup9.ItemIndex;

    TheDEMs := MakeDEMlist;
    Criteria := MakeCriteriaList;
    BaseFilter := GISdb[DB].MyData.Filter;

    try
        GetDEMIXpaths(True);
        Self.Visible := false;
        {$IfDef RecordDEMIX} HighlightLineToDebugFile('Enter Teval_scores_graph_form.RadioGroup9Click, choice=' + IntToStr(Choice) + ' DEMs=' + IntToStr(TheDEMs.Count)); {$EndIf}
        case Choice of
           0 : if MDDef.DEMIX_average_criteria then AverageWithFilters(DB,'',Criteria,true) else MultipleByEachCriterion(Choice);
           1 : begin
                   if MDDef.DEMIX_slope_filters then AverageWithFilters(DB,'AVG_SLOPE',Criteria,true);
                   if MDDef.DEMIX_ruff_filters then AverageWithFilters(DB,'AVG_ROUGH',Criteria,true);
                   if MDDef.DEMIX_forest_filters then AverageWithFilters(DB,'FOREST_PC',Criteria,true);
                   if MDDef.DEMIX_barren_filters then AverageWithFilters(DB,'BARREN_PC',Criteria,true);
                   if MDDef.DEMIX_urban_filters then AverageWithFilters(DB,'URBAN_PC',Criteria,true);
               end;
           2 : if MDDef.DEMIX_average_criteria then MultipleAverageRanksOrEvaluations(true,Criteria) else MultipleByEachCriterion(Choice);
        end;
    finally
      RadioGroup9.ItemIndex := -1;
      {$IfDef RecordDEMIX} HighlightLineToDebugFile('Exit Teval_scores_graph_form.RadioGroup9Click, choice=' + IntToStr(Choice) + ' DEMs=' + IntToStr(TheDEMs.Count)); {$EndIf}
      TheDEMs.Destroy;
      Criteria.Destroy;
      EndDEMIXProcessing(db);
      GISdb[DB].ApplyGISFilter(BaseFilter);
      Self.Visible := true;
    end;
end {Teval_scores_graph_form.RadioGroup9Click};


initialization
finalization
end.
