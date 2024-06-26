unit demix_evals_scores_graphs;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}


//{$Define ConvertDBFtoDB}


{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      {$Define RecordDEMIX}
      //{$Define RecordChangeDB}
   {$Else}
   {$EndIf}
{$EndIf}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,StrUtils,
  BaseGraf,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  Teval_scores_graph_form = class(TForm)
    BitBtn1: TBitBtn;
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
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    RadioGroup5: TRadioGroup;
    BitBtn11: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn13: TBitBtn;
    CheckBox4: TCheckBox;
    BitBtn14: TBitBtn;
    //BitBtn15: TBitBtn;
    BitBtn16: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn17: TBitBtn;
    BitBtn23: TBitBtn;
    BitBtn24: TBitBtn;
    Edit4: TEdit;
    Label4: TLabel;
    RadioGroup3: TRadioGroup;
    RadioGroup6: TRadioGroup;
    BitBtn25: TBitBtn;
    BitBtn26: TBitBtn;
    Label5: TLabel;
    Edit5: TEdit;
    BitBtn27: TBitBtn;
    RadioGroup7: TRadioGroup;
    Memo2: TMemo;
    BitBtn28: TBitBtn;
    BitBtn29: TBitBtn;
    BitBtn30: TBitBtn;
    BitBtn31: TBitBtn;
    BitBtn32: TBitBtn;
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
    BitBtn36: TBitBtn;
    ComboBox1: TComboBox;
    Edit6: TEdit;
    Label6: TLabel;
    RadioGroup8: TRadioGroup;
    RadioGroup9: TRadioGroup;
    BitBtn4: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn22: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
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
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure RadioGroup5Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    //procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure BitBtn19Click(Sender: TObject);
    procedure BitBtn21Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure BitBtn23Click(Sender: TObject);
    procedure BitBtn24Click(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure RadioGroup6Click(Sender: TObject);
    procedure BitBtn25Click(Sender: TObject);
    procedure BitBtn26Click(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure BitBtn20Click(Sender: TObject);
    procedure BitBtn27Click(Sender: TObject);
    procedure RadioGroup7Click(Sender: TObject);
    procedure BitBtn28Click(Sender: TObject);
    procedure BitBtn29Click(Sender: TObject);
    procedure BitBtn30Click(Sender: TObject);
    procedure BitBtn31Click(Sender: TObject);
    procedure BitBtn32Click(Sender: TObject);
    procedure BitBtn33Click(Sender: TObject);
    procedure BitBtn34Click(Sender: TObject);
    procedure BitBtn35Click(Sender: TObject);
    procedure BitBtn36Click(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
    procedure RadioGroup8Click(Sender: TObject);
    procedure RadioGroup9Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn22Click(Sender: TObject);
  private
    { Private declarations }
    procedure ChangeDBonForm(Newdb : integer);
    function AssembleDEMlist : tStringList;
    function Criteria : tStringList;
    function MakeGeomorphFilters: tStringList;
    procedure LoadDEMsInMemo;
  public
    { Public declarations }
     db_U10,db_u80,db_u120,db_Full,
     db,originalDB : integer;
     XaxisParam,YaxisParam,ValueParam : shortstring;
     UseDEMs : tStringList;
  end;

var
  eval_scores_graph_form : Teval_scores_graph_form;


procedure StartDEMIXgraphs(DB : integer);

implementation

{$R *.dfm}

uses
   Nevadia_main,
   Petmar,Petmar_types,Petmar_db,Petimage_form,PetImage,
   DEMdataBase,
   DEMdefs,
   DEMIX_graphs,
   DEMIX_control,
   DEMIX_definitions;

const
   DoingRanks : boolean = true;


procedure StartDEMIXgraphs(DB : integer);
var
  eval_scores_graph_form : Teval_scores_graph_form;
  i : integer;
begin
   GetDEMIXpaths(false,db);
   eval_scores_graph_form := Teval_scores_graph_form.Create(Application);
   if (db = -4) then begin
      eval_scores_graph_form.BitBtn4Click(nil);
      db := eval_scores_graph_form.db;
   end;

   RecognizeDEMIXVersion(DB);
   //SetParamsForDEMIXmode;
   LoadDEMIXnames;
   GISdb[db].dbtablef.OpenDEMIXgraphs1.Enabled := false;
   eval_scores_graph_form.ChangeDBonForm(db);
   eval_scores_graph_form.Show;
end;


procedure Teval_scores_graph_form.LoadDEMsInMemo;
var
   i : integer;
   fName : shortstring;
   theDEMs : tStringList;
begin
   Memo3.Clear;
   for I := 1 to NumDEMIXtestDEM do begin
      fName := DEMIXshort[i];
      if GISdb[db].MyData.FieldExists(fName) and UseRetiredDEMs[i] then begin
         Memo3.Lines.Add(fName);
      end;
   end;
   if (Memo3.Lines.Count = 0) and GISdb[db].MyData.FieldExists('DEM') then begin
      theDEMs := GISdb[db].MyData.ListUniqueEntriesInDB('DEM');
      for I := 0 to pred(theDEMs.Count) do begin
         Memo3.Lines.Add(theDEMs[i]);
      end;
   end;
end;


procedure Teval_scores_graph_form.ChangeDBonForm(Newdb : integer);
var
   i : integer;
   fName : shortstring;
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
      BitBtn13.Enabled := GISdb[DB].MyData.FieldExists('BARREN_PC') and GISdb[DB].MyData.FieldExists('AVG_SLOPE');
      BitBtn24.Enabled := GISdb[DB].MyData.FieldExists('BARREN_PC') and GISdb[DB].MyData.FieldExists('AVG_SLOPE');
      BitBtn25.Enabled := GISdb[DB].MyData.FieldExists('BARREN_PC') and GISdb[DB].MyData.FieldExists('AVG_SLOPE');
      BitBtn26.Enabled := GISdb[DB].MyData.FieldExists('BARREN_PC') and GISdb[DB].MyData.FieldExists('AVG_SLOPE');
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

function Teval_scores_graph_form.Criteria : tStringList;
var
   j : integer;
begin
   Result := tStringList.Create;
   for j := 0 to pred(Memo1.Lines.Count) do begin
      Result.Add(Memo1.Lines[j]);
   end;
end;


procedure Teval_scores_graph_form.BitBtn10Click(Sender: TObject);
var
   i,j,k,ColBigBitmap : integer;
   HL : shortstring;
   aName : PathStr;
   Legend,BigBitmap : tMyBitmap;
   GeomorphFilters,DEMList: tStringList;


   procedure DoElevationRange(adb : integer; BaseCompareDEM,aFilter : shortstring);
   var
      j : integer;
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
            gr := WinningPercentagesComparedToCOP(db,BaseCompareDEM,Criteria,DEMList,HL);
            AddGraphToBigBitmap(ColBigBitmap,pred(DEMList.Count),1,gr,BigBitmap);
            {$IfDef RecordDEMIX} WriteLineToDebugFile('Added '+ ' DEMs=' + IntToStr(DEMList.Count)); {$EndIf}
            gr.Destroy;
         end
         else begin
            {$IfDef RecordDEMIX} WriteLineToDebugFile('DEM ' + BaseCompareDEM +  ' not in ' + DEMIXModeName); {$EndIf}
         end;
         {$IfDef RecordDEMIX} WriteLineToDebugFile('DoElevationRange out, DEMs=' + IntToStr(DEMList.Count)); {$EndIf}
      end
      else begin
         {$IfDef RecordDEMIX} WriteLineToDebugFile('DoElevationRange, no filter matches for ' + aFilter); {$EndIf}
      end;
   end;


begin
  {$IfDef RecordDEMIX} HighlightLineToDebugFile('Teval_scores_graph_form.BitBtn10Click in'); {$EndIf}
   if GISdb[db].MyData.FieldExists('AVG_SLOPE') then begin
      //GeomorphFilters := MakeGeomorphFilters;
      GeomorphFilters := tStringList.Create;
      GeomorphFilters.Add('');
      for i := 0 to pred(GeomorphFilters.Count) do begin
            DEMList := tStringList.Create;   //AssembleDEMList;
            DEMList.Add('DELTA');
            DEMList.Add('DILUV');
            DEMList.Add('COAST');
            DEMList.Add('FABDEM');
            DEMList.Add('ASTER');
            DEMList.Add('NASA');
            DEMList.Add('SRTM');
            DEMList.Add('TANDEM');
            DEMList.Add('ALOS');
            DEMList.Add('COP');
        {$IfDef RecordDEMIX} WriteLineToDebugFile('DEMs=' + IntToStr(DEMList.Count)); {$EndIf}
         for j := 7 to 9 do begin
           {$IfDef RecordDEMIX} WriteLineToDebugFile('j=' + IntToStr(j) + '  ' + DEMList[j] +'  DEMs=' + IntToStr(DEMList.Count)); {$EndIf}
           ColBigBitmap := 0;
           CreateBitmap(BigBitmap,1200,4500);
           DoElevationRange(db_U10,DEMlist[j],GeomorphFilters[i]);
           DoElevationRange(db_U80,DEMlist[j],GeomorphFilters[i]);
           DoElevationRange(db_U120,DEMlist[j],GeomorphFilters[i]);
           DoElevationRange(db_Full,DEMlist[j],GeomorphFilters[i]);
           {$IfDef RecordDEMIX} WriteLineToDebugFile('DEMs=' + IntToStr(DEMList.Count)); {$EndIf}
           Legend := DEMIXTestDEMLegend(true, DEMList);     //gr[1].MakeLegend(gr[1].GraphDraw.LegendList,false);
           aName := MDTempDir + 'win_lose_tie_' + DEMlist[j] + '.png';
           FinishBigMap(BigBitmap,Legend,aname);
           {$IfDef RecordDEMIX} WriteLineToDebugFile('j=' + IntToStr(j) + '  Saved, ' + aName + '  DEMs=' + IntToStr(DEMList.Count)); {$EndIf}
         end;
         {$IfDef RecordDEMIX} WriteLineToDebugFile('Done DEMs loop'); {$EndIf}
         DEMList.Destroy;
         {$IfDef RecordDEMIX} WriteLineToDebugFile('DEMList.Destroyed'); {$EndIf}
      end;
      GISdb[db].ClearGISFilter;
      GISdb[db].ShowStatus;
      GeomorphFilters.Destroy;

   end
   else MessageToContinue('Add tile characters to DB');
end;

procedure Teval_scores_graph_form.BitBtn11Click(Sender: TObject);
begin
   DEMIX_evaluations_graph(DB,yasBarren,AssembleDEMList,Criteria,true);
end;

procedure Teval_scores_graph_form.BitBtn12Click(Sender: TObject);
begin
   WhiskerPlotsByCluster(DB);
end;

procedure Teval_scores_graph_form.BitBtn13Click(Sender: TObject);
begin
   BestBySlopeRough(db,Criteria,true,XaxisParam,YaxisParam,ValueParam);
end;

procedure Teval_scores_graph_form.BitBtn14Click(Sender: TObject);
begin
   DEMIX_evaluations_graph(DB,yasForest,AssembleDEMList,Criteria,true);
end;


procedure Teval_scores_graph_form.BitBtn16Click(Sender: TObject);
begin
   DEMIX_evaluations_graph(DB,yasBestEvalColoredBySlope,AssembleDEMList,Criteria,true);
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
   FilterJustOneGraph(DB,Criteria,MakeGeomorphFilters,true);
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
   DEMIX_evaluations_graph(DB,yasBestEval,AssembleDEMList,Criteria,true);
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

procedure Teval_scores_graph_form.BitBtn24Click(Sender: TObject);
begin
   BestBySlopeRough(db,Criteria,false,XaxisParam,YaxisParam,'BEST_EVAL');
end;

procedure Teval_scores_graph_form.BitBtn25Click(Sender: TObject);
begin
   BestBySlopeRough(db,Criteria,false,XaxisParam,YaxisParam,'COP');
end;

procedure Teval_scores_graph_form.BitBtn26Click(Sender: TObject);
begin
   BestBySlopeRough(db,Criteria,false,XaxisParam,YaxisParam,'ALOS');
end;

procedure Teval_scores_graph_form.BitBtn27Click(Sender: TObject);
begin
   LastDataBase := DEMIX_final_DB_dir;
   db := OpenMultipleDataBases('New DB for DEMIX graphs');
   ChangeDBonForm(db);
end;


procedure Teval_scores_graph_form.BitBtn28Click(Sender: TObject);
var
   i,j : integer;
   HL,TopLabel : shortstring;
   Filters : tStringList;
   gr : array[0..25] of tThisBaseGraph;
   Legend,BigBitmap : tMyBitmap;
begin
   if GISdb[db].MyData.FieldExists('AVG_SLOPE') then begin
      Filters := MakeGeomorphFilters;
      for i := 0 to pred(Filters.Count) do begin //filters to tile characteristics
         if Filters.Strings[i] = '(None)' then GISdb[db].ClearGISFilter
         else GISdb[db].ApplyGISFilter(Filters.Strings[i]);
         HL := DEMIXModeName + '_' + CriteriaFamily + '_Evaluation';
         TopLabel := GISdb[db].MyData.Filter + NumTilesString(DB);
         gr[i] := PlotBestEvalVersusPercentileMultipleCriteria(DB,Criteria,true,TopLabel,HL);
         if MDDef.DEMIX_combined_graph then AddGraphToBigBitmap(succ(i),Filters.Count,1,gr[i],BigBitmap);
      end;
      if MDDef.DEMIX_combined_graph then begin
         Legend := gr[1].MakeLegend(gr[1].GraphDraw.LegendList,false);
         FinishBigMap(BigBitmap,Legend,'',true);
      end;
      GISdb[db].ClearGISFilter;
      GISdb[db].ShowStatus;
   end
   else MessageToContinue('Add tile characters to DB');
end;


procedure Teval_scores_graph_form.BitBtn29Click(Sender: TObject);
var
   i,j : integer;
   HL : shortstring;
   gr : array[0..25] of tThisBaseGraph;
   Legend,BigBitmap : tMyBitmap;
   GeomorphFilters: tStringList;
begin
   if GISdb[db].MyData.FieldExists('AVG_SLOPE') then begin
      GeomorphFilters := MakeGeomorphFilters;
      j := 0;
      for i := 0 to pred(GeomorphFilters.Count) do begin
         GISdb[db].ApplyGISFilter(GeomorphFilters.Strings[i]);
         if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
            HL := GISdb[DB].MyData.Filter + ' (tiles=' + IntToStr(GISdb[DB].NumUniqueEntriesInDB('DEMIX_TILE')) + ')';
            inc(j);
            gr[j] := WinningPercentagesComparedToCOP(db,ComboBox1.Text,Criteria,AssembleDEMList,HL);
            if MDDef.DEMIX_combined_graph then AddGraphToBigBitmap(j,Memo2.Lines.Count,1,gr[j],BigBitmap);
         end;
      end;
      if MDDef.DEMIX_combined_graph then begin
         Legend := gr[1].MakeLegend(gr[1].GraphDraw.LegendList,false);
         FinishBigMap(BigBitmap,Legend);
      end;
      GISdb[db].ClearGISFilter;
      GISdb[db].ShowStatus;
   end
   else MessageToContinue('Add tile characters to DB');
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
   i,NewDB,MinHoriz,MaxHoriz : integer;
   Suff,HL : ShortString;
   DEMList : tStringList;
begin
   HL := BeforeSpecifiedString(GISdb[db].dbName,'DEMIX');
   DEMList := AssembleDEMList;
   if DoingRanks then begin
      HL := 'Average Ranks';
      Suff := '_SCR';
      MinHoriz := 1;
      MaxHoriz := DEMList.Count;
   end
   else begin
      HL := 'Average Evaluations';
      Suff := '';
      MinHoriz := 0;
      MaxHoriz := 1;
   end;
   NewDB := AverageScoresOfDEMs(DB,AssembleDEMList,Suff,MakeGeomorphFilters);
   AverageScoresGraph(NewDB,DEMList,HL,true,MinHoriz,MaxHoriz);
   DEMList.Destroy;
end;

procedure Teval_scores_graph_form.BitBtn32Click(Sender: TObject);
begin
   BitBtn31Click(Sender);
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

procedure Teval_scores_graph_form.BitBtn36Click(Sender: TObject);
begin
   LoadDEMsInMemo;
end;


procedure Teval_scores_graph_form.BitBtn3Click(Sender: TObject);
begin
   wmdem.Closeallgraphs1Click(Sender);
   wmdem.Closeallpictureviewwindows1Click(Sender);
end;


procedure Teval_scores_graph_form.BitBtn4Click(Sender: TObject);

   procedure TryOne(Which : shortString; var db_num : integer; var fName : PathStr);
   begin
      if FileExists(fName) then begin
         OpenNumberedGISDataBase(db_num,fName,True);
      end
      else begin
         db_num := OpenMultipleDataBases(Which + ' DB for DEMIX graphs');
         fName := GISdb[db_num].dbFullName;
      end;
   end;

begin
   LastDataBase := DEMIX_final_DB_dir;
   TryOne('U10',db_U10,MDDef.DEMIX_U10DBfName);
   TryOne('U80',db_U80,MDDef.DEMIX_U80DBfName);
   TryOne('U120',db_U120,MDDef.DEMIX_U120DBfName);
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
   DEMIX_evaluations_graph(DB,yasSlope,AssembleDEMList,Criteria,true);
end;

procedure Teval_scores_graph_form.BitBtn8Click(Sender: TObject);
begin
   DEMIX_evaluations_graph(DB,yasRuff,AssembleDEMList,Criteria,true);
end;

procedure Teval_scores_graph_form.BitBtn9Click(Sender: TObject);
begin
   //YaxisWhat := yasRelief;
   DEMIX_evaluations_graph(DB,yasRelief,AssembleDEMList,Criteria,true);
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
var
   i : integer;
begin
   MDDef.DEMIX_graph_Retired_DEMs := CheckBox4.Checked;
   Memo3.Lines.Clear;
   for I := 1 to NumDEMIXtestDEM do begin
      UseRetiredDEMs[i] := MDDef.DEMIX_graph_Retired_DEMs or ((DEMIXshort[i] <> 'ASTER') and (DEMIXshort[i] <> 'NASA') and (DEMIXshort[i] <> 'SRTM'));
      if UseRetiredDEMs[i] then Memo3.Lines.Add(DEMIXshort[i]);
   end;
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
   //YAxisWhat := RadioGroup2.ItemIndex;
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

(*
   XaxisParam := 'BARREN_PC';
   YaxisParam := 'AVG_SLOPE';
   ValueParam := 'BEST_EVAL';
   UseDEMs := Nil;
*)
   //Criteria := Nil;
end;

procedure Teval_scores_graph_form.RadioGroup2Click(Sender: TObject);
begin
   if RadioGroup2.ItemIndex = 0 then DEMIX_AreaAverageScores_graph(DB,AssembleDEMList);
   if RadioGroup2.ItemIndex = 1 then GraphAverageScoresByTile(DB,AssembleDEMList,Nil,Nil);
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

      function AverageScoreWithFilters(DB : integer; DEMList : tStringList; LandParam : shortstring; LowBin,BinSize,HighBin : integer; AddLegend : boolean) : tThisBaseGraph;
      var
         MinHoriz,MaxHoriz,NewDB,i : integer;
         Suff,HL : ShortString;
         Slope : integer;
         GeomorphFilters,Labels : tStringList;
      begin
         {$IfDef RecordDEMIX} WriteLineToDebugFile('AverageScoreWithFilters, ' + LandParam); {$EndIf}
         Labels := tStringList.Create;
         GeomorphFilters := tStringList.Create;

         if LandParam = 'Users' then begin
            for i := 0 to pred(Memo2.Lines.Count) do begin
               if Memo2.Lines[i] = '(None)' then begin
                 GeomorphFilters.Add('');
                 Labels.Add('All tiles');
               end
               else begin
                  GeomorphFilters.Add(Memo2.Lines[i]);
                  Labels.Add(Memo2.Lines[i]);
               end;
            end;
         end
         else begin
            Labels.Add('All tiles');
            Labels.Add(LandParam + '<' + IntToStr(LowBin) + '%');
            GeomorphFilters.Add('');
            GeomorphFilters.Add(LandParam + '<' + IntToStr(LowBin));
            Slope := LowBin;
            while Slope <= HighBin do begin
               GeomorphFilters.Add(LandParam + '>=' + IntToStr(Slope) + ' AND ' + LandParam + '<' + IntToStr(Slope + BinSize));
               Labels.Add(LandParam + ' ' + IntToStr(Slope) + ' to ' + IntToStr(Slope + BinSize)+ '%');
               Slope := Slope + BinSize;
            end;
            GeomorphFilters.Add(LandParam + '>' + IntToStr(HighBin));
            Labels.Add(LandParam + '>' + IntToStr(HighBin) + '%');
         end;

         if DoingRanks then begin
            HL := 'Average Ranks';
            Suff := '_SCR';
            MinHoriz := 1;
            MaxHoriz := DEMList.Count;
         end
         else begin
            HL := 'Average Evaluations';
            Suff := '';
            MinHoriz := 0;
            MaxHoriz := 1;
         end;

         NewDB := AverageScoresOfDEMs(DB,DEMList,Suff,GeomorphFilters,Labels);
         Result := AverageScoresGraph(NewDB,DEMList,HL,AddLegend,MinHoriz,MaxHoriz);
      end;

      procedure MultipleAverageRanksOrEvaluations;
      var
         BigBitmap,bmp : tMyBitmap;
         y : integer;
         gr1,gr2,gr3,gr4 : tThisBaseGraph;
      begin
         {$IfDef RecordDEMIX} WriteLineToDebugFile('Teval_scores_graph_form.BitBtn40Click in'); {$EndIf}
         CreateBitmap(BigBitmap,1200,4500);
         BigBitmap.Canvas.Font.Size := 20;
         BigBitmap.Canvas.Font.Style := [fsBold];
         gr1 := AverageScoreWithFilters(DB,AssembleDEMList,'BARREN_PC',10,10,90,false);
         BigBitmap.Canvas.TextOut(gr1.GraphDraw.LeftMargin + 5,2, DEMIXModeName + ', ' + CriteriaFamily + ' Criteria');
         y := 15 + BigBitmap.Canvas.TextHeight(CriteriaFamily + ' Criteria');

         CopyImageToBitmap(gr1.Image1,bmp);
         BigBitmap.Canvas.Draw(2,y,bmp);
         y := y + 10 + bmp.Height;
         gr2 := AverageScoreWithFilters(DB,AssembleDEMList,'FOREST_PC',10,10,90,false);
         CopyImageToBitmap(gr2.Image1,bmp);
         BigBitmap.Canvas.Draw(2,y,bmp);
         y := y + 10 + bmp.Height;
         gr3 := AverageScoreWithFilters(DB,AssembleDEMList,'AVG_ROUGH',2,2,20,false);
         CopyImageToBitmap(gr3.Image1,bmp);
         BigBitmap.Canvas.Draw(2,y,bmp);
         y := y + 10 + bmp.Height;
         gr4 := AverageScoreWithFilters(DB,AssembleDEMList,'AVG_SLOPE',5,5,70,true);
         CopyImageToBitmap(gr4.Image1,bmp);
         BigBitmap.Canvas.Draw(2,y,bmp);
         GetImagePartOfBitmap(BigBitmap);
         DisplayBitmap(BigBitmap);
         {$IfDef RecordDEMIX} WriteLineToDebugFile('Teval_scores_graph_form.BitBtn40Click out'); {$EndIf}
      end;


var
   Choice : integer;
begin
    DoingRanks := Sender = RadioGroup8;
    if Sender = RadioGroup8 then Choice := RadioGroup8.ItemIndex
    else Choice := RadioGroup9.ItemIndex;

    case Choice of
       0 : AverageScoreWithFilters(DB,AssembleDEMList,'Users',5,5,70,true);
       1 : AverageScoreWithFilters(DB,AssembleDEMList,'AVG_SLOPE',5,5,70,true);
       2 : AverageScoreWithFilters(DB,AssembleDEMList,'AVG_ROUGH',2,2,20,true);
       3 : AverageScoreWithFilters(DB,AssembleDEMList,'FOREST_PC',10,10,90,true);
       4 : AverageScoreWithFilters(DB,AssembleDEMList,'BARREN_PC',10,10,90,true);
       5 : MultipleAverageRanksOrEvaluations;
    end;
    RadioGroup8.ItemIndex := -1;
    RadioGroup9.ItemIndex := -1;
end;

initialization
finalization
end.
