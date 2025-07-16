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
    Label7: TLabel;
    Label8: TLabel;
    BitBtn1: TBitBtn;
    RadioGroup3: TRadioGroup;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn38: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtn11: TBitBtn;
    BitBtn13: TBitBtn;
    BitBtn14: TBitBtn;
    BitBtn15: TBitBtn;
    BitBtn24: TBitBtn;
    BitBtn25: TBitBtn;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    RadioGroup10: TRadioGroup;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    Label9: TLabel;
    Label10: TLabel;
    BitBtn26: TBitBtn;
    BitBtn31: TBitBtn;
    BitBtn32: TBitBtn;
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
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn24Click(Sender: TObject);
    procedure BitBtn25Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure BitBtn26Click(Sender: TObject);
    procedure BitBtn31Click(Sender: TObject);
    procedure BitBtn32Click(Sender: TObject);
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
   DEMCoord,
   DEMstat,
   DEMmapf,
   DEM_manager,
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
      //eval_scores_graph_form.ComboBox4.Items := FieldsInDB;
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
         Memo3.Lines.Add(theDEMs.Strings[i]);
         Combobox4.Items.Add(theDEMs.Strings[i]);
         Combobox5.Items.Add(theDEMs.Strings[i]);
      end;
      Combobox4.Text := theDEMs.Strings[0];
      Combobox5.Text := theDEMs.Strings[1];
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
   if TileCharacteristicsInDB(DB) then begin
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



procedure Teval_scores_graph_form.BitBtn13Click(Sender: TObject);
var
   TheDEMs,Criteria,Findings,Filters1,DEMs2,GraphList : tStringList;
   aLine,DEM,ThisWinners : shortstring;
   fName : PathStr;
   db2,i,j,k,This,TotalWinners,Slope : integer;
   Winners : array[0..50] of integer;
   Tolerance : float32;


    function OneGraph(SlopeFilter : shortstring; TheDEMs : tStringList; WhichDEMs : shortstring) : tThisBaseGraph;
    var
       i,j,k : integer;
       TStr : shortstring;
    begin
       Findings := tStringList.Create;
       aline := 'CRITERION,TOLERANCE,TILES,WINNERS';
       for j := 0 to pred(TheDEMs.Count) do aline := aline + ',' + theDEMs.Strings[j];
       Findings.Add(aline);
        for i := 0 to pred(Criteria.Count) do begin
           GISdb[db].ApplyGISFilter(SlopeFilter + ' AND CRITERION=' + QuotedStr(Criteria.Strings[i]));
           GISdb[db].EmpSource.Enabled := false;
           Tolerance := CriterionTieTolerance(Criteria.Strings[i]);
           aline := Criteria.Strings[i] + ',' + RealToString(Tolerance,-8,-6) + ',' + IntToStr(GISdb[db].MyData.FiltRecsInDB);
           for k := 0 to 50 do Winners[k] := 0;
           TotalWinners := 0;

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
        TStr := SlopeFilter;
        StripInvalidPathNameChars(TStr);

        fName := NextFileNumber(MDtempDir,'wins_by_' + TStr + '_','.dbf');
        db2 := StringList2CSVtoDB(Findings,fName);
        Result := FractionOfWinnersGraph(db2,TheDEMs,Criteria);
        Result.GraphDraw.LLCornerText := SlopeFilter + ' ' + WhichDEMs;
        Result.GraphDraw.LeftMargin := 100;
        Result.Height := 100 + 25 * succ(Criteria.Count);
        Result.GraphDraw.MarginsGood := true;
        Result.RedrawDiagram11Click(Nil);
        FinishGraph(Result,GraphList);
    end;


var
   gr : tGraphArray;
   Panels : tStringList;
   BigBitmap,Bitmap,Legend : tMyBitmap;
   StartY : integer;
begin
    Self.Visible := false;
    GetDEMIXpaths(True);
    TheDEMs := AssembleDEMList;
    Criteria := MakeCriteriaList;

    GISdb[DB].EmpSource.Enabled := false;


    Filters1 := tStringList.Create;
    Filters1.LoadFromFile(DEMIXSettingsDir + 'filters_avg_slope.txt');
    DEMs2 := tStringList.Create;
    DEMs2.LoadFromFile(DEMIXSettingsDir + 'DEMs_2.txt');

    GraphList := tStringList.Create;
    Findings := tStringList.Create;
    Panels := tStringList.Create;
    for i := 0 to pred(Filters1.Count) do begin
       wmDEM.SetPanelText(1,IntToStr(succ(i)) + '/' + IntToStr(Filters1.Count) + '  ' + Filters1[i] ,true);

       j := 0;
       gr[j] := OneGraph(Filters1.Strings[i],TheDEMs,'All test DEMs');
       inc(j);
       gr[j] := OneGraph(Filters1.Strings[i],DEMs2,'Unrestricted test DEMs');
       inc(j);
       fName := MergeGraphPanelsHorizontal(j,gr,false);
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
     Legend := DEMIXtestDEMLegend;
     Fname := NextFileNumber(MDtempDir,'Winner_','.png');
     FinishBigBitMapWithLegend(BigBitmap,Legend,fName);
    TheDEMs.Destroy;
    Criteria.Destroy;
    Filters1.Destroy;
    DEMs2.Destroy;
    Panels.Destroy;

    EndDEMIXProcessing;
    Self.Visible := true;
end;


procedure Teval_scores_graph_form.BitBtn14Click(Sender: TObject);
begin
   Self.Visible := false;
   MainGraphOptions(DB,AssembleDEMList,Nil,2);
   Self.Visible := true;
end;

procedure Teval_scores_graph_form.BitBtn15Click(Sender: TObject);
begin
   Self.Visible := false;
   MainGraphOptions(DB,AssembleDEMList,Nil,1);
   Self.Visible := true;
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
   Self.Visible := false;
   FilterJustOneGraph(DB,MakeCriteriaList,MakeGeomorphFilters,true);
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
      BestBySlopeRough(db,MakeCriteriaList,AssembleDEMList,RadioGroup10.ItemIndex,ComboBox2.Text,ComboBox3.Text,ComboBox4.Text,ComboBox5.Text);
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
begin
   DisplayBitmap(DEMIXTestDEMLegend(true, AssembleDEMList,MDDef.DEMIX_xsize));
end;

procedure Teval_scores_graph_form.BitBtn23Click(Sender: TObject);
begin
   AllGraphsOneImage(MDDef.NumGraphCols);
end;

procedure Teval_scores_graph_form.BitBtn24Click(Sender: TObject);
begin
   Self.Visible := false;
   MainGraphOptions(DB,AssembleDEMList,Nil,3);
   Self.Visible := true;
end;

procedure Teval_scores_graph_form.BitBtn25Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := DEMIXSettingsDir;
   if GetExistingFileName('DEMIX filters','filters_*.txt',fName) then begin
      Memo2.Lines.LoadFromFile(fName);
   end;
end;

procedure Teval_scores_graph_form.BitBtn26Click(Sender: TObject);
begin
   Self.Visible := false;
   MainGraphOptions(DB,AssembleDEMList,Nil,4);
   Self.Visible := true;
end;

procedure Teval_scores_graph_form.BitBtn27Click(Sender: TObject);
begin
   LastDataBase := DEMIX_final_DB_dir;
   db := OpenMultipleDataBases('New DB for DEMIX graphs');
   ChangeDBonForm(db);
end;


procedure Teval_scores_graph_form.BitBtn28Click(Sender: TObject);
begin
   Self.Visible := false;
   BestEvalGraphPerCriterionMultipleFilters(db,MakeGeomorphFilters,MakeCriteriaList,CriteriaFamily);
   Self.Visible := true;
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

procedure Teval_scores_graph_form.BitBtn31Click(Sender: TObject);
var
   DEMs,Criteria : tStringList;
   i,j,k,ref,rg,tg : integer;
   AreaName,Criterion,DEMIX_TILE : shortstring;
   fName : PathStr;
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
         if (Criterion = 'OPENU') then begin
            gr[k].GraphDraw.MinHorizAxis := 50;
            gr[k].GraphDraw.MaxHorizAxis := 120;
            gr[k].GraphDraw.MinVertAxis := 50;
            gr[k].GraphDraw.MaxVertAxis := 120;
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
         gr[k].RedrawDiagram11Click(Nil);
      end;

begin
   Self.Visible := false;
   GetDEMIXpaths(True);
   AreaName := 'state_line';
   DEMIX_TILE := 'N35VW116G';
   DEMs := AssembleDEMlist;
   Criteria := MakeCriteriaList;
   k := -1;
   MDdef.AddFUVtoR2 := true;
   OpenMaps := false;
   fName := DEMIX_ref_1sec + AreaName + '_dtm' + Ref1SecPointStr + '.tif';
   LoadNewDEM(ref,fName,OpenMaps);
   DEMglb[ref].AreaName := 'Reference DTM ELEV';
   for i := 0 to pred(DEMs.Count) do begin
      fName := DEMIX_test_dems + AreaName + '_' + DEMs.Strings[i] + '.tif';
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
           gr[k] := GridScatterGram(DEMglb[rg].sfBoundBox2tGridLimits(DEMIXtileBoundingBox(DEMIX_TILE)),ref,Tests[j]);
           RedrawGraph(k);
        end
        else begin
            rg := CreateSingleLSPGrid(true,ref,Criterion);
            DEMglb[rg].AreaName := 'Reference DTM ' + Criterion;
            tg := CreateSingleLSPGrid(true,tests[j],Criterion);
            DEMglb[tg].AreaName := DEMs.Strings[j] + ' ' + Criterion;
            inc(k);
            gr[k] := GridScatterGram(DEMglb[rg].sfBoundBox2tGridLimits(DEMIXtileBoundingBox(DEMIX_TILE)),rg,Tg);
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
   DEMs,Criteria,Findings : tStringList;
   refpt,refarea,ref09,test,cop,alos,
   rgpt,rgarea,rg09,tgtest,tgcop,tgalos,
   i,j,k : integer;
   AreaName,Criterion,DEMIX_TILE : shortstring;
   baseDir,
   fName1,fname2,fname3,fname4,fname5,fname6 : PathStr;
   aline : shortstring;
   OpenMaps : boolean;

   function bb(DEMIX_Tile : shortstring; DEM : integer)  : tGridLimits;
   begin
      Result := DEMglb[DEM].sfBoundBox2tGridLimits(DEMIXtileBoundingBox(DEMIX_TILE));
   end;

begin
   Self.Visible := false;
   GetDEMIXpaths(True);
   AreaName := 'state_line';
   DEMIX_TILE := 'N36ZW115G';

   AreaName := 'mt_rushmore';
   DEMIX_TILE := 'N43YW104G';

   AreaName := 'camp_david';
   DEMIX_TILE := 'N39VW078G';

   AreaName := 'kerrville_tx';
   DEMIX_TILE := 'N30MW100H';

   AreaName := 'mormon_mtns';
   DEMIX_TILE := 'N36ZW115G';

   AreaName := 'mt_rushmore';
   DEMIX_TILE := 'N43YW104G';


   BaseDir := 'C:\Users\pguth\Downloads\' + AreaName + '\';

   fName1 := BaseDir + 'cop_matching_ref_dem.tif';
   fName2 := BaseDir + 'alos_matching_ref_dem.tif';
   fName3 := BaseDir + 'gedtm_1_1_matching_ref_dem.tif';
   fName4 := BaseDir + 'gedtm_1_1.tif';
   fName5 := BaseDir + 'cop.tif';
   fName6 := BaseDir + 'alos.tif';

   //DEMs := AssembleDEMlist;
   //Criteria := MakeCriteriaList;

   Criteria := tStringList.Create;
   Criteria.LoadFromFile('C:\microdem\demix_settings\criteria_4_range_fuv.txt');
   k := -1;
   OpenMaps := false;

   wmDEM.SetPanelText(2,'Loads DEMs',true);

   LoadNewDEM(refpt,fName1,OpenMaps);
   LoadNewDEM(refarea,fName2,OpenMaps);
   LoadNewDEM(ref09,fName3,OpenMaps);
   LoadNewDEM(test,fName4,OpenMaps);
   LoadNewDEM(cop,fName5,OpenMaps);
   LoadNewDEM(alos,fName6,OpenMaps);

   findings := tStringList.Create;
   Findings.Add('AREA,DEMIX_TILE,CRITERION,FUV_1,FUV_2,FUV_3,FUV_4,FUV_5,COP,ALOS');
   //for j := 0 to pred(DEMs.Count) do begin
     for i := 0 to pred(Criteria.Count) do begin
        Criterion := NoSuffixCriterion(Criteria.Strings[i]);
        wmDEM.SetPanelText(2,Criterion ,true);

        if (Criterion = 'ELEV') then begin
           //RealToString(GetFUVForPairGrids(RefGridLimits,UsingRef,usingPointGrids[i]),-12,8);
           aLine := AreaName + ',' + DEMIX_TILE + ',' + Criterion + ',' +
                       RealToString(GetFUVForPairGrids(bb(DEMIX_Tile,refpt),refpt,test),-12,8) + ',' +
                       RealToString(GetFUVForPairGrids(bb(DEMIX_Tile,refarea),refarea,test),-12,8) + ',' +
                       RealToString(GetFUVForPairGrids(bb(DEMIX_Tile,test),test,refpt),-12,8) + ',' +
                       RealToString(GetFUVForPairGrids(bb(DEMIX_Tile,test),test,refarea),-12,8) + ',' +
                       RealToString(GetFUVForPairGrids(bb(DEMIX_Tile,ref09),test,ref09),-12,8) + ',' +
                       RealToString(GetFUVForPairGrids(bb(DEMIX_Tile,cop),cop,refpt),-12,8) + ',' +
                       RealToString(GetFUVForPairGrids(bb(DEMIX_Tile,alos),alos,refarea),-12,8);
           Findings.Add(aLine);
        end
        else begin
            rgpt := CreateSingleLSPGrid(OpenMaps,refpt,Criterion);
            rgarea := CreateSingleLSPGrid(OpenMaps,refarea,Criterion);
            rg09 := CreateSingleLSPGrid(OpenMaps,ref09,Criterion);
            tgtest := CreateSingleLSPGrid(OpenMaps,test,Criterion);
            tgcop := CreateSingleLSPGrid(OpenMaps,cop,Criterion);
            tgalos := CreateSingleLSPGrid(OpenMaps,alos,Criterion);
            aLine :=  AreaName + ',' + DEMIX_TILE + ',' + Criterion + ',' +
                       RealToString(GetFUVForPairGrids(bb(DEMIX_Tile,rgpt),rgpt,tgtest),-12,8) + ',' +
                       RealToString(GetFUVForPairGrids(bb(DEMIX_Tile,rgarea),rgarea,tgtest),-12,8) + ',' +
                       RealToString(GetFUVForPairGrids(bb(DEMIX_Tile,tgtest),tgtest,rgpt),-12,8) + ',' +
                       RealToString(GetFUVForPairGrids(bb(DEMIX_Tile,tgtest),tgtest,rgarea),-12,8) + ',' +
                       RealToString(GetFUVForPairGrids(bb(DEMIX_Tile,rg09),tgtest,rg09),-12,8) + ',' +
                       RealToString(GetFUVForPairGrids(bb(DEMIX_Tile,tgcop),tgcop,rgpt),-12,8) + ',' +
                       RealToString(GetFUVForPairGrids(bb(DEMIX_Tile,tgalos),tgalos,rgarea),-12,8);
           Findings.Add(aLine);
        end;
      end;

   PetDBUtils.StringList2CSVtoDB(Findings,MDtempDir + 'GEDTM.dbf');
   EndDEMIXProcessing;
   Self.Visible := true;
   if (not OpenMaps) then CloseAllDEMs;
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
//var
   //i,j,k,ColBigBitmap : integer;
   //HL : shortstring;
   //aName : PathStr;
   //Legend,BigBitmap : tMyBitmap;
begin
   {$IfDef RecordDEMIX} HighlightLineToDebugFile('Teval_scores_graph_form.BitBtn8Click in'); {$EndIf}
    BitBtn4Click(Sender);   //If needed loads db_U10,db_U80,db_U120,db_Full))
    Self.Visible := false;
    if ValidDB(db_Full) then BestEvalGraphPerCriterionMultipleFilters(db_Full,MakeGeomorphFilters,MakeCriteriaList,'Full');
    if MDdef.DEMIX_AllowCoastal then begin
      if ValidDB(db_U120) then BestEvalGraphPerCriterionMultipleFilters(db_U120,MakeGeomorphFilters,MakeCriteriaList,'U120');
      if ValidDB(db_U80) then BestEvalGraphPerCriterionMultipleFilters(db_U80,MakeGeomorphFilters,MakeCriteriaList,'U80');
      if ValidDB(db_U10) then BestEvalGraphPerCriterionMultipleFilters(db_U10,MakeGeomorphFilters,MakeCriteriaList,'U10');
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

procedure Teval_scores_graph_form.CheckBox5Click(Sender: TObject);
begin
   MDDef.FUVExpandScales := CheckBox5.Checked;
end;

procedure Teval_scores_graph_form.CheckBox6Click(Sender: TObject);
begin
   MDdef.DEMIX_UseMedian := CheckBox6.Checked;
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
   CheckBox5.Checked := MDDef.FUVExpandScales;
   CheckBox6.Checked := MDdef.DEMIX_UseMedian;
   Edit1.Text := IntToStr(MDDef.DEMIXlegendFontSize);
   Edit2.Text := IntToStr(MDDef.DEMIX_ysize);
   Edit3.Text := IntToStr(MDDef.DEMIX_xsize);
   Edit4.Text := IntToStr(MDDef.DEMIXUseBins);
   Edit5.Text := IntToStr(MDDef.NumGraphCols);
   Edit6.Text := IntToStr(MDDef.DefaultGraphFont.Size);

   RadioGroup5.ItemIndex := MDDef.DemixSymSize - 1;

   RadioGroup3.ItemIndex := MDDef.SummarySymbol;
   RadioGroup7.ItemIndex := MDDef.DEMIX_groupWonLost;

   db_U10 := 0;
   db_u80 := 0;
   db_u120 := 0;
   db_Full := 0;
   Width := 1064;
end;

procedure Teval_scores_graph_form.RadioGroup1Click(Sender: TObject);
begin
  {$IfDef RecordDEMIX} HighlightLineToDebugFile('Enter Teval_scores_graph_form.RadioGroup1Click, choice=' + IntToStr(RadioGroup1.ItemIndex)); {$EndIf}
  SetColorForProcessing;
  Self.Visible := false;
  DEMIX_evaluations_graph(DB,RadioGroup1.ItemIndex,AssembleDEMList,MakeCriteriaList,true);
  RadioGroup1.ItemIndex := -1;
  SetColorForWaiting;
  Self.Visible := true;
  {$IfDef RecordDEMIX} HighlightLineToDebugFile('Exit Teval_scores_graph_form.RadioGroup1Click, choice=' + IntToStr(RadioGroup1.ItemIndex)); {$EndIf}
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
   DoingRanks : boolean;
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
         MakeLandParamFilters(LandParam,GeomorphFilters,Labels,Memo2);
         if DoingRanks then Suff := '_SCR' else Suff := '';
         UseFilter := MakeCriteriaFilter(CriteriaInFilter);
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
                if (CriteriaInFilter.Count = 1) then  HL := 'Criterion ' + TStr + ': ' + CriteriaInFilter.Strings[0]
                else begin
                  HL := 'Criteria ' + TStr + ': '  + CriteriaInFilter.Strings[0];
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
         y,i,MaxLeft,MaxWide{,MaxHigh} : integer;
         Title : shortstring;
         gr : tGraphArray;
         fName : PathStr;
      begin
         {$IfDef RecordDEMIXByLandcover} WriteLineToDebugFile('MultipleAverageRanksOrEvaluations'); {$EndIf}
         for I := 1 to 5 do begin
            gr[pred(i)] := AverageWithFilters(DB,TheLandTypes[i],CriteriaInFilter,false);
         end;
         MaxLeft := 0;
         MaxWide := 0;
         //MaxHigh := 0;
         for i := 0 to 4 do begin
            if (gr[i].GraphDraw.LeftMargin > MaxLeft) then MaxLeft := gr[i].GraphDraw.LeftMargin;
            if (gr[i].Width > MaxWide) then MaxWide := gr[i].Width;
            //if (gr[i].Height > MaxHigh) then MaxHigh := gr[i].Height;
         end;
         for i := 0 to 4 do begin
            gr[i].GraphDraw.LeftMargin := MaxLeft;
            gr[i].Width := MaxWide;
            //gr[i].Height := MaxHigh;
         end;
         for i := 0 to 4 do begin
            gr[i].RedrawDiagram11Click(Nil);
         end;
         fName := MergeGraphPanelsVertical(4,gr,(Not LeftMarginLabels),nil);  //DEMIXTestDEMLegend(true, AssembleDEMList));
         if Findings = Nil then DisplayBitmap(fName,'DEMIX')
         else Findings.Add(fName);
         {$IfDef RecordDEMIXByLandCover} WriteLineToDebugFile('Teval_scores_graph_form.BitBtn40Click out'); {$EndIf}
      end;


      procedure MultipleByEachCriterion;
      var
         i : integer;
         CriteriaInFilter,Findings : tStringList;
         Legend : tMyBitmap;
      begin
         {$IfDef RecordDEMIX} WriteLineToDebugFile('In MultipleByEachCriterion, Criteria=' + IntToStr(Criteria.Count)); {$EndIf}
         Findings := tStringList.Create;
         for i := 0 to pred(Criteria.Count) do begin
            CriteriaInFilter := tStringList.Create;
            CriteriaInFilter.Add(Criteria.Strings[i]);
            {$IfDef RecordDEMIX} WriteLineToDebugFile('MultipleByEachCriterion: ' + Criteria.Strings[i]); {$EndIf}
            wmDEM.SetPanelText(2,'Criterion ' + IntToStr(succ(i)) + '/' + IntToStr(Criteria.Count) + ' ' + Criteria.Strings[i],true);
            MultipleAverageRanksOrEvaluations((i=0),CriteriaInFilter,Findings);
            CriteriaInFilter.Destroy;
         end;
         Legend := DEMIXTestDEMLegend(true, AssembleDEMList);
         MergeVerticalPanels(Findings,Legend,'DEMIX');
         {$IfDef RecordDEMIX} WriteLineToDebugFile('Out MultipleAverageRanksOrEvaluations, Criteria=' + IntToStr(Criteria.Count)); {$EndIf}
      end;


var
   Choice : integer;
begin {Teval_scores_graph_form.RadioGroup9Click}
    DoingRanks := (Sender = RadioGroup8);
    if (Sender = RadioGroup8) then Choice := RadioGroup8.ItemIndex
    else Choice := RadioGroup9.ItemIndex;

    TheDEMs := AssembleDEMList;
    Criteria := MakeCriteriaList;

    try
        GetDEMIXpaths(True);
        Self.Visible := false;
        {$IfDef RecordDEMIX} HighlightLineToDebugFile('Enter Teval_scores_graph_form.RadioGroup9Click, choice=' + IntToStr(Choice) + ' DEMs=' + IntToStr(TheDEMs.Count)); {$EndIf}
        case Choice of
           0 : AverageWithFilters(DB,'Users',Criteria,true);
           1 : AverageWithFilters(DB,'AVG_SLOPE',Criteria,true);
           2 : AverageWithFilters(DB,'AVG_ROUGH',Criteria,true);
           3 : AverageWithFilters(DB,'FOREST_PC',Criteria,true);
           4 : AverageWithFilters(DB,'BARREN_PC',Criteria,true);
           5 : AverageWithFilters(DB,'URBAN_PC',Criteria,true);
           6 : MultipleAverageRanksOrEvaluations(true,Criteria);
           7 : MultipleByEachCriterion;
        end;
    finally
      RadioGroup8.ItemIndex := -1;
      RadioGroup9.ItemIndex := -1;
      {$IfDef RecordDEMIX} HighlightLineToDebugFile('Exit Teval_scores_graph_form.RadioGroup9Click, choice=' + IntToStr(Choice) + ' DEMs=' + IntToStr(TheDEMs.Count)); {$EndIf}
      TheDEMs.Destroy;
      Criteria.Destroy;
      EndDEMIXProcessing(db);
      Self.Visible := true;
    end;
end {Teval_scores_graph_form.RadioGroup9Click};


initialization
finalization
end.
