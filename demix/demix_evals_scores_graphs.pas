unit demix_evals_scores_graphs;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,StrUtils,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  Teval_scores_graph_form = class(TForm)
    RadioGroup1: TRadioGroup;
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
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    RadioGroup4: TRadioGroup;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtn10: TBitBtn;
    RadioGroup5: TRadioGroup;
    BitBtn11: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn13: TBitBtn;
    CheckBox4: TCheckBox;
    BitBtn14: TBitBtn;
    BitBtn15: TBitBtn;
    BitBtn16: TBitBtn;
    BitBtn2: TBitBtn;
    Memo1: TMemo;
    BitBtn18: TBitBtn;
    BitBtn19: TBitBtn;
    BitBtn20: TBitBtn;
    BitBtn21: TBitBtn;
    BitBtn17: TBitBtn;
    BitBtn22: TBitBtn;
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
    procedure BitBtn1Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure RadioGroup4Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure RadioGroup5Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure BitBtn19Click(Sender: TObject);
    procedure BitBtn21Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure BitBtn22Click(Sender: TObject);
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
  private
    { Private declarations }
    procedure ChangeDBonForm(Newdb : integer);
  public
    { Public declarations }
     db,originalDB : integer;
     XaxisParam,YaxisParam,ValueParam : shortstring;
     Criteria : tStringList;
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


procedure StartDEMIXgraphs(DB : integer);
var
  eval_scores_graph_form : Teval_scores_graph_form;
  i : integer;
begin
   GetDEMIXpaths(false,db);
   GISdb[db].dbtablef.OpenDEMIXgraphs1.Enabled := false;
   eval_scores_graph_form := Teval_scores_graph_form.Create(Application);
   eval_scores_graph_form.ChangeDBonForm(db);
   eval_scores_graph_form.Show;
end;


procedure Teval_scores_graph_form.ChangeDBonForm(Newdb : integer);
begin
   if ValidDB(NewDB) then begin
      db := NewDB;
      if GISdb[db].MyData.FieldExists('DELTA') then DEMIXModeName := 'U10'
      else if GISdb[db].MyData.FieldExists('DILUV') then DEMIXModeName := 'U80'
      else if GISdb[db].MyData.FieldExists('COAST') then DEMIXModeName := 'U120'
      else DEMIXModeName := 'ALL';

      Caption := 'DEMIX graphs: ' + GISdb[db].dbName;
      GISdb[db].EmpSource.Enabled := false;
      if (Criteria = Nil) then begin
         Criteria := GISdb[db].MyData.ListUniqueEntriesInDB('CRITERION');
         Memo1.Lines := Criteria;
      end;
      RadioGroup4.Enabled := (Criteria.IndexOf('ELEV_SSIM') >= 0);
      BitBtn13.Enabled := GISdb[DB].MyData.FieldExists('BARREN_PC') and GISdb[DB].MyData.FieldExists('AVG_SLOPE');
      BitBtn24.Enabled := GISdb[DB].MyData.FieldExists('BARREN_PC') and GISdb[DB].MyData.FieldExists('AVG_SLOPE');
      BitBtn25.Enabled := GISdb[DB].MyData.FieldExists('BARREN_PC') and GISdb[DB].MyData.FieldExists('AVG_SLOPE');
      BitBtn26.Enabled := GISdb[DB].MyData.FieldExists('BARREN_PC') and GISdb[DB].MyData.FieldExists('AVG_SLOPE');
   end;
end;

procedure Teval_scores_graph_form.BitBtn10Click(Sender: TObject);
begin
   WinningPercentagesComparedToCOP(db,Criteria);
end;

procedure Teval_scores_graph_form.BitBtn11Click(Sender: TObject);
begin
   YaxisWhat := yasBarren;
   DEMIX_evaluations_graph(DB,Criteria,true);
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
   YaxisWhat := yasForest;
   DEMIX_evaluations_graph(DB,Criteria,true);
end;

procedure Teval_scores_graph_form.BitBtn15Click(Sender: TObject);
begin
   YaxisWhat := yasBestEvalByCriterion;
   DEMIX_evaluations_graph(DB,Criteria,true);
end;

procedure Teval_scores_graph_form.BitBtn16Click(Sender: TObject);
begin
   YaxisWhat := yasBestEvalColoredBySlope;
   DEMIX_evaluations_graph(DB,Criteria,true);
end;

procedure Teval_scores_graph_form.BitBtn17Click(Sender: TObject);
begin
   YaxisWhat := yasBestEvalFilteredBySlope;
   DEMIX_evaluations_graph(DB,Criteria,true);
end;

procedure Teval_scores_graph_form.BitBtn18Click(Sender: TObject);
var
   pn : integer;
begin
    if Criteria <> nil then Criteria.Destroy;
    GISdb[DB].EmpSource.Enabled := false;
    Criteria := GISdb[DB].MyData.ListUniqueEntriesInDB('CRITERION');
    if (Sender = BitBtn21) or (Criteria.Count = 1) or MultiSelectSingleColumnStringList('Criteria to graph',pn,Criteria,true,true) then begin
       Memo1.Lines.Clear;
       Memo1.Lines := Criteria;
    end;
end;

procedure Teval_scores_graph_form.BitBtn19Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := DEMIXSettingsDir;
   if GetExistingFileName('DEMIX tiles','tile*.txt',fName) then begin
      Criteria.Clear;
      Criteria.LoadFromFile(fName);
      Memo1.Lines := Criteria;
   end;
end;

procedure Teval_scores_graph_form.BitBtn1Click(Sender: TObject);
begin
   if true or StrUtils.AnsiContainsText(UpperCase(GISdb[db].DBName),'_SORT') then begin
      DEMIXVertAxisLabel := 'Sorted by best evaluation in tile ' {+ GISdb[db].MyData.Filter};
      YAxisSort := yasBestEval;
      YAxisWhat := yasBestEval;
      DEMIX_evaluations_graph(DB,Criteria,true);
   end
   else begin
      MessageToContinue('Sort database first');
   end;
end;

procedure Teval_scores_graph_form.BitBtn20Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := DEMIXSettingsDir;
   if GetFileNameDefaultExt('list of criteria','*.txt',fName) then Criteria.SaveToFile(fName);
end;

procedure Teval_scores_graph_form.BitBtn21Click(Sender: TObject);
begin
   BitBtn18Click(Sender);
end;

procedure Teval_scores_graph_form.BitBtn22Click(Sender: TObject);
begin
   WinningPercentagesComparedToCOPFilteredBySlope(db,Criteria);
end;

procedure Teval_scores_graph_form.BitBtn23Click(Sender: TObject);
begin
   AllGraphsOneImage(MDDef.NumGraphCols);
end;

procedure Teval_scores_graph_form.BitBtn24Click(Sender: TObject);
begin
   ValueParam := 'BEST_EVAL';
   BestBySlopeRough(db,Criteria,false,XaxisParam,YaxisParam,ValueParam );
end;

procedure Teval_scores_graph_form.BitBtn25Click(Sender: TObject);
begin
   ValueParam := 'COP';
   BestBySlopeRough(db,Criteria,false,XaxisParam,YaxisParam,ValueParam );
end;

procedure Teval_scores_graph_form.BitBtn26Click(Sender: TObject);
begin
   ValueParam := 'ALOS';
   BestBySlopeRough(db,Criteria,false,XaxisParam,YaxisParam,ValueParam );
end;

procedure Teval_scores_graph_form.BitBtn27Click(Sender: TObject);
var
   newDB : integer;
begin
   LastDataBase := DEMIX_final_DB_dir;
   db := OpenMultipleDataBases('New DB for DEMIX graphs');
   ChangeDBonForm(Newdb);
end;

procedure Teval_scores_graph_form.BitBtn28Click(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(Memo2.Lines.Count) do begin
      if Memo2.Lines[i] = '(None)' then GISdb[db].ClearGISFilter
      else GISdb[db].ApplyGISFilter(Memo2.Lines[i]);
      YaxisWhat := yasBestEvalByCriterion;
      DEMIX_evaluations_graph(DB,Criteria,true);
   end;
   GISdb[db].ClearGISFilter;
end;

procedure Teval_scores_graph_form.BitBtn2Click(Sender: TObject);
begin
   CombineAllPanelGraphs;
end;

procedure Teval_scores_graph_form.BitBtn3Click(Sender: TObject);
begin
   wmdem.Closeallgraphs1Click(Sender);
   wmdem.Closeallpictureviewwindows1Click(Sender);
end;

procedure Teval_scores_graph_form.BitBtn4Click(Sender: TObject);
begin
   if (YaxisWhat = yawArea) then DEMIX_AreaAverageScores_graph(DB);
   if (YaxisWhat = yawTile) then GraphAverageScoresByTile(DB,Nil,Nil);
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
   YaxisWhat := yasSlope;
   DEMIX_evaluations_graph(DB,Criteria,true);
end;

procedure Teval_scores_graph_form.BitBtn8Click(Sender: TObject);
begin
   YaxisWhat := yasRuff;
   DEMIX_evaluations_graph(DB,Criteria,true);
end;

procedure Teval_scores_graph_form.BitBtn9Click(Sender: TObject);
begin
   YaxisWhat := yasRelief;
   DEMIX_evaluations_graph(DB,Criteria,true);
end;

procedure Teval_scores_graph_form.CheckBox1Click(Sender: TObject);
begin
   DEMIX_combined_graph := CheckBox1.Checked;
end;

procedure Teval_scores_graph_form.CheckBox2Click(Sender: TObject);
begin
   PanelsByTestDEM := CheckBox2.Checked;
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
   for I := 1 to NumDEMIXtestDEM do UseRetiredDEMs[i] := MDDef.DEMIX_graph_Retired_DEMs or ((DEMIXshort[i] <> 'ASTER') and (DEMIXshort[i] <> 'NASA') and (DEMIXshort[i] <> 'SRTM'));
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

procedure Teval_scores_graph_form.FormActivate(Sender: TObject);
begin
   RecognizeDEMIXVersion(DB);
end;

procedure Teval_scores_graph_form.FormCreate(Sender: TObject);
begin
   XAxisWhat := RadioGroup1.ItemIndex;
   YAxisWhat := RadioGroup2.ItemIndex;
   CheckBox1.Checked := DEMIX_combined_graph;
   CheckBox2.Checked := PanelsByTestDEM;
   CheckBox3.Checked := MovieByTestDEM;
   CheckBox4.Checked := MDDef.DEMIX_graph_Retired_DEMs;
   Edit1.Text := IntToStr(MDDef.DEMIXlegendFontSize);
   Edit2.Text := IntToStr(MDDef.DEMIX_ysize);
   Edit3.Text := IntToStr(MDDef.DEMIX_xsize);
   Edit4.Text := IntToStr(MDDef.DEMIXUseBins);
   Edit5.Text := IntToStr(MDDef.NumGraphCols);
   RadioGroup3.ItemIndex := MDDef.SummarySymbol;
   RadioGroup6.ItemIndex := MDDef.TwoParameterVisualization;
   RadioGroup7.ItemIndex := MDDef.DEMIX_groupWonLost;

   XaxisParam := 'BARREN_PC';
   YaxisParam := 'AVG_SLOPE';
   ValueParam := 'BEST_EVAL';
   Criteria := Nil;
end;

procedure Teval_scores_graph_form.RadioGroup1Click(Sender: TObject);
begin
   XAxisWhat := RadioGroup1.ItemIndex;
end;

procedure Teval_scores_graph_form.RadioGroup2Click(Sender: TObject);
begin
   YAxisWhat := RadioGroup2.ItemIndex;
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
   DemixSymSize := RadioGroup5.ItemIndex + 3;
end;

procedure Teval_scores_graph_form.RadioGroup6Click(Sender: TObject);
begin
   MDDef.TwoParameterVisualization := RadioGroup6.ItemIndex;
end;

procedure Teval_scores_graph_form.RadioGroup7Click(Sender: TObject);
begin
   MDDef.DEMIX_groupWonLost := RadioGroup7.ItemIndex;
end;

initialization
finalization
end.
