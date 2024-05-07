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
  private
    { Private declarations }
    procedure ChangeDBonForm(Newdb : integer);
  public
    { Public declarations }
     db,originalDB : integer;
     Criteria : tStringList;
  end;

var
  eval_scores_graph_form : Teval_scores_graph_form;


procedure StartDEMIXgraphs(DB : integer);

implementation

{$R *.dfm}

uses
   Nevadia_main,
   Petmar,Petmar_types,Petmar_db,Petimage_form,
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
   db := NewDB;
   Caption := 'DEMIX graphs: ' + GISdb[db].dbName;
   GISdb[db].EmpSource.Enabled := false;
   Criteria := GISdb[db].MyData.ListUniqueEntriesInDB('CRITERION');
   RadioGroup4.Enabled := (Criteria.IndexOf('ELEV_SSIM') >= 0);
   BitBtn13.Enabled := GISdb[DB].MyData.FieldExists('BARREN_PC') and GISdb[DB].MyData.FieldExists('AVG_SLOPE');
end;

procedure Teval_scores_graph_form.BitBtn10Click(Sender: TObject);
begin
   WinningPercentagesComparedToCOP(db);
end;

procedure Teval_scores_graph_form.BitBtn11Click(Sender: TObject);
begin
   YaxisWhat := yasBarren;
   DEMIX_evaluations_graph(DB,true);
end;

procedure Teval_scores_graph_form.BitBtn12Click(Sender: TObject);
begin
   WhiskerPlotsByCluster(DB);
end;

procedure Teval_scores_graph_form.BitBtn13Click(Sender: TObject);
begin
    BestBySlopeRough(db);
end;

procedure Teval_scores_graph_form.BitBtn14Click(Sender: TObject);
begin
   YaxisWhat := yasForest;
   DEMIX_evaluations_graph(DB,true);
end;

procedure Teval_scores_graph_form.BitBtn15Click(Sender: TObject);
begin
   YaxisWhat := yasBestEvalByCriterion;
   DEMIX_evaluations_graph(DB,true);
end;

procedure Teval_scores_graph_form.BitBtn16Click(Sender: TObject);
begin
   YaxisWhat := yasBestEvalBySlope;
   DEMIX_evaluations_graph(DB,true);
end;

procedure Teval_scores_graph_form.BitBtn1Click(Sender: TObject);
var
   aFilter : shortstring;
begin
   if StrUtils.AnsiContainsText(UpperCase(GISdb[db].DBName),'_SORT') then begin
      DEMIXVertAxisLabel := 'Sorted by best evaluation in tile ' + GISdb[db].MyData.Filter;
      YAxisSort := yasBestEval;
      YAxisWhat := yasBestEval;
      DEMIX_evaluations_graph(DB,true);
   end
   else begin
      MessageToContinue('Sort database first');
   end;

(*
   if (XAxisWhat = xawEvaluation) and (YaxisWhat = yawTile) then begin
      DEMIX_evaluations_graph(DB,true);
   end
   else if (XAxisWhat = xawScore) and (YaxisWhat = yawTile) then begin
      DEMIX_evaluations_graph(DB,false);
   end;

   if (XAxisWhat = xawScore) and (YaxisWhat = yawTile) {and (YaxisSort = yasName)} then GraphAverageScoresByTile(DB,Nil,Nil);
   if (XAxisWhat = xawScore) and (YaxisWhat = yawArea) {and (YaxisSort = yasName)} then DEMIX_AreaAverageScores_graph(DB);
*)
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
   DEMIX_evaluations_graph(DB,true);
end;

procedure Teval_scores_graph_form.BitBtn8Click(Sender: TObject);
begin
   YaxisWhat := yasRuff;
   DEMIX_evaluations_graph(DB,true);
end;

procedure Teval_scores_graph_form.BitBtn9Click(Sender: TObject);
begin
   YaxisWhat := yasRelief;
   DEMIX_evaluations_graph(DB,true);
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

procedure Teval_scores_graph_form.FormActivate(Sender: TObject);
begin
   RecognizeDEMIXVersion(DB);
end;

procedure Teval_scores_graph_form.FormCreate(Sender: TObject);
begin
   XAxisWhat := RadioGroup1.ItemIndex;
   YAxisWhat := RadioGroup2.ItemIndex;
   //YAxisSort := RadioGroup3.ItemIndex;

   CheckBox1.Checked := DEMIX_combined_graph;
   CheckBox2.Checked := PanelsByTestDEM;
   CheckBox3.Checked := MovieByTestDEM;
   CheckBox4.Checked := MDDef.DEMIX_graph_Retired_DEMs;
   Edit1.Text := IntToStr(MDDef.DEMIXlegendFontSize);
   Edit2.Text := IntToStr(MDDef.DEMIX_ysize);
   Edit3.Text := IntToStr(MDDef.DEMIX_xsize);
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
var
   SortField : shortstring;
begin
   //YAxisSort := RadioGroup3.ItemIndex;
   case YaxisSort of
      yasSlope : SortField := 'AVG_SLOPE';
      yasRuff  : SortField := 'AVG_RUFF';
      yasRelief : SortField := 'RELIEF';
      yasBestEval  : SortField := 'BEST_EVAL';
      else SortField := '';
   end;

   if (SortField <> '') and (not StrUtils.AnsiContainsText(GISdb[db].dbName,'_sorted_')) then begin
      if (not GISdb[db].MyData.FieldExists(SortField)) then begin
         //AddStatisticsToDEMIXdb(db);
         EvalRangeAndBestEvalForCriterion(DB);
         OriginalDB := db;
      end;
      db := SortDataBase(DB,true,SortField,ExtractFilePath(GISdb[db].dbFullName));
      GISdb[db].AddSequentialIndex(RecNoFName,true);
   end;
   BitBtn1Click(Sender);
end;



procedure Teval_scores_graph_form.RadioGroup4Click(Sender: TObject);
begin
   SSIM_FUV_scatterplot(db,RadioGroup4.Items[RadioGroup4.ItemIndex]);
end;

procedure Teval_scores_graph_form.RadioGroup5Click(Sender: TObject);
begin
   DemixSymSize := RadioGroup5.ItemIndex + 3;
end;

initialization
finalization
end.
