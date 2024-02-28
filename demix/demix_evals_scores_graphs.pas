unit demix_evals_scores_graphs;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}



interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  Teval_scores_graph_form = class(TForm)
    RadioGroup1: TRadioGroup;
    BitBtn1: TBitBtn;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    CheckBox1: TCheckBox;
    BitBtn2: TBitBtn;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    BitBtn3: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     db : integer;
  end;

var
  eval_scores_graph_form : Teval_scores_graph_form;


procedure StartDEMIXgraphs(DB : integer);

implementation

{$R *.dfm}

uses
   Nevadia_main,
   DEMIX_graphs,
   DEMIX_control,
   DEMIX_definitions;



procedure StartDEMIXgraphs(DB : integer);
var
  eval_scores_graph_form : Teval_scores_graph_form;
begin
   GetDEMIXpaths(false);
   eval_scores_graph_form := Teval_scores_graph_form.Create(Application);
   eval_scores_graph_form.db := db;
   eval_scores_graph_form.Show;

end;

(*
const
   yasName = 0;
   yasSlope = 1;
   yasRuff = 2;
   yasRelief = 3;
   xawEvaluation = 0;
   xawScore = 1;
   yawArea = 0;
   yawTile = 1;
   DEMIX_combined_graph : boolean = true;
var
   XAxisWhat,YAxisWhat,YAxisSort : integer;
*)

procedure Teval_scores_graph_form.BitBtn1Click(Sender: TObject);
begin
   if (XAxisWhat = xawEvaluation) and (YaxisWhat = yawTile) then begin
      DEMIX_evaluations_graph(DB);
   end;
   if (XAxisWhat = xawScore) and (YaxisWhat = yawTile) and (YaxisSort = yasName) then GraphAverageScoresByTile(DB,Nil,Nil);
   if (XAxisWhat = xawScore) and (YaxisWhat = yawArea) and (YaxisSort = yasName) then DEMIX_AreaAverageScores_graph(DB);
end;



procedure Teval_scores_graph_form.BitBtn2Click(Sender: TObject);
begin
   AddStatisticsToDEMIXdb(db);
end;

procedure Teval_scores_graph_form.BitBtn3Click(Sender: TObject);
begin
   wmdem.Closeallgraphs1Click(Sender);
   wmdem.Closeallpictureviewwindows1Click(Sender);
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

procedure Teval_scores_graph_form.FormCreate(Sender: TObject);
begin
   XAxisWhat := RadioGroup1.ItemIndex;
   YAxisWhat := RadioGroup2.ItemIndex;
   YAxisSort := RadioGroup3.ItemIndex;

   CheckBox1.Checked := DEMIX_combined_graph;
   CheckBox2.Checked := PanelsByTestDEM;
   CheckBox3.Checked := MovieByTestDEM;

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
   YAxisSort := RadioGroup3.ItemIndex;
end;

end.
