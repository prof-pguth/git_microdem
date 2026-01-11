unit demfabricregion;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons,
  Petmar_types,DEMMapf,BaseGraf;

type
  TFabricOptions = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    OKBtn: TButton;
    HelpBtn: TBitBtn;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    Label3: TLabel;
    Label4: TLabel;
    Edit2: TEdit;
    Edit3: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Edit1: TEdit;
    BitBtn3: TBitBtn;
    Edit5: TEdit;
    Label2: TLabel;
    BitBtn4: TBitBtn;
    Label5: TLabel;
    Edit4: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
  private
    { Private declarations }
  public
     { Public declarations }
     GrainFlatGraph,GrainOrgGraph,GrainDirGraph,GrainReliefGraph : tThisBaseGraph;
     xDEMg1,yDEMg1 : float64;
     BaseMap : tMapForm;
  end;

var
   FabricOptions : TFabricOptions;

implementation

{$R *.DFM}


Uses
   PETMAR,DEMDefs,Make_grid, nevadia_main;


procedure TFabricOptions.BitBtn1Click(Sender: TObject);
begin
   BaseMap.FigureGrainByRegionSize(round(xDEMg1),round(yDEMg1));
end;

procedure TFabricOptions.BitBtn2Click(Sender: TObject);
begin
   ChangeDEMNowDoing(GrainByRegionSize);
end;

procedure TFabricOptions.BitBtn3Click(Sender: TObject);
begin
   BaseMap.FindMostOrganizedRegion;
end;

procedure TFabricOptions.BitBtn4Click(Sender: TObject);
begin
   MakeMomentsGrid(BaseMap.MapDraw.DEMonMap,'Q');
end;

procedure TFabricOptions.CheckBox1Click(Sender: TObject);
begin
    MDDef.GrainNets := CheckBox1.Checked;
end;

procedure TFabricOptions.CheckBox2Click(Sender: TObject);
begin
    MDDef.GrainFlatGraph := CheckBox2.Checked;
end;

procedure TFabricOptions.CheckBox3Click(Sender: TObject);
begin
    MDDef.GrainOrgGraph := CheckBox3.Checked;
end;

procedure TFabricOptions.CheckBox4Click(Sender: TObject);
begin
    MDDef.GrainDir := CheckBox4.Checked;
end;

procedure TFabricOptions.CheckBox5Click(Sender: TObject);
begin
    MDDef.GrainAspects := CheckBox5.Checked;
end;

procedure TFabricOptions.CheckBox6Click(Sender: TObject);
begin
   MDDef.GrainReliefGraph := CheckBox6.Checked;
end;


procedure TFabricOptions.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDDef.MinPointsForSSO);
end;

procedure TFabricOptions.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDDef.FirstBoxSize);
end;

procedure TFabricOptions.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,MDDef.LastBoxSize);
end;

procedure TFabricOptions.Edit4Change(Sender: TObject);
begin
   CheckEditString(Edit4.Text,MDDef.FabricCalcThin);
end;

procedure TFabricOptions.Edit5Change(Sender: TObject);
begin
    CheckEditString(Edit5.Text,MDDef.PointSeparation);
end;

procedure TFabricOptions.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
   if (FabricOptions.GrainFlatGraph <> Nil) then FabricOptions.GrainFlatGraph.CanCloseGraph := true;
   if (FabricOptions.GrainOrgGraph <> Nil) then FabricOptions.GrainOrgGraph.CanCloseGraph := true;
   if (FabricOptions.GrainDirGraph <> Nil) then FabricOptions.GrainDirGraph.CanCloseGraph := true;
   if (FabricOptions.GrainReliefGraph <> Nil) then FabricOptions.GrainReliefGraph.CanCloseGraph := true;
end;


procedure TFabricOptions.FormCreate(Sender: TObject);
begin
    GrainFlatGraph := Nil;
    GrainOrgGraph  := Nil;
    GrainDirGraph  := Nil;
    GrainReliefGraph := Nil;
    CheckBox1.Checked := MDDef.GrainNets;
    CheckBox2.Checked := MDDef.GrainFlatGraph;
    CheckBox3.Checked := MDDef.GrainOrgGraph;
    CheckBox4.Checked := MDDef.GrainDir;
    CheckBox5.Checked := MDDef.GrainAspects;
    CheckBox6.Checked := MDDef.GrainReliefGraph;
    Edit1.Text := IntToStr(MDDef.MinPointsForSSO);
    Edit2.Text := IntToStr(MDDef.FirstBoxSize);
    Edit3.Text := IntToStr(MDDef.LastBoxSize);
    Edit4.Text := IntToStr(MDDef.FabricCalcThin);
    Edit5.Text := IntTOStr(MDDef.PointSeparation);
    wmDEM.FormPlacementInCorner(self);
end;


procedure TFabricOptions.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\FabricByRegion.htm');
end;

procedure TFabricOptions.OKBtnClick(Sender: TObject);
begin
   Close;
   FabricOptions := Nil;
   BaseMap.BackToWandering;
end;

initialization
   FabricOptions := Nil;
finalization
end.


















