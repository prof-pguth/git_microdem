unit demix_filter;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$Define RecordFullDEMIX}
{$EndIf}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons;

type
  TDemixFilterForm = class(TForm)
    BitBtn1: TBitBtn;
    GroupBox1: TGroupBox;
    Memo1: TMemo;
    GroupBox2: TGroupBox;
    Memo2: TMemo;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Memo5: TMemo;
    Memo4: TMemo;
    Memo3: TMemo;
    Load: TBitBtn;
    GroupBox6: TGroupBox;
    Memo6: TMemo;
    BitBtn2: TBitBtn;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure LoadClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    DB : integer;
    DEMsTypeUsing,
    TilesUsing,
    LandTypesUsing,
    CriteriaUsing,
    CandidateDEMsUsing : tStringList;
    procedure GetUsingStringLists;
    procedure DoCriteriaGraph;

  end;

//var
  //DemixFilterForm: TDemixFilterForm;

procedure DoDEMIXFilter(DB : integer);


implementation

{$R *.dfm}

uses
   Petmar,Petmar_types,Petmar_db,PetMath,PetImage,PetImage_form,
   DEMDatabase,DEMDbTable,DEMdef_routines,DEMDefs,
   BaseGraf,
   DEMIX_control;

procedure DoDEMIXFilter(DB : integer);
var
  DemixFilterForm: TDemixFilterForm;
  i: Integer;
begin
   DemixFilterForm := TDemixFilterForm.Create(Application);
   DemixFilterForm.db := db;
   for i := pred(DemixFilterForm.Memo4.Lines.Count) downto 0 do begin
      if not GISdb[db].MyData.FieldExists(DemixFilterForm.Memo4.Lines[i]) then DemixFilterForm.Memo4.Lines.Delete(i);
   end;
   DemixFilterForm.Memo6.Lines.LoadFromFile(DEMIXSettingsDir + 'demix_areas_sorted_by_lat.txt');
   DemixFilterForm.DEMsTypeUsing := tStringList.Create;
   DemixFilterForm.TilesUsing := tStringList.Create;

   DemixFilterForm.LandTypesUsing := tStringList.Create;
   DemixFilterForm.CriteriaUsing := tStringList.Create;
   DemixFilterForm.CandidateDEMsUsing := tStringList.Create;

   DemixFilterForm.BitBtn1.Enabled := GISdb[db].MyData.FieldExists('DEM');

   DemixFilterForm.Show;
end;



procedure TDemixFilterForm.BitBtn2Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := DEMIXSettingsDir;
   if GetExistingFileName('DEMIX criteria','*.txt',fName) then begin
      Memo4.Lines.LoadFromFile(fName);
   end;
end;

procedure TDemixFilterForm.DoCriteriaGraph;
var
   aFilter,DEMstr : shortstring;
   i,j,k,m,DEM : integer;
   Graph : tThisBaseGraph;
   rfile : array[0..10] of file;
   Symbol : tFullSymbolDeclaration;
   v : array[1..2] of float32;
   AllGraphs : tStringList;
   fName : PathStr;
begin
   {$IfDef RecordFullDEMIX} WriteLineToDebugFile('Start TDemixFilterForm.DoCriteriaGraph'); {$EndIf}
   GISdb[DB].EmpSource.Enabled := false;
   AllGraphs := tStringList.Create;
   for i := 0 to pred(DEMsTypeUsing.Count) do begin
      for j := 0 to pred(TilesUsing.Count) do begin
         for k :=  0 to pred(LandTypesUsing.Count) do begin
            aFilter := 'REF_TYPE=' + QuotedStr(DEMsTypeUsing[i]) + ' AND DEMIX_TILE=' + QuotedStr(TilesUsing[j])+ ' AND LAND_TYPE=' + QuotedStr(LandTypesUsing[k]);
            GISdb[db].ApplyGISFilter(aFilter);
            if GISdb[db].MyData.FiltRecsInDB > 0 then begin
               {$IfDef RecordFullDEMIX} WriteLineToDebugFile(aFilter); {$EndIf}
               Graph := tThisBaseGraph.Create(Application);

               Graph.GraphDraw.LegendList := tStringList.Create;
               for DEM := 0 to pred(CandidateDEMsUsing.Count) do begin
                  Symbol := SymbolFromDEMName(CandidateDEMsUsing[DEM]);
                  Symbol.DrawingSymbol := FilledBox;
                  Graph.OpenPointFile(rfile[DEM],Symbol);
                  Graph.GraphDraw.LegendList.Add(CandidateDEMsUsing[DEM]);
                  Graph.GraphDraw.LineSize256[DEM] := 2;
               end;

               Graph.Caption := aFilter;
               Graph.GraphDraw.GraphAxes := YFullGridOnly;
               Graph.GraphDraw.MinVertAxis := 999;
               Graph.GraphDraw.MaxVertAxis := -999;
               Graph.GraphDraw.HorizLabel := aFilter;
               Graph.GraphDraw.GraphLeftLabels := tStringList.Create;
               Graph.GraphDraw.GraphBottomLabels := tStringList.Create;
               Graph.GraphDraw.GraphBottomLabels := tStringList.Create;

               GISdb[db].MyData.first;
               while not GISdb[db].MyData.eof do begin
                  DEMstr := GISdb[db].MyData.GetFieldByNameAsString('DEM');
                  DEM := CandidateDEMsUsing.IndexOf(DEMstr);
                  if DEM <> -1 then begin
                     for m := 0 to pred(CriteriaUsing.Count) do begin
                        Graph.GraphDraw.GraphBottomLabels.Add(IntToStr(m) + ',' + CriteriaUsing[m]);
                        v[1] := m;
                        v[2] := GISdb[db].MyData.GetFieldByNameAsFloat(CriteriaUsing[m]);
                        CompareValueToExtremes(v[2],Graph.GraphDraw.MinVertAxis,Graph.GraphDraw.MaxVertAxis);
                        BlockWrite(rfile[DEM],v,1);
                     end;
                  end;
                  GISdb[db].MyData.Next;
               end;
               Graph.GraphDraw.MinHorizAxis := -0.5;
               Graph.GraphDraw.MaxHorizAxis := CriteriaUsing.Count - 0.5;
               Graph.GraphDraw.MinVertAxis := Graph.GraphDraw.MinVertAxis - 1;
               Graph.GraphDraw.MaxVertAxis := Graph.GraphDraw.MaxVertAxis + 1;
               Graph.GraphDraw.SetShowAllLines(true,2);
               Graph.GraphDraw.VertGraphBottomLabels := false;
               Graph.GraphDraw.ShowVertAxis0 := true;
               Graph.AutoScaleAndRedrawDiagram(false,false,false,false);
               Graph.Height := MDDef.DEMIX_ysize;
               Graph.Width := MDDef.DEMIX_xsize;
               Graph.RedrawDiagram11Click(Nil);

               Graph.Image1.Canvas.Draw(Graph.GraphDraw.LeftMargin+15,Graph.GraphDraw.TopMargin+10,Graph.MakeLegend(Graph.GraphDraw.LegendList,false));
               fName := NextFileNumber(MDTempDir,'big_graph_','.png');
               SaveImageAsBMP(Graph.Image1,fName);
               AllGraphs.Add(fName);
            end;
         end;
      end;
   end;
   fName := NextFileNumber(MDtempDir,'criteria_by_tile_','.png');
   MakeBigBitmap(AllGraphs,'',fName,4);
   DisplayBitmap(fName,'');

   GISdb[DB].ClearGISFilter;
   GISdb[DB].EmpSource.Enabled := true;
   {$IfDef RecordFullDEMIX} WriteLineToDebugFile('End TDemixFilterForm.DoCriteriaGraph'); {$EndIf}
end;


procedure TDemixFilterForm.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDDef.DEMIX_xsize);
   CheckEditString(Edit2.Text,MDDef.DEMIX_ysize);
end;

procedure TDemixFilterForm.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDDef.DEMIX_ysize);
end;

procedure TDemixFilterForm.FormCreate(Sender: TObject);
begin
   Edit1.Text := IntToStr(MDDef.DEMIX_xsize);
   Edit2.Text := IntToStr(MDDef.DEMIX_ysize);
end;


procedure TDemixFilterForm.BitBtn1Click(Sender: TObject);
begin
   GetUsingStringLists;
   DoCriteriaGraph;
end;

procedure TDemixFilterForm.GetUsingStringLists;

   procedure DoOne(Memo : tMemo; var SL : tStringList);
   var
      i : integer;
   begin
      sl.Clear;
      for i := 0 to pred(Memo.Lines.Count) do begin
         if (Memo.Lines[i] <> '') then sl.Add(Memo.Lines[i]);
      end;
   end;


begin
   DoOne(Memo1,DEMsTypeUsing);
   DoOne(Memo2,TilesUsing);
   DoOne(Memo3,LandTypesUsing);
   DoOne(Memo4,CriteriaUsing);
   DoOne(Memo5,CandidateDEMsUsing);
end;



procedure TDemixFilterForm.LoadClick(Sender: TObject);
var
   fName : PathStr;
begin
   fName := DEMIXSettingsDir;
   if GetExistingFileName('DEMIX tiles','*.txt',fName) then begin
      Memo2.Lines.LoadFromFile(fName);
   end;
end;


initialization
finalization
end.
