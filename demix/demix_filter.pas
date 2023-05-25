unit demix_filter;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordFullDEMIX}
  {$Define RecordDEMIX}
{$EndIf}


interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  StrUtils,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.Grids, Vcl.Graphics,
  Petmar_types;

const
   MaxDemixArray = 6;
type
  tDEMixarray = array[1..MaxDemixArray] of integer;


  TDemixFilterForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    BitBtn5: TBitBtn;
    CheckBox3: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox1: TCheckBox;
    ComboBox4: TComboBox;
    BitBtn4: TBitBtn;
    ComboBox3: TComboBox;
    ComboBox2: TComboBox;
    ComboBox1: TComboBox;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    GroupBox3: TGroupBox;
    Memo3: TMemo;
    GroupBox5: TGroupBox;
    Memo5: TMemo;
    GroupBox1: TGroupBox;
    Memo1: TMemo;
    GroupBox6: TGroupBox;
    Memo6: TMemo;
    BitBtn3: TBitBtn;
    GroupBox2: TGroupBox;
    Memo2: TMemo;
    Load: TBitBtn;
    GroupBox4: TGroupBox;
    Memo4: TMemo;
    BitBtn2: TBitBtn;
    Edit2: TEdit;
    Edit1: TEdit;
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    BitBtn1: TBitBtn;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    StringGrid1: TStringGrid;
    BitBtn6: TBitBtn;
    ComboBox8: TComboBox;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    Label4: TLabel;
    Edit3: TEdit;
    BitBtn9: TBitBtn;
    BitBtn10: TBitBtn;
    CheckBox4: TCheckBox;
    BitBtn11: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn13: TBitBtn;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    BitBtn14: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure LoadClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure ComboBox5Change(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure ComboBox7Change(Sender: TObject);
    procedure ComboBox8Change(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
  private
    { Private declarations }
    procedure ZeroDEMs;
    procedure UncheckAllLoadCheckboxes;
  public
    { Public declarations }
    DB : integer;
    DEMsTypeUsing,
    TilesUsing,
    LandTypesUsing,
    CriteriaUsing,
    CandidateDEMsUsing : tStringList;
    MergeDEMs,RefDEMs,RefDEMsv1,RefDEMsHalfSec,TestDEMs,DiffDSMDEMs,DiffDTMDEMs : tDEMixarray;
    procedure GetUsingStringLists;
    procedure DoCriteriaGraph;
    procedure LoadDEMsForCurrentArea(var AreaName : Petmar_types.shortstring; LoadMaps : boolean = true);
  end;


procedure DoDEMIXFilter(DB : integer);


implementation

{$R *.dfm}

uses
   Petmar,Petmar_db,PetMath,PetImage,PetImage_form,
   DEMDatabase,DEMDbTable,DEMdef_routines,DEMDefs,DEMcoord,
   BaseGraf,DEM_Manager,DEMstat,BaseMap,DEMlosw,Make_Grid,
   DEMIX_control,DEMmapf, nevadia_main;



procedure DoDEMIXFilter(DB : integer);
var
  DemixFilterForm: TDemixFilterForm;
  i: Integer;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('DoDEMIXFilter in'); {$EndIf}
   GetDEMIXpaths(false);
   DemixFilterForm := TDemixFilterForm.Create(Application);
   DemixFilterForm.db := db;
   DemixFilterForm.ZeroDEMs;

(*
   for i := pred(DemixFilterForm.Memo4.Lines.Count) downto 0 do begin
      if not GISdb[db].MyData.FieldExists(DemixFilterForm.Memo4.Lines[i]) then DemixFilterForm.Memo4.Lines.Delete(i);
   end;
   DemixFilterForm.Memo6.Lines.LoadFromFile(DEMIXSettingsDir + 'demix_areas_sorted_by_lat.txt');
*)

   DemixFilterForm.ComboBox1.Items.LoadFromFile(DEMIXSettingsDir + 'demix_tiles_list.txt');
   DemixFilterForm.ComboBox4.Items.LoadFromFile(DEMIXSettingsDir + 'demix_areas_list.txt');
   DemixFilterForm.ComboBox1.ItemIndex := 0;
   //DemixFilterForm.ComboBox4.ItemIndex := 0;
   DemixFilterForm.ComboBox4.Text := 'state_line';

   DemixFilterForm.ComboBox5.Items.LoadFromFile(DEMIXSettingsDir + 'demix_tiles_list.txt');
   DemixFilterForm.ComboBox5.ItemIndex := 0;
   DemixFilterForm.ComboBox6.Items.LoadFromFile(DEMIXSettingsDir + 'demix_criteria.txt');
   DemixFilterForm.ComboBox6.ItemIndex := 0;

   DemixFilterForm.DEMsTypeUsing := tStringList.Create;
   DemixFilterForm.TilesUsing := tStringList.Create;

   DemixFilterForm.LandTypesUsing := tStringList.Create;
   DemixFilterForm.CriteriaUsing := tStringList.Create;
   DemixFilterForm.CandidateDEMsUsing := tStringList.Create;
   DemixFilterForm.Edit3.Text := RealToString(MDDef.TopCutLevel,-8,-2);

   //DemixFilterForm.BitBtn1.Enabled := GISdb[db].MyData.FieldExists('DEM');

   DemixFilterForm.Show;

   //new_orleans_ALOS_N29ZW091L_elev_to_DTM.z
   //MakeGraphOfDifferenceDistribution('N29ZW091L','elev','dtm');
   {$IfDef RecordDEMIX} WriteLineToDebugFile('DoDEMIXFilter out'); {$EndIf}
end;


procedure TDemixFilterForm.UncheckAllLoadCheckboxes;
begin
   CheckBox1.Checked := false;
   CheckBox2.Checked := false;
   CheckBox3.Checked := false;
   CheckBox4.Checked := false;
   CheckBox5.Checked := false;
   CheckBox6.Checked := false;
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

procedure TDemixFilterForm.BitBtn3Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := DEMIXSettingsDir;
   if GetExistingFileName('Area names','*.txt',fName) then begin
      Memo6.Lines.LoadFromFile(fName);
   end;
end;


procedure TDemixFilterForm.BitBtn4Click(Sender: TObject);
begin
   MakeGraphOfDifferenceDistribution(ComboBox1.Items[ComboBox1.ItemIndex],ComboBox2.Items[ComboBox2.ItemIndex],ComboBox3.Items[ComboBox3.ItemIndex]);
end;

procedure TDemixFilterForm.ZeroDEMs;
var
   i : integer;
begin
   for i := 1 to 6 do begin
      MergeDEMs[i] := 0;
      DiffDSMDEMs[i] := 0;
      RefDEMs[i] := 0;
      RefDEMsv1[i] := 0;
      DiffDTMDEMs[i] := 0;
      TestDEMs[i] := 0;
      RefDEMsHalfSec[i] := 0;
   end;
end;



procedure TDemixFilterForm.BitBtn5Click(Sender: TObject);
var
   AreaName : Petmar_types.shortstring;
begin
   LoadDEMsForCurrentArea(AreaName,true);
end;


procedure TDemixFilterForm.BitBtn6Click(Sender: TObject);
var
   Filter : shortString;
   i,j : integer;
   v1,v2,diff : float32;
begin
   Filter := 'DEMIX_TILE=' + QuotedStr(ComboBox5.Text) + ' AND CRITERION=' + QuotedStr(ComboBox6.Text) +  ' AND REF_TYPE=' + QuotedStr(ComboBox7.Text) +  ' AND LAND_TYPE=' + QuotedStr(ComboBox8.Text);
   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.BitBtn6Click filter=' + Filter); {$EndIf}
   gisDB[DEMIX_DB_v1].ApplyGISFilter(Filter);
   gisDB[DEMIX_DB_v2].ApplyGISFilter(Filter);
   if (GISdb[DEMIX_DB_v2].MyData.FiltRecsInDB = 1) and (GISdb[DEMIX_DB_v1].MyData.FiltRecsInDB = 1) then begin
      for i := 1 to NumDEMIXDEM do begin
         v1 := GISdb[DEMIX_DB_v1].MyData.GetFieldByNameAsFloat(DEMIXDEMTypeName[i]);
         v2 := GISdb[DEMIX_DB_v2].MyData.GetFieldByNameAsFloat(DEMIXDEMTypeName[i]);
         Diff := v2 - v1;
         StringGrid1.Cells[i,1] := RealToString(v1,8,2);
         StringGrid1.Cells[i,2] := RealToString(v2,8,2);
         StringGrid1.Cells[i,3] := RealToString(Diff,8,2);
      end;
   end
   else begin
      for i := 1 to NumDEMIXDEM do begin
         for j := 1 to 3 do
            StringGrid1.Cells[i,j] := '';
      end;
   end;
end;


procedure TDemixFilterForm.BitBtn7Click(Sender: TObject);
const
   DEMType : array[1..2] of shortstring = ('DTM','DSM');
var
   i,j,k : integer;
   DEMarea : ANSIString;
   fName : PathStr;

   procedure GetRefDEM(theRefDEMs : tDEMixarray; TestDEM : integer; DEMArea,RefPointOrArea,theDEMtype,TestDEMseriesName : shortstring);
   var
      i,refDEMsurface : integer;
      refDEM : shortString;
   begin
      for I := 1 to 6 do begin
         if ValidDEM(RefDEMs[i]) then begin
            refDEM := UpperCase(DEMGlb[RefDEMs[i]].AreaName);
            refDEMsurface := IsDEMaDSMorDTM(refDEM);
            if StrUtils.AnsiContainsText(refDEM,RefPointOrArea) then begin
               if (refDEMSurface = DEMisDSM) and (theDEMtype = 'DSM') then begin
                  DiffDSMDEMs[i] := MakeDifferenceMap(theRefDEMs[i],TestDEM,true,false,false,TestDEMseriesName + '_Delta_to_' + DEMGlb[RefDEMs[i]].AreaName)
               end
               else if (refDEMSurface = DEMisDTM) and (theDEMtype = 'DTM') then begin
                  DiffDTMDEMs[i] := MakeDifferenceMap(theRefDEMs[i],TestDEM,true,false,false,TestDEMseriesName + '_Delta_to_' + DEMGlb[RefDEMs[i]].AreaName);
               end;
            end;
         end;
      end;
   end;

var
   theRefDEMs : tDEMixarray;
   RefPointOrArea,SeriesName,AreaName : shortstring;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.BitBtn7Click (difference maps) in'); {$EndIf}
   UncheckAllLoadCheckboxes;
   CheckBox2.Checked := (Sender = BitBtn7) or (Sender = BitBtn10);
   CheckBox3.Checked := true;
   CheckBox4.Checked := (Sender = BitBtn11);
   LoadDEMsForCurrentArea(AreaName,false);

   if (Sender = BitBtn11) then theRefDEMs := RefDEMsv1 else theRefDEMs := RefDEMs;

   for j := 1 to 2 do begin
      for i := 1 to 6 do begin
         //this will not work yet for the high latitude areas
         if ValidDEM(TestDEMs[i]) then begin
            DEMArea := UpperCase(DEMGlb[TestDEMs[i]].AreaName);
            for  k := 1 to 6 do if StrUtils.AnsiContainsText(DEMArea,DEMIXDEMTypeName[k]) then SeriesName := DEMIXDEMTypeName[k];
            {$IfDef RecordDEMIX} WriteLineToDebugFile('Area=' + DEMArea + ' test DEM=' + DEMIXDEMTypeName[i] ); {$EndIf}
            if StrUtils.AnsiContainsText(DEMArea,'ALOS') then RefPointOrArea := 'AREA' else RefPointOrArea := 'POINT';
            GetRefDEM(theRefDEMs,TestDEMs[i],DEMArea,RefPointOrArea,DEMType[j],SeriesName);
         end;
      end;
   end;
   fName := NextFileNumber(MDTempDir,ComboBox4.Text + '_difference_maps_','.png');
   Bigimagewithallmaps(3,fName);
   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.BitBtn7Click out'); {$EndIf}
end;


procedure TDemixFilterForm.BitBtn8Click(Sender: TObject);
//var
   //i : integer;
begin
   (*
   for i := 1 to 6 do begin
      //must do these so the values are zeroed
      CloseSingleDEM(MergeDEMs[i]);
      CloseSingleDEM(DiffDSMDEMs[i]);
      CloseSingleDEM(DiffDTMDEMs[i]);
      CloseSingleDEM(RefDEMs[i]);
      CloseSingleDEM(TestDEMs[i]);
      CloseSingleDEM(RefDEMsHalfSec[i]);
   end;
   *)
   CloseAllDEMs;
   ZeroDEMs;
end;

procedure TDemixFilterForm.BitBtn9Click(Sender: TObject);
var
   i : integer;
   fName : PathStr;
begin
   CheckEditString(Edit3.Text,MDDef.TopCutLevel);
   MDDef.BottomCutLevel := -MDDef.TopCutLevel;
   for i := 1 to 6 do begin
      if ValidDEM(DiffDSMDEMs[i]) then DEMGlb[DiffDSMDEMs[i]].SelectionMap.DoCompleteMapRedraw;
      if ValidDEM(DiffDTMDEMs[i]) then DEMGlb[DiffDTMDEMs[i]].SelectionMap.DoCompleteMapRedraw;
   end;
   fName := NextFileNumber(MDTempDir,ComboBox4.Text + '_difference_maps_','.png');
   Bigimagewithallmaps(3,fName);
end;

procedure TDemixFilterForm.ComboBox5Change(Sender: TObject);
begin
   BitBtn6Click(Sender);
end;

procedure TDemixFilterForm.ComboBox6Change(Sender: TObject);
begin
   BitBtn6Click(Sender);
end;

procedure TDemixFilterForm.ComboBox7Change(Sender: TObject);
begin
   BitBtn6Click(Sender);
end;

procedure TDemixFilterForm.ComboBox8Change(Sender: TObject);
begin
   BitBtn6Click(Sender);
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
            if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
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

procedure TDemixFilterForm.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,MDDef.TopCutLevel);
   MDDef.BottomCutLevel := -MDDef.TopCutLevel;
end;

procedure TDemixFilterForm.FormCreate(Sender: TObject);
var
   i : integer;
begin
   Edit1.Text := IntToStr(MDDef.DEMIX_xsize);
   Edit2.Text := IntToStr(MDDef.DEMIX_ysize);
   //Edit3.Text := RealToString(MDDef.TopCutLevel ,-8,-2);
   for i := 1 to NumDEMIXDEM do begin
      StringGrid1.Cells[i,0] := DEMIXDEMTypeName[i];
   end;
   StringGrid1.Cells[0,1] := 'v1';
   StringGrid1.Cells[0,2] := 'v2';
   StringGrid1.Cells[0,3] := 'Difference';
end;


procedure TDemixFilterForm.BitBtn10Click(Sender: TObject);
var
   i : integer;
begin
   try
      HeavyDutyProcessing := true;
      for i := 0 to pred(ComboBox4.Items.Count) do begin
         wmdem.SetPanelText(2,IntToStr(succ(i)) + '/' + IntToStr(ComboBox4.Items.Count));
         ComboBox4.Text := ComboBox4.Items[i];
         BitBtn7Click(Sender);   //load dems and draw difference maps
         CloseAllDEMs;
      end;
   finally
      HeavyDutyProcessing := false;
      ShowDefaultCursor;
   end;
end;


procedure TDemixFilterForm.BitBtn11Click(Sender: TObject);
begin
   BitBtn10Click(Sender);
end;



procedure TDemixFilterForm.BitBtn12Click(Sender: TObject);
const
   ProfileLength = 250;
var
   //i,j,k : integer;
   //DEMarea : ANSIString;
   //fName : PathStr;
   xloc,yloc : integer;
   Lat,Long{,LatLeft,LongLeft,LatRight,LongRight} : float64;
   LocMax : float32;
   //LOS1 : TDEMLOSF;
   //Legend : tMyBitmap;
   //theFiles : tStringList;
   AreaName : shortstring;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.BitBtn12Click (terrain profiles) in'); {$EndIf}
   UncheckAllLoadCheckboxes;
   CheckBox2.Checked := true;
   CheckBox3.Checked := true;
   LoadDEMsForCurrentArea(AreaName,false);
   DEMglb[RefDEMs[1]].FindLocationOfMaximum(DEMglb[RefDEMs[1]].FullDEMGridLimits,xloc,yloc,LocMax);
   DEMglb[RefDEMs[1]].DEMGridToLatLongDegree(xloc,yloc,lat,long);
   DrawProfilesThroughPeak(RefDEMs[1],Lat,Long);
end;


procedure TDemixFilterForm.BitBtn13Click(Sender: TObject);
var
   i : integer;
begin
   try
      HeavyDutyProcessing := true;
      for i := 0 to pred(ComboBox4.Items.Count) do begin
         wmdem.SetPanelText(2,IntToStr(succ(i)) + '/' + IntToStr(ComboBox4.Items.Count));
         ComboBox4.Text := ComboBox4.Items[i];
         BitBtn12Click(Sender);   //load dems and draw topo profiles
         CloseAllDEMs;
         CloseAllDataBases;
      end;
   finally
      HeavyDutyProcessing := false;
      ShowDefaultCursor;
   end;
end;


procedure TDemixFilterForm.BitBtn14Click(Sender: TObject);
var
   AreaName,fName : PathStr;
   i,j,DiffMaps,COPDEM,ALOSDEM,NewGrid : integer;
begin
   UncheckAllLoadCheckboxes;
   CheckBox5.Checked := true;
   CheckBox6.Checked := true;
   LoadDEMsForCurrentArea(AreaName,true);
   Diffmaps := 0;
   for i := 1 to MaxDemixArray do if ValidDEM(RefDEMsHalfSec[i]) then begin
      for j := 1 to MaxDemixArray do if ValidDEM(TestDEMs[j]) then begin
         inc(DiffMaps);
         DiffDTMDEMs[DiffMaps] := MakeDifferenceMap(RefDEMsHalfSec[i],TestDEMs[j],true,false,false,DEMglb[RefDEMsHalfSec[i]].AreaName + '_Delta_to_' + DEMglb[TestDEMs[j]].AreaName);
      end;
   end;

   COPDEM := 0;
   ALOSDEM := 0;
   for j := 1 to MaxDemixArray do if ValidDEM(TestDEMs[j]) then begin
      if StrUtils.AnsiContainsText(UpperCase(DEMglb[TestDEMs[j]].AreaName),'COP') then COPDEM := j;
      if StrUtils.AnsiContainsText(UpperCase(DEMglb[TestDEMs[j]].AreaName),'ALOS') then ALOSDEM := j;
   end;

   for i := 1 to MaxDemixArray do if ValidDEM(RefDEMsHalfSec[i]) then begin
      fName := 'alos_cop_high_low.dem';
      NewGrid := TwoDEMHighLowMap(RefDEMsHalfSec[i],ALOSDEM,COPDEM,MDDef.TopCutLevel,true,fName);
      if MDDef.AutoMergeStartDEM then begin
         DEMGlb[NewGrid].SelectionMap.MergeAnotherDEMreflectance(COPDEM,true);
      end;
   end;
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



procedure TDemixFilterForm.LoadDEMsForCurrentArea(var AreaName: ShortString;  LoadMaps: boolean);
var
   DEMs : integer;

      procedure LoadFromPath(var Which : tDEMIXarray; aPath : PathStr; Ext : ANSIstring; LoadMaps : boolean; Limit : shortstring; What : shortstring);
      var
         FilesWanted : tStringList;
         j : integer;
         fName,NewName : PathStr;
      begin
         FilesWanted := tStringList.Create;
         FindMatchingFiles(aPath,Ext,FilesWanted,1);
         if Limit = '' then DEMs := 0;
         for j := 0 to pred(FilesWanted.Count) do begin
            fName := FilesWanted.Strings[j];
            if StrUtils.AnsiContainsText(UpperCase(fname),UpperCase(AreaName)) then begin
               if (Limit = '') or StrUtils.AnsiContainsText(UpperCase(fname),UpperCase(Limit)) then begin
                  if (DEMs = MaxDemixArray) then begin
                     MessageToContinue('Too many DEMs in ' + aPath);
                  end
                  else begin
                     inc(DEMs);
                     Which[DEMs] := OpenNewDEM(fName,LoadMaps);
                  end;
               end;
            end;
         end;
         FilesWanted.Free;
         {$IfDef RecordDEMIX} WriteLineToDebugFile('Loaded ' + What + '=' + IntToStr(DEMs)); {$EndIf}
      end;

var
   i : integer;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.LoadDEMsForArea in'); {$EndIf}
   AreaName := ComboBox4.Text;
   ZeroDEMs;

   if CheckBox1.Checked then LoadFromPath(MergeDEMs,DEMIX_Ref_Merge,'*.dem',LoadMaps,'','Merge');
   if CheckBox2.Checked then LoadFromPath(RefDEMs,DEMIX_Ref_1sec,'*.tif',LoadMaps,'','Ref');
   if CheckBox3.Checked then LoadFromPath(TestDEMs,DEMIX_test_dems,'*.dem',LoadMaps,'','Test');
   if CheckBox4.Checked then LoadFromPath(RefDEMsv1,DEMIX_Ref_1sec_v1,'*.tif',LoadMaps,'','Ref_v1');
   if CheckBox5.Checked then LoadFromPath(RefDEMsHalfSec,DEMIX_Ref_Half_sec,'*.tif',LoadMaps,'','Ref_half_sec');
   if CheckBox6.Checked and (not CheckBox3.Checked) then begin
      DEMs := 0;
      LoadFromPath(TestDEMs,DEMIX_test_dems,'*.dem',LoadMaps,'COP','COP & ALOS Test');
      LoadFromPath(TestDEMs,DEMIX_test_dems,'*.dem',LoadMaps,'ALOS','COP & ALOS Test');
   end;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.LoadDEMsForArea out'); {$EndIf}
end;


initialization
finalization
end.
