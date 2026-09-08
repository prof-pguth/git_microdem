unit dsm_dtm_compare;
{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

//{$Define ExDEMIXexperimentalOptions}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$Define RecordDSM_DTM_Compare}
{$EndIf}


interface

uses
//needed for inline of core DB functions
   Petmar_db,
   Data.DB,

   {$IfDef UseFireDacSQLlite}
      FireDAC.Stan.ExprFuncs,
      FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
      FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
      FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
      FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Comp.Client, FireDAC.Comp.DataSet,
      FireDAC.Phys.SQLite, FireDAC.Comp.UI,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end core DB functions definitions

  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.Menus;

type
  TCompareDSM_DTMform = class(TForm)
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    Memo1: TMemo;
    Memo2: TMemo;
    ComboBox1: TComboBox;
    GroupBox3: TGroupBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    BitBtn10: TBitBtn;
    BitBtn11: TBitBtn;
    BitBtn38: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn13: TBitBtn;
    Label1: TLabel;
    Edit1: TEdit;
    BitBtn14: TBitBtn;
    Memo3: TMemo;
    BitBtn15: TBitBtn;
    BitBtn16: TBitBtn;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    BitBtn18: TBitBtn;
    BitBtn17: TBitBtn;
    BitBtn19: TBitBtn;
    BitBtn20: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn6: TBitBtn;
    //CheckBox1: TCheckBox;
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure ComboBox7Change(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn38Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure BitBtn19Click(Sender: TObject);
    procedure BitBtn20Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    //procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
    procedure GraphsCurrentRec;
    procedure LoadMemo1;
    function DEMComparingList : tStringList;
    function ComparingCriteriaList: tStringList;
    function ResolutionsList: tStringList;
  public
    { Public declarations }
     db,OnRec,DTM_1sec,DSM_1sec,HRDTM,HRDSM,LCgrid : integer;
     theDTMs : tstringlist;
     DEMIX_scale_compare : boolean;
     BaseFilter,DEMIXtileFieldName : shortstring;
  end;


procedure StartDSMandDTMcomparison(inDB : integer);


implementation

{$R *.dfm}

uses
   DEMDataBase,DEMIX_graphs,DEMIX_definitions,
   DEMdefs, DEMdef_routines,
   Make_grid,
   DEM_manager, DEM_NLCD, DEMcoord,
   Petmar,Petmar_Types, PetDBUtils,PETImage,PetImage_form,PetMath,
   BaseGraf,
   Nevadia_Main;


procedure StartDSMandDTMcomparison(inDB : integer);
var
  CompareDSM_DTMform: TCompareDSM_DTMform;
  ResString : shortstring;
  Resolutions : tStringList;
begin
  if TileCharacteristicsInDB(inDB) then begin
      {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('StartDSMandDTMcomparison in, ' + GISdb[inDB].DBname); {$EndIf}
      MDdef.DBsOnAllMaps := false;
      MDDef.CountHistograms := false;
      CompareDSM_DTMform := TCompareDSM_DTMform.Create(Application);
      CompareDSM_DTMform.db := inDB;
      CompareDSM_DTMform.BaseFilter := GISdb[inDB].MyData.Filter;

     if GISdb[indb].MyData.FieldExists('GRID_SEC') then ResString := 'GRID_SEC'
     else if GISdb[indb].MyData.FieldExists('GRID_M') then ResString := 'GRID_M'
     else begin
        ResString := '';
        CompareDSM_DTMform.Memo3.Visible := false;
     end;
     if (ResString <> '') then begin
        GISdb[inDB].EmpSource.Enabled := false;
        Resolutions := GISdb[indb].MyData.ListUniqueEntriesInDB(ResString);
        SortStringListNumerically(Resolutions);
        CompareDSM_DTMform.Memo3.Lines := Resolutions;
     end;

     //CompareDSM_DTMform.CheckBox1.Checked := MDDef.AllFilter1SameGraph;

      GISdb[inDB].EmpSource.Enabled := false;
      CompareDSM_DTMform.DEMIX_scale_compare := GISdb[inDB].MyData.FieldExists('DEM_1') and GISdb[inDB].MyData.FieldExists('DEM_2');
      CompareDSM_DTMform.LoadMemo1;
      if not GISdb[inDB].MyData.FieldExists(MDDef.DEMIX_SingleCriterion) then MDDef.DEMIX_SingleCriterion := CompareDSM_DTMform.ComboBox1.Items[0];
      CompareDSM_DTMform.ComboBox1.Text := MDDef.DEMIX_SingleCriterion;

      if GISdb[inDB].MyData.FieldExists('DEMIX_TILE') then CompareDSM_DTMform.DEMIXtileFieldName := 'DEMIX_TILE'
      else CompareDSM_DTMform.DEMIXtileFieldName := 'DTM_NAME';

      if CompareDSM_DTMform.DEMIX_scale_compare then begin
         if not GISdb[inDB].MyData.FieldExists('COMPARE') then begin
             GISdb[inDB].MyData.InsureFieldPresentAndAdded(ftString,'COMPARE',35);
             GISdb[inDB].MyData.First;
             while Not GISdb[inDB].MyData.eof do begin
                GISdb[inDB].MyData.Edit;
                GISdb[inDB].MyData.SetFieldByNameAsString('COMPARE',GISdb[inDB].MyData.GetFieldByNameAsString('DEM1') + '_to_' + GISdb[inDB].MyData.GetFieldByNameAsString('DEM2'));
                GISdb[inDB].MyData.Next;
             end;
         end;
      end;
      CompareDSM_DTMform.theDTMs := GISdb[CompareDSM_DTMform.db].MyData.ListUniqueEntriesInDB(CompareDSM_DTMform.DEMIXtileFieldName);
      GISdb[inDB].ClearGISFilter;
      CompareDSM_DTMform.BitBtn15.Visible := GISdb[inDB].MyData.FieldExists('GRID_THIN') and GISdb[inDB].MyData.FieldExists('DSM_NAME') and GISdb[inDB].MyData.FieldExists('DTM_NAME');
      CompareDSM_DTMform.Show;
      {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('StartDSMandDTMcomparison out, ' + GISdb[inDB].DBname); {$EndIf}
  end
  else begin
     {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('StartDSMandDTMcomparison fail no tile characteristics, ' + GISdb[inDB].DBname); {$EndIf}
  end;
end;

procedure TCompareDSM_DTMform.ComboBox1Change(Sender: TObject);
begin
  MDDef.DEMIX_SingleCriterion := ComboBox1.Text;
end;

procedure TCompareDSM_DTMform.ComboBox6Change(Sender: TObject);
begin
   MDDef.DEMIX_filter1_fName := ExpandFullFilterName(ComboBox6.Text);
end;

procedure TCompareDSM_DTMform.ComboBox7Change(Sender: TObject);
begin
   MDDef.DEMIX_filter2_fName := ExpandFullFilterName(ComboBox7.Text);
end;


function TCompareDSM_DTMform.DEMComparingList : tStringList;
var
   i : integer;
   TStr : shortstring;
begin
   Result := tStringList.Create;
   for i := 0 to pred(Memo1.Lines.Count) do begin
      TStr := trim(Memo1.Lines[i]);
      if (TStr <> '') then Result.Add(TStr);
   end;
end;


procedure TCompareDSM_DTMform.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDdef.DEMIX_MaxTilesInLegend);
end;


procedure TCompareDSM_DTMform.FormCreate(Sender: TObject);
begin
    {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('TCompareDSM_DTMform.FormCreate in'); {$EndIf}
    OnRec := 0;
    DTM_1sec := 0;
    DSM_1sec := 0;
    HRDTM := 0;
    HRDSM := 0;
    LCgrid := 0;
    Edit1.Text := IntToStr(MDdef.DEMIX_MaxTilesInLegend);

    MDDef.DEMIX_xsize := 725;
    MDDef.DEMIX_ysize := 525;
    BitBtn11.Caption := 'Graph size: ' + IntToStr(MDDef.DEMIX_xsize) + ' by ' + IntToStr(MDDef.DEMIX_ysize);

    LoadTwoDEMIXfilters(ComboBox6,ComboBox7);

    if MDDEF.DEMIX_UseMedian then RadioGroup2.ItemIndex := 1 else RadioGroup2.ItemIndex := 0;
    RadioGroup3.ItemIndex := MDDef.DEMIX_MultiGraphCommonScaling;
    {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('TCompareDSM_DTMform.FormCreate out'); {$EndIf}
end;


function TCompareDSM_DTMform.ResolutionsList : tStringList;
var
   i : integer;
   TStr : shortstring;
begin
   Result := tStringList.Create;
   for i := 0 to pred(Memo3.Lines.Count) do begin
      TStr := trim(Memo3.Lines[i]);
      if (TStr <> '') then Result.Add(TStr);
   end;
end;


function TCompareDSM_DTMform.ComparingCriteriaList : tStringList;
var
   i : integer;
   TStr : shortstring;
begin
   Result := tStringList.Create;
   for i := 0 to pred(Memo2.Lines.Count) do begin
      TStr := trim(Memo2.Lines[i]);
      if (TStr <> '') then Result.Add(TStr);
   end;
end;


procedure ReloadMemo2(db : integer; var Memo2 : tMemo; ComboBox1 : tComboBox);
var
   MultSeries : tStringList;
   i : integer;
begin
   Memo2.Lines.Clear;
   ComboBox1.Items.Clear;
   PetDBUtils.GetFields(GISdb[db].MyData,GISdb[db].dbOpts.VisCols,NumericFieldTypes,MultSeries);
   RemoveInvalidCriterion(MultSeries);
   for i := 0 to pred(MultSeries.Count) do begin
       Memo2.Lines.Add(MultSeries[i]);
       ComboBox1.Items.Add(MultSeries[i]);
   end;
   MultSeries.Destroy;
end;


procedure TCompareDSM_DTMform.LoadMemo1;
var
   Comparisons,MultSeries : tStringList;
   i : integer;
begin
   Memo1.Lines.Clear;
   if GISdb[db].MyData.FieldExists('COMPARE') then begin
       Memo1.Visible := true;
       Comparisons := GISdb[db].MyData.ListUniqueEntriesInDB('COMPARE');
       RemoveInvalidCriterion(Comparisons);
       for i := 0 to pred(Comparisons.Count) do
           Memo1.Lines.Add(Comparisons[i]);
       if (Comparisons.Count < 2) then begin
          Memo1.Enabled := false;
          BitBtn10.Enabled := false;
       end;
       Comparisons.Destroy;

       ReloadMemo2(db,Memo2,ComboBox1);
   end
   else begin
      Memo1.Visible := false;
   end;
end;


procedure TCompareDSM_DTMform.BitBtn10Click(Sender: TObject);
begin
   {$IfDef RecordDSM_DTM_Compare} HighlightLineToDebugFile('TCompareDSM_DTMform.BitBtn10Click in'); {$EndIf}
   Self.Hide;
   GraphMultipleParamsByDEMResolution(db,MDDef.DEMIX_SingleCriterion,DEMIXtileFieldName,ResolutionsList,DEMComparingList);
   Self.Show;
end;


procedure TCompareDSM_DTMform.BitBtn11Click(Sender: TObject);
begin
   if GetNewBMPSize(MDDef.DEMIX_xsize,MDDef.DEMIX_ysize,'DEM comparison graphs') then begin
      BitBtn11.Caption := 'Graph size: ' + IntToStr(MDDef.DEMIX_xsize) + ' by ' + IntToStr(MDDef.DEMIX_ysize);
   end;
end;

procedure TCompareDSM_DTMform.BitBtn12Click(Sender: TObject);
begin
   wmdem.Closeallpictureviewwindows1Click(Sender);
end;

procedure TCompareDSM_DTMform.BitBtn13Click(Sender: TObject);
begin
   wmdem.Closeallgraphs1Click(Sender);
end;

procedure TCompareDSM_DTMform.BitBtn14Click(Sender: TObject);
begin
   GridOfTerrainScatterPlots(DB,ComparingCriteriaList,DEMComparingList,Nil);
end;


procedure TCompareDSM_DTMform.BitBtn15Click(Sender: TObject);
begin
   ManyGraphsSlopeVersusResolutionManyTiles(db,'DSM_SLOPE');
end;

procedure TCompareDSM_DTMform.BitBtn16Click(Sender: TObject);
begin
   ManyGraphsSlopeVersusResolutionManyTiles(db,'DTM_SLOPE');
end;

procedure TCompareDSM_DTMform.BitBtn17Click(Sender: TObject);
begin
   GridOfTerrainScatterPlots(DB,MakeStringListFromString(MDDef.DEMIX_SingleCriterion),DEMComparingList,Nil);
end;

procedure TCompareDSM_DTMform.BitBtn18Click(Sender: TObject);
begin
   ReloadMemo2(db,Memo2,ComboBox1);
end;

procedure TCompareDSM_DTMform.BitBtn19Click(Sender: TObject);
begin
   LoadMemo1;
end;

procedure TCompareDSM_DTMform.BitBtn20Click(Sender: TObject);
begin
   Self.Hide;
   GridScatterPlotByDEMResolution(db,MDDef.DEMIX_SingleCriterion,'EDGE_ALL');
   Self.Show;
end;

procedure TCompareDSM_DTMform.BitBtn2Click(Sender: TObject);
begin
   ManyGraphsSlopeVersusResolutionManyTiles(db,'DEM1_SLOPE');
end;

procedure TCompareDSM_DTMform.BitBtn38Click(Sender: TObject);
begin
   SaveMDDefaults;
end;


procedure TCompareDSM_DTMform.BitBtn6Click(Sender: TObject);
var
  BaseFilter,af : shortstring;
begin
   {$IfDef RecordDSM_DTM_Compare} HighlightLineToDebugFile('TCompareDSM_DTMform.BitBtn6Click in'); {$EndIf}
   Self.Hide;
   GraphMultParamsByAvgSlope_DEMResolution(db,DEMIXtileFieldName,ResolutionsList,MakeStringListFromString(MDDef.DEMIX_SingleCriterion), DEMComparingList);
   Self.Show;
end;


procedure TCompareDSM_DTMform.BitBtn8Click(Sender: TObject);
var
  BaseFilter,af : shortstring;
  i : integer;
begin
   {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('TCompareDSM_DTMform.BitBtn8Click in'); {$EndIf}
   Self.Hide;
   //BaseFilter := GISdb[db].MyData.Filter;
   GraphMultParamsByAvgSlope_DEMResolution(db,DEMIXtileFieldName,ResolutionsList,ComparingCriteriaList,DEMcomparingList);
   //GISdb[db].ApplyGISfilter(BaseFilter);
   Self.Show;
end;


procedure TCompareDSM_DTMform.BitBtn9Click(Sender: TObject);
begin
   {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('TCompareDSM_DTMform.BitBtn9Click in'); {$EndIf}
   Self.Hide;
   GraphMultParamsByAvgSlope_DEMResolution(db,DEMIXtileFieldName,ResolutionsList,MakeStringListFromString(MDDef.DEMIX_SingleCriterion),DEMcomparingList,true);
   Self.Show;
end;


procedure TCompareDSM_DTMform.GraphsCurrentRec;
var
   gr1,gr2 : tThisBaseGraph;
   TileStats : shortstring;
begin
   Self.Hide;
   if DEMIX_scale_compare then begin
      // July 2026, unclear if this works
       GISdb[db].ApplyGISFilter('TILE=' + QuotedStr(TheDTMs.Strings[onRec]));
       TileStats := 'Slope=' + GISdb[db].MyData.GetFieldByNameAsString('AVG_SLOPE') + '% ' +
                    'Barren=' + GISdb[db].MyData.GetFieldByNameAsString('BARREN_PC') + '% ' +
                    'Forest=' + GISdb[db].MyData.GetFieldByNameAsString('FOREST_PC') + '%';
       gr1 := GraphDEMIX_CompareDSMandDTMslopes(DB,TheDTMs.Strings[onRec],TileStats,ResolutionsList);
       GISdb[db].ClearGISFilter;
   end
   else begin
       gr1 := GraphCompareDSMandDTMslopes(DB,TheDTMs.Strings[onRec]);
       gr2 := GraphDSMandDTMdifferences(DB,TheDTMs.Strings[OnRec],MDDef.DEMIX_SingleCriterion);
       if (gr2 <> nil) then begin
          gr2.Left := 1200;
          gr2.Top := 50;
       end;
   end;
   if (gr1 <> nil) then begin
      gr1.Left := 1;
      gr1.Top := 50;
   end;
   Self.Show;
end;


procedure TCompareDSM_DTMform.RadioGroup2Click(Sender: TObject);
begin
   MDDEF.DEMIX_UseMedian := (RadioGroup2.ItemIndex = 1);
end;

procedure TCompareDSM_DTMform.RadioGroup3Click(Sender: TObject);
begin
   MDDef.DEMIX_MultiGraphCommonScaling := RadioGroup3.ItemIndex;
end;


initialization
finalization
end.
