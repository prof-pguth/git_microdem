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
   {$Define RecordDEMIX}
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
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    MainMenu1: TMainMenu;
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
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    //procedure BitBtn10Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure ComboBox7Change(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn38Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
    procedure GraphsCurrentRec;
    procedure LoadMemo1;
    function DEMComparingList : tStringList;
    function ComparingCriteriaList: tStringList;
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
   Petmar,Petmar_Types, PetDBUtils,PETImage,PetImage_form,
   BaseGraf,
   Nevadia_Main;


procedure StartDSMandDTMcomparison(inDB : integer);
var
  CompareDSM_DTMform: TCompareDSM_DTMform;
begin
  if TileCharacteristicsInDB(inDB,true) then begin
      MDdef.DBsOnAllMaps := false;
      MDDef.CountHistograms := false;
      CompareDSM_DTMform := TCompareDSM_DTMform.Create(Application);
      CompareDSM_DTMform.db := inDB;
      CompareDSM_DTMform.BaseFilter := GISdb[inDB].MyData.Filter;
      CompareDSM_DTMform.OnRec := 0;
      CompareDSM_DTMform.DTM_1sec := 0;
      CompareDSM_DTMform.DSM_1sec := 0;
      CompareDSM_DTMform.HRDTM := 0;
      CompareDSM_DTMform.HRDSM := 0;
      CompareDSM_DTMform.LCgrid := 0;
      CompareDSM_DTMform.Edit1.Text := IntToStr(MDdef.DEMIX_MaxTilesInLegend);

      MDDef.DEMIX_xsize := 725;
      MDDef.DEMIX_ysize := 525;
      CompareDSM_DTMform.BitBtn11.Caption := 'Graph size: ' + IntToStr(MDDef.DEMIX_xsize) + ' by ' + IntToStr(MDDef.DEMIX_ysize);

      LoadTwoDEMIXfilters(CompareDSM_DTMform.ComboBox6,CompareDSM_DTMform.ComboBox7);

      if MDDEF.DEMIX_UseMedian then CompareDSM_DTMform.RadioGroup2.ItemIndex := 1 else CompareDSM_DTMform.RadioGroup2.ItemIndex := 0;
      if MDDef.DEMIX_MultiGraphCommonScaling then CompareDSM_DTMform.RadioGroup3.ItemIndex := 0 else CompareDSM_DTMform.RadioGroup3.ItemIndex := 1;

      GISdb[inDB].EmpSource.Enabled := false;
      CompareDSM_DTMform.DEMIX_scale_compare := GISdb[inDB].MyData.FieldExists('DEM_1') and GISdb[inDB].MyData.FieldExists('DEM_2');
      CompareDSM_DTMform.LoadMemo1;
      if not GISdb[inDB].MyData.FieldExists(MDDef.DEMIX_SingleCriterion) then MDDef.DEMIX_SingleCriterion := CompareDSM_DTMform.ComboBox1.Items[0];
      CompareDSM_DTMform.ComboBox1.Text := MDDef.DEMIX_SingleCriterion;

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
         CompareDSM_DTMform.DEMIXtileFieldName := 'DEMIX_TILE';
      end
      else begin
         CompareDSM_DTMform.DEMIXtileFieldName := 'DTM_NAME';
      end;
      CompareDSM_DTMform.theDTMs := GISdb[CompareDSM_DTMform.db].MyData.ListUniqueEntriesInDB(CompareDSM_DTMform.DEMIXtileFieldName);
      GISdb[inDB].ClearGISFilter;
      CompareDSM_DTMform.Show;
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
begin
   Result := tStringList.Create;
   for i := 0 to pred(Memo1.Lines.Count) do begin
      //if GISdb[DB].MyData.FieldExists(Memo1.Lines[i]) then
         Result.Add(Memo1.Lines[i]);
   end;
end;


procedure TCompareDSM_DTMform.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDdef.DEMIX_MaxTilesInLegend);
end;

function TCompareDSM_DTMform.ComparingCriteriaList : tStringList;
var
   i : integer;
begin
   Result := tStringList.Create;
   for i := 0 to pred(Memo2.Lines.Count) do
      Result.Add(Memo2.Lines[i]);
end;


procedure TCompareDSM_DTMform.LoadMemo1;
var
   Comparisons,MultSeries : tStringList;
   i : integer;
begin
   Memo1.Lines.Clear;
   Comparisons := GISdb[db].MyData.ListUniqueEntriesInDB('COMPARE');
   RemoveInvalidCriterion(Comparisons);

   for i := 0 to pred(Comparisons.Count) do
       Memo1.Lines.Add(Comparisons[i]);
   if Comparisons.Count < 2 then begin
      Memo1.Enabled := false;
      BitBtn10.Enabled := false;
   end;
   Comparisons.Destroy;

   Memo2.Lines.Clear;
   PetDBUtils.GetFields(GISdb[db].MyData,GISdb[db].dbOpts.VisCols,NumericFieldTypes,MultSeries);
   RemoveInvalidCriterion(MultSeries);
   for i := 0 to pred(MultSeries.Count) do begin
       Memo2.Lines.Add(MultSeries[i]);
       ComboBox1.Items.Add(MultSeries[i]);
   end;
   MultSeries.Destroy;
end;


procedure TCompareDSM_DTMform.BitBtn10Click(Sender: TObject);
begin
   GraphTwoParameterGridsByDEMresolution(db,MDDef.DEMIX_SingleCriterion,DEMIXtileFieldName,DEMComparingList);
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

procedure TCompareDSM_DTMform.BitBtn1Click(Sender: TObject);
begin
   if (OnRec < pred(theDTMs.Count)) then begin
      inc(onRec);
      GraphsCurrentRec;
   end;
end;


procedure TCompareDSM_DTMform.BitBtn2Click(Sender: TObject);
var
   DSMname,DTMname : PathStr;
   AreaName : shortstring;
begin
   AreaName :=  GISdb[DB].MyData.GetFieldByNameAsString('AREA');
   DSMName := ExtractFilePath(GISdb[DB].DBFullName) + AreaName + '\source\' + GISdb[DB].MyData.GetFieldByNameAsString('DSM_NAME') + '.tif';
   DTMName := ExtractFilePath(GISdb[DB].DBFullName) + AreaName + '\source\' + GISdb[DB].MyData.GetFieldByNameAsString('DTM_NAME') + '.tif';
   HRDTM := OpenNewDEM(DTMName);
   HRDSM := OpenNewDEM(DSMName);
   if not ValidDEM(LCgrid) then LCgrid := LoadLC10LandCover('',DEMglb[HRDTM].SelectionMap.MapDraw.MapCorners.BoundBoxGeo,true);
end;


procedure TCompareDSM_DTMform.BitBtn38Click(Sender: TObject);
begin
   SaveMDDefaults;
end;

procedure TCompareDSM_DTMform.BitBtn3Click(Sender: TObject);
begin
   if (OnRec > 0) then begin
      dec(onRec);
      GraphsCurrentRec;
   end;
end;

procedure TCompareDSM_DTMform.BitBtn4Click(Sender: TObject);
begin
   onRec:= 0;
   GraphsCurrentRec;
end;

procedure TCompareDSM_DTMform.BitBtn5Click(Sender: TObject);
begin
   OnRec := pred(theDTMs.Count);
   GraphsCurrentRec;
end;


procedure TCompareDSM_DTMform.BitBtn6Click(Sender: TObject);
var
   DSMname,DTMname : PathStr;
   AreaName : shortstring;
begin
   AreaName := GISdb[DB].MyData.GetFieldByNameAsString('AREA');
   DSMName := ExtractFilePath(GISdb[DB].DBFullName) + AreaName + '\' + GISdb[DB].MyData.GetFieldByNameAsString('COUNTRY') + '_' +
        GISdb[DB].MyData.GetFieldByNameAsString('DSM_NAME') + '_ref_test_dem\ref_dtm_srtm.tif';
   DTMName := ExtractFilePath(GISdb[DB].DBFullName) + AreaName + '\' + GISdb[DB].MyData.GetFieldByNameAsString('COUNTRY') + '_' +
        GISdb[DB].MyData.GetFieldByNameAsString('DTM_NAME') + '_ref_test_dem\ref_dtm_srtm.tif';
   DTM_1sec := OpenNewDEM(DTMName);
   DSM_1sec := OpenNewDEM(DSMName);
   if not ValidDEM(LCgrid) then LCGrid := LoadLC10LandCover('',DEMglb[DTM_1sec].SelectionMap.MapDraw.MapCorners.BoundBoxGeo,true);
end;


procedure TCompareDSM_DTMform.BitBtn7Click(Sender: TObject);

    procedure MakeSlopeMap(DEM : integer);
    begin
        if ValidDEM(DEM) then CreateEvansSlopeMapPercent(true,DEM);
    end;

begin
   MakeSlopeMap(DTM_1sec);
   MakeSlopeMap(DSM_1sec);
   MakeSlopeMap(HRDTM);
   MakeSlopeMap(HRDSM);
end;


procedure TCompareDSM_DTMform.BitBtn8Click(Sender: TObject);
begin
   GraphTwoParameterGridsByAverageSlopeAndResolution(db,DEMIXtileFieldName,ComparingCriteriaList);
end;



procedure TCompareDSM_DTMform.BitBtn9Click(Sender: TObject);
begin
   GraphTwoParameterGridsByAverageSlopeAndResolution(db,DEMIXtileFieldName,MakeStringListFromString(MDDef.DEMIX_SingleCriterion),true);
end;


procedure TCompareDSM_DTMform.GraphsCurrentRec;
var
   gr1,gr2 : tThisBaseGraph;
   TileStats : shortstring;
begin
   if DEMIX_scale_compare then begin
       GISdb[db].ApplyGISFilter('TILE=' + QuotedStr(TheDTMs.Strings[onRec]));
       TileStats := 'Slope=' + GISdb[db].MyData.GetFieldByNameAsString('AVG_SLOPE') + '% ' +
                    'Barren=' + GISdb[db].MyData.GetFieldByNameAsString('BARREN_PC') + '% ' +
                    'Forest=' + GISdb[db].MyData.GetFieldByNameAsString('FOREST_PC') + '%';
       gr1 := GraphDEMIX_CompareDSMandDTMslopes(DB,TheDTMs.Strings[onRec],TileStats);
       GISdb[db].ClearGISFilter;
   end
   else begin
       gr1 := GraphCompareDSMandDTMslopes(DB,TheDTMs.Strings[onRec]);
       gr2 := GraphDSMandDTMdifferences(DB,TheDTMs.Strings[OnRec],MDDef.DEMIX_SingleCriterion);
       if gr2 <> nil then begin
          gr2.Left := 1200;
          gr2.Top := 50;
       end;
   end;
   if gr1 <> nil then begin
      gr1.Left := 1;
      gr1.Top := 50;
   end;
end;


procedure TCompareDSM_DTMform.RadioGroup2Click(Sender: TObject);
begin
   MDDEF.DEMIX_UseMedian := (RadioGroup2.ItemIndex = 1);
end;

procedure TCompareDSM_DTMform.RadioGroup3Click(Sender: TObject);
begin
   MDDef.DEMIX_MultiGraphCommonScaling := RadioGroup3.ItemIndex = 0;
end;


end.
