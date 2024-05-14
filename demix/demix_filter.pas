unit demix_filter;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordFullDEMIX}
  //{$Define RecordDEMIXLoad}
  //{$Define RecordSSIMprep}
  {$Define RecordDEMIX}
  //{$Define TrackFUV}    //should probably use only when doing a single tile
  //{$Define RecordSSIMprepFull}
  //{$Define RecordDEMIXDiffMaps}
  //{$Define TrackOpenHandles}
  //{$Define RecordGridSave}
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

  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  StrUtils,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.Grids, Vcl.Graphics,Vcl.ExtCtrls,
  Petmar_types,DEMDefs,demix_definitions,Demix_control;

type
  TDemixFilterForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    LoadCurrentAreaBitBtn5: TBitBtn;
    CheckBox3: TCheckBox;
    LoadOneSecRefCheckBox: TCheckBox;
    CheckBox1: TCheckBox;
    ComboBox4: TComboBox;
    BitBtn4: TBitBtn;
    ComboBox3: TComboBox;
    ComboBox2: TComboBox;
    ComboBox1: TComboBox;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn11: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn13: TBitBtn;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    BitBtn14: TBitBtn;
    RadioGroup1: TRadioGroup;
    BitBtn15: TBitBtn;
    BitBtn16: TBitBtn;
    BitBtn17: TBitBtn;
    BitBtn18: TBitBtn;
    BitBtn19: TBitBtn;
    BitBtn20: TBitBtn;
    BitBtn21: TBitBtn;
    BitBtn22: TBitBtn;
    BitBtn23: TBitBtn;
    BitBtn24: TBitBtn;
    BitBtn25: TBitBtn;
    BitBtn26: TBitBtn;
    BitBtn27: TBitBtn;
    BitBtn28: TBitBtn;
    GroupBox7: TGroupBox;
    Label6: TLabel;
    Label5: TLabel;
    Label4: TLabel;
    Edit5: TEdit;
    Edit4: TEdit;
    Edit3: TEdit;
    BitBtn29: TBitBtn;
    BitBtn30: TBitBtn;
    BitBtn31: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn32: TBitBtn;
    BitBtn34: TBitBtn;
    BitBtn39: TBitBtn;
    CheckBox4: TCheckBox;
    CheckBox23: TCheckBox;
    TabSheet3: TTabSheet;
    Label2: TLabel;
    Label1: TLabel;
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
    BitBtn1: TBitBtn;
    GroupBox8: TGroupBox;
    Memo7: TMemo;
    RadioGroup2: TRadioGroup;
    Settings: TTabSheet;
    CheckBox2: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    threedembestrgm_checkbox: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    TabSheet4: TTabSheet;
    Label3: TLabel;
    AreaProgress: TEdit;
    TileProgress: TEdit;
    CurrentOperation: TEdit;
    Memo8: TMemo;
    TabSheet5: TTabSheet;
    BitBtn33: TBitBtn;
    BitBtn35: TBitBtn;
    BitBtn36: TBitBtn;
    BitBtn37: TBitBtn;
    //BitBtn6: TBitBtn;
    CheckBox18: TCheckBox;
    CheckBox16: TCheckBox;
    BitBtn38: TBitBtn;
    BitBtn40: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure LoadClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure BitBtn19Click(Sender: TObject);
    procedure BitBtn20Click(Sender: TObject);
    procedure BitBtn21Click(Sender: TObject);
    procedure BitBtn22Click(Sender: TObject);
    procedure BitBtn23Click(Sender: TObject);
    procedure BitBtn24Click(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure BitBtn25Click(Sender: TObject);
    procedure BitBtn26Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure BitBtn27Click(Sender: TObject);
    procedure threedembestrgm_checkboxClick(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure BitBtn28Click(Sender: TObject);
    procedure BitBtn29Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure CheckBox10Click(Sender: TObject);
    procedure BitBtn30Click(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure CheckBox11Click(Sender: TObject);
    procedure CheckBox16Click(Sender: TObject);
    procedure BitBtn31Click(Sender: TObject);
    procedure LoadCurrentAreaBitBtn5Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn32Click(Sender: TObject);
    procedure BitBtn33Click(Sender: TObject);
    procedure BitBtn34Click(Sender: TObject);
    procedure BitBtn35Click(Sender: TObject);
    procedure BitBtn36Click(Sender: TObject);
    procedure BitBtn37Click(Sender: TObject);
    procedure BitBtn39Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox23Click(Sender: TObject);
    procedure CheckBox18Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure LoadOneSecRefCheckBoxClick(Sender: TObject);
    procedure BitBtn38Click(Sender: TObject);
    procedure BitBtn40Click(Sender: TObject);
  private
    { Private declarations }
    procedure ZeroDEMs;
    procedure UncheckAllLoadCheckboxes;
    procedure MakeBigDiffferenceMapImage(Param : shortstring);
    procedure MakeDifferenceMaps(WhatType : integer);
    procedure DifferenceMapsAllAreas(WhatFor : integer);
    procedure GetUsingStringLists;
    procedure MakeGeomorphometricMaps(What : shortstring; var  DEMSwanted : tDEMbooleanArray);
  public
    { Public declarations }
    DB : integer;
    DEMsTypeUsing,
    TilesUsing,
    LandTypesUsing,
    CriteriaUsing,
    TileParameters,
    CandidateDEMsUsing : tStringList;
    MergeDEMs,RefDEMsHalfSec,DiffDSMDEMs,DiffDTMDEMs : tDEMIXindexes;
    function LoadDEMsForCurrentArea(AreaName : Petmar_types.shortstring; LoadRefMaps,LoadTestDEMmaps : boolean) : boolean;
  end;


procedure DoDEMIXFilter(DB : integer);


implementation

{$R *.dfm}

uses
   Petmar,PetMath,PetImage,PetImage_form,BaseGraf,
   DEMDatabase,DEMDbTable,DEMdef_routines,DEMcoord,
   DEM_Manager,DEMstat,BaseMap,DEMlosw,Make_Grid,
   DEMmapf, nevadia_main, PetDBUtils,
   DEMIX_Graphs;

const
   SaveDifferenceDistribution : boolean = false;

const
   ssimOneArea = 1;
   ssimAllAreas = 2;
   ssimDiluvium = 3;
var
   SSIMOutPath : PathStr;
   LoadRefDEMMaps,LoadTestDEMMaps : boolean;

(*
procedure TDemixFilterForm.SSIMComputations(Option : integer);
var
   Correlations,TileFill : tStringList;
   i,j : integer;
   AreaName : shortstring;
   fName : PathStr;

      procedure ProcessOneArea(AreaName : shortString; Correlations : tStringList; TileFill : TstringList = nil);
      var
         I : integer;
         AllTiles : tStringList;
      begin
         begin
            AllTiles := Nil;
            for i := 1 to MaxDemixDEM do begin
               if ValidDEM(RefDEMs[i]) then begin
                  if not StrUtils.AnsiContainsText(DEMGlb[RefDEMs[i]].AreaName,'_dsm') then begin //currently only doing for DTMs
                     if StrUtils.AnsiContainsText(DEMGlb[RefDEMs[i]].AreaName,'1sec') and StrUtils.AnsiContainsText(DEMGlb[RefDEMs[i]].AreaName,'_point') then begin
                        AllTiles := DEMIXtilesOnDEM(RefDEMs[i],TileFill);
                        TileFill.SaveToFile(MDtempDir + 'tilefill.csv');
                        {$If Defined(RecordSSIMprep)} WriteLineToDebugFile('Tiles in dtm_ref=' + IntToStr(AllTiles.Count)); {$EndIf}
                     end;
                  end;
               end;
            end;
            if (AllTiles = nil) then Memo8.Lines.Add('Incorrect ref DEMs for ' + ComboBox4.Text)
            else SSIMprep(AreaName,'Area ' + AreaName, AllTiles,Correlations);
            {$If Defined(RecordSSIMprepFull)} Correlations.SaveToFile('c:\temp\ssim\' + ComboBox4.Text + '.csv'); {$EndIf}
         end;
         BitBtn8Click(Nil);
      end;

      procedure DiluviumOption;
      var
         fName : PathStr;
         Areas : tStringList;
         //Area  : shortstring;
         db : TMyData;
         i{,j} : integer;
         OpenMaps : boolean;
      begin
         {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.BitBtn39Click in'); {$EndIf}
         OpenMaps := false;
         fName := DEMIXSettingsDir + 'tiles_compare_diluvium.dbf';
         if FileExists(fName) then begin
            db := TMyData.Create(fName);
            Areas := db.UniqueEntriesInDB('AREA');
            db.Destroy;
         end
         else begin
            fName := DEMIXSettingsDir + 'areas_diluvium_dem.txt';
            if FileExists(fName) then begin
               Areas := tStringList.Create;
               Areas.LoadFromFile(fName);
            end
            else begin
               MessageToContinue('Missing list of areas');
               exit;
            end;
         end;
         for i := 0 to pred(Areas.Count) do begin
            {$IfDef RecordDEMIX} WriteLineToDebugFile('Area=' + IntToStr(i) + '  ' + Areas[i]); {$EndIf}
            AreaName := Areas[i];
            AreaProgress.Text := IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + '  ' + AreaName;
            if LoadDEMsForCurrentArea(AreaName,DEMIXOpenMap,DEMIXOpenMap) then begin
               //MaskAllDEMsToDiluvium;
               ProcessOneArea(AreaName,Correlations,TileFill);
            end;
         end;
         Areas.Destroy;
         {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.BitBtn39Click in'); {$EndIf}
      end;


begin
   try
      Correlations := tStringList.Create;
      HeavyDutyProcessing := true;
      PageControl1.ActivePage := TabSheet4;
      Label3.Caption := 'Processing start: ' + TimeToStr(now);
      SSIMOutPath := 'G:\wine_contest\ssim_map_analysis\';
      FindDriveWithPath(SSIMOutPath);
      TileFill := tStringList.Create;
      TileFill.Add('DEMIX_TILE,PC_FULL');
      if (Option = ssimOneArea) then begin
         {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.BitBtn5, SSIM for one area'); {$EndIf}
         AreaName := ComboBox4.Text;
         AreaProgress.Text := AreaName;
         if LoadDEMsForCurrentArea(AreaName,false,false) then ProcessOneArea(ComboBox4.Text,Correlations,TileFill);
      end
      else if (Option = ssimAllAreas) then begin
         {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.BitBtn5, SSIM for all areas'); {$EndIf}
         for j := 0 to pred(ComboBox4.Items.Count) do begin
            ComboBox4.Text := ComboBox4.Items[j];
            AreaName := ComboBox4.Text;
            AreaProgress.Text := IntToStr(succ(j)) + '/' + IntToStr(ComboBox4.Items.Count) + '  ' + AreaName;
            if LoadDEMsForCurrentArea(AreaName,false,false) then ProcessOneArea(ComboBox4.Text,Correlations,TileFill);
         end;
      end
      else begin
         SSIMOutPath := 'G:\wine_contest\ssim_diluvium_map_analysis\';
         FindDriveWithPath(SSIMOutPath);
         DiluviumOption;
      end;

      if (TileFill <> Nil) then begin
         fName := NextFileNumber(SSIMOutPath,'demix_tile_fill_','.dbf');
         StringList2CSVtoDB(TileFill,fName);
         {$IfDef RecordDEMIX} WriteLineToDebugFile('TileFill=' + fName); {$EndIf}
      end;

      fName := NextFileNumber(SSIMOutPath,'FUV_grid_correlations_','.dbf');
      StringList2CSVtoDB(Correlations,fName);
      {$IfDef RecordDEMIX} WriteLineToDebugFile('Correlations=' + fName); {$EndIf}
   finally
      HeavyDutyProcessing := false;
      PageControl1.ActivePage := TabSheet1;
      AreaProgress.Text := '';
      TileProgress.Text := '';
      CurrentOperation.Text := '';
   end;
end;
*)


procedure ExpandCumulativeRangeForThisGrid(DEM : integer; var Min,Max : float32);
begin
   if (DEMGlb[DEM].DEMheader.MaxElev > Max) then Max := DEMGlb[DEM].DEMheader.MaxElev;
   if (DEMGlb[DEM].DEMheader.MinElev < Min) then Min := DEMGlb[DEM].DEMheader.MinElev;
end;


procedure RescaleDEMs(DEMsWanted : tDEMbooleanArray);
   //this moves all RRI maps to same color scale, but does not work very well
var
   Max,Min : float32;
   i : integer;
   Maps : tstringlist;
begin
   Max := -9999;
   Min := 9999;
   for i := 1 to MaxDemixDEM do begin
      if DEMsWanted[i] then ExpandCumulativeRangeForThisGrid(i, Min,Max);
   end;
   for i := 1 to MaxDemixDEM do begin
      if DEMsWanted[i] then if (DEMGlb[i].SelectionMap <> Nil) then DEMGlb[i].SelectionMap .RecolorMapWithElevationRange(Min,Max);
   end;
   PickMapsFromDEMsWanted(Maps,DEMSwanted);
   Bigimagewithallmaps(3,'',Maps);
end;


procedure ZeroMapIndexes;
var
   i : integer;
begin
   for i := 1 to MaxDemixDEM do begin
      SlopeMap[i] := 0;
      TestRuffMap[i] := 0;
      TestRRI[i] := 0;
      TestTPI[i] := 0;
      TestHillshade[i] := 0;
      RefSlopeMap[i] := 0;
      RefRuffMap[i] := 0;
      RefRRI[i] := 0;
      RefTPI[i] := 0;
      RefHillshade[i] := 0;
   end;
end;


procedure LoadComboBoxFromDBField(db : integer; ComboBox : tComboBox; aField : shortstring);
begin
   GISdb[DB].EmpSource.Enabled := false;
   ComboBox.Items := GISdb[db].MyData.ListUniqueEntriesInDB(aField);
   ComboBox.ItemIndex := 0;
   GISdb[DB].EmpSource.Enabled := true;
end;


procedure DoDEMIXFilter(DB : integer);
var
  DemixFilterForm: TDemixFilterForm;
begin
   {$If Defined(RecordDEMIX) or Defined(TrackOpenHandles)} WriteOpenHandlestoDebugLog('DoDEMIXFilter in'); {$EndIf}
   GetDEMIXpaths(false);
   DemixFilterForm := TDemixFilterForm.Create(Application);
   DemixFilterForm.db := db;
   DEMIXFilterForm.Caption := 'DEMIX DB: ' + GISdb[db].dbName;
   DemixFilterForm.ZeroDEMs;

   if false then begin
      LoadComboBoxFromDBField(db,DemixFilterForm.ComboBox1,'DEMIX_TILE');
      LoadComboBoxFromDBField(db,DemixFilterForm.ComboBox4,'AREA');
   end
   else begin
      DemixFilterForm.ComboBox1.Items.LoadFromFile(DEMIXSettingsDir + 'tiles_list.txt');
      DemixFilterForm.ComboBox4.Items.LoadFromFile(AreaListFName);
   end;

   DemixFilterForm.ComboBox1.Text := MDDef.DEMIX_default_tile;
   DemixFilterForm.ComboBox4.Text := MDDef.DEMIX_default_area;

   DemixFilterForm.Show;
   {$If Defined(RecordDEMIX) or Defined(TrackOpenHandles)} WriteOpenHandlestoDebugLog('DoDEMIXFilter out'); {$EndIf}
end;


const
   ElevSpecified = 1;   //BitBtn7;
   ElevAll       = 2;   //BitBtn10;
   //ElevV1        = 3;   //BitBtn11;
   SlopeAll      = 4;   //BitBtn20;
   RuffAll       = 5;   //BitBtn21;
   AllAll        = 6;


procedure TDemixFilterForm.MakeDifferenceMaps(WhatType : integer);
var
   i,j,k : integer;
   DEMarea : ANSIString;
   Param : shortString;

   procedure GetRefDEMDifferenceMap(DiffType,aTestDEM : integer; DEMArea,RefPointOrArea,theDEMtype,TestDEMseriesName : shortstring);
   var
      i,refDEMsurfaceType: integer;
      refDEMname : shortString;

      function MakeTheMap(i : integer) : integer;
      var
        SlopeTest,SlopeRef,RuffTest,RuffRef,BackgroundGrid,DEM1,DEM2 : integer;
      begin
         BackgroundGrid := RefDEMs[i];
         SlopeRef := 0;
         SlopeTest := 0;
         RuffTest := 0;
         RuffRef := 0;
         if (DiffType = SlopeAll) then begin
            SlopeTest := CreateSlopeMap(TestDEMs[aTestDEM],false);
            SlopeRef := CreateSlopeMap(RefDEMs[i],false);
            DEM1 := SlopeTest;
            DEM2 := SlopeRef;
            Param := 'slope';
         end
         else if (DiffType = RuffAll) then begin
            RuffTest := CreateSlopeRoughnessSlopeStandardDeviationMap(TestDEMs[aTestDEM],5,SlopeTest,false);
            RuffRef := CreateSlopeRoughnessSlopeStandardDeviationMap(RefDEMs[i],5,SlopeRef,false);
            DEM1 := RuffTest;
            DEM2 := RuffRef;
            Param := 'roughness';
         end
         else begin
            DEM1 := TestDEMs[aTestDEM];
            DEM2 := RefDEMs[i];
            Param := 'elev';
         end;
         {$IfDef RecordDEMIX} WriteLineToDebugFile('test DEM=' + DEMGlb[DEM1].AreaName + ' ' + DEMGlb[DEM1].zRange + ' ref DEM=' + DEMGlb[DEM2].AreaName + ' ' + DEMGlb[DEM2].zRange); {$EndIf}
         Result := MakeDifferenceMap(DEM1,DEM2,RefDEMs[i],BackgroundGrid,true,false,false,TestDEMseriesName + '_' + Param + '_Delta_to_Reference_' + theDEMtype);  //DEMGlb[DEM2].AreaName);
         CloseSingleDEM(SlopeRef);
         CloseSingleDEM(SlopeTest);
         CloseSingleDEM(RuffRef);
         CloseSingleDEM(RuffTest);
      end;


   begin
      if ValidDEM(aTestDEM) then begin
         {$IfDef TrackOpenHandles} WriteOpenHandlestoDebugLog('GetRefDEMDifferenceMap in'); {$EndIf}
         for I := 1 to MaxDEMIXDEM do begin
            if ValidDEM(RefDEMs[i]) then begin
               refDEMname := UpperCase(DEMGlb[RefDEMs[i]].AreaName);
               refDEMsurfaceType := IsDEMaDSMorDTM(refDEMname);
               if StrUtils.AnsiContainsText(refDEMname,RefPointOrArea) then begin
                  if (refDEMSurfaceType = DEMisDSM) and (theDEMtype = 'DSM') then begin
                     DiffDSMDEMs[aTestDEM] := MakeTheMap(i);
                  end
                  else if (refDEMSurfaceType = DEMisDTM) and (theDEMtype = 'DTM') then begin
                     DiffDTMDEMs[aTestDEM] := MakeTheMap(i);
                  end;
               end;
            end;
         end;
         {$IfDef TrackOpenHandles} WriteOpenHandlestoDebugLog('GetRefDEMDifferenceMap out'); {$EndIf}
      end;
   end;

var
   theRefDEMs : tDEMIXindexes;
   RefPointOrArea,SeriesName,AreaName : shortstring;
begin
   {$If Defined(RecordDEMIX) or Defined(TrackOpenHandles)} WriteOpenHandlestoDebugLog('TDemixFilterForm.MakeDifferenceMaps in, type=' + IntToStr(WhatType)); {$EndIf}
   AreaName := ComboBox4.Text;
   LoadDEMsForCurrentArea(AreaName,true,true);  //needs hillshade maps for background on difference maps
   SaveBackupDefaults;
   MDDef.HighlightDiffMap := 1;
   MDDef.ScaleBarLocation.DrawItem := true;
   MDDef.ScaleBarLocation.MapPosition := lpNEMap;
   MDDef.MapNameLocation.DrawItem := true;
   MDDef.MapNameLocation.MapPosition := lpSMap;
   MDDef.GridLegendLocation.DrawItem := true;
   MDDef.GridLegendLocation.MapPosition := lpNWMap;

   theRefDEMs := RefDEMs;
   for j := 2 downto 1 do begin
      for i := 1 to MaxDEMIXDEM do begin
         //this will not work yet for high latitude areas
         if ValidDEM(TestDEMs[i]) then begin
            //wmdem.SetPanelText(1,'i=' + IntToStr(i) + '  j=' + IntToStr(j));
            DEMArea := UpperCase(DEMGlb[TestDEMs[i]].AreaName);
            for k := 1 to NumDEMIXtestDEM do if StrUtils.AnsiContainsText(DEMArea,DEMIXDEMTypeName[k]) then SeriesName := DEMIXDEMTypeName[k];
            if StrUtils.AnsiContainsText(DEMArea,'ALOS') then RefPointOrArea := 'AREA' else RefPointOrArea := 'POINT';
            if (WhatType = AllAll) then begin
               GetRefDEMDifferenceMap(ElevAll,i,DEMArea,RefPointOrArea,RefDEMType[j],SeriesName);
               GetRefDEMDifferenceMap(SlopeAll,i,DEMArea,RefPointOrArea,RefDEMType[j],SeriesName);
               GetRefDEMDifferenceMap(RuffAll,i,DEMArea,RefPointOrArea,RefDEMType[j],SeriesName);
            end
            else GetRefDEMDifferenceMap(WhatType,i,DEMArea,RefPointOrArea,RefDEMType[j],SeriesName);
         end;
      end;
   end;

   MakeBigDiffferenceMapImage(Param);
   RestoreBackupDefaults;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.BitBtn7Click out, DEMs=' + IntToStr(NumDEMDataSetsOpen) + '  Maps=' + IntToStr(NumOpenMaps)); {$EndIf}
end;



procedure TDemixFilterForm.RadioGroup2Click(Sender: TObject);
begin
   Memo1.Enabled := RadioGroup2.ItemIndex in [0,1,2];    //DEMsTypeUsing
   Memo2.Enabled := RadioGroup2.ItemIndex in [99];       //TilesUsing
   Memo3.Enabled := RadioGroup2.ItemIndex in [1];        //LandTypesUsing
   Memo4.Enabled := RadioGroup2.ItemIndex in [0,2];        //CriteriaUsing
   Memo5.Enabled := RadioGroup2.ItemIndex in [99];       //CandidateDEMsUsing
   Memo7.Enabled := RadioGroup2.ItemIndex in [0,1,2];      //TileParameters
end;

procedure TDemixFilterForm.threedembestrgm_checkboxClick(Sender: TObject);
begin
   MDDef.MakeRGB_Best_Map := threedembestrgm_checkbox.Checked;
end;

procedure TDemixFilterForm.UncheckAllLoadCheckboxes;
begin
   CheckBox1.Checked := false;
   LoadOneSecRefCheckBox.Checked := false;
   CheckBox3.Checked := false;
   //CheckBox4.Checked := false;
   CheckBox5.Checked := false;
   CheckBox6.Checked := false;
end;


procedure TDemixFilterForm.MakeBigDiffferenceMapImage(Param : shortstring);
var
   i : integer;
   theFiles : tStringList;
   fName : PathStr;
begin
   theFiles := tStringList.Create;
   for I := 1 to MaxDEMIXDEM do begin
      if ValidDEM(DiffDSMDEMs[i]) then begin
         fName := NextFileNumber(MDtempdir,'diff_map_','.bmp');
         SaveImageAsBMP(DEMGlb[DiffDSMDEMs[i]].SelectionMap.Image1,fName);
         theFiles.Add(fName);
      end;
   end;
   for I := 1 to MaxDEMIXDEM do begin
      if ValidDEM(DiffDTMDEMs[i]) then begin
         fName := NextFileNumber(MDtempdir,'diff_map_','.bmp');
         SaveImageAsBMP(DEMGlb[DiffDTMDEMs[i]].SelectionMap.Image1,fName);
         theFiles.Add(fName);
      end;
   end;
   fName := NextFileNumber(DEMIX_diff_maps_dir,Param + '_' + ComboBox4.Text + '_difference_maps_','.png');
   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.MakeBigDiffferenceMapImage ' + fName + '  n=' + IntToStr(theFiles.Count)); {$EndIf}
   MakeBigBitmap(theFiles,'Difference maps',fName,3);
end;

procedure TDemixFilterForm.BitBtn20Click(Sender: TObject);
begin
   MakeDifferenceMaps(SlopeAll);
end;

procedure TDemixFilterForm.BitBtn21Click(Sender: TObject);
begin
   MakeDifferenceMaps(RuffAll);
end;

procedure TDemixFilterForm.BitBtn22Click(Sender: TObject);
begin
   DifferenceMapsAllAreas(SlopeAll);
end;

procedure TDemixFilterForm.BitBtn23Click(Sender: TObject);
begin
   DifferenceMapsAllAreas(RuffAll);
end;


procedure TDemixFilterForm.BitBtn24Click(Sender: TObject);
begin
   LoadComboBoxFromDBField(db, ComboBox1,'DEMIX_TILE');
end;


procedure TDemixFilterForm.BitBtn25Click(Sender: TObject);
var
   i : integer;
   Results : tStringList;
   OutName : PathStr;
   DEMType : shortstring;

      procedure DoArea;
      var
         AreaName : PathStr;
         Table : tMyData;
         Sum : float64;
         aLine : shortstring;
         RuffRef,SlopeRef,RuffALOS,SlopeALOS,RuffCOP,SlopeCOP,
         i,j,COPDEM,ALOSDEM : integer;
         BestElevGrid,BestSlopeGrid,BestRuffGrid : tDEMIXindexes;


         procedure AddResults(What : shortString; fName : PathStr);
         begin
            Table := tMyData.Create(fName);
            Sum := Table.FieldSum('N');
            aline := AreaName + ',' + What  + ',' + DEMtype;
            Table.First;
            while not Table.eof do begin
               aline := aline + ',' + RealToString(Table.GetFieldByNameAsInteger('N') * 100 / Sum, -12,-2);
               Table.Next;
            end;
            Results.Add(aline);
            {$IfDef RecordDEMIX} WriteLineToDebugFile(aline); {$EndIf}
            Table.Destroy;
         end;

      begin
         UncheckAllLoadCheckboxes;
         CheckBox5.Checked := true;
         CheckBox6.Checked := true;
         AreaName := ComboBox4.Text;
         LoadDEMsForCurrentArea(AreaName,false,false);

         COPDEM := 0;
         ALOSDEM := 0;
         for j := 1 to MaxDemixDEM do if ValidDEM(TestDEMs[j]) then begin
            if StrUtils.AnsiContainsText(UpperCase(DEMglb[TestDEMs[j]].AreaName),'COP') then COPDEM := TestDEMs[j];
            if StrUtils.AnsiContainsText(UpperCase(DEMglb[TestDEMs[j]].AreaName),'ALOS') then ALOSDEM := TestDEMs[j];
         end;

         for i := 1 to MaxDemixDEM do begin
            if ValidDEM(RefDEMsHalfSec[i]) then begin
               if IsDEMaDSMorDTM(DEMglb[RefDEMsHalfSec[i]].AreaName) = DEMisDSM then DEMtype := 'DSM' else DEMType := 'DTM';
               SlopeRef := 0; //to force return of the slope map in addition to the ruff map
               RuffRef := CreateSlopeRoughnessSlopeStandardDeviationMap(RefDEMsHalfSec[i],5,SlopeRef,false);
               SlopeCOP := 0;
               RuffCOP := CreateSlopeRoughnessSlopeStandardDeviationMap(COPDEM,5,SlopeCOP,false);
               SlopeALOS := 0;
               RuffALOS := CreateSlopeRoughnessSlopeStandardDeviationMap(ALOSDEM,5,SlopeALOS,false);

               BestElevGrid[i] := TwoDEMHighLowMap(RefDEMsHalfSec[i],ALOSDEM,COPDEM,MDDef.DEMIXSimpleTolerance,false,'elev_alos_cop_high_low_ref_' + AreaName,false);
               AddResults('Elevation',DEMGlb[BestElevGrid[i]].VATFileName);
               BestSlopeGrid[i] := TwoDEMHighLowMap(SlopeRef,SlopeALOS,SlopeCOP,MDDef.DEMIXSlopeTolerance,false,'slope_alos_cop_high_low_ref_' + AreaName,false);
               AddResults('Slope',DEMGlb[BestSlopeGrid[i]].VATFileName);
               BestRuffGrid[i] := TwoDEMHighLowMap(RuffRef,RuffALOS,RuffCOP,MDDef.DEMIXRuffTolerance,false,'ruff_alos_cop_high_low_ref_' + AreaName,false);
               AddResults('Roughness',DEMGlb[BestRuffGrid[i]].VATFileName);
            end;
         end;
      end;


begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.BitBtn25Click in'); {$EndIf}
   try
      HeavyDutyProcessing := true;
      Results := tStringList.Create;
      Results.Add('AREA,PARAMETER,DEM,BOTH_HIGH,HIGH_GOOD,HIGH_LOW,GOOD_HIGH,BOTH_GOOD,GOOD_LOW,LOW_HIGH,LOW_GOOD,BOTH_LOW');

      for i := 0 to pred(ComboBox4.Items.Count) do begin
         wmdem.SetPanelText(2,IntToStr(succ(i)) + '/' + IntToStr(ComboBox4.Items.Count));
         ComboBox4.Text := ComboBox4.Items[i];
         wmdem.SetPanelText(3,ComboBox4.Text);
         DoArea;
         CloseAllDEMs;
      end;
   finally
      OutName := NextFileNumber(MDTempDir,'cop-alos-compare_','.dbf');
      PetdbUtils.StringList2CSVtoDB(Results,OutName);
      HeavyDutyProcessing := false;
      ShowDefaultCursor;
   end;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.BitBtn25Click out'); {$EndIf}
end;


procedure TDemixFilterForm.BitBtn26Click(Sender: TObject);
begin
   GISDB[DB].DisplayTable;
   GISdb[db].dbtablef.DEMIXPopUpMenu1.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure TDemixFilterForm.BitBtn27Click(Sender: TObject);
begin
   MakeDifferenceMaps(AllAll);
end;

procedure TDemixFilterForm.BitBtn28Click(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(ComboBox4.Items.Count) do begin
      wmdem.SetPanelText(2,IntToStr(succ(i)) + '/' + IntToStr(ComboBox4.Items.Count));
      ComboBox4.Text := ComboBox4.Items[i];
      ComboBox4.ItemIndex := i;
      if not FileExists(DEMIX_distrib_graph_dir + ComboBox1.Text + '_difference_distrib_graphs_1.png') then begin
         BitBtn14Click(Sender);
      end;
   end;
end;

procedure TDemixFilterForm.BitBtn29Click(Sender: TObject);
begin
   MDDef.TopCutLevel := 0.5;
   MDDef.DEMIXSlopeTolerance := 0.5;
   MDDef.DEMIXRuffTolerance := 0.2;
   FormCreate(nil);
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


procedure TDemixFilterForm.BitBtn30Click(Sender: TObject);
begin
   //SSIMComputations(ssimOneArea);
end;


procedure TDemixFilterForm.BitBtn31Click(Sender: TObject);
begin
(*var
   AllTiles, Correlations : tStringList;
   fName : PathStr;
   AreaName : shortstring;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.BitBtn31Click in (single tile SSIM) ' + ComboBox1.Text); {$EndIf}
   AreaName := GetAreaNameForDEMIXTile(DB, ComboBox1.Text);
   if (AreaName <> '') then begin
      HeavyDutyProcessing := true;
      PageControl1.ActivePage := TabSheet4;
      Label3.Caption := 'Processing start: ' + TimeToStr(now);
      ComboBox4.Text := AreaName;
      if LoadDEMsForCurrentArea(AreaName,false,false) then begin
         AllTiles := tStringList.Create;
         AllTiles.Add(ComboBox1.Text);
         AreaProgress.Text := AreaName;

         Correlations := tStringList.Create;
         SSIMprep(AreaName,'Single tile ' + ComboBox1.Text, AllTiles,Correlations);
         fName := NextFileNumber('c:\temp\ssim\','FUV_grid_correlations_','.dbf');
         StringList2CSVtoDB(Correlations,fName);
         {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.BitBtn31Click out (single tile SSIM)'); {$EndIf}
      end;
      HeavyDutyProcessing := false;
      PageControl1.ActivePage := TabSheet1;
      AreaProgress.Text := '';
      TileProgress.Text := '';
      CurrentOperation.Text := '';
   end;
   *)
end;


procedure TDemixFilterForm.BitBtn32Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := DEMIXSettingsDir;
   if GetExistingFileName('DEMIX areas','*.txt',fName) then begin
      ComboBox4.Items.LoadFromFile(fName);
      ComboBox4.ItemIndex := 0;
   end;
end;


procedure TDemixFilterForm.BitBtn33Click(Sender: TObject);
var
   i,UseDSM,UseDTM : integer;
   DiffGrid : tDEMIXindexes;
   DEMSwanted : tDEMbooleanArray;
   GridLimits: tGridLimits;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.BitBtn33Click in'); {$EndIf}
   MakeGeomorphometricMaps('TPI',DEMSwanted);


exit;

   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.BitBtn33Click TPI done'); {$EndIf}
   InitializeDEMsWanted(DEMsWanted,false);
   for I := 1 to MaxDEMIXDEM do begin
      if ValidDEM(TestDEMs[i]) then begin
         GetReferenceDEMsForTestDEM(TestSeries[i],UseDSM,UseDTM);
         DiffGrid[i] := MakeDifferenceMap(RefTPI[UseDTM],TestTPI[i],TestTPI[i],0,true,false,false);
         GridLimits := DEMGlb[TestTPI[i]].sfBoundBox2tGridLimits(DEMIXtileBoundingBox(ComboBox1.Text));
         DiffGrid[i] := MakeDifferenceMapOfBoxRegion(RefTPI[UseDTM],TestTPI[i],TestTPI[i],0,GridLimits,true,false,false);
         DEMsWanted[DiffGrid[i]] := true;
      end;
   end;
   CreateGridHistograms(DEMSwanted);
   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.BitBtn33Click out'); {$EndIf}
end;


procedure TDemixFilterForm.BitBtn34Click(Sender: TObject);
var
   AreaName,TileName : shortstring;
begin
   if ValidDB(DB) then begin
      TileName := ComboBox1.Text;
      AreaName := GetAreaNameForDEMIXTile(DB,TileName);
      if (AreaName = '') then MessageToContinue('No data for DEMIX tile')
      else begin
         LoadThisDEMIXTile(AreaName,TileName);
         ComboBox4.Text := AreaName;
         BitBtn34.Enabled := false;
      end;
   end
   else MessageToContinue('No valid DB');
end;


procedure TDemixFilterForm.MakeGeomorphometricMaps(What : shortstring; var  DEMSwanted : tDEMbooleanArray);
var
   i,ng,ThisRefDEM : integer;
   GridLimits: tGridLimits;
   bb : sfBoundBox;
   fName : PathStr;
   Bitmap : tMyBitmap;
   Maps,AllGraphs : tStringList;
   Graphs : array[1..12] of tThisBaseGraph;
begin
   {$IfDef RecordGeomorphMaps} WriteLineToDebugFile('TDemixFilterForm.MakeGeomorphometricMaps in, ' + What); {$EndIf}
   InitializeDEMsWanted(DEMsWanted,false);
   bb := DEMIXtileBoundingBox(ComboBox1.Text);

   if (UpperCase(What) = 'ELEV') then begin
      for i := 1 to MaxDemixDEM do begin
         if ValidDEM(TestDEMs[i]) then begin
            DEMSWanted[TestDEMs[i]] := true;
            //DEMGlb[TestDEMs[i]].SelectionMap.DoFastMapRedraw;
         end;
         if ValidDEM(RefDEMs[i]) then begin
            DEMSWanted[RefDEMs[i]] := true;
            //DEMGlb[RefDEMs[i]].SelectionMap.DoFastMapRedraw;
         end;
      end;
   end
   else begin
      //make reference DEM grids
      for i := 1 to MaxDemixDEM do begin
         if ValidDEM(UsingRefDEMs[i]) then begin
            GridLimits := DEMGlb[UsingRefDEMs[i]].sfBoundBox2tGridLimits(DEMIXtileBoundingBox(ComboBox1.Text));
            if What = 'RRI' then begin
               RefRRI[i] := MakeSpecifiedTPIGrid(UsingRefDEMs[i],GridLimits,nmRRI,true);
               DEMGlb[RefRRI[i]].AreaName := DEMGlb[RefDEMs[i]].AreaName + '_RRI';
               DEMSWanted[RefRRI[i]] := true;
            end;
            if UpperCase(What) = 'RUFF' then begin
               RefRuffMap[i] := BoxCarDetrendDEM(true,RefDEMs[i],GridLimits,3);
               DEMGlb[RefRuffMap[i]].AreaName := DEMglb[RefDEMs[i]].AreaName + '_Ruff';
               DEMSWanted[RefRuffMap[i]] := true;
            end;
            if What = 'TPI' then begin
               RefTPI[i] := BoxCarDetrendDEM(true,RefDEMs[i],GridLimits,3);
               DEMGlb[RefTPI[i]].AreaName := DEMGlb[RefDEMs[i]].AreaName + '_TPI';
               DEMSWanted[RefTPI[i]] := true;
            end;
         end;
      end;

      //make test DEM grids
      for i := 1 to MaxDemixDEM do begin
         if ValidDEM(TestDEMs[i]) then begin
            GridLimits := DEMGlb[TestDEMs[i]].sfBoundBox2tGridLimits(DEMIXtileBoundingBox(ComboBox1.Text));
            if What = 'RRI' then begin
               TestRRI[i] := MakeSpecifiedTPIGrid(TestDEMs[i],GridLimits,nmRRI,true);
               DEMGlb[TestRRI[i]].AreaName := DEMGlb[TestDEMs[i]].AreaName + '_RRI';
               DEMSWanted[TestRRI[i]] := true;
            end;
            if UpperCase(What) = 'RUFF' then begin
               TestRuffMap[i] := BoxCarDetrendDEM(true,TestDEMs[i],GridLimits,3);
               DEMGlb[TestRuffMap[i]].AreaName := DEMGlb[TestDEMs[i]].AreaName + '_Ruff';
               DEMSWanted[TestRuffMap[i]] := true;
            end;

            if What = 'TPI' then begin
               TestTPI[i] := BoxCarDetrendDEM(true,TestDEMs[i],GridLimits,3);
               DEMGlb[TestTPI[i]].AreaName := DEMGlb[TestDEMs[i]].AreaName + '_TPI';
               DEMSWanted[TestTPI[i]] := true;
            end;
         end;
      end;
   end;

   AllGraphs := tStringList.Create;
   ng := 0;
   for i := 1 to MaxDEMDataSets do begin
      if DEMsWanted[i] then begin
         if (not StrUtils.AnsiContainsText(DEMGlb[i].AreaName,'ref')) then begin
            {$IfDef RecordGeomorphMaps} WriteLineToDebugFile('Scattergram for ' + DEMGlb[i].AreaName); {$EndIf}
            if What = 'RRI' then ThisRefDEM := GetReferenceDEMforTestDEM(i,RefRRI);
            if What = 'TPI' then ThisRefDEM := GetReferenceDEMforTestDEM(i,RefTPI);
            if UpperCase(What) = 'RUFF' then ThisRefDEM := GetReferenceDEMforTestDEM(i,RefRuffMap);
            if (UpperCase(What) = 'ELEV') then ThisRefDEM := GetReferenceDEMforTestDEM(i,RefDEMs);
            if ValidDEM(ThisRefDEM) then begin
               GridLimits := DEMGlb[i].sfBoundBox2tGridLimits(bb);
               inc(ng);
               Graphs[ng] := GridScatterGram(GridLimits,i,ThisRefDEM);

               CopyImageToBitmap(Graphs[ng].Image1,Bitmap);
               fName := NextFileNumber(MDtempDir,'scattergram_','.bmp');
               Bitmap.SaveToFile(fName);
               AllGraphs.Add(fName);
               Bitmap.Free;
            end
            else begin
               {$IfDef RecordGeomorphMaps} HighlightLineToDebugFile('No ref DEM for ' + DEMGlb[i].AreaName); {$EndIf}
            end;
         end;
      end;
   end;
   MakeBigBitmap(AllGraphs,'','',3);

   for i := 1 to MaxDEMDataSets do begin
      if DEMsWanted[i] and (DEMGlb[i].SelectionMap <> Nil) then begin
         DEMGlb[i].SelectionMap.ClipDEMtoregionwithdata1Click(nil);
         DEMGlb[i].SelectionMap.SubsetAndZoomMapFromGeographicBounds(bb);
      end;
   end;

   PickMapsFromDEMsWanted(Maps,DEMSwanted);
   BigImageWithAllMaps(4,'',Maps);

   if false {not working at the moment} then CreateGridHistograms(DEMSwanted);
   {$IfDef RecordGeomorphMaps} WriteLineToDebugFile('TDemixFilterForm.MakeGeomorphometricMaps out, ' + What); {$EndIf}
end;

procedure TDemixFilterForm.BitBtn35Click(Sender: TObject);
var
   DEMSwanted : tDEMbooleanArray;
begin
   MakeGeomorphometricMaps('RRI',DEMSwanted);
end;

procedure TDemixFilterForm.BitBtn36Click(Sender: TObject);
var
   DEMSwanted : tDEMbooleanArray;
begin
   MakeGeomorphometricMaps('Ruff',DEMSwanted);
end;


procedure TDemixFilterForm.BitBtn37Click(Sender: TObject);
var
   DEMSwanted : tDEMbooleanArray;
begin
   MakeGeomorphometricMaps('elev',DEMSwanted);
end;


procedure TDemixFilterForm.BitBtn38Click(Sender: TObject);
begin
   SaveMDdefaults;
end;

procedure TDemixFilterForm.BitBtn39Click(Sender: TObject);
begin
   //SSIMComputations(ssimDiluvium);
end;

(*
procedure TDemixFilterForm.SSIMprep(AreaName,WhatFor : shortstring; AllTiles : tStringList; GridCorrelations : tStringList);
const
   TileNormalize = false;
var
   OutPath : PathStr;

         procedure SaveGrids(TileName : shortstring; TheRefDEMs,TheTestDEMs : tDEMIXindexes; What : shortstring = '');
         var
            fName : PathStr;
            DEM : integer;
            bb,bb_pia : sfBoundBox;


               procedure SaveOneDEMindex(RefDEM : boolean; TheDEMs : tDEMIXindexes);
               var
                  aName : shortstring;
                  DEM : integer;
                  bg : tGridLimits;
               begin
                  for DEM := 1 to MaxDemixDEM do begin
                     if ValidDEM(TheDEMs[DEM]) then begin
                        aName := WhatTestDEMisThis(DEMGlb[TheDEMs[DEM]].AreaName);
                        if RefDEM then begin //this is a reference DEM
                           if StrUtils.AnsiContainsText(DEMGlb[TheDEMs[DEM]].AreaName,'1.5') then begin
                              aName := 'hi_lat_point_ref';
                           end
                           else begin
                              if StrUtils.AnsiContainsText(DEMGlb[TheDEMs[DEM]].AreaName,'dsm_ref') then aName := 'dsm_ref'
                              else aName := 'dtm_ref';
                              if StrUtils.AnsiContainsText(DEMGlb[TheDEMs[DEM]].AreaName,'point') then aName := aName + '_point'
                              else aName := aName + '_area';
                           end;
                        end;
                        if StrUtils.AnsiContainsText(DEMGlb[TheDEMs[DEM]].AreaName,'area') or StrUtils.AnsiContainsText(DEMGlb[TheDEMs[DEM]].AreaName,'ALOS') then begin
                           bg := DEMGlb[TheDEMs[DEM]].sfBoundBox2tGridLimits(bb_pia);
                           {$IfDef RecordSSIMprepFull} WriteLineToDebugFile(aName + ' PIA grid, ' + sfBoundBoxToString(bb_pia,8)); {$EndIf}
                        end
                        else begin
                           bg := DEMGlb[TheDEMs[DEM]].sfBoundBox2tGridLimits(bb);
                        end;

                        fName := OutPath + What +  '_' + aName + '_norm.tif';
                        if (succ(bg.XGridHigh - bg.XGridLow) > 100) and (succ(bg.YGridHigh - bg.YGridLow) > 100) then begin
                            {$If Defined(RecordSSIMprepFull) or Defined(RecordGridSave)} WriteLineToDebugFile('Save ' + fName + '  ' + GridLimitsToString(bg) + '  ' + GridLimistsSizeToString(bg)); {$EndIf}
                            DEMGlb[TheDEMs[DEM]].SaveGridSubsetGeotiff(bg,fName);
                        end
                        else begin
                            {$If Defined(RecordSSIMprepFull) or Defined(RecordGridSave)} WriteLineToDebugFile('Skip tile ' + fName + '  ' + GridLimitsToString(bg) + '  ' + GridLimistsSizeToString(bg)); {$EndIf}
                            DeleteFileIfExists(fName);
                        end;
                     end;
                  end;
               end;


         var
            OldSetting : boolean;
         begin
            {$If Defined(RecordSSIMprepFull) or Defined(RecordGridSave)} WriteLineToDebugFile('Save ' + What); {$EndIf}
            for DEM := 1 to MaxDemixDEM do begin
               if ValidDEM(TheRefDEMs[DEM]) then begin
                  if StrUtils.AnsiContainsText(DEMGlb[TheRefDEMs[DEM]].AreaName,'area') then begin
                     bb_pia := DEMIXtileBoundingBox(TileName,true);
                     {$IfDef RecordSSIMprepFull} WriteLineToDebugFile(TileName + ' bb:  ' + sfBoundBoxToString(bb_pia,8) + ' ' + sfBoundBoxSizeMinutesToString(bb_pia)); {$EndIf}
                     {$IfDef RecordSSIMprepFull} WriteLineToDebugFile('Area ref bb: ' + sfBoundBoxToString(DEMGlb[TheRefDEMs[DEM]].DEMBoundBoxGeo,8) + ' ' + sfBoundBoxSizeMinutesToString(DEMGlb[TheRefDEMs[DEM]].DEMBoundBoxGeo));                     {$EndIf}
                     bb_pia := IntersectionTwoGeoBoundBoxes(bb_pia,DEMGlb[TheRefDEMs[DEM]].DEMBoundBoxGeo);
                     {$IfDef RecordSSIMprepFull} WriteLineToDebugFile('Intersect bb:  ' + sfBoundBoxToString(bb_pia,8) + '  ' + sfBoundBoxSizeMinutesToString(bb_pia)); {$EndIf}
                     SafeAdjustGeoBoundBoxForPixelIsArea(bb_pia);
                     {$IfDef RecordSSIMprepFull} WriteLineToDebugFile('Area safe bb:  ' + sfBoundBoxToString(bb_pia,8) + '  '+ sfBoundBoxSizeMinutesToString(bb_pia)); {$EndIf}
                  end
                  else begin
                     //OK for either the point or hi-lat refernces
                     bb := IntersectionTwoGeoBoundBoxes(DEMIXtileBoundingBox(TileName),DEMGlb[TheRefDEMs[DEM]].DEMBoundBoxGeo);
                  end;
               end;
            end;
            try
               OldSetting := WantShowProgress;
               WantShowProgress := false;
               SkipMenuUpdating := true;
               if DEMsinIndex(TheTestDEMs) <> NumDEMIXtestDEM then begin
                  {$IfDef RecordSSIMprep} HighlightLineToDebugFile(TileName + ' ' + What + ' only TestDEMs=' + IntToStr(DEMsinIndex(TheTestDEMs))); {$EndIf}
               end;
               SaveOneDEMindex(false,TheTestDEMs);
               SaveOneDEMindex(true,TheRefDEMs);
            finally
               WantShowProgress := OldSetting;
               SkipMenuUpdating := false;
            end;
         end;


         procedure NormalizeGrids(TheDEMs,TheRefs : tDEMIXindexes; What : shortstring = '');
         var
            Min,Max : float32;
            fName : PathStr;
            aName : shortstring;
            DEM : integer;
            bg : tGridLimits;

                  procedure CycleToNormalize(WhichGrids : tDEMIXindexes);
                  var
                     DEM : integer;
                  begin
                     for DEM := 1 to MaxDemixDEM do begin
                        if ValidDEM(WhichGrids[DEM]) then begin
                           NormalizeDEMforSSIM(WhichGrids[DEM],What);
                        end;
                     end;
                  end;

         var
            OldSetting : boolean;
         begin
            {$IfDef RecordSSIMprepFull} WriteLineToDebugFile('Normalize ' + What + '  Refs=' + IntToStr(DEMsinIndex(TheRefs)) + '  Test=' + IntToStr(DEMsinIndex(TheDEMs))); {$EndIf}
            Min := 99e39;
            Max := -99e39;
            try
               OldSetting := WantShowProgress;
               WantShowProgress := false;
               SkipMenuUpdating := true;
               What := UpperCase(What);
               CycleToNormalize(TheDEMs);
               CycleToNormalize(TheRefs);
            finally
               WantShowProgress := OldSetting;
               SkipMenuUpdating := false;
            end;
            {$IfDef RecordSSIMprepFull} WriteLineToDebugFile('Done Normalize ' + What + '  range=' + RealToString(Min,-8,-2) + ' to ' + RealToString(Max,-8,-2)); {$EndIf}
         end;


      procedure MakeAreaGeomorphGrids;
      var
         i : integer;
      begin
         {$IfDef RecordSSIMprep} WriteLineToDebugFile('MakeAreaGeomorphGrids in'); {$EndIf}
         ZeroMapIndexes;
         for i := 1 to MaxDemixDEM do begin
            if ValidDEM(TestDEMs[i]) then begin
               TileProgress.Text := 'TEST DEM: ' + IntToStr(i) + '  ' + DEMGlb[TestDEMs[i]].AreaName;
               if MDDef.SSIM_hill then TestHillshade[i] := MakeSingleNewDerivativeMap('R',TestDEMs[i],0,false);
               if MDDef.SSIM_rri then TestRRI[i] := MakeTRIGrid(TestDEMs[i],nmRRI,false);
               if MDDef.SSIM_slope or MDDef.SSIM_ruff then TestRuffMap[i] := CreateSlopeRoughnessSlopeStandardDeviationMap(TestDEMs[i],5,SlopeMap[i],false);
               if MDDef.SSIM_tpi then TestTPI[i] := DEMGlb[TestDEMs[i]].BoxCarDetrendDEM(false,DEMGlb[TestDEMs[i]].FullDEMGridLimits,3);
            end;
         end;
         {$IfDef RecordSSIMprepFull} WriteLineToDebugFile('MakeAreaGeomorphGrids test grids done'); {$EndIf}
         for i := 1 to MaxDemixDEM do begin
            if ValidDEM(UsingRefDEMs[i]) then begin
               TileProgress.Text := 'Ref DEM: ' + IntToStr(i) + '  ' + DEMGlb[UsingRefDEMs[i]].AreaName;
               if MDDef.SSIM_tpi then RefTPI[i] := DEMGlb[UsingRefDEMs[i]].BoxCarDetrendDEM(false,DEMGlb[RefDEMs[i]].FullDEMGridLimits,3);
               if MDDef.SSIM_hill then RefHillshade[i] := MakeSingleNewDerivativeMap('R',UsingRefDEMs[i],0,false);
               if MDDef.SSIM_rri then RefRRI[i] := MakeTRIGrid(UsingRefDEMs[i],nmRRI,false);
               if MDDef.SSIM_slope or MDDef.SSIM_ruff then RefRuffMap[i] := CreateSlopeRoughnessSlopeStandardDeviationMap(UsingRefDEMs[i],5,RefSlopeMap[i],false);
            end;
         end;
         {$IfDef RecordSSIMprepFull} WriteLineToDebugFile('MakeAreaGeomorphGrids ref grids done'); {$EndIf}
         {$If Defined(RecordSSIMprepFull)} WriteLineToDebugFile('MakeAreaGeomorphGrids, testDEMs=' + IntToStr(DEMsinIndex(TestDEMs))); {$EndIf}
         {$If Defined(RecordSSIMprepFull)} WriteLineToDebugFile('MakeAreaGeomorphGrids, RRI grids=' + IntToStr(DEMsinIndex(TestRRI))); {$EndIf}
      end;

      procedure SaveGridsForOneTile(TileName : shortstring);
      begin
         {$If Defined(RecordSSIMprepFull) or Defined(RecordGridSave)} WriteLineToDebugFile(TileName); {$EndIf}
         {if TileNormalize then OutPath := 'c:\temp\ssim_tile_normalize\' + TileName + '\'
         else} OutPath := SSIMOutPath + TileName + '\';
         SafeMakeDir(OutPath);
         if MDDef.SSIM_hill then SaveGrids(TileName,RefHillshade,TestHillshade,'hill');
         if MDDef.SSIM_elev then SaveGrids(TileName,UsingRefDEMs,TestDEMs,'elev');
         if MDDef.SSIM_slope then SaveGrids(TileName,RefSlopeMap,SlopeMap,'slope');
         if MDDef.SSIM_ruff then SaveGrids(TileName,RefRuffMap,TestRuffMap,'ruff');
         if MDDef.SSIM_rri then SaveGrids(TileName,RefRRI,TestRRI,'RRI');
         if MDDef.SSIM_tpi then SaveGrids(TileName,RefTPI,TestTPI,'TPI');
      end;

      procedure GetGridCorrelations(TileName,What : shortstring; TestDEMs,RefDEMs : tDEMIXindexes);
      var
         i,j,ThisRefDEM,ThisTestDEM,DEMProcessed : integer;
         bb : sfBoundBox;
         bgRef,bgTest : tGridLimits;
         r,covar,Mean1,Mean2,StdDev1,StdDev2 : float64;
         Line1,Line2 : shortstring;
      begin
         ThisRefDEM := 0;
         ThisTestDEM := 0;
         Line1 := 'DEMIX_TILE,AREA,REF_TYPE,CRITERION';
         Line2 := TileName + ',' + AreaName + ',DTM,' + What + '_FUV';
         DEMProcessed := 0;
         for i := 1 to MaxDemixDEM do begin
            ThisTestDEM := TestDEMs[i];
            if ValidDEM(ThisTestDEM) then begin
               //wmdem.SetPanelText(1,What + ' correlations for test DEM=' + IntToStr(i));
               ThisRefDEM := GetReferenceDEMforTestDEM(ThisTestDEM,RefDEMs);
               if ValidDEM(ThisRefDEM) then begin
                  if StrUtils.AnsiContainsText(DEMGlb[ThisTestDEM].AreaName,'ALOS') then bb := DEMIXtileBoundingBox(TileName,true)
                  else bb := DEMIXtileBoundingBox(TileName);
                  bgTest := DEMGlb[ThisTestDEM].sfBoundBox2tGridLimits(bb);
                  if CovariancesFromTwoGrids(bgTest,ThisTestDEM,ThisRefDEM, r,covar,Mean1,Mean2,StdDev1,StdDev2) then begin
                     Line1 := Line1 + ',' + WhatTestDEMisThis(DEMGlb[ThisTestDEM].AreaName);
                     Line2 := Line2 + ',' + RealToString(sqr(r),-12,-6);
                     inc(DEMProcessed);
                     {$IfDef TrackFUV}
                        if StrUtils.AnsiContainsText(DEMGlb[ThisTestDEM].AreaName,'ALOS') then begin
                           WriteLineToDebugFile('Test=' + DEMGlb[ThisTestDEM].AreaName + '  Ref=' + DEMGlb[ThisRefDEM].AreaName + RealToString(r,12,6));
                        end;
                     {$EndIf}
                  end;
               end;
            end;
         end;
         if (DEMProcessed = NumDEMIXtestDEM) then begin
            if (GridCorrelations.Count = 0) then GridCorrelations.Add(Line1);
            GridCorrelations.Add(Line2);
         end
         else begin
            Memo3.Lines.Add('Processing error tile: ' + TileName);
         end;
         {$If Defined(RecordSSIMprepFull)} WriteLineToDebugFile(What + ' GetGridCorrelations, testDEMs=' + IntToStr(DEMsinIndex(TestDEMs))); {$EndIf}
      end;

var
   i,j : integer;
begin {TDemixFilterForm.SSIMprep}
   if (AllTiles = Nil) or (AllTiles.Count = 0) then begin
      exit;
   end;
   {$If Defined(RecordDEMIX) or Defined(RecordSSIMprep)} WriteLineToDebugFile('SSIMprep in'); {$EndIf}
   try
      SaveBackupDefaults;
      MDDef.MultShadeReliefMode := mhsEightFixed;
      if ValidDEM(RefDEMsHalfSec[1]) then begin
         UsingRefDEMs := RefDEMsHalfSec;
         for i := 1 to MaxDemixDEM do ReinterpolateTestDEMtoHalfSec(TestDEMs[i],true);
      end
      else UsingRefDEMs := RefDEMs;

      if (not PathIsValid(OutPath)) or AnswerIsYes(OutPath + ' exists; redo area') then begin
         CurrentOperation.Text := 'Make geomorph grids';
         MakeAreaGeomorphGrids;
         if (GridCorrelations <> Nil) then begin
            CurrentOperation.Text := 'Grid correlations ';
            for i := 0 to pred(AllTiles.Count) do begin
               TileProgress.Text := IntToStr(succ(i)) + '/' + IntToStr(AllTiles.Count) + '  ' + AllTiles[i];
               if MDDef.SSIM_elev  then GetGridCorrelations(AllTiles[i],'ELEV',TestDEMs,UsingRefDEMs);
               if MDDef.SSIM_slope then GetGridCorrelations(AllTiles[i],'SLOPE',SlopeMap,RefSlopeMap);
               if MDDef.SSIM_ruff  then GetGridCorrelations(AllTiles[i],'RUFF',TestRuffMap,RefRuffMap);
               if MDDef.SSIM_rri   then GetGridCorrelations(AllTiles[i],'RRI',TestRRI,RefRRI);
               if MDDef.SSIM_hill  then GetGridCorrelations(AllTiles[i],'HILL',TestHillshade,RefHillshade);
               if MDDef.SSIM_tpi   then GetGridCorrelations(AllTiles[i],'TPI',TestTPI,RefTPI);
            end;
         end;
         TileProgress.Text := '';
         {$If Defined(RecordSSIMprep)} WriteLineToDebugFile('SSIMprep start normalize grids'); {$EndIf}
         CurrentOperation.Text := 'Normalize grids';
         if MDDef.SSIM_elev then NormalizeGrids(TestDEMS,UsingRefDEMs,'Elev');
         if MDDef.SSIM_hill then NormalizeGrids(TestHillshade,RefHillshade,'Hill');
         if MDDef.SSIM_rri then NormalizeGrids(TestRRI,RefRRI,'RRI');
         if MDDef.SSIM_slope then NormalizeGrids(SlopeMap,RefSlopeMap,'Slope');
         if MDDef.SSIM_ruff then NormalizeGrids(TestRuffMap,RefRuffMap,'Ruff');
         if MDDef.SSIM_tpi then NormalizeGrids(TestTPI,RefTPI,'TPI');
         {$IfDef RecordSSIMprepFull} WriteLineToDebugFile('After Normalize elev ' + '  Refs=' + IntToStr(DEMsinIndex(UsingRefDEMs)) + '  Test=' + IntToStr(DEMsinIndex(TestDEMS))); {$EndIf}
         {$IfDef RecordSSIMprepFull} WriteLineToDebugFile('After Normalize RRI  ' + '  Refs=' + IntToStr(DEMsinIndex(RefRRIMap)) + '  Test=' + IntToStr(DEMsinIndex(RRIMap))); {$EndIf}

         {$If Defined(RecordSSIMprep)} WriteLineToDebugFile('SSIMprep start save grids, tiles=' + IntToStr(AllTiles.Count)); {$EndIf}
         CurrentOperation.Text := 'Save grids';
         for i := 0 to pred(AllTiles.Count) do begin
            TileProgress.Text := IntToStr(succ(i)) + '/' + IntToStr(AllTiles.Count) + '  ' + AllTiles[i];
            SaveGridsForOneTile(AllTiles[i]);
         end;

         {$If Defined(RecordSSIMprep)} WriteLineToDebugFile('SSIMprep start close grids'); {$EndIf}
         CurrentOperation.Text := 'Close tile geomorph grids';
         for j := 1 to MaxDemixDEM do begin
            CloseSingleDEM(SlopeMap[j]);
            CloseSingleDEM(TestRuffMap[j]);
            CloseSingleDEM(TestRRI[j]);
            CloseSingleDEM(TestTPI[j]);
            CloseSingleDEM(RefSlopeMap[j]);
            CloseSingleDEM(RefRuffMap[j]);
            CloseSingleDEM(RefRRI[j]);
            CloseSingleDEM(TestHillshade[j]);
            CloseSingleDEM(RefHillshade[j]);
            CloseSingleDEM(RefTPI[j]);
         end;
      end;
      CurrentOperation.Text := 'Clean memory for area';
      FreeAndNil(AllTiles);
      if DEMIXCloseMap then BitBtn8Click(Nil);
      CleanUpTempDirectory(false);
      CurrentOperation.Text := '';
   finally
      CloseAllDataBases;
      RestoreBackupDefaults;
      TileProgress.Text := '';
      //EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX) or Defined(RecordSSIMprep)} WriteLineToDebugFile('SSIMprep out'); {$EndIf}
end {TDemixFilterForm.SSIMprep};
*)


procedure TDemixFilterForm.BitBtn3Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := DEMIXSettingsDir;
   if GetExistingFileName('Area names','*.txt',fName) then begin
      Memo6.Lines.LoadFromFile(fName);
   end;
end;


procedure TDemixFilterForm.BitBtn40Click(Sender: TObject);
begin
//cannot be enabled until area is loaded via the new structure
   WBT_CreateDEMIX_GeomorphonGrids(true);
end;

procedure TDemixFilterForm.BitBtn4Click(Sender: TObject);
begin
{$IfDef ExDEMIXexperimentalOptions}
{$Else}
   MakeHistogramOfDifferenceDistribution(ComboBox1.Items[ComboBox1.ItemIndex],ComboBox2.Items[ComboBox2.ItemIndex],ComboBox3.Items[ComboBox3.ItemIndex]);
{$EndIf}
end;


procedure TDemixFilterForm.BitBtn5Click(Sender: TObject);
begin
   //SSIMComputations(ssimAllAreas);
end;


procedure TDemixFilterForm.ZeroDEMs;
var
   i : integer;
begin
   for i := 1 to MaxDemixDEM do begin
      MergeDEMs[i] := 0;
      DiffDSMDEMs[i] := 0;
      RefDEMs[i] := 0;
      DiffDTMDEMs[i] := 0;
      TestDEMs[i] := 0;
      RefDEMsHalfSec[i] := 0;
   end;
end;



procedure TDemixFilterForm.BitBtn7Click(Sender: TObject);
begin
    MakeDifferenceMaps(ElevSpecified);
end;


procedure TDemixFilterForm.BitBtn8Click(Sender: TObject);
begin
   CloseAllDEMs;
   ZeroDEMs;
   wmdem.CloseAlldataandwindows1Click(nil);
   CleanUpTempDirectory;
   BitBtn34.Enabled := true;
end;

procedure TDemixFilterForm.BitBtn9Click(Sender: TObject);
var
   i : integer;
begin
   CheckEditString(Edit3.Text,MDDef.TopCutLevel);
   MDDef.BottomCutLevel := -MDDef.TopCutLevel;
   for i := 1 to 6 do begin
      if ValidDEM(DiffDSMDEMs[i]) then DEMGlb[DiffDSMDEMs[i]].SelectionMap.DoCompleteMapRedraw;
      if ValidDEM(DiffDTMDEMs[i]) then DEMGlb[DiffDTMDEMs[i]].SelectionMap.DoCompleteMapRedraw;
   end;
   MakeBigDiffferenceMapImage('Elev');
end;

procedure TDemixFilterForm.CheckBox10Click(Sender: TObject);
begin
   MDDef.RGBbestSeparates := CheckBox10.Checked;
end;

procedure TDemixFilterForm.CheckBox11Click(Sender: TObject);
begin
   MDDef.DEMIX_default_half_sec_ref := CheckBox11.Checked;
end;

procedure TDemixFilterForm.CheckBox16Click(Sender: TObject);
begin
   MDDef.DEMIX_open_ref_DSM := CheckBox16.Checked;
end;


procedure TDemixFilterForm.CheckBox18Click(Sender: TObject);
begin
   MDDef.DEMIX_overwrite_enabled := CheckBox18.Checked;
end;


procedure TDemixFilterForm.CheckBox23Click(Sender: TObject);
begin
   MDDef.LoadRefDEMMaps := CheckBox23.Checked;
end;


procedure TDemixFilterForm.CheckBox2Click(Sender: TObject);
begin
   MDDef.MakeCOP_ALOS_diffMaps := CheckBox2.Checked;
end;


procedure TDemixFilterForm.CheckBox3Click(Sender: TObject);
begin
   MDDef.LoadTestDEMs := CheckBox3.Checked;
end;

procedure TDemixFilterForm.CheckBox4Click(Sender: TObject);
begin
   MDDef.LoadTestDEMMaps := CheckBox4.Checked;
end;

procedure TDemixFilterForm.CheckBox7Click(Sender: TObject);
begin
   MDDef.MakeCOP_ALOS_Best_Map := CheckBox7.Checked;
end;

procedure TDemixFilterForm.CheckBox8Click(Sender: TObject);
begin
   MDDef.MakeCOP_ALOS_Cat_Maps := CheckBox8.Checked;
end;

procedure TDemixFilterForm.CheckBox9Click(Sender: TObject);
begin
   MDDef.MakeCOP_FABDEM_diffMaps := CheckBox9.Checked;
end;

procedure TDemixFilterForm.ComboBox1Change(Sender: TObject);
begin
   MDDef.DEMIX_default_tile := ComboBox1.Text;
end;

procedure TDemixFilterForm.ComboBox4Change(Sender: TObject);
begin
   MDDef.DEMIX_default_area := ComboBox4.Text;
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
   MDDef.DEMIXSimpleTolerance := MDDef.TopCutLevel;
end;

procedure TDemixFilterForm.Edit4Change(Sender: TObject);
begin
   CheckEditString(Edit4.Text,MDDef.DEMIXSlopeTolerance);
end;

procedure TDemixFilterForm.Edit5Change(Sender: TObject);
begin
   CheckEditString(Edit5.Text,MDDef.DEMIXRuffTolerance);
end;

procedure TDemixFilterForm.FormCreate(Sender: TObject);
var
   i : integer;
begin
   Edit1.Text := IntToStr(MDDef.DEMIX_xsize);
   Edit2.Text := IntToStr(MDDef.DEMIX_ysize);
   Edit3.Text := RealToString(MDDef.TopCutLevel,-8,-2);
   Edit4.Text := RealToString(MDDef.DEMIXSlopeTolerance,-8,-2);
   Edit5.Text := RealToString(MDDef.DEMIXRuffTolerance,-8,-2);

   CheckBox2.Checked := MDDef.MakeCOP_ALOS_diffMaps;
   CheckBox3.Checked := MDDef.LoadTestDEMs;
   CheckBox8.Checked := MDDef.MakeCOP_ALOS_Cat_Maps;
   CheckBox7.Checked := MDDef.MakeCOP_ALOS_Best_Map;
   CheckBox9.Checked := MDDef.MakeCOP_FABDEM_diffMaps;
   CheckBox10.Checked := MDDef.RGBbestSeparates;
   CheckBox11.Checked := MDDef.DEMIX_default_half_sec_ref;

   CheckBox16.Checked := MDDef.DEMIX_open_ref_DSM;
   CheckBox18.Checked := MDDef.DEMIX_overwrite_enabled;
   CheckBox23.Checked := MDDef.LoadRefDEMMaps;

   CheckBox4.Checked := MDDef.LoadTestDEMMaps;
   LoadOneSecRefCheckBox.Checked := MDDef.LoadRefDEMs;
   CheckBox3.Checked := MDDef.LoadRefDEMs;

   threedembestrgm_checkbox.Checked := MDDef.MakeRGB_Best_Map;

   if (Sender <> Nil) then begin
      DEMsTypeUsing := tStringList.Create;
      TilesUsing := tStringList.Create;
      LandTypesUsing := tStringList.Create;
      CriteriaUsing := tStringList.Create;
      CandidateDEMsUsing := tStringList.Create;
      TileParameters := tStringList.Create;
   end;
   ZeroMapIndexes;
   PageControl1.ActivePage := TabSheet1;
end;


procedure TDemixFilterForm.FormDestroy(Sender: TObject);
begin
   DEMsTypeUsing.Destroy;
   TilesUsing.Destroy;
   LandTypesUsing.Destroy;
   CriteriaUsing.Destroy;
   CandidateDEMsUsing.Destroy;
   TileParameters.Destroy;
end;

procedure TDemixFilterForm.BitBtn10Click(Sender: TObject);
begin
   DifferenceMapsAllAreas(ElevSpecified);
end;


procedure TDemixFilterForm.DifferenceMapsAllAreas(WhatFor : integer);
var
   i : integer;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.DifferenceMapsAllAreas, whatfor=' + IntToStr(WhatFor)); {$EndIf}
   try
      HeavyDutyProcessing := true;
      for i := 0 to pred(ComboBox4.Items.Count) do begin
         wmdem.SetPanelText(2,IntToStr(succ(i)) + '/' + IntToStr(ComboBox4.Items.Count));
         ComboBox4.Text := ComboBox4.Items[i];
         wmdem.SetPanelText(3,ComboBox4.Text);
         MakeDifferenceMaps(WhatFor);
         CloseAllDEMs;
         wmdem.Closeallgraphs1Click(nil);
         wmdem.Closeallpictureviewwindows1Click(nil);
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
var
   xloc,yloc : integer;
   Lat,Long : float64;
   LocMax : float32;
   AreaName : shortstring;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.BitBtn12Click (terrain profiles) in'); {$EndIf}
   CloseAllDEMs;
   UncheckAllLoadCheckboxes;
   LoadOneSecRefCheckBox.Checked := true;
   CheckBox3.Checked := true;
   AreaName := ComboBox4.Text;
   LoadDEMsForCurrentArea(AreaName,false,false);
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
   RGB_Best_Map : tStringList;
   RuffRef,SlopeRef,RuffALOS,SlopeALOS,RuffCOP,SlopeCOP,RuffFab,SlopeFab,i,j,DiffMaps,COPDEM,ALOSDEM,FABDEM,Cop_FAB_diff : integer;
   NewElevGrid,NewSlopeGrid,NewRuffGrid,BestElevGrid,BestSlopeGrid,BestRuffGrid,RGBBestElevGrid,RGBBestSlopeGrid,RGBBestRuffGrid : tDEMIXindexes;

      function MakeNewMap(What : shortstring; RefDEM,ALOSDEM,COPDEM,MergeDEM : integer) : integer;

          procedure DoOption(Cats : shortstring; Use4Cats : boolean);
          begin
               fName := What + Cats + '_alos_cop_high_low_ref_' + DEMGlb[RefDEM].AreaName + '.dem';
               Result := TwoDEMHighLowMap(RefDEM,ALOSDEM,COPDEM,MDDef.TopCutLevel,Use4Cats,fName);
               if ValidDEM(Result) then begin
                  if MDDef.AutoMergeStartDEM then begin
                     DEMGlb[Result].SelectionMap.MergeAnotherDEMreflectance(MergeDEM,true);
                  end;
               end
               else begin
                  {$IfDef RecordDEMIX} WriteLineToDebugFile('MakeNewMap failed, ' + fName); {$EndIf}
               end;
          end;

      begin
         {$IfDef RecordDEMIX} WriteLineToDebugFile('Make new map ' + what); {$EndIf}
         if RadioGroup1.ItemIndex in [0,2] then DoOption('_4_cats',true);
         if RadioGroup1.ItemIndex in [1,2] then DoOption('_9_cats',false);
      end;


      function MakeBestMap(RefDEM,ALOSDEM,COPDEM,MergeDEM : integer; Tolerance : float32; What : shortstring) : integer;
      begin
         Result := BestCopOrALOSmap(RefDEM,ALOSDEM,COPDEM,Tolerance,What);
         if ValidDEM(Result) then begin
            if MDDef.AutoMergeStartDEM then begin
               DEMGlb[Result].SelectionMap.MergeAnotherDEMreflectance(MergeDEM,true);
            end;
         end
         else begin
            {$IfDef RecordDEMIX} WriteLineToDebugFile('MakeBestMap failed, ' + fName); {$EndIf}
         end;
      end;

      function MakeRGBBestMap(RefDEM,ALOSDEM,COPDEM,FabDEM,MergeDEM : integer; Tolerance : float32; What : shortstring) : integer;
      var
         bmp : tMyBitmap;
         fName : PathStr;
      begin
        {$IfDef RecordDEMIX} WriteLineToDebugFile('MakeRGBBestMap, ' + What); {$EndIf}
         Result := RGBBestOfThreeMap(RefDEM,ALOSDEM,COPDEM,FABDEM,MergeDEM,Tolerance,What);
         bmp := DEMGlb[Result].SelectionMap.CreateMapAndLegendSideBySide;
         fName := Petmar.NextFileNumber(MDTempDir,'rgb_map_with_legend_','.bmp');
         bmp.SaveToFile(fName);
         bmp.Free;
         RGB_Best_Map.Add(fName);
      end;

var
   z,pc : float32;
   refDEMSurfaceType : integer;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.BitBtn14Click (COP-ALOS) in'); {$EndIf}
   BitBtn8Click(Sender);  //close and zero all DEMs
   GetDEMIXpaths(true);
   UncheckAllLoadCheckboxes;
   CheckBox5.Checked := true;
   CheckBox6.Checked := true;
   AreaName := ComboBox4.Text;
   LoadDEMsForCurrentArea(AreaName,true,true);
   {$IfDef RecordDEMIX} WriteLineToDebugFile('AreaName: ' + AreaName); {$EndIf}

   COPDEM := 0;
   ALOSDEM := 0;
   FABDEM := 0;
   for j := 1 to MaxDemixDEM do if ValidDEM(TestDEMs[j]) then begin
      if StrUtils.AnsiContainsText(UpperCase(DEMglb[TestDEMs[j]].AreaName),'COP') then COPDEM := TestDEMs[j];
      if StrUtils.AnsiContainsText(UpperCase(DEMglb[TestDEMs[j]].AreaName),'ALOS') then ALOSDEM := TestDEMs[j];
      if StrUtils.AnsiContainsText(UpperCase(DEMglb[TestDEMs[j]].AreaName),'FABDEM') then FABDEM := TestDEMs[j];
   end;

   if MDDef.MakeCOP_ALOS_diffMaps then begin
      Diffmaps := 0;
      for i := 1 to MaxDemixDEM do if ValidDEM(RefDEMsHalfSec[i]) then begin
         for j := 1 to MaxDemixDEM do if ValidDEM(TestDEMs[j]) then begin
            inc(DiffMaps);
            DiffDTMDEMs[DiffMaps] := MakeDifferenceMap(RefDEMsHalfSec[i],TestDEMs[j],RefDEMsHalfSec[i],0,true,false,false,DEMglb[RefDEMsHalfSec[i]].AreaName + '_Delta_to_' + DEMglb[TestDEMs[j]].AreaName);
         end;
      end;
   end;

   if MDDef.MakeCOP_FABDEM_diffMaps then begin
      MakeDifferenceMap(COPDEM,RefDEMsHalfSec[1],COPDEM,COPDEM,true,false,false,AreaName + '_COPDEM_minus_Ref');
      MakeDifferenceMap(FABDEM,RefDEMsHalfSec[1],FABDEM,COPDEM,true,false,false,AreaName + '_FABDEM_minus_Ref');
      Cop_FAB_diff := MakeDifferenceMap(COPDEM,FABDEM,COPDEM,COPDEM,true,false,false,AreaName + '_COPDEM_minus_FABDEM');

      {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.BitBtn14Click Cop_FAB_diff ' + DEMGlb[Cop_FAB_diff].ZRange ); {$EndIf}
      z := DEMGlb[Cop_FAB_diff].DEMheader.MinElev;
      WriteLineToDebugFile('');
      WriteLineToDebugFile(DEMGlb[Cop_FAB_diff].AreaName);
      WriteLineToDebugFile('Min: ' + RealToString(z,-8,-2));
      z := round(z);
      while z <= 0 do begin
         if (z > -0.2) then z := -0.1;
         pc := DEMGlb[Cop_FAB_diff]. PercentileOfElevation(z);
         if (pc > 0.11) then WriteLineToDebugFile('Below ' + RealToString(z,4,1) + 'm  ' +  RealToString(pc,8,1) + '%');
         z := z + 0.5;
      end;
      WriteLineToDebugFile('');
   end;


   if MDDef.MakeCOP_ALOS_Cat_Maps or MDDef.MakeCOP_ALOS_Best_Map or MDDef.MakeRGB_Best_Map then begin
      for i := 1 to MaxDemixDEM do begin
         if MDDef.MakeRGB_Best_Map then RGB_Best_Map := tStringList.Create;
         if ValidDEM(RefDEMsHalfSec[i]) then begin
            refDEMsurfaceType := IsDEMaDSMorDTM(UpperCase(DEMGlb[RefDEMsHalfSec[i]].AreaName));
            {$IfDef RecordDEMIX} WriteLineToDebugFile('Do half sec, ' + DEMGlb[RefDEMsHalfSec[i]].AreaName); {$EndIf}

            SlopeRef := 0; //to force return of the slope map in addition to the ruff map
            RuffRef := CreateSlopeRoughnessSlopeStandardDeviationMap(RefDEMsHalfSec[i],5,SlopeRef,false);
            SlopeCOP := 0;
            RuffCOP := CreateSlopeRoughnessSlopeStandardDeviationMap(COPDEM,5,SlopeCOP,false);
            SlopeALOS := 0;
            RuffALOS := CreateSlopeRoughnessSlopeStandardDeviationMap(ALOSDEM,5,SlopeALOS,false);
            if MDDef.MakeRGB_Best_Map then begin
               SlopeFAB := 0;
               RuffFAB:= CreateSlopeRoughnessSlopeStandardDeviationMap(FABDEM,5,SlopeFAB,false);
            end;
            {$IfDef RecordDEMIX} WriteLineToDebugFile('Slope/ruff created'); {$EndIf}

            if MDDef.MakeRGB_Best_Map then begin
               {$IfDef RecordDEMIX} WriteLineToDebugFile('MDDef.MakeRGB_Best_Map'); {$EndIf}
               RGBBestElevGrid[i] := MakeRGBBestMap(RefDEMsHalfSec[i],ALOSDEM,COPDEM,FABDEM,RefDEMsHalfSec[i],MDDef.DEMIXSimpleTolerance,
                   AreaName + '_best_elevation_ref_' + RefDEMType[refDEMSurfaceType] + '±' + RealToString(MDDef.DEMIXSimpleTolerance,-4,-2));
               RGBBestSlopeGrid[i] := MakeRGBBestMap(SlopeRef,SlopeALOS,SlopeCOP,SlopeFAB,RefDEMsHalfSec[i],MDDef.DEMIXSlopeTolerance,
                   AreaName + '_best_slope_ref_' + RefDEMType[refDEMSurfaceType] + '±' + RealToString(MDDef.DEMIXSlopeTolerance,-4,-2));
               RGBBestRuffGrid[i] := MakeRGBBestMap(RuffRef,RuffALOS,RuffCOP,RuffFAB,RefDEMsHalfSec[i],MDDef.DEMIXRuffTolerance,
                   AreaName + '_best_roughness_ref_' + RefDEMType[refDEMSurfaceType] + '±' + RealToString(MDDef.DEMIXRuffTolerance,-4,-2));
            end;

            if MDDef.MakeCOP_ALOS_Best_Map then begin
               {$IfDef RecordDEMIX} WriteLineToDebugFile('MDDef.MakeCOP_ALOS_Best_Map'); {$EndIf}
               BestElevGrid[i] := MakeBestMap(RefDEMsHalfSec[i],ALOSDEM,COPDEM,RefDEMsHalfSec[i],MDDef.DEMIXSimpleTolerance,
                   AreaName + '_elevation_ref_' +  RefDEMType[refDEMSurfaceType] + '±' + RealToString(MDDef.DEMIXSimpleTolerance,-4,-2));
               BestSlopeGrid[i] := MakeBestMap(SlopeRef,SlopeALOS,SlopeCOP,RefDEMsHalfSec[i],MDDef.DEMIXSlopeTolerance,
                   AreaName + '_slope_ref_' +  RefDEMType[refDEMSurfaceType] + '±' + RealToString(MDDef.DEMIXSlopeTolerance,-4,-2));
               BestRuffGrid[i] := MakeBestMap(RuffRef,RuffALOS,RuffCOP,RefDEMsHalfSec[i],MDDef.DEMIXRuffTolerance,
                   AreaName + '_roughness_ref_' +  RefDEMType[refDEMSurfaceType] + '±' + RealToString(MDDef.DEMIXRuffTolerance,-4,-2));
            end;

            if MDDef.MakeCOP_ALOS_Cat_Maps then begin
               {$IfDef RecordDEMIX} WriteLineToDebugFile('MDDef.MakeCOP_ALOS_Cat_Maps'); {$EndIf}
               NewElevGrid[i] := MakeNewMap('elev',RefDEMsHalfSec[i],ALOSDEM,COPDEM,RefDEMsHalfSec[i]);
               NewSlopeGrid[i] := MakeNewMap('slope',SlopeRef,SlopeALOS,SlopeCOP,RefDEMsHalfSec[i]);
               NewRuffGrid[i] := MakeNewMap('ruff',RuffRef,RuffALOS,RuffCOP,RefDEMsHalfSec[i]);
            end;
         end;
         if MDDef.MakeRGB_Best_Map then MakeBigBitmap(RGB_Best_Map,'Best for ' + AreaName,'',1);
      end;
   end;

   EndDEMIXProcessing;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.BitBtn14Click (COP-ALOS) out'); {$EndIf}
end;


procedure TDemixFilterForm.BitBtn15Click(Sender: TObject);
//has to be on form to loop through the params types
{$IfDef ExDEMIXexperimentalOptions}
begin
{$Else}
var
   i : integer;
   graph : array[0..2] of tThisBaseGraph;
   FileList : tStringList;
   fName : PathStr;
begin
   FileList := tStringList.Create;
   for i := 0 to 2 do begin //the params loop
      graph[i] := Nil;
      graph[i] := MakeHistogramOfDifferenceDistribution(ComboBox1.Items[ComboBox1.ItemIndex],ComboBox2.Items[i],ComboBox3.Items[ComboBox3.ItemIndex]);
      if (Graph[i] <> Nil) then begin
         fName := NextFileNumber(MDtempdir,'diff_graph_','.bmp');
         SaveImageAsBMP(Graph[i].Image1,fName);
         FileList.Add(fName);
         if SaveDifferenceDistribution then Graph[i].ViewGraphData;
      end;
   end;
   fName := NextFileNumber(MDTempDir,ComboBox1.Text + '_difference_distrib_graphs_','.png');
   MakeBigBitmap(FileList,'',fName,3);
{$EndIf}
end;


procedure TDemixFilterForm.BitBtn16Click(Sender: TObject);
//has to be on form to loop through the params and RefDEM types
{$IfDef ExDEMIXexperimentalOptions}
begin
{$Else}

var
   i,j : integer;
   graph : array[0..1,0..2] of tThisBaseGraph;
   FileList : tStringList;
   fName : PathStr;
begin
   FileList := tStringList.Create;
   for j := 1 downto 0 do begin //ref DEM loop
      ComboBox3.ItemIndex := j;
      ComboBox3.Text := ComboBox3.Items[j];
      for i := 0 to 2 do begin //params loop
         Graph[j,i] := MakeHistogramOfDifferenceDistribution(ComboBox1.Items[ComboBox1.ItemIndex],ComboBox2.Items[i],ComboBox3.Items[j]);
         if Graph[j,i] <> Nil then begin
            fName := NextFileNumber(MDtempdir,'diff_graph_','.bmp');
            SaveImageAsBMP(Graph[j,i].Image1,fName);
            FileList.Add(fName);
         end;
      end;
   end;
   fName := NextFileNumber(DEMIX_distrib_graph_dir,ComboBox1.Text + '_difference_distrib_graphs_','.png');
   MakeBigBitmap(FileList,'',fName,3);
{$EndIf}
end;


procedure TDemixFilterForm.BitBtn17Click(Sender: TObject);
begin
   GISDB[DB].DisplayTable;
end;

procedure TDemixFilterForm.BitBtn18Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := DEMIXSettingsDir + 'tile*.txt';
   if GetExistingFileName('DEMIX tiles','*.txt',fName) then begin
      ComboBox1.Items.LoadFromFile(fName);
      ComboBox1.ItemIndex := 0;
   end;
end;

procedure TDemixFilterForm.BitBtn19Click(Sender: TObject);
var
   i : integer;
begin
   try
      HeavyDutyProcessing := true;
      for i := 0 to pred(ComboBox1.Items.Count) do begin
         wmdem.SetPanelText(2,IntToStr(succ(i)) + '/' + IntToStr(ComboBox1.Items.Count));
         ComboBox1.Text := ComboBox1.Items[i];
         ComboBox1.ItemIndex := i;
         if not FileExists(DEMIX_distrib_graph_dir + ComboBox1.Text + '_difference_distrib_graphs_1.png') then begin
            BitBtn16Click(Sender);
            wmdem.Closeallgraphs1Click(Sender);
            wmdem.Closeallpictureviewwindows1Click(Sender);
         end;
      end;
   finally
      HeavyDutyProcessing := false;
      ShowDefaultCursor;
   end;
end;


procedure TDemixFilterForm.BitBtn1Click(Sender: TObject);
{$IfDef ExDEMIXexperimentalOptions}
begin
{$Else}
var
   NumGraph: integer;
begin
   GetUsingStringLists;
   case RadioGroup2.ItemIndex of
      0 : NumGraph := TileParameters.Count * DEMsTypeUsing.Count;
      1 : NumGraph := TileParameters.Count;
      2 : NumGraph := TileParameters.Count * CriteriaUsing.Count;
      3 : NumGraph := TileParameters.Count;
   end;
   if AnswerIsYes('This will create ' + IntToStr(NumGraph) + ' graphs; Proceed') then begin
      MultipleBestByParametersSortByValue(DB, RadioGroup2.ItemIndex, DEMsTypeUsing,TilesUsing,LandTypesUsing,CandidateDEMsUsing,CriteriaUsing,TileParameters);
   end;
   //DoCriteriaGraph;
{$EndIf}
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
   DoOne(Memo7,TileParameters);
end;


procedure TDemixFilterForm.LoadClick(Sender: TObject);
var
   fName : PathStr;
begin
   fName := DEMIXSettingsDir;
   if GetExistingFileName('DEMIX tiles','tile*.txt',fName) then begin
      Memo2.Lines.LoadFromFile(fName);
   end;
end;


procedure TDemixFilterForm.LoadCurrentAreaBitBtn5Click(Sender: TObject);
var
   AreaName : Petmar_types.shortstring;
   DEMIXRefDEM : integer;
begin
   AreaName := ComboBox4.Text;
   if MDDef.LoadRefDEMs then LoadDEMIXReferenceDEMs(AreaName,DEMIXRefDEM,MDDef.LoadRefDEMMaps);
   if MDDef.LoadTestDEMs then LoadDEMIXCandidateDEMs(AreaName,MDDef.LoadTestDEMMaps);
end;



function TDemixFilterForm.LoadDEMsForCurrentArea(AreaName: ShortString;  LoadRefMaps,LoadTestDEMmaps : boolean) : boolean;
var
   LoadResults,TStr : shortstring;
   AllTiles : tStringList;
   i,j : integer;


      procedure LoadFromPath(var Which : tDEMIXindexes; aPath : PathStr; Ext : ANSIstring; LoadMaps : boolean; What : shortstring);
      var
         FilesWanted : tStringList;
         i,j,DEMs : integer;
         fName : PathStr;
      begin
         FilesWanted := tStringList.Create;
         FindMatchingFiles(aPath,Ext,FilesWanted,1);
         RemoveFilesThatDoNotHaveString(FilesWanted,AreaName);
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile(AreaName + ' ' + What + ' DEMs=' + IntToStr(FilesWanted.Count)); {$EndIf}
         DEMs := 0;
         for j := 0 to pred(FilesWanted.Count) do begin
            fName := FilesWanted.Strings[j];
            if StrUtils.AnsiContainsText(UpperCase(fname),UpperCase(AreaName)) then begin
               if (DEMs = MaxDemixDEM) then begin
                  Memo8.Lines.Add('Too many DEMs in ' + aPath);
               end
               else begin
                  if (What = 'Ref') then begin
                     if StrUtils.AnsiContainsText(UpperCase(fname),UpperCase('DSM')) and (not MDDef.DEMIX_open_ref_DSM) then begin
                        {$If Defined(RecordDEMIXLoad)} WriteLineToDebugFile('Not opening DSM,  ' + fName); {$EndIf}
                     end
                     else if StrUtils.AnsiContainsText(fname,'ref') then begin
                        inc(DEMs);
                        Which[DEMs] := OpenNewDEM(fName,LoadMaps);
                     end
                  end
                  else begin
                     for i := 1 to NumDEMIXtestDEM do begin
                        if StrUtils.AnsiContainsText(fname,UpperCase(DEMIXShort[i])) then begin
                           inc(DEMs);
                           Which[i] := OpenNewDEM(fName,LoadMaps);
                        end;
                     end;
                  end;
               end;
              {$If Defined(RecordDEMIXLoad)} WriteLineToDebugFile('DEM=' + IntToStr(Which[i]) + '   ' + fName); {$EndIf}
            end;
         end;
         FilesWanted.Free;
         LoadResults := LoadResults + What + '=' + IntToStr(DEMs) + '  ';
         {$IfDef RecordDEMIXLoad} WriteLineToDebugFile('Loaded ' + What + '=' + IntToStr(DEMs)); {$EndIf}
      end;

begin
   Result := true;
   MDDef.DEMIX_default_area := AreaName;
   CurrentOperation.Text := 'Load DEMs for ' + AreaName;
   {$IfDef RecordDEMIXLoad} WriteLineToDebugFile('TDemixFilterForm.LoadDEMsForArea in ' + AreaName); {$EndIf}
   ZeroDEMs;
   LoadResults := '';

   if LoadOneSecRefCheckBox.Checked then LoadFromPath(RefDEMs,DEMIX_Ref_1sec,'*.tif',LoadRefMaps,'Ref');

   if CheckBox3.Checked then begin
      LoadFromPath(TestDEMs,DEMIX_test_dems,'*.dem',LoadTestDEMmaps,'Test');

      if (DEMsinIndex(TestDEMs) <> NumDEMIXtestDEM) then begin
         TStr := 'Found: ' + IntToStr(DEMsinIndex(TestDEMs)) + ' test DEMs; need ' + IntToStr(NumDEMIXtestDEM) + ' for ' + AreaName;
         Memo8.Lines.Add(TStr);
         {$IfDef RecordDEMIX}
            HighlightLineToDebugFile(TStr);
            for i := 1 to MaxDemixDEM do begin
               if ValidDEM(TestDEMs[i]) then WriteLineToDebugFile('Found: ' + DEMGlb[TestDEMs[i]].AreaName);
            end;
         {$EndIf}
         Result := false;
      end;
   end;

   if CheckBox1.Checked then LoadFromPath(MergeDEMs,DEMIX_Ref_Merge,'*.dem',LoadRefMaps,'Merge');
   if CheckBox5.Checked then LoadFromPath(RefDEMsHalfSec,DEMIX_Ref_Half_sec,'*.tif',LoadRefMaps,'Ref_half_sec');
   if CheckBox6.Checked and (not CheckBox3.Checked) then begin
      LoadFromPath(TestDEMs,DEMIX_test_dems,'*.dem',LoadTestDEMmaps,'COP & ALOS Test');
      LoadFromPath(TestDEMs,DEMIX_test_dems,'*.dem',LoadTestDEMmaps,'COP & ALOS Test');
      LoadFromPath(TestDEMs,DEMIX_test_dems,'*.dem',LoadTestDEMmaps,'COP & ALOS Test');
   end;

   ComboBox1.Items.Clear;
   AllTiles := nil;
   for i := 1 to MaxDemixDEM do begin
      if ValidDEM(RefDEMs[i]) and (AllTiles = Nil) then begin
         AllTiles := DEMIXTilesOnDEM(RefDEMs[i]);
      end;
   end;
   if (AllTiles <> Nil) then begin
      if (AllTiles.Count > 0) then begin
         for i := 0 to pred(AllTiles.Count) do ComboBox1.Items.Add(AllTiles.Strings[i]);
         ComboBox1.Text := AllTiles.Strings[0];
      end;
      AllTiles.Destroy;
   end;
   BitBtn34.Enabled := false;

   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.LoadDEMsForArea out ' + AreaName + '  ' + LoadResults); {$EndIf}
end;



procedure TDemixFilterForm.LoadOneSecRefCheckBoxClick(Sender: TObject);
begin
   MDDef.LoadRefDEMs := LoadOneSecRefCheckBox.Checked;
end;

initialization
finalization
end.
