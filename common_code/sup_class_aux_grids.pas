  unit sup_class_aux_grids;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define SupClassAuxGrids}
{$EndIf}


interface

uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}


  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  System.UITypes,
  Dialogs, StdCtrls, Buttons,ClipBrd,
  Petmar_types,DEMMapf, ExtCtrls, ComCtrls, Vcl.Grids;

const
   MaxGeomorpFilters = 10;

type
  TSupClassAuxGrids = class(TForm)
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Edit1: TEdit;
    Use1: TCheckBox;
    Use2: TCheckBox;
    Use3: TCheckBox;
    Use4: TCheckBox;
    Use5: TCheckBox;
    RadioGroup2: TRadioGroup;
    BitBtn10: TBitBtn;
    StringGrid1: TStringGrid;
    BitBtn12: TBitBtn;
    CheckBox11: TCheckBox;
    Lock1: TCheckBox;
    Lock2: TCheckBox;
    Lock3: TCheckBox;
    Lock4: TCheckBox;
    Lock5: TCheckBox;
    RadioGroup1: TRadioGroup;
    BitBtn1: TBitBtn;
    Use6: TCheckBox;
    Lock6: TCheckBox;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    CheckBox8: TCheckBox;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    Lock7: TCheckBox;
    Use7: TCheckBox;
    BitBtn11: TBitBtn;
    BitBtn13: TBitBtn;
    Req1: TCheckBox;
    Req2: TCheckBox;
    Req3: TCheckBox;
    Req4: TCheckBox;
    Req5: TCheckBox;
    Req6: TCheckBox;
    Req7: TCheckBox;
    Use8: TCheckBox;
    Lock8: TCheckBox;
    Req8: TCheckBox;
    Edit4: TEdit;
    Label3: TLabel;
    BitBtn15: TBitBtn;
    Label4: TLabel;
    Edit5: TEdit;
    BitBtn16: TBitBtn;
    BitBtn18: TBitBtn;
    BitBtn14: TBitBtn;
    RadioGroup3: TRadioGroup;
    BitBtn20: TBitBtn;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    Memo4: TMemo;
    Panel1: TPanel;
    BitBtn19: TBitBtn;
    BitBtn17: TBitBtn;
    Req9: TCheckBox;
    Req10: TCheckBox;
    Lock9: TCheckBox;
    Lock10: TCheckBox;
    Use9: TCheckBox;
    Use10: TCheckBox;
    Label1: TLabel;
    Edit2: TEdit;
    RadioGroup4: TRadioGroup;
    RadioGroup5: TRadioGroup;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CancelBtnClick(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure CheckBox11Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure Use1Click(Sender: TObject);
    procedure Use2Click(Sender: TObject);
    procedure Use3Click(Sender: TObject);
    procedure Use4Click(Sender: TObject);
    procedure Use5Click(Sender: TObject);
    procedure Use6Click(Sender: TObject);
    procedure Use7Click(Sender: TObject);
    procedure Use8Click(Sender: TObject);
    procedure Req1Click(Sender: TObject);
    procedure Req2Click(Sender: TObject);
    procedure Req3Click(Sender: TObject);
    procedure Req4Click(Sender: TObject);
    procedure Req5Click(Sender: TObject);
    procedure Req6Click(Sender: TObject);
    procedure Req7Click(Sender: TObject);
    procedure Req8Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure BitBtn19Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure BitBtn20Click(Sender: TObject);
    procedure Use9Click(Sender: TObject);
    procedure Use10Click(Sender: TObject);
    procedure Req9Click(Sender: TObject);
    procedure Req10Click(Sender: TObject);
  private
    { Private declarations }
    procedure HideFilterDEMs;
    procedure LabelPercentiles;
    function CheckLimits(i: integer): boolean;
    procedure FinishGridSetup;
    procedure StartGridSetup;
    procedure FillInDEMinGrid(DEM, Row: integer);
    procedure CheckUse(Setting: boolean);
    procedure CheckIfUserChangedAnything;
    procedure DrawMask;
    procedure EvaluateTrainingPoints;
    procedure CloseMaskingDEMs;
  public
    { Public declarations }
    BaseMap    : tMapForm;
    TrainingSetDB,
    LastMask,
    MatchesNeeded : integer;
    MaskDEM,ClusterDEM,LastDEMtoUse : integer;
    InitialSetupOver : boolean;
    NeedRecalc,
    UseGrids : boolean;
    TheClass : shortstring;
    LastClassSet : PathStr;
    UseBand,ReqBand : array[1..MaxGeomorpFilters] of Boolean;
    ClassDEMS : array[1..MaxGeomorpFilters] of integer;
    LowVals,HighVals : array[1..MaxGeomorpFilters] of float64;
    procedure GridValuesAtPoint(Lat,Long : float64);
  end;


procedure ClassWithAuxGrids(theBaseMap : tMapForm; aTrainingSetDB : integer; aClass : shortstring);

var
  SupClassAuxGrids : TSupClassAuxGrids;

implementation

{$R *.dfm}

uses
   {$IfDef ExSats}
   {$Else}
      DEMEros,
   {$EndIf}
   {$IfDef ExGeoStats}
   {$Else}
      DEMStat,
   {$EndIf}

   DEMDefs,DEMCoord, DEMDef_Routines, BaseGraf,DEMDataBase,
   PetImage,Petmar,
   Mask_opts2,
   Make_Tables,
   rgb_colors_three_params,
   PetDBUtils, toggle_db_use,
   BaseMap, dem_manager,Nevadia_main, DEM_indexes,
   DataBaseCreate,  multigrid;


procedure ZeroGlobalVar;
begin
  SupClassAuxGrids := Nil;
end;

procedure ClassWithAuxGrids;
begin
   {$IfDef SupClassAuxGrids} WriteLineToDebugFile('ClassWithAuxGrids in'); {$EndIf}
   SupClassAuxGrids := tSupClassAuxGrids.Create(Application);
   SupClassAuxGrids.TheClass := aClass;
   SupClassAuxGrids.Edit4.Text := aClass;
   SupClassAuxGrids.BaseMap := theBaseMap;
   SupClassAuxGrids.TrainingSetDB := aTrainingSetDB;
   SupClassAuxGrids.InitialSetupOver := true;
   SupClassAuxGrids.BitBtn9.Enabled := ValidSatImage(SupClassAuxGrids.BaseMap.MapDraw.SatOnMap);
   SupClassAuxGrids.Show;
   {$IfDef SupClassAuxGrids} WriteLineToDebugFile('ClassWithAuxGrids out'); {$EndIf}
end;


procedure TSupClassAuxGrids.BitBtn10Click(Sender: TObject);
var
   DEMsWanted : tDEMbooleanArray;
   Continue : boolean;
   i,j : integer;
begin
   {$IfDef SupClassAuxGrids} WriteLineToDebugFile('TSupClassAuxGrids.BitBtn10Click (Pick open grids) in'); {$EndIf}
   NeedRecalc := true;
   GetMultipleDEMsFromList('Grid for class statistics',DEMsWanted);
   Continue := false;
   for i := 1 to MaxDEMDataSets do if DEMsWanted[i] then Continue := true;

   if not Continue then begin
      {$IfDef SupClassAuxGrids} WriteLineToDebugFile('Did not pick any grids'); {$EndIf}
      exit;
   end;
   StartGridSetup;

   j := 0;
   for i := 1 to MaxDEMDataSets do if DEMsWanted[i] then begin
      {$IfDef SupClassAuxGrids} WriteLineToDebugFile('Picked ' + DEMGlb[i].AreaName); {$EndIf}
       j := StringGrid1.RowCount;
       ClassDEMS[j] := i;
       UseBand[j] := true;
       LowVals[j] := DEMGlb[i].DEMheader.MinElev;
       HighVals[j] := DEMGlb[i].DEMheader.MaxElev;
       FillInDEMinGrid(i,j);
   end;
   RadioGroup2Click(Sender);
   FinishGridSetup;
end;


procedure TSupClassAuxGrids.FillInDEMinGrid(DEM,Row : integer);
begin
    StringGrid1.RowCount := Row + 1;
    StringGrid1.Cells[0,Row] := IntToStr(Row) + '--' + IntToStr(DEM);
    StringGrid1.Cells[1,Row] := DEMGlb[DEM].AreaName;
    StringGrid1.Cells[2,Row] := RealToString(LowVals[Row],-12,-2);
    StringGrid1.Cells[3,Row] := RealToString(HighVals[Row],-12,-2);
end;

procedure TSupClassAuxGrids.StartGridSetup;
var
   i : integer;
begin
   StringGrid1.Cells[0,0] := 'Grid';
   StringGrid1.Cells[1,0] := 'Name';
   StringGrid1.Cells[2,0] := 'Min';
   StringGrid1.Cells[3,0] := 'Max';
   StringGrid1.Cells[4,0] := 'Percentiles';

   StringGrid1.RowCount := 1;
   for I := 1 to MaxGeomorpFilters do begin
      ClassDEMS[i] := 0;
      UseBand[i] := false;
      ReqBand[i] := false;
   end;
end;

procedure TSupClassAuxGrids.Use10Click(Sender: TObject);
begin
   NeedRecalc := true;
end;

procedure TSupClassAuxGrids.Use1Click(Sender: TObject);
begin
   NeedRecalc := true;
end;

procedure TSupClassAuxGrids.Use2Click(Sender: TObject);
begin
   NeedRecalc := true;
end;

procedure TSupClassAuxGrids.Use3Click(Sender: TObject);
begin
   NeedRecalc := true;
end;

procedure TSupClassAuxGrids.Use4Click(Sender: TObject);
begin
   NeedRecalc := true;
end;

procedure TSupClassAuxGrids.Use5Click(Sender: TObject);
begin
   NeedRecalc := true;
end;

procedure TSupClassAuxGrids.Use6Click(Sender: TObject);
begin
   NeedRecalc := true;
end;

procedure TSupClassAuxGrids.Use7Click(Sender: TObject);
begin
   NeedRecalc := true;
end;

procedure TSupClassAuxGrids.Use8Click(Sender: TObject);
begin
   NeedRecalc := true;
end;

procedure TSupClassAuxGrids.Use9Click(Sender: TObject);
begin
   NeedRecalc := true;
end;

procedure TSupClassAuxGrids.FinishGridSetup;
begin
   StringGrid1.FixedRows := 1;
   StringGrid1.ColWidths[1] := 180;

   Use1.Visible := ClassDEMs[1] <> 0;
   Use2.Visible := ClassDEMs[2] <> 0;;
   Use3.Visible := ClassDEMs[3] <> 0;
   Use4.Visible := ClassDEMs[4] <> 0;
   Use5.Visible := ClassDEMs[5] <> 0;
   Use6.Visible := ClassDEMs[6] <> 0;
   Use7.Visible := ClassDEMs[7] <> 0;
   Use8.Visible := ClassDEMs[8] <> 0;
   Use9.Visible := ClassDEMs[9] <> 0;
   Use10.Visible := ClassDEMs[10] <> 0;

   Lock1.Visible := ClassDEMs[1] <> 0;
   Lock2.Visible := ClassDEMs[2] <> 0;;
   Lock3.Visible := ClassDEMs[3] <> 0;
   Lock4.Visible := ClassDEMs[4] <> 0;
   Lock5.Visible := ClassDEMs[5] <> 0;
   Lock6.Visible := ClassDEMs[6] <> 0;
   Lock7.Visible := ClassDEMs[7] <> 0;
   Lock8.Visible := ClassDEMs[8] <> 0;
   Lock9.Visible := ClassDEMs[9] <> 0;
   Lock10.Visible := ClassDEMs[10] <> 0;

   Req1.Visible := ClassDEMs[1] <> 0;
   Req2.Visible := ClassDEMs[2] <> 0;;
   Req3.Visible := ClassDEMs[3] <> 0;
   Req4.Visible := ClassDEMs[4] <> 0;
   Req5.Visible := ClassDEMs[5] <> 0;
   Req6.Visible := ClassDEMs[6] <> 0;
   Req7.Visible := ClassDEMs[7] <> 0;
   Req8.Visible := ClassDEMs[8] <> 0;
   Req9.Visible := ClassDEMs[9] <> 0;
   Req10.Visible := ClassDEMs[10] <> 0;

   ApplicationProcessMessages;
   LabelPercentiles;

   ShowDefaultCursor;
end;


function TSupClassAuxGrids.CheckLimits(i : integer) : boolean;
begin
   Result := false;
    if (ClassDEMs[i] <> 0) then begin
        case i of
            1 : Result := (not Lock1.Checked) and Use1.Checked;
            2 : Result := (not Lock2.Checked) and Use2.Checked;
            3 : Result := (not Lock3.Checked) and Use3.Checked;
            4 : Result := (not Lock4.Checked) and Use4.Checked;
            5 : Result := (not Lock5.Checked) and Use5.Checked;
            6 : Result := (not Lock6.Checked) and Use6.Checked;
            7 : Result := (not Lock7.Checked) and Use7.Checked;
            8 : Result := (not Lock8.Checked) and Use8.Checked;
            9 : Result := (not Lock9.Checked) and Use9.Checked;
           10 : Result := (not Lock10.Checked) and Use10.Checked;
        end;
    end;
end;

procedure TSupClassAuxGrids.RadioGroup1Click(Sender: TObject);
begin
   NeedRecalc := true;
end;

procedure TSupClassAuxGrids.RadioGroup2Click(Sender: TObject);
var
   fName : PathStr;
   Table : tMyData;
   i     : integer;
begin
   {$IfDef SupClassAuxGrids} WriteLineToDebugFile('TSupClassAuxGrids.RadioGroup2Click (stats wanted) in, index=' + IntToStr(RadioGroup2.ItemIndex),true); {$EndIf}
   if (TrainingSetDB = 0) or (theClass <> '') then begin
      RadioGroup2.Enabled := false;
      exit;
   end;

   if (RadioGroup2.ItemIndex = 5) then BitBtn1Click(Sender)
   else for i := 1 to MaxGeomorpFilters do begin
      if CheckLimits(i) then begin
         fName := GISdb[TrainingSetDB].GridStatsName(ClassDEMs[i],DefaultDBExt);
         {$IfDef SupClassAuxGrids} WriteLineToDebugFile(fName);    {$EndIf}
         if FileExists(fName) then begin
           Table := tMyData.Create(fName);
           Table.ApplyFilter('NAME=' + QuotedStr(TheClass));
           if (Table.RecordCount = 1) then begin
              case RadioGroup2.ItemIndex of
                 0 : begin
                        LowVals[i] := Table.GetFieldByNameAsFloat('MIN');
                        HighVals[i] := Table.GetFieldByNameAsFloat('MAX');
                     end;
                 1 : begin
                        LowVals[i] := Table.GetFieldByNameAsFloat('PERC_5');
                        HighVals[i] := Table.GetFieldByNameAsFloat('PERC_95');
                     end;
                 2 : begin
                        LowVals[i] := Table.GetFieldByNameAsFloat('PERC_10');
                        HighVals[i] := Table.GetFieldByNameAsFloat('PERC_90');
                     end;
                 3 : begin
                        LowVals[i] := Table.GetFieldByNameAsFloat('QUANT_25');
                        HighVals[i] := Table.GetFieldByNameAsFloat('QUANT_75');
                     end;
              end {case};
              StringGrid1.Cells[2,i] := RealToString(LowVals[i],-12,-3);
              StringGrid1.Cells[3,i] := RealToString(HighVals[i],-12,-3);
           end {if Table.RecordCount = 1};
           Table.Destroy;
         end
         else MessageToContinue('Missing ' + fName);
      end {if Check};
   end {for i};
   LabelPercentiles;
end;


procedure TSupClassAuxGrids.RadioGroup3Click(Sender: TObject);
begin
   if ClassDEMS[succ(RadioGroup3.ItemIndex)] <> 0 then begin
      BaseMap.MapDraw.AssignSecondDEM(ClassDEMS[succ(RadioGroup3.ItemIndex)]);
      BaseMap.MapDraw.DeleteSingleMapLayer(BaseMap.MapDraw.SecondGridfName);
      BaseMap.DoFastMapRedraw;
      BaseMap.Locationsonly1Click(Nil);
   end;
end;

procedure TSupClassAuxGrids.Req10Click(Sender: TObject);
begin
   NeedRecalc := true;
end;

procedure TSupClassAuxGrids.Req1Click(Sender: TObject);
begin
   NeedRecalc := true;
end;

procedure TSupClassAuxGrids.Req2Click(Sender: TObject);
begin
   NeedRecalc := true;
end;

procedure TSupClassAuxGrids.Req3Click(Sender: TObject);
begin
   NeedRecalc := true;
end;

procedure TSupClassAuxGrids.Req4Click(Sender: TObject);
begin
   NeedRecalc := true;
end;

procedure TSupClassAuxGrids.Req5Click(Sender: TObject);
begin
   NeedRecalc := true;
end;

procedure TSupClassAuxGrids.Req6Click(Sender: TObject);
begin
   NeedRecalc := true;
end;

procedure TSupClassAuxGrids.Req7Click(Sender: TObject);
begin
   NeedRecalc := true;
end;

procedure TSupClassAuxGrids.Req8Click(Sender: TObject);
begin
   NeedRecalc := true;
end;


procedure TSupClassAuxGrids.Req9Click(Sender: TObject);
begin
   NeedRecalc := true;
end;

procedure TSupClassAuxGrids.BitBtn11Click(Sender: TObject);
begin
   CheckUse(true);
end;


procedure TSupClassAuxGrids.GridValuesAtPoint(Lat,Long : float64);
var
   xg1,yg1 : float64;
   z : float32;
   Band : integer;
   TStr : shortString;
begin
   if InitialSetupOver and (ClassDEMs[1] <> 0) then begin
       Memo2.Clear;
       Memo2.Font.Color := clGreen;
       Memo3.Clear;
       Memo3.Font.Color := clRed;
       DEMGlb[ClassDEMs[1]].LatLongDegreeToDEMGrid(Lat,Long,xg1,yg1);
       for Band := 1 to MaxGeomorpFilters do begin
          if (ClassDEMs[Band] <> 0) then begin
            if DEMGlb[ClassDEMs[Band]].GetElevMeters(round(xg1),round(yg1),z) then begin
               TStr := DEMGlb[ClassDEMs[Band]].AreaName + '  ' + RealToString(z,-12,-4);
               if ReqBand[Band] then TStr := 'Req ' + TStr;
               
               if (z >= LowVals[Band]) and (z <= HighVals[Band]) then Memo2.Lines.Add(TStr)
               else Memo3.Lines.Add(TStr);
            end
            else Memo3.Lines.Add(DEMGlb[ClassDEMs[Band]].AreaName + ' no data');
          end;
       end;
   end;
end;


procedure TSupClassAuxGrids.DrawMask;
begin

   DEMGlb[LastMask].AreaName := 'Class mask ';
   DEMGlb[LastMask].DEMheader.ElevUnits := euIntCode;
   DEMGlb[LastMask].CheckMaxMinElev;

   if MDDef.MaskMapShow in [0,2] then begin
      if DEMGlb[LastMask].SelectionMap = Nil then DEMGlb[LastMask].SetUpMap(false)
      else DEMGlb[LastMask].SelectionMap.DoCompleteMapRedraw;
      {$IfDef SupClassAuxGrids} WriteLineToDebugFile('Map created'); {$EndIf}
   end;
   if MDDef.MaskMapShow in [1,2] then begin
      BaseMap.MapDraw.AssignSecondDEM(LastMask);
      BaseMap.MapDraw.DeleteSingleMapLayer(BaseMap.MapDraw.SecondGridfName);
      BaseMap.DoFastMapRedraw;
      BaseMap.Locationsonly1Click(Nil);
      {$IfDef SupClassAuxGrids} WriteLineToDebugFile('Overlay created'); {$EndIf}
   end;
end;



procedure TSupClassAuxGrids.CheckIfUserChangedAnything;
var
   i,NumGrids : integer;
begin
   HideFilterDEMs;
   MatchesNeeded := succ(RadioGroup1.ItemIndex);
   NumGrids := 0;
   for i := 1 to MaxGeomorpFilters do begin
      if (ClassDEMs[i] <> 0) then inc(NumGrids);
   end;
   if MatchesNeeded > NumGrids then MatchesNeeded := NumGrids;
   CheckEditString(Edit5.Text,MDDef.ExpandNeighborsRequired);
   CheckEditString(Edit2.Text,MDDef.ShrinkNeighborsRequired);
   MDDef.ExpandRadius := succ(RadioGroup4.ItemIndex);
   MDDef.ShrinkRadius := succ(RadioGroup5.ItemIndex);
end;


procedure TSupClassAuxGrids.BitBtn12Click(Sender: TObject);
var
   xg1,yg1,N,Band  : integer;
   z : float32;
   TStr : ShortString;
   Hist : array[1..MaxGeomorpFilters] of integer;
   i : Integer;

   function MeetsCriteria(xg1,yg1 : integer) : integer;
   var
      Band : integer;
   begin
       Result := 0;
       for Band := 1 to MaxGeomorpFilters do begin
          if UseBand[Band] and (ClassDEMs[Band] <> 0) then begin
             if DEMGlb[ClassDEMs[Band]].GetElevMeters(xg1,yg1,z) and (z >= LowVals[Band]) and (z <= HighVals[Band]) then begin
                inc(Result);
             end
             else if ReqBand[Band] then begin
                Result := 0;
                exit;
             end;
          end;
       end;
   end;


begin
   {$IfDef SupClassAuxGrids} WriteLineToDebugFile('TSupClassAuxGrids.BitBtn12Click (mask) in',true); {$EndIf}

   CheckIfUserChangedAnything;
   if NeedRecalc then begin
       if Memo1.Lines.Count > 0 then begin
           Memo4.Lines.Add('');
           Memo4.Lines.Add('==============================================');
           Memo4.Lines.Add('');
       end;

       for i := 0 to pred(Memo1.Lines.Count) do Memo4.Lines.Add(Memo1.Lines[i]);

       Memo1.Clear;
       if MDdef.OverWriteFeatureDBs and (LastMask <> 0) then begin
          CloseSingleDEM(LastMask);
          {$IfDef SupClassAuxGrids} WriteLineToDebugFile('Closed lastmask=' + IntToStr(LastMask));{$EndIf}
          LastMask := 0;
          ApplicationProcessMessages;
       end;

       LastMask := DEMGlb[ClassDEMs[1]].CloneAndOpenGridSetMissing(ByteDEM,'Mask',euIntCode);

       for n := 1 to MaxGeomorpFilters do Hist[n] := 0;
       {$IfDef SupClassAuxGrids} WriteLineToDebugFile('Mask setup over');{$EndIf}

       if ShowSatProgress then StartProgress('Masking');

       for xg1 := 0 to pred(DEMGlb[LastMask].DEMheader.NumCol) do begin
          if (xg1 mod 100 = 0) and ShowSatProgress then UpDateProgressBar(xg1/DEMGlb[LastMask].DEMheader.NumCol);
          for yg1 := 0 to pred(DEMGlb[LastMask].DEMheader.NumRow) do begin
             N := MeetsCriteria(xg1,yg1);
             if (N > 0) then begin
                inc(Hist[n]);
                if N >= MatchesNeeded then begin
                   if MDDef.FuzzyMatches then DEMGlb[LastMask].SetGridElevation(xg1,yg1,N)
                   else DEMGlb[LastMask].SetGridElevation(xg1,yg1,MDDef.MaskCode);
                end;
             end;
          end;
       end {for xg1};
       EndProgress;
       {$IfDef SupClassAuxGrids} WriteLineToDebugFile('Mask grid created'); {$EndIf}

       Memo1.Lines.Add('');
       Memo1.Lines.Add('Matches required: ' + IntToStr(MatchesNeeded));
       Memo1.Lines.Add('Using:');
       for Band := 1 to MaxGeomorpFilters do if UseBand[Band] and (ClassDEMs[Band] <> 0) then begin
          TStr := '  ' + DEMGlb[ClassDEMs[Band]].AreaName + ' ' + RealToString(LowVals[Band],-12,-2) +
               ' to ' + RealToString(HighVals[Band],-12,-2) + '  ' + StringGrid1.Cells[4,Band];
          if ReqBand[Band] then TStr := TStr + '  Req';
         {$IfDef SupClassAuxGrids} WriteLineToDebugFile(TStr); {$EndIf}
          Memo1.Lines.Add(TStr);
       end;
       Memo1.Lines.Add('');

       for n := 1 to MaxGeomorpFilters do if Hist[N] > 0 then  begin
          TStr := 'Grids matching ' + IntToStr(n) + IntegerToString(Hist[N],8);
          {$IfDef SupClassAuxGrids} WriteLineToDebugFile(TStr); {$EndIf}
          Memo1.Lines.Add(TStr);
       end;
       {$IfDef SupClassAuxGrids} WriteLineToDebugFile('Start through training set'); {$EndIf}
       EvaluateTrainingPoints;
   end;
   NeedRecalc := false;
   DrawMask;

   {$IfDef SupClassAuxGrids}WriteLineToDebugFile('TSupClassAuxGrids.BitBtn12Click out'); {$EndIf}
end;

procedure TSupClassAuxGrids.EvaluateTrainingPoints;
var
   Lat,Long : float64;
   z : float32;
   xg1,yg1,i,n,NPts : integer;
   ch : char;
   OK : boolean;
   ClassInDB : tStringList;
begin
   if (TrainingSetDB <> 0) and (GISDB[TrainingSetDB] <> Nil) then begin
      GISDB[TrainingSetDB].AddFieldToDataBase(ftString,'CLASS_GOOD',1);
      Memo1.Lines.Add('');
      GISDB[TrainingSetDB].DBFieldUniqueEntries('NAME',ClassInDB);
      for i := 0 to pred(ClassInDB.Count) do begin
          GISDB[TrainingSetDB].MyData.ApplyFilter('NAME=' + QuotedStr(ClassInDB[i]));
          GISDB[TrainingSetDB].EmpSource.Enabled := false;
          NPts := 0;
          while not GISDB[TrainingSetDB].MyData.eof do begin
              if GISDB[TrainingSetDB].ValidLatLongFromTable(Lat,Long) then begin
                 DEMGlb[ClassDEMs[1]].LatLongDegreeToDEMGridInteger(Lat,Long,xg1,yg1);
                 OK := DEMGlb[LastMask].GetElevMeters(xg1,yg1,z);
                 if OK and MDdef.FuzzyMatches then OK := z >= MatchesNeeded;

                 if OK then begin
                   inc(NPts);
                   if UpperCase(TheClass) = UpperCase(ClassInDB[i]) then ch := 'Y'
                   else ch := 'N';
                 end
                 else begin
                    if UpperCase(TheClass) <> UpperCase(ClassInDB[i]) then ch := 'Y'
                    else ch := 'N';
                 end;
                 GISDB[TrainingSetDB].MyData.Edit;
                 GISDB[TrainingSetDB].MyData.SetFieldByNameAsString('CLASS_GOOD',ch);
              end;
             GISDB[TrainingSetDB].MyData.Next;
          end;
          n := GISDB[TrainingSetDB].MyData.RecordCount;
          Memo1.Lines.Add(ClassInDB[i] + '  ' + IntToStr(Npts) + '/' + IntToStr(n) +
              RealToString(100 * Npts / n,8,2) + '%');
      end;
      GISDB[TrainingSetDB].MyData.ApplyFilter('');
      GISDB[TrainingSetDB].dbTablef.ShowStatus;
      ClassInDB.Free;
   end;
end;

procedure TSupClassAuxGrids.BitBtn13Click(Sender: TObject);
begin
   CheckUse(False);
end;

procedure TSupClassAuxGrids.BitBtn14Click(Sender: TObject);
begin
   GridMaskOptions;
   NeedRecalc := true;
end;


procedure TSupClassAuxGrids.BitBtn1Click(Sender: TObject);
var
   i : integer;
begin
   NeedRecalc := true;
   for i := 1 to MaxGeomorpFilters do begin
      if CheckLimits(i) then begin
         if (Sender = BitBtn1) or (Sender = BitBtn3) then begin
            ReadDefault('Min ' + DEMGlb[ClassDEMs[i]].AreaName,LowVals[i]);
            StringGrid1.Cells[2,i] := RealToString(LowVals[i],-12,-3);
         end;
         if (Sender = BitBtn1) or (Sender = BitBtn4) then begin
            ReadDefault('Max ' + DEMGlb[ClassDEMs[i]].AreaName,HighVals[i]);
            StringGrid1.Cells[3,i] := RealToString(HighVals[i],-12,-3);
         end;
      end {if Check};
   end {for i};
   LabelPercentiles;
end;


procedure TSupClassAuxGrids.BitBtn20Click(Sender: TObject);
begin
   if (TrainingSetDB = 0) or AnswerIsYes('Replace training set') then begin
      NeedRecalc := true;
      TrainingSetDB := BaseMap.OpenDBonMap('training set','');
   end;
end;

procedure TSupClassAuxGrids.BitBtn2Click(Sender: TObject);
begin
   PetDBUtils.HTMLReport('Grid settings',StringGrid1);
end;

procedure TSupClassAuxGrids.BitBtn3Click(Sender: TObject);
begin
   BitBtn1Click(Sender);
end;

procedure TSupClassAuxGrids.BitBtn4Click(Sender: TObject);
begin
   BitBtn1Click(Sender);
end;

procedure TSupClassAuxGrids.BitBtn5Click(Sender: TObject);
var
   sl : tStringList;
   i,db : integer;
   fName : PathStr;
   ch : char;
begin
   {$IfDef SupClassAuxGrids} WriteLineToDebugFile('TSupClassAuxGrids.BitBtn5Click (save DEM list) in'); {$EndIf}
   HideFilterDEMs;

   Sl := tStringList.Create;
   sl.Add('DEM_NAME,MIN,MAX,REQ');
   for I := 1 to MaxGeomorpFilters do if (ClassDEMs[i] <> 0) then begin
      if UseBand[i] or AnswerIsYes('Include ' + DEMGlb[ClassDEMs[i]].DEMFileName) then begin
         if ReqBand[i] then ch := 'Y' else ch := 'N';
         sl.Add(DEMGlb[ClassDEMs[i]].DEMFileName + ',' + RealToString(LowVals[i],-18,-2) + ',' +
               RealToString(HighVals[i],-18,-2) + ',' + ch);
      end;
   end;
   fName := LastClassSet;
   if GetFileNameDefaultExt('Grid list','CSV file|*.csv;*.dbf' ,FName) then begin
      fName := ChangeFileExt(fName,'.csv');
      {$IfDef SupClassAuxGrids} WriteLineToDebugFile('save, fname=' + fName);   {$EndIf}
      db := BaseMap.StringListToLoadedDatabase(sl,fname);
      GISdb[db].MyData.TrimAllStringFields;
      CloseAndNilNumberedDB(db);
   end
   else SL.Destroy;
   {$IfDef SupClassAuxGrids} WriteLineToDebugFile('TSupClassAuxGrids.BitBtn5Click out'); {$EndIf}
end;

procedure TSupClassAuxGrids.BitBtn6Click(Sender: TObject);
var
   i,db : integer;
   fName : PathStr;
begin
   NeedRecalc := true;
   db := OpenMultipleDataBases('List of grids','',false);
   if (db <> 0) then begin
      CloseMaskingDEMs;
      Caption := GISdb[db].dbName;
      LastClassSet := GISdb[db].dbFullName;
      StartGridSetup;
      I := 0;
      while not GISdb[db].MyData.eof do begin
         fName := GISdb[db].MyData.GetFieldByNameAsString('DEM_NAME');
         if FileExists(fName) then begin
            inc(i);
            ClassDEMs[i] := OpenNewDEM(fName,MDDef.OpenGridMaps);
            LowVals[i] := GISdb[db].MyData.GetFieldByNameAsFloat('MIN');
            HighVals[i] := GISdb[db].MyData.GetFieldByNameAsFloat('MAX');
            if GISdb[db].MyData.FieldExists('REQ') then ReqBand[i] := GISdb[db].MyData.GetFieldByNameAsString('REQ') = 'Y';
            if ReqBand[i] then begin
               case i of
                   1 : Req1.Checked := true;
                   2 : Req2.Checked := true;
                   3 : Req3.Checked := true;
                   4 : Req4.Checked := true;
                   5 : Req5.Checked := true;
                   6 : Req6.Checked := true;
                   7 : Req7.Checked := true;
                   8 : Req8.Checked := true;
                   9 : Req9.Checked := true;
                  10 : Req10.Checked := true;
               end;
            end;
            FillInDEMinGrid(ClassDEMs[i],i);
         end
         else begin
            MessageToContinue('Missing: ' + fName);
         end;
         GISdb[db].MyData.Next;
      end;
   end;
   CloseAndNilNumberedDB(db);
   FinishGridSetup;
end;

procedure TSupClassAuxGrids.BitBtn7Click(Sender: TObject);
var
   i : integer;
begin
   NeedRecalc := true;
   i := 1;
   while (i <= MaxGeomorpFilters) and (ClassDems[i] <> 0) do inc(i);
   if (i <= MaxGeomorpFilters) then begin
      ClassDEMs[i] := OpenNewDEM('',false);
      if ClassDEMs[i] <> 0 then begin
         LowVals[i] := DEMGlb[ClassDEMs[i]].DEMheader.MinElev;
         HighVals[i] := DEMGlb[ClassDEMs[i] ].DEMheader.MaxElev;
         FillInDEMinGrid(ClassDEMs[i],i);
         FinishGridSetup;
      end;
   end;
end;

procedure TSupClassAuxGrids.BitBtn8Click(Sender: TObject);
var
   fName : PathStr;
begin
    fName := ExtractFilePath(DEMGlb[LastMask].DEMFileName);
    DEMGlb[LastMask].WriteNewFormatDEM(fName);
    Memo1.Lines.SaveToFile(fName + '_metadata.txt');
end;

procedure TSupClassAuxGrids.BitBtn9Click(Sender: TObject);
var
   i : integer;
begin
   for I := 1 to MaxGeomorpFilters do begin
      if ClassDEMs[i] <> 0 then begin
         MultiGrid.ClassStatsForGrid(soGraph,ClassDEMs[i],TrainingSetDB);
      end;
   end;
end;


procedure TSupClassAuxGrids.HideFilterDEMs;
begin
   if InitialSetupOver then begin
      UseBand[1] := Use1.Checked;
      UseBand[2] := Use2.Checked;
      UseBand[3] := Use3.Checked;
      UseBand[4] := Use4.Checked;
      UseBand[5] := Use5.Checked;
      UseBand[6] := Use6.Checked;
      UseBand[7] := Use7.Checked;
      UseBand[8] := Use8.Checked;
      UseBand[9] := Use9.Checked;
      UseBand[10] := Use10.Checked;

      ReqBand[1] := Req1.Checked;
      ReqBand[2] := Req2.Checked;
      ReqBand[3] := Req3.Checked;
      ReqBand[4] := Req4.Checked;
      ReqBand[5] := Req5.Checked;
      ReqBand[6] := Req6.Checked;
      ReqBand[7] := Req7.Checked;
      ReqBand[8] := Req8.Checked;
      ReqBand[9] := Req9.Checked;
      ReqBand[10] := Req10.Checked;
   end;
end;

procedure TSupClassAuxGrids.CheckUse(Setting : boolean);
begin
   if InitialSetupOver then begin
      Use1.Checked := Setting;
      Use2.Checked := Setting;
      Use3.Checked := Setting;
      Use4.Checked := Setting;
      Use5.Checked := Setting;
      Use6.Checked := Setting;
      Use7.Checked := Setting;
      Use8.Checked := Setting;
      Use9.Checked := Setting;
      Use10.Checked := Setting;
   end;
end;


function MakeDEMMask(DEM : integer; Min,Max : float64; var NumPts : integer) : tMyBitmap;
var
   x,y : integer;
   z{,zavg}  : float32;
   i : integer;
   p0 : pRGB;
   fName : PathStr;
begin
   if (DEM=0) or (DEMGlb[DEM] = Nil) then begin
      exit;
   end;

   fName := GeomorphAtlasDir + DEMGlb[DEM].AreaName + '.bmp';
   {$IfDef TrackAtlas} WriteLineToDebugFile('MakeDEMMask for ' + DEMGlb[DEM].AreaName+'  fName = ' + fName+'  Range = ' + RealToString(Min,8,2) + ' to ' + RealToString(Max,8,2)); {$EndIf}
   CreateBitmap(Result,DEMGlb[DEM].DEMheader.NumCol,DEMGlb[DEM].DEMheader.NumRow);
   if FileExists(fName) then begin
      Result.LoadFromFile(fName);
   end
   else begin
      NumPts := 0;
      if abs(Max-Min) > 0.00000001 then begin
         StartProgressAbortOption('Map ' + DEMGlb[DEM].AreaName);
         y := 0;
         while y <= pred(DEMGlb[DEM].DEMheader.NumRow) do begin
            if (y mod 50 = 0) then UpDateProgressBar(y/pred(DEMGlb[DEM].DEMheader.NumRow));
            p0 := Result.ScanLine[pred(DEMGlb[DEM].DEMheader.NumRow)-y];
            x := 0;
            while x <= pred(DEMGlb[DEM].DEMheader.NumCol) do begin
               for i := 1 to MaxGeomorpFilters do begin
                  if (DEMGlb[DEM].GetElevMeters(x,y,z) and (z >= Min) and (z <=Max)) then begin
                     p0[x] := rgbTripleRed;
                     inc(NumPts);
                  end;
               end;
               inc(x);
            end;
            if WantOut then break;
            inc(y);
         end;
      end;

      Result.SaveToFile(fName);
      {$IfDef ExSats}
      {$Else}
      with DEMGlb[DEM],DEMheader do begin
         SaveWorldFile(FName,DEMxSpacing,DEMySpacing,DEMSWcornerLong,DEMSWcornerLat + LatSizeMap,'WGS84',1,'N');
      end;
      {$EndIf}

      EndProgress;
   end;
   {$IfDef TrackAtlas} WriteLineToDebugFile('MakeDEMMask out'); {$EndIf}
end;


procedure TSupClassAuxGrids.CancelBtnClick(Sender: TObject);
begin
   Close;
end;


procedure TSupClassAuxGrids.CheckBox11Click(Sender: TObject);
begin
    MDdef.OverWriteFeatureDBs := CheckBox11.Checked;
end;

procedure TSupClassAuxGrids.LabelPercentiles;
   var
      i : integer;
begin
   if InitialSetupOver then begin
      for I := 1 to MaxGeomorpFilters do if CheckLimits(i) then
         StringGrid1.Cells[4,i] := RealToString(DEMGlb[ClassDems[i]].PercentileofElevation(LowVals[i]),-18,-2) + '--' +
               RealToString(DEMGlb[ClassDems[i]].PercentileofElevation(HighVals[i]),-18,-2) + '%';
   end;
end;


procedure TSupClassAuxGrids.Edit4Change(Sender: TObject);
begin
   TheClass := Edit4.Text;
end;


procedure TSupClassAuxGrids.CloseMaskingDEMs;
var
   i : integer;
begin
   for i := 1 to MaxGeomorpFilters do if (ClassDEMS[i] <> 0) then CloseSingleDEM(ClassDEMS[i]);
   CloseSingleDEM(LastMask);
end;


procedure TSupClassAuxGrids.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
   CloseMaskingDEMs;
   ZeroGlobalVar;
end;


procedure TSupClassAuxGrids.FormCreate(Sender: TObject);
var
   i : integer;
begin
   {$IfDef SupClassAuxGrids} WriteLineToDebugFile('TSupClassAuxGrids.FormCreate in'); {$EndIf}
   WMDEM.FormPlacementInCorner(Self);
   for i := 1 to MaxGeomorpFilters do ClassDEMS[i] := 0;
   InitialSetupOver := false;
   LastMask := 0;
   CheckBox11.Checked := MDdef.OverWriteFeatureDBs;
   CheckBox8.Checked := MDdef.OpenGridMaps;
   Edit2.Text := IntToStr(MDDef.ShrinkNeighborsRequired);
   Edit5.Text := IntToStr(MDDef.ExpandNeighborsRequired);
   RadioGroup4.ItemIndex := pred(MDDef.ExpandRadius);
   RadioGroup5.ItemIndex := pred(MDDef.ShrinkRadius);

   NeedRecalc := true;
   {$IfDef SupClassAuxGrids} WriteLineToDebugFile('TSupClassAuxGrids.FormCreate out'); {$EndIf}
end;


procedure TSupClassAuxGrids.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html/atlas_classify.htm');
end;


procedure TSupClassAuxGrids.BitBtn15Click(Sender: TObject);
var
   NewHeadRecs : tDEMheader;
   xg1,yg1,NPts,NewMask     : integer;
   z : float32;


   function MeetsCriteria(xg1,yg1 : integer) : boolean;
   var
      Band : integer;
   begin
       Result := false;
       for Band := 1 to MaxGeomorpFilters do begin
          if ReqBand[Band] and (ClassDEMs[Band] <> 0) then begin
             if DEMGlb[ClassDEMs[Band]].GetElevMeters(xg1,yg1,z) and (z >= LowVals[Band]) and (z <= HighVals[Band]) then begin
             end
             else begin
                exit;
             end;
          end;
       end;
       Result := true;
   end;


begin
   {$IfDef SupClassAuxGrids} WriteLineToDebugFile('TSupClassAuxGrids.BitBtn15Click (expand) in'); {$EndIf}

   CheckIfUserChangedAnything;

   NewHeadRecs := DEMGlb[ClassDEMs[1]].DEMheader;
   NewHeadRecs.DEMPrecision := byteDEM;
   NewHeadRecs.ElevUnits := euIntCode;
   if not OpenAndZeroNewDEM(true,NewHeadRecs,NewMask,'New mask',InitDEMMissing) then exit;
   NPts := 0;
   if ShowSatProgress then StartProgress('Masking');

   for xg1 := 0 to pred(DEMGlb[LastMask].DEMheader.NumCol) do begin
      if (xg1 mod 100 = 0) and ShowSatProgress then UpDateProgressBar(xg1/DEMGlb[LastMask].DEMheader.NumCol);
      for yg1 := 0 to pred(DEMGlb[LastMask].DEMheader.NumRow) do begin
         if (Sender = BitBtn18) then begin
            if DEMGlb[LastMask].GetElevMeters(xg1,yg1,z) then begin
                if DEMGlb[LastMask].ValidNeighborsInBox(xg1,yg1,MDdef.ExpandRadius) > MDDef.ExpandNeighborsRequired then begin
                   DEMGlb[NewMask].SetGridElevation(xg1,yg1,MDDef.MaskCode);
                end
                else inc(NPts);
            end;
         end
         else begin
             if DEMGlb[LastMask].GetElevMeters(xg1,yg1,z) then DEMGlb[NewMask].SetGridElevation(xg1,yg1,z)
             else begin
                if MeetsCriteria(xg1,yg1) then begin
                   if DEMGlb[LastMask].ValidNeighborsInBox(xg1,yg1,MDDef.ShrinkRadius) > MDDef.ShrinkNeighborsRequired then begin
                      DEMGlb[NewMask].SetGridElevation(xg1,yg1,MDDef.MaskCode);
                      inc(NPts);
                   end;
                end;
             end;
         end;
      end;
   end {for xg1};
   EndProgress;
   {$IfDef SupClassAuxGrids} WriteLineToDebugFile('Mask grid created'); {$EndIf}
   CloseSingleDEM(LastMask);
   LastMask := NewMask;
   DrawMask;

   Memo1.Lines.Add('');
   if (Sender = BitBtn18) then begin
      Memo1.Lines.Add('Pts deleted (neighbors<' + IntToStr(MDDef.ShrinkNeighborsRequired) +  ',  rad=' + IntToStr(MDDef.ShrinkRadius)  + ')  ' + IntToStr(Npts));
   end
   else begin
      Memo1.Lines.Add('Pts expanded (neighbors<' + IntToStr(MDDef.ExpandNeighborsRequired) +  ',  rad=' + IntToStr(MDDef.ExpandRadius)  + ')  ' + IntToStr(Npts));
   end;

   EvaluateTrainingPoints;
   {$IfDef SupClassAuxGrids} WriteLineToDebugFile('TSupClassAuxGrids.BitBtn15Click out'); {$EndIf}
end;


procedure TSupClassAuxGrids.BitBtn16Click(Sender: TObject);
var
   i,j,xg1,yg1,N,NPts     : integer;
   Lat,Long : float64;
   z : float32;
   TStr : ShortString;
   FName : PathStr;
   ClasseInDB,Results : tStringList;
begin
   {$IfDef SupClassAuxGrids} WriteLineToDebugFile('TSupClassAuxGrids.BitBtn16Click (mask) in'); {$EndIf}

   CheckIfUserChangedAnything;

   if (TrainingSetDB <> 0) and (GISDB[TrainingSetDB] <> Nil) then begin
      GISDB[TrainingSetDB].DBFieldUniqueEntries('NAME',ClasseInDB);
      Memo1.Lines.Add('');
      Results := tStringList.Create;
      TStr := 'NAME';
      for i := 0 to pred(ClasseInDB.Count) do TStr := TStr + ',' + ClasseInDB[i];
      Results.Add(TStr);
      for j := 1 to MaxGeomorpFilters do if ClassDEMs[j] <> 0 then begin
        TStr := DEMGlb[ClassDEMs[j]].AreaName;
        for i := 0 to pred(ClasseInDB.Count) do begin
            GISDB[TrainingSetDB].MyData.ApplyFilter('NAME=' + QuotedStr(ClasseInDB[i]));
            GISDB[TrainingSetDB].EmpSource.Enabled := false;
            NPts := 0;
            while not GISDB[TrainingSetDB].MyData.eof do begin
                if GISDB[TrainingSetDB].ValidLatLongFromTable(Lat,Long) then begin
                   DEMGlb[ClassDEMs[1]].LatLongDegreeToDEMGridInteger(Lat,Long,xg1,yg1);
                   if DEMGlb[ClassDEMs[j]].GetElevMeters(xg1,yg1,z) and (z >= LowVals[j]) and (z <= HighVals[j]) then inc(NPts);
                end;
                GISDB[TrainingSetDB].MyData.Next;
            end;
            n := GISDB[TrainingSetDB].MyData.RecordCount;
            TStr := TStr + ',' + RealToString(100 * Npts / n,8,2) + '%';
         end;
         Results.Add(TStr);
      end;
      GISDB[TrainingSetDB].MyData.ApplyFilter('');
      GISDB[TrainingSetDB].dbTablef.ShowStatus;
      fName := Petmar.NextFileNumber(MDTempDir, 'temp_','csv');
      BaseMap.StringListToLoadedDatabase(Results,fName);
   end;
   {$IfDef SupClassAuxGrids} WriteLineToDebugFile('TSupClassAuxGrids.BitBtn12Click out'); {$EndIf}
 end;

procedure TSupClassAuxGrids.BitBtn17Click(Sender: TObject);
begin
   Memo4.Clear;
end;

procedure TSupClassAuxGrids.BitBtn18Click(Sender: TObject);
begin
   {$IfDef SupClassAuxGrids} WriteLineToDebugFile('TSupClassAuxGrids.BitBtn18Click (shrink) in',true); {$EndIf}
   BitBtn15Click(Sender);
end;

procedure TSupClassAuxGrids.BitBtn19Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := ExtractFilePath(LastClassSet);
   if Petmar.GetFileNameDefaultExt('classification history','*.*',fName) then Memo4.Lines.SaveToFile(fName);
end;


initialization
   ZeroGlobalVar;
finalization
   {$IfDef SupClassAuxGrids} WriteLineToDebugFile('SupClassAuxGrids active in sup_class_aux_grids'); {$EndIf}
end.
       *
