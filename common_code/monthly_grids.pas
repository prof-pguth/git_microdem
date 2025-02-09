
unit monthly_grids;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordMonthlyFilter}
   //{$Define MultiGridMapSize}
   //{$Define RecordMultiGrids}
{$EndIf}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons,
  Vcl.ComCtrls,
  DEMDefs;

type
  TGridTimeSeriesControlForm = class(TForm)
    BitBtn22: TBitBtn;
    RadioGroup8: TRadioGroup;
    BitBtn21: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    BitBtn1: TBitBtn;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    RadioGroup1: TRadioGroup;
    CheckBox7: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    Timer1: TTimer;
    BitBtn6: TBitBtn;
    TrackBar1: TTrackBar;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    BitBtn9: TBitBtn;
    procedure RadioGroup8Click(Sender: TObject);
    procedure BitBtn22Click(Sender: TObject);
    procedure BitBtn21Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn6Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
  private
    { Private declarations }
    UseGrid : array[1..MaxMultiGrid] of boolean;
  public
    { Public declarations }
  end;

var
  GridTimeSeriesControlForm: TGridTimeSeriesControlForm;


procedure StartMontlyTimeSeries;



implementation

{$R *.dfm}

uses
   Multigrid,DEMDatabase,DEMMapf,DEMCoord,DEM_Manager,
   Petmar,Petmath,Petmar_types,
   Nevadia_Main;


procedure StartMontlyTimeSeries;

      procedure EnableCheckBox(CheckBox : tCheckBox; MG : integer);
      begin
         CheckBox.Visible := MultiGridArray[MG] <> Nil;
         CheckBox.Checked := MultiGridArray[MG] <> Nil;
         if CheckBox.Visible then begin
            CheckBox.Caption := MultiGridArray[MG].MG_Name;
            if (MultiGridArray[MG].MapOwner <> Nil) then begin
              {$If Defined(MultiGridMapSize)} WriteLineToDebugFile('EnableCheckBox 1, ' + MultiGridArray[MG].MapOwner.MapDraw.MapSizeString); {$EndIf}
               MultiGridArray[MG].MapOwner.MapDraw.MapType :=  mtElevSpectrum;
               MultiGridArray[MG].MapOwner.MapDraw.MapXSize := 800;
               MultiGridArray[MG].MapOwner.MapDraw.MapYSize := 800;
               MultiGridArray[MG].MapOwner.MapDraw.MapAspectCheck;
              {$If Defined(MultiGridMapSize) } WriteLineToDebugFile('EnableCheckBox 2 ' + MultiGridArray[MG].MapOwner.MapDraw.MapSizeString); {$EndIf}
               MultiGridArray[MG].MapOwner.DoCompleteMapRedraw;
              {$If Defined(MultiGridMapSize) } WriteLineToDebugFile('EnableCheckBox 3 ' + MultiGridArray[MG].MapOwner.MapDraw.MapSizeString); {$EndIf}
            end;
         end;
      end;

      procedure EnableDBCheckBox(CheckBox : tCheckBox; DB : integer);
      begin
         CheckBox.Visible := MonthlyDBArray[DB] <> Nil;
         CheckBox.Checked := MonthlyDBArray[DB] <> Nil;
         if CheckBox.Visible then begin
            CheckBox.Caption := MonthlyDBArray[DB].DB_Name;
         end;
      end;


begin
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('StartMontlyTimeSeries in'); {$EndIf}
   if (GridTimeSeriesControlForm = Nil) then begin
      {$IfDef RecordMultiGrids} WriteLineToDebugFile('StartMontlyTimeSeries form created'); {$EndIf}
      GridTimeSeriesControlForm := TGridTimeSeriesControlForm.Create(Application);
   end;
   EnableCheckBox(GridTimeSeriesControlForm.CheckBox1,1);
   EnableCheckBox(GridTimeSeriesControlForm.CheckBox2,2);
   EnableCheckBox(GridTimeSeriesControlForm.CheckBox3,3);
   EnableCheckBox(GridTimeSeriesControlForm.CheckBox8,4);
   EnableCheckBox(GridTimeSeriesControlForm.CheckBox9,5);
   EnableDBCheckBox(GridTimeSeriesControlForm.CheckBox4,1);
   EnableDBCheckBox(GridTimeSeriesControlForm.CheckBox5,2);
   EnableDBCheckBox(GridTimeSeriesControlForm.CheckBox6,3);
   GridTimeSeriesControlForm.Show;
   GridTimeSeriesControlForm.RadioGroup8Click(nil);
   UpdateMenusForAllMaps;
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('StartMontlyTimeSeries out'); {$EndIf}
end;


procedure TGridTimeSeriesControlForm.BitBtn1Click(Sender: TObject);
begin
   wmDEM.PopUpMenu8.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure TGridTimeSeriesControlForm.BitBtn21Click(Sender: TObject);
begin
   If RadioGroup8.ItemIndex = 0 then RadioGroup8.ItemIndex := 11
   else RadioGroup8.ItemIndex := RadioGroup8.ItemIndex - 1
end;


procedure TGridTimeSeriesControlForm.BitBtn22Click(Sender: TObject);
begin
   If (RadioGroup8.ItemIndex = 11) then RadioGroup8.ItemIndex := 0
   else RadioGroup8.ItemIndex := RadioGroup8.ItemIndex + 1;
end;


procedure TGridTimeSeriesControlForm.BitBtn2Click(Sender: TObject);
var
  i: Integer;
begin
   for i := 1 to MaxMultiGrid do
      if (MultiGridArray[i] <> Nil) and UseGrid[i] then
         MultiGridArray[i].SumGrids;
   BitBtn2.Enabled := false;
end;

procedure TGridTimeSeriesControlForm.BitBtn3Click(Sender: TObject);
var
  i: Integer;
begin
   for i := 1 to MaxMultiGrid do
      if (MultiGridArray[i] <> Nil) and UseGrid[i] then
         MultiGridArray[i].CreateAverageValueGrid;
   BitBtn3.Enabled := false;
end;

procedure TGridTimeSeriesControlForm.BitBtn4Click(Sender: TObject);
begin
   CloseAllMultigrids;
   StartMontlyTimeSeries;
end;


procedure TGridTimeSeriesControlForm.BitBtn5Click(Sender: TObject);
begin
   Timer1.Enabled := true;
end;

procedure TGridTimeSeriesControlForm.BitBtn6Click(Sender: TObject);
begin
   Timer1.Enabled := false;
end;

procedure TGridTimeSeriesControlForm.BitBtn7Click(Sender: TObject);
var
  i: Integer;
begin
   for i := 1 to MaxMultiGrid  do
      if (MultiGridArray[i] <> Nil) and UseGrid[i] then
         MultiGridArray[i].CreateMinMaxRange;
end;

procedure TGridTimeSeriesControlForm.BitBtn8Click(Sender: TObject);
var
  i: Integer;
begin
   for i := 1 to  MaxMultiGrid do
      if (MultiGridArray[i] <> Nil) and UseGrid[i] then
         MultiGridArray[i].CreateMinMaxMonthGrids;
end;

procedure TGridTimeSeriesControlForm.BitBtn9Click(Sender: TObject);
var
  i,MaskGrid,Band : Integer;
  fName : PathStr;
begin
   if GetDEM(MaskGrid,true,'masking grid') then begin
      for i := 1 to  MaxMultiGrid do begin
         if (MultiGridArray[i] <> Nil) and UseGrid[i] then begin
            for Band := 1 to MultiGridArray[i].NumGrids do begin
               ParallelRowsDone := 0;
               EditsDone := 0;
               MaskGridFromSecondGrid(MultiGridArray[i].Grids[Band],MaskGrid,msSecondMissing);
               DEMGlb[MultiGridArray[i].Grids[Band]].CheckMaxMinElev;
               fName := ChangeFileExt(DEMGlb[MultiGridArray[i].Grids[Band]].DEMFileName,'.dem');
               DEMGlb[MultiGridArray[i].Grids[Band]].WriteNewFormatDEM(fName);
            end;
         end;
      end;
   end;
end;

procedure TGridTimeSeriesControlForm.CheckBox1Click(Sender: TObject);
begin
   UseGrid[1] := CheckBox1.Checked;
end;

procedure TGridTimeSeriesControlForm.CheckBox2Click(Sender: TObject);
begin
   UseGrid[2] := CheckBox2.Checked;
end;

procedure TGridTimeSeriesControlForm.CheckBox3Click(Sender: TObject);
begin
   UseGrid[3] := CheckBox3.Checked;
end;

procedure TGridTimeSeriesControlForm.CheckBox8Click(Sender: TObject);
begin
   UseGrid[4] := CheckBox8.Checked;
end;

procedure TGridTimeSeriesControlForm.CheckBox9Click(Sender: TObject);
begin
   UseGrid[5] := CheckBox9.Checked;
end;

procedure TGridTimeSeriesControlForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Timer1.Enabled := false;
end;

procedure TGridTimeSeriesControlForm.FormCreate(Sender: TObject);
begin
   wmdem.FormPlacementInCorner(Self);
   UseGrid[5] := true;
   UseGrid[4] := true;
   UseGrid[3] := true;
   UseGrid[2] := true;
   UseGrid[1] := true;
   BitBtn9.Visible := MDDef.ProgramOption = ExpertProgram;
end;

procedure TGridTimeSeriesControlForm.RadioGroup8Click(Sender: TObject);
var
   j,Month : integer;

   procedure aMonthlyDBArray(WhichOne : integer);
   var
      i : integer;
   begin
      if (MonthlyDBArray[WhichOne] <> Nil) and (MonthlyDBArray[WhichOne].CurrentMonth <> Month) then begin
         for i := 1 to 12 do begin
            GISDB[MonthlyDBArray[WhichOne].DBs[i]].dbPlotted := false;
            GISDB[MonthlyDBArray[WhichOne].DBs[i]].LayerIsOn:= false;
         end;
         MonthlyDBArray[WhichOne].CurrentMonth := Month;
         GISDB[MonthlyDBArray[WhichOne].DBs[Month]].dbPlotted := true;
         GISDB[MonthlyDBArray[WhichOne].DBs[Month]].LayerIsOn := true;
         GISDB[MonthlyDBArray[WhichOne].DBs[Month]].dbOpts.DBLegendLocation.DrawItem := CheckBox7.Checked;
         GISDB[MonthlyDBArray[WhichOne].DBs[Month]].dbOpts.DBLegendLocation.MapPosition := lpNEMap;
         GISDB[MonthlyDBArray[WhichOne].DBs[Month]].dbOpts.VectorThinFactor := succ(RadioGroup1.ItemIndex);
         MonthlyDBArray[WhichOne].MapOwner.MapDraw.BaseTitle := MonthName[Month] + ' ' + MonthlyDBArray[WhichOne].LongName;
         GISDB[MonthlyDBArray[WhichOne].DBs[Month]].RedrawLayerOnMap;
      end;
   end;

   procedure aMonthlyMultiGrid(WhichOne : integer);
   begin
      if (MultiGridArray[WhichOne] <> Nil) then MultiGridArray[WhichOne].MapOwner.SetMultibandToShowOnMap(Month);
   end;

   procedure DisableControls(Setting : boolean);
   begin
      RadioGroup8.Enabled := Setting;
      BitBtn21.Enabled := Setting;
      BitBtn22.Enabled := Setting;
   end;


begin
   DisableControls(false);
   ThinToPlot := succ(RadioGroup1.ItemIndex);
   Month := succ(RadioGroup8.ItemIndex);
   MDDef.GridLegendLocation.DrawItem := CheckBox7.Checked;
   MDDef.GridLegendLocation.MapPosition := lpNEMap;
   if CheckBox1.Checked then aMonthlyMultiGrid(1);
   if CheckBox2.Checked then aMonthlyMultiGrid(2);
   if CheckBox3.Checked then aMonthlyMultiGrid(3);
   if CheckBox8.Checked then aMonthlyMultiGrid(4);
   if CheckBox9.Checked then aMonthlyMultiGrid(5);

   if CheckBox4.Checked then aMonthlyDBArray(1);
   if CheckBox5.Checked then aMonthlyDBArray(2);
   if CheckBox6.Checked then aMonthlyDBArray(3);

   for j := 1 to MaxDataBase do begin
      if (GISdb[j] <> nil) and GISdb[j].LayerIsOn and
          ( (GISdb[j].dbOpts.TimeFilter <> '') or (GISdb[j].MyData.FieldExists(GISdb[j].MonthFieldName)) or (GISdb[j].dbOpts.DBAutoShow = dbasVector) ) then begin
         {$IfDef RecordMonthlyFilter} WriteLineToDebugFile('Monthly filter ' + IntToStr(Month) + ' for ' + GISdb[j].dbName); {$EndIf}
         GISdb[j].MakeNewMonthlyFilterAndRedraw(Month);
         GISdb[j].TheMapOwner.Image1.Canvas.Font.Size := 18;
         GISdb[j].TheMapOwner.Image1.Canvas.TextOut(5,5,MonthName[Month]);
      end;
   end;
   ThinToPlot := 1;
   DisableControls(true);
   RadioGroup1.Enabled := (NumOpenMonthlyDBArray > 0);
end;


procedure TGridTimeSeriesControlForm.Timer1Timer(Sender: TObject);
begin
   BitBtn22Click(Sender);
end;

procedure TGridTimeSeriesControlForm.TrackBar1Change(Sender: TObject);
begin
   Timer1.Interval := 100 * TrackBar1.Position;
end;

initialization
   GridTimeSeriesControlForm := Nil;
finalization
   {$IfDef RecordMonthlyFilter} WriteLineToDebugFile('RecordMonthlyFilter active in monthly_grids'); {$EndIf}
end.
