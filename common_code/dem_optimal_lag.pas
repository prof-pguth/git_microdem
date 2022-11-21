unit dem_optimal_lag;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}



{$I nevadia_defines.inc}

{$Define NoParallelFor}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$IfDef Debug}
      //{$Define RecordFullLagProblems}
      //{$Define RecordLagProblems}
   {$EndIf}
{$EndIf}


interface

uses
  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids, Buttons,
  System.IOUtils,System.UITypes,
  System.Threading,System.SyncObjs,
  Petmar_types,DEMDefs,DEMMapf;

type
  TLagOptionsForm = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    HelpBtn: TBitBtn;
    OKBtn: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn1: TBitBtn;
    CheckBox1: TCheckBox;
    Edit6: TEdit;
    Edit5: TEdit;
    Label2: TLabel;
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    Label4: TLabel;
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit2: TEdit;
    Label3: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  private
    { Private declarations }
      procedure CheckChanges;
  public
    { Public declarations }
    MainDEM,SubDEM : integer;
    procedure Sensitivty(Lat, Long: float64);
  end;

procedure GetOptimalLagParameters(BaseMap : tMapForm; OtherDEM : integer);

var
   LagOptionsForm : TLagOptionsForm;

implementation

{$R *.dfm}

uses
   Petmar,PetMath,SystemCriticalU,
   BaseGraf,Thread_timers,
   Petmar_db,
   DEM_Manager,
   DEMdatabase,
   DEMDef_routines,
   Nevadia_main,
   DEMCoord,DEM_indexes,DEMstat;


procedure GetOptimalLagParameters(BaseMap : tMapForm; OtherDEM : integer);
var
   SubDEM : integer;
begin
   LagOptionsForm := TLagOptionsForm.Create(Application);
   LagOptionsForm.MainDEM := BaseMap.MapDraw.DEMonMap;
   LagOptionsForm.SubDEM := OtherDEM;
   LagOptionsForm.CheckBox1.Checked := (SubDEM = 0);
   LagOptionsForm.Label3.Caption := 'Fixed: ' + DEMGlb[BaseMap.MapDraw.DEMonMap].AreaName;
   if (LagOptionsForm.SubDEM <> 0) then LagOptionsForm.Label4.Caption := 'Shift: ' + DEMGlb[LagOptionsForm.SubDEM].AreaName;
   LagOptionsForm.Show;
end;


procedure TLagOptionsForm.CheckChanges;
begin
   CheckEditString(Edit3.Text,MDDef.ShiftLoX);
   CheckEditString(Edit4.Text,MDDef.ShiftHighX);
   CheckEditString(Edit2.Text,MDDef.ShiftLoY);
   CheckEditString(Edit1.Text,MDDef.ShiftHighY);
   CheckEditString(Edit5.Text,MDDef.LagSearchRadius);
   CheckEditString(Edit6.Text,MDDef.LagCenterShift);
end;



procedure TLagOptionsForm.BitBtn1Click(Sender: TObject);
var
   BigResults : tStringList;
   fName : PathStr;
var
   DEM,db,i : integer;
   Findings,DEMs : tStringList;
   MomentVar : tMomentVar;
   BoxLimits: tGridLimits;
   TStr : shortstring;
begin
  try
      CheckChanges;
      SaveMDdefaults;
      BigResults := Nil;
      if MDDef.GeomorphMapsFullDEM then begin
         BoxLimits := DEMGlb[MainDEM].FullDEMGridLimits;
      end
      else begin
         BoxLimits := DEMGlb[MainDEM].SelectionMap.MapDraw.MapAreaDEMGridLimits;
      end;

      if CheckBox1.Checked then begin
         DEMs := tStringList.Create;
         for DEM := 1 to MaxDEMDataSets do begin
           if (DEM <> MainDEM) and ValidDEM(DEM) then begin
              Memo1.Lines.Add(DEMGlb[DEM].AreaName + '  ' + TimeToStr(Now));
              OneLag(MainDEM,DEM,BoxLimits,BigResults);
              DEMs.Add(DEMGlb[DEM].AreaName);
           end;
         end;
         fName := System.IOUtils.TPath.Combine(MDTempDir, ptTrim(DEMGlb[MainDEM].AreaName) + '_shift_' + IntToStr(MDDef.LagSearchRadius) + '.dbf');
         db := DEMGlb[MainDEM].SelectionMap.StringListToLoadedDatabase(BigResults,fName);
         Findings := tStringList.Create;
         Findings.Add('Shift from ' + DEMGlb[MainDEM].AreaName);
         Findings.Add('');
         for i := 0 to pred(DEMs.Count) do begin
            GISDB[db].ApplyGISFilter('DEM=' + QuotedStr(DEMs.Strings[i]));
            MomentVar := GISdb[db].GetFieldStatistics('TOTAL_LAG');
            TStr := DEMs.strings[i] + ' avg lag=' + RealToString(MomentVar.mean,-18,-4) + ' std lag=' + RealToString(MomentVar.sdev,-18,-4) + '  max lag=' + RealToString(MomentVar.MaxZ,-18,-4) + ' npts=' + IntToStr(MomentVar.NPts);
            Findings.Add(TStr);
            Memo1.Lines.Add(TStr);
         end;
         GISDB[db].ClearGISFilter;
         DisplayAndPurgeStringList(Findings,'Shift from ' + DEMGlb[MainDEM].AreaName);
      end
      else begin
         OneLag(MainDEM,SubDEM,BoxLimits,BigResults);
      end;
  finally
  end;
end;


procedure TLagOptionsForm.BitBtn2Click(Sender: TObject);
begin
   ChangeDEMNowDoing(LagSizeSensitivity);
end;


procedure TLagOptionsForm.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   Action := caFree;
end;


procedure TLagOptionsForm.Sensitivty(Lat,Long : float64);
var
   CorrelationMatrix : tStringList;
   fName : PathStr;
   db : integer;

   procedure Region(LagsOut : integer; AddToTable : boolean = true);
   var
      x,y : integer;
      GridLimits : tGridLimits;
      x1,y1,x2,y2,xlag,ylag,Npts : integer;
      MaxR,NoLagR,zRange,avgSlope,BestA,BestB : float64;
   begin
     {$IfDef RecordLagProblems} WriteLineToDebugFile('Region=' + IntToStr(LagsOut)); {$EndIf}
      DEMGlb[MainDEM].LatLongDegreeToDEMGridInteger(Lat,Long,x,y);
      GridLimits.XGridLow := x - LagsOut;
      GridLimits.XGridHigh := x + LagsOut;
      GridLimits.YGridLow := y - LagsOut;
      GridLimits.YGridHigh := y + LagsOut;

      DEMGlb[MainDEM].SelectionMap.MapDraw.DEMGridToScreen(GridLimits.XGridLow,GridLimits.YGridHigh,x1,y1);
      DEMGlb[MainDEM].SelectionMap.MapDraw.DEMGridToScreen(GridLimits.XGridHigh,GridLimits.YGridLow,x2,y2);
      DEMGlb[MainDEM].SelectionMap.Image1.Canvas.Pen.Color := clRed;
      DEMGlb[MainDEM].SelectionMap.Image1.Canvas.Pen.Width := 2;
      DEMGlb[MainDEM].SelectionMap.Image1.Canvas.Brush.Style := bsClear;
      DEMGlb[MainDEM].SelectionMap.Image1.Canvas.Rectangle(x1,y1,x2,y2);

      Lag_and_Shift(x,y,MainDEM,SubDEM,GridLimits,NPts,xLag,YLag,MaxR,NoLagR,zRange,AvgSlope,BestA,BestB,CorrelationMatrix);

      if AddToTable then Memo1.Lines.Add(IntegerToString(LagsOut,4) + RealToString(2*LagsOut*DEMGlb[MainDEM].AverageSpace,8,1) +
          RealToString(MaxR,8,4) + IntegerToString(xLag,4) + IntegerToString(yLag,6) + RealToString(sqrt(sqr(xlag)+sqr(ylag)),6,2)  + RealToString(zRange,8,1) + RealToString(AvgSlope,8,2)
          + RealToString(BestA,15,4) + RealToString(BestB,15,4)   );
   end;

begin
  {$IfDef RecordLagProblems} WriteLineToDebugFile('TLagOptionsForm.Sensitivty in'); {$EndIf}
   CheckChanges;
   Memo1.Lines.Add(LatLongDegreeToString(Lat,Long) );
   Memo1.Lines.Add('Pixels  Meters  r2    Xlag  ylag  lag  relief avg_slope   Intercept-A     Slope_B');
   CorrelationMatrix := Nil;
   Region(3);
   Region(4);
   Region(5);
   Region(7);
   Region(10);
   Region(12);
   Region(15);
   Region(20);
   Region(25);
   Region(50);
   Region(75);
   Region(100);
   Memo1.Lines.Add('=========================');
   Memo1.Lines.Add('');

   CorrelationMatrix := tStringList.Create;
   Region(MDDef.LagSearchRadius,false);

   fName := Petmar.NextFileNumber(MDTempDir, 'Lag_correlation_map_', DefaultDBExt);
   db := DEMGlb[MainDEM].SelectionMap.StringListToLoadedDatabase(CorrelationMatrix,fName);
   GISdb[db].dbOpts.DBAutoShow := dbasColorByNumeric;
   GISdb[db].dbOpts.FloatColorField := 'R2';
   GISdb[db].RedrawLayerOnMap;

  {$IfDef RecordLagProblems} WriteLineToDebugFile('TLagOptionsForm.Sensitivty out'); {$EndIf}
end;


procedure TLagOptionsForm.OKBtnClick(Sender: TObject);
begin
   Close;
end;


procedure TLagOptionsForm.RadioGroup1Click(Sender: TObject);
begin
  MDDef.GeomorphMapsFullDEM := RadioGroup1.ItemIndex = 0;
end;

procedure TLagOptionsForm.FormCreate(Sender: TObject);
begin
    if MDDef.GeomorphMapsFullDEM then RadioGroup1.ItemIndex := 0 else RadioGroup1.ItemIndex := 1;
    Edit3.Text := IntToStr(MDdef.ShiftLoX);
    Edit4.Text := IntToStr(MDdef.ShiftHighX);
    Edit2.Text := IntToStr(MDdef.ShiftLoY);
    Edit1.Text := IntToStr(MDdef.ShiftHighY);
    Edit5.Text := IntToStr(MDDef.LagSearchRadius);
    Edit6.Text := IntToStr(MDDef.LagCenterShift);
    wmDEM.FormPlacementInCorner(Self,lpNEMap);
end;


procedure TLagOptionsForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\find_optimal_lag.htm');
end;

initialization
finalization
   {$IfDef RecordLagProblems} WriteLineToDebugFile('RecordLagProblems active in dem_optimal_lag');  {$EndIf}
   {$IfDef RecordFullLagProblems} WriteLineToDebugFile('RecordFullLagProblems active in dem_optimal_lag'); {$EndIf}
end.




//removed from  ASTER_Lag_and_Shift
   if false then begin
       if (not AutomaticRun) and AnswerIsYes('Lag map') then with MDDef.ShiftDEMControls do begin
          Results.Add('Create lag map');
          MDDef.DoLagMap := true;
          XWhereMaxR := 0;
          YWhereMaxR := 0;
          LagLimit := PassSize[1];
          SampInc := PassThin[1];
          ReadDefault('Lag limit',LagLimit);
          ReadDefault('Sampling increment',SampInc);
          APass(1,LagLimit,SampInc);
       end;

       with DEMGLB[SubDEM],HeadRecs do begin
          if MDDef.ShiftDEMControls.DifferenceMaps and (not AutomaticRun) then MakeDifferenceMapOfBoxRegion(MainDEM,SubDEM,GridLimits,true,false,true,'Original differences');

          if AutomaticRun or AnswerIsYes('Horizontally Shift DEM ' + AreaName) then begin
             hdfSWCornerx := hdfSWCornerx - XWhereMaxR * fLongInterval;
             hdfSWCornerY := hdfSWCornerY - YWhereMaxR * fLatInterval;
             AreaName := 'Horiz shift ' + AreaName;
             DefineDEMVariables(true);
             if MDDef.ShiftDEMControls.DifferenceMaps and (not AutomaticRun) then MakeDifferenceMapOfBoxRegion(MainDEM,SubDEM,GridLimits,true,false,true,'Horizontal shift differences');

             if AutomaticRun or AnswerIsYes('Vertically Shift DEM ' + AreaName) then begin
                AreaName := 'Horiz/Vert shift ' + AreaName;
                for xs := 0 to pred(NumCol) do begin
                   for ys := 0 to pred(NumRow) do begin
                      if GetElevMeters(xs,ys,z) then SetGridElevation(xs,ys,z - AverageDiff);
                   end;
                end;
                if MDDef.ShiftDEMControls.DifferenceMaps and (not AutomaticRun) then MakeDifferenceMapOfBoxRegion(MainDEM,SubDEM,GridLimits,true,false,true,'Full shift differences');
             end;
             if (not AutomaticRun) then SelectionMap.DoFastMapRedraw;
          end;
       end;
       if (Results <> Nil) then Petmar.DisplayAndPurgeStringList(Results,'Optimal lag');
   end;

