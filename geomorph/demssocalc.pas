unit demssocalc;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IFDEF DEBUG}
   //{$Define NoParallelFor} //used to debug only
   {$IfDef RecordProblems}   //normally only defined for debugging specific problems
      //{$Define ShowDEMSSOCalc}
   {$EndIf}
{$ELSE}
   //{$Define ShowDEMSSOCalc}
{$ENDIF}


interface

uses
   Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,Data.db,    System.Threading,System.SyncObjs,
   Buttons, ExtCtrls, OkCancl2, Dialogs,
   DemMapf,DEMLOSW, ComCtrls;

type
  TSSOCalcDlg = class(TOKRightDlg)
    HelpBtn: TButton;
    BitBtn3: TBitBtn;
    Memo1: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label12: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit5: TEdit;
    BitBtn1: TBitBtn;
    Edit4: TEdit;
    BitBtn2: TBitBtn;
    Edit9: TEdit;
    Wavelength: TTabSheet;
    Panel1: TPanel;
    Label8: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label22: TLabel;
    Label9: TLabel;
    CheckBox3: TCheckBox;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit19: TEdit;
    Edit24: TEdit;
    CheckBox2: TCheckBox;
    BitBtn4: TBitBtn;
    TabSheet2: TTabSheet;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    BitBtn7: TBitBtn;
    Edit13: TEdit;
    Label16: TLabel;
    Label17: TLabel;
    Edit14: TEdit;
    Label18: TLabel;
    BitBtn8: TBitBtn;
    Edit15: TEdit;
    Label19: TLabel;
    Edit16: TEdit;
    Label20: TLabel;
    BitBtn9: TBitBtn;
    RadioGroup1: TRadioGroup;
    CheckBox1: TCheckBox;
    Label21: TLabel;
    Edit17: TEdit;
    Label23: TLabel;
    Edit18: TEdit;
    CheckBox6: TCheckBox;
    Label24: TLabel;
    Edit20: TEdit;
    ComboBox1: TComboBox;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    TabSheet3: TTabSheet;
    CheckBox5: TCheckBox;
    CheckBox7: TCheckBox;
    RadioGroup2: TRadioGroup;
    CheckBox8: TCheckBox;
    CheckBox4: TCheckBox;
    Edit8: TEdit;
    RedrawSpeedButton12: TSpeedButton;
    procedure HelpBtnClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure Edit19Change(Sender: TObject);
    procedure Edit7Change(Sender: TObject);
    procedure Edit10Change(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure Edit14Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Edit17Change(Sender: TObject);
    procedure Edit18Change(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure Edit20Change(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
  private
    { Private declarations }
     theBaseMap : tMapForm;
     theLOSview : TDEMLOSF;
     NeedToDraw,PointFabrics : boolean;
     procedure DrawOverlays;
  public
    { Public declarations }
     ResultsGIS : integer;
     procedure CheckNewValues;
  end;


procedure DrawTopoGrainOverlay(BaseMap : tMapForm; LOSview : TDEMLOSF; DoPointFabrics : boolean = false; ActiveTabSheet : integer = 1; inGIS : integer = 0);

var
   SSOCalcDlg : TSSOCalcDlg;


implementation

{$R *.DFM}

uses
   PETMAR,Petmath,Petmar_types,Petmar_db,BaseGraf,PetDButils,
   rgb_colors_three_params,Make_tables,Map_overlays,
   DEMDefs,DEMCoord,DEMDataBase,
   DEMStat,DEMDef_routines,
   Nevadia_main, basemap, PETImage;


procedure DrawTopoGrainOverlay(BaseMap : tMapForm; LOSview : TDEMLOSF; DoPointFabrics : boolean = false; ActiveTabSheet : integer = 1; inGIS : integer = 0);
var
   FieldsInDB : tStringList;
begin
   SSOCalcDlg := TSSOCalcDlg.Create(Application);
   with SSOCalcDlg do begin
      case ActiveTabSheet of
         1 : PageControl1.ActivePage := TabSheet1;
         2 : PageControl1.ActivePage := Wavelength;
      end;
      theBaseMap := BaseMap;
      theLOSview := LOSview;
      ResultsGIS := inGIS;
      PointFabrics := DoPointFabrics;
      BitBtn5.Enabled := false;
      BitBtn5.Enabled := DoPointFabrics;
      BitBtn2.Enabled := (ResultsGIS = 0) and (theBaseMap <> Nil);
      Edit1.Enabled := (ResultsGIS = 0);
      Edit2.Enabled := (ResultsGIS = 0);
      Edit14.Enabled := (ResultsGIS = 0);
      CheckBox6.Enabled := (ResultsGIS = 0);
      Edit8.Enabled := (ResultsGIS = 0);
      CheckBox4.Enabled := (ResultsGIS = 0);
      if (ResultsGIS <> 0) then  begin
         GetFieldsLinkPossible(GISdb[ResultsGIS].MyData,GISdb[ResultsGIS].LinkTable,GISdb[ResultsGIS].dbOpts.VisCols,NumericFieldTypes,FieldsInDB);
         ComboBox1.Items := FieldsInDB;
         ComboBox1.Text := GISdb[ResultsGIS].dbOpts.FloatColorField;
         FieldsInDB.Free;
      end;

      BitBtn4.Enabled := (theLOSView <> Nil);
      BitBtn8.Enabled := (theLOSView <> Nil);

      BitBtn3.Enabled := (theBaseMap <> Nil) and (ResultsGIS = 0);
      BitBtn9.Enabled := theBaseMap <> Nil;
      RadioGroup1.Enabled := (theBaseMap <> Nil) and (ResultsGIS = 0);

      Edit4.Enabled := theBaseMap <> Nil;
      Edit5.Enabled := theBaseMap <> Nil;
      CheckNewValues;
      if (theBaseMap = Nil) and (theLOSView = Nil) then begin
         if (ShowModal <> idCancel) then SSOCalcDlg.DrawOverlays;
         SSOCalcDlg.Free;
      end
      else begin
         SSOCalcDlg.FormStyle := fsStayOnTop;
         SSOCalcDlg.Show;
         SSOCalcDlg.Visible := true;
      end;
   end;
end;

procedure TSSOCalcDlg.CancelBtnClick(Sender: TObject);
begin
   if (theBaseMap <> Nil) or (theLOSView <> Nil) then begin
      Close;
   end;
end;

procedure TSSOCalcDlg.CheckBox1Click(Sender: TObject);
begin
   MDdef.FabColorByField := CheckBox1.Checked;
end;


procedure TSSOCalcDlg.CheckBox2Click(Sender: TObject);
begin
   MDdef.PlotCrest := CheckBox2.Checked;
end;

procedure TSSOCalcDlg.CheckBox3Click(Sender: TObject);
begin
  MDDef.DoWaveLength := CheckBox3.Checked;
end;

procedure TSSOCalcDlg.CheckBox5Click(Sender: TObject);
begin
    MDDef.GemorphAspectRose := CheckBox5.Checked;
end;

procedure TSSOCalcDlg.CheckBox6Click(Sender: TObject);
begin
   MDDef.SSOallInTable := CheckBox6.Checked;
end;

procedure TSSOCalcDlg.CheckBox7Click(Sender: TObject);
begin
   MDDef.GemorphSSOPoles := CheckBox7.Checked;
end;

procedure TSSOCalcDlg.CheckBox8Click(Sender: TObject);
begin
   MDDef.PointFabricTerrBlowup := CheckBox8.Checked;
end;

procedure TSSOCalcDlg.CheckNewValues;
var
   xsize,ysize,xpts,ypts : integer;
begin
    {$IfDef ShowDEMSSOCalcFull} WriteLineToDebugFile('TSSOCalcDlg.CheckNewValues in'); {$EndIf}
    CheckEditString(Edit1.Text,MDDef.PointSeparation);
    CheckEditString(Edit2.Text,MDDef.SSOBoxSizeMeters);
    CheckEditString(Edit3.Text,MDDef.GrainLengthMultiple);
    CheckEditString(Edit4.Text,MDDef.MinPointsForSSO);
    CheckEditString(Edit5.Text,MDdef.FlatnessCutoff);
    CheckEditString(Edit6.Text,MDDef.MinWaveHeight);
    CheckEditString(Edit8.Text,MDDef.FabricAmplitudeDistance);
    CheckEditString(Edit9.Text,MDdef.OrganizationCutoff);
    CheckEditString(Edit13.Text,MDDef.CrestThreadInterval);
    CheckEditString(Edit14.Text,MDDef.SSOSampleIncr);
    CheckEditString(Edit15.Text,MDDef.DuneSamplingInterval);
    CheckEditString(Edit16.Text,MDDef.CrestMaxGap);
    CheckEditString(Edit24.Text,MDDef.WaveHtValuesNeeded);

    MDdef.DoFabricAmplitude := CheckBox4.Checked;

    if (TheBaseMap <> Nil) and  (DEMGlb[theBaseMap.MapDraw.DEMonMap] <> Nil) then begin
      xsize := round(0.5 * MDDef.SSOBoxSizeMeters / DEMGlb[theBaseMap.MapDraw.DEMonMap].AverageXSpace);
      ysize := round(0.5 * MDDef.SSOBoxSizeMeters / DEMGlb[theBaseMap.MapDraw.DEMonMap].AverageYSpace);
      xpts := succ(2*xsize);
      YPts := succ(2*ysize);
      if ((xpts * ypts) >= MDDef.MinPointsForSSO) then Label6.Font.Color := clBlack
      else Label6.Font.Color := clRed;

      if ((xpts div MDDef.SSOSampleIncr * ypts div MDDef.SSOSampleIncr) >= MDDef.MinPointsForSSO) then Label18.Font.Color := clBlack
      else Label18.Font.Color := clRed;
      Label6.Caption := xPts.ToString + 'x' + ypts.ToString + '=' + (xpts*ypts).ToString + ' pts/box';
      Label18.Caption := ( xpts div MDDef.SSOSampleIncr).ToString + 'x' + ( ypts div MDDef.SSOSampleIncr).ToString + '=' + ((xpts div MDDef.SSOSampleIncr) * (ypts div MDDef.SSOSampleIncr)).ToString + ' pts used';
      xsize := Round(theBaseMap.MapDraw.MapXSize * theBaseMap.MapDraw.ScreenPixelSize / MDDef.PointSeparation);
      ysize := Round(theBaseMap.MapDraw.MapYSize * theBaseMap.MapDraw.ScreenPixelSize / MDDef.PointSeparation);
      Label7.Caption := IntToStr(xsize) + 'x' + IntToStr(ysize) + ' vectors';
      Label9.Caption := IntToStr(round(MDdef.WavelengthCompDist / DEMGlb[theBaseMap.MapDraw.DEMonMap].AverageSpace)) + ' pts';
      NeedToDraw := true;
   end;
   {$IfDef ShowDEMSSOCalcFull} WriteLineToDebugFile('TSSOCalcDlg.CheckNewValues out'); {$EndIf}
end;

procedure TSSOCalcDlg.ComboBox1Change(Sender: TObject);
begin
  GISdb[ResultsGIS].dbOpts.FloatColorField := ComboBox1.Text;
end;

procedure TSSOCalcDlg.BitBtn2Click(Sender: TObject);
begin
   CheckNewValues;
   DrawOverlays;
   BitBtn5.Enabled := true;
end;

procedure TSSOCalcDlg.BitBtn3Click(Sender: TObject);
begin
   CheckNewValues;
   DrawOverlays;
end;


procedure TSSOCalcDlg.BitBtn4Click(Sender: TObject);
{$IfDef ExWaveLengthHeight}
begin
{$Else}
var
   WavelengthMean,WavelengthMedian,WavelengthStdDev,HeightMean,HeightMedian,HeightStd : float64;
begin
   if (theLOSView <> Nil) then begin
     CheckNewValues;
     theLOSView.FindWaveLengthHeight(True,WavelengthMean,WavelengthMedian,WavelengthStdDev,HeightMean,HeightMedian,HeightStd,Memo1);
     theLOSView.FormResize(Nil);
   end;
{$EndIf}
end;


procedure TSSOCalcDlg.BitBtn5Click(Sender: TObject);
begin
   CheckNewValues;
   ChangeDEMNowDoing(GetPointFabric);
end;


procedure TSSOCalcDlg.BitBtn6Click(Sender: TObject);
begin
   CheckNewValues;
   ChangeDEMNowDoing(GetPointFabric);
end;


procedure ReliefInflectionStrip(DEM : integer; var Findings : tStringList; GridLimits  : tGridLimits);
var
   Col,Row,ColInc,RowInc : integer;
   Lat,Long,Distance,Relief : float64;
begin
   ColInc := round(MDDef.PointSeparation / DEMGlb[DEM].AverageXSpace);
   RowInc := round(MDDef.PointSeparation / DEMGlb[DEM].AverageYSpace);
   Row := GridLimits.YGridLow + RowInc div 2;
   while Row <= GridLimits.YGridHigh do begin
      Col := GridLimits.XGridLow + ColInc div 2;
      while Col <= GridLimits.XGridHigh do begin
         {$IfDef VCL}
            if (ThreadsNumDone mod 100 = 0) then UpdateProgressBar(ThreadsNumDone/ThreadsToDo);
            TInterlocked.Increment(ThreadsNumDone);
         {$EndIf}
         if DEMGlb[DEM].FindReliefInflectionGraph(Col,Row,Distance,Relief) then begin
            DEMGlb[DEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
            Findings.Add(RealToString(Lat,-12,-6) + ',' + RealToString(Long,-12,-6) + ',' +RealToString(Distance,-8,-2) + ',' +RealToString(Relief,-12,-2));
         end;
         inc(Col,ColInc);
      end;
      inc(Row,RowInc);
   end;
end;


procedure TSSOCalcDlg.BitBtn7Click(Sender: TObject);
var
   i,it  : integer;
   fName : PathStr;
   Limits : tGridLimits;
   PartLimits :  tGridLimitsArray;
   Findings : array[1..MaxThreadsAllowed] of tStringList;
begin
   {$IfDef ShowDEMSSOCalc} WriteLineToDebugFile('tSSOCalcDlg.BitBtn7Click in'); {$EndIf}
   if (TheBaseMap <> Nil) then begin
      fName := MDTempDir + ptTrim(DEMGLB[theBaseMap.MapDraw.DEMOnMap].AreaName) + '_inflection' + DefaultDBExt;
      for i := 1 to MDdef.MaxThreadsForPC do Findings[i] := tStringList.Create;
      Findings[1].Add('LAT,LONG,INFLECT_M,HEIGHT_M');

      ThreadsNumDone := 0;
      ThreadsToDo := DEMGlb[theBaseMap.MapDraw.DEMonMap].DEMheader.NumRow div round(MDDef.PointSeparation / DEMGlb[theBaseMap.MapDraw.DEMonMap].AverageXSpace);

      {$IfDef RecordGetFabricAtPoint} WriteLineToDebugFile('ThreadsToDo :=' + IntToStr(ThreadsToDo)); {$EndIf}
      Limits := theBaseMap.GridLimitsForGeomorphOps;
      ShowHourglassCursor;
      StartProgress('Fabric region size');
      {$IfDef NoParallelFor}
         ReliefInflectionStrip(theBaseMap.MapDraw.DEMonMap,Findings[1],Limits);
      {$Else}
         PartLimits := GetLimitsForParallelLoops(Limits,round(MDDef.PointSeparation / DEMGlb[theBaseMap.MapDraw.DEMonMap].AverageYSpace));
         TParallel.For(1,MDdef.MaxThreadsForPC,
            procedure (Value: Integer)
            begin
               ReliefInflectionStrip(theBaseMap.MapDraw.DEMonMap,Findings[Value],PartLimits[Value]);
            end);
            ThreadsWorking := false;

            for it := 2 to MDdef.MaxThreadsForPC do begin
              for I := 0 to pred(Findings[it].Count) do
                 Findings[1].Add(Findings[it].Strings[i]);
              Findings[it].Free;
            end;
      {$EndIf}
      EndProgress;
      it := theBaseMap.StringListToLoadedDatabase(Findings[1],fName);
   end;
   {$IfDef ShowDEMSSOCalc} WriteLineToDebugFile('tSSOCalcDlg.BitBtn7Click out');  {$EndIf}
end;


procedure TSSOCalcDlg.BitBtn8Click(Sender: TObject);
{$IfDef ExWaveLengthHeight}
begin
{$Else}
var
   Results : tStringList;
   fName : PathStr;
begin
    Results := Nil;
    CrestsAlongProfile(theLOSView,Results,Memo1);
    if (Results.Count > 1) then  begin
       fName := NextFileNumber(MDTempDir, DEMGlb[theLOSView.LOSdraw.DEMonView].AreaName + '_dune_crests-',DefaultDBExt);
       theLOSView.BaseMap.StringListToLoadedDatabase(Results,fName);
    end
    else begin
       Results.Free;
       Memo1.Lines.Add('No crests found');
    end;
{$EndIf}
end;


procedure TSSOCalcDlg.BitBtn9Click(Sender: TObject);
begin
   {$IfDef ExWaveLengthHeight}
   {$Else}
      ComputeDunecrestspacingheight(theBaseMap,theBaseMap.MapDraw.MapAreaDEMGridLimits,Memo1);
   {$EndIf}
end;


procedure TSSOCalcDlg.RadioGroup1Click(Sender: TObject);
begin
   MDDef.GeomorphMapsFullDEM := (RadioGroup1.ItemIndex = 0);
end;


procedure TSSOCalcDlg.RadioGroup2Click(Sender: TObject);
begin
  MDDef.SSObyPole := RadioGroup2.ItemIndex = 0;
end;


procedure TSSOCalcDlg.RedrawSpeedButton12Click(Sender: TObject);
begin
   CheckNewValues;
   if ValidDB(ResultsGIS) then GISDB[ResultsGIS].RedrawLayerOnMap;
end;


procedure tSSOCalcDlg.DrawOverlays;
var
   fName : PathStr;
begin
   if NeedToDraw and (TheBaseMap <> Nil) then begin
      SetColorForProcessing;
      {$IfDef ShowDEMSSOCalc} WriteLineToDebugFile('tSSOCalcDlg.DrawOverlays in and drawing'); {$EndIf}
      CloseAndNilNumberedDB(ResultsGIS);
      fName := NextFileNumber(MDTempDir,ptTrim(DEMGLB[theBaseMap.MapDraw.DEMOnMap].AreaName) + '_fabric_'  + MDDef.SSOBoxSizeMeters.ToString + '_', DefaultDBExt);
      ResultsGIS := DEMGlb[theBaseMap.MapDraw.DEMonMap].OrientationTable(fName,theBaseMap);
      AddOverlay(theBaseMap,ovoDatabases);
      GISdb[ResultsGIS].dbOpts.dbAutoShow := dbasTerrainFabric;
      GISdb[ResultsGIS].RedrawLayerOnMap;
      NeedToDraw := false;

      Memo1.Lines.Add(ExtractFileNameNoExt(fName));
      Memo1.Lines.Add('Insufficient points: ' + SSOfailPts.ToString);
      Memo1.Lines.Add('Regions too flat:    ' + SSOfailFlat.ToString);
      Memo1.Lines.Add('Unorganized Regions: ' + SSOfailOrg.ToString);
      Memo1.Lines.Add('');
      SetColorForWaiting;
      {$IfDef ShowDEMSSOCalc} WriteLineToDebugFile('tSSOCalcDlg.DrawOverlays out'); {$EndIf}
   end
   else begin
      {$IfDef ShowDEMSSOCalc} WriteLineToDebugFile('tSSOCalcDlg.DrawOverlays in but NO drawing'); {$EndIf}
   end;
end;


procedure TSSOCalcDlg.HelpBtnClick(Sender: TObject);
begin
  DisplayHTMLTopic('html\micr96nn.htm');
end;

procedure TSSOCalcDlg.OKBtnClick(Sender: TObject);
begin
   CheckNewValues;
   if (theBaseMap <> Nil) or (theLOSView <> Nil) then begin
      Close;
   end;
end;


procedure TSSOCalcDlg.BitBtn1Click(Sender: TObject);
begin
   PickLineSizeAndColor('terrain fabric',BitBtn1,MDDef.GrainColor,MDDef.GrainLineWidth);
   NeedToDraw := true;
end;


procedure TSSOCalcDlg.FormCreate(Sender: TObject);
begin
   wmdem.FormPlacementInCorner(self);
   NeedToDraw := false;
   TheBaseMap := Nil;
   ResultsGIS := 0;
   CheckBox1.Checked := MDdef.FabColorByField;
   CheckBox2.Checked := MDdef.PlotCrest;
   CheckBox3.Checked := MDDef.DoWaveLength;
   CheckBox4.Checked := MDdef.DoFabricAmplitude;
   CheckBox6.Checked := MDDef.SSOallInTable;
   CheckBox5.Checked := MDDef.GemorphAspectRose;
   CheckBox7.Checked := MDDef.GemorphSSOPoles;

   if MDDef.SSObyPole then RadioGroup2.ItemIndex := 0
   else RadioGroup2.ItemIndex := 1;

   Edit1.Text := IntToStr(MDDef.PointSeparation);
   Edit2.Text := IntToStr(MDDef.SSOBoxSizeMeters);
   Edit3.Text := IntToStr(MDDef.GrainLengthMultiple);
   Edit4.Text := IntToStr(MDDef.MinPointsForSSO);
   Edit5.Text := RealToString(MDdef.FlatnessCutoff,-8,2);
   Edit6.Text := RealToString(MDDef.MinWaveHeight,-12,-3);
   Edit7.Text := IntToStr(MDdef.WavelengthCompDist);
   Edit8.Text := IntToStr(MDDef.FabricAmplitudeDistance);
   Edit9.Text := RealToString(MDdef.OrganizationCutoff,-8,2);
   Edit10.Text := IntToStr(MDDef.FirstBoxSize);
   Edit11.Text := IntToStr(MDDef.LastBoxSize);
   Edit12.Text := IntToStr(MDDef.PlateauTolerance);
   Edit13.Text := IntToStr(MDDef.CrestThreadInterval);
   Edit14.Text := IntToStr(MDDef.SSOSampleIncr);
   Edit15.Text := IntToStr(MDDef.DuneSamplingInterval);
   Edit16.Text := IntToStr(MDDef.CrestMaxGap);

   Edit19.Text := IntToStr(MDdef.PeakPitPostings);
   Edit20.Text := IntToStr(MDdef.FabricCalcThin);
   Edit24.Text := IntToStr(MDDef.WaveHtValuesNeeded);
   ColorLineWidthBitBtn(BitBtn1,MDDef.GrainColor,MDDef.GrainLineWidth);

   Edit17.Text := RealToString(MDdef.FabColorMin,-8,-2);
   Edit18.Text := RealToString(MDdef.FabColorMax,-8,-2);

   if MDDef.GeomorphMapsFullDEM then RadioGroup1.ItemIndex := 0
   else RadioGroup1.ItemIndex := 1;

   {$IfDef ExWaveLengthHeight}
      BitBtn4.Visible := false;
      BitBtn8.Visible := false;
      BitBtn9.Visible := false;
   {$EndIf}
end;


procedure TSSOCalcDlg.Edit10Change(Sender: TObject);
begin
  CheckEditString(Edit10.Text,MDDef.FirstBoxSize);
  CheckEditString(Edit11.Text,MDDef.LastBoxSize);
  CheckEditString(Edit12.Text,MDDef.PlateauTolerance);
end;


procedure TSSOCalcDlg.Edit14Change(Sender: TObject);
begin
   CheckNewValues;
end;

procedure TSSOCalcDlg.Edit17Change(Sender: TObject);
begin
   CheckEditString(Edit17.Text,MDdef.FabColorMin);
   CheckEditString(Edit18.Text,MDdef.FabColorMax);
end;

procedure TSSOCalcDlg.Edit18Change(Sender: TObject);
begin
   CheckEditString(Edit18.Text,MDdef.FabColorMax);
end;

procedure TSSOCalcDlg.Edit19Change(Sender: TObject);
begin
   CheckEditString(Edit19.Text,MDdef.PeakPitPostings );
   if (theLOSView <> Nil) then
      Label11.Caption := 'Region (points); size=' + RealToString(MDdef.PeakPitPostings*2*theLOSView.LOSdraw.FormSectLenMeters /theLOSView.LOSdraw.PixLong,-12,0) + ' m';
end;

procedure TSSOCalcDlg.Edit1Change(Sender: TObject);
begin
   CheckNewValues;
end;

procedure TSSOCalcDlg.Edit20Change(Sender: TObject);
begin
  CheckEditString(Edit20.Text,MDdef.FabricCalcThin);
end;

procedure TSSOCalcDlg.Edit2Change(Sender: TObject);
begin
   CheckNewValues;
end;

procedure TSSOCalcDlg.Edit3Change(Sender: TObject);
begin
   CheckNewValues;
end;

procedure TSSOCalcDlg.Edit4Change(Sender: TObject);
begin
   CheckNewValues;
end;

procedure TSSOCalcDlg.Edit5Change(Sender: TObject);
begin
   CheckNewValues;
end;


procedure TSSOCalcDlg.Edit7Change(Sender: TObject);
begin
   CheckEditString(Edit7.Text,MDdef.WavelengthCompDist);
end;


initialization
finalization
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing demssocalc out'); {$EndIf}
end.

