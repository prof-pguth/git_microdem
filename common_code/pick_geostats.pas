unit pick_geostats;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}



{$I nevadia_defines.inc}

{$IFDEF DEBUG}
  //{$Define NoParallelFor} //used to debug only
{$ELSE}
  //{$Define NoParallelFor}
{$ENDIF}


{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$IfDef Debug}
      //{$Define RecordGeostats}
      //{$Define RecordMoments}
      //{$Define RecordMapMaking}
   {$EndIf}
{$EndIf}


interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  System.SyncObjs, System.Threading,System.SysUtils,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
  DEMDefs, Vcl.ComCtrls, Vcl.Grids;

type
  TPickGeoStat = class(TForm)
    Panel1: TPanel;
    Edit1: TEdit;
    Label1: TLabel;
    Radiogroup1: TRadioGroup;
    CheckBox2: TCheckBox;
    BitBtn10: TBitBtn;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Button6: TButton;
    Button5: TButton;
    BitBtn6: TBitBtn;
    BitBtn12: TBitBtn;
    Button7: TButton;
    TabSheet2: TTabSheet;
    BitBtn14: TBitBtn;
    BitBtn15: TBitBtn;
    Differen: TBitBtn;
    BitBtn8: TBitBtn;
    TabSheet3: TTabSheet;
    BitBtn11: TBitBtn;
    Ge: TBitBtn;
    BitBtn9: TBitBtn;
    TabSheet4: TTabSheet;
    Button8: TButton;
    BitBtn4: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn13: TBitBtn;
    TabSheet5: TTabSheet;
    Button1: TButton;
    BitBtn16: TBitBtn;
    TabSheet6: TTabSheet;
    BitBtn7: TBitBtn;
    Memo1: TMemo;
    TabSheet7: TTabSheet;
    Button11: TButton;
    Button2: TButton;
    Button10: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button9: TButton;
    Label2: TLabel;
    Button15: TButton;
    BitBtn5: TBitBtn;
    BitBtn17: TBitBtn;
    Label3: TLabel;
    Label4: TLabel;
    Edit2: TEdit;
    BitBtn18: TBitBtn;
    BitBtn19: TBitBtn;
    CheckBox1: TCheckBox;
    BitBtn20: TBitBtn;
    StringGrid1: TStringGrid;
    BitBtn21: TBitBtn;
    CheckBox3: TCheckBox;
    BitBtn22: TBitBtn;
    Label5: TLabel;
    Label6: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    BitBtn23: TBitBtn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn1Click(Sender: TObject);
    procedure Radiogroup1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure DifferenClick(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure GeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure BitBtn19Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure BitBtn20Click(Sender: TObject);
    procedure BitBtn21Click(Sender: TObject);
    procedure BitBtn22Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure BitBtn23Click(Sender: TObject);
  private
    //procedure GetBatchRegionSize;
    procedure NeedSingleDEM;
    { Private declarations }
  public
    { Public declarations }
     CurDEM : integer;
     DEMsWanted : tDEMbooleanArray;
     Closing : boolean;
     GroupTitle : shortstring;
     GridLimits : tGridLimits;
     InitialDEMs : tStringList;
  end;


procedure DoGeoStatAnalysis;
procedure CloseGeoStatAnalysis;

var
   PickGeoStat : TPickGeoStat;

implementation

{$R *.dfm}

uses
   DEMCoord,DEMStat,MEM_Power_Spect,BaseMap,PetFouri,Petmar,Petmar_types,Petmar_db,Geomorph_point_class,DEMOptions,
   geomorph_region_size_graph,
   Thread_Timers,
   PetDBUtils,
   DEM_Manager,
   Make_grid,
   DEMDef_routines,
   DEM_Indexes,
   Nevadia_main,
   Make_tables,
   DEMDatabase,
   DEMMapf,
   Moment_Opts,
   Slope_Graph_opts,
   DEM_hist_opts,
   demslopecompare,
   gdal_tools,
   Netopts,
   MD_use_tools,
   demtrendopt, PETMath, demstringgrid;


procedure CloseGeoStatAnalysis;
begin
   if (PickGeoStat <> Nil) then begin
      PickGeostat.Close;
      PickGeostat := Nil;
   end;
end;


procedure DoGeoStatAnalysis;
var
   i : integer;
begin
   {$IfDef RecordGeostats} WriteLineToDebugFile('DoGeoStatAnalysis in'); {$EndIf}
   PickGeoStat := TPickGeoStat.Create(Application);
   PickGeoStat.CurDEM := 0;
   for i := 1 to MaxDEMDataSets do begin
      PickGeoStat.DEMsWanted[i] := ValidDEM(i) and (not DEMGlb[i].HiddenGrid);
   end;

   PickGeoStat.RadioGroup1.ItemIndex := 1;
   PickGeoStat.Edit1.Text := IntToStr(MDDef.StatSampleIncr);
   PickGeoStat.Radiogroup1Click(Nil);
   PickGeoStat.Show;
   {$IfDef RecordGeostats} WriteLineToDebugFile('DoGeoStatAnalysis out'); {$EndIf}
end;


procedure TPickGeoStat.Radiogroup1Click(Sender: TObject);
begin
   MDDef.GeomorphMapsFullDEM := (RadioGroup1.ItemIndex = 0);
   if (CurDEM <> 0) and ValidDEM(CurDEM) then begin
      if MDDef.GeomorphMapsFullDEM then begin
         GridLimits := DEMGlb[CurDEM].FullDEMGridLimits;
      end
      else begin
         GridLimits := DEMGlb[CurDEM].SelectionMap.MapDraw.MapAreaDEMGridLimits;
      end;
      DEMGlb[CurDEM].FilledGridBox(GridLimits);
   end;
end;

procedure TPickGeoStat.BitBtn10Click(Sender: TObject);
begin
   DisplayHTMLTopic('html\geostatistical_analysis.htm');
end;

procedure TPickGeoStat.BitBtn11Click(Sender: TObject);
begin
   Geomorph_point_class.PointClassification(CurDEM);
end;

procedure TPickGeoStat.BitBtn12Click(Sender: TObject);
var
   j : integer;
begin
   if CheckBox2.Checked then begin
      for j := 1 to MaxDEMDataSets do if DEMsWanted[j] then SingleDEMHistogram(j);
   end
   else begin
      SingleDEMHistogram(CurDEM);
   end;
end;

procedure TPickGeoStat.BitBtn13Click(Sender: TObject);
{$IfDef ExWaveLengthHeight}
begin
{$Else}
var
   i : integer;
begin
   if CheckBox2.Checked then begin
      for i := 1 to MaxDEMDataSets do if ValidDEM(i) then DEMGlb[i].SelectionMap.FabricAtPoint1Click(Nil);
   end
   else begin
      ComputeDunecrestspacingheight(DEMGlb[CurDem].SelectionMap,GridLimits,Nil);
   end;
{$EndIf}
end;


procedure TPickGeoStat.BitBtn15Click(Sender: TObject);

         (*
         procedure DoSSODiagramBySlope(CurDEM : integer; GridLimits : tGridLimits);
         var
            Results   : tStringList;

                  procedure DoSlopeRange(UseMin,UseMax : float64);
                  var
                     SSOvars : tSSOvars;
                  begin
                     DEMGlb[CurDEM].SSOComputations(GridLimits,SSOvars,true,'','',UseMin,UseMax);
                     Results.Add(RealToString(UseMin,6,0) + RealToString(UseMax,6,0) + IntegerToString(SSOvars.NumPts,9) + RealToString(SSOvars.s2s3,10,2) + RealToString(SSOvars.TheDipDirs[3],12,0));
                  end;

         begin
            MDDef.NetDef.NetUsed := Schmidt;
            if (DEMGlb[CurDEM].DEMheader.MaxElev - DEMGlb[CurDEM].DEMheader.MinElev) > 5 then begin
               Results := tStringList.Create;
               Results.Add('    Slopes        n         s2s3    Direction');
               Results.Add('================================================');
               DoSlopeRange(0,89.90);
               DoSlopeRange(0,MDDef.GeomorphSlopeCut[1]);
               DoSlopeRange(MDDef.GeomorphSlopeCut[1],MDDef.GeomorphSlopeCut[2]);
               DoSlopeRange(MDDef.GeomorphSlopeCut[2],MDDef.GeomorphSlopeCut[3]);
               DoSlopeRange(MDDef.GeomorphSlopeCut[3],MDDef.GeomorphSlopeCut[4]);
               DoSlopeRange(MDDef.GeomorphSlopeCut[4],89.90);
               DisplayAndPurgeStringList(Results,'Organization by slope');
            end
            else MessageToContinue(InsuffDEMrelief);
         end;
         *)

begin
(*
   var  j : integer;
   if CheckBox2.Checked then begin
      for j := 1 to MaxDEMDataSets do if ValidDEM(j) then DoSSODiagramBySlope(j,GridLimits);
   end
   else begin
      DoSSODiagramBySlope(CurDEM,GridLimits);
   end;
*)
end;

procedure TPickGeoStat.BitBtn16Click(Sender: TObject);
begin
   if (CurDEM = 0) then GetDEM(CurDEM,true,'single DEM geomorphometry');
   Radiogroup1Click(Sender);
   GetTrendOptions(0,CurDEM,GridLimits, DEMGlb[CurDEM].SelectionMap);
   //DEMTrendOpt.ComputeTrendSurface(0,CurDEM,GridLimits,  DEMGlb[CurDEM].SelectionMap);
end;

procedure TPickGeoStat.BitBtn17Click(Sender: TObject);
begin
   SlopeRegionSize(CurDEM,false);
end;

procedure TPickGeoStat.BitBtn18Click(Sender: TObject);
begin
   MicronetOptions(Nil);
end;

procedure TPickGeoStat.BitBtn19Click(Sender: TObject);
var
   Method : integer;
begin
   DEMDef_routines.SaveBackupDefaults;   //so we save slope region size
      for Method:= FirstSlopeMethod to LastSlopeMethod do begin
         MDdef.SlopeAlg := Method;
         CreateSlopeMap(CurDEM);
      end;
   DEMDef_routines.RestoreBackupDefaults;  //to restore slope region size
end;


procedure TPickGeoStat.BitBtn1Click(Sender: TObject);
{$IfDef ExGeostats}
begin
{$Else}
var
   FracDim,r  : float32;
begin
   DEMGlb[CurDEM].FractalBox(GridLimits,FracDim,r);
{$EndIf}
end;


procedure TPickGeoStat.BitBtn20Click(Sender: TObject);

      procedure SaveDEM(DEM : integer);
      begin
         //DEMGlb[DEM].SaveAsGeotiff(MDTempDir + DEMGlb[DEM].AreaName + '.tif');
      end;

var
   Radius,Box,DEM,NoSlopeMap : integer;
begin
   {$IfDef RecordMapMaking} WriteLineToDebugFile('TPickGeoStat.BitBtn20Click in'); {$EndIf}
   try
      if (CurDEM = 0) then GetDEM(CurDEM,true,'single DEM geomorphometry');
      HeavyDutyProcessing := true;
      CreateSlopeMap(CurDEM);

      DEM := MakeSingleNewDerivativeMap('g',CurDEM);  //rugosity
      SaveDEM(DEM);
      DEM := CreateRoughnessMap(CurDEM);
      SaveDEM(DEM);
      DEM := CreateRoughnessMap2(CurDEM,true,false);
      SaveDEM(DEM);
      DEM := CreateRoughnessMapAvgVector(CurDEM,true);
      SaveDEM(DEM);
      {$IfDef RecordMapMaking} WriteLineToDebugFile('TPickGeoStat.BitBtn20Click singles done, start loop'); {$EndIf}

      for Radius := 1 to 4 do begin
         Box := succ(2*Radius);
         DEM := CreateRoughnessSlopeStandardDeviationMap(CurDEM,Box);
         {$IfDef RecordMapMaking} WriteLineToDebugFile('TPickGeoStat.BitBtn20Click CreateRoughnessSlopeStandardDeviationMap done'); {$EndIf}
         SaveDEM(DEM);
         {$IfDef RecordMapMaking} WriteLineToDebugFile('TPickGeoStat.BitBtn20Click DEM saved'); {$EndIf}
         WhiteBox_AverageNormalVectorAngularDeviation(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap,Box);
         WhiteBox_CircularVarianceOfAspect(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap,Box);
         SagaVectorRuggednessMap(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap,Box);
         GrassVectorRuggedness(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap,Box);
         {$IfDef RecordMapMaking} WriteLineToDebugFile('TPickGeoStat.BitBtn20Click loope done, radius=' + IntToStr(Radius)); {$EndIf}
      end;
   finally
      HeavyDutyProcessing := false;
   end;

   {$IfDef RecordMapMaking} WriteLineToDebugFile('TPickGeoStat.BitBtn20Click out'); {$EndIf}
end;



procedure TPickGeoStat.NeedSingleDEM;
begin
   if (CurDEM = 0) and GetDEM(CurDEM,true,'single DEM geomorphometry') then CheckBox2.Checked := false;
end;


procedure TPickGeoStat.BitBtn21Click(Sender: TObject);
var
   NewMap,FirstMap : integer;

   procedure MatchAndSave;
   begin
      if ValidDEM(NewMap) then begin
         MatchAnotherDEMMap(NewMap,CurDEM);
         //if NewMap <> FirstMap then MaskStripFromSecondGrid(DEMGlb[FirstMap].FullDEMGridLimits,NewMap.DEMonMap,SecondGrid,HowMask);
         DEMGlb[NewMap].SaveAsGeotiff(ExtractFilePath(DEMGlb[CurDEM].DEMFileName) + DEMGlb[NewMap].AreaName + '.tif');
      end;
   end;

begin
   SaveBackupDefaults;
   NeedSingleDEM;

   MDdef.SlopeAlg := smEightNeighborsUnweighted;
   NewMap := CreateSlopeMap(CurDEM);
   FirstMap := NewMap;
   DEMGlb[NewMap].AreaName := 'md_evans_slope';
   MatchAndSave;

   MDdef.SlopeAlg := smEightNeighborsWeighted;
   NewMap := CreateSlopeMap(CurDEM);
   DEMGlb[NewMap].AreaName := 'md_horn_slope';
   MatchAndSave;

   MDdef.SlopeAlg := smFourNeighbors;
   NewMap := CreateSlopeMap(CurDEM);
   DEMGlb[NewMap].AreaName := 'md_zt_slope';
   MatchAndSave;

   NewMap := WhiteBoxSlopeMap(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap);
   MatchAndSave;

   NewMap := GRASSSlopeMap(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap);
   DEMGlb[NewMap].DEMHeader.RasterPixelIsGeoKey1025 := 1;
   MatchAndSave;

   NewMap := GDAL_SlopeMap_ZT(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap, DEMGlb[CurDEM].GDAL_ScaleFactorString);
   DEMGlb[NewMap].DEMHeader.RasterPixelIsGeoKey1025 := 1;
   MatchAndSave;

   NewMap := GDAL_SlopeMap_Horn(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap, DEMGlb[CurDEM].GDAL_ScaleFactorString);
   DEMGlb[NewMap].DEMHeader.RasterPixelIsGeoKey1025 := 1;
   MatchAndSave;

   RestoreBackupDefaults;
end;


procedure TPickGeoStat.BitBtn22Click(Sender: TObject);
begin
   if (CurDEM = 0) then GetDEM(CurDEM,true,'single DEM geomorphometry');
   HeavyDutyProcessing := true;
   CreateSlopeMap(CurDEM);
   MakeTRIGrid(CurDEM,true,true);
   MakeTRIGrid(CurDEM,false,true);
   GDAL_TRI_Riley(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap);
   GDAL_TRI_Wilson(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap);
   GDAL_TPI(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap);
   Whitebox_TRI(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap);
   SagaTRIMap(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap);
   SagaTPIMap(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap);
   GRASSTRIMap(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap);
   GRASSTPIMap(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap);
   HeavyDutyProcessing := false;
end;


procedure TPickGeoStat.BitBtn23Click(Sender: TObject);
begin
   GetMultipleDEMsFromList('Geomorphometry',DEMsWanted);
end;

procedure TPickGeoStat.BitBtn2Click(Sender: TObject);
{$IfDef ExGeostats}
begin
{$Else}
var
   SlopeByColumn,SlopeByRow : float32;
begin
   NeedSingleDEM;
   DEMStat.FastFourierTransform(CurDEM,GridLimits,SlopeByColumn,SlopeByRow);
{$EndIf}
end;


procedure TPickGeoStat.BitBtn3Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      SemiVariogramOptions;
      if (CurDEM = 0) then begin
         var j : integer;
         for j := 1 to MaxDEMDataSets do if DEMsWanted[j] then begin
            DEMStat.SemiVariogram(j,DEMGlb[j].FullDEMGridLimits);
         end;
      end
      else begin
         Radiogroup1Click(Sender);
         DEMStat.SemiVariogram(CurDEM,GridLimits);
      end;
   {$EndIf}
end;


procedure TPickGeoStat.BitBtn4Click(Sender: TObject);
var
   NS_Slope,EW_Slope : float32;
begin
   MEM_Power_Spect.GetMEMPowerSpectra(NS_Slope,EW_Slope,CurDEM,GridLimits);
end;


procedure TPickGeoStat.BitBtn5Click(Sender: TObject);
var
   Col,Row,DeltaSlope : integer;
   s1 : float64;
   SlopeCompareOptions    : TSlopeCompareOptions;
   SlpAsp1,SlpAsp2        : tSlopeAspectRec;
begin
   SaveBackupDefaults;
   NeedSingleDEM;

   SlopeCompareOptions := TSlopeCompareOptions.Create(Application);
   with SlopeCompareOptions do begin
     SlopeCompareOptions.ShowModal;
     DeltaSlope := DEMGlb[CurDEM].CloneAndOpenGrid(FloatingPointDEM,DEMGlb[CurDEM].AreaName + ' Slope Algorithm Difference',PercentSlope);

     StartProgress('Differences');
     for Col := 0 to pred(DEMGlb[1].DEMheader.NumCol) do begin
        if (Col mod 50 = 0) then UpdateProgressBar(Col/ pred(DEMGlb[CurDEM].DEMheader.NumCol));
        for Row := 0 to pred(DEMGlb[1].DEMheader.NumRow) do begin
           MDdef.SlopeAlg := SlopeMethod1;
           if DEMGlb[CurDEM].GetSlopeAndAspect(Col,Row,SlpAsp1) then begin
              MDdef.SlopeAlg := SlopeMethod2;
              if DEMGlb[CurDEM].GetSlopeAndAspect(Col,Row,SlpAsp2) then begin
                 s1 := (SlpAsp1.SlopePercent-SlpAsp2.SlopePercent);
                 if (RadioGroup1.ItemIndex = 0) then s1 := abs(s1);
                 DEMGlb[DeltaSlope].SetGridElevation(Col,Row,s1);
              end;
           end {if};
          end {for Row};
     end {for Col};
     EndProgress;
     DEMGlb[DeltaSlope].SetUpMap(DeltaSlope,true,mtElevRainbow);
     DEMGlb[DeltaSlope].SelectionMap.Closable := false;
   end;
   SlopeCompareOptions.Free;
   RestoreBackupDefaults;
end;


procedure TPickGeoStat.DifferenClick(Sender: TObject);
begin
   {$IfDef RecordMoments} WriteLineToDebugFile('TPickGeoStat.DifferenClick, moments'); {$EndIf}
   MDDef.CountHistograms:= false;
   ElevMomentReport(DEMSWanted,GroupTitle,Memo1,False,GridLimits,CurDEM);
end;

procedure TPickGeoStat.BitBtn6Click(Sender: TObject);
begin
   Button15Click(Sender);
   AspectDistributionBySlope(CurDEM,DEMGlb[CurDEM].FullDEMGridLimits);
end;


procedure TPickGeoStat.BitBtn7Click(Sender: TObject);
begin
   Button15Click(Sender);
   AspectDistributionByAlgorithm(CurDEM,GridLimits);
end;

procedure TPickGeoStat.BitBtn8Click(Sender: TObject);
begin
   {$IfDef RecordGeostats} WriteLineToDebugFile('TPickGeoStat.BitBtn8Click in'); {$EndIf}
   MDDef.SSObyPole := (Sender = BitBtn8);
   if CheckBox2.Checked then DoAnSSODiagram(0,GridLimits)
   else DoAnSSODiagram(CurDEM,GridLimits);
end;

procedure TPickGeoStat.BitBtn9Click(Sender: TObject);
begin
   MakeGraphsByRegionSize(CurDEM,-999,-999);
end;


procedure TPickGeoStat.Button10Click(Sender: TObject);
begin
   PickSlopeAspectMethod('',MDdef.SlopeAlg);
   Label2.Caption := SlopeMethodName(MDdef.SlopeAlg);
end;

procedure TPickGeoStat.Button11Click(Sender: TObject);
begin
   SetGeomorphDefaults;
end;

procedure TPickGeoStat.Button12Click(Sender: TObject);
begin
   {$IfDef RecordMoments} WriteLineToDebugFile('TPickGeoStat.Button12Click, moments with optons'); {$EndIf}
   Moment_Opts.SetMomentOptions;
   DifferenClick(Sender);
end;

procedure TPickGeoStat.Button13Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      ElevMomentReport(DEMSWanted,'',Memo1,true,GridLimits,CurDEM);
   {$EndIf}
end;

procedure TPickGeoStat.Button14Click(Sender: TObject);
begin
   {$IfDef ExComplexGeoStats}
   {$Else}
      if (CurDEM = 0) then begin
         SaveBackupDefaults;
         MDDef.ShowAspectRose := false;
         MDDef.ShowSlopeFreq := false;
         MDDef.ShowElevSlope := false;
         MDDef.ShowCumSlope := false;
         MDDef.ShowElevSlopeDeg := false;
         ElevationSlopePlot(CurDEM,MDDef.ElevBinSize);
         RestoreBackupDefaults;
      end
      else DEMGlb[CurDEM].CreateWholeDEMHistogram;
   {$EndIf}
end;


procedure TPickGeoStat.Button15Click(Sender: TObject);
begin
   if GetDEM(CurDEM,true,'single DEM geomorphometry') then begin
      CheckBox2.Checked := false;
      Radiogroup1Click(Sender);
   end;
end;


procedure TPickGeoStat.Button1Click(Sender: TObject);
var
  Order : integer;
  zr : float64;
  Answer : tStringList;
  TrendSurf : tTrendSurf;
begin
   if (CurDEM = 0) then GetDEM(CurDEM,true,'single DEM geomorphometry');
   Radiogroup1Click(Sender);
   Answer := tStringList.Create;
   Answer.Add(DEMGlb[CurDEM].AreaName);
   Answer.Add('Order  r²');
   SaveBackupDefaults;
   MDDef.TrendSurfaceOverVoids := false;
   MDDef.TrendMapDev := false;
   MDDef.TrendText := false;
   MDDef.TrendHistDev := false;
   MDDef.TrendOpenMaps := true;
   MDDef.TrendSurfMap := true;
   TrendSurf := tTrendSurf.Create;
   TrendSurf.SetDEM(CurDEM,DEMGlb[CurDEM].FullDEMGridLimits);
   for Order := 1 to 8 do begin
      TrendSurf.CurrentOrderTrendSurface := Order;
      TrendSurf.ComputeTrendSurface;
      zr := TrendSurf.r2;
      Answer.Add(IntToStr(Order) + RealToString(zr,12,4));
   end;
   TrendSurf.Destroy;
   Petmar.DisplayAndPurgeStringList(Answer,'Trend surface orders ' + DEMGlb[CurDEM].AreaName);
   RestoreBackupDefaults;
end;

procedure TPickGeoStat.Button2Click(Sender: TObject);
var
   i : integer;
begin
   for i := 1 to 4 do ReadDefault('Slope boundary ' + i.ToString,MdDef.GeomorphSlopeCut[i]);
end;

procedure TPickGeoStat.Button5Click(Sender: TObject);
begin
   ElevationSlopePlot(CurDEM,MDDef.ElevBinSize);
end;

procedure TPickGeoStat.Button6Click(Sender: TObject);
var
   SlopeGraphOpts : Tslopegraphopts;
begin
   SlopeGraphOpts := Tslopegraphopts.Create(Application);
   InsureFormOnScreen(SlopeGraphOpts,Mouse.CursorPos.X,Mouse.CursorPos.Y);
   SlopeGraphOpts.ShowModal;
   Button5Click(Sender);
end;

procedure TPickGeoStat.Button7Click(Sender: TObject);
begin
   DEM_Hist_Opts.SetHistogramOptions;
   BitBtn12Click(Sender);
end;

procedure TPickGeoStat.Button8Click(Sender: TObject);
begin
   GetMemOptions;
end;

procedure TPickGeoStat.Button9Click(Sender: TObject);
begin
   SlopeRegionSize(CurDEM);
end;

procedure TPickGeoStat.CheckBox1Click(Sender: TObject);
begin
   MDDef.QuickSlopeSpacings := CheckBox1.Checked;
end;

procedure TPickGeoStat.CheckBox2Click(Sender: TObject);
begin
   if CheckBox2.Checked then CurDEM := 0;
end;

procedure TPickGeoStat.CheckBox3Click(Sender: TObject);
begin
   MDDef.CorrectAspectTrue := CheckBox3.Checked;
end;

procedure TPickGeoStat.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDDef.StatSampleIncr);
end;

procedure TPickGeoStat.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDDef.NetDef.MaxContourConcentration);
end;

procedure TPickGeoStat.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,MDDef.ElevHistBinSize);
end;

procedure TPickGeoStat.Edit4Change(Sender: TObject);
begin
   CheckEditString(Edit4.Text,MDDef.SlopeHistBinSize);
end;

procedure TPickGeoStat.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   if not Closing then begin
      Closing := true;
      CloseGeoStatAnalysis;
   end;
end;


procedure TPickGeoStat.FormCreate(Sender: TObject);
var
   j : integer;
begin
   CheckBox1.Checked := MDDef.QuickSlopeSpacings;
   CheckBox3.Checked := MDDef.CorrectAspectTrue;

   if MDDef.GeomorphMapsFullDEM then RadioGroup1.ItemIndex := 0
   else RadioGroup1.ItemIndex := 1;
   Edit2.Text := RealToString(MDDef.NetDef.MaxContourConcentration,-12,-2);

   PageControl1.ActivePage := TabSheet1;
   Label2.Caption := SlopeMethodName(MDdef.SlopeAlg);
   GroupTitle := '';
   InitialDEMs := tStringList.Create;
   for j := 1 to MaxDEMDataSets do if ValidDEM(j) then InitialDEMs.Add(IntToStr(j));
   Closing := false;
   StringGrid1.Cells[0,0] := 'Region (m)';
   StringGrid1.Cells[0,1] := IntToStr(MDDef.BatchRegionSize[1]);  //'150';
   StringGrid1.Cells[0,2] := IntToStr(MDDef.BatchRegionSize[2]);  //'250';
   StringGrid1.Cells[0,3] := IntToStr(MDDef.BatchRegionSize[3]);  //'500';
   StringGrid1.Cells[0,4] := IntToStr(MDDef.BatchRegionSize[4]);  //'750';
   StringGrid1.Cells[0,5] := IntToStr(MDDef.BatchRegionSize[5]);  //'1000';
   Edit3.Text := RealToString(MDDef.ElevHistBinSize,-8,-2);
   Edit4.Text := RealToString(MDDef.SlopeHistBinSize,-8,-2);
   wmdem.FormPlacementInCorner(Self);
end;


procedure TPickGeoStat.GeClick(Sender: TObject);
var
   dbName : array[1..MaxThreadsAllowed] of PathStr;
   dbTable : array[1..MaxThreadsAllowed] of tMyData;
   i,db : integer;
   PartLimits : tGridLimitsArray;
   theFileNames : tStringList;

         procedure MakeTables(NumT : integer);
         var
            i : integer;
         begin
            for i := 1 to NumT do begin
               if (i=1) then dbName[i] := '' else dbName[i] := '-' + i.ToString;
               dbName[i] := MDTempDir + ptTrim(DEMGlb[CurDem].AreaName) + dbName[i] + DefaultDBExt;
               Make_tables.CreateGeomorphometryAttributesTable(dbname[i],false);
               dbTable[i] := tMyData.Create(dbName[i]);
            end;
         end;

begin
   {$IfDef RecordGeostats} WriteLineToDebugFile('TPickGeoStat.GeClick in'); {$EndIf}
   GetGeomorphBlockOpts(gbDB,CurDEM,GridLimits);
   {$IfDef NoParallelFor}
      MakeTables(1);
      DEMStat.NewBlockGeoStats(gbGrid,dbTable[1],CurDEM,GridLimits);
      DEMGlb[CurDem].SelectionMap.OpenDBonMap('',dbName[1]);
   {$Else}
      MakeTables(MDdef.MaxThreadsForPC);
      PartLimits := GetLimitsForParallelLoops(GridLimits);

        TParallel.For(1,MDdef.MaxThreadsForPC,
          procedure (Value: Integer)
          begin
            DEMStat.NewBlockGeoStats(gbGrid,dbTable[Value],CurDEM,PartLimits[Value]);
          end);

      ThreadsWorking := false;
      for i := 1 to MDdef.MaxThreadsForPC do dbTable[i].Destroy;
      db := DEMGlb[CurDem].SelectionMap.OpenDBonMap('',dbName[1]);
      theFileNames := tStringList.Create;
      for i := 2 to MDdef.MaxThreadsForPC do theFileNames.Add(dbName[i]);
      GISdb[db].MergeDataBases(theFileNames);
      theFileNames.Free;
   {$EndIf}
   {$IfDef RecordGeostats} WriteLineToDebugFile('TPickGeoStat.GeClick out'); {$EndIf}
end;


initialization
   PickGeostat := Nil;
finalization
   {$IfDef RecordGeostats} WriteLineToDebugFile('RecordGeostats active in pick_geostats'); {$EndIf}
   {$IfDef NoParallelFor} WriteLineToDebugFile('NoParallelFor active in pick_geostats'); {$EndIf}
end.
