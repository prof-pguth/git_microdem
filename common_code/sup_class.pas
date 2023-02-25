unit sup_class;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordSatTrainProblems}
   //{$Define RecordUnsupClass}
   //{$Define RecordSupClass}
   //{$Define RecordFullClass}
   //{$Define RecordSatClass}
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

   {$IfDef UseBDETables}
      dbTables,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end core DB functions definitions

  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Math, System.UITypes,
  Dialogs, StdCtrls, Buttons, ExtCtrls, Grids, ComCtrls,
  MVClusterClientDataSet,
  Petmar_types,
  DEMDefs,DEMMapf;


type
  Tsupclasform = class(TForm)
    CheckBox1: TCheckBox;
    Panel1: TPanel;
    Label1: TLabel;
    Edit1: TEdit;
    ClassifyButton: TBitBtn;
    HelpBtn: TBitBtn;
    TrackBar1: TTrackBar;
    CheckBox6: TCheckBox;
    RadioGroup2: TRadioGroup;
    Edit2: TEdit;
    Label2: TLabel;
    RadioGroup1: TRadioGroup;
    BitBtn7: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn1: TBitBtn;
    Panel2: TPanel;
    BitBtn2: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    Label3: TLabel;
    ComboBox1: TComboBox;
    BitBtn6: TBitBtn;
    BitBtn8: TBitBtn;
    Label4: TLabel;
    Edit3: TEdit;
    BitBtn3: TBitBtn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure ClassifyButtonClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
  private
    { Private declarations }
    procedure FindClasses;
    procedure TryToOpenTrainSet(MGUsed,TrainingPointsDB,ClassesDB : integer);
    procedure SetBandLimitsToUse(MGUsed : integer);
  public
    { Public declarations }
    BaseMap : tMapForm;
    UseMG   : integer;
    Redrawing : boolean;

   NumClass         : integer;
   ClassesFileName  : PathStr;
   Classes          : ^ClassesType;
   ClassMissingData,
   ClassUnclassified  : integer;
   ClassCount      : array[1..MaxClass] of LongInt;
  end;

procedure StartSupervisedClassification(var Map : tMapForm);
//procedure UnsupervisedClassification(MGUsed : integer; var MapOwner : tMapForm);
function UnsupervisedClassification(MGUsed : integer; var MapOwner : tMapForm) : integer;


var
  supclasform: Tsupclasform;

implementation

{$R *.dfm}

uses
   {$IfDef ExGIS}
   {$Else}
   demdatabase,
   {$EndIf}

  Petmar,PetMath,DEMMapDraw, PetDBUtils, BaseGraf,DEMCoord,Geotiff,DEMDef_routines,Make_Tables,
  DEM_Manager,PetImage,
  Nevadia_main, map_overlays, multigrid, basemap, clusterOptions;



procedure Tsupclasform.TryToOpenTrainSet(MGUsed,TrainingPointsDB,ClassesDB : integer);
var
   i : integer;


    procedure LoadClassDefinitions;
    begin
       {$IfDef RecordSatClass} WriteLineToDebugFile('TMapForm.TryToOpenTrainSet, LoadClassDefinitions: ' + GISDB[ClassesDB].DBName);   {$EndIf}
       if (Classes = Nil) then New(Classes);
       NumClass := 0;
       GISDB[ClassesDB].MyData.First;
       while not GISDB[ClassesDB].MyData.EOF do begin
          inc(NumClass);
          Classes^[NumClass].ClassColor := GISDB[ClassesDB].MyData.PlatformColorFromTable;
          Classes^[NumClass].ClassName := GISDB[ClassesDB].MyData.GetFieldByNameAsString('CLASS');
          Classes^[NumClass].UseClass := GISDB[ClassesDB].MyData.GetFieldByNameAsString('USE') = 'Y';
          GISDB[ClassesDB].MyData.Next;
       end;
       {$IfDef RecordSatClass} WriteLineToDebugFile('NumClass=' + IntToStr(NumClass)); {$EndIf}
    end;


    procedure LoadClassStats;
    var
       i,band : integer;
       Missing : int64;
       fName : PathStr;
       zs : ^bfarray32;
       Line : AnsiString;
       q95,q90,q10,q5,
       rt,Min,Max,Median : float64;
       MomentVar : tMomentVar;
    begin
       {$IfDef RecordSatClass} WriteLineToDebugFile('TMapForm.TryToOpenTrainSet, LoadClassStats'); {$EndIf}
       GISDB[TrainingPointsDB].EmpSource.Enabled := false;
       for i := 1 to NumClass do begin
           GISDB[TrainingPointsDB].MyData.ApplyFilter('CLASS=' + QuotedStr(Classes^[i].ClassName));
           Classes^[i].ClassSize := GISDB[TrainingPointsDB].MyData.FiltRecsInDB;
           while not GISDB[TrainingPointsDB].MyData.eof do begin
              for Band := 1 to MaxBands do begin
                 if (MultiGridArray[MGUsed].Grids[Band] <> 0) then begin
                    fName := 'BAND_' + IntToStr(Band);
                    if GISDB[TrainingPointsDB].MyData.FieldExists(fName) and (GISDB[TrainingPointsDB].MyData.FiltRecsInDB > 1) then begin
                        New(zs);
                        GetFieldValuesInArrayLinkPossible(GISDB[TrainingPointsDB].MyData,Nil,'','',fName,zs^,MomentVar.Npts,Missing,Min,Max);
                        if (MomentVar.NPts > 1) then with MomentVar do begin
                           moment(zs^,MomentVar,msAll);
                           Classes^[i].Mean[Band] := mean;
                           Classes^[i].StdDev[Band] := sdev;
                           q95 := Quantile(95,zs^,MomentVar.NPts,true);
                           q90 := Quantile(90,zs^,MomentVar.NPts,true);
                           q10 := Quantile(10,zs^,MomentVar.NPts,true);
                           q5 := Quantile(5,zs^,MomentVar.NPts,true);
                           Classes^[i].ClassPerc95[Band] := q95;
                           Classes^[i].ClassPerc90[Band] := q90;
                           Classes^[i].ClassPerc10[Band] := q10;
                           Classes^[i].ClassPerc5[Band] := q5;
                        end;
                        Dispose(zs);
                        Classes^[i].ClassMin[Band] := Min;
                        Classes^[i].ClassMax[Band] := Max;
                        Line := Classes^[i].ClassName + ',' +
                                fName + ',' +
                                RealToString(min,-12,-2)  + ',' +
                                RealToString(q5,-12,-2)  + ',' +
                                RealToString(q10,-12,-2)  + ',' +
                                RealToString(MomentVar.mean,-12,-2) + ',' +
                                RealToString(q90,-12,-2)  + ',' +
                                RealToString(q95,-12,-2)  + ',' +
                                RealToString(max,-12,-2)  + ',' +
                                RealToString(MomentVar.sdev,-12,-2);
                        {$IfDef RecordSatClass} WriteLineToDebugFile(Line); {$EndIf}
                    end;
                    GISDB[TrainingPointsDB].MyData.Next;
                 end;
              end;
           end;
       end;
       GISDB[TrainingPointsDB].ClearGISFilter;
       GISDB[TrainingPointsDB].MyData.ApplyFilter('');
       GISDB[TrainingPointsDB].dbTablef.ShowStatus;
    end;



begin
   {$IfDef RecordSatClass} WriteLineToDebugFile('TMapForm.TryToOpenTrainSet in'); {$EndIf}

   if (ClassesDB = 0) or (TrainingPointsDB = 0) then begin
      {$IfDef RecordSatClass} WriteLineToDebugFile('No training data'); {$EndIf}
      MessageToContinue('No training data');
   end
   else begin
      for i := 1 to MaxBands do MultiGridArray[MGUsed].UseBand[i] := true;
      LoadClassDefinitions;
      LoadClassStats;
   end;
   {$IfDef RecordSatClass} WriteLineToDebugFile('TMapForm.TryToOpenTrainSet out'); {$EndIf}
end;

procedure Tsupclasform.SetBandLimitsToUse(MGUsed : integer);
var
   c,i : integer;
begin
   {$IfDef RecordSatClass} WriteLineToDebugFile('TMapForm.SetBandLimitsToUse in'); {$EndIf}
   if (MGUsed = 0) then exit;

   for c := 1 to NumClass do begin
      for i := 1 to MultiGridArray[MGUsed].PossGrids do begin
         if (MultiGridArray[MGUsed].Grids[i] <> 0) then begin
            case MDDef.ClassLimitMethod of
                 clmMeanStd : begin
                                 Classes^[c].ClassLowLimit[i] := Classes^[c].Mean[i] - MDDef.SatTrainStdDev * Classes^[c].StdDev[i];
                                 Classes^[c].ClassHighLimit[i] := Classes^[c].Mean[i] + MDDef.SatTrainStdDev * Classes^[c].StdDev[i];
                               end;
                 clm5_95     : begin
                                 Classes^[c].ClassLowLimit[i] := Classes^[c].ClassPerc5[i];
                                 Classes^[c].ClassHighLimit[i] := Classes^[c].ClassPerc95[i];
                               end;
                 clm10_90    : begin
                                 Classes^[c].ClassLowLimit[i] := Classes^[c].ClassPerc10[i] ;
                                 Classes^[c].ClassHighLimit[i] := Classes^[c].ClassPerc90[i] ;
                               end;
                 clmMinMax   : begin
                                 Classes^[c].ClassLowLimit[i] := Classes^[c].ClassMin[i] ;
                                 Classes^[c].ClassHighLimit[i] := Classes^[c].ClassMax[i] ;
                               end;
            end;
            {$IfDef RecordSatClass} WriteLineToDebugFile(Classes^[c].ClassName + ',' + 'Band_' + IntToStr(i) + ',' + RealToString(Classes^[c].ClassLowLimit[i],-12,-1) + ',' + RealToString(Classes^[c].ClassHighLimit[i],-12,-1));          {$EndIf}
         end;
      end {for i};
   end {for c};
   {$IfDef RecordSatClass} WriteLineToDebugFile('TMapForm.SetBandLimitsToUse out'); {$EndIf}
end;


function UnsupervisedClassification(MGUsed : integer; var MapOwner : tMapForm) : integer;
var
   BestClass,x,y,i,j,ClassDEM : integer;
   N : array[0..MaxClusters] of integer;
   PV : float32;
   aSum,aMin : float64;
   Table : tMyData;
   aLine : ANSIString;
   fName2,fName3,UnSupClassDir : PathStr;
   ClassName : shortstring;
   vat : tStringList;
   Color : tColor;
   MVClusterClientDataSet : TMVClusterClientDataSet;
   MinSampler,Sampler : integer;
   Limits : tGridLimits;
   FieldsUsed : byte;
   MetaData : tStringList;
   FieldsToUse : array[1..MaxBands] of AnsiString;
begin
   {$IfDef RecordUnsupClass} WriteLineToDebugFile('UnsupervisedClassification in, MG=' + IntToStr(MGUsed)); {$EndIf}
   NakedMapOptions;
   MVClusterClientDataSet := TMVClusterClientDataSet.Create(Application);
   DefineMICRODEMClusteringOptions(MVClusterClientDataSet);

   if MDDef.UnSupClassFullImage then Limits := DEMGlb[MultiGridArray[MGUsed].Grids[MultiGridArray[MGUsed].FirstValidGrid]].FullDEMGridLimits
   else Limits := MapOwner.MapDraw.MapAreaDEMGridLimits;

   Sampler := 1;
   while ((Limits.XGridHigh-Limits.XGridLow) div Sampler) * ((Limits.YGridHigh-Limits.YGridLow) div Sampler) > EdburgGeneralFuncsMaxObservations do inc(Sampler);

   MinSampler := Sampler;
   if not ClusterOptions.GetClusterOptions(Sampler,false) then exit;

   if (Sampler < MinSampler) then begin
      MessageToContinue('Min sampling=' + IntToStr(MinSampler));
      Sampler := MinSampler;
   end;

   MetaData := tStringList.Create;
   MetaData.Add('Sampling factor: ' + IntToStr(Sampler));
   MetaData.Add('Power: ' + RealToString(MDDef.ClassDistancePower,-8,-2));
   MetaData.Add('Max clusters=' + IntToStr(MDDef.NumClusters));
   MetaData.Add('Cluster iterations: ' + IntToStr(MDDef.ClusterIterations));
   MetaData.Add('Cluster initialization: ' +  IntToStr(ord(MDDef.ClusterInitialization)));
   FieldsUsed := 0;
   for i := 1 to MaxGridsInMG do begin
         if MultiGridArray[MGUsed].UseBand[i] and ValidDEM(MultiGridArray[MGUsed].Grids[i]) then begin
            {$IfDef RecordUnsupClass} WriteLineToDebugFile('Use band=' + IntToStr(i)); {$EndIf}
            MVClusterClientDataSet.FieldDefs.Add('BAND_' + IntToStr(i),ftFloat, 0, False);
            inc(FieldsUsed);
            FieldsToUse[FieldsUsed] := 'BAND_' + IntToStr(i);
         end
         else begin
            aLine := 'Skip band=' + IntToStr(i);
            MetaData.Add(aline);
         end;
      end;
      MetaData.Add('Bands used = ' + IntToStr(FieldsUsed));

      MVClusterClientDataSet.CreateDataset;
      MVClusterClientDataSet.Open;
      {$IfDef RecordUnsupClass} WriteLineToDebugFile('MVClusterClientDataSet.Open over, bands used=' + IntToStr(FieldsUsed) + '  First valid grid=' + IntToStr(MultiGridArray[MGUsed].FirstValidGrid)); {$EndIf}
      StartProgress('Load sample pts ' + MultiGridArray[MGUsed].MG_Name);
      y := Limits.YGridLow;
      while y <= Limits.YGridHigh do begin
         {$IfDef RecordSatLoops} WriteLineToDebugFile('y=' + IntToStr(y)); {$EndIf}
         if (y mod 50 = 0) then UpdateProgressBar((y-Limits.YGridLow)/(Limits.YGridHigh-Limits.YGridLow));
         x := Limits.XGridLow;
         while x <= Limits.XGridHigh do begin
            if DEMGlb[MultiGridArray[MGUsed].Grids[MultiGridArray[MGUsed].FirstValidGrid]].GridInDataSet(x,y) then begin
               if DEMGlb[MultiGridArray[MGUsed].Grids[MultiGridArray[MGUsed].FirstValidGrid]].GetElevMeters(x,y,PV) then begin
                  MVClusterClientDataSet.Insert;
                  for i := 1 to MaxGridsInMG do begin
                     if MultiGridArray[MGUsed].UseBand[i] and ValidDEM(MultiGridArray[MGUsed].Grids[i]) then begin
                        DEMGlb[MultiGridArray[MGUsed].Grids[i]].GetElevMetersOnGrid(x,y,PV);
                        MVClusterClientDataSet.FieldByName('BAND_' + IntToStr(i)).AsFloat := PV;
                     end;
                  end;
                  MVClusterClientDataSet.Post;
                 end;
                 inc(x,sampler);
               end;
           end;
         inc(y,sampler);
      end;
      aLine := 'Sampling points=' + IntToStr(MVClusterClientDataSet.RecordCount);
      MetaData.Add('');
      MetaData.Add(aline);
      wmDEM.SetPanelText(0,'Save pts');
      MVClusterClientDataSet.SaveToFile(MDTempDir + 'points.cds',dfBinary);
      {$IfDef RecordUnsupClass} WriteLineToDebugFile('TMapForm.Unsupervisedclassification1Click data loaded  ' + aLine); {$EndIf}
      EndProgress;
      ShowHourglassCursor;
      wmDEM.SetPanelText(0,'K Means Clustering');
      MVClusterClientDataSet.KMeansClustering(FieldsToUse, FieldsUsed, MDTempDir + 'Cluster_Results.HTML');
      MVClusterClientDataSet.SaveToFile(MDTempDir + 'clusters.cds',dfBinary);
      {$IfDef RecordUnsupClass} WriteLineToDebugFile('KMeansClustering over'); {$EndIf}
       wmDEM.SetPanelText(0,'');
   UnSupClassDir := ExtractFilePath(MultiGridArray[MGUsed].BasePath) + 'unsup_class\';
   SafeMakeDir(UnSupClassDir);
   fName2 := NextFileNumber(UnSupClassDir,MultiGridArray[MGUsed].MG_Name + '_unsup_class_' ,'.dem');
   ClassName := ExtractFileNameNoExt(fName2);
   {$IfDef RecordUnsupClass} WriteLineToDebugFile('Directory created'); {$EndIf}
   ApplicationProcessMessages;
   ClassDEM := DEMGlb[MultiGridArray[MGUsed].Grids[MultiGridArray[MGUsed].FirstValidGrid]].CloneAndOpenGridSetMissing(byteDEM,ClassName,UnDefined);

   {$IfDef RecordUnsupClass} WriteLineToDebugFile('New grid created'); {$EndIf}

   for i := 1 to MaxClusters do n[i] := 0;

   StartProgress('Unsupervised Class ' + MultiGridArray[MGUsed].MG_Name);
   y := Limits.YGridLow;
   while (y <= Limits.YGridHigh) do begin
      if (y mod 50 = 0) then begin
         UpDateProgressBar((y-Limits.YGridLow)/(Limits.YGridHigh-Limits.YGridLow));
         {$IfDef RecordSatLoops} WriteLineToDebugFile('y=' + IntToStr(y)); {$EndIf}
      end;

      x := Limits.XGridLow;
      while (x <= Limits.XGridHigh) do begin
         if DEMGlb[MultiGridArray[MGUsed].Grids[MultiGridArray[MGUsed].FirstValidGrid]].GridInDataSet(x,y) then begin
            if DEMGlb[MultiGridArray[MGUsed].Grids[MultiGridArray[MGUsed].FirstValidGrid]].GetElevMetersOnGrid(x,y,PV) then begin
                aMin := 99e39;
                for j := 1 to MVClusterClientDataSet.NClusters do if (MVClusterClientDataSet.ClsCounts[j] > 0) then begin
                   aSum := 0;
                   for i := 1 to MaxGridsInMG do begin
                      if MultiGridArray[MGUsed].UseBand[i] and ValidDEM(MultiGridArray[MGUsed].Grids[i]) then begin
                         DEMGlb[MultiGridArray[MGUsed].Grids[i]].GetElevMeters(x,y,PV);
                         PV := PV - MVClusterClientDataSet.ClsCenters[j,i];
                         PV := Math.Power(abs(pv),MDDef.ClassDistancePower);
                         aSum:= aSum + PV;
                      end;
                   end;
                   if (aSum < aMin ) then begin
                      aMin := aSum;
                      BestClass:= j;
                   end;
                end; // for j
                DEMGlb[ClassDEM].SetGridElevation(x,y,BestClass);
                inc(n[BestClass]);
            end;
         end;
         inc(x);
      end {for x};
      inc(y);
   end;
   EndProgress;

   DEMGlb[ClassDEM].CheckMaxMinElev;
   DEMGlb[ClassDEM].WriteNewFormatDEM(fName2);
   DEMGlb[ClassDEM].DEMFileName := fName2;
   {$IfDef RecordUnsupClass} WriteLineToDebugFile('UnsupervisedClassification created ' + fName2); {$EndIf}

   fName3 := ChangeFileExt(fName2, '.metadata.txt');
   fName2 := ChangeFileExt(fName2, '.vat.dbf');
   {$IfDef RecordUnsupClass} WriteLineToDebugFile('Create VAT table ' + fName2); {$EndIf}
    Vat := tStringList.Create;
    Vat.add('VALUE,NAME,N,USE,COLOR');
    i := 1;
    for j := 1 to MVClusterClientDataSet.NClusters do begin
       if (n[j] > 0) then begin
          Color := WinGraphColors[i mod 15];
          Vat.add(IntToStr(j) + ',' + 'Cluster ' + IntToStr(i) + ',' + IntToStr(n[j]) + ',Y,' + IntToStr(Color));
          MetaData.Add('Cluster ' + IntToStr(i) + '   n=' + IntToStr(n[j]));
          inc(i);
       end;
    end;
    StringList2CSVtoDB(vat,fName2,true);

    MetaData.SaveToFile(fName3);
    MetaData.Destroy;

    {$IfDef RecordUnsupClass} WriteLineToDebugFile('Call setupmap'); {$EndIf}
    DEMGlb[ClassDEM].SetUpMap(ClassDEM,true,mtDEMVATTable);

    MVClusterClientDataSet.Free;
    wmDEM.SetPanelText(0,'');
    ShowDefaultCursor;
    RestoreBackupDefaults;
    Result := ClassDEM;

   {$IfDef RecordUnsupClass} WriteLineToDebugFile('UnsupervisedClassification out'); {$EndIf}
end;



procedure StartSupervisedClassification;
begin
   {$IfDef RecordSupClass} WriteLineToDebugFile('StartSupervisedClassification in  BaseMap = ' + Map.Caption); {$EndIf}
   supclasform := Tsupclasform.Create(Application);
   supclasform.BaseMap := Map;
   supclasform.UseMG := Map.MapDraw.MultiGridOnMap;
   supclasform.BitBtn3.Enabled := false;
   supclasform.Show;
   {$IfDef RecordSupClass} WriteLineToDebugFile('StartSupervisedClassification out, basemap=' + supclasform.BaseMap.Caption); {$EndIf}
end;



procedure Tsupclasform.BitBtn10Click(Sender: TObject);
var
   c : integer;
begin
   for c := 1 to NumClass do
      if Classes^[c].UseClass then
         GISdb[MultiGridArray[UseMG].TrainingPointsDB].dbtablef.MakeGridBandsInClassBox(Classes^[c].ClassName);
end;


procedure Tsupclasform.BitBtn1Click(Sender: TObject);
begin
   MultiGridArray[BaseMap.MapDraw.MultiGridOnMap].MakeSlopeCorrelationGrids;
end;


procedure Tsupclasform.BitBtn2Click(Sender: TObject);
begin
   {$IfDef RecordSupClass}   WriteLineToDebugFile('Tsupclasform.BitBtn2Click (new training set) in');   {$EndIf}
   MultiGridArray[UseMG].NewTrainingSet;
   FindClasses;
   {$IfDef RecordSupClass} WriteLineToDebugFile('Tsupclasform.BitBtn2Click (new training set) out'); {$EndIf}
end;

procedure Tsupclasform.BitBtn3Click(Sender: TObject);
begin
   MessageToContinue('disabled currently');
end;


procedure Tsupclasform.FindClasses;
var
   DataThere : tStringList;
   i : integer;
begin
   {$IfDef RecordSupClass} WriteLineToDebugFile('Tsupclasform.FindClasses in'); {$EndIf}
   GISdb[MultiGridArray[UseMG].TrainingPointsDB].EmpSource.Enabled := false;
   DataThere := GISdb[MultiGridArray[UseMG].TrainingPointsDB].MyData.UniqueEntriesInDB('CLASS');
   if (DataThere.Count > 0) then begin
      ComboBox1.Items := DataThere;
      ComboBox1.Text := DataThere[0];
      MultiGridArray[UseMG].CurrentClass := DataThere[0];
      MultiGridArray[UseMG].CurrentColor := ConvertTColorToPlatformColor(Color);
      if GISDB[MultiGridArray[UseMG].ClassesDB].MyData.RecordCount = 0 then begin
         for i := 0 to pred(DataThere.Count) do begin
            inc(NumClass);
            GISdb[MultiGridArray[UseMG].TrainingPointsDB].MyData.ApplyFilter('CLASS=' + QuotedStr(DataThere.Strings[i]));
            GISDB[MultiGridArray[UseMG].ClassesDB].MyData.Insert;
            GISDB[MultiGridArray[UseMG].ClassesDB].MyData.SetFieldByNameAsString('CLASS',DataThere.Strings[i]);
            GISDB[MultiGridArray[UseMG].ClassesDB].MyData.SetFieldByNameAsString('USE','Y');
            GISDB[MultiGridArray[UseMG].ClassesDB].MyData.SetFieldByNameAsInteger('COLOR',GISdb[MultiGridArray[UseMG].TrainingPointsDB].MyData.GetFieldByNameAsInteger('COLOR'));
            GISDB[MultiGridArray[UseMG].ClassesDB].MyData.Post;
         end;
      end;
   end;
   DataThere.Free;
   GISdb[MultiGridArray[UseMG].TrainingPointsDB].EmpSource.Enabled := true;
   ClassifyButton.Enabled := true;
   {$IfDef RecordSupClass} WriteLineToDebugFile('Tsupclasform.FindClasses out'); {$EndIf}
end;

procedure Tsupclasform.BitBtn4Click(Sender: TObject);
begin
   {$IfDef RecordSupClass} WriteLineToDebugFile('Tsupclasform.BitBtn4Click (load training set)'); {$EndIf}
   MultiGridArray[UseMG].LoadTrainingSet;
   FindClasses;
end;

procedure Tsupclasform.BitBtn5Click(Sender: TObject);
var
   fName : shortstring;
   color : tPlatformcolor;
begin
   fName := 'new class';
   Petmar.GetString('class name',fName,true,Petmar_types.DBaseFieldNameChars);
   Color := claRed;
   QueryColor(Color);
   ComboBox1.Items.Add(fName);
   ComboBox1.Text := fName;
   inc(NumClass);

   MultiGridArray[UseMG].CurrentClass := fName;
   MultiGridArray[UseMG].CurrentColor := Color;
   GISDB[MultiGridArray[UseMG].ClassesDB].MyData.Insert;
   GISDB[MultiGridArray[UseMG].ClassesDB].MyData.SetFieldByNameAsString('CLASS',fName);
   GISDB[MultiGridArray[UseMG].ClassesDB].MyData.SetFieldByNameAsString('USE','Y');
   GISDB[MultiGridArray[UseMG].ClassesDB].MyData.SetFieldByNameAsInteger('NPTS',0);
   GISDB[MultiGridArray[UseMG].ClassesDB].MyData.SetFieldByNameAsInteger('COLOR',ConvertPlatformColorToTColor(Color));
   GISDB[MultiGridArray[UseMG].ClassesDB].MyData.Post;
end;

procedure Tsupclasform.BitBtn6Click(Sender: TObject);
begin
   ChangeDEMNowDoing(PickTrainingPoints);
end;

procedure Tsupclasform.BitBtn7Click(Sender: TObject);
begin
  (*
   if VerifyRecordsToUse(SatImage[BaseMap.MapDraw.SATonMap].SelectionMap.ClassesFileName,'NAME','Classes to classify') then begin
      SatImage[BaseMap.MapDraw.SATonMap].SelectionMap.LoadClassDefinitions;
   end;
   *)
   MessageToContinue('disabled currently');
end;

procedure Tsupclasform.BitBtn8Click(Sender: TObject);
begin
   ChangeDEMNowDoing(PickTrainingBox);
end;

procedure Tsupclasform.BitBtn9Click(Sender: TObject);
var
   c : integer;
begin
   for c := 1 to NumClass do
      if Classes^[c].UseClass then
         GISdb[MultiGridArray[UseMG].TrainingPointsDB].dbtablef.MakeDistanceGridToClassCentroid(Classes^[c].ClassName);
end;

procedure Tsupclasform.CheckBox6Click(Sender: TObject);
begin
   MDDef.ShowSupClassDB := CheckBox6.Checked;
end;


procedure Tsupclasform.ClassifyButtonClick(Sender: TObject);


      procedure CreateSatTrainingDEM(ClassDEM : integer);
      var
        {$IfDef RecordFullClass}
           nd : integer;
        {$EndIf}
         x,y,ThisClass : integer;

             function ClassifyColor(x,y : integer; var ThisClass : integer) : boolean;
             label
                NextClass;
             var
                c,Band,BandsInBox : integer;
                MinDist,Dist : Extended;
                Pt : array[1..MaxBands] of float32;
             begin
                {$IfDef RecordFullClass}  nd := nd + 1; if nd < 10 then WriteLineToDebugFile('ClassifyColor in'); {$EndIf}
                 Result := false;
                 MinDist := 99e38;
                 for c := 1 to NumClass do if (Classes^[c].UseClass) and (Classes^[c].ClassSize > 0) then begin
                    BandsInBox := 0;
                    for Band := 1 to MultiGridArray[UseMG].PossGrids do begin
                       if MultiGridArray[UseMG].UseBand[Band] and (MultiGridArray[UseMG].Grids[Band] <> 0) then begin
                          DEMGlb[MultiGridArray[UseMG].Grids[Band]].GetElevMetersOnGrid(x,y,Pt[Band]);
                          if (Pt[Band] >= Classes^[c].ClassLowLimit[Band]) and
                             (Pt[Band] <= Classes^[c].ClassHighLimit[Band]) then inc(BandsInBox);   //goto NotThisClass;
                         {$IfDef RecordFullClass}
                            if (nd < 10) then WriteLineToDebugFile('Band=' + IntToStr(Band) + '  Ref=' + RealToString(Pt[Band],-12,0) +
                                '  Low=' + RealToString(Classes^[c].ClassLowLimit[Band],-12,0)+ '  High=' + RealToString(Classes^[c].ClassHighLimit[Band],-12,0));
                         {$EndIf}
                       end;
                    end;
                   {$IfDef RecordFullClass} if (nd < 10) then WriteLineToDebugFile('Class=' + IntToStr(c) + '   Bands in box=' + IntToStr(BandsInBox)); {$EndIf}
                    if (BandsInBox >= MDDef.BandsRequiredInBox) then begin
                       Dist := 0;
                       for Band := 1 to MultiGridArray[UseMG].NumGrids do if MultiGridArray[UseMG].UseBand[Band] then begin
                          Dist := Dist + Math.Power(abs(Pt[Band] - Classes^[c].Mean[Band]),MDDef.ClassDistancePower);
                       end;
                      {$IfDef RecordFullClass} if (nd < 10) then WriteLineToDebugFile('Dist=' + RealToString(Dist,-12,-2)); {$EndIf}
                       if (Dist < MinDist) then begin
                          MinDist := Dist;
                          ThisClass := c;
                       end {if};
                    end;
                    NextClass:;
                 end {for c};
                 if MinDist < 1e38 then begin
                    with Classes^[ThisClass] do begin
                       inc(ClassCount[ThisClass]);
                       Result := true;
                    end {if};
                 end;
             end;

      begin
         {$IfDef RecordSatTrainProblems} WriteLineToDebugFile('CreateSatTrainingDEM in'); {$EndIf}
         SetBandLimitsToUse(UseMG);
         ClassMissingData := 0;
         ClassUnclassified := 0;
         {$IfDef RecordFullClass}
            nd := 0;
         {$EndIf}

         StartProgressAbortOption('Classify');
           for y := 0 to pred(DEMGlb[MultiGridArray[UseMG].Grids[1]].DEMheader.NumRow) do begin
              if (y mod 100 = 0) then UpDateProgressBar(y/DEMGlb[MultiGridArray[UseMG].Grids[1]].DEMheader.NumRow);
              for x := 0 to pred(DEMGlb[MultiGridArray[UseMG].Grids[1]].DEMheader.NumCol) do begin
                 if DEMGlb[MultiGridArray[UseMG].Grids[1]].MissingDataInGrid(x,y) then begin
                    inc(ClassMissingData);
                    DEMGlb[ClassDEM].SetGridMissing(x,y);
                 end
                 else begin
                    if ClassifyColor(x,y,ThisClass) then begin
                       DEMGlb[ClassDEM].SetGridElevation(x,y,ThisClass);
                    end
                    else begin
                       inc(ClassUnclassified);
                       DEMGlb[ClassDEM].SetGridElevation(x,y,0);
                    end;
                 end;
              end;
           end;
           EndProgress;
          {$IfDef RecordSatTrainProblems} WriteLineToDebugFile('CreateSatTrainingDEM out, no datas= ' + IntToStr(ClassUnclassified)); {$EndIf}
      end;


var
   i,Total,ClassDEM : integer;
   UnClass,Classed  : float64;
   TStr             : AnsiString;
   Results,Metadata : tStringList;
   fName,fName2 : PathStr;
begin
   {$IfDef RecordSatTrainProblems} WriteLineToDebugFile('Tsupclasform.ClassiClick in'); {$EndIf}

   TryToOpenTrainSet(UseMG,MultiGridArray[UseMG].TrainingPointsDB,MultiGridArray[UseMG].ClassesDB);
   SetBandLimitsToUse(UseMG);

   if (NumClass = 0) then begin
      MessageToContinue('No classes');
      exit;
   end;

   {$IfDef RecordSatTrainProblems}
      WriteLineToDebugFile('Classes used');
      for i := 1 to NumClass do begin
         if Classes^[i].UseClass then
            WriteLineToDebugFile(IntToStr(i) + '  ' + Classes^[i].ClassName  + '   ' +IntToStr(Classes^[i].ClassSize));
      end;
      WriteLineToDebugFile('');
   {$EndIf}

   fName2 := NextFileNumber(MultiGridArray[UseMG].SupClassDir, 'sc_' ,'.dem');
   //ClassName := MultiGridArray[UseMG].MG_Name + '_' + ExtractFileNameNoExt(fName2);
   ClassDEM := DEMGlb[MultiGridArray[UseMG].Grids[1]].CloneAndOpenGridSetMissing(byteDEM,ClassName,Undefined);

   try
      {$IfDef RecordSatTrainProblems} WriteLineToDebugFile('DisplayClassifiedBand 2'); {$EndIf}

      if MDDef.BandsRequiredInBox >  MultiGridArray[UseMG].NumGrids then begin
         ReadDefault('Bands that must be in limits', MDDef.BandsRequiredInBox);
      end;

       CreateSatTrainingDEM(ClassDEM);
       DEMGlb[ClassDEM].CheckMaxMinElev;
       DEMGlb[ClassDEM].WriteNewFormatDEM(fName2);
       {$IfDef RecordSatTrainProblems} WriteLineToDebugFile('Save DEM  ' + fName2 ); {$EndIf}

       Results := tStringList.Create;
       Results.Add('NAME,VALUE,N,USE,COLOR');
       MetaData := tStringList.Create;
       Metadata.Add('MICRODEM Supervised classification ' + TimeToStr(Now) + '  ' + DateToStr(Now));
       Metadata.Add('Classes considered:');
       Metadata.Add('Class limits: ' + RadioGroup1.Items[RadioGroup2.ItemIndex]);
       if RadioGroup2.ItemIndex = 4 then Metadata.Add('Std dev=' + RealToString(MDDef.SatTrainStdDev,-8,-2));

       Metadata.Add('Bands required in limits: ' + IntToStr(MDDef.BandsRequiredInBox));
       Metadata.Add('');
       for i := 1 to NumClass do if Classes^[i].UseClass then begin
          TStr := Classes^[i].ClassName +  ',' + IntToStr(i) + ',' + IntToStr(ClassCount[i]);
          Metadata.Add(TStr);
          TStr := TStr + ',Y,' + IntToStr(ConvertPlatformColorToTcolor(Classes^[i].ClassColor));
          Results.Add(TStr);
          {$IfDef RecordSatTrainProblems}   WriteLineToDebugFile(TStr); {$EndIf}
       end {for i};
       TStr := 'Unclassified' + ',255,' + IntToStr(ClassUnclassified);
       Metadata.Add(TStr);

       {$IfDef RecordSatTrainProblems} WriteStringListToDebugFile(Metadata); {$EndIf}

       fName := ChangeFileExt(fName2,'.metadata.txt');
       Metadata.SaveToFile(fName);
       Metadata.Free;
       Results.Add('Unclassified,255,' + IntToStr(ClassUnclassified) + ',Y,' + IntToStr(ConvertPlatformColorToTColor(MDDef.MissingDataColor)));
       fName := ChangeFileExt(fName2,'.vat.csv');
       Results.SaveToFile(fName);
       DoCSVFileImport(fName);
       SysUtils.DeleteFile(fName);
       {$IfDef RecordSatTrainProblems} WriteLineToDebugFile('Save VAT  ' + fName); {$EndIf}
       ApplicationProcessMessages;

        UnClass := 100;
        Total := ClassUnclassified;
        for i := 1 to NumClass do Total := Total + ClassCount[i];
        Results := tStringList.Create;
        Results.Add('NAME,N,PERCENT,COLOR');
        for i := 1 to NumClass do if Classes^[i].UseClass then begin
           Classed := 100.0 * ClassCount[i] / Total;
           UnClass := UnClass - Classed;
           TStr := Classes^[i].ClassName;
           StripCharacter(TStr,',');
           TStr := TStr + ',' + IntToStr(ClassCount[i]) + ',' + RealToString(Classed,8,2) + ',' + IntToStr(ConvertPlatformColorToTColor(Classes^[i].ClassColor));
           Results.Add(TStr);
           {$IfDef RecordSatTrainProblems} WriteLineToDebugFile(TStr); {$EndIf}
        end {for i};
        Results.Add('Unclassified' + ',' + IntToStr(ClassUnclassified) + ',' + RealToString(UnClass,8,2) + ',' + IntToStr(clWhite));
        fName := MDTempDir + ClassName + '_class_results.csv';

        if MDDef.ShowSupClassDB then BaseMap.DisplayAndPurgeStringListDB(Results,fName);
        CloseSingleDEM(ClassDEM);
        LoadNewDEM(ClassDEM,fName2,true);
        DEMGlb[ClassDEM].SelectionMap.GridVATLegend1Click(Nil);
  finally
     EndProgress;
     {$IfDef RecordSatTrainProblems} WriteLineToDebugFile('DisplayClassifiedBand done'); {$EndIf}
   end;
end;

procedure Tsupclasform.ComboBox1Change(Sender: TObject);
begin
   MultiGridArray[UseMG].CurrentClass := ComboBox1.Text;
   GISDB[MultiGridArray[UseMG].ClassesDB].MyData.ApplyFilter('CLASS=' + QuotedStr(MultiGridArray[UseMG].CurrentClass));
   MultiGridArray[UseMG].CurrentColor := GISDB[MultiGridArray[UseMG].ClassesDB].MyData.PlatformColorFromTable;
   GISDB[MultiGridArray[UseMG].ClassesDB].MyData.ApplyFilter('');
end;

procedure Tsupclasform.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDDef.SatTrainStdDev);
end;

procedure Tsupclasform.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDDef.BandsRequiredInBox);
end;

procedure Tsupclasform.Edit3Change(Sender: TObject);
begin
  CheckEditString(Edit3.Text,MDDef.MaxPointsAddInBox);
end;

procedure Tsupclasform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
   if (Classes <> Nil) then Dispose(Classes);
end;

procedure Tsupclasform.FormCreate(Sender: TObject);
begin
   Redrawing := false;
   Classes := Nil;
   NumClass := 0;

   Edit1.Text := realToString(MDDef.SatTrainStdDev,-8,-2);
   Edit2.Text := IntToStr(MDDef.BandsRequiredInBox);
   Edit3.Text := IntToStr(MDDef.MaxPointsAddInBox);

   TrackBar1.Position := MDDef.ClassifyIHSOpacity;
   CheckBox6.Checked := MDDef.ShowSupClassDB;
   RadioGroup1.ItemIndex := ord(MDDef.ClassLimitMethod);
   RadioGroup2.ItemIndex := pred(round(MDDef.ClassDistancePower * 2));
   WMDEM.FormPlacementInCorner(Self);
end;


procedure Tsupclasform.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\train_classify.htm');
end;


procedure Tsupclasform.RadioGroup1Click(Sender: TObject);
begin
   MDDef.ClassLimitMethod := tClassLimitMethod(RadioGroup1.ItemIndex);
   SetBandLimitsToUse(UseMG);
end;

procedure Tsupclasform.RadioGroup2Click(Sender: TObject);
begin
   MDDef.ClassDistancePower := 0.5 * succ(RadioGroup2.ItemIndex);
end;


procedure Tsupclasform.TrackBar1Change(Sender: TObject);
begin
   {$IfDef RecordSatTrainProblems} WriteLineToDebugFile('Tsupclasform.TrackBar1Change in'); {$EndIf}
   if (BaseMap <> Nil) and (not Redrawing) then begin
       MDDef.ClassifyIHSOpacity := TrackBar1.Position;
       Redrawing := true;
       {$IfDef RecordSatTrainProblems} WriteLineToDebugFile('Tsupclasform.TrackBar1Change 2'); {$EndIf}
        BaseMap.DoFastMapRedraw;
        Redrawing := false;
       {$IfDef RecordSatTrainProblems} WriteLineToDebugFile('Tsupclasform.TrackBar1Change out'); {$EndIf}
   end;
end;


initialization
finalization
  {$IfDef RecordSatTrainProblems} WriteLineToDebugFile('RecordSatTrainProblems active in sup_class'); {$EndIf}
  {$IfDef RecordUnsupClass} WriteLineToDebugFile('RecordUnsupClass active in sup_class'); {$EndIf}
  {$IfDef RecordSupClass} WriteLineToDebugFile('RecordSupClass active in sup_class'); {$EndIf}
  {$IfDef RecordFullClass} WriteLineToDebugFile('RecordFullClass active in sup_class'); {$EndIf}
  {$IfDef RecordSatClass} WriteLineToDebugFile('RecordSatClass active in sup_class'); {$EndIf}
end.
