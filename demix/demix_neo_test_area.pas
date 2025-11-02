unit demix_neo_test_area;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}


//{$Define DoOnlyThreeLandTypes}  //faster operation when tracking down errors


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordDEMIXneo}
   //{$Define RecordLoadDEMIX}
{$EndIf}


interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics,AnsiStrings, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  StrUtils,
  petmar,petmar_types;


type
  TNew_area_evals_form = class(TForm)
    RadioGroup1: TRadioGroup;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    AreaName : shortstring;
  end;


  procedure StartAreaEvals;
  procedure CrossScaleDEMComparison(DEMIXNeoMode : integer);


var
  New_area_evals_form: TNew_area_evals_form;

implementation

{$R *.dfm}

uses
   demix_definitions,demix_control,demstringgrid,
   PetMath,PetdbUtils,Petmar_DB,
   dem_Manager,
   Make_grid,
   nevadia_main,
   DEMdataBase,
   dem_nlcd,
   demdefs,demstat,DEMcoord;


procedure CrossScaleDEMComparison(DEMIXNeoMode : integer);
//this is hard coded during active development for the current options
const
   Overwrite = true;
   DoDiffDist = true;
   OpenMaps = false;
var
   fName,DataDir,DEMName : PathStr;
   Areas,TheDEMs : tStringList;
   DEMList,SlopeList : tDEMBooleanArray;
   i,n,aDEM,aSlope,ESA_LC10 : integer;
   Fixed : int64;
   Findings : tStringList;


   procedure OpenElevationAndSlopeGrids;
   const
      NumDEMs = 7;
      DEMsInOrder : array[1..NumDEMs] of shortstring = ('Point_cloud_DSM','CopDEM_upsample','AW3D30_upsample','Native_5m_DTM','HRDEM_downsample','Point_cloud_NVS','FABDEM_upsample');
   var
      i : integer;
      DEMName : ShortString;
   begin
      DataDir := 'J:\aaa_neo_eval\oxnard\multiple_0.15sec\';
      InitializeDEMsWanted(DEMList,false);
      InitializeDEMsWanted(SlopeList,false);
      n := 0;
      for I := 1 to NumDEMs do begin
         DEMName := DataDir + DEMsinOrder[i] + '.tif';
         if FileExists(DEMName) then begin
            inc(n);
            aDEM := OpenNewDEM(DEMName,OpenMaps);
            DEMList[aDEM] := true;
            aSlope := CreateEvansSlopeMapPercent(OpenMaps, aDEM);
            fName := MDTempDir + DEMGlb[aSlope].AreaName + '.dem';
            DEMGlb[aSlope].WriteNewFormatDEM(fName);
            SlopeList[aSlope] := true;
         end;
      end;
      {$IfDef RecordLoadDEMIX} WriteDEMListToDebug('Elevation DEMs',DEMlist); WriteDEMListToDebug('Slope grids',SlopeList); {$EndIf}
      ESA_LC10 := LoadLC10LandCover('',DEMGlb[1].DEMBoundBoxGeo,OpenMaps);
      {$IfDef RecordDEMIXneo} WriteLineToDebugFile('Loadeds DEMs and created slope maps, n=' + IntToStr(n)); {$EndIf}
   end;


   procedure MakeGraphDifferenceDistributionsAllLandCover;
   var
      zs : ^bfArray32;
      Findings : tstringList;
      MomentVar : tMomentVar;


      procedure ComputeDifferenceDistribution(Parameter,LandType : shortstring; LandCoverCode,Ref,Test : integer);

            function DoParameter(Test,Ref : integer) : boolean;
            var
               Col,Row,i : integer;
               Difference,zref,ztest : float32;
            begin
               PetMath.InitializeMomentVar(MomentVar);
               for Col := 0 to pred(DEMglb[ref].DemHeader.NumCol) do begin
                  for Row := 0 to pred(DEMglb[ref].DemHeader.NumRow) do begin
                     if DEMGlb[Ref].GetElevMetersOnGrid(col,row,zref) and DEMGlb[test].GetElevMetersOnGrid(col,row,ztest) then begin
                        Difference := ztest-zref;
                        inc(MomentVar.NPts);
                        zs^[MomentVar.NPts] := Difference;
                     end;
                  end;
               end;
               moment(zs^,MomentVar,msAll);
               for i := 0 to pred(MomentVar.NPts) do zs^[i] := abs(zs^[i]);
               MomentVar.LE90 := Percentile(90,zs^,MomentVar.NPts,false);
            end;

      var
         aLine : shortstring;
      begin {procedure ComputeDifferenceDistribution}
         {$IfDef RecordDEMIXneoFull} WriteLineToDebugFile('Start landcover for ' + LandType + ' ref=' + IntToStr(Ref) + ' test=' + IntToStr(Test)); {$EndIf}
         wmdem.SetPanelText(2,Parameter,true);
         wmdem.SetPanelText(3,LandType,true);

         if (LandCoverCode <> 0) then begin
            DEMGLb[ESA_LC10].MarkOutsideRangeMissing(LandCoverCode-0.01,LandCoverCode+0.01,Fixed,false);
            MaskGridFromSecondGrid(Ref,ESA_LC10, msSecondMissing);
            MaskGridFromSecondGrid(Test,ESA_LC10, msSecondMissing);
         end;

         DoParameter(Test,Ref);
         aline := 'Test area, test area,' + DEMGlb[Test].AreaName + ',' + Parameter + ',' + LandType + ',' + DEMIXMomentStatsString(MomentVar);
         Findings.Add(aLine);

         if (LandCoverCode <> 0) then begin
            DEMGLb[ESA_LC10].ReloadDEM(true);
            DEMGlb[Ref].ReloadDEM(true);
            DEMGlb[Test].ReloadDEM(true);
         end;
      end {procedure ComputeDifferenceDistribution};


      procedure DoOneTestDEM(RefElev,RefSlope,RefRuff,TestElev : integer; ManyLandcover : boolean);
      var
         i,Ref,Test,
         TestSlope,
         TestRuff : integer;
      begin
        TestSlope := 0;
        TestRuff := CreateSlopeRoughnessSlopeStandardDeviationMap(TestElev,5,TestSlope,false);
        fName := MDTempDir + 'slope_' + DEMGlb[TestElev].AreaName + '.dem';
        DEMGlb[TestSlope].WriteNewFormatDEM(fName);
        fName := MDTempDir + 'ruffslope_' + DEMGlb[TestElev].AreaName + '.dem';
        DEMGlb[TestRuff].WriteNewFormatDEM(fName);

        for I := 1 to 3 do begin
           {$IfDef RecordDEMIXneoFull} WriteLineToDebugFile('Start loop for ' + DiffParams[i]); {$EndIf}
           if (i = 1) then begin
              Ref := RefElev;
              Test := TestElev;
           end
           else if (i = 2) then begin
              Ref := RefSlope;
              Test := TestSlope;
           end
           else if (i = 3) then begin
              Ref := RefRuff;
              Test := TestRuff;
           end;

           ComputeDifferenceDistribution(DiffParams[i],'All',0,Ref,Test);
           if ManyLandCover then begin
             ComputeDifferenceDistribution(DiffParams[i],'Forest',10,Ref,Test);
             {$IfDef DoOnlyThreeLandTypes}
             {$Else}
                ComputeDifferenceDistribution(DiffParams[i],'Shrub',20,Ref,Test);
                ComputeDifferenceDistribution(DiffParams[i],'Grassland',30,Ref,Test);
                ComputeDifferenceDistribution(DiffParams[i],'Urban',50,Ref,Test);
                ComputeDifferenceDistribution(DiffParams[i],'Barren',60,Ref,Test);
             {$EndIf}
           end;
        end;
        CloseSingleDEM(TestElev);
        CloseSingleDEM(TestSlope);
        CloseSingleDEM(TestRuff);
      end;



   var
      Area,aLine,Parameter : shortstring;
      RefElev,RefSlope,RefRuff,TestElev,i,j : integer;
      DEMFiles,DEMNames,TestDEMs : tStringList;
      Table : tMyData;
   begin {procedure MakeGraphDifferentDistributionsAllLandCover}
      {$IfDef RecordDEMIXneo} WriteLineToDebugFile('MakeGraphDifferentDistributionsAllLandCover in');  {$EndIf}
      GetDEMIXpaths;
      SetColorForProcessing;
      Area := 'oxnard';
      DataDir := 'J:\aaa_neo_eval\' + Area + '\';

      GetDOSPath('with DEMs',DataDir);

      Findings := tStringList.Create;
      aLine := 'DEMIX_TILE,AREA,DEM,PARAMETER,LANDTYPE';
      for i := 1 to 10 do aLine := aLine + ',' + LongParamSuffixes[i];
      Findings.Add(aline);

      DEMFiles := Nil;
      FindMatchingFiles(DataDir,'*.tif',DEMfiles);

     fName := DemixSettingsDir + 'demix_dems.dbf';
     Table := tMyData.Create(fName);
     Table.ApplyFilter('USE=' + QuotedStr('Y'));
     TestDEMs := Table.ListUniqueEntriesInDB('SHORT_NAME',false);
     TestDEMs.Add('ref_DTM');
     Table.Destroy;

     DEMnames := tStringList.Create;
     for i := 0 to pred(TestDEMs.Count) do begin
        aline := 'mia';
        for j := 0 to pred(DEMfiles.Count) do begin
            if (StrUtils.AnsiContainsText(Uppercase(DEMfiles.Strings[j]),UpperCase(TestDEMs.Strings[i]))) then begin
               aLine := DEMfiles.Strings[j];
            end;
        end;
        if (aLine <> 'mia') then begin
           DEMnames.add(aLine);
        end
        else begin
           {$IfDef RecordDEMIXneo} WriteLineToDebugFile('Test DEM Missing: ' + TestDEMs.Strings[i]); {$EndIf}
        end;
     end;
     DEMFiles.Destroy;

     {$IfDef RecordDEMIXneo} WriteLineToDebugFile('Setup done'); {$EndIf}
     RefElev := OpenNewDEM(DEMnames[0],false);
     if ValidDEM(RefElev) then begin
        {$IfDef RecordDEMIXneo} WriteLineToDebugFile('Ref DEM: ' + DEMglb[RefElev].AreaName); {$EndIf}
        New(zs);
        ESA_LC10 := LoadLC10LandCover('',DEMGlb[RefElev].DEMBoundBoxGeo,false);

        RefSlope := 0;
        RefRuff := CreateSlopeRoughnessSlopeStandardDeviationMap(RefElev,5,RefSlope,false);
        fName := MDTempDir + 'slope_' + DEMGlb[refElev].AreaName + '.dem';
        DEMGlb[refSlope].WriteNewFormatDEM(fName);
        fName := MDTempDir + 'ruff_' + DEMGlb[refElev].AreaName + '.dem';
        DEMGlb[refRuff].WriteNewFormatDEM(fName);

        for i := 1 to pred(DEMNames.Count) do begin
           {$IfDef RecordDEMIXneo} WriteLineToDebugFile('Test DEM: ' + DEMnames[i]); {$EndIf}
           TestElev := OpenNewDEM(DEMnames[i],false);
           if ValidDEM(TestElev) then begin
              DoOneTestDEM(RefElev,RefSlope,RefRuff,TestElev,false);
           end;
        end;

        Dispose(zs);
        fName := NextFileNumber(DataDir,'difference_dist_by_DEMs_','.dbf');
        StringList2CSVtoDB(Findings,fName);
        CloseSingleDEM(RefElev);
        CloseSingleDEM(RefSlope);
        CloseSingleDEM(RefRuff);
      end;
      SetColorForWaiting;
   end {procedure MakeGraphDifferentDistributionsAllLandCover};


   procedure CorrelationsForFilteredLandCover(What : shortstring; Code : integer; var DesiredList : tDEMBooleanArray);
   var
      i,j,DEM1,DEM2 : integer;
      aLine,Name1 : shortstring;
      NPts : int64;
      r,covar,Mean1,Mean2,StdDev1,StdDev2,MeanDiff,MeanAbsDiff : float64;
   begin
      {$IfDef RecordDEMIXNeo} WriteLineToDebugFile('CorrelationsForFilteredLandCover Start for ' + What); {$EndIf}
      wmdem.SetPanelText(3,What,true);

      if (Code <> 0) then begin
         {$IfDef RecordDEMIXNeoFull} WriteLineToDebugFile('CorrelationsForFilteredLandCover Masking'); {$EndIf}
         DEMGLb[ESA_LC10].MarkOutsideRangeMissing(Code-0.01,Code+0.01,Fixed,false);
         for i := 1 to MaxDEMDataSets do begin
            if ValidDEM(i) and DesiredList[i] then begin
               MaskGridFromSecondGrid(i,ESA_LC10, msSecondMissing);
            end;
         end;
      end;

      if (DEMIXNeoMode = 1) then begin
         GridCorrelationMatrix(gcmR,DEMList, What + ' Pixels Elevation Map',1,DataDir + 'elevation_pixels_correlations_by_landcover.csv');
         GridCorrelationMatrix(gcmR,SlopeList, What + ' Pixels Slope Map',1,DataDir + 'slope_pixels_correlations_by_landcover.csv');
      end;

      if (DEMIXNeoMode in [2,6,7]) then begin
         {$IfDef RecordDEMIXneoFull} WriteLineToDebugFile('DEMIXNeoMode =' + IntToStr(DEMIXNeoMode) + ', ' + DEMName + '  ' + What); {$EndIf}
         for i := 1 to MaxDEMDataSets do begin
            if ValidDEM(i) and DesiredList[i] then begin
               //only looking for a single DEM (with DEMname) to compare to all the others
               aLine := '';
               Name1 := DEMIXshortenDEMName(DEMglb[i].AreaName);  //it might be slope, so go to base name
               if (Name1 = DEMname) then begin
                  aLine := DEMname + ',' + What;
                  DEM1 := i;
                  for j := 1 to MaxDEMDataSets do begin
                     if ValidDEM(j) and DesiredList[j] then begin
                        DEM2 := j;
                        {$IfDef RecordDEMIXneoFull} WriteLineToDebugFile('Analysis 2 compute, ' + IntToStr(DEM1) + ' ' + DEMGlb[DEM1].AreaName + ' ' + IntToStr(DEM2) + ' ' + DEMGlb[DEM2].AreaName); {$EndIf}
                        if (DEM1 = DEM2) then r := 1.0
                        else begin
                           CorrelationTwoGrids(DEMGlb[DEM1].FullDEMGridLimits, DEM1,DEM2);  //,NPts, r,covar,Mean1,Mean2,StdDev1,StdDev2,MeanDiff,MeanAbsDiff);
                           if (r < -99) then begin
                              //so we can put a breakpoint here
                              {$IfDef RecordDEMIXneo} WriteLineToDebugFile('Analysis 2 problem, ' + IntToStr(DEM1) + ' ' + DEMGlb[DEM1].AreaName + ' ' + IntToStr(DEM2) + ' ' + DEMGlb[DEM2].AreaName); {$EndIf}
                              CorrelationTwoGrids(DEMGlb[DEM1].FullDEMGridLimits, DEM1,DEM2);  //,NPts, r,covar,Mean1,Mean2,StdDev1,StdDev2,MeanDiff,MeanAbsDiff);
                           end
                        end;
                        aLine := aLine + ',' + RealToString(r,12,-8);
                     end;
                  end;
                  Findings.Add(aLine);
                  {$IfDef RecordDEMIXneoFull} WriteLineToDebugFile(aLine); {$EndIf}
               end;
            end;
         end;
     end;

      if (Code <> 0) then begin
         {$IfDef RecordDEMIXNeoFull} WriteLineToDebugFile('CorrelationsForFilteredLandCover Unmasking'); {$EndIf}
         for i := 1 to MaxDEMDataSets do begin
            if ValidDEM(i) and DesiredList[i] then begin
               DEMGlb[i].ReloadDEM(true);  //reload grid so next filter can run
            end;
         end;
         DEMGlb[ESA_LC10].ReloadDEM(true);
      end;
      {$IfDef RecordDEMIXNeoFull} WriteLineToDebugFile('CorrelationsForFilteredLandCover end for ' + What); {$EndIf}
   end {procedure CorrelationsForFilteredLandCover};


   procedure CycleThroughLandCover(var DesiredList : tDEMBooleanArray);
   begin
      {$IfDef RecordDEMIXNeo} WriteLineToDebugFile('CycleThroughLandCover in'); {$EndIf}
      CorrelationsForFilteredLandCover('All',0,DesiredList);
      if DEMIXneoMode in [1,2] then begin
         {$IfDef DoOnlyThreeLandTypes}
            CorrelationsForFilteredLandCover('Barren',60,DesiredList);
            CorrelationsForFilteredLandCover('Forest',10,DesiredList);
         {$Else}
            CorrelationsForFilteredLandCover('Barren',60,DesiredList);
            CorrelationsForFilteredLandCover('Grassland',30,DesiredList);
            CorrelationsForFilteredLandCover('Shrub',20,DesiredList);
            CorrelationsForFilteredLandCover('Forest',10,DesiredList);
            CorrelationsForFilteredLandCover('Urban',50,DesiredList);
         {$EndIf}
      end;
      {$IfDef RecordDEMIXNeo} WriteLineToDebugFile('CycleThroughLandCover out'); {$EndIf}
   end;


   procedure MakeCorrelationMatrixJustOneDEMAllLandCover;

            procedure CorrelateOneDEM(dName : PathStr; var theList : tDEMBooleanArray);
            begin
               {$IfDef RecordDEMIXneo} WriteLineToDebugFile('CorrelateOneDEM=' + dName); {$EndIf}
               DEMName := dName;
               wmdem.SetPanelText(2,DEMname,true);
               CycleThroughLandCover(theList);
            end;

         function DoOneParameter(ItsName : shortstring; var theList : tDEMBooleanArray) : tGridForm;
         var
            aLine : shortstring;
            i : integer;
         begin
            {$IfDef RecordDEMIXneo} WriteLineToDebugFile('DoOneParameter=' + ItsName); {$EndIf}
            Findings := tStringList.Create;
            aLine := 'DEM/Grid,LandCover';
            for i := 1 to MaxDEMDataSets do begin
               if ValidDEM(i) and DEMList[i] then begin
                  aLine := aLine + ',' + DEMIXShortenDEMName(DEMGlb[i].AreaName);
               end;
            end;
            Findings.Add(aline);
            CorrelateOneDEM('Native_5m_DSM',theList);
            CorrelateOneDEM('Native_5m_DTM',theList);
            if (DEMIXneoMode = 7) then fName := '_one_dem_correlation_multiple_landcovers'
            else if (DEMIXneoMode = 1) then fName := '_one_dem_correlation_only_all_landcover'
            else fName := ItsName + '_one_dem_correlation_ALL_merged_landcover_mode_' + IntToStr(DEMIXneoMode);
            fName := DataDir + ItsName + fName + '.csv';
            if (Findings.Count > 1) then begin
               Findings.SaveToFile(fName);
               Result := OpenCorrelationMatrix(ItsName + ' correlations',fName);
               Result.BitBtn6Click(Nil);
            end
            else begin
               Findings.Destroy;
               {$IfDef RecordDEMIXneo} HighLightLineToDebugFile('Fail DoOneParameter=' + ItsName); {$EndIf}
            end;
         end;

   begin {procedure MakeCorrelationMatrixJustOneDEMAllLandCover}
      OpenElevationAndSlopeGrids;
      DoOneParameter('Elevation',DEMList);
      DoOneParameter('Slope',SlopeList);
   end {procedure MakeCorrelationMatrixJustOneDEMAllLandCover};


begin {procedure MakeGraphDifferentDistributionsAllLandCover}
   {$IfDef RecordDEMIXneo} HighLightLineToDebugFile('Start CrossScaleDEMComparison, Mode=' + IntToStr(DEMIXNeoMode)); {$EndIf}
   DEMIXanalysismode := DEMIXneo;
   MDDef.DEMIX_mode := dmFull;

(*
   MDDef.SSIM_elev := true;
   MDDef.SSIM_slope := true;
   MDDef.SSIM_ruff := true;
   MDDef.SSIM_rri := true;
   MDDef.SSIM_hill := true;
   MDDef.SSIM_tpi := true;
   MDDef.SSIM_Openness := false;
   MDDef.SSIM_PLANC := true;
   MDDef.SSIM_PROFC := true;
   MDDef.SSIM_TANGC := true;

   MDDef.SSIM_Rotor := false;
   MDDef.SSIM_ConvergeIndex := false;
   MDDef.SSIM_flow := false;
   MDDef.SSIM_LS := false;
   MDDef.SSIM_Wet := false;
   MDDef.SSIM_HAND := false;
*)

   MDDef.DoSSIM := false;
   MDDef.DoFUV := true;

   LockStatusBar := true;
   SetColorForProcessing;

   if DEMIXNeoMode in [1,2,6,7] then MakeCorrelationMatrixJustOneDEMAllLandCover;
   if DEMIXNeoMode in [3] then MakeGraphDifferenceDistributionsAllLandCover;

   if DEMIXNeoMode in [4,5] then begin
      fName := 'J:\aaa_neo_eval\neo_test_tiles.dbf';
      OpenNumberedGISDataBase(NewFormatDEMIXDB,fName,true);
      GetDEMIXpaths;
      DEMIX_initialized := true;
      SetParamsForDEMIXmode;

      Areas := tStringList.Create;
      GISdb[NewFormatDEMIXDB].ApplyGISFilter('DTM<>' + QuotedStr(''));
      Areas := GISdb[NewFormatDEMIXDB].MyData.ListUniqueEntriesInDB('AREA');
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Neo Areas with data=' + IntToStr(Areas.Count)); {$EndIf}

      if DEMIXNeoMode in [4] then begin
         //this option is more hard-wired different from the traditional mode
         HighlightLineToDebugFile('Start Difference distributions');
         ComputeDEMIX_Diff_Dist_tile_stats(Overwrite,Areas);
         GISdb[NewFormatDEMIXDB].ClearGISFilter;
      end;

      if DEMIXNeoMode in [5] then begin
         MDDef.DoSSIM := false;
         MDDef.DoFUV := true;
         //this option should be les hard-wired new code
         HighlightLineToDebugFile('Start FUV distributions');
         AreaSSIMandFUVComputations(Overwrite,false,Areas);
         GISdb[NewFormatDEMIXDB].ClearGISFilter;
      end;
   end;

   SetColorForWaiting;
   LockStatusBar := false;
   wmDEM.ClearStatusBarPanelText;

   {$IfDef RecordDEMIXneo} WriteLineToDebugFile('End CrossScaleDEMComparison'); {$EndIf}
end {procedure MakeGraphDifferentDistributionsAllLandCover};



procedure StartAreaEvals;
var
   New_area_evals_form : TNew_area_evals_form;
begin
   New_area_evals_form := TNew_area_evals_form.Create(Application);
   New_area_evals_form.AreaName := '';
   New_area_evals_form.Show;
end;


procedure TNew_area_evals_form.BitBtn1Click(Sender: TObject);
var
   i : integer;
begin
   if (RadioGroup1.ItemIndex = 0) then begin
      for i := 1 to NumPtDEMs do begin
          GridScatterGram(DEMglb[PointDEMs[0]].FullDEMGridLimits,PointDEMs[i],PointDEMs[0] );
      end;
   end
   else begin
      for i := 1 to NumPtDEMs do begin
          GridScatterGram(DEMglb[PointGrids[0]].FullDEMGridLimits,PointGrids[i],PointGrids[0] );
          if RadioGroup1.ItemIndex = 3 then GridScatterGram(DEMglb[PointGrids2[0]].FullDEMGridLimits,PointGrids2[i],PointGrids2[0] );
      end;
   end;
end;



procedure TNew_area_evals_form.BitBtn2Click(Sender: TObject);
var
   DEMList : tDEMBooleanArray;
   i : integer;
begin
   InitializeDEMsWanted(DEMList,false);
   if RadioGroup1.ItemIndex = 0 then begin
      for i := 0 to NumPtDEMs do begin
         DEMList[PointDEMs[i]] := true;
      end;
   end
   else begin
       for i := 0 to NumPtDEMs do begin
         DEMList[PointGrids[i]] := true;
      end;
   end;
   CreateGridHistograms(DEMList);
end;


procedure TNew_area_evals_form.BitBtn3Click(Sender: TObject);
var
   i : integer;
begin
   MDDef.HighlightDiffMap := 2;
   if (RadioGroup1.ItemIndex = 0) then begin
      for i := 1 to NumPtDEMs do begin
          MakeDifferenceMap(PointDEMs[i],PointDEMs[0],PointDEMs[0],0,true,false,false);
      end;
   end
   else begin
      for i := 1 to NumPtDEMs do begin
          MakeDifferenceMap(PointGrids[i],PointGrids[0],PointGrids[0],0,true,false,false);
          if RadioGroup1.ItemIndex = 3 then MakeDifferenceMap(PointGrids2[i],PointGrids2[0],PointGrids2[0],0,true,false,false);;
      end;
   end;
end;

procedure TNew_area_evals_form.RadioGroup1Click(Sender: TObject);
begin
   {$IfDef RecordDEMIXneo} WriteLineToDebugFile('TNew_area_evals_form.RadioGroup1Click=' + IntToStr(RadioGroup1.ItemIndex) + '  ' + AreaName); {$EndIf}
   if (RadioGroup1.ItemIndex <> 0) then ClearDerivedGrids;
   case RadioGroup1.ItemIndex of
       //0 : CreateDEMIXSlopeRoughnessGrids(AreaName,true,true);
       1 : CreateDEMIXSlopeRoughnessGrids(AreaName,true,true);
       2 : CreateDEMIXDerivedGrids('HILL_',AreaName,true,false);
       3 : CreateDEMIXDerivedGrids('RRI_',AreaName,true,false);
       4 : CreateDEMIXOpennessGrids(AreaName,true,false);
   end;
end;



end.
