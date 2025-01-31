unit demix_neo_test_area;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$Define RecordDEMIXneo}
{$EndIf}



interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
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
  procedure CrossScaleDEMComparison;


var
  New_area_evals_form: TNew_area_evals_form;

implementation

{$R *.dfm}

uses
   demix_definitions,demix_control,PetMath,PetdbUtils,
   dem_Manager,
   Make_grid,
   nevadia_main,
   DEMdataBase,
   dem_nlcd,
   demdefs,demstat,DEMcoord;


procedure CrossScaleDEMComparison;
//this is hard coded during active development for the current options
const
   Overwrite = true;
   DoDiffDist = true;
var
   fName,DataDir : PathStr;
   Areas,TheDEMs : tStringList;
   DEMList,SlopeList : tDEMBooleanArray;
   i,aDEM,aSlope,ESA_LC10 : integer;
   Fixed : int64;

   procedure CorrelationsForFilteredLandCover(Analysis : integer; What : shortstring; Code : integer; DesiredList : tDEMBooleanArray);
   var
      i : integer;
   begin
      {$IfDef RecordDEMIXNeo} WriteLineToDebugFile('Start correlations for ' + What); {$EndIf}
      DEMGLb[ESA_LC10].MarkOutsideRangeMissing(Code-0.01,Code+0.01,Fixed,false);
      for i := 1 to MaxDEMDataSets do begin
         if ValidDEM(i) and DesiredList[i] then begin
            DEMGlb[i].SelectionMap.MaskFromSecondGrid(ESA_LC10, msSecondMissing);
         end;
      end;
      if Analysis = 1 then GridCorrelationMatrix(gcmR,SlopeList, What + ' Pixels Slope Map');
      for i := 1 to MaxDEMDataSets do begin
         if ValidDEM(i) and DesiredList[i] then begin
            DEMGlb[i].ReloadDEM(true);  //reload grid so next filter can run
         end;
      end;
      DEMGlb[ESA_LC10].ReloadDEM(true);
   end;

   procedure MakeCorrelationMatrixForAllDEMsAllLandCover;
   var
      i : integer;
   begin
      DataDir := 'J:\aaa_neo_eval\oxnard\multiple_0.15sec\';
      InitializeDEMsWanted(DEMList,false);
      InitializeDEMsWanted(SlopeList,false);
      TheDEMs := Nil;
      FindMatchingFiles(DataDir,'*.tif',TheDEMs);
      for i := 0 to pred(TheDEMs.Count) do begin
         aDEM := OpenNewDEM(TheDEMs.Strings[i]);
         DEMList[aDEM] := true;
         aSlope := CreateSlopeMapPercent(true, aDEM);
         fName := MDTempDir + DEMGlb[aSlope].AreaName + '.dem';
         DEMGlb[aSlope].WriteNewFormatDEM(fName);
         SlopeList[aSlope] := true;
      end;
      {$IfDef RecordDEMIXneo} WriteLineToDebugFile('Loadeds DEMs and created slope maps, n=' + IntToStr(TheDEMs.Count)); {$EndIf}

      GridCorrelationMatrix(gcmR,SlopeList,'All Pixels Slope Map');

      ESA_LC10 := LoadLC10LandCover('',DEMGlb[1].DEMBoundBoxGeo,true);
      CorrelationsForFilteredLandCover(1,'Forest',10,SlopeList);
      CorrelationsForFilteredLandCover(1,'Shrub',20,SlopeList);
      CorrelationsForFilteredLandCover(1,'Grassland',30,SlopeList);
      CorrelationsForFilteredLandCover(1,'Urban',50,SlopeList);
      CorrelationsForFilteredLandCover(1,'Barren',60,SlopeList);
   end;



   procedure MakeGraphDifferentDistributionsAllLandCover;
   var
      i : integer;
      MomentVar : tMomentVar;
      zs : ^bfArray32;
      Findings : tstringList;




      procedure ComputeDifferenceDistribution(Parameter,LandType : shortstring; Code,Ref,Test : integer);

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
      begin
         {$IfDef RecordDEMIXneo} WriteLineToDebugFile('Start landcover for ' + LandType + ' ref=' + IntToStr(Ref) + ' test=' + IntToStr(Test)); {$EndIf}

         wmdem.SetPanelText(2,Parameter);
         wmdem.SetPanelText(3,LandType);

         if (Code <> 0) then begin
            DEMGLb[ESA_LC10].MarkOutsideRangeMissing(Code-0.01,Code+0.01,Fixed,false);
            MaskGridFromSecondGrid(Ref,ESA_LC10, msSecondMissing);
            MaskGridFromSecondGrid(Test,ESA_LC10, msSecondMissing);
         end;

         DoParameter(Test,Ref);
         aline := 'Test area, test area,' + Parameter + ',' + LandType + ',' + DEMIXMomentStatsString(MomentVar);
         Findings.Add(aLine);

         if (Code <> 0) then begin
            DEMGLb[ESA_LC10].ReloadDEM(true);
            DEMGlb[Ref].ReloadDEM(true);
            DEMGlb[Test].ReloadDEM(true);
         end;
      end;


   var
      aLine,Parameter : shortstring;
      RefElev,TestElev,Ref,Test,
      RefSlope,TestSlope,
      TestRuff,RefRuff : integer;
   begin
      New(zs);
      DataDir := 'J:\aaa_neo_eval\oxnard\multiple_0.15sec\';
      Findings := tStringList.Create;
      aLine := 'DEMIX_TILE,AREA,PARAMETER,LANDTYPE';
      for i := 1 to 10 do aLine := aLine + ',' + LongParamSuffixes[i];
      Findings.Add(aline);

      RefElev := OpenNewDEM(DataDir + 'ref_dtm_0.15sec.tif');
      TestElev := OpenNewDEM(DataDir + 'NeoDTM_0.15sec.tif');
      ESA_LC10 := LoadLC10LandCover('',DEMGlb[RefElev].DEMBoundBoxGeo,true);
      RefSlope := 0;
      RefRuff := CreateSlopeRoughnessSlopeStandardDeviationMap(RefElev,5,RefSlope,true);
      fName := MDTempDir + 'slope_' + DEMGlb[refElev].AreaName + '.dem';
      DEMGlb[refSlope].WriteNewFormatDEM(fName);
      fName := MDTempDir + 'ruff_' + DEMGlb[refElev].AreaName + '.dem';
      DEMGlb[refRuff].WriteNewFormatDEM(fName);

      TestSlope := 0;
      TestRuff := CreateSlopeRoughnessSlopeStandardDeviationMap(TestElev,5,TestSlope,true);
      fName := MDTempDir + 'slope_' + DEMGlb[TestElev].AreaName + '.dem';
      DEMGlb[TestSlope].WriteNewFormatDEM(fName);
      fName := MDTempDir + 'ruffslope_' + DEMGlb[TestElev].AreaName + '.dem';
      DEMGlb[TestRuff].WriteNewFormatDEM(fName);



      for I := 1 to 3 do begin
         {$IfDef RecordDEMIXneo} WriteLineToDebugFile('Start loop for ' + DiffParams[i]); {$EndIf}
         if i = 1 then begin
            Ref := RefElev;
            Test := TestElev;
         end
         else if i = 2 then begin
            Ref := RefSlope;
            Test := TestSlope;
         end
         else if i = 3 then begin
            Ref := RefRuff;
            Test := TestRuff;
         end;

         ComputeDifferenceDistribution(DiffParams[i],'All',0,Ref,Test);
         ComputeDifferenceDistribution(DiffParams[i],'Forest',10,Ref,Test);
         ComputeDifferenceDistribution(DiffParams[i],'Shrub',20,Ref,Test);
         ComputeDifferenceDistribution(DiffParams[i],'Grassland',30,Ref,Test);
         ComputeDifferenceDistribution(DiffParams[i],'Urban',50,Ref,Test);
         ComputeDifferenceDistribution(DiffParams[i],'Barren',60,Ref,Test);
      end;

      Dispose(zs);
      fName := NextFileNumber(MDTempDir,'difference_dist_by_landcover_','.dbf');
      StringList2CSVtoDB(Findings,fName);
   end;



begin
   {$IfDef RecordDEMIXneo} WriteLineToDebugFile('Start CrossScaleDEMComparison'); {$EndIf}

   DEMIXanalysismode := DEMIXneo;
   MDDef.DEMIX_mode := dmFull;

   MDDef.SSIM_elev := true;
   MDDef.SSIM_slope := true;
   MDDef.SSIM_ruff := true;
   MDDef.SSIM_rri := true;
   MDDef.SSIM_hill := true;
   MDDef.SSIM_tpi := true;
   MDDef.SSIM_Openness := false;

   MDDef.SSIM_Rotor := false;
   MDDef.SSIM_ConvergeIndex := false;
   MDDef.SSIM_flow := false;
   MDDef.SSIM_LS := false;
   MDDef.SSIM_Wet := false;
   MDDef.SSIM_HAND := false;
   MDDef.SSIM_PLANC := false;
   MDDef.SSIM_PROFC := false;
   MDDef.SSIM_TANGC := false;

   MDDef.DoSSIM := false;
   MDDef.DoFUV := true;

   //MakeGraphDifferentDistributionsAllLandCover;


   MakeCorrelationMatrixForAllDEMsAllLandCover;


(*

   fName := 'J:\aaa_neo_eval\neo_test_tiles.dbf';
   OpenNumberedGISDataBase(NewFormatDEMIXDB,fName,true);
   GetDEMIXpaths;
   DEMIX_initialized := true;
   SetParamsForDEMIXmode;

   Areas := tStringList.Create;
   GISdb[NewFormatDEMIXDB].ApplyGISFilter('DTM<>' + QuotedStr(''));
   Areas := GISdb[NewFormatDEMIXDB].MyData.ListUniqueEntriesInDB('AREA');
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Neo Areas with data=' + IntToStr(Areas.Count)); {$EndIf}

   if DoDiffDist then begin
      //this option is more hard-wired different from the traditional mode
      HighlightLineToDebugFile('Start Difference distributions');
      ComputeDEMIX_Diff_Dist_tile_stats(Overwrite,Areas);
      GISdb[NewFormatDEMIXDB].ClearGISFilter;
   end;

   if MDDef.DoSSIM or MDDef.DoFUV then begin
      //this option should be les hard-wired new code
      HighlightLineToDebugFile('Start FUV distributions');
      AreaSSIMandFUVComputations(Overwrite,false,Areas);
      GISdb[NewFormatDEMIXDB].ClearGISFilter;
   end;
*)
   SetColorForWaiting;
   {$IfDef RecordDEMIXneo} WriteLineToDebugFile('End CrossScaleDEMComparison'); {$EndIf}
end;





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
          //MakeChangeMap(PointDEMs[i],PointDEMs[0],DEMglb[PointDEMs[0]].FullDEMGridLimits);
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
   if (RadioGroup1.ItemIndex <> 0) then ClearDerivedGrids;
   case RadioGroup1.ItemIndex of
       1 : CreateDEMIXSlopeRoughnessGrids(AreaName,true,true);
       2 : CreateDEMIXhillshadeGrids(AreaName,true,true);
       3 : CreateDEMIXSlopeRoughnessGrids(AreaName,true,true);
       4 : CreateDEMIXRRIgrids(AreaName,true,true);
   end;
end;



end.
