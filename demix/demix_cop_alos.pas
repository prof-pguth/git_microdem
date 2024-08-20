unit demix_cop_alos;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


//9/15/2023   this is experimental, and has not been tested recently


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordDEMIX}
   //{$Define RecordDEMIXLoad}
   //{$Define RecordDEMIXsave}
   //{$Define RecordCreateHalfSec}
   //{$Define StartCOPALOSmaps}
   //{$Define Differencemaps}
   //{$Define RecordDEMIXMovies}
   //{$Define RecordFullDEMIX}
   //{$Define ShowDEMIXWhatsOpen}
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

    System.SysUtils,System.Classes,System.Diagnostics,StrUtils,WinAPI.Windows,
    VCL.ExtCtrls,VCL.Forms, VCL.Graphics,
    Petmar,Petmar_types,BaseGraf;

const
   caBest = 1;
   ca4cat = 2;
   ca9cat = 3;

//map comparisons
   procedure CopAlosCompareReference;
   procedure PixelByPixelCopAlos;
   procedure COP_ALOS_compare(What : integer);
   procedure HighLowCopAlosGeomorphometry(fName : PathStr = '');
   procedure CreateDifferenceMaps(DEMIXhalfSecDir : PathStr = '');

   procedure OpenHalfSecCopALOS(OpenDir : boolean; InDir : PathStr = '');
   procedure CreateHalfSecSlopeRuffMaps(RefDTMHalfSec,ALOSHalfSec,COPHalfSec : integer);
   procedure BestMapCOPALOSThreeParams;
   procedure HighLowMapCOPALOSThreeParams;


implementation

uses
   Nevadia_Main,
   demix_definitions, DEMIX_control,
   DEMstat,Make_grid,PetImage,PetImage_form,new_petmar_movie,DEMdatabase,PetDButils,Pick_several_dems,
   DEMCoord,DEMdefs,DEMMapf,DEMDef_routines,DEM_Manager,DEM_indexes,PetMath;

const
   GeomorphNames : array[1..3] of shortstring = ('Elevation','Slope','Roughness');

 const
   ALOSHalfSecfName : PathStr = 'alos.tif';
   CopHalfSecfName : PathStr  = 'cop.tif';
   HalfSecRefDTMfName : PathStr = 'ref_dtm.dem';
   HalfSecRefDSMfName = 'ref_dsm.tif';

   SlopeMapfName = 'slope_ref_dtm.tif';
   RuffMapfName = 'roughness_ref_dtm.tif';
   AspectMapfName = 'aspect_ref_dtm.tif';

   COP_ALOS_DifffName = 'cop-alos-diff.tif';
   COP_ALOS_DSM4fName = 'cop-alos-dsm4.tif';
   COP_ALOS_DTM4fName = 'cop-alos-dtm4.tif';
   COP_ALOS_DSM9fName = 'cop-alos-dsm9.tif';
   COP_ALOS_DTM9fName = 'cop-alos-dtm9.tif';
   BestCOP_ALOS_slopefName = 'best_slope_ref_dtm.tif';
   BestCOP_ALOS_rufffName = 'best_roughness_ref_dtm.tif';
   BestCOP_ALOS_elevfName = 'best_elev_ref_dtm.tif';

   DTMElevDiffMapALOSfName = 'elev_diff_dtm_alos.tif';
   DTMElevDiffMapCOPfName = 'elev_diff_dtm_cop.tif';
   DTMSlopeDiffMapALOSfName = 'slope_diff_dtm_alos.tif';
   DTMSlopeDiffMapCOPfName = 'slope_diff_dtm_cop.tif';
   DTMRuffDiffMapALOSfName = 'ruff_diff_dtm_alos.tif';
   DTMRuffDiffMapCOPfName = 'ruff_diff_dtm_cop.tif';

var
   ThisArea : PathStr;
   ALOSHalfSec,COPHalfSec,RefDTMhalfsec,
   RuffALOSHalfSec,RuffCOPHalfSec,RuffRefDTMPointhalfsec : integer;
   RuffRefDTMarea,SlopeRefDTMarea,
   RuffRefDTMPoint,SlopeRefDTMPoint : integer;

const
   SlopeALOSHalfSec : integer = 0;
   SlopeCOPHalfSec : integer = 0;
   SlopeRefDTMPointhalfsec : integer = 0;
   RefAspectMapHalfSec : integer = 0;

type
  t3DEM = array[1..3] of integer;


function OpenHalfSec(fName : PathStr; What : shortstring) : integer;
begin
   if not FileExists(fName) then fName := ChangeFileExt(fName,'.tif');
   Result := OpenNewDEM(fName,true,what);
end;


procedure CreateDifferenceMaps(DEMIXhalfSecDir : PathStr = '');
var
   i,ref,RefElev : integer;
   MapList : array[1..3] of tStringList;
   RuffTestDEM,SlopeTestDEM,ElevDiff,SlopeDiff,RuffDiff : array[1..6] of integer;
begin
   {$If Defined(Differencemaps)} WriteLineToDebugFile('CreateDifferenceMaps started'); {$EndIf}
   NakedMapOptions;   //includesSaveBackupDefaults;
   CleanUpTempDirectory;
   MDdef.DefaultMapXSize := 1150;
   MDdef.DefaultMapYSize := 1300;
   MDDef.ScaleBarLocation.DrawItem := true;
   MDDef.ShowRoamOnAllMaps := false;
   MDDef.DefaultGraphBackgroundColor := RGBtrip(233,233,233);

   if (DEMIXhalfSecDir = '') then GetDOSPath('Test files',DEMIXhalfSecDir);

   Stopwatch := TStopwatch.StartNew;

   RefDTMpoint := OpenHalfSec(DEMIXhalfSecDir + Ref1SecPointStr + '.dem','ref dem');
   RefDTMarea := OpenHalfSec(DEMIXhalfSecDir + Ref1SecAreaStr + '.dem','ref dem');

   for I := 1 to NumDEMIXtestDEM do begin
      TestDEMs[i] := OpenHalfSec(DEMIXhalfSecDir + DEMIXDEMTypeName[i] + '.dem',DEMIXDEMTypeName[i]);
      TestSeries[i] := DEMIXDEMTypeName[i];
   end;

   RuffRefDTMPoint := CreateSlopeRoughnessSlopeStandardDeviationMap(RefDTMpoint,3,SlopeRefDTMPoint );
   RuffRefDTMArea := CreateSlopeRoughnessSlopeStandardDeviationMap(RefDTMarea,3,SlopeRefDTMarea );

   for i := 1 to 3 do MapList[i] := tStringList.Create;

   MDDef.GridLegendLocation.DrawItem := true;
   for I := 1 to 6 do begin
      RuffTestDEM[i] := CreateSlopeRoughnessSlopeStandardDeviationMap(TestDEMs[i],3,SlopeTestDEM[i]);
   end;

   SetMapsForBigBitmaps(false);
   BigImagewithallmaps(3,ImageDir + ThisArea + '_cop_or_alos_better.png');
   for I := 1 to 6 do begin
      if TestSeries[i] = 'ALOS' then RefElev := RefDTMarea else RefElev := RefDTMpoint;
      ElevDiff[i] := MakeDifferenceMap(RefElev,TestDEMs[i],RefElev,RefElev,true,false,false);
      DEMGlb[ElevDiff[i]].AreaName := 'Elevation_difference,_' + TestSeries[i] + '_-_' + 'Reference_DTM';
      //DEMGlb[ElevDiff[i]].SelectionMap.MergeAnotherDEMreflectance(RefElev);
   end;
   BigImagewithallmaps(3,ImageDir + ThisArea + '_Elevation_compare_reference.png');


   SetMapsForBigBitmaps(false);
   BigImagewithallmaps(3,ImageDir + ThisArea + '_cop_or_alos_better.png');
   for I := 1 to 6 do begin
      if TestSeries[i] = 'ALOS' then RefElev := RefDTMarea else RefElev := RefDTMpoint;
      if TestSeries[i] = 'ALOS' then Ref := SlopeRefDTMarea else Ref := SlopeRefDTMpoint;
      SlopeDiff[i] := MakeDifferenceMap(Ref,SlopeTestDEM[i],Ref,Ref,true,false,false);
      DEMGlb[SlopeDiff[i]].AreaName := 'Slope_difference,_' + TestSeries[i] + '_-_' + 'Reference_DTM';
      //DEMGlb[SlopeDiff[i]].SelectionMap.MergeAnotherDEMreflectance(RefElev);
   end;
   BigImagewithallmaps(3,ImageDir + ThisArea + '_Slope_compare_reference.png');

   SetMapsForBigBitmaps(false);
   BigImagewithallmaps(3,ImageDir + ThisArea + '_cop_or_alos_better.png');
   for I := 1 to 6 do begin
      if TestSeries[i] = 'ALOS' then RefElev := RefDTMarea else RefElev := RefDTMpoint;
      if TestSeries[i] = 'ALOS' then Ref := RuffRefDTMarea else Ref := RuffRefDTMpoint;
      RuffDiff[i] := MakeDifferenceMap(Ref,RuffTestDEM[i],Ref,Ref,true,false,false);
      DEMGlb[RuffDiff[i]].AreaName := 'Roughness_difference,_' + TestSeries[i] + '_-_' + 'Reference_DTM';
      //DEMGlb[RuffDiff[i]].SelectionMap.MergeAnotherDEMreflectance(RefElev);
   end;
   BigImagewithallmaps(3,ImageDir + ThisArea + '_Roughness_compare_reference.png');

   RestoreBackupDefaults;
   ResetGraphVariables;
   {$If Defined(Differencemaps)} WriteLineToDebugFile('CreateDifferenceMaps out ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4)); {$EndIf}

end;


procedure OpenHalfSecCopALOS(OpenDir : boolean; InDir : PathStr = '');
//all tests used external SSD hard drive
//   16.7633 sec on Dell Inspiron  Intel64 Family 6 Model 165 Stepping 5  CPU Speed: 2904 MHz
//   17.7185 sec on Surface Laptop  Intel64 Family 6 Model 140 Stepping 1  CPU Speed: 3302 MHz
//   48.1982 sec on Dell T5610 Precision workstation Intel64 Family 6 Model 62 Stepping 4   CPU Speed: 2494 MHz
//   60.1726 sec on HP laptop    AMD64 Family 23 Model 24 Stepping 1    CPU Speed: 2296 MHz
var
   DEM4 : integer;
begin
   NakedMapOptions;   //includesSaveBackupDefaults;
   CleanUpTempDirectory;
   MDdef.DefaultMapXSize := 1150;
   MDdef.DefaultMapYSize := 1300;
   MDDef.ScaleBarLocation.DrawItem := true;
   MDDef.DefaultGraphBackgroundColor := RGBtrip(233,233,233);

   if OpenDir then begin
      if (InDir <> '') then DEMIXhalfSecDir := InDir
      else GetDOSPath('Half sec test files',DEMIXhalfSecDir);
      ThisArea := LastSubDir(ExtractFilePath(DEMIXhalfSecDir));
      {$If Defined(StartCOPALOSmaps)} WriteLineToDebugFile('OpenHalfSecCopALOS start'); {$EndIf}

      Stopwatch := TStopwatch.StartNew;

      RefDTMhalfsec := OpenHalfSec(DEMIXhalfSecDir + HalfSecRefDTMfName,'ref dem');
      ALOSHalfSec := OpenHalfSec(DEMIXhalfSecDir + 'ALOS_0.5sec_bilinear.dem','alos');
      COPHalfSec := OpenHalfSec(DEMIXhalfSecDir + 'COP_0.5sec_bilinear.dem','cop');
      {$If Defined(StartCOPALOSmaps)} WriteLineToDebugFile('DEMs opened'); {$EndIf}
   end
   else begin
      PickSeveralExistingDEMs('Reference DEM','ALOS','COPDEM','',RefDTMhalfsec,ALOSHalfSec,COPHalfSec,DEM4);
   end;

   CreateHalfSecSlopeRuffMaps(RefDTMHalfSec,ALOSHalfSec,COPHalfSec);
   RefAspectMapHalfSec := MakeAspectMap(RefDTMHalfSec);
   MDDef.GridLegendLocation.DrawItem := true;

   BestMapCOPALOSThreeParams;
   HighLowMapCOPALOSThreeParams;

   {$If Defined(StartCOPALOSmaps)} if OpenDir then WriteLineToDebugFile(RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' OpenHalfSecCopALOS'); {$EndIf}
   RestoreBackupDefaults;
   ResetGraphVariables;
end;


procedure CreateHalfSecSlopeRuffMaps(RefDTMHalfSec,ALOSHalfSec,COPHalfSec : integer);
begin
   RuffRefDTMPointhalfsec := CreateSlopeRoughnessSlopeStandardDeviationMap(RefDTMHalfSec,3,SlopeRefDTMPointhalfsec);
   RuffALOSHalfSec := CreateSlopeRoughnessSlopeStandardDeviationMap(ALOSHalfSec,3,SlopeALOSHalfSec);
   RuffCOPHalfSec := CreateSlopeRoughnessSlopeStandardDeviationMap(COPHalfSec,3,SlopeCOPHalfSec);
   {$If Defined(StartCOPALOSmaps)} WriteLineToDebugFile('Slope/roughness maps opened'); {$EndIf}
end;


procedure SetLLtext(i : integer);
begin
   if (i in [1..3]) then DefGraphLLText := GeomorphNames[i]
   else DefGraphLLText := '';
end;


procedure MakeHistograms(DEM : t3DEM);
var
   i,j : integer;
   fName : PathStr;
   Bitmap : tMyBitmap;
   Graph : array[1..4] of tThisBaseGraph;
   GraphList : tStringList;
begin
   GraphList := tStringList.Create;
   for i := 1 to 3 do begin
      SetLLtext(i);
      HistogramsFromVATDEM(DEM[i],RefDTMhalfsec,SlopeRefDTMPointhalfsec,SlopeRefDTMPointhalfsec,RefAspectMapHalfSec,Graph[1],Graph[2],Graph[3],Graph[4]);
      for j := 1 to 4 do begin
         CopyImageToBitmap(Graph[j].Image1,Bitmap);
         Bitmap.Height := Bitmap.Height + 45;
         fName := NextFileNumber(MDtempDir,'graph_4_biggie_','.bmp');
         Bitmap.SaveToFile(fName);
         Bitmap.Free;
         GraphList.Add(fName);
      end;
      fName := NextFileNumber(MDtempDir,'graph_4_biggie_','.bmp');
      Bitmap := Graph[1].MakeLegend(Graph[1].GraphDraw.LegendList,false);
      Bitmap.SaveToFile(fName);
      Bitmap.Free;
      GraphList.Add(fName);
   end;
   MakeBigBitmap(GraphList,ThisArea,'',5);
end;


procedure BestMapCOPALOSThreeParams;
var
   DEM : t3DEM;
   i : integer;
begin
   {$If Defined(StartCOPALOSmaps)} WriteLineToDebugFile('BestMapCOPALOSThreeParams enter'); {$EndIf}
   SetMapsForBigBitmaps(false);
   DEM[1] := BestCopOrALOSmap(RefDTMHalfSec,ALOSHalfSec,COPHalfSec,MDDef.DEMIXSimpleTolerance,ThisArea + '_ALOS-COP-better-elevation');
   DEM[2] := BestCopOrALOSmap(SlopeRefDTMPointhalfsec,SlopeALOSHalfSec,SlopeCOPHalfSec,MDDef.DEMIXSlopeTolerance,ThisArea + '_ALOS-COP-better-slope');
   DEM[3] := BestCopOrALOSmap(RuffRefDTMPointhalfsec,RuffALOSHalfSec,RuffCOPHalfSec,MDDef.DEMIXRuffTolerance,ThisArea + '_ALOS-COP-better-roughness');
   {$If Defined(StartCOPALOSmaps)} WriteLineToDebugFile('BestMapCOPALOSThreeParams, 3 maps done'); {$EndIf}
   for i := 1 to 3 do begin
      DEMGlb[DEM[i]].SelectionMap.MergeAnotherDEMreflectance(RefDTMHalfSec);
   end;
   {$If Defined(StartCOPALOSmaps)} WriteLineToDebugFile('BestMapCOPALOSThreeParams hillshade merges dones'); {$EndIf}
   BigImagewithallmaps(3,ImageDir + ThisArea + '_cop_or_alos_better.png');
   MakeHistograms(DEM);
   {$If Defined(StartCOPALOSmaps)} WriteLineToDebugFile('BestMapCOPALOSThreeParams big image map done'); {$EndIf}
end;


procedure HighLowMapCOPALOSThreeParams;
var
   DEM : t3DEM;
   i : integer;
begin
   {$If Defined(StartCOPALOSmaps)} WriteLineToDebugFile('HighLowMapCOPALOSThreeParams entered'); {$EndIf}
   SetMapsForBigBitmaps(false);
   DEM[1] := TwoDEMHighLowMap(RefDTMHalfSec,ALOSHalfSec,COPHalfSec,MDDef.DEMIXSimpleTolerance,true,ThisArea + '_COP-ALOS_Compared_Ref_DTM');
   DEM[2] := TwoDEMHighLowMap(SlopeRefDTMPointhalfsec,SlopeALOSHalfSec,SlopeCOPHalfSec,MDDef.DEMIXSlopeTolerance,true,ThisArea + '_COP-ALOS_Compared_Ref_Slope');
   DEM[3] := TwoDEMHighLowMap(RuffRefDTMPointhalfsec,RuffALOSHalfSec,RuffCOPHalfSec,MDDef.DEMIXRuffTolerance,true,ThisArea + '_COP-ALOS_Compared_Ref_Roughness');

   for i := 1 to 3 do begin
      DEMGlb[DEM[i]].SelectionMap.MergeAnotherDEMreflectance(RefDTMHalfSec);
   end;
   {$If Defined(StartCOPALOSmaps)} WriteLineToDebugFile('HighLowMapCOPALOSThreeParams hillshade merges dones'); {$EndIf}
   BigImagewithallmaps(3,ImageDir + ThisArea + '_cop_or_alos_compared_reference.png');

   MakeHistograms(DEM);
   {$If Defined(StartCOPALOSmaps)} WriteLineToDebugFile('HighLowMapCOPALOSThreeParams big image map'); {$EndIf}
end;


procedure COP_ALOS_compare(What : integer);
var
   RefDEM,ALOS,Cop,DEM4 : integer;
begin
   PickSeveralExistingDEMs('Reference DEM','ALOS','COPDEM','', RefDEM,ALOS,Cop,DEM4);
   ReadDefault('Tolerance',MDDef.DEMIXSimpleTolerance);
   case What of
      caBest : BestCopOrALOSmap(RefDEM,ALOS,Cop,MDDef.DEMIXSimpleTolerance,'ALOS-COP-better');
      ca4Cat : TwoDEMHighLowMap(RefDEM,ALOS,COP,MDDef.DEMIXSimpleTolerance,true,'ALOS-COP-better');
      ca9Cat : TwoDEMHighLowMap(RefDEM,ALOS,COP,MDDef.DEMIXSimpleTolerance,false,'ALOS-COP-better');
   end;
end;


procedure PixelByPixelCopAlos;
const
   Params : array[1..3] of shortstring = ('best_elev_ref_dtm.vat.dbf','best_slope_ref_dtm.vat.dbf','best_roughness_ref_dtm.vat.dbf');
var
   Dir : PathStr;
   Results,TheFiles : tStringList;
   Sum : float64;
   i,j,db : integer;
   fName : PathStr;
   aLine : shortstring;
begin
   Dir := 'H:\aa_half_sec_test\';
   Results := tStringList.Create;
   Results.Add('AREA,PARAM,ALOS_BEST,BOTH_TIED,COP_BEST');
   for I := 1 to 3 do begin
       TheFiles := tStringList.Create;
       FindMatchingFiles(Dir,Params[i],TheFiles,2);
       for j := 0 to pred(TheFiles.Count) do begin
          fName := TheFiles.Strings[j];
          OpenNumberedGISDataBase(db,fName);
          GISdb[db].RenameField('NAME',GeomorphNames[i]);
          sum := GISdb[db].MyData.FieldSum('N');
          aLine := LastSubDir(ExtractFilePath(fName)) + ',' + GeomorphNames[i];
          GISdb[db].MyData.First;
          while not GISdb[db].MyData.eof do begin
             aline := aline + ',' + RealToString(100 * GISdb[db].MyData.GetFieldByNameAsInteger('N') / Sum,-12,-2);
             GISdb[db].MyData.Next;
          end;
          Results.Add(aline);
       end;
       TheFiles.Destroy;
   end;
   fName := NextFileNumber(MDTempDir,'p_by_cop_alos','.dbf');
   StringList2CSVtoDB(Results,fName);
end;


procedure CopAlosCompareReference;


   procedure DoOne(VatName,OutName : PathStr; Cats : integer);
   var
      TheFiles,Results : tStringList;
      i : Integer;
      Table : tMyData;
      Sum : float64;
      Fname : PathStr;
      aLine : shortstring;
   begin
      TheFiles := tStringList.Create;
      Petmar.FindMatchingFiles('H:\aa_half_sec_test\',VATname,TheFiles,2);
      Results := tStringList.Create;
      if (Cats = 4) then Results.Add('AREA,BOTH_HIGH,BOTH_CLOSE,BOTH_LOW,COMPLEX')
      else Results.Add('AREA,HIGH_HIGH,HIGH_GOOD,HIGH_LOW,GOOD_HIGH,GOOD_GOOD,GOOD_LOW,LOW_HIGH,LOW_GOOD,LOW_LOW');
      for i := 0 to pred(TheFiles.Count) do begin
         fName := TheFiles.Strings[i];
         Table := tMyData.Create(fName);
         Sum := Table.FieldSum('N');
         aline := LastSubDir(ExtractFilePath(fName));
         Table.First;
         while not Table.eof do begin
            aline := aline + ',' + RealToString(Table.GetFieldByNameAsInteger('N') * 100 / Sum, -12,-2);
            Table.Next;
         end;
         Results.Add(aline);
         Table.Destroy;
      end;
      TheFiles.Free;
      PetdbUtils.StringList2CSVtoDB(Results,OutName);
   end;


begin
   DoOne('cop-alos-dtm4.vat.dbf',mdTempDir + 'dtm-4.dbf',4);
   DoOne('cop-alos-dsm4.vat.dbf',mdTempDir + 'dsm-4.dbf',4);
   DoOne('cop-alos-dtm9.vat.dbf',mdTempDir + 'dtm-9.dbf',9);
   DoOne('cop-alos-dsm9.vat.dbf',mdTempDir + 'dsm-9.dbf',9);
end;



procedure HighLowCopAlosGeomorphometry(fName : PathStr = '');
var
   AreaName : shortString;
   SaveDir : PathStr;
   HalfSecRefDTM,SlopeMap,RuffMap,AspectMap,
   DTMElevDiffMapALOS,DTMElevDiffMapCOP : integer;

      procedure DifferenceStats(DEM,RefDEM : integer);
      var
         Diff5,Diff95 : float32;
      begin
          Diff5 := DEMGlb[DEM].FindPercentileElevation(5);
          Diff95 := DEMGlb[DEM].FindPercentileElevation(95);
      end;

begin
   if FileExists(fName) or GetFileFromDirectory('DEMIX area database','*.dbf',fName) then begin
      {$IfDef RecordDEMIX} writeLineToDebugFile('HighLowCopAlosGeomorphometry ' + fName); {$EndIf}
      AreaName := ExtractFileNameNoExt(fName);
      SaveDir := 'H:\aa_half_sec_test\' + AreaName + '\';

      DTMElevDiffMapALOS := OpenNewDEM(SaveDir + 'elev_diff_dtm_alos.tif');
      DTMElevDiffMapCOP := OpenNewDEM(SaveDir + 'elev_diff_dtm_cop.tif');

      HalfSecRefDTM := OpenNewDEM(SaveDir + 'ref_dtm.tif');
      SlopeMap := OpenNewDEM(SaveDir + 'slope_ref_dtm.tif');
      RuffMap := OpenNewDEM(SaveDir + 'roughness_ref_dtm.tif');
      AspectMap := OpenNewDEM(SaveDir + 'aspect_ref_dtm.tif');

      if FileExists(SaveDir + 'slope_ref_dtm.tif') then begin
         SlopeMap := OpenNewDEM(SaveDir + 'slope_ref_dtm.tif');
         RuffMap := OpenNewDEM(SaveDir + 'roughness_ref_dtm.tif');
      end
      else begin
         SlopeMap := -1;   //forces creation of slope and roughness maps
         RuffMap := CreateSlopeRoughnessSlopeStandardDeviationMap(HalfSecRefDTM,3,SlopeMap);
         AspectMap := MakeAspectMap(HalfSecRefDTM);
      end;
   end;
end;


end.
