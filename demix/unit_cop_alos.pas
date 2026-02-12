unit demix_cop_alos;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$Define RecordDEMIX}
   {$Define RecordDEMIXLoad}
   {$Define RecordDEMIXsave}
   {$Define RecordCreateHalfSec}
   //{$Define RecordDEMIXMovies}
   {$Define RecordDEMIXVDatum}
   //{$Define RecordFullDEMIX}
   //{$Define ShowDEMIXWhatsOpen}
{$EndIf}


interface

uses
    System.SysUtils,System.Classes,StrUtils,VCL.ExtCtrls,VCL.Forms, VCL.Graphics, WinAPI.Windows,
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
   procedure OpenDEMIXRidges(fName : PathStr = '');


implementation

uses
   Nevadia_Main,
   DEMIX_Control,
   DEMstat,Make_grid,PetImage,PetImage_form,new_petmar_movie,DEMdatabase,PetDButils,Pick_several_dems,
   DEMCoord,DEMdefs,DEMMapf,DEMDef_routines,DEM_Manager,DEM_indexes,Petmar_db,PetMath;


procedure COP_ALOS_compare(What : integer);
var
   RefDEM,ALOS,Cop,DEM4 : integer;
begin
   PickSeveralExistingDEMs('Reference DEM','ALOS','COPDEM','', RefDEM,ALOS,Cop,DEM4);
   ReadDefault('Tolerance',MDDef.DEMIXSimpleTolerance);
   case What of
      caBest : BestCopOrALOSmap(RefDEM,ALOS,Cop,MDDef.DEMIXSimpleTolerance,'ALOS-COP-better');
      ca4Cat : TwoDEMHighLowMap(RefDEM,ALOS,COP,'Ref DEM',MDDef.DEMIXSimpleTolerance,true);
      ca9Cat : TwoDEMHighLowMap(RefDEM,ALOS,COP,'Ref DEM',MDDef.DEMIXSimpleTolerance,false);
   end;
end;


procedure PixelByPixelCopAlos;
const
   Params : array[1..3] of shortstring = ('best_elev_ref_dtm.vat.dbf','best_slope_ref_dtm.vat.dbf','best_roughness_ref_dtm.vat.dbf');
   Names : array[1..3] of shortstring = ('elevation','slope','roughness');
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
          GISdb[db].RenameField('NAME',Names[i]);
          sum := GISdb[db].MyData.FieldSum('N');
          aLine := LastSubDir(ExtractFilePath(fName)) + ',' + Names[i];
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


 const
   ALOSHalfSecfName = 'alos.tif';
   CopHalfSecfName = 'cop.tif';

   HalfSecRefDSMfName = 'ref_dsm.tif';
   HalfSecRefDTMfName = 'ref_dtm.tif';

   COP_ALOS_DifffName = 'cop-alos-diff.tif';
   COP_ALOS_DSM4fName = 'cop-alos-dsm4.tif';
   COP_ALOS_DTM4fName = 'cop-alos-dtm4.tif';
   COP_ALOS_DSM9fName = 'cop-alos-dsm9.tif';
   COP_ALOS_DTM9fName = 'cop-alos-dtm9.tif';
   SlopeMapfName = 'slope_ref_dtm.tif';
   RuffMapfName = 'roughness_ref_dtm.tif';
   AspectMapfName = 'aspect_ref_dtm.tif';
   BestCOP_ALOS_slopefName = 'best_slope_ref_dtm.tif';
   BestCOP_ALOS_rufffName = 'best_roughness_ref_dtm.tif';
   BestCOP_ALOS_elevfName = 'best_elev_ref_dtm.tif';

   DTMElevDiffMapALOSfName = 'elev_diff_dtm_alos.tif';
   DTMElevDiffMapCOPfName = 'elev_diff_dtm_cop.tif';
   DTMSlopeDiffMapALOSfName = 'slope_diff_dtm_alos.tif';
   DTMSlopeDiffMapCOPfName = 'slope_diff_dtm_cop.tif';
   DTMRuffDiffMapALOSfName = 'ruff_diff_dtm_alos.tif';
   DTMRuffDiffMapCOPfName = 'ruff_diff_dtm_cop.tif';



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


procedure OpenDEMIXRidges(fName : PathStr = '');
const
   CatName : array[1..7] of shortstring = ('Ref','ALOS','ALOS+Ref', 'COP','Cop+Ref','ALOS+COP','ALOS+Cop+Ref');
   CatColor : array[1..7] of tColor = (clBlue,clGreen,clAqua,clLime,clTeal,clPurple,clBrown);
var
   AreaName : shortstring;
   SaveDir : PathStr;
   Fixed,i,x,y,SumDEM : integer;
   z : float32;
   Higher,Lower : array[1..3] of integer;
   Hist : array[1..7] of int64;
   Merge : tDEMbooleans;
   VAT : tStringList;
begin
   if FileExists(fName) or GetFileFromDirectory('DEMIX area database','*.dbf',fName) then begin
      {$IfDef RecordDEMIX} writeLineToDebugFile('OpenDEMIXRidges ' + fName); {$EndIf}
      AreaName := ExtractFileNameNoExt(fName);
      SaveDir := fName[1] + ':\aa_half_sec_test\' + AreaName + '\';
      ZeroDEMs;
      HalfSecRefDTM := OpenNewDEM(SaveDir + HalfSecRefDTMfName);
      HalfSecALOS := OpenNewDEM(SaveDir + ALOSHalfSecfName);
      HalfSecCOP := OpenNewDEM(SaveDir + CopHalfSecfName);

      NumHighLowNeighborsMaps(HalfSecRefDTM,MDDef.WoodRegionRadiusPixels,MDdef.MinDeltaZToClassify, Higher[1],Lower[1]);
      NumHighLowNeighborsMaps(HalfSecALOS,MDDef.WoodRegionRadiusPixels,MDdef.MinDeltaZToClassify, Higher[2],Lower[2]);
      NumHighLowNeighborsMaps(HalfSecCOP,MDDef.WoodRegionRadiusPixels,MDdef.MinDeltaZToClassify, Higher[3],Lower[3]);
      ShowHourglassCursor;
      for I := 1 to 3 do DEMGlb[Lower[i]].MarkBelowMissing(4.5,Fixed);
      DEMGlb[Lower[1]].ReclassifyRange(1,8,1);
      DEMGlb[Lower[2]].ReclassifyRange(1,8,2);
      DEMGlb[Lower[3]].ReclassifyRange(1,8,4);


      for i := 1 to MaxDEMDataSets do Merge[i] := false;
      for I := 1 to 3 do Merge[Lower[i]] := true;
      ShowHourglassCursor;
      SumDEM := SumDEMs(Lower[1],Merge,AreaName + '_ridges',false,false);
      DEMGlb[SumDEM].MarkBelowMissing(0.5,Fixed);

      for i := 1 to 7 do Hist[i] := 0;
      ShowHourglassCursor;

      for x := 0 to pred(DEMGlb[SumDEM].DEMheader.NumCol) do begin
         for y := 0 to pred(DEMGlb[SumDEM].DEMheader.NumRow) do begin
            if DEMGlb[SumDEM].GetElevMetersOnGrid(x,y,z) then begin
               inc(Hist[round(z)]);
            end;
         end;
      end;

      Vat := tStringList.Create;
      Vat.add('VALUE,NAME,N,USE,COLOR');
      for i := 7 downto 1 do if (Hist[i] > 0) then Vat.add(IntToStr(i) + ',' + CatName[i] + ',' + IntToStr(Hist[i]) + ',Y,' + IntToStr(CatColor[i]));

      fName := MDTempDir + AreaName + '.vat.dbf';
      StringList2CSVtoDB(vat,fName,true);
      DEMGlb[SumDEM].VATFileName := fName;
      DEMglb[SumDEM].CheckMaxMinElev;
      DEMglb[SumDEM].SetUpMap(SumDEM,true,mtDEMVATTable);
   end;
end;






end.
