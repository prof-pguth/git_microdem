{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}
{                                   }
{ include file for demeros          }
{___________________________________}


function IsLandsat(fName : PathStr; var MPath : PathStr) : boolean;
begin
   Result := false;
   fName := ExtractFileNameNoExt(fName);
   if (fName <> '') then begin
      if ((fName[1] = 'L') and (fName[3] in ['1'..'9'])) then begin
         //LC82020362016198LGN00_
         Result := true;
         MPath := ExtractFilePath(fName) + Copy(fName,1,22);
      end
      else if ((fName[1] = 'L') and (fName[4] in ['1'..'9'])) then begin
         Result := true;
         MPath := ExtractFilePath(fName) + Copy(fName,1,41);
      end;
   end;
end;

function IsMSS(fName : PathStr) : boolean;
begin
   fName := ExtractFileNameNoExt(fName);
   Result := (fName[2] = 'M');
end;


function IsLandsatLook(fName : PathStr) : boolean;
var
   MPath : PathStr;
begin
   Result := IsLandsat(fname,MPath) and (StrUtils.AnsiContainsText(Uppercase(fName),'_T1.tif')) or
      (StrUtils.AnsiContainsText(Uppercase(fName),'_TIR.TIF')) or (StrUtils.AnsiContainsText(Uppercase(fName),'LANDSAT_LOOK') ) or (StrUtils.AnsiContainsText(Uppercase(fName),'_T1_REFL') )
      or (StrUtils.AnsiContainsText(Uppercase(fName),'_T2_REFL') )
      or (StrUtils.AnsiContainsText(Uppercase(fName),'_RT_QB') ) or (StrUtils.AnsiContainsText(Uppercase(fName),'_RT_TIR') ) or (StrUtils.AnsiContainsText(Uppercase(fName),'_RT_REFL') )
end;

function tSatImage.GetLandsatBQAName : shortstring;
begin
   if (LandsatNumber in [7]) then Result := LandSatDir + SceneBaseName + 'BQA.tif'
   else Result := LandSatDir + SceneBaseName + 'BQA.tif';
   if not FileExists(Result) then Result := '';
end;

function tSatImage.GetLandsatMetadataName : shortstring;
begin
   Result := LandSatDir + SceneBaseName + 'MTL.txt';
   if not FileExists(Result) then Result := LandSatDir + SceneBaseName +  '.meta';
   if not FileExists(Result) then Result := '';
end;

function tSatImage.IsLandsatImageAnalysisBand(Band : integer) : boolean;
begin
   if (LandsatNumber in [8,9]) then Result := Band in [1..7,9]
   else if (LandsatNumber = 7) then Result := Band in [1..5,7]
   else Result := Band in [1..5,7];
end;


procedure GetLandsatMetadata(fName : PathStr; var LandsatMetadata : tLandsatMetadata);
var
   Month,Day,i,j : integer;
   Mname,baseName,basePath : PathStr;
   MData : tStringList;
   aLine : string;
   PostEqual : ANSIstring;
begin
   {$IfDef RecordTMSat} WriteLineToDebugFile('GetLandsatMetadata for ' + fName); {$EndIf}
   BasePath := ExtractFilePath(fName);
   if IsLandsat(fName,BaseName) then begin
      if (IsLandSatLook(fName)) then begin
      end
      else begin
         fName := ExtractFileName(fName);
         //https://landsat.usgs.gov/what-are-naming-conventions-landsat-scene-identifiers
         if (fName[5] = '_') then begin //Landsat Product Identifier
            //LE07_L1GT_092074_20050605_20170114_01_T2_B1.TIF
            LandsatMetadata.TM_No := StrToInt(fName[4]);
            LandsatMetadata.Path := StrToInt( Copy(fName,11,3));
            LandsatMetadata.Row := StrToInt( Copy(fName,14,3));
            LandsatMetadata.Year := StrToInt( Copy(fName,18,4) );
            Month := StrToInt( Copy(fName,22,2) );
            Day := StrToInt( Copy(fName,24,2) );
            LandsatMetadata.JDay := AnnualJulianDay(LandsatMetadata.Year,Month,Day);
         end
         else begin //Scene ID
            LandsatMetadata.TM_No := StrToInt(fName[3]);
            LandsatMetadata.Path := StrToInt( Copy(fName,4,3));
            LandsatMetadata.Row := StrToInt( Copy(fName,7,3));
            LandsatMetadata.Year := StrToInt( Copy(fName,10,4) );
            LandsatMetadata.JDay := StrToInt( Copy(fName,14,3));
            JulianDay(LandsatMetadata.JDay,LandsatMetadata.Year,Month,Day);
         end;
         LandsatMetadata.Sensor := fName[2];
         LandsatMetadata.Date := DateToStr(EncodeDate(LandsatMetadata.Year,Month,Day));

         LandSatMetadata.CloudCover := -99;
         LandSatMetadata.SunAzimuth := -99;
         LandSatMetadata.SunElevation := -99;
         LandSatMetadata.ImageQuality := 0;
         LandSatMetadata.SceneTime := '';
         for j := 1 to 11 do begin
            LandSatMetadata.RadianceMult[j] := -99;
            LandSatMetadata.RadianceAdd[j] := -99;
            LandSatMetadata.ReflectanceMult[j] := -99;
            LandSatMetadata.ReflectanceAdd[j] := -99;
            if (j >= 10) then begin
               LandSatMetadata.K1Const[j] := -99;
               LandSatMetadata.K2Const[j] := -99;
            end;
         end;

         MName := BasePath + BaseName + 'MTL.txt';
         if FileExists(MName) then begin
            {$IfDef RecordTMSat} WriteLineToDebugFile('Found metadata ' + MName); {$EndIf}
            Mdata := tStringList.Create;
            Mdata.LoadFromFile(MName);
            for I := 0 to pred(Mdata.Count) do begin
               aLine := UpperCase(ptTrim(Mdata[i]));
               PostEqual := ptTrim(Petmar_types.AfterSpecifiedCharacter(aline,'='));
               StripCharacter(PostEqual,'"');
               if PostEqual <> 'NULL' then begin
                  if StrUtils.AnsiContainsText(aline,'CLOUD_COVER ') then LandSatMetadata.CloudCover := strtoFloat(PostEqual);
                  if StrUtils.AnsiContainsText(aline,'SENSOR_ID ') then LandSatMetadata.SENSOR_ID := PostEqual;

                  if StrUtils.AnsiContainsText(aline,'IMAGE_QUALITY') then LandSatMetadata.ImageQuality := strtoInt(PostEqual);
                  if StrUtils.AnsiContainsText(aline,'SUN_AZIMUTH') then LandSatMetadata.SunAzimuth := strtoFloat(PostEqual);
                  if StrUtils.AnsiContainsText(aline,'SUN_ELEVATION') then LandSatMetadata.SunElevation := strtoFloat(PostEqual);
                  if StrUtils.AnsiContainsText(aline,'SCENE_CENTER_TIME') or StrUtils.AnsiContainsText(aline,'SCENE_CENTER_SCAN_TIME') then LandSatMetadata.SceneTime := PostEqual;
                  if StrUtils.AnsiContainsText(aline,'CORNER_UL_LAT') then LandSatMetadata.LatHi := strtoFloat(PostEqual);
                  if StrUtils.AnsiContainsText(aline,'CORNER_LR_LAT') then LandSatMetadata.LatLow := strtoFloat(PostEqual);
                  if StrUtils.AnsiContainsText(aline,'CORNER_UL_LON') then LandSatMetadata.LongHi := strtoFloat(PostEqual);
                  if StrUtils.AnsiContainsText(aline,'CORNER_LR_LON') then LandSatMetadata.LongLow := strtoFloat(PostEqual);
                  for j := 1 to 11 do begin
                     if StrUtils.AnsiContainsText(aline,'RADIANCE_MULT_BAND_'+ IntToStr(j) + ' ') then LandSatMetadata.RadianceMult[j] := strtoFloat(PostEqual);
                     if StrUtils.AnsiContainsText(aline,'RADIANCE_ADD_BAND_'+ IntToStr(j) + ' ') then LandSatMetadata.RadianceAdd[j] := strtoFloat(PostEqual);
                     if StrUtils.AnsiContainsText(aline,'REFLECTANCE_MULT_BAND_'+ IntToStr(j) + ' ') then LandSatMetadata.ReflectanceMult[j] := strtoFloat(PostEqual);
                     if StrUtils.AnsiContainsText(aline,'REFLECTANCE_ADD_BAND_'+ IntToStr(j) + ' ') then LandSatMetadata.ReflectanceAdd[j] := strtoFloat(PostEqual);

                     if StrUtils.AnsiContainsText(aline,'FILE_NAME_BAND_'+ IntToStr(j) + ' ') then begin
                        if LandSatMetadata.BandFileName[j] = '' then begin
                           //to skip the processing history present for Level 2 data
                           LandSatMetadata.BandFileName[j] := PostEqual;
                        end;
                     end;
                  end;
                  for j := 10 to 11 do begin
                     if StrUtils.AnsiContainsText(aline,'K1_CONSTANT_BAND_'+ IntToStr(j) + ' ') then LandSatMetadata.K1Const[j] := strtoFloat(PostEqual);
                     if StrUtils.AnsiContainsText(aline,'K2_CONSTANT_BAND_'+ IntToStr(j) + ' ') then LandSatMetadata.K2Const[j] := strtoFloat(PostEqual);
                  end;
               end;
            end;
         end
         else begin
            {$IfDef RecordTMSat} WriteLineToDebugFile('No metadata'); {$EndIf}
            MessageToContinue('No metatada file for this LANDSAT scene; problems likely');
         end;
         {$IfDef RecordTMSat} WriteLineToDebugFile('GetLandsatMetadata Out, basename= ' + BaseName); {$EndIf}
      end;
   end;
end;

function LandsatSceneMetadata(fName : PathStr; var LandsatNumber : byte; var MPath : PathStr) : tStringList;
//LC80410342013213LGN00
var
   Month,Day,i : integer;
   MData : tStringList;
   aLine : string;
   dPath : PathStr;
   LandsatMetadata : tLandsatMetadata;
begin
   {$IfDef RecordTMSat} WriteLineToDebugFile('LandsatSceneMetadata in, fname= ' + fName); {$EndIf}
   Result := Nil;
   if IsLandsat(fName,MPath) then begin
      if IsMSS(fName) then begin
         LandSatNumber := StrToInt(Copy(ExtractFileName(fName),3,2));
      end
      else begin
         GetLandsatMetadata(fName, LandsatMetadata);
         Result := TStringList.Create;
         Result.Add('-----------------------');
         Result.Add('Landsat ' + IntToStr(LandsatMetadata.TM_No) + '   Sensor ' + LandsatMetadata.Sensor);
         LandsatNumber := LandsatMetadata.TM_No;
         Result.Add('WRS Path ' + IntToStr(LandsatMetadata.Path) + '   WRS Row ' + IntToStr(LandsatMetadata.Row));
         Result.Add('Year  ' + IntToStr(LandsatMetadata.Year) + '   Julian day ' + IntToStr(LandsatMetadata.JDay));
         JulianDay(LandsatMetadata.JDay,LandsatMetadata.Year,Month,Day);
         Result.Add('Collected  ' + DateToStr(EncodeDate(LandsatMetadata.Year,Month,Day)));
         dPath := ExtractFilePath(fName) + MPath + 'MTL.txt';
         if FileExists(dPath) then begin
            Mdata := tStringList.Create;
            Mdata.LoadFromFile(dPath);
            for I := 0 to pred(Mdata.Count) do begin
               aLine := ptTrim(Mdata[i]);
               if StrUtils.AnsiContainsText(aline,'CLOUD_COVER') then Result.Add(aLine);
               if StrUtils.AnsiContainsText(aline,'IMAGE_QUALITY') then Result.Add(aLine);
               if StrUtils.AnsiContainsText(aline,'SUN_AZIMUTH') then Result.Add(aLine);
               if StrUtils.AnsiContainsText(aline,'SUN_ELEVATION') then Result.Add(aLine);
               if StrUtils.AnsiContainsText(aline,'SCENE_CENTER_TIME') then Result.Add(aLine);
            end;
         end;
         Result.Add('-----------------------');
      end;
   end;
  {$IfDef RecordTMSat} WriteLineToDebugFile('LandsatSceneMetadata out, Landsat=' + IntToStr(LandsatNumber) + '   mpath=' + Mpath); {$EndIf}
end;


function ShortLandsatName(fName : PathStr) : ShortString;
var
   Year,Month,Day,JDay : integer;
   LName : shortString;
begin
   fName := ExtractFileName(fName);
   if (fName[5] = '_') then begin //Landsat Product Identifier
      Year := StrToInt( Copy(fName,18,4) );
      Month := StrToInt( Copy(fName,22,2) );
      Day := StrToInt( Copy(fName,24,2) );
      JDay := AnnualJulianDay(Year,Month,Day);
      LName := fName[1] + fName[4];
      if FName[2] = 'M' then LName := LName + ' MSS'
      else if FName[2] = 'T' then LName := LName + ' TM'
      else if FName[2] = 'E' then LName := LName + ' ETM+'
      else if FName[2] = 'C' then LName := LName + ' OLI';
      Result := LName + '_' + IntToStr(Month) + '-' + IntToStr(Day) + '-' + IntToStr(Year);
   end
   else begin //Scene ID
      Year := StrToInt( Copy(fName,10,4) );
      JDay := StrToInt( Copy(fName,14,3));
      LName := fName[1] + fName[3];
      Result := LName + '_' + IntToStr(Year) + '_' + IntToStr(JDay);
   end;
end;


function LandsatBand(fName : PathStr) : integer;
begin
   fName := ExtractFileNameNoExt(fName);
   if  (StrUtils.AnsiContainsText(fName,'sr_band')) then begin
(*
LC08_L1TP_039035_20180918_20180928_01_T1_sr_band2.tif
*)
      Delete(fname,1,48);
   end
   else if (fName[5] = '_') then begin //Landsat Product Identifier
(*
LE07_L1GT_092074_20050605_20170114_01_T2_B5
*)
      Delete(fname,1,42);
      if fname[2] = '_' then Delete(fname,2,pred(length(fname)));
   end
   else begin //Scene ID
(*
LC80150332015229LGN00_B1
*)
      Delete(fname,1,23);
   end;
   Result := StrToInt(fName);
end;


function HistogramLandsatName(fName : PathStr) : ShortString;
var
   MPath : PathStr;
begin
   if IsLandsat(fName,MPath) and (not IsLandsatLook(fName)) then begin
      Result := ExtractFilePath(fName) + 'band_hist\' + ExtractFileNameNoExt(fName) + TMhistfName;
   end
   else begin
      Result := fName + TMhistfName;
   end;
end;


{$IfDef ExLandsatQA}
{$Else}

   procedure tSatImage.GetLandsatQAMap(What : ShortString; fName : PathStr; LowBitV,HighBitV : integer);
   var
      Mname,MPath,fName2 : PathStr;
      zw : word;
      NewDEM,theDEM,x,y : integer;
      BitSet : array[0..3] of int32;
      zf : float64;
      i : integer;
      VAT : tStringList;
   begin
       {$IfDef RecordTMSat} WriteLineToDebugFile('GetLandsatQAMap in'); {$EndIf}
       if IsLandsat(fName,MPath) then  begin
          MName := LandSatDir + SceneBaseName + 'BQA.TIF';
          if FileExists(MName) then begin
              LoadNewDEM(theDEM,MName,false);
              NewDEM := DEMGlb[theDEM].CloneAndOpenGrid(ByteDEM,What + ' mask', euIntCode);
              StartProgress('QA bits');
              for i := 0 to 3 do BitSet[i] := 0;

              for x := 0 to pred(DEMGlb[theDEM].DEMheader.NumCol) do begin
                 if (x mod 100 = 0) then UpdateProgressBar(x/DEMGlb[theDEM].DEMheader.NumCol);

                 for y := 0 to pred(DEMGlb[theDEM].DEMheader.NumRow) do  begin
                    DEMGlb[theDEM].GetElevMetersOnGrid(x,y,zf);
                    zw := round(zf);
                    if (HighBitV = 99) then begin
                       if IsBitSet(zw,LowBitV) then begin
                          DEMGlb[NewDEM].SetGridElevation(x,y,3);
                          inc(BitSet[3]);
                       end
                       else inc(BitSet[0]);
                    end
                    else begin
                       if IsBitSet(zw,LowBitV) and IsBitSet(zw,HighBitV) then begin
                          DEMGlb[NewDEM].SetGridElevation(x,y,3);
                          inc(BitSet[3]);
                       end
                       else if (not IsBitSet(zw,LowBitV)) and IsBitSet(zw,HighBitV) then begin
                          DEMGlb[NewDEM].SetGridElevation(x,y,2);
                          inc(BitSet[2]);
                       end
                       else if IsBitSet(zw,LowBitV) then begin
                          DEMGlb[NewDEM].SetGridElevation(x,y,1);
                          inc(BitSet[1]);
                       end
                       else inc(BitSet[0]);
                    end;
                 end;
              end;
              CloseSingleDEM(theDEM);
              EndProgress;
              DEMGlb[NewDEM].CheckMaxMinElev;
              VAT := tStringList.Create;
              VAT.Add('value,class,color,n');

              if BitSet[0] > 0 then VAT.Add('0,Not checked,' + IntToStr(clSilver) + ',' + IntToStr(BitSet[0]));
              if BitSet[1] > 0 then VAT.Add('1,Low odds,' + IntToStr(clRed) + ',' + IntToStr(BitSet[1]));
              if BitSet[2] > 0 then VAT.Add('2,Medium odds,' + IntToStr(clYellow) + ',' + IntToStr(BitSet[2]));
              if BitSet[3] > 0 then VAT.Add('3,High odds,' + IntToStr(clLime) + ',' + IntToStr(BitSet[3]));
              fName2 := MDtempDir + DEMGlb[NewDEM].AreaName + '.dem';
              DEMGlb[NewDEM].WriteNewFormatDEM(fName2);
              fName2 := ChangeFileExt(fName2,'.vat.dbf');
              StringList2CSVtoDB(VAT,fName2,true);
              DEMGlb[NewDEM].SetUpMap(true,mtDEMVATTable,DEMGlb[NewDEM].AreaName);
              {$IfDef RecordProblems} WriteLineToDebugFile(DEMGlb[NewDEM].AreaName ); for i := 0 to 3 do WriteLineToDebugFile(IntToStr(i) + IntegerToString(BitSet[i],12)); {$EndIf}
          end;
       end
       else begin
          {$IfDef RecordTMSat} WriteLineToDebugFile('Missing ' + MName); {$EndIf}
       end;
   end;


   function tSatImage.GetLandsatQA(fName : PathStr) : tStringList;
   type
      int3 = array[1..3] of integer;
   var
      Mname,MPath: PathStr;
      zf : float64;
      theDEM,x,y,z,Bit : integer;
      BitsSet : array[0..15] of integer;
      Bitter : array[0..15] of boolean;
      Water,Snow,Cirrus,Cloud : int3;

         procedure SetPair(var Param : Int3; Low,High : integer);
         begin
            if Bitter[High] and Bitter[Low] then inc(Param[3])
            else if Bitter[High] then inc(Param[2])
            else if Bitter[Low] then inc(Param[1]);
         end;

   begin
       Result := Nil;
       if IsLandsat(fName,MPath) then begin
          MName := MPath + '_BQA.TIF';
          if FileExists(MName) then begin
              LoadNewDEM(theDEM,MName,false);
              for x := 0 to 15 do BitsSet[x] := 0;
              for x := 1 to 3 do begin
                 Water[x] := 0;
                 Snow[x] := 0;
                 Cirrus[x] := 0;
                 Cloud[x] := 0;
              end;
              StartProgress('QA bits');
              for x := 0 to pred(DEMGlb[theDEM].DEMheader.NumCol) do begin
                 if x mod 100 = 0 then UpdateProgressBar(x/DEMGlb[theDEM].DEMheader.NumCol);
                 for y := 0 to pred(DEMGlb[theDEM].DEMheader.NumRow) do begin
                    DEMGlb[theDEM].GetElevMetersOnGrid(x,y,zf);
                    z := round(zf);
                    for Bit := 0 to 15 do begin
                       Bitter[Bit] := IsBitSet(z,Bit);
                       if Bitter[Bit] then Inc(BitsSet[Bit]);
                    end;
                    SetPair(Water,4,5);
                    SetPair(Snow,10,11);
                    SetPair(Cirrus,12,13);
                    SetPair(Cloud,14,15);
                 end;
              end;
              CloseSingleDEM(theDEM);
              EndProgress;
              Result := tStringList.Create;
              for Bit := 0 to 15 do begin
                 if BitsSet[Bit] > 0 then Result.Add('Bit ' + IntegerToString(Bit,2) + '   n=' + IntegerToString(BitsSet[Bit],8));
              end;
              Result.Add('');
              Result.Add('              Low     Moderate     High');
              Result.Add('Water     ' + IntegerToString(Water[1],10) +  IntegerToString(Water[2],10)  + IntegerToString(Water[3],10));
              Result.Add('Snow/ice  ' + IntegerToString(Snow[1],10) +  IntegerToString(Snow[2],10)  + IntegerToString(Snow[3],10));
              Result.Add('Cirrus    ' + IntegerToString(Cirrus[1],10) +  IntegerToString(Cirrus[2],10)  + IntegerToString(Cirrus[3],10));
              Result.Add('Cloud     ' + IntegerToString(Cloud[1],10) +  IntegerToString(Cloud[2],10)  + IntegerToString(Cloud[3],10));
          end;
       end;
   end;

{$EndIf}



