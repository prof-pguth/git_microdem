{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}
{                                        }
{        include file for md_use_tools   }
{________________________________________}


//type
   //tlas2Las = (lasAssignProjection,lasToUTM,lasAssignEPSGreprojectUTM,lasAssignUTM,lasAssignGeo,lasReprojectSpecifiedtoGeo);

      procedure LasToLasTools(How : tlas2Las);
      const
         StartDir : PathStr = '';
      label
         LetMeOutOfHere;
      var
         fName,OutName,PName,bName : PathStr;
         lf :  tLAS_data;
         elevparams,
         params,params2 : Shortstring;
         i : integer;
         DefaultFilter : byte;
         bf,theFileNames : tStringList;
      begin
         pName := lastools_bindir + 'las2las64.exe';
         if GetLASToolsFileName(pName) then begin
            Params := '';
            Params2 := '';
            elevparams := '';
            if (how = lasAssignProjection {Sender = AssignProjection1}) or (How = lasAssignEPSGreprojectUTM) then begin
               params2 := ' -epsg 26985';
               GetString('Assign input EPSG code',params2,false,ReasonableTextChars);
               if (how = lasAssignProjection) {Sender = AssignProjection1} then begin
                  //params := ' -sp83 OH_N -feet -elevation_feet  -target_meter -target_utm auto';
                  //params := ' -sp83 MD -meter -elevation_meter  -target_meter -target_utm auto';
                  params := '-target_epsg 26985';
                  GetString('Assign target projection parameters',params,false,ReasonableTextChars);
               end;
            end;

            if (how = lasToUTM) or (How = lasAssignUTM) or (How = lasAssignEPSGreprojectUTM) then begin
               PickUTMzone(MDDef.DefaultUTMZone);
            end;
            DefaultFilter := 1;
            fName := MainMapData;

            bf := tStringList.Create;
            bf.Add('set LASTOOLS_PROJ=C:\OSGeo4W\bin');
            bf.Add('set PROJ_LIB=C:\OSGeo4W\share\proj');

            theFileNames := tStringList.Create;
            if (StartDir = '') then StartDir := LastLidarDirectory;
            theFileNames.Add(StartDir);

            if GetMultipleFiles('input LAS or LAZ files','Lidar files|*.las;*.laz|*.laz|LAS files|*.las|LAZ files|*.laz',theFileNames,DefaultFilter) then begin
               if AnswerIsYes('Are elevations in feet') then elevparams := ' -elevation_feet';

               for I := 0 to pred(TheFileNames.Count) do begin
                  fName := theFileNames.Strings[i];

                  if (i=0) then begin
                     StartDir := ExtractFilePath(fName);
                     if StrUtils.AnsiContainsText(fName,' ') then begin
                        if not AnswerIsYes('Filename with space in file/path name will fail, continue anyway') then begin
                           goto LetMeOutOfHere;
                        end;
                     end;
                  end;

                  if (how = lasAssignProjection) then begin
                     OutName := ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_assigned.las';
                  end
                  else if (How = lasAssignUTM) then begin
                     OutName := (ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_utm.las');
                     params := '-meter -epsg ' + IntToStr(GetEPSGforUTMZone('WGS84',MDDef.DefaultLatHemi,MDDef.DefaultUTMZone));
                  end
                  else if (How = lasReprojectSpecifiedtoGeo) then begin
                     OutName := ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_geo.las';
                     params := '-target_epsg 4326';
                  end
                  else begin
                     if (How = lasAssignGeo) then begin
                        lf := tLAS_data.Create(FName);
                        if (lf.lasProjDef.lasProj.ModelType = LasLatLong) then begin
                           MDDef.DefaultUTMZone := GetUTMZone(0.5 * (lf.LAS_LatLong_Box.xmax - lf.LAS_LatLong_Box.xmin));
                        end;
                        if (lf.LAS_LatLong_Box.ymax > 0) then MDDef.DefaultLatHemi := 'N' else MDDef.DefaultLatHemi := 'S';
                        lf.Destroy;
                     end;
                     params := '-target_meter -proj_epsg ' + IntToStr(GetEPSGforUTMZone('WGS84',MDDef.DefaultLatHemi,MDDef.DefaultUTMZone));
                     OutName := (ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_utm.las');
                  end;
                  if (not FileExists(Outname)) then begin
                     bf.Add('REM ' + IntToStr(i) + '/' + IntToStr(TheFileNames.Count));
                     bf.Add(pName +  ' -i ' + fName + ' -o ' + OutName + ' ' + params + Params2 + elevparams);
                  end;
              end;
              bName := NextFileNumber(MDTempDir,'las2las2_','.bat');
              EndBatchFile(bName,bf,false);
            end;

           LetMeOutOfHere:;
            TheFileNames.Free;
         end;
      end;


procedure ConvertDEMtoLAZ(Memo1 : tMemo);
var
   InName,OutName : PathStr;
   Settings : shortstring;
   FilesWanted : tStringList;
   I,DEM : integer;
begin
   FilesWanted := tstringList.Create;
   FilesWanted.Add(LastDEMName);
   if GetMultipleFiles('DEMs to convert tyo LAZ',DEMFilterMasks,FilesWanted ,MDDef.DefaultDEMFilter) then begin
      Memo1.Visible := true;
      for I := 0 to pred(FilesWanted.Count) do begin
         Memo1.Lines.Add(TimeToStr(Now) + IntegerToString(i,4) + '/' + IntToStr(FilesWanted.Count));
         InName := FilesWanted.Strings[i];
         LoadNewDEM(DEM,InName,true);
         InName := DEMGlb[DEM].SelectionMap.GeotiffDEMNameOfMap;
         OutName := ChangeFileExt(DEMGlb[DEM].DEMFileName,'.laz');
         Lastools_DEMToLAZ(InName,OutName,'');
         CloseSingleDEM(DEM);
      end;
   end;
   FilesWanted.Free;
end;



function GetLASToolsFileName(var fName : PathStr) : boolean;
var
   BaseName : PathStr;
begin
   Result := true;
   if not FileExists(fName) then begin
      {$If Defined(RecordGDAL) or Defined(RecordGDALOpen) or Defined(OpenLasTools)} HighlightLineToDebugFile('GetGDALFileNames in, but not in ' + fName); {$EndIf}
      FindPath('Map library',':\gis_software\LAStools\bin\',LAStools_BinDir);
      BaseName := ExtractFileName(fName);
      fName := LAStools_BinDir + BaseName;
      if not FileExists(fName) then begin
         {$If Defined(RecordGDAL) or Defined(RecordGDALOpen) or Defined(OpenLasTools)} HighlightLineToDebugFile('File not in ' + fName); {$EndIf}
         if GetDOSPath('LAStools binary directory, something like ' +  LAStools_BinDir,LAStools_BinDir) then begin
            fName := LAStools_BinDir + BaseName;
            if not FileExists(fName) then begin
               {$If Defined(RecordGDAL) or Defined(RecordGDALOpen) or Defined(OpenLasTools)} HighlightLineToDebugFile('After user said, but no go for  ' + fName); {$EndIf}
               MessageToContinue('Could not find ' + fName);
               Result := false;
            end;
         end
         else begin
            Result := false;
         end;
      end;
   end;
end;


procedure LAStoolsTextToLAS;
var
   fName,PName : PathStr;
   cmd : ANSIString;
   UTMzone,ParseVals : shortstring;
   i : integer;
   DefaultFilter : byte;
   Ext : extstr;
   theFileNames,bf : tStringList;
begin
   pName := LAStools_BinDir + 'txt2las.exe';
   if GetLASToolsFileName(pName) then begin
      DefaultFilter := 1;
      theFileNames := tStringList.Create;
      theFileNames.Add(MainMapData);
      if GetMultipleFiles('input ascii file','xyz ASCII file|*.txt;*.csv;*.xyz;*.xyzi;*.yxz',theFileNames,DefaultFilter) then begin
         Ext := UpperCase(ExtractFileExt(theFileNames.Strings[i]));
         ParseVals := '';
         if (Ext = '.TXT') or (Ext = '.CSV') then begin
            ParseVals := 'xyzirc';
            GetString('line interpretation',ParseVals,false,['a'..'z']);
         end;
         UTMzone := '31U';
         GetString('UTM zone',UTMzone,true,ReasonableTextChars);
         bf := tStringList.Create;
         for I := 0 to pred(TheFileNames.Count) do begin
            fName := theFileNames.Strings[i];
            cmd := lastools_txt2las_cmd(fName,UTMZone,ParseVals);
            bf.Add('REM   ' + IntToStr(i) + '/' + IntToStr(TheFileNames.Count) + '   started ' + TimeToStr(now));
            bf.Add('echo %time%');
            bf.add(cmd);
         end;
         EndBatchFile(MDTempDir + 'lastools_txt2las.bat',bf);
      end;
      TheFileNames.Free;
   end;
end;


procedure BlastTinCreate(InName,OutName : PathStr; GridSize : float64);
var
   cmd : ansistring;
   pName : PathStr;
begin
  {$IfDef RecordUseOtherPrograms} WriteLineToDebugFile('BlastTinCreate, infile=' + InName + '  outfile=' + OutName); {$EndIf}
   OutName :=  ChangeFileExt(OutName,'.asc');
   pName := LAStools_BinDir + 'txt2las.exe';
   if GetLASToolsFileName(pName) then begin
      cmd := pName + ' -step ' + RealToString(GridSize,-12,-2) + ' -i ' +InName + ' -o ' + OutName;
      WinExecAndWait32(cmd);
      OpenNewDEM(OutName);
   end;
end;


procedure Lastools_DEMToLAZ(InName,OutName : PathStr; Extra : shortString = '');
var
   cmd : ansistring;
   pName : PathStr;
begin
   pName := LAStools_BinDir + 'demzip.exe';
   if GetLASToolsFileName(pName) then begin
      cmd := pName + ' -i ' + InName + ' -o ' + OutName  + Extra;
      WinExecAndWait32(cmd);
   end;
end;


function lastools_txt2las_cmd(inName : PathStr; UTMzone : shortString; ParseVals : shortstring = '') : shortstring;
var
   pName : PathStr;
begin
   pName := LAStools_BinDir + 'txt2las.exe';
   if GetLASToolsFileName(pName) then begin
     if ParseVals = '' then begin
        ParseVals := LowerCase(ExtractFileExt(inName));
        Delete(ParseVals,1,1);
     end;
     if (UTMZone <> '') then UTMZone := ' -utm ' + UTMzone;
     Result := pName + ' -parse ' + ParseVals + UTMZone + ' -v  -i ' + inName;
   end;
end;


procedure CallLasInfo;
var
   fName,OutName,PName : PathStr;
   cmd : ANSIString;
   i : integer;
   DefaultFilter : byte;
   theFileNames : tStringList;
begin
   pName := LAStools_BinDir + '\lasinfo.exe';
   if GetLASToolsFileName(pName) then begin
      DefaultFilter := 1;
      theFileNames := tStringList.Create;
      theFileNames.Add(LastLidarDirectory);
      if GetMultipleFiles('input LAS file','LAS file|*.las;*.laz',theFileNames,DefaultFilter) then begin
         for I := 0 to pred(TheFileNames.Count) do begin
            fName := theFileNames.Strings[i];
            OutName := MDtempDir + ExtractFileNameNoExt(fName) + '_las_info.txt';
            cmd := pName +  ' -i ' + fName + ' -o ' + OutName;
            WinExecAndWait32(cmd);
            ShowInNotepadPlusPlus(OutName,ExtractFileName(OutName));
        end;
      end;
      TheFileNames.Free;
   end;
end;

