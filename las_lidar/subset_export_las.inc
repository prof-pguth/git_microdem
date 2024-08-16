
{$IfDef MSWindows}

procedure LidarASCIIout(fName : PathStr; Output : tLASAsciiOutput);
var
   LasData : Las_Lidar.tLAS_data;
begin
    LasData := Las_Lidar.tLAS_data.Create(fName);
    LASdata.ExportXYZ(Output);
    LasData.Destroy;
end;


procedure LidarASCIIout(Output : tLASAsciiOutput);
var
   fName : PathStr;
begin
   fname := LastLidarDirectory;
   while GetFileFromDirectory('LAS file','*.las',fName) do begin
      LidarASCIIout(fName,Output);
   end;
end;


procedure LAS2GeoJSON(fName : PathStr);
var
   LasData : Las_Lidar.tLAS_data;
begin
    LasData := Las_Lidar.tLAS_data.Create(fName);
    LASdata.ExportGeoJSON;
    LasData.Destroy;
end;


procedure LAS2GeoJSON;
var
   fName : PathStr;
begin
   fname := LastLidarDirectory;
   while GetFileFromDirectory('LAS file','*.las',fName) do begin
      LAS2GeoJSON(fName);
   end;
end;


procedure tLAS_data.SplitLASfile;
const
   DirsNS : array[1..4] of shortstring = ('N2','N1','S1','S2');
   DirsEW : array[1..4] of shortstring = ('W2','W1','E1','E2');
   Dirs   : array[1..4] of shortstring = ('NW','NE','SE','SW');
var
   f,i,j,RecsRead,Method : integer;
   Dir : DirStr;
   bName : NameStr;
   Ext : ExtStr;
   Code : shortstring;
   Cut1,Cut2,Cut3,
   x,y : float64;
   fName : array[1..4] of PathStr;
   NewLas : array[1..4] of tCreateLasFile;
begin
   {$IfDef RecordCreateLASfiles} writeLineToDebugFile('tLAS_data.SplitLASfile in ' + LASfileName + '   ' + AllRangeString(LASHeader) ); {$EndIf}
   FSplit(LASfileName,Dir,bName,Ext);
   reset(LasFile,1);
   BlockRead(LasFile,LasHeader,SizeOf(LasHeader));

   if (LAS_y_range > 2 * LAS_x_range) then Method := 1
   else if (LAS_x_range > 2 * LAS_y_range) then Method := 2
   else Method := 3;

   {$IfDef RecordCreateLASfiles} writeLineToDebugFile('Split method =' + IntToStr(Method) ); {$EndIf}

   for f := 1 to 4 do begin
      NewLas[f] := tCreateLasFile.Create;
      case Method of
         1 : Code := DirsNS[f];
         2 : Code := DirsEW[f];
         3 : Code := Dirs[f];
      end;
      fName[f] := Dir + bName + '-' + Code + Ext;
      NewLas[f].NewLasHeader := LasHeader;
      NewLas[f].CreateNewLASfile(fName[f],lasProjectionDefinition,lasHeader);
   end;

    if (Method = 1) then begin
        Cut1 := LasHeader.MinY + 0.75 * LAS_y_range;
        Cut2 := LasHeader.MinY + 0.50 * LAS_y_range;
        Cut3 := LasHeader.MinY + 0.25 * LAS_y_range;
    end
    else if (Method = 2) then begin
        Cut1 := LasHeader.MinX + 0.75 * LAS_x_range;
        Cut2 := LasHeader.MinX + 0.50 * LAS_x_range;
        Cut3 := LasHeader.MinX + 0.25 * LAS_x_range;
    end
    else begin
        Cut1 := LasHeader.MinY + 0.5 * LAS_y_range;
        Cut2 := LasHeader.MinX + 0.5 * LAS_x_range;
        Cut3 := LasHeader.MinX + 0.5 * LAS_x_range;
    end;

   StartProgress('LAS Split ' + ExtractFileName(LASfileName));
   PrepDataRead;
   for i := 0 to ReadsRequired do begin
       UpdateProgressBar(i/ReadsRequired);
       ReadPoints(RecsRead);
       for j := 1 to RecsRead do begin
          if (Method = 1) then begin
              y := ExpandLAS_Y(j);
              if y > Cut1 then f := 1
              else if y > Cut2 then f := 2
              else if y > Cut3 then f := 3
              else f := 4;
          end
          else if (Method = 2) then begin
              x := ExpandLAS_X(j);
              if x > Cut1 then f := 4
              else if x > Cut2 then f := 3
              else if x > Cut3 then f := 2
              else f := 1;
          end
          else begin
              x := ExpandLAS_X(j);
              y := ExpandLAS_Y(j);
              if y > Cut1 then begin
                 if x > Cut2 then f := 2
                 else f := 1;
              end
              else begin
                 if x > Cut3 then f := 3
                 else f := 4;
              end;
          end;
          NewLas[f].AddShotToOutputBuffer(Self,j);
      end;
   end;
   FreeLASRecordMemory;

   for f := 1 to 4 do begin
      NewLas[f].Destroy;
   end;
   EndProgress;
end;


function tLAS_data.OldExportBinary(Layer : integer; var GeometryFName,ColorsFName : PathStr; ExportFilter : tLASClassificationCategory = lccAll) : boolean;
//exports LAS point cloud to binary files for use in FMX viewer
var
   Points : ^tPointXYZIArray;
   i,j,last,xc,yc,RecsRead,NPts : integer;
   Outf : file;
   z,xutm,yutm : float64;
   bmp : tMyBitmap;
   Range : float64;
   ThisPoint : tLASClassificationCategory;
   Color : tPlatformColor;
begin
   {$IfDef RecordLASexport} writeLineToDebugFile('tLAS_data.OldExportBinary in layer=' + IntToStr(Layer) + ', ' + ExtractFileNameNoExt(GeometryFName)); {$EndIf}
   Range := MDDef.MaxValidZinPointCloud - MDDef.LowValidZinPointCloud;
   New(Points);
   NPts := 0;
   StartProgress('LAS export, ' + ExtractFileNameNoExt(GeometryFName) + ' pts=' + IntToStr(Self.NumPointRecs));
   PrepDataRead;
   for i := 0 to ReadsRequired do begin
       UpdateProgressBar(i/ReadsRequired);
       ReadPoints(RecsRead);
       for j := 1 to RecsRead do begin
          z := ExpandLAS_Z(j);
          if (not MDDef.LasElevChecks) or ((z >= MDDef.LowValidZinPointCloud) and (z <= MDDef.MaxValidZinPointCloud)) then begin
             ThisPoint := LASClassificationCategory(j);
             if (ExportFilter = lccAll) or (ThisPoint = ExportFilter) then begin
                inc(NPts);
                GetShotCoordinatesUTM(j,xutm, yutm);
                Points^[NPTs].x := xutm;
                Points^[NPTs].y := yutm;
                Points^[NPTs].z := ExpandLAS_Z(j);

                if MDdef.ls.ColorCoding = lasccCloudID then Color := MDDef.CloudMapSymbol[Layer].Color
                else GetColor(j,LasHeader.MinZ,LasHeader.MaxZ,Color);
                Points^[NPTs].Int := Color.rgbtRed;
                Points^[NPTs].Int2 := Color.rgbtGreen;
                Points^[NPTs].Int3 := Color.rgbtBlue;
                if (NPts >= MaxPts) then break;
             end;
          end;
      end;
      //if (NPts >= MaxPts) then break;
   end;
   FreeLASRecordMemory;
   if (GeometryFName = '') then GeometryFName := Petmar.NextFileNumber(MDTempDir, 'cloud_slicer_','.xyzib');
   case MDdef.ls.ColorCoding of
         lasccIntensity : ColorsFName := Palette256Bitmap(p256Gray);
         lasccPointSourceID,
         lasccUserData  : case Layer of
                             1 : ColorsFName := Palette256Bitmap(p256RedRamp);
                             2 : ColorsFName := Palette256Bitmap(p256GreenRamp);
                             3 : ColorsFName := Palette256Bitmap(p256BlueRamp);
                             else ColorsFName := Palette256Bitmap(p256Gray);
                          end;
         else ColorsFName := FullPaletteBitmap;
   end;

   result := (NPts > 0);
   if Result then begin
      AssignFile(Outf,GeometryFName);
      rewrite(Outf,1);
      BlockWrite(OutF,Points^,NPTs * SizeOf(tPointXYZI));
      CloseFile(outf);
   end;
   Dispose(Points);
   ShowDefaultCursor;
   EndProgress;
   {$IfDef RecordLASexport} writeLineToDebugFile('tLAS_data.OldExportBinary out pts done, NPTs=' + IntToStr(Npts)); {$EndIf}
end;


procedure tLAS_data.ExportXYZ(Output : tLASAsciiOutput; ScaleFactor : float64 = -99; CenterOutput : boolean = true);
var
   Category,i,j,RecsRead,Npts : integer;
   xoff,yoff,zoff : float64;
   r,g,b : byte;
   results : tStringList;
   TStr : shortstring;
begin
   {$IfDef RecordLASexport} writeLineToDebugFile('tLAS_data.ExportXYZ in'); {$EndIf}
   StartProgress('LAS2XYZ');
   if (ScaleFactor < 0) then ReadDefault('Scale factor',ScaleFactor);

   if CenterOutput then begin
      FigureOffsets(false,true,xoff,yoff,zoff);
   end
   else begin
      xoff := 0;
      yoff := 0;
      zoff := 0;
   end;
   NPts := 0;
   Results := tStringList.Create;
   PrepDataRead;
   for i := 0 to ReadsRequired do begin
       UpdateProgressBar(i/ReadsRequired);
       ReadPoints(RecsRead);
       for j := 1 to RecsRead do begin
          inc(NPts);
          Category := LASclassification(j);
          if (Output = lasascXYYonly) then TStr := ''
          else if (Output = lasascClassInt) then TStr := ' ' + IntToStr(Category) + ' ' + IntToStr(GetShotIntensity(j))
          else begin
             if GetRGBComponents(j,r,g,b)then
                TStr := ' ' + IntToStr(r) + ' ' + IntToStr(g) + ' ' + IntToStr(b)
             else TStr := '';
          end;
          Results.Add(RealToString(ScaleFactor*(ExpandLAS_X(j)-xoff),-12,-MDDef.GeoJSONP_xydec) + ' ' + RealToString(ScaleFactor*(ExpandLAS_Y(j)-yoff),-12,-MDDef.GeoJSONP_xydec) + ' ' +
                      RealToString(ScaleFactor*(ExpandLAS_Z(j)- zoff),-12,-MDDef.GeoJSONP_zdec)  +  TStr);
      end;
   end;
   FreeLASRecordMemory;

   Results.SaveToFile(ChangeFileExt(LasfileName,'.xyz'));
   Results.Free;
   EndProgress;
   if (not QuietActions) then MessageToContinue('Extracted xyz points: ' + IntToStr(NPts));
end;


procedure tLAS_data.ExportXYZ_DB(LatLong : boolean; fName : PathStr; MaxPoint : int64);
label
   Done;
var
   Category,i,j,RecsRead,Npts : integer;
   Lat,Long,xutm,yutm : float64;
   Table : tMyData;
begin
   {$IfDef RecordLASexport} WriteLineToDebugFile('tLAS_data.ExportXYZ in'); {$EndIf}
   StartProgress('LAS export to DB');
   NPts := 0;
   if LatLong then CreateLatLongTable(fName)
   else CreateXYZTable(fName,false);
   Table := tMyData.Create(fName);
   PrepDataRead;
   for i := 0 to ReadsRequired do begin
       UpdateProgressBar(i/ReadsRequired);
       ReadPoints(RecsRead);
       for j := 1 to RecsRead do begin
          inc(NPts);
          Table.Insert;
          if LatLong then begin
             GetShotCoordinatesLatLong(j,Lat,Long);
             Table.SetFieldByNameAsFloat('LAT',Lat);
             Table.SetFieldByNameAsFloat('LONG',Long);
          end
          else begin
             GetShotCoordinatesUTM(j,xUTM,yUTM);
             Table.SetFieldByNameAsFloat('X',xutm);
             Table.SetFieldByNameAsFloat('Y',yutm);
          end;
          Table.SetFieldByNameAsFloat('Z',ExpandLAS_Z(j));

          Table.Post;
          if (Npts = MaxPoint) then begin
             MessageToContinue('Too many points, only ' + IntToStr(MaxPoint) + '/' + IntToStr(NumPointRecs) + ' exported');
             goto Done;
          end;
      end;
   end;
   Done:;
   FreeLASRecordMemory;
   Table.Free;
   EndProgress;
end;


procedure tLAS_data.ExportIcesat(MapOwner : tMapForm; Lat,Long : float64);
var
   i,j,RecsRead : integer;
   Boxsize : float64;
   GPSTime : TDateTime;
   outf : tstringlist;
   line : shortstring;
   fName : Pathstr;
   bb : sfBoundBox;
   wYear,wmonth,wDay : Word;
begin
  {$IfDef RecordLASexport} WriteLineToDebugFile('tLAS_data.ExportXYZ in'); {$EndIf}
   BoxSize := 0.5 * 1 / 3600;
   bb.XMax := Long + Boxsize;
   bb.XMin := Long - Boxsize;
   bb.YMax := Lat + Boxsize;
   bb.YMin := Lat - Boxsize;
(*
Create point type 1, with the following fields:
PointSourceID : track, single value for the entire track
GPSTime : date, noon of the day the track was acquired, single value for entire track
Classification : Confidence; 20-24 (the confidence plus 20, so it is not interpreted as a standard category
UserData : beam, single value for the entire track  GT1R = 1 GT1L = 2 GT2R = 3 GT2L = 4 GT3R = 5 GT3L = 6
*)
   StartProgress('LAS export to DB');
   outf := tStringlist.Create;
   Outf.Add('LAT,LONG,Z,TRACK,BEAM,QUALITY,DATE');
   PrepDataRead;
   for i := 0 to ReadsRequired do begin
       UpdateProgressBar(i/ReadsRequired);
       ReadPoints(RecsRead);
       for j := 1 to RecsRead do begin
          GetShotCoordinatesLatLong(j,Lat,Long);
          if PointInBoundingBox(Lat,Long,bb) then begin
             GPSTime := GPSTimeToDateTime(round(GetGPSTime(j)));
             DecodeDate(GPSTime,wYear,wmonth,wDay);
             line := RealToString(Lat,-12,-7) + ',' + RealToString(Long,-12,-7) + ',' + RealToString(ExpandLAS_Z(j),-12,-2) + ',' + IntToStr(PointSourceID(j)) + ',' + IntToStr(UserDataRec(j)) + ',' +
                 IntToStr(LASClassification(j) - 20) + ',' + IntToStr(wMonth) + '/'  + IntToStr(wDay) + '/' + IntToStr(wYear);
             OutF.Add(Line);
          end;
      end;
   end;
   FreeLASRecordMemory;
   fName := Petmar.NextFileNumber(MDTempDir, 'points_around_','.csv');
   MapOwner.StringListToLoadedDatabase(outf,fName);
   EndProgress;
end;


procedure SubsetLASfiles(fName : PathStr; Memo1 : tMemo = nil);

            function FileSize(fileName : wideString) : Int64;
            // https://www.thoughtco.com/file-size-in-bytes-using-delphi-1057888
            var
               sr : TSearchRec;
            begin
               if FindFirst(fileName, faAnyFile, sr ) = 0 then  result := Int64(sr.FindData.nFileSizeHigh) shl Int64(32) + Int64(sr.FindData.nFileSizeLow)
               else result := -1;
               FindClose(sr);
            end;


         procedure DoOneDir(fName : PathStr);
         var
            LasData : Las_Lidar.tLAS_data;
            TheFiles : tStringList;
            I, Found,Round : integer;
            fSize : Int64;
            FirstFile : PathStr;
         begin
            FirstFile := '';
            LastLidarDirectory := fName;
            if ValidPath(fName) then begin
               repeat
                   {$IfDef RecordLAS_subset} writeLineToDebugFile('start for loop'); {$EndIf}
                   TheFiles := Nil;
                   Petmar.FindMatchingFiles(LastLidarDirectory,'*.las',TheFiles);
                   Found := 0;
                   for I := 0 to pred(TheFiles.Count) do begin
                      fName := TheFiles.Strings[i];
                      Fsize := GetFileSize(fName);
                      if (fName <> FirstFile) and ((abs(FSize) > MDDef.LASMaxSubsetSize  * 1000 * 1000) or (FSize < 0)) then begin
                          {$IfDef RecordLAS_subset} writeLineToDebugFile('process=' + fName); {$EndIf}
                          inc(Found);
                          if (Memo1 <> Nil) then Memo1.Lines.Add(TimeToStr(Now) + '  ' + IntToStr(Found) + '  ' + ExtractFileName(fName) + '  ' + SmartMemorySizeBytes(fSize));
                          LasData := Las_Lidar.tLAS_data.Create(fName);
                          LasData.SplitLasFile;
                          LasData.Destroy;
                          ApplicationProcessMessages;
                          SysUtils.DeleteFile(fName);
                          {$IfDef RecordLAS_subset} WriteLineToDebugFile('   done'); {$EndIf}
                      end {if Fsize};
                   end {for i};
                   if (FirstFile = '') then FirstFile := fName;
                   {$IfDef RecordLAS_subset} writeLineToDebugFile('for loop over'); {$EndIf}
                   TheFiles.Free;
                   {$IfDef RecordLAS_subset} writeLineToDebugFile('TheFiles.Free over'); {$EndIf}
               until (Found = 0);
               {$IfDef RecordLAS_subset} writeLineToDebugFile('repeat loop over'); {$EndIf}
               if (Memo1 <> Nil) then Memo1.Lines.Add(TimeToStr(Now) + '  done');
            end;
         end;

var
   Paths : tStringList;
   i : integer;
begin
   {$IfDef RecordLAS_subset} writeLineToDebugFile('SubsetLASfiles, in'); {$EndIf}
   ReadDefault('Max LAS file size (MB)',MDDef.LASMaxSubsetSize);

   Paths := tStringList.Create;
   Paths.Add(LastLidarDirectory);

   if GetMultipleDirectories('Lidar project',Paths) then begin
      for i := 0 to pred(Paths.Count) do begin
         LastLidarDirectory := Paths[i];
         DoOneDir(LastLidarDirectory);
      end;
   end;
   Paths.Free;
   {$IfDef RecordLAS_subset} WriteLineToDebugFile('SubsetLASfiles out'); {$EndIf}
end;

{$EndIf}


procedure tLAS_data.ExportGeoJSON;
var
   i,j,RecsRead,Npts,Decs : integer;
   xoff,yoff,zoff : float64;
   sl : tStringList;
   JSONstring : ANSIstring;
   fName : PathStr;
begin
  {$IfDef RecordLASexport} writeLineToDebugFile('tLAS_data.ExportGeoJSON in'); {$EndIf}
   StartProgress('LAS2GeoJSON');
   FigureOffsets(false,true,xoff, yoff,zoff);
   if LasProjectionDefinition.LASProjection.ThisIsUTMFile then Decs := MDDef.GeoJSONP_xydec
   else Decs := 7;

   sl := tStringList.Create;

   JSONString := '{"v":"GeoJSON-P 0.2"' +
                  ',"npts":' + IntToStr(NumPointRecs) +
                  ',"minx":' + RealToString(LasHeader.MinX,-18,-Decs) +
                  ',"maxx":' + RealToString(LasHeader.MaxX,-18,-Decs) +
                  ',"miny":' + RealToString(LasHeader.MinY,-18,-Decs) +
                  ',"maxy":' + RealToString(LasHeader.MaxY,-18,-Decs) +
                  ',"minz":' + RealToString(LasHeader.MinZ,-18,-2) +
                  ',"maxz":' + RealToString(LasHeader.MaxZ,-18,-2) +
                  ',"offx":' + RealToString(xoff,-18,-Decs) +
                  ',"offy":' + RealToString(yoff,-18,-Decs) +
                  ',"offz":' + RealToString(zoff,-18,-2) +
                  ',"proj":';

   if lasProjectionDefinition.LASProjection.ThisIsUTMFile then JSONString := JSONString + '"UTM"'
   else JSONString :=  JSONString  + '"GCS"';
   JSONString := JSONString + ',"data":[';
   sl.Add(JSONstring);
   JSONString := '';
   NPts := 0;
   PrepDataRead;
   for i := 0 to ReadsRequired do begin
      UpdateProgressBar(i/ReadsRequired);
      ReadPoints(RecsRead);
      {$IfDef RecordLASexport} WriteLineToDebugFile('Read: ' + IntToStr(i) + '/' + IntToStr(ReadsRequired) + '   Rec=' + IntToStr(RecsRead));   {$EndIf}
       for j := 1 to RecsRead do begin
          inc(NPts);
          JSONString :=  JSONString + RealToString(ExpandLAS_X(j)-xoff,-12,-Decs) + ',' +
                                      RealToString(ExpandLAS_Y(j)-yoff,-12,-Decs) + ',' +
                                      RealToString(ExpandLAS_Z(j)-zoff,-12,-MDDef.GeoJSONP_zdec) + ',' +
                                      IntToStr(LASclassification(j)) + ',' +
                                      IntToStr(GetShotIntensity(j)) + ',';
          if (j mod 100 = 0) then begin
             {$IfDef RecordLASexport}  if (j mod 1000 = 0) then WriteLineToDebugFile('    Saved: ' + IntToStr(j)); {$EndIf}
             sl.Add(JSONstring);
             JSONString := '';
          end;
      end;
   end;
   FreeLASRecordMemory;
   EndProgress;
   Delete(JSONString,Length(JSONString),1);
   JSONString :=  JSONString + ']}';
   sl.Add(JSONstring);

   fName := ChangeFileExt(LasFileName,'.json');
   sl.SaveToFile(fName);
   sl.Free;
   {$IfDef VCL}
      MainBZ2(fName);
   {$EndIf}
   ShowDefaultCursor;
   if not QuietActions then MessageToContinue('Extracted points: ' + IntToStr(NPts));
   {$IfDef RecordLASexport} writeLineToDebugFile('tLAS_data.ExportGeoJSON out,  Extracted points: ' + IntToStr(NPts)); {$EndIf}
end;


