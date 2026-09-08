
{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{    include file for petdbutil.pas      }
{________________________________________}


function IsThisGPSTrackFile(FileWanted : PathStr) : boolean;
var
   Ext : ExtStr;
begin
    Ext := UpperCase(ExtractFileExt(FileWanted));
    Result := ExtEquals(Ext,'.gpx') or
              ExtEquals(Ext,'.fit') or
              ExtEquals(Ext,'.tcx') or
             (ExtEquals(Ext,'.csv') and StrUtils.AnsiContainsText(FileWanted,'gps_location_') );
end;



function ImportExerciseTrack(var FileWanted : PathStr; ShowTable : boolean = false; MapOwner : tMapForm = nil) : integer;
var
   Ext : ExtStr;
   i : integer;
   sl : tStringList;
   CheckDeleteUnusedFields : boolean;
   BasePath,tName : PathStr;
   ID : shortstring;

      procedure AddNav;
      begin
         if MDDef.AddFitNav then begin
            GISDB[i].AddNavFields;
            GISDB[i].ApplyGISFilter('DIST_KM<0.0004');
            GISDB[i].DeleteAllSelectedRecords;
            {$IfDef RecordGPX} WriteLineToDebugFile('AddNavFields done'); {$EndIf}
         end;
      end;

begin
    if IsThisGPSTrackFile(FileWanted) then begin
       {$IfDef RecordGPX} WriteLineToDebugFile('ImportExerciseTrack in, ' + FileWanted); {$EndIf}
       Ext := UpperCase(ExtractFileExt(FileWanted));
       ShowHourglassCursor;
       try
          SaveBackupDefaults;
          MDDef.UseMeters := false;
          MDdef.AddSpeed := true;
          MDdef.Add3DDist := false;
          HeavyDutyProcessing := true;
          SetColorForProcessing;
          BasePath := 'c:\mapdata\tracks\';
          SafeMakeDir(BasePath);
          tName := FileWanted;
          if ExtEquals(Ext,'.TCX') then begin
             sl := FitBitTCXtoStringList(FileWanted,ID);
             {$IfDef RecordGPX} WriteLineToDebugFile('FitBitTCXtoStringList, recs=' + IntToStr(sl.count)); {$EndIf}
             FileWanted := BasePath + ID + '.dbf';
             if FileExists(FileWanted) then begin
                sl.Destroy;
                File2Trash(tName);
             end
             else begin
               i := StringList2CSVtoDB(sl,FileWanted,false,false,false);
               AddNav;
               CloseSingleDB(i);
             end;
          end
          else if ExtEquals(Ext,'.csv') then begin
               sl := tStringList.Create;
               sl.LoadFromFile(FileWanted);
               FileWanted := BasePath + ExtractFileNameNoExt(FileWanted) + '.dbf';
               FileWanted := StringReplace(FileWanted,'gps_location','gps_track',[rfReplaceAll, rfIgnoreCase]);
               i := StringList2CSVtoDB(sl,FileWanted,false,false,false);
               if ValidDB(i) then begin
                   {$IfDef RecordGPX} WriteLineToDebugFile('DB created'); {$EndIf}
                   GISDB[i].ApplyGISFilter('DATASOURCE=' + QuotedStr('Fitbit App'));  //because files have duplicates, from Fitbit and the phone'
                   GISDB[i].DeleteAllSelectedRecords;
                   {$IfDef RecordGPX} WriteLineToDebugFile('Duplicates removed'); {$EndIf}
                   ConvertTimeStringToDecimalHours(i,false);
                   {$IfDef RecordGPX} WriteLineToDebugFile('Converted time'); {$EndIf}
                   AddNav;
                   CloseSingleDB(i);
               end
               else begin
                   {$IfDef RecordGPX} WriteLineToDebugFile('ImportExerciseTrack GPSBabel_fit2gpx fail'); {$EndIf}
               end;
          end
          else if ExtEquals(Ext,'.gpx') or ExtEquals(Ext,'.fit') then begin
            if ExtEquals(Ext,'.gpx') then begin
               {$IfDef RecordFIT} WriteLineToDebugFile('open GPX, picked: ' + FileWanted); {$EndIf}
               tName := MDtempDir + ExtractFileNameNoExt(FileWanted) + '.gpx';
               CopyFile(FileWanted,tName);
               CheckDeleteUnusedFields := true;
            end
            else if ExtEquals(Ext,'.fit') then begin
               {$IfDef RecordFIT} WriteLineToDebugFile('open FIT, picked: ' + FileWanted); {$EndIf}
               tName := BasePath + ExtractFileName(FileWanted);
               MoveFile(FileWanted,tName);
               FileWanted := tName;
               {$IfDef RecordFIT} WriteLineToDebugFile('open FIT, copied to: ' + FileWanted); {$EndIf}
               tName := MDtempDir + ExtractFileNameNoExt(FileWanted) + '.gpx';
               if GPSBabel_fit2gpx(FileWanted,tName) then begin
                   {$IfDef RecordFIT} WriteLineToDebugFile('GPSBabel_fit2gpx created ' + tName); {$EndIf}
                   if (GetFileSize(tName) < 500) then begin
                      Result := 0;
                      exit;
                  end;
               end
               else begin
                  {$IfDef RecordFIT} WriteLineToDebugFile('GPSBabel_fit2gpx failed'); {$EndIf}
                  Result := 0;
                  exit;
               end;
            end;
            if FileExists(tName) then begin
               FileWanted := BasePath;
               {$IfDef RecordFIT} WriteLineToDebugFile('call GPXtoDBF'); {$EndIf}
               GPXtoDBF(tName,FileWanted);
               {$IfDef RecordFIT} WriteLineToDebugFile('GPX processed ' + FileWanted); {$EndIf}
               //OpenNumberedGISDataBase(Result,FileWanted,ShowTable,MapOwner);
            end;
          end;
       finally
          RestoreBackupDefaults;
          HeavyDutyProcessing := false;
          SetColorForWaiting;
       end;
    end;
end;


function DecimalHoursFromTimeStr(TimeStr : ANSIstring) : float32;
begin
   Result := 1.0 * StrToInt(Copy(TimeStr,12,2)) + StrToInt(Copy(TimeStr,15,2)) / 60 + StrToInt(Copy(TimeStr,18,2)) / 3600;
end;


procedure GPXtoDBF(inName : PathStr; var OutName : PathStr);
{
      <trkpt lat="38.991316203" lon="-76.502945870">
        <ele>-16.000</ele>
        <time>2024-11-15T16:53:17Z</time>
        <speed>9.891000</speed>
      </trkpt>
}
var
   FileInMemory,OutList : tStringList;
   i,j,GISNum : integer;
   Hours : float64;
   TimeStr,LatLongStr,ElevStr,Str : AnsiString;

   procedure CheckForLatLongString(Str : AnsiString; var LatLongStr : AnsiString);
   begin
       if StrUtils.AnsiContainsText(Str,'<TRKPT') then begin
          Petmar_Types.BeforeSpecifiedCharacterANSI(Str,'"',true,true);
          LatLongStr := Petmar_Types.BeforeSpecifiedCharacterANSI(Str,'"',true,true) + ',';
          Petmar_Types.BeforeSpecifiedCharacterANSI(Str,'"',true,true);
          LatLongStr := LatLongStr + Petmar_Types.BeforeSpecifiedCharacterANSI(Str,'"') + ',';
       end;
   end;

   procedure CheckForElevStr(Str : AnsiString; var ElevStr : AnsiString);
   var
      j : integer;
   begin
       if (Str <> '') and StrUtils.AnsiContainsText(Str,'<ELE>') then begin
          for j := length(str) downto 1 do
             if not (str[j] in ['-','.','0'..'9']) then Delete(str,j,1);
          ElevStr := Str + ',';
       end;
   end;
(*
2025-02-04T15:41:50.000-05:00
*)
   procedure CheckForTimeStr(Str : AnsiString; var TimeStr : AnsiString);
   begin
       if (Str <> '') and StrUtils.AnsiContainsText(Str,'<TIME') then begin
           Petmar_Types.BeforeSpecifiedCharacterANSI(Str,'>',true,true);
           TimeStr := Petmar_Types.BeforeSpecifiedCharacterANSI(Str,'<');
           Hours := DecimalHoursFromTimeStr(TimeStr);
           TimeStr := TimeStr + ',' + RealToString(Hours,-12,-6);
       end;
   end;

begin
   if FileExists(inName) and ValidPath(OutName) then begin
      {$IfDef RecordGPX} WriteLineToDebugFile('GPXtoDBF, in for ' + inName); {$EndIf}
      ShowHourglassCursor;
      OutName := OutName + ExtractFileNameNoExt(inName) + '.dbf';
      if FileExists(OutName) then begin
         File2Trash(inName);
         exit;
      end;

      FileInMemory := tStringList.Create;
      FileInMemory.LoadFromFile(inName);
      OutList := tStringList.Create;
      OutList.Add('LAT,LONG,ELEV,DTG,DEC_HOURS');
      TimeStr := '';
      LatLongStr := '';
      ElevStr := '';
      for I := 0 to pred(FileInMemory.count) do begin
         Str := UpperCase(ptTrim(UpperCase(FileInMemory.Strings[i])));
         {$IfDef RecordFullGPX} WriteLineToDebugFile('line=' + Str); {$EndIf}
         CheckForLatLongString(Str,LatLongStr);
         CheckForElevStr(Str,ElevStr);
         CheckForTimeStr(Str,TimeStr);
         if StrUtils.AnsiContainsText(Str,'</TRKPT>') then begin
            if (ElevStr = '') then ElevStr := '-999,';
            Outlist.Add(LatLongStr + ElevStr + TimeStr);
            TimeStr := '';
            LatLongStr := '';
            ElevStr := '';
         end;
      end;
      {$IfDef RecordGPX} WriteLineToDebugFile('GPXtoDBF, parsed, lines=' + IntToStr(Outlist.Count)); {$EndIf}
      {$IfDef RecordFullGPX} OutList.SaveToFile(MDtempDir + 'gpx_stringlist.txt'); {$EndIf}

      GISNum := StringList2CSVtoDB(OutList,OutName,false,false,false);
      if ValidDB(GISNum) then begin
          {$IfDef RecordGPX} WriteLineToDebugFile('StringListCSVtoTable done'); {$EndIf}
          if MDDef.AddFitNav then begin
             GISDB[GisNUM].AddNavFields;
             {$IfDef RecordGPX} WriteLineToDebugFile('AddNavFields done'); {$EndIf}
          end;
          CloseSingleDB(GISnum);
      end
      else begin
           {$IfDef RecordGPX} WriteLineToDebugFile('StringListCSVtoTable fail'); {$EndIf}
      end;
      FileInMemory.Destroy;
      {$IfDef RecordGPX} WriteLineToDebugFile('GPXtoDBF, out for ' + outName); {$EndIf}
   end
   else begin
      MessageToContinue('GPXtoDBF missing ' + InName);
      {$IfDef RecordGPX} WriteLineToDebugFile('GPXtoDBF missing ' + InName); {$EndIf}
   end;
end;

procedure ConvertTimeStringToDecimalHours(DBonTable : integer; InputHoursMinSec : boolean);
//2026-05-27T00:18:45Z            //google takeout export
//2026-06-13T18:11:27.000-04:00   //fitbit exports
var
   TStr,TimeFName : ShortString;
   Hours : float64;
   i,rc : integer;
begin
   {$If Defined(RecordEditsDone) or Defined(RecordEditDBProblems)} WriteLineToDebugFile('Tdbtablef.TimefieldHHMMSStohours1Click in ' + GISdb[DBonTable].dbName + ' Time field to decimal hours'); {$EndIf}
    if not InputHoursMinSec then begin
       if GISdb[DBonTable].MyData.FieldExists('TIME') then TimeFName := 'TIME'
       else if GISdb[DBonTable].MyData.FieldExists('TIMESTAMP') then TimeFName := 'TIMESTAMP'
       else exit;
    end;
    if GISdb[DBonTable].AddFieldToDataBase(ftFloat,'DEC_HOURS',12,6) then begin
       {$IfDef RecordEditDB} WriteLineToDebugFile('dec_hours added'); {$EndIf}
    end;

    GISdb[DBonTable].ClearGISFilter;
    GISdb[DBonTable].EmpSource.Enabled := false;
    ShowHourglassCursor;
    GISdb[DBonTable].MyData.First;
    rc := ProgressIncrement(GISdb[DBonTable].MyData.FiltRecsInDB);
    i := 0;
    while not GISdb[DBonTable].MyData.EOF do begin
       if (i mod rc = 0) then UpdateProgressBar(i/GISdb[DBonTable].MyData.FiltRecsInDB);
       inc(i);
       GISdb[DBonTable].MyData.Edit;
       if not InputHoursMinSec then begin
          TStr := UpperCase(GISdb[DBonTable].MyData.GetFieldByNameAsString(TimeFName));
          if ANSIContainsText(TStr,':') then begin
             if TStr[5] = '-' then begin
                Hours := DecimalHoursFromTimeStr(TStr);
             end
             else begin
                if (TStr[2] = ':') then Tstr := '0' + TStr;
                Hours := 1.0 * StrToInt(Copy(TStr,1,2)) + StrToInt(Copy(TStr,4,2)) / 60 + StrToInt(Copy(TStr,7,2)) / 3600;
                if (Length(TStr) > 8) and ANSIContainsText(TStr,'PM') then Hours := Hours + 12;
             end;
          end
          else begin
             TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString(TimeFName);
             while (length(TStr) < 6) do Tstr := '0' + TStr;
             Hours := 1.0 * StrToInt(Copy(TStr,1,2)) + StrToInt(Copy(TStr,3,2)) / 60 + StrToInt(Copy(TStr,5,2)) / 3600;
          end;
       end
       else begin
           Hours := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('HOUR') + GISdb[DBonTable].MyData.GetFieldByNameAsInteger('MINUTE') /60 + GISdb[DBonTable].MyData.GetFieldByNameAsInteger('SECOND') /3600;
       end;
       GISdb[DBonTable].MyData.SetFieldByNameAsFloat('DEC_HOURS',Hours);
       GISdb[DBonTable].MyData.Next;
    end;
    GISdb[DBonTable].ShowStatus;
   {$If Defined(RecordEditsDone) or Defined(RecordEditDBProblems)} WriteLineToDebugFile('Tdbtablef.TimefieldHHMMSStohours1Click out'); {$EndIf}
end;


function FitBitTCXtoStringList(fName : PathStr; var ID : shortstring) : tStringList;
var
   FileInMemory : tStringList;
   DecHours : float32;
   i,j : integer;
   Str : AnsiString;
   ElevStr,LatStr,LongStr,TimeStr,HeartStr,DistStr : ANSIstring;

   procedure Check(What : shortstring; var ThatString : ANSIString);
   var
      StartString,EndString : shortstring;
   begin
       StartString := '<' + What + '>';
       if StrUtils.AnsiContainsText(Str,StartString) then begin
          ThatString  := Str;
          ThatString := ANSIReplaceStr(ThatString,StartString,'');
          ThatString := ANSIReplaceStr(ThatString,'</' + What + '>','');
       end;
   end;

begin
   ShowHourglassCursor;
   FileInMemory := tStringList.Create;
   FileInMemory.LoadFromFile(fName);
   Result := tStringList.Create;
   ID := '';
   Result.Add('LAT,LONG,TIME,ELEV_M,HEART_RATE,DEC_HOURS');
   for I := 0 to pred(FileInMemory.count) do begin
       Str := UpperCase(ptTrim(UpperCase(FileInMemory.Strings[i])));
       if str = '<TRACKPOINT>' then begin
          ElevStr := '';
          LatStr  := '';
          LongStr := '';
          TimeStr := '';
          HeartStr := '';
          DistStr := '';
       end;
       Check('ID',ID);
       Check('TIME',TimeStr);
       Check('LATITUDEDEGREES',LatStr);
       Check('LONGITUDEDEGREES',LongStr);
       Check('ALTITUDEMETERS',ElevStr);
       Check('VALUE',HeartStr);
       Check('DISTANCEMETERS',DistStr);
       if (str = '</TRACKPOINT>') then begin
          if (LatStr <> '') and (LongStr <> '') then begin
              ReplaceCharacter(ID,':','_');
              ElevStr := RealToString(StrToFloat(ElevStr),-12,2);
              DecHours := DecimalHoursFromTimeStr(TimeStr);
              Result.Add(LatStr + ',' + LongStr + ',' + TimeStr + ',' + ElevStr + ',' + HeartStr + ',' + RealToString(DecHours,-12,-6));
          end;
       end;
    end;
   FileInMemory.Destroy;
   File2Trash(fName);
end;


procedure LineKMLtoStringList(fName : PathStr);
var
   FileInMemory : tStringList;
   i,j,k : integer;
   Str, aLine : ANSIString;
   ZStr,NameStr,LatStr,LongStr : shortstring;
   Stop,  Adding : boolean;
   ShapeFileCreator : tShapeFileCreation;
begin
   ShowHourglassCursor;
   FileInMemory := tStringList.Create;
   FileInMemory.LoadFromFile(fName);
   fName := ChangeFileExt(fName,DefaultDBExt);
   ShapeFileCreator := tShapeFileCreation.Create(WGS84DatumConstants,fName,true,3);
   Adding := false;
   i := 0;
   while I <= pred(FileInMemory.count) do begin
       Str := UpperCase(ptTrim(UpperCase(FileInMemory.Strings[i])));
        if copy(str,1,10) = '<PLACEMARK' then begin
           Delete(Str,1,10);
           Adding := true;
        end;
        if Adding then begin
           if Copy(Str,1,6) = '<NAME>' then begin
              Delete(Str,1,6);
              NameStr := Petmar_Types.BeforeSpecifiedCharacterANSI(Str,'<');
           end;
           if (Copy(Str,1,13)) = '<COORDINATES>' then begin
              repeat
                  inc(i);
                  Str := UpperCase(ptTrim(UpperCase(FileInMemory.Strings[i])));
                  Stop := (Copy(Str,1,14)) = '</COORDINATES>';
                  if not stop then repeat
                     LongStr := Petmar_Types.BeforeSpecifiedCharacterANSI(Str,',',true,true);
                     LatStr := Petmar_Types.BeforeSpecifiedCharacterANSI(Str,',',true,true);
                     ZStr := Petmar_Types.BeforeSpecifiedCharacterANSI(Str,' ',true,true);
                     ShapeFileCreator.AddPointToShapeStream(StrToFloat(LatStr),StrToFloat(LongStr));
                  until length(Str) = 0;
              until stop;
           end;
           if str = '</PLACEMARK>' then begin
              Adding := false;
              ShapeFileCreator.RecordName := NameStr;
              ShapeFileCreator.ProcessRecordForShapeFile;
           end;
        end;
        inc(i);
      end;
    ShapeFileCreator.CloseShapeFiles;
    FileInMemory.Destroy;
end;


function PointKMLtoStringList(fName : PathStr) : tStringList;
var
   FileInMemory : tStringList;
   NumIcons,
   i,j : integer;
   Str : AnsiString;
   NameStr,LatStr,LongStr,IconStr : shortstring;
   Adding,DoingIcon : boolean;
   IconNames : array[1..2,1..25] of ShortString;
begin
   ShowHourglassCursor;
   FileInMemory := tStringList.Create;
   FileInMemory.LoadFromFile(fName);

   Result := tStringList.Create;
   Result.Add('LAT,LONG,NAME,ICON');

    Adding := false;
    DoingIcon := false;
    NumIcons := 0;
    for I := 0 to pred(FileInMemory.count) do begin
       Str := UpperCase(ptTrim(UpperCase(FileInMemory.Strings[i])));
       if copy(str,1,9) = '<STYLE ID' then begin
           inc(NumIcons);
           DoingIcon := true;
           Petmar_Types.BeforeSpecifiedCharacterANSI(Str,'"',true,true);
           Str := Petmar_Types.BeforeSpecifiedCharacterANSI(Str,'"');
           {$IfDef RecordKML} WriteLineToDebugFile('Icon=' + IntToStr(NumIcons) + ' ' + Str); {$EndIf}
           IconNames[1,NumIcons] := Str;
       end;
       if DoingIcon then begin
           if copy(str,1,6) = '<HREF>' then begin
              Delete(Str,1,6);
              Str := Petmar_Types.BeforeSpecifiedCharacterANSI(Str,'<');
              {$IfDef RecordKML} WriteLineToDebugFile('    ' + Str); {$EndIf}
              IconNames[2,NumIcons] := Str;
           end;
           if copy(str,1,7) = '</STYLE' then DoingIcon := false;
       end;
       if Copy(str,1,10) = '<PLACEMARK' then begin
           Adding := true;
           DoingIcon := false;
           Delete(Str,1,10);
       end;
       if Adding then begin
           if Copy(Str,1,6) = '<NAME>' then begin
              Delete(Str,1,6);
              NameStr := Petmar_Types.BeforeSpecifiedCharacterANSI(Str,'<');
              if (NameStr = 'UNTITLED PLACEMARK') then NameStr := 'Placemark';
              Petmar.ReplaceCharacter(NameStr,',',';');
           end;
           if Copy(Str,1,10) = '<STYLEURL>' then begin
              Petmar_Types.BeforeSpecifiedCharacterANSI(Str,'#',true,true);
              str := Petmar_Types.BeforeSpecifiedCharacterANSI(Str,'<',true,true);
              for j := 1 to 25 do begin
                 if str = IconNames[1,j] then begin
                    IconStr := IconNames[2,j];
                    break;
                 end;
              end;
           end;
           if (Copy(Str,1,13)) = '<COORDINATES>' then begin
              Delete(Str,1,13);
              LongStr := Petmar_Types.BeforeSpecifiedCharacterANSI(Str,',',true,true);
              LatStr := Petmar_Types.BeforeSpecifiedCharacterANSI(Str,',',true,true);
           end;
           if str = '</PLACEMARK>' then begin
              Adding := false;
              Result.Add(LatStr + ',' + LongStr + ',' + NameStr + ',' + IconStr);
           end;
        end;
      end;
   FileInMemory.Destroy;
end;
