
unit MrSidImagery;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordOpenMrSid}
   //{$Define RecordMrSidLimits}
{$EndIf}


interface

uses
   Windows, Messages, Classes, Graphics,SysUtils,StrUtils,
   System.IOUtils,
   Petmar,Petmar_types;


type
   tMrSidInfo = packed record
      Width,Height,Bands,
      UTMZone,
      SidLevels : int32;
      LatHemi   : AnsiChar;
      ulx,uly,
      lrx,lry,
      dx,dy    : float64;
      SPCS_SID : boolean;
   end;


procedure MrSidInfo(SidName : PathStr);
procedure ExtractTiffFromSID(var fName : PathStr);
function GetMrSidInfo(fName : PathStr) : tStringList;
function FillMrSidInfoRec(fName : PathStr) : tMrSidInfo;
function SubsetTiffFromSID(var fname : PathStr; DisplayIt : boolean; LatHi,LongLow,LatLow,LongHi : float64; SPCS : boolean) : PathStr;
procedure GetMrSidGeoLimits(fName : PathStr; var LatLow,LongLow,LatHigh,LongHigh : float64);
Procedure ConvertsSIDsToTiff;


implementation

uses
   DEMDefs,DEMDEF_routines,BaseMap,DEMEROS,DEMcoord;



Procedure ConvertsSIDsToTiff;
var
   fName   : PathStr;
   Command : ShortString;
   theFileNames : tStringList;
   i : integer;
   DefaultFilter : byte;
begin
   if MrSidEnabled then begin
      theFileNames := tStringList.Create;
      if GetMultipleFiles('input MrSID files','MrSIDfiles|*.sid',theFileNames,DefaultFilter) then  begin
         for I := 0 to pred(theFileNames.Count) do begin
            fName := theFileNames.Strings[i];
            Command := MrSidDecodeName +  ' -i ' + fName + ' -wf -o ' + ChangeFileExt(fName,'.tif');
            WinExecAndWait32(Command);
         end;
      end;
      theFileNames.Free;
   end;
end;



function FillMrSidInfoRec(fName : PathStr) : tMrSidInfo;
var
   Info : tStringList;
   DigitizeDatum : ShortString;
   RegVars : BaseMap.tRegVars;
   MapProjection : tMapProjection;
   fName2 : PathStr;
begin
   Info := GetMrSidInfo(fName);
   ApplicationProcessMessages;

   Result.Width  := FindIntegerFieldInStringList(info,'width',':');
   Result.Height := FindIntegerFieldInStringList(info,'height',':');
   Result.Bands  := FindIntegerFieldInStringList(info,'nband',':');
   Result.SidLevels  := FindIntegerFieldInStringList(info,'number of levels',':');

   Result.ulx  := FindFloatFieldInStringList(info,'X UL',':');
   Result.uly  := FindFloatFieldInStringList(info,'Y UL',':');
   if (Result.uly < 0) then Result.uly := Result.uly + 10000000;

   Result.dx  := FindFloatFieldInStringList(info,'X res',':');
   Result.dy  := FindFloatFieldInStringList(info,'Y res',':');

   Result.lrx  := Result.ulx + Result.dx * Result.Width;
   Result.lry  := Result.uly + Result.dy * Result.Height;

   Info.Free;

   fName2 := ChangeFileExt(fname,'.datum');
   if not FileExists(fName2) then begin
      fName := ExtractFileName(fName);

      if (fname[1] in ['N','S']) and (fName[2] = '-') and (fName[5] = '-')  then begin
         Result.LatHemi := fName[1];
         Result.UTMZone := StrToInt(Copy(fname,3,2));
         Info := tStringList.Create;
         Info.Add('WGS84');
         Info.Add(IntToStr(Result.UTMZone));
         Info.Add(Result.LatHemi);
         Info.SaveToFile(fName2);
         Info.Free;
      end;
   end;

   FindExistingWorldFile(fName);
   ReadWorldFile(MapProjection,DigitizeDatum,fName,RegVars);
   Result.SPCS_SID := FileExists(ExtractFilePath(fName + 'spcs.txt'));
   {$IfDef RecordOpenMrSid} WriteLineToDebugFile('FillMrSidInfoRec, UTM zone=' + IntToStr(Result.UTMZone)); {$EndIf}
end;


function GetMrSidInfo(fName : PathStr) : tStringList;
var
   Outname : PathStr;
begin
   {$IfDef RecordOpenMrSid} WriteLineToDebugFile('GetMrSidInfo for ' + fName); {$EndIf}
   Result := Nil;
   if StrUtils.AnsiContainsText(fName,' ') then MessageToContinue('Path/file name cannot contain spaces')
   else begin
      OutName := System.IOUtils.TPath.Combine(MDTempDir,'sid.txt');
      WinExecAndWait32(MrSidInfoName + ' ' +  fName + ' -log ' + OutName);
      {$IfDef RecordOpenMrSid} WriteLineToDebugFile('Command: ' + MrSidInfoName + ' ' +  fName + ' -log ' + OutName); {$EndIf}
      Result := tStringList.Create;
      Result.LoadFromFile(Outname);
      SysUtils.DeleteFile(Outname);
   end;
end;


procedure GetMrSidGeoLimits(fName : PathStr; var LatLow,LongLow,LatHigh,LongHigh : float64);
var
   SidInfo : tMrSidInfo;
begin
   {$IfDef RecordOpenMrSid} WriteLineToDebugFile('GetMrSidGeoLimits for ' + fName);  {$EndIf}
   SidInfo := FillMrSidInfoRec(fName);
   RedefineWGS84DatumConstants(SidInfo.UTMZone,SidInfo.LatHemi);
   WGS84DatumConstants.UTMtoLatLongDegree(SidInfo.lrx,SidInfo.uly,LatHigh,LongHigh);
   WGS84DatumConstants.UTMtoLatLongDegree(SidInfo.ulx,SidInfo.lry,LatLow,LongLow);
end;


procedure MrSidInfo(SidName : PathStr);
var
   Output : tStringList;
begin
   if GetFileFromDirectory('Mr Sid image','*.sid',SidName) then begin
      Output := GetMrSidInfo(SidName);
      Petmar.DisplayAndPurgeStringList(Output,'Mr Sid: ' + SidName);
   end;
end;



function SubsetTiffFromSID(var fname : PathStr; DisplayIt : boolean; LatHi,LongLow,LatLow,LongHi : float64; SPCS : boolean) : PathStr;
var
   SidInfo        : tMrSidInfo;
   fName2,fName3  : PathStr;
   Resolution,xul,yul,xlr,ylr : float64;
   LevelString,Command,SubsetString : ShortString;
   LevelDesired : integer;
   Options : tStringList;
   High,Wide,Memory : int64;

   procedure ShowOption;
   begin
      Memory := round(1.0 * High * Wide * SidInfo.Bands);
      Options.Add(IntToStr(LevelDesired) + RealToString(Resolution,12,2) + ' m   ' + IntToStr(Wide) + ' x ' + IntToStr(High) + '   ' + Petmar_types.SmartMemorySizeBytes(Memory));
   end;


begin
   if not MrSidEnabled then begin
      {$IfDef RecordOpenMrSid} WriteLineToDebugFile('Problem no tools for MrSidImagery');{$EndIf}
      Result := '';
      MessageToContinue('MrSid tools missing');
      DisplayHTMLTopic('html\mr_sid.htm');
      exit;
   end;

   SidInfo := FillMrSidInfoRec(fName);

   {$IfDef RecordOpenMrSid}
      WriteLineToDebugFile('SubsetTiffFromSID ' + fName);
      WriteStringListToDebugFile(GetMrSidInfo(fName));
   {$EndIf}

   Result := NextFileNumber(MDTempDir, ExtractFileNameNoExt(fName) + '_sid_','.tif');

   if (abs(LatHi) < 0.001) and (abs(LatLow) < 0.001)  then begin
      SubsetString := '';
      High := SidInfo.Height;
      Wide := SidInfo.Width;
      xul := SidInfo.ulx;
      yul := SidInfo.uly;
   end
   else begin
      if SPCS then begin
         xul := LongLow;
         yul := LatHi;
         xlr := LongHi;
         ylr := LatLow;
      end
      else begin
         {$IfDef RecordOpenMrSid} WriteLineToDebugFile('UTM zone=' + IntToStr(SidInfo.UTMZone)); {$EndIf}
         WGS84DatumConstants.DefineDatumFromUTMZone('WGS84',SidInfo.UTMZone,HemiFromLat(0.5 * (LatHi + LatLow)),'SubsetTiffFromSID');
         WGS84DatumConstants.ForwardProjectDegrees(LatHi,LongLow,xul,yul);
         WGS84DatumConstants.ForwardProjectDegrees(LatLow,LongHi,xlr,ylr);
         {$IfDef RecordMrSidLimits} WriteLineToDebugFile('NE corner ' + LatLongDegreeToString(LatHi,LongHi) + '   SW corner ' + LatLongDegreeToString(LatLow,LongLow)); {$EndIf}
       end;

      if (xul < SidInfo.ulx) then xul := SidInfo.ulx;
      if (yul > SidInfo.uly) then yul := SidInfo.uly;
      if (xlr > SidInfo.lrx) then xlr := SidInfo.lrx;
      if (ylr < SidInfo.lry) then ylr := SidInfo.lry;

      High := (round((ylr - yul) / SidInfo.dy));
      Wide := round((xlr - xul) / SidInfo.dx);

      SubsetString := ' -coord geo -ulxy ' + RealToString(xul,-18,1) + ' ' + RealToString(yul,-18,1) + ' -lrxy ' + RealToString(xlr,-18,1) + ' ' + RealToString(ylr,-18,1);
   end;

   Resolution := SidInfo.dx;
   LevelDesired := 0;
   if DisplayIt then begin
      Options := tStringList.Create;
      ShowOption;
      while ((High > MDDef.MaxMrSidImageSize) or (Wide > MDDef.MaxMrSidImageSize)) and (LevelDesired < SidInfo.SidLevels) do begin
         High := High div 2;
         Wide := Wide div 2;
         Resolution := Resolution * 2;
         inc(LevelDesired);
         ShowOption;
      end;
      if MDDef.AskAboutSIDLevel and (LevelDesired <> 0) then begin
         {$IfDef RecordOpenMrSid} WriteLineToDebugFile('SID ask user');  {$EndIf}
         Petmar.GetFromListZeroBased('Desired blowup level ' + ExtractFileName(fName),LevelDesired,Options);
         {$IfDef RecordOpenMrSid} WriteLineToDebugFile('SID replies'); {$EndIf}
      end;
      Options.Free;
   end
   else LevelDesired := SidInfo.SidLevels;

   if (LevelDesired = 0) then LevelString := ''
   else LevelString := ' -s ' + IntToStr(LevelDesired) + ' ';

   Command := MrSidDecodeName +  ' -i ' + fName + SubsetString + LevelString + ' -wf -o ' + Result;

   {$IfDef RecordOpenMrSid}
      WriteLineToDebugFile('call decoder');
      WriteLineToDebugFile('command=' + command);
      WriteLineToDebugFile('SubsetString=' + SubsetString);
      WriteLineToDebugFile('LevelString=' + LevelString);
   {$EndIf}

   WinExecAndWait32(Command);

   {$IfDef RecordOpenMrSid} WriteLineToDebugFile('decoded'); {$EndIf}

   if FileExists(ChangeFileExt(fName,'.datum')) then CopyFile(ChangeFileExt(fName,'.datum'),ChangeFileExt(Result,'.datum'));
   if FileExists(ChangeFileExt(fName,'.all')) then CopyFile(ChangeFileExt(fName,'.all'),ChangeFileExt(Result,'.datum'));
   if FileExists(ChangeFileExt(fName,'.prj')) then CopyFile(ChangeFileExt(fName,'.prj'),ChangeFileExt(Result,'.prj'));

   fName2 := Result;
   GetDefaultWorldFile(fname2);
   Options := tStringList.Create;
   Options.LoadFromFile(fname2);
   Options.Strings[4] := FloatToStr(xul);
   Options.Strings[5] := FloatToStr(yul);
   Options.SaveToFile(fname2);
   Options.Free;

   fName3 := MDTempDir + 'spcs.txt';
   SysUtils.DeleteFile(fName3);
   fName2 := ExtractFilePath(fname) + 'spcs.txt';
   if FileExists(fName2) then CopyFile(fName2,fName3);
   {$IfDef RecordOpenMrSid} WriteLineToDebugFile('SubsetTiffFromSID out'); {$EndIf}
end;


procedure ExtractTiffFromSID(var fname : PathStr);
begin
   {$IfDef RecordOpenMrSid} WriteLineToDebugFile('ExtractTiffFromSID ' + fname); {$EndIf}
   fName := SubsetTiffFromSID(fname,true,0,0,0,0,false);
   {$IfDef RecordOpenMrSid} WriteLineToDebugFile('Extract complete'); {$EndIf}
end;

initialization
finalization
   {$IfDef RecordOpenMrSid} WriteLineToDebugFile('RecordOpenMrSid active in MrSidImagery'); {$EndIf}
   {$IfDef RecordMrSidLimits} WriteLineToDebugFile('RecordMrSidLimits active in MrSidImagery'); {$EndIf}
end.



