
{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}
{                                   }
{ include file for petmar_types     }
{___________________________________}



function CPUname : string;
//http://delphi.cjcsoft.net/viewthread.php?tid=46937
var
  Reg: TRegistry;
begin
  {$IfDef VCL}
    CPUname := '';
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKeyReadOnly('Hardware\Description\System\CentralProcessor\0') then
        CPUname := Reg.ReadString('Identifier');
    finally
      Reg.Free;
    end;
  {$EndIf}
end;


function GetCpuSpeed: string;
//http://delphi.cjcsoft.net/viewthread.php?tid=46937
var
  Reg: TRegistry;
begin
  {$IfDef VCL}
     Reg := TRegistry.Create;
     try
       Reg.RootKey := HKEY_LOCAL_MACHINE;
       if Reg.OpenKeyReadOnly('HARDWARE\DESCRIPTION\System\CentralProcessor\0') then begin
         Result := 'CPU Speed: ' + IntToStr(Reg.ReadInteger('~MHz')) + ' MHz';
         Reg.CloseKey;
       end
       else Result := '';
     finally
       Reg.Free;
     end;
  {$EndIf}
end;


function GetScreenColorDepth : integer;
var
   ScreenDC : HDC;
begin
   ScreenDC := GetDC(0);
   Result := GetDeviceCaps(ScreenDC,BitsPixel);
   ReleaseDC(0,ScreenDC);
end;


function GetComputerNetName: AnsiString;
var
  buffer : array[0..255] of Char;
  size   : dword;
begin
  size := 256;
  if GetComputerName(buffer, size) then Result := buffer
  else Result := '';
end;


function GetNumberProcessors : integer;
var
   SysInfo : TSystemInfo;
begin
  GetSystemInfo(SysInfo);
  Result := SysInfo.dwNumberOfProcessors;
end;


function SystemMemorySize : int64;
var
   vMem : tMemoryStatusEx;
begin
   vMem.dwLength := SizeOf(tMemoryStatusEx);
   GlobalMemoryStatusEx(vMem);
   Result := vMem.ullTotalPhys;
end;


function PhysicalMemoryFree : int64;
var
   vMem : tMemoryStatusEx;
begin
   vMem.dwLength := SizeOf(tMemoryStatus);
   GlobalMemoryStatusEx(vMem);
   Result := vMem.ullAvailPhys;
end;


function IDMonitor(theForm : Forms.tForm) : Integer;
var
   i,x,y,Top,Left,Right,Bottom : integer;
begin
   x := theForm.Left;
   y := theForm.Top;
   {$IfDef RecordMonitorProblems} WriteLineToDebugFile('form at ' + IntToStr(x) + 'x' + IntToStr(y)); {$EndIf}
   Result := -99;
   for i := 0 to pred(Screen.MonitorCount) do begin
       Top := Screen.Monitors[i].Top;
       Left := Screen.Monitors[i].Left;
       Right := Screen.Monitors[i].Left + Screen.Monitors[i].Width;
       Bottom := Screen.Monitors[i].Top + Screen.Monitors[i].Height;
      {$IfDef RecordMonitorProblems} WriteLineToDebugFile('Monitor ' + IntToStr(succ(i)) + '  x=' + IntToStr(Left) + ' to ' + IntToStr(Right) + '   y=' +IntToStr(Top) + ' to ' + IntToStr(Bottom)); {$EndIf}
       if (x >= Left) and (x <= Right) and (y >= Top) and (y <= Bottom) then Result := i;
   end;
   {$IfDef RecordMonitorProblems}  WriteLineToDebugFile('form on Monitor ' + IntToStr(succ(Result))); {$EndIf}
end;


function HardwareString(IncludeIP : boolean = true) : AnsiString;
var
   MemStr,TStr,TStr2,TStr3,TStr4  : ShortString;
   i : integer;
   ch : ansichar;
   OSVersion : tOSVersion;
begin
   MemStr := OSversion.ToString +  MessLineBreak + 'Total Physical Memory: ' + SmartMemorySizeBytes(SystemMemorySize) +  MessLineBreak;
   TStr := CPUName + MessLineBreak + GetCPUSpeed + MessLineBreak + 'Processors: ' + IntToStr(GetNumberProcessors) + MessLineBreak;
   TStr2 := 'Monitors: ' + IntToStr(Screen.MonitorCount) + '  Screen Resolution, Colors: ' + IntToStr(GetScreenColorDepth)+ ' bits' + MessLineBreak;
   for i := 0 to pred(Screen.MonitorCount) do begin
       TStr2 := Tstr2 + 'Monitor ' + IntToStr(succ(i)) + ': ' + IntToStr(Screen.Monitors[i].Width) +  'x' + IntToStr(Screen.Monitors[i].Height) + '   ' +
          IntToStr(Screen.Monitors[i].PixelsPerInch) + ' dpi';
       if (Screen.MonitorCount > 0) then  TStr2 := Tstr2 + ' Left=' + IntToStr(Screen.Monitors[i].Left) + '  top=' + IntToStr(Screen.Monitors[i].Top);
       TStr2 := Tstr2 + MessLineBreak;
   end;
   TStr2 := TStr2 + MessLineBreak + 'Video card: ' + GetVideoCardName + MessLineBreak;

   try
     if (Printer.Printers.Count = 0) then TStr3 := 'No default printer'
     else TStr3 := 'Printer resolution: ' +  IntToStr(GetDeviceCaps(Printer.Handle,HorzRes)) + 'x' + IntToStr(GetDeviceCaps(Printer.Handle,VertRes)) + ' at ' + IntToStr(GetDeviceCaps(Printer.Handle,LogPixelsY)) + ' dpi';
   except
      TStr3 := 'Print detection error';
   end;
   TStr3 := TStr3 + MessLineBreak;
   ch := UpCase(ProgramRootDir[1]);
   TStr4 := 'Computer name: ' + GetComputerNetName + MessLineBreak + 'Free disk space on ' + ch + ': ' + SmartMemorySizeBytes(DiskFree(ord(ch) - ord('A') + 1)) + MessLineBreak;
   Result := MemStr + TStr + TStr2 + TStr3 + TStr4;
end;


function GetVideoCardName(): String;
// https://stackoverflow.com/questions/12966946/how-to-get-the-installed-video-card-delphi
const
  WbemUser = '';
  WbemPassword = '';
  WbemComputer = 'localhost';
  wbemFlagForwardOnly = $00000020;
var
  FSWbemLocator: OLEVariant;
  FWMIService: OLEVariant;
  FWbemObjectSet: OLEVariant;
  FWbemObject: OLEVariant;
  oEnum: IEnumvariant;
  iValue: LongWord;
begin
  Result := 'not available';
  try
    FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
    FWMIService := FSWbemLocator.ConnectServer(WbemComputer, 'root\CIMV2', WbemUser, WbemPassword);
    FWbemObjectSet := FWMIService.ExecQuery('SELECT Name,PNPDeviceID  FROM Win32_VideoController', 'WQL', wbemFlagForwardOnly);
    oEnum := IUnknown(FWbemObjectSet._NewEnum) as IEnumvariant;
    while oEnum.Next(1, FWbemObject, iValue) = 0 do begin
      result := String(FWbemObject.Name);
      FWbemObject := Unassigned;
    end;
  except
    on E: Exception do begin end;
  end;
end;



(*
  Unit allows Delphi application's Version Information to be dynamically  retrieved at run time; thus removing the need to manually update version
     labels on splash screens and about boxes.

  Original code sourced from the Borland Delphi IDE newsgroup (author unknown).  A.Tippett, 11th January 2000

  Modified 20th March 2000 to add the GetBuildInfo procedure, allowing the major/minor/release/build version information to be retrieved individually.
*)


type
  PTransBuffer = ^TTransBuffer;
  TTransBuffer = array[1..13] of smallint;

const
  CInfoStr : array[1..13] of AnsiString =
    ('FileVersion', 'CompanyName', 'FileDescription', 'InternalName', 'LegalCopyright', 'LegalTradeMarks', 'OriginalFileName', 'ProductName', 'ProductVersion', 'Comments', 'CurrentProgramVersion',
       'CurrentDatabaseVersion','VersionDetails');


procedure GetProjectVersionInfo(AVersionList: TStrings);
{ Returns ALL of version information as separate entries of a TString list. Each element accessed by indexing the TString list thus: AVersionList[0], AVersionList[1] etc..}
var
  filename: AnsiString;
  i:        integer;
  infoSize: DWORD;
  ptrans:   PTransBuffer;
  transStr: AnsiString;
  typeStr:  AnsiString;
  value:    pANSIchar;
  verBuf:   pointer;
  verSize:  DWORD;
  wnd:      DWORD;
begin
  AVersionList.Clear;
  filename := Application.ExeName;
  infoSize := GetFileVersioninfoSize(pchar(filename), wnd);

  if (infoSize <> 0) then begin
    GetMem(verBuf, infoSize);
    try
      if GetFileVersionInfo(pchar(filename), wnd, infoSize, verBuf) then begin
        VerQueryValue(verBuf, pchar('\VarFileInfo\Translation'),Pointer(ptrans), verSize);
        transStr := IntToHex(ptrans^[1], 4) + IntToHex(ptrans^[2], 4);
        for i := Low(CInfoStr) to High(CInfoStr) do begin
          typeStr := 'StringFileInfo\' + transStr + '\' + CInfoStr[i];
          if VerQueryvalue(verBuf, pchar(typeStr), Pointer(value), verSize) then
            AVersionList.Add(value);
        end
      end;
    finally
      FreeMem(verBuf);
    end;
  end;
end;


procedure GetBuildInfo(var V1, V2, V3, V4: Word);
{returns Major/Minor/Release/Build values of version information.}
var
  VerInfoSize:  DWORD;
  VerInfo:      Pointer;
  VerValueSize: DWORD;
  VerValue:     PVSFixedFileInfo;
  Dummy:        DWORD;
begin
   VerInfoSize := GetFileVersionInfoSize(pchar(ParamStr(0)), Dummy);
   GetMem(VerInfo, VerInfoSize);
   GetFileVersionInfo(pchar(ParamStr(0)), 0, VerInfoSize, VerInfo);
   VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
   with VerValue^ do begin
       V1 := dwFileVersionMS shr 16;
       V2 := dwFileVersionMS and $FFFF;
       V3 := dwFileVersionLS shr 16;
       V4 := dwFileVersionLS and $FFFF;
   end;
   FreeMem(VerInfo, VerInfoSize);
end;


function BuildString : shortString;
var
   v1,v2,v3,v4 : word;
begin
   GetBuildInfo(V1, V2, V3, V4);
   Result := IntToStr(v1) + '.' + IntToStr(v2) + '.' + IntToStr(v3) + '.' + IntToStr(v4);
end;




