unit petmar_ini_file;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      //{$Define RecordINIfiles}
      //{$Define DontWriteIniFile}
   {$EndIf}
{$EndIf}


interface


uses
   sysutils,
   iniFiles,petmar,petmar_types;

type
   tIniWhat = (iniRead,IniWrite,iniInit);
   {$IfDef MSWindows}
      tMyIniFile = tMemIniFile;
   {$Else}
      tMyIniFile = tIniFile;
   {$EndIf}

   tMDiniFile = class
      private
         function RestoreDefault(WhichSection : ShortString) : boolean;   inline;
      public
         IniFile : tMyIniFile;
         iniWhat : tIniWhat;
         SectionDefaultsToRestore : ShortString;
         constructor OpenMDiniFile(DesirediniWhat : tIniWhat; SingleSection : ShortString = ''; ExplicitName : PathStr = '');
         destructor CloseMDiniFile;
         procedure ACharacter(Section,Keyword : shortString; var NewValue : ANSIChar; ParamDefValue : ShortString);
         procedure AParameter(Section,Keyword : shortString; var NewValue : int16; ParamDefValue : int16); overload;
         procedure AParameter(Section,Keyword : shortString; var NewValue : int32; ParamDefValue : int32); overload;
         procedure AParameter(Section,Keyword : shortString; var NewValue : int64; ParamDefValue : int64); overload;
         procedure AParameter(Section,Keyword : shortString; var NewValue : word; ParamDefValue : word); overload;
         procedure AColorParameter(Section,Keyword : shortString; var NewValue : tPlatformColor; ParamDefValue : tPlatformColor);  overload;
         procedure AParameter(Section,Keyword : shortString; var NewValue : byte; ParamDefValue : byte);  overload;
         procedure AParameter(Section,Keyword : shortString; var NewValue : boolean; ParamDefValue : boolean); overload;
         procedure AParameter(Section,Keyword : shortString; var NewValue : ShortString; ParamDefValue : ShortString); overload;
         procedure AParameterFloat(Section,Keyword : shortString; var NewFloatValue : float64; ParamDefValue : float64);
         procedure AParameterShortFloat(Section,Keyword : shortString; var NewFloatValue : float32; ParamDefValue : float32);
         procedure InitializeMyFont(KeyWord: ShortString; var aFont: tMyFont; Name: shortstring; Size: SmallInt; Color: tPlatformColor);
         procedure ASymbol(Section,Keyword : shortString; var Sym : tFullSymbolDeclaration; DefSym : tDrawingSymbol; Color : tPlatformColor; Size : integer);
   end;


implementation


constructor tMDiniFile.OpenMDiniFile(DesirediniWhat : tIniWhat; SingleSection: ShortString = ''; ExplicitName : PathStr = '');
var
   fName : PathStr;
begin
   if (ExplicitName <> '') then fName := ExplicitName
   else fName := IniFileName;
   {$IfDef RecordINIfiles} if FileExists(fname) then WriteLineToDebugFile('open INI file: ' + fName) else WriteLineToDebugFile('Missing ini file: ' + fName); {$EndIf}
   {$IfDef DontWriteIniFile}
   {$Else}
      IniFile := TMyIniFile.Create(fName);
   {$EndIf}
   SectionDefaultsToRestore := SingleSection;
   iniWhat := DesirediniWhat;
   {$IfDef RecordINIfiles} WriteLineToDebugFile('opened ok'); {$EndIf}
end;


function tMDiniFile.RestoreDefault(WhichSection : ShortString) : boolean;
begin
   Result := (iniWhat = iniInit) and ((SectionDefaultsToRestore = '') or (UpperCase(SectionDefaultsToRestore) = UpperCase(WhichSection)));
end;

procedure tMDiniFile.AParameter(Section,Keyword : shortString; var NewValue : int16; ParamDefValue : int16);
begin
   if (IniWhat = iniRead) then NewValue := IniFile.ReadInteger(Section,KeyWord,ParamDefValue);
   if (iniWhat = iniWrite) then IniFile.WriteInteger(Section,KeyWord,NewValue);
   if (iniWhat = iniInit) and RestoreDefault(Section) then NewValue := ParamDefValue;
end;

procedure tMDiniFile.AParameter(Section,Keyword : shortString; var NewValue : int32; ParamDefValue : int32);
begin
   if (IniWhat = iniRead) then NewValue := IniFile.ReadInteger(Section,KeyWord,ParamDefValue);
   if (iniWhat = iniWrite) then IniFile.WriteInteger(Section,KeyWord,NewValue);
   if (iniWhat = iniInit) and RestoreDefault(Section) then NewValue := ParamDefValue;
end;

procedure tMDiniFile.AParameter(Section,Keyword : shortString; var NewValue : int64; ParamDefValue : int64);
begin
   if (IniWhat = iniRead) then NewValue := IniFile.ReadInteger(Section,KeyWord,ParamDefValue);
   if (iniWhat = iniWrite) then IniFile.WriteInteger(Section,KeyWord,NewValue);
   if (iniWhat = iniInit) and RestoreDefault(Section) then NewValue := ParamDefValue;
end;

procedure tMDiniFile.AParameter(Section,Keyword : shortString; var NewValue : word; ParamDefValue : word);
begin
   if (IniWhat = iniRead) then NewValue := IniFile.ReadInteger(Section,KeyWord,ParamDefValue);
   if (iniWhat = iniWrite) then IniFile.WriteInteger(Section,KeyWord,NewValue);
   if (iniWhat = iniInit) and RestoreDefault(Section) then NewValue := ParamDefValue;
end;


procedure tMDiniFile.AColorParameter(Section,Keyword : shortString; var NewValue : tPlatFormColor; ParamDefValue : tPlatFormColor);
begin
   if (IniWhat = iniRead) then begin
      NewValue := ConvertTColorToPlatformColor(IniFile.ReadInteger(Section,KeyWord,ConverTPlatFormColorToTColor(ParamDefValue)));
   end;
   if (iniWhat = iniWrite) then IniFile.WriteInteger(Section,KeyWord,ConverTPlatFormColorToTColor(NewValue));
   if (iniWhat = iniInit) and RestoreDefault(Section) then NewValue := ParamDefValue;
end;


procedure tMDiniFile.AParameter(Section,Keyword : shortString; var NewValue : byte; ParamDefValue : byte);
begin
   if (IniWhat = iniRead) then NewValue := IniFile.ReadInteger(Section,KeyWord,ParamDefValue);
   if (iniWhat = iniWrite) then IniFile.WriteInteger(Section,KeyWord,NewValue);
   if (iniWhat = iniInit) and RestoreDefault(Section) then NewValue := ParamDefValue;
end;


destructor tMDiniFile.CloseMDiniFile;
begin
   {$If Defined(Android) or Defined(MacOS)}
   {$Else}
      IniFile.UpdateFile;
      IniFile.Free;
   {$EndIf}
end;


procedure tMDiniFile.AParameter(Section,Keyword : shortString; var NewValue : boolean; ParamDefValue : boolean);
begin
    if (IniWhat = iniWrite) then IniFile.WriteBool(Section,KeyWord,NewValue);
    if (IniWhat = iniRead) then NewValue := IniFile.ReadBool(Section,KeyWord,ParamDefValue);
    if (iniWhat = iniInit) and RestoreDefault(Section) then NewValue := ParamDefValue;
end;

procedure tMDiniFile.ACharacter(Section,Keyword : shortString; var NewValue : ANSIChar; ParamDefValue : ShortString);
var
   TStr : shortString;
begin
    if (IniWhat = iniWrite) then IniFile.WriteString(Section,KeyWord,NewValue);
    if (IniWhat = iniRead) then begin
       TStr := IniFile.ReadString(Section,KeyWord,ParamDefValue);
       if TStr = '' then NewValue := ParamDefValue[1]
       else NewValue := TStr[1];
    end;
    if (iniWhat = iniInit) and RestoreDefault(Section) then NewValue := ParamDefValue[1];
end;


procedure tMDiniFile.AParameter(Section,Keyword : shortString; var NewValue : ShortString; ParamDefValue : ShortString);
var
   TStr : ANSIString;
begin
    if (IniWhat = iniWrite) then IniFile.WriteString(Section,KeyWord,NewValue);
    if (IniWhat = iniRead) then begin
       TStr := IniFile.ReadString(Section,KeyWord,ParamDefValue);
       if (TStr = '') then NewValue := ParamDefValue
       else begin
          if length(TStr) > 255 then begin
             MessageToContinue('Too long for ' + Keyword + '  ' + TStr);
             NewValue := ParamDefValue;
          end
          else NewValue := TStr;
       end;
    end;
    if (iniWhat = iniInit) and RestoreDefault(Section) then NewValue := ParamDefValue;
end;

procedure tMDiniFile.AParameterFloat(Section,Keyword : shortString; var NewFloatValue : float64; ParamDefValue : float64);
begin
    {$IfDef VCL}
       if (IniWhat = iniWrite) then IniFile.WriteFloat(Section,KeyWord,NewFloatValue);
       if (iniWhat = iniInit) and ((SectionDefaultsToRestore = '') or (SectionDefaultsToRestore = Section)) then NewFloatValue := ParamDefValue;
       if (IniWhat = iniRead) then NewFloatValue := IniFile.ReadFloat(Section,KeyWord,ParamDefValue);
    {$EndIf}
end;


procedure tMDiniFile.AParameterShortFloat(Section,Keyword : shortString; var NewFloatValue : float32; ParamDefValue : float32);
begin
    {$IfDef VCL}
       if (IniWhat = iniWrite) then IniFile.WriteFloat(Section,KeyWord,NewFloatValue);
       if (iniWhat = iniInit) and ((SectionDefaultsToRestore = '') or (SectionDefaultsToRestore = Section)) then NewFloatValue := ParamDefValue;
       if (IniWhat = iniRead) then NewFloatValue := IniFile.ReadFloat(Section,KeyWord,ParamDefValue);
    {$EndIf}

    {$IfDef FMX}
       if (iniWhat in [iniInit,iniRead]) and ((SectionDefaultsToRestore = '') or (SectionDefaultsToRestore = Section)) then NewFloatValue := ParamDefValue;
    {$EndIf}
end;

procedure tMDiniFile.ASymbol(Section,Keyword : shortString; var Sym : tFullSymbolDeclaration; DefSym : tDrawingSymbol; Color : tPlatformColor; Size : integer);
begin
    if (IniWhat = iniWrite) then begin
       IniFile.WriteInteger(Section,KeyWord+'.color',ConvertPlatformColorToTColor(Sym.Color));
       IniFile.WriteInteger(Section,KeyWord+'.size',Sym.Size);
       IniFile.WriteInteger(Section,KeyWord+'.sym',ord(Sym.DrawingSymbol));
    end;
    if (IniWhat = iniRead) then begin
       Sym.Color := ConvertTColorToPlatformColor(IniFile.ReadInteger(Section,KeyWord+'.color',ConvertPlatformColorToTColor(Color)));
       Sym.Size := IniFile.ReadInteger(Section,KeyWord+'.size',Size);
       Sym.DrawingSymbol := tDrawingSymbol(IniFile.ReadInteger(Section,KeyWord+'.sym',ord(DefSym)));
    end;
    if (iniWhat = iniInit) and RestoreDefault(Section) then begin
       Sym.Color := Color;
       Sym.Size := Size;
       Sym.DrawingSymbol := DefSym;
    end;
end;


procedure tMDiniFile.InitializeMyFont(KeyWord : ShortString; var aFont : tMyFont; Name : shortstring; Size : SmallInt; Color : tPlatformColor);
begin
    if (IniWhat = iniWrite) then begin
       IniFile.WriteString('Fonts',KeyWord+'.name',aFont.Name);
       IniFile.WriteInteger('Fonts',KeyWord+'.color',ConvertPlatformColorToTColor(aFont.Color));
       IniFile.WriteInteger('Fonts',KeyWord+'.size',aFont.Size);
    end;
    if (IniWhat = iniRead) then begin
       aFont.Name := IniFile.ReadString('Fonts',KeyWord+'.name',Name);
       aFont.Color := ConvertTColorToPlatformColor(IniFile.ReadInteger('Fonts',KeyWord+'.color',ConvertPlatformColorToTColor(Color)));
       aFont.Size := IniFile.ReadInteger('Fonts',KeyWord+'.size',Size);
    end;
    if (iniWhat = iniInit) then begin
       aFont.Name := Name;
       aFont.Color := Color;
       aFont.Size := Size;
    end;
end;



end.
