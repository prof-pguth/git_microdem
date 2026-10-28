{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{   include file for petmar.pas          }
{________________________________________}

//this intended to remove any routines that do not work with FireMonkey
//some of these might work in Windows FireMonkey


function AvailablePhysicalMemoryString : shortstring;
var
  vGlobalMemoryStatus : TMemoryStatus;
begin
  GlobalMemoryStatus(vGlobalMemoryStatus );
  Result := ' Available RAM: ' + SmartMemorySizeBytes(vGlobalMemoryStatus.dwAvailPhys);
end;


procedure DragDirections;
begin
   MessageToContinue('Pick NW corner; drag to SE corner' + MessLineBreak + ' with mouse button depressed.');
end;


function GetDOSPath(TheMessage : AnsiString; var Path : PathStr) : boolean;
begin
   {$IfDef VCL}
      with TFileOpenDialog.Create(nil) do
        try
          Title := 'Folder/directory for ' + TheMessage;
          Options := [fdoPickFolders];
          FileName := LastSubDir(Path);
          DefaultFolder := GetParentDirectory(Path);
          if Execute then begin
             Path := FileName;
             if Path[length(Path)] <> '\' then Path := FileName + '\';
          end
          else Path := '';
          Result := Path <> '';
        finally
          Free;
        end;
   {$EndIf}
end;



function FindPath(What,NeededName : shortstring; var TheDir : PathStr) : boolean;
var
   ch : ansichar;
   Locations : tStringList;
   Dir : PathStr;
   PickedNum : integer;

   function FromDriveLetter(ch : ansichar) : Pathstr;
   begin
      Result := ch + NeededName;
   end;

begin
   Locations := tStringList.Create;
   TheDir := '';
   Result := false;
   for ch := 'A' to 'Z' do begin
      Dir := FromDriveLetter(ch);
      if ValidPath(dir) then Locations.Add(Dir + '  (' + GetVolumeName(ch) + ')');
   end;
   if (Locations.Count = 0) then begin
      {$IfDef WarnMapIndexes} MessageToContinue('No map indexes found'); {$EndIf}
   end
   else if (Locations.Count = 1) then begin
      Dir := Locations.Strings[0];
      TheDir := FromDriveLetter(Dir[1]);
      Result := true;
   end
   else begin
      {$IfDef VCL}
          if MultiSelectSingleColumnStringList(What,PickedNum,Locations,true) then begin
             Dir := Locations.Strings[PickedNum];
             TheDir := FromDriveLetter(Dir[1]);
             Result := true;
          end;
      {$EndIf}
      exit;
   end;
   Locations.Destroy;
end;



function GetMultipleDirectories(TheMessage : AnsiString; var Paths : tStringList) : boolean;
var
   fod : TFileOpenDialog;
   i : integer;
begin
   {$IfDef VCL}
      if (Paths.Count = 0) then Paths.Add(MainMapData);

      fod := TFileOpenDialog.Create(nil);
      try
         fod.Title := 'Folders/directories for ' + TheMessage;
         fod.Options := [fdoPickFolders,fdoAllowMultiSelect];
         fod.FileName := LastSubDir(Paths[0]);
         fod.DefaultFolder := GetParentDirectory(Paths[0]);
         Paths.Clear;
         if fod.Execute then begin
            for i := 0 to pred(fod.Files.Count) do begin
               Paths.Add(fod.Files[i] + '\');
            end;
         end;
         Result := (Paths.Count > 0);
     finally
       fod.Free;
     end;
   {$EndIf}
end;


procedure DownloadListOfFiles(Memo1 : tMemo = nil);
var
   FileList,FilesWanted : tStringList;
   NumSucc,NumFail, i,{j,}f,runs : Integer;
   Input,OutPath : PathStr;
   TStr,inName,OutName : AnsiString;
   DefFilter : byte;

       function FileAlreadyDownLoaded(fName : PathStr) : boolean;
       //might already have extracted from laz to las, or reprojected las
       begin
          Result := FileExists(fName) or FileExists(ChangeFileExt(FName,'.las')) or FileExists(ChangeFileExt(FName,'_utm.las'));
       end;

begin
   try
      WMdem.Color := clInactiveCaption;
      inPut := '';
      DefFilter := 1;
      FilesWanted := tStringList.Create;
      FilesWanted.Add(System.IOUtils.TPath.GetDownloadsPath);
      if GetMultipleFiles(' to download','Text or csv*.txt;*.csv',FilesWanted,DefFilter) then begin
         for f := 0 to pred(FilesWanted.Count) do begin
            Input := FilesWanted.Strings[f];
            if FileExists(Input) then begin
               TStr := IntToStr(succ(f)) + '/' + IntToStr(FilesWanted.Count);
               wmdem.SetPanelText(1,'List: ' + TStr,true);
               Runs := 1;
               FileList := tStringList.Create;
               FileList.LoadFromFile(Input);
               OutPath := ExtractFilePath(Input) + ExtractFileNameNoExt(Input) + '\';
               SafeMakeDir(OutPath);
               if Memo1 <> Nil then begin
                   Memo1.Visible := true;
                   Memo1.Lines.Add(TimeToStr(Now) + ' Download list ' + IntToStr(succ(f)) + '/' + IntToStr(FilesWanted.Count) + '  ' + ExtractFileNameNoExt(Input));
                   Memo1.Lines.Add('');
                   Memo1.Lines.Add(TimeToStr(Now) + ' Check for files already downloaded, files= ' + IntToStr(FileList.Count));
               end;
               TStr := 'Started: ' + TimeToStr(Now);
               wmdem.SetPanelText(0,TStr,true);
               repeat
                 NumSucc := 0;
                 NumFail := 0;
                 for i := pred(FileList.Count) downto 0 do begin
                    InName := FileList.Strings[i];
                    if FileAlreadyDownloaded(DEMdef_routines.TheOutputName(OutPath,InName,false)) then begin
                       FileList.Delete(i);
                    end;
                 end;
                if Memo1 <> Nil then Memo1.Lines.Add(TimeToStr(Now) + ' start download, files= ' + IntToStr(FileList.Count));

                 for i := 0 to pred(FileList.Count) do begin
                    TStr := IntToStr(succ(i)) + '/' + IntToStr(FileList.Count) + '  (' + RealToString(100*succ(i)/ FileList.Count,-8,1) + '%)';
                    wmdem.SetPanelText(2,TStr);
                    InName := FileList.Strings[i];
                    if (InName <> '') then begin
                       OutName := TheOutputName(OutPath,InName,false);
                       if FileAlreadyDownloaded(OutName) then begin
                       end
                       else begin
                          TStr := ExtractFileNameNoExt(OutName);
                          if DownloadFileFromWeb(InName,OutName,false) then begin
                             TStr := TStr + '  ' + SmartMemorySizeBytes(GetFileSize(OutName)) +  ' success';
                             inc(NumSucc);
                          end
                          else begin
                             TStr := '***** failure ' + TStr + ' failure *****';
                             inc(NumFail);
                          end;
                          if Memo1 <> Nil then Memo1.Lines.Add(IntToStr(succ(I)) + '/' + IntToStr(FileList.Count) + '  ' + TimeToStr(Now) + ' ' + TStr);
                       end;
                    end;
                    if WantOut then begin
                       if Memo1 <> Nil then Memo1.Lines.Add('Aborted');
                       break;
                    end;
                 end;
                 if Memo1 <> Nil then  begin
                     Memo1.Lines.Add('Done; downloads=' + intToStr(NumSucc) + '  failures=' + IntToStr(NumFail));
                     Memo1.Lines.Add('');
                     Memo1.Lines.Add('');
                 end;
                 if (NumFail = 0) then DeleteFileIfExists(Input);
                 inc(Runs);
               until Wantout or (NumFail = 0) or (Runs > 5);
               FileList.Free;
            end;
         end;
      end;
      FilesWanted.Free;
   finally
      WMdem.Color := clScrollBar;
      wmdem.ClearStatusBarPanelText;
   end;
end;


         procedure ShowInNotepadPlusPlus(inName : tStringList; Capt : shortstring = '');
         var
             fName : PathStr;
         begin
            fName := MDTempDir + ExtractFileNameNoExt(Capt) + '.txt';
            inName.SaveToFile(fName);
            inName.Free;
            ShowInNotepadPlusPlus(fName);
         end;


         procedure ShowInNotepadPlusPlus(fName : PathStr; Capt : shortstring = '');
         var
            ppName,Params,DefaultDir : ANSIstring;
         begin
            ppName := 'C:\Program Files\Notepad++\notepad++.exe';
            if (not FileExists(ppName)) then ppName := 'C:\Program Files (x86)\Notepad++\notepad++.exe';
            if FileExists(ppName) then begin
               ppName := '"' + ppName + '"';
               Params := fName;
               DefaultDir := '';
               ExecuteFile(ppName, Params, DefaultDir);
            end
            else begin
               if (Capt = '') then Capt := fname;
               QuickOpenEditWindow(FName,Capt);
            end;
         end;
		 
		 
function File2Trash(const FileName: string): boolean;
var
    fos: TSHFileOpStruct;
begin
   FillChar(fos, SizeOf(fos), 0);
   with fos do begin
     wFunc  := FO_DELETE;
     pFrom  := PChar(ExpandFileName(FileName)+#0#0);
     fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION or FOF_SILENT or FOF_NOERRORUI;
   end;
   Result := (0 = ShFileOperation(fos));
end;


         procedure ColorLineWidthBitmap(var Bitmap : tMyBitmap; Color : TPlatformColor; Size : integer; Minimize : boolean = true);
         begin
            Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(Color);
            Bitmap.Canvas.Pen.Width := Size;
            DrawLine(Bitmap,1,Bitmap.height div 2, Bitmap.Width- 1,Bitmap.height div 2);
            if Minimize then PetImage.GetImagePartOfBitmap(Bitmap);
         end;


         procedure ColorLineWidthBitBtn(var BitBtn : tBitBtn; Color : tPlatformColor; Size : integer);
         var
            Bitmap : tMyBitmap;
         begin
            CreateBitmap(Bitmap,20,BitBtn.Height - 4);
            ColorLineWidthBitmap(Bitmap,Color,Size);
            BitBtn.Glyph := Bitmap;
            Bitmap.Free;
         end;


         procedure ColorBitBtn(var BitBtn : tBitBtn; Color : tPlatformColor);
         begin
            ColorBitBtn(BitBtn,ConvertPlatformColorToTColor(Color));
         end;

         procedure ColorBitBtn(var BitBtn : tBitBtn; Color : tColor);
         var
            Bitmap : tMyBitmap;
         begin
            CreateBitmap(Bitmap,20,20);
            Bitmap.Canvas.Pen.Color := Color;
            Bitmap.Canvas.Brush.Color := Color;
            Bitmap.Canvas.Rectangle(1,1,18,18);
            BitBtn.Glyph := Bitmap;
            Bitmap.Free;
         end;


         procedure SymbolOnButton(var BitBtn : tBitBtn; Symbol : tDrawingSymbol; SymbolSize : integer; SymbolColor : tPlatformColor);
         var
            aBitmap : tMyBitmap;
         begin
            CreateBitmap(aBitmap,20,20);
            ScreenSymbol(aBitmap.Canvas,10,10,Symbol,SymbolSize,SymbolColor);
            PetImage.GetImagePartOfBitmap(aBitmap);
            BitBtn.Glyph := aBitmap;
            FreeAndNil(aBitmap);
         end;

         procedure SymbolOnButton(var BitBtn : tBitBtn; Symbol : tFullSymbolDeclaration);
         begin
            SymbolOnButton(BitBtn,Symbol.DrawingSymbol,Symbol.Size,Symbol.Color);
         end;


         function FontInstalled(Const FontName:String) : Boolean;
         Begin
            Result := Screen.Fonts.IndexOf(FontName)>=0
         End;


         function GetFromList(InMessage : ANSIstring; var PickedNum : integer; InList : TStringList; CanCancel : boolean = false) : boolean;
         begin
            PickedNum := pred(PickedNum);
            Result := MultiSelectSingleColumnStringList(InMessage,PickedNum,InList,CanCancel);
         end;

         function GetFromList(InMessage : ANSIstring; InList : TStringList; CanCancel : boolean = false) : ShortString;
         var
           PickedNum : integer;
         begin
            PickedNum := 1;
            if MultiSelectSingleColumnStringList(InMessage,PickedNum,InList,CanCancel) then Result := InList.Strings[PickedNum]
            else Result := '';
         end;


         function GetMultipleFromList(InMessage : ANSIstring; var PickedNum : integer; var InList : TStringList; CanCancel : boolean = false) : boolean;
         begin
            {$IfDef RecordListPick} WriteLineToDebugFile('GetMultipleFromList in, choices=' + IntToStr(InList.Count)); {$EndIf}
            PickedNum := pred(PickedNum);
            Result := MultiSelectSingleColumnStringList(InMessage,PickedNum,InList,CanCancel,true);
            {$IfDef RecordListPick} WriteLineToDebugFile('GetMultipleFromList out, choices=' + IntToStr(InList.Count)); {$EndIf}
         end;


         function MultiSelectSingleColumnStringList(InMessage : ANSIstring; var PickedNum : integer; var InList : TStringList; CanCancel : boolean = false; MultiPick : boolean = false) : boolean;
         {$IfDef FMX}
         begin
            Result := false;
         {$EndIf}
         {$IfDef VCL}
         var
           PetList : TPetList;
           i : integer;
         begin
            if (InList.Count = 0) then Result := false
            else begin
                PickedNum := 0;
                PetList := TPetList.Create(Application);
                PetList.Caption := InMessage + '  (' + IntToStr(InList.Count) + ' choices)';
                PetList.ListBox1.ItemIndex := PickedNum;
                for i := 0 to pred(Inlist.Count) do PetList.ListBox1.Items.Add(InList.Strings[i]);
                PetList.CancelBtn.Enabled := CanCancel;
                PetList.Panel3.Caption := PetList.ListBox1.Items[PickedNum];
                PetList.Width:= 400;
                PetList.ListBox1.MultiSelect := MultiPick;

                PetList.ShowModal;
                Result := not PetList.Cancel;
                if Result then begin
                   if (PetList.ListBox1.ItemIndex <> -1) then PickedNum := PetList.ListBox1.ItemIndex;
                   if MultiPick then begin
                      for I := pred(InList.Count) downto 0 do begin
                         if PetList.ListBox1.Selected[i] then begin
                           {$IfDef RecordListPick} WriteLineToDebugFile('Picked ' + InList.Strings[i]); {$EndIf}
                         end
                         else begin
                           {$IfDef RecordListPick} WriteLineToDebugFile('Rejected ' + InList.Strings[i]); {$EndIf}
                            Inlist.Delete(i);
                         end;
                      end;
                   end;
                end
                else begin
                   InList.Clear;
                end;
                PetList.Destroy;
            end;
         {$EndIf}
         end;


         procedure GetString(Prompt : Shortstring; var Input : ShortString;  ForceCaps : boolean; ValidChars : characters);
         var
            i : integer;
            OK : boolean;
         begin
            repeat
               PETMARCommonForm.Caption := 'Input String';
               PETMARCommonForm.Panel1.Caption := Prompt;
               PETMARCommonForm.Edit1.Text := Input;
               if ForceCaps then PETMARCommonForm.Edit1.CharCase := ecUpperCase
               else PETMARCommonForm.Edit1.CharCase := ecNormal;
               PETMARCommonForm.FormActivate(Nil);
               PETMARCommonForm.ShowModal;
               Input := PETMARCommonForm.Edit1.Text;
               OK := true;
               for I := Length(Input) downto 1 do if not (Input[i] in ValidChars) then begin
                  Delete(Input,i,1);
                  Ok := false;
               end;
            until OK and (Error = 0);
            if ForceCaps then Input := UpperCase(Input);
         end;


         Procedure GetAngle;
         var
            get_angle_form : Tget_angle_form;
         begin
            get_angle_form := Tget_angle_form.Create(Application);
            with get_angle_form do begin
               Angle := TheAngle;
               Edit5.Text := RealToString(Angle,-18,-8);
               RadioGroup4.ItemIndex := Ord(AngleMeasure);
               Caption := Mess;
               ShowModal;
               TheAngle := Angle;
               AngleMeasure := tAngleMeasure(RadioGroup4.ItemIndex);
               Close;
            end;
         end;


         function GetFileNameDefaultExt(Mess,Filters : ANSIstring; var FName : PathStr; AppendOption : boolean = false) : boolean; overload;
         var
            DefExt : integer;
         begin
            DefExt := 1;
            Result := GetFileNameDefaultExtSaveExt(Mess,Filters,FName,DefExt,AppendOption);
         end;


         function GetExistingFileName(Mess,Filters : ANSIstring; var FName : PathStr) : boolean;
         begin
            PETMARCommonForm.SaveDialog1.Title := Mess;
            PETMARCommonForm.SaveDialog1.Filter := Filters;

            PETMARCommonForm.SaveDialog1.InitialDir := ExtractFilePath(FName);
            PETMARCommonForm.SaveDialog1.FileName := ExtractFileName(fName);
            PETMARCommonForm.SaveDialog1.FilterIndex := 1;

            if PETMARCommonForm.SaveDialog1.Execute then begin
               if ExtractFileName(PETMARCommonForm.SaveDialog1.FileName) = '' then Result := false
               else begin
                  FName := PETMARCommonForm.SaveDialog1.FileName;
                  Result := true;
               end;
            end;
         end;


         function GetFileNameDefaultExtSaveExt(Mess,Filters : ANSIstring; var FName : PathStr; var df : integer; AppendOption : boolean = false) : boolean;
         var
            OK : boolean;
            tempString : ShortString;
            Ext,Extra : ExtStr;
            i : integer;
         begin
            PETMARCommonForm.SaveDialog1.Title := Mess;
            PETMARCommonForm.SaveDialog1.Filter := Filters;
            if FileExists(fName) or ValidPath(ExtractFilePath(fName)) then begin
               PETMARCommonForm.SaveDialog1.InitialDir := ExtractFilePath(FName);
               PETMARCommonForm.SaveDialog1.FileName := ExtractFileName(fName);
            end
            else begin
               PETMARCommonForm.SaveDialog1.InitialDir := Copy(fName,1,pred(Length(fName)));
               PETMARCommonForm.SaveDialog1.FileName := fName;
            end;
            PETMARCommonForm.SaveDialog1.FilterIndex := df;

            repeat
               if PETMARCommonForm.SaveDialog1.Execute then begin
                  {$IfDef RecordFileFilter} WriteLineToDebugFile('Filter index: ' + IntToStr(SaveDialog1.FilterIndex) + '  Filter: ' + Filters); {$EndIf}

                  OK := true;
                  if ExtractFileName(PETMARCommonForm.SaveDialog1.FileName) = '' then Result := false
                  else begin
                     FName := PETMARCommonForm.SaveDialog1.FileName;
                     CleanUpFileName(fName);
                     Result := true;
                     if (PETMARCommonForm.SaveDialog1.FileName = '') then OK := false
                     else begin
                        if FileExists(fName) then begin
                           tempString := '  file ' + fName;
                           if AppendOption then begin
                              OK := AnswerIsYes('append' + TempString);
                              if not OK then begin
                                 OK := AnswerIsYes('Overwrite' + TempString);
                                 if OK then DeleteFileIfExists(FName);
                              end;
                           end
                           else begin
                              OK := AnswerIsYes('Overwrite' + TempString);
                              if OK then DeleteFileIfExists(FName);
                           end;
                        end
                        else begin
                           Ext := ExtractFileExt(FName);
                           if Length(Ext) < 2 then begin
                              for I := 0 to pred(PETMARCommonForm.SaveDialog1.FilterIndex) do begin
                                 Extra := BeforeSpecifiedCharacterANSI(Filters,'*',true,true);
                                 Ext := BeforeSpecifiedCharacterANSI(Filters,'|',true,true);
                                 {$IfDef RecordFileFilter} WriteLineToDebugFile(' Ext ' + IntToStr(i) + ': ' + Ext); {$EndIf}
                              end;
                              FName := FName + Ext;
                           end;
                           df := PETMARCommonForm.SaveDialog1.FilterIndex;
                        end;
                     end;
                  end;
               end
               else begin
                  Result := false;
                  OK := true;
               end;
            until OK;
         end;

         procedure GetAngleInRadians(Message : shortstring; var Angle : float64);
         begin
            Angle := Angle / DegToRad;
            ReadDefault(Message,Angle);
            Angle := Angle * DegToRad;
         end;


         function GetMultipleFiles(InMessage,Masks : ANSIstring; var FilesWanted : TStringList; var DefaultFilter : byte) : boolean;
         var
            i : integer;
         begin
            {$If Defined(RecordDialogs)} WriteLineToDebugFile('GetMultipleFiles in, Masks=' + Masks); {$EndIf}
            PETMARCommonForm.OpenDialog1.Options := [ofAllowMultiSelect,ofEnableSizing];
            PETMARCommonForm.OpenDialog1.Title := 'Open files ' + InMessage;
            if (FilesWanted.Count = 0) then FilesWanted.Add(ProgramRootDir);
            PETMARCommonForm.OpenDialog1.FileName := ExtractFileName(FilesWanted.Strings[0]);
            PETMARCommonForm.OpenDialog1.InitialDir := ExtractFilePath(FilesWanted.Strings[0]);
            PETMARCommonForm.OpenDialog1.Filter := Masks;
            PETMARCommonForm.OpenDialog1.FilterIndex := DefaultFilter;
            {$If Defined(RecordDialogs)} WriteLineToDebugFile('GetMultipleFiles to user'); {$EndIf}

            if PETMARCommonForm.OpenDialog1.Execute then begin
               {$If Defined(RecordDialogs)} WriteLineToDebugFile('User done'); {$EndIf}
               FilesWanted.Clear;
               FilesWanted.Sorted := true;
               for i := 0 to pred(PETMARCommonForm.OpenDialog1.Files.Count) do begin
                  FilesWanted.Add(PETMARCommonForm.OpenDialog1.Files[i]);
               end;
               Result := (PETMARCommonForm.OpenDialog1.Files.Count > 0);
               DefaultFilter := PETMARCommonForm.OpenDialog1.FilterIndex;
            end
            else Result := false;
            {$If Defined(RecordDialogs)} WriteLineToDebugFile('GetMultipleFiles out'); {$EndIf}
         end;


         function GetFileMultipleMask(InMessage,Masks : ANSIstring; var FileWanted : PathStr; var DefaultFilter : byte) : boolean;
         begin
            {$If Defined(RecordDialogs)} WriteLineToDebugFile('GetFileMultipleMask in, Masks=' + Masks); {$EndIf}
            PETMARCommonForm.OpenDialog1.Options := [ofEnableSizing];
            PETMARCommonForm.OpenDialog1.Title := 'Open files ' + InMessage;
            PETMARCommonForm.OpenDialog1.FileName := ExtractFileName(FileWanted);
            PETMARCommonForm.OpenDialog1.InitialDir := ExtractFileDir(FileWanted);
            PETMARCommonForm.OpenDialog1.Filter := Masks;
            PETMARCommonForm.OpenDialog1.FilterIndex := DefaultFilter;
            if PETMARCommonForm.OpenDialog1.Execute then begin
               if (PETMARCommonForm.OpenDialog1.FileName = '') then FileWanted := ''
               else begin
                  FileWanted := ExpandFileName(PETMARCommonForm.OpenDialog1.FileName);
                  DefaultFilter := PETMARCommonForm.OpenDialog1.FilterIndex;
               end;
               Result := (FileWanted <> '');
            end
            else begin
               Result := false;
               FileWanted := '';
            end;
         end;



         procedure EditTFont(Font : tFont);
         var
            MyFont : tMyFont;
         begin
            PETMARCommonForm.FontDialog1.Font := Font;
            if PETMARCommonForm.FontDialog1.Execute then begin
               GetMyFontFromWindowsFont(MyFont,PETMARCommonForm.FontDialog1.Font);
               LoadMyFontIntoWindowsFont(MyFont,Font);
            end;
         end;

         procedure EditMyFont(var MyFont : tMyFont);
         begin
            LoadMyFontIntoWindowsFont(MyFont,PETMARCommonForm.FontDialog1.Font);
            if PETMARCommonForm.FontDialog1.Execute then GetMyFontFromWindowsFont(MyFont,PETMARCommonForm.FontDialog1.Font);
         end;

         function MyFontToString(MyFont : tMyFont) : ShortString;
         begin
            if (MyFont.Name = '') then Result := ''
            else Result := MyFont.Name + '  pts=' + IntToStr(MyFont.Size) + '  color=' + IntToStr(ConvertPlatformColorToTColor(MyFont.Color));
         end;

         procedure LoadMyFontIntoWindowsFont(MyFont : tMyFont; WindFont : tFont);
         begin
            WindFont.Name := MyFont.Name;
            WindFont.Size := MyFont.Size;
            WindFont.Color := ConvertPlatformColorToTColor(MyFont.Color);
            WindFont.Style := [];
            if MyFont.Bold then WindFont.Style := WindFont.Style + [fsBold];
            if MyFont.Italics then WindFont.Style := WindFont.Style + [fsItalic];
            if MyFont.Underline then WindFont.Style := WindFont.Style + [fsUnderline];
         end;

         procedure GetMyFontFromWindowsFont(var MyFont : tMyFont; WindFont : tFont);
         begin
            MyFont.Name := WindFont.Name;
            MyFont.Size := WindFont.Size;
            MyFont.Color := ConvertTColorToPlatformColor(WindFont.Color);
            MyFont.Bold := fsBold in WindFont.Style;
            MyFont.Italics := fsItalic in WindFont.Style;
            MyFont.Underline := fsUnderline in WindFont.Style;
         end;


         procedure InitializeMyFont(var MyFont : tMyFont; Name : shortstring; Size : SmallInt; Color : tPlatformColor);
         begin
            MyFont.Name := Name;
            MyFont.Size := Size;
            MyFont.Color := Color;
            MyFont.Italics := false;
            MyFont.Bold := false;
            MyFont.Underline := false;
         end;

         (*
         function ColorString(Color : tRGBTriple) : shortString;
         begin
            Result := RGBString(Color.rgbtRed,Color.rgbtGreen,Color.rgbtBlue) + ' gray=' + IntToStr(round(0.3 * Color.rgbtRed  + 0.59 * Color.rgbtGreen  + 0.11 * Color.rgbtBlue ));
         end;
         *)


         procedure CrossWithHole(Canvas : tCanvas; x,y : integer);
         begin
            Canvas.MoveTo(x+2,y);  Canvas.LineTo(x+8,y);
            Canvas.MoveTo(x-2,y);  Canvas.LineTo(x-8,y);
            Canvas.MoveTo(x,y+2);  Canvas.LineTo(x,y+8);
            Canvas.MoveTo(x,y-2);  Canvas.LineTo(x,y-8);
         end;


         procedure TextOutVertical(Canvas : TCanvas; x,y : integer; Words : ShortString; ClearBack : boolean = false);
         begin
            CanvasTextOutAngle(Canvas,x,y,900,Words);
         end;


         procedure CanvasTextOutAngle(c : TCanvas; x,y : Integer; d : Word; s : ShortString);
         {------------------------------------------------------------------------------
           PURPOSE
             To output rotated text in the same font as the font on the supplied canvas.
             The font must be a scaleable font.

           INPUT PARAMETERS
             c - The canvas on which to output the text.
             x,y - The x,y screen coordinates you would normally supply TextOut.
             d - The angle in tenths of degrees.
             s - The text to be output to the canvas.
           HISTORY
             6/18/1995 First version written by Curtis Keisler.
         -------------------------------------------------------------------------------}
         var
           LogRec         : TLOGFONT;     {* Storage area for font information *}
           OldFontHandle,                 {* The old font handle *}
           NewFontHandle  : HFONT;        {* Temporary font handle *}
         begin
           {* Get the current font information. We only want to modify the angle *}
           GetObject(c.Font.Handle, SizeOf(LogRec), Addr(LogRec));
           try
              {* Modify angle (0.1°) between the base line of a character and the x-axis}
              LogRec.lfEscapement := d;

              {* Create a new font handle using the modified old font handle *}
              NewFontHandle := CreateFontIndirect(LogRec);

              {* Save the old font handle! We have to put it back when we are done! *}
              OldFontHandle := SelectObject(c.Handle,NewFontHandle);

              {* Output the text *}
              c.TextOut(x,y,s);
           finally
              {* Put the font back the way we found it! *}
              NewFontHandle := SelectObject(c.Handle,OldFontHandle);

              {* Delete the temporary (NewFontHandle) that we created *}
              DeleteObject(NewFontHandle);
           end;
         end; {* CanvasTextOutAngle *}


         procedure CopyPartOfScreenToBitmap(var Bitmap2 : tMyBitmap; Left,Top,Right,Bottom : integer);
         var
            Bitmap : tMyBitmap;
         begin
            Delay(100);
            CaptureActiveWindowToClipboard;
            Delay(100);
            Bitmap := tMyBitmap.Create;
            Bitmap.Assign(Clipboard);
            CreateBitmap(Bitmap2,Right - Left,Bottom - Top);
            Bitmap2.Canvas.CopyRect(Rect(0,0,Bitmap2.Width,Bitmap2.Height),Bitmap.Canvas,Rect(Left,Top,Right,Bottom));
         end;


         procedure QueryTColor(var Color : tColor);
         begin
            PETMARCommonForm.ColorDialog1.Color := Color;
            if PETMARCommonForm.ColorDialog1.Execute then Color := PETMARCommonForm.ColorDialog1.Color;
         end;

         procedure QueryTColor(BitBtn : tBitBtn; var Color : tColor);
         begin
            QueryTColor(Color);
            ColorBitBtn(BitBtn,ConvertTColorToPlatformColor(Color));
         end;


         procedure QueryColor(var Color : tPlatformColor);
         var
            acolor : tcolor;
         begin
            aColor := ConvertPlatformColorToTColor(Color);
            QueryTColor(acolor);
            Color := ConvertTColorToPlatformColor(acolor);
         end;


         procedure QueryColor(BitBtn : tBitBtn; var Color : tPlatformColor); overload;
         var
            Color1 : tColor;
         begin
            Color1 := ConvertPlatformColorToTColor(Color);
            QueryTColor(BitBtn,Color1);
            Color := ConvertTColorToPlatformColor(Color1);
         end;


         procedure HardwareOnLine;
         begin
            MessageToContinue(HardwareString,true);
         end;


         procedure DisplayHTMLTopic(Name : Ansistring);
         var
            fName : Ansistring;
         begin
            fName := ProgramRootDir + Application.HelpFile;
            if FileExists(fname) then begin
               ReplaceCharacter(Name,'\','/');
               ExecuteFile(ProgramRootDir + 'keyhh.exe','-MDhelp microdem.chm::/' + Name,ProgramRootDir);
            end
            else begin
               {$IfDef AllowUSNAhelp}
                  ReplaceCharacter(Name,'\','/');
                  fname := 'https://www.usna.edu/Users/oceano/pguth/md_help/' + Name;
                  ExecuteFile(fName,'','');
               {$EndIf}
            end;
            StopSplashing;
            {$IfDef RecordHelp} WriteLineToDebugFile('DisplayHTMLTopic ' + Name + ' from ' + fName); {$EndIf}
         end;


         procedure PickLineSizeAndColor(WhatFor : shortString; BitButton : tBitBtn; var LineColor : tPlatformColor; var LineSize :  byte);
         var
            i : integer;
         begin
            i := LineSize;
            PickLineSizeAndColor(WhatFor,BitButton,LineColor,i);
            LineSize := i;
         end;


         procedure PickLineSizeAndColor(WhatFor : shortString; BitButton : tBitBtn; var LineColor : tPlatformColor; var LineSize :  float32);
         var
            i : integer;
         begin
            i := round(LineSize);
            PickLineSizeAndColor(WhatFor,BitButton,LineColor,i);
            LineSize := i;
         end;


         procedure PickLineSizeAndColor(WhatFor : shortString; BitButton : tBitBtn; var LineColor : tPlatformColor; var LineSize :  integer);
         var
            GraphColorsForm : Tlineparamsform;
         begin
            GraphColorsForm := Tlineparamsform.Create(Application);
            with GraphColorsForm do begin
               if (WhatFor <> '') then Caption := WhatFor;
               TheColor := LineColor;
               TheSize := LineSize;
               graphcolorsform.ShowModal;
               LineColor := graphcolorsform.theColor;
               LineSize := graphcolorsform.theSize;
               if (BitBUtton <> Nil) then ColorLineWidthBitBtn(BitButton,TheColor,TheSize);
               Free;
            end;
         end;


         procedure PickSymbol(BitButton : tBitBtn; var DrSymbol : tDrawingSymbol; var SymSize : byte; var WantColor : tPlatformColor; WhatFor : shortstring = '');
         begin
            GetSymbol(DrSymbol,SymSize,WantColor,WhatFor);
            if (BitButton <> Nil) then SymbolOnButton(BitButton,DrSymbol,SymSize,WantColor);
         end;


         procedure PickSymbol(BitButton : tBitBtn; var Symbol : tFullSymbolDeclaration; WhatFor : shortstring = '');
         begin
            PickSymbol(BitButton,Symbol.DrawingSymbol,Symbol.Size,Symbol.Color,WhatFor);
         end;


         procedure PETMARAboutBox(ProgramName : ANSIstring; Modifier : ANSIstring = '');
         var
            AboutBox: tPetmarAboutBox;
            BitMap : tMyBitmap;
         begin
            StopSplashing;
            AboutBox := TPetmarAboutBox.Create(Application);
            petImage.CreateBitmap(Bitmap,110,160);
            BitMap.Canvas.Pen.Color := clYellow;
            BitMap.Canvas.Rectangle(0,0,110,160);
            BitMap.Canvas.Brush.Color := clYellow;
            BitMap.Canvas.FloodFill(5,5,clYellow,fsBorder);
            with AboutBox do begin
               DisplayNevadella(BitMap.Canvas,4,-45,1,clBlue);
               Image1.Picture.Graphic := Bitmap;
               BitMap.Free;
               ProgramIcon.Height := Application.Icon.Height;
               ProgramIcon.Width := Application.Icon.Width;
               Label2.Caption := Modifier;
               Label3.Caption := '';

               //Label3.Caption := 'Database: ' + DBdriver;
               ProgramIcon.Canvas.Draw(0,0,Application.Icon);
               Version.Caption := BuildString;
               ProductName.Caption := ShortEXEName;
               ShowModal;
            end;
            AboutBox.Free;
         end;


         function LocalIP: string;
         type
            TaPInAddr = array [0..10] of PInAddr;
            PaPInAddr = ^TaPInAddr;
         var
             phe: PHostEnt;
             pptr: PaPInAddr;
             Buffer: array [0..63] of ANSIchar;
             i: Integer;
             GInitData: TWSADATA;
         begin
             WSAStartup($101, GInitData);
             Result := '';
             GetHostName(Buffer, SizeOf(Buffer));
             phe := GetHostByName(buffer);
             if phe = nil then Exit;
             pptr := PaPInAddr(Phe^.h_addr_list);
             i := 0;
             while pptr^[i] <> nil do    begin
               result := StrPas(inet_ntoa(pptr^[i]^));
               Inc(i);
             end;
             WSACleanup;
         end;



         procedure StringListToContinue(SaveOut : boolean; Mess : tStringList);
         var
            mess_cont_form: Tmess_cont_form;
            x,y,
            i,ml : integer;
         begin
            x := 0;
            y := 0;
            if SaveOut then begin
               mess_cont_form := Tmess_cont_form.Create(Application);
               ml := 0;
               for i := 0 to pred(Mess.Count) do begin
                  if Length(Mess.Strings[i]) > ml then ml := Length(Mess.Strings[i]);
                  mess_cont_form.Memo1.Lines.Add(Mess.Strings[i]);
               end;
               mess_cont_form.Height := 100 + (18 * Mess.Count);
               if (ml <  20) then mess_cont_form.Width := 200
               else mess_cont_form.Width := 20 + (8 * ml);

               if (x <> 0) and (y <> 0) then begin
                  if x + mess_cont_form.Width > Screen.Width then x := x - mess_cont_form.Width;
                  mess_cont_form.Left := x;
                  if y + mess_cont_form.Height > Screen.Height then y := y - mess_cont_form.Height;
                  mess_cont_form.Top := y;
               end;
               mess_cont_form.ShowModal;
               mess_cont_form.Free;
            end
            else begin
               if DualMonitor and (x = 0) and (y = 0) then begin
                  x := 400;
                  y := 400;
               end;
               if (x = 0) and (y = 0) then MessageDlg(Mess[0],mtInformation,[mbOK],0)
               else MessageDlgPos(Mess[0],mtInformation,[mbOK],0,x,y);
            end;
         end;


         function GetNewBMPSize(var x,y : int32;  aCapt : shortstring) : boolean;
         var
            NewBMPForm : tNewBMPForm;
         begin
            Result := false;
            NewBMPForm := TNewBMPForm.Create(Application);
            if (aCapt <> '') then NewBMPForm.Caption := aCapt;
            NewBMPForm.WidthEdit.Text := IntToStr(x);
            NewBMPForm.HeightEdit.Text := IntToStr(y);
            if (NewBMPForm.ShowModal <> idCancel) then begin
               x := StrToInt(NewBMPForm.WidthEdit.Text);
               y := StrToInt(NewBMPForm.HeightEdit.Text);
               result := true;
            end;
            NewBMPForm.Free;
         end;


         procedure QuickOpenEditWindow(FName : PathStr; WindowCaption : ANSIstring; ListenToClear : boolean = false; AddLineNumbers : boolean = false);
         var
            Max,i : integer;
            EditWindow : TPetEditF;
            S : TFileStream;
         begin
            if (FName = '') then GetFileFromDirectory('File to display','*.*',fName);
            if WindowCaption = '' then WindowCaption := ExtractFileName(fName);
            if FileExists(FName) then try
               EditWindow := TPetEditf.Create(Application);
               EditWindow.ListenToClear := ListenToClear;
               EditWindow.NoOpenNewOptions;
               with EditWindow do begin
                  FileInWindow := FName;
                  S:= TfileStream.Create(FName,fmOpenRead + fmShareDenyNone);
                  try
                     RichEdit1.Lines.LoadFromStream(S);
                  Finally
                     S.Free;
                  end;

                  if AddLineNumbers then AddLineNumbers1Click(nil);
                  EditWindow.Caption := WindowCaption;
                  with RichEdit1.Lines do if (Count < 25) then ClientHeight := 14 * Count + 100
                  else EditWindow.Height := 300;
                  Max := 0;
                  for i := 0 to pred(RichEdit1.Lines.Count) do
                     if length(RichEdit1.Lines[i]) > Max then Max := length(RichEdit1.Lines[i]);
                  if (Max < 30) then ClientWidth := 300
                  else if (Max < 64) then ClientWidth := 9 * Max
                  else begin
                     ClientWidth := 640;
                     RichEdit1.ScrollBars := ssBoth;
                  end;
               end;
            finally
            end;
         end;


         procedure ModalEditWindow(FName : PathStr; WindowCaption : ANSIstring);
         var
            EditWindow : TPetEditF;
         begin
            if FileExists(FName) then begin
               EditWindow := TPetEditf.Create(Application);
               with EditWindow do begin
                  NoOpenNewOptions;
                  FormStyle := fsNormal;
                  Visible := false;
                  FileInWindow := FName;
                  RichEdit1.Lines.LoadFromFile(FName);
                  Caption := WindowCaption;
                  Width := 640;
                  Height := 300;
                  ShowModal;
                  Close;
               end;
            end;
         end;


         procedure ModalStringListDisplay(var aList : tStringList; WindowCaption : ANSIstring; Purge : boolean = true);
         var
            fName : PathStr;
            I : Integer;
         begin
            fName := System.IOUtils.TPath.Combine(MDTempDir, 'temp_string_list.txt');
            aList.SaveToFile(fName);
            ModalEditWindow(FName,WindowCaption);
            {$IfDef TrackFileDeletion} MessageToContinue('ModalStringListDisplay call DeleteFileIfExists'); {$EndIf}
            aList.Clear;
            if Purge then aList.Free
            else begin
               aList.LoadFromFile(fName);
               for I := pred(aList.Count) downto 0 do begin
                  aList.Strings[i] := ptTrim(aList.Strings[i]);
                  if aList.Strings[i] = '' then Alist.Delete(i);
               end;
            end;
            DeleteFileIfExists(FName);
         end;


         procedure DisplayString(Findings : String; WindowTitle : shortString; ListenToClear : boolean = false);
         var
            FName : pathStr;
         begin
            fname := NextFileNumber(MDTempDir,'results_','.txt');
            tfile.WriteAllText(fName,Findings);
            QuickOpenEditWindow(FName,WindowTitle,ListenToClear);
         end;


         procedure DisplayAndPurgeStringList(var Findings : tStringList; WindowTitle : shortString; ListenToClear : boolean = false);
         var
            FName : pathStr;
         begin
            if (Findings <> Nil) then begin
               if Findings.Count > 0 then begin
                  FName := NextFileNumber(MDTempDir,'results+','.txt');
                  Findings.SaveToFile(FName);
                  QuickOpenEditWindow(FName,WindowTitle,ListenToClear);
               end;
               Findings.Free;
               Findings := Nil;
            end;
         end;


         procedure GetDate(var Month,Day,Year : word);
         var
            Duration : word;
         begin
            GetDateAndDuration(Month,Day,Year,Duration,false);
         end;


         procedure GetDateAndDuration(var Month,Day,Year,Duration : word; ShowDuration : boolean = true);
         var
            iMonth,iDay,iYear : word;
         begin
            if Month = 99 then begin
               DecodeDate(Now,iYear,iMonth,iDay);
               Year := iYear;
               Month := iMonth;
               Day := iDay;
            end;
            GetDateForm := TGetDateForm.Create(Application);

            with GetDateForm do begin
               Edit1.Text := IntToStr(Month);
               Edit2.Text := IntToStr(Day);
               Edit3.Text := IntToStr(Year);
               Edit4.Text := IntToStr(Duration);
               Edit4.Visible := ShowDuration;
               Label4.Visible := ShowDuration;
               //CheckBox1.Checked := MDDef.RiseSet;
               //CheckBox2.Checked := MDDef.MoonPhase;
               ShowModal;
               CheckEditString(Edit1.Text,Month);
               if (Month > 12) then Month := 12;
               CheckEditString(Edit2.Text,Day);
               if (Day > 31) then Day := 31;
               CheckEditString(Edit3.Text,Year);
               CheckEditString(Edit4.Text,Duration);
               //MDDef.RiseSet := CheckBox1.Checked;
               //MDDef.MoonPhase := CheckBox2.Checked;
            end;
         end;


         procedure PickPattern(WhatFor : shortString; var Style : tBrushStyle; var FillColor,BorderColor : tPlatformColor; var inBorderWidth : integer; BackBMP : tMyBitmap = Nil);
         var
           PickFillForm : PickFillPattern.TPickFillForm;
         begin
            PickFillForm := TPickFillForm.Create(Application);
            with PickFillForm do begin
              Caption := 'Pick pattern for ' + WhatFor;
              BaseBitmap := BackBMP;
              PickStyle := Style;
              PickFillColor := FillColor;
              PickBorderColor := BorderColor;
              PickFillForm.PickBorderWidth := pred(inBorderWidth);
              ColorBitBtn(BitBtn2,FillColor);
              ColorBitBtn(BitBtn1,BorderColor);
              SpinButton1UpClick(Nil);
              ShowModal;
              Style := PickStyle;
              FillColor := PickFillColor;
              BorderColor := PickBorderColor;
              inBorderWidth := PickFillForm.PickBorderWidth;
            end;
         end;


         procedure DrawPatternOnBitmap(var Bitmap : tMyBitmap; FillColor,SymbolLineBorderColor : tPlatformColor; SymbolFill : tBrushStyle; SymbolLineBorderSize : integer);
         begin
            Bitmap.Canvas.Brush.Color := ConvertPlatformColorToTColor(FillColor);
            Bitmap.Canvas.Brush.Style := SymbolFill;
            Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(SymbolLineBorderColor);
            Bitmap.Canvas.Pen.Width := SymbolLineBorderSize;
            Bitmap.Canvas.Rectangle(2,2,Bitmap.Width-2,Bitmap.Height-2);
         end;


         procedure FillPatternOnBitBtn(BitBtn : tBitBtn; FillColor,SymbolLineBorderColor : tPlatformColor; SymbolFill : tBrushStyle; SymbolLineBorderSize : integer);
         var
            Bitmap : tMyBitmap;
         begin
            CreateBitmap(bitmap,20,20);
            DrawPatternOnBitmap(Bitmap, FillColor,SymbolLineBorderColor,SymbolFill,SymbolLineBorderSize);
            BitBtn.Glyph := Bitmap;
            Bitmap.Free;
         end;


         procedure SimulateKeystroke(Key : byte;    extra : DWORD);
         begin
            keybd_event(Key, extra,  0,  0);
            keybd_event(Key, extra, KEYEVENTF_KEYUP, 0);
         end;


         procedure CaptureActiveWindowToClipboard;
         begin
            ApplicationProcessMessages;
            Delay(250);
            SimulateKeystroke(VK_SNAPSHOT, 1);
            Delay(250);
            ApplicationProcessMessages;
         end;


         procedure CaptureEntireScreenToClipboard;
         begin
            ApplicationProcessMessages;
            Delay(250);
            SimulateKeystroke(VK_SNAPSHOT, 0);
            Delay(250);
            ApplicationProcessMessages;
         end;



         procedure PrintImageToSpecifiedSize(Image1 : TImage; PrinterXInches,PrinterYInches : float64);
         {prints out to scale}

                procedure PrintAnImage(DestRect,SourceRect : TRect; ABitmap : tMyBitmap; PrintTitle : ShortString = '');
               var
                  Header,Bits : Pointer;
                  HeaderSize,
                  BitsSize       : cardinal;
                  Total,OnPart,
                  DivideFactor,x,y  : integer;
                  xyDestRect,xySourceRect : tRect;
               begin
                  DivideFactor := 1;
                  while ((DestRect.Right - DestRect.Left) div DivideFactor) * ((DestRect.Bottom - DestRect.Top) div DivideFactor) > 1500000 do inc(DivideFactor);
                  {$IfDef RecordPrint} WriteLineToDebugFile(' Divide factor set to ' + IntToStr(DivideFactor)); {$EndIf}
                  Printer.BeginDoc;
                  GetDIBSizes(ABitmap.Handle,HeaderSize,BitsSize);
                  GetMem(Header,HeaderSize);
                  GetMem(Bits,BitsSize);
                  try
                     GetDIB(ABitmap.Handle, ABitmap.Palette, Header^, Bits^);
                     StartProgress('Print ' + PrintTitle);
                     Total := sqr(DivideFactor);
                     OnPart := 0;
                     for x := 0 to pred(DivideFactor) do begin
                        for y := 0 to pred(DivideFactor) do begin
                           inc(OnPart);
                           UpdateProgressBar(OnPart / Total);
                           xySourceRect.Left := SourceRect.Left + x * (SourceRect.Right - SourceRect.Left) div DivideFactor;
                           xySourceRect.Right := xySourceRect.Left + (SourceRect.Right - SourceRect.Left) div DivideFactor;
                           xySourceRect.Top := SourceRect.Top + y * (SourceRect.Bottom - SourceRect.Top) div DivideFactor;
                           xySourceRect.Bottom := xySourceRect.Top + (SourceRect.Bottom - SourceRect.Top) div DivideFactor;
                           xyDestRect.Left := DestRect.Left + x * (DestRect.Right - DestRect.Left) div DivideFactor;
                           xyDestRect.Right := xyDestRect.Left + (DestRect.Right - DestRect.Left) div DivideFactor;
                           xyDestRect.Top := DestRect.Top + (pred(DivideFactor) - y)* (DestRect.Bottom - DestRect.Top) div DivideFactor;
                           xyDestRect.Bottom := xyDestRect.Top + (DestRect.Bottom - DestRect.Top) div DivideFactor;
                           StretchDIBits(Printer.Canvas.Handle, xyDestRect.Left, xyDestRect.Top,xyDestRect.Right - xyDestRect.Left, xyDestRect.Bottom - xyDestRect.Top,
                               xySourceRect.Left,xySourceRect.Top,xySourceRect.Right - xySourceRect.Left,xySourceRect.Bottom - xySourceRect.Top,Bits, tBitmapInfo(Header^),DIB_RGB_COLORS, SRCCOPY);
                        end;
                     end;
                  finally
                     FreeMem(Header, HeaderSize);
                     FreeMem(Bits, BitsSize);
                     EndProgress;
                     Printer.EndDoc;
                  end;
               end;

         var
            Bitmap  : tMyBitmap;
            PrinterXDPI,PrinterYDPI,
            sh,sw,
            PrinterHeight,PrinterWidth,x,y,
            XReps,YReps,xstart,ystart,xsize,ysize: integer;
            TargetDPI   : float64;
         begin
            PrinterWidth := GetDeviceCaps(Printer.Handle,HorzRes);
            PrinterXDPI := GetDeviceCaps(Printer.Handle,LogPixelsX);
            PrinterHeight := GetDeviceCaps(Printer.Handle,VertRes);
            PrinterYDPI := GetDeviceCaps(Printer.Handle,LogPixelsY);

            TargetDPI := Image1.Width / PrinterXInches;
            xReps := succ(trunc(PrinterXInches / (PrinterWidth / PrinterXDPI)));
            yReps := succ(trunc(PrinterYInches / (PrinterHeight / PrinterYDPI)));

            sw := round(PrinterWidth * TargetDPI / PrinterXDPI);
            sh := round(PrinterHeight * TargetDPI / PrinterYDPI);

            {$IfDef RecordPrint} WriteLineToDebugFile('In PrintImageToSpecifiedSize Sheets required: '+ IntToStr(xreps) + ' by ' + IntToStr(yreps));           {$EndIf}

            ShowHourglassCursor;
            for x := 0 to pred(XReps) do begin
               for y := 0 to pred(YReps) do begin
                  xstart := sw * x;
                  ystart := sh * y;
                  xsize := sw;
                  if xstart + xsize > Image1.Width then Xsize := abs(xstart - Image1.Width);

                  ysize := sh;
                  if ystart + ysize > Image1.Height then ysize := abs(Image1.Height - ystart);

                  CreateBitmap(Bitmap,xsize,ysize);
                  Bitmap.Canvas.CopyRect(Rect(0,0,xsize,ysize),Image1.Canvas, Rect(xstart,ystart,xstart+xsize,ystart+ysize));
                  PrintAnImage(Rect(0,0,round(PrinterWidth*xsize/sw),round(PrinterHeight*ysize/sh)),Rect(0,0,Bitmap.Width,Bitmap.Height),Bitmap);
                  Bitmap.Free;
               end;
            end;
            ShowDefaultCursor;
         end;


         procedure DisplayNevadella;
         const
            Nevadella : array[1..424,1..2] of byte = (
            (0,0),(100,100),(100,92),(99,91),(99,87),(98,86),(98,84),(97,83),
            (97,81),(96,80),(96,79),(95,78),(95,77),(94,76),(94,75),(93,74),
            (92,73),(92,72),(91,71),(90,70),(89,69),(89,68),(88,67),(87,66),
            (86,65),(85,64),(84,63),(83,62),(82,61),(81,61),(80,60),(79,59),
            (78,58),(77,58),(76,57),(75,56),(74,56),(73,55),(72,55),(71,54),
            (70,54),(67,53),(66,52),(64,52),(63,51),(59,51),(58,50),(42,50),
            (41,51),(37,51),(36,52),(34,52),(33,53),(31,53),(30,54),(29,54),
            (28,55),(27,55),(26,56),(25,56),(24,57),(23,58),(22,58),(21,59),
            (20,60),(19,61),(18,61),(17,62),(16,63),(15,64),(14,65),(13,66),
            (12,67),(11,68),(11,69),(10,70),(9,71),(8,72),(8,73),(7,74),
            (6,75),(6,76),(5,77),(5,78),(4,79),(4,80),(3,81),(3,83),
            (2,84),(2,86),(1,87),(1,91),(0,92),(0,100),(0,0),(36,64),
            (35,64),(35,65),(33,67),(34,66),(32,68),(32,69),(31,70),(31,72),
            (30,73),(30,78),(0,0),(64,64),(65,64),(65,65),(66,66),(67,67),
            (68,68),(68,69),(69,70),(69,72),(70,73),(70,78),(0,0),(0,100),
            (10,83),(0,0),(100,100),(90,83),(0,0),(10,83),(90,83),(0,0),
            (40,83),(48,63),(0,0),(60,83),(52,63),(0,0),(48,63),(52,63),
            (0,0),(46,68),(54,68),(0,0),(44,72),(56,72),(0,0),(42,76),
            (58,76),(0,0),(30,78),(36,64),(0,0),(70,78),(64,64),(0,0),
            (15,85),(85,85),(0,0),(12,90),(87,90),(0,0),(12,90),(15,85),
            (0,0),(87,90),(85,85),(0,0),(45,90),(45,85),(0,0),(55,90),
            (55,85),(0,0),(15,92),(85,92),(0,0),(12,97),(87,97),(0,0),
            (12,97),(15,92),(0,0),(85,92),(87,97),(0,0),(45,97),(45,92),
            (0,0),(55,97),(55,92),(0,0),(10,99),(90,99),(0,0),(12,105),
            (87,105),(0,0),(10,99),(8,115),(0,0),(90,99),(92,115),(0,0),
            (8,115),(12,105),(0,0),(92,115),(87,105),(0,0),(12,97),(15,92),
            (0,0),(85,92),(87,97),(0,0),(45,99),(45,105),(0,0),(55,99),
            (55,105),(0,0),(17,107),(83,107),(0,0),(19,111),(81,111),(0,0),
            (19,111),(17,107),(0,0),(81,111),(83,107),(0,0),(45,111),(45,107),
            (0,0),(55,111),(55,107),(0,0),(19,113),(81,113),(0,0),(21,117),
            (79,117),(0,0),(21,117),(19,113),(0,0),(79,117),(81,113),(0,0),
            (45,117),(45,113),(0,0),(55,117),(55,113),(0,0),(21,119),(79,119),
            (0,0),(23,123),(77,123),(0,0),(23,123),(21,119),(0,0),(77,123),
            (79,119),(0,0),(45,123),(45,119),(0,0),(55,123),(55,119),(0,0),
            (23,125),(77,125),(0,0),(25,129),(75,129),(0,0),(25,129),(23,125),
            (0,0),(75,129),(77,125),(0,0),(45,129),(45,125),(0,0),(55,129),
            (55,125),(0,0),(25,131),(75,131),(0,0),(27,135),(73,135),(0,0),
            (27,135),(25,131),(0,0),(73,135),(75,131),(0,0),(45,135),(45,131),
            (0,0),(55,135),(55,131),(0,0),(27,137),(73,137),(0,0),(29,141),
            (71,141),(0,0),(29,141),(27,137),(0,0),(71,141),(73,137),(0,0),
            (45,141),(45,137),(0,0),(55,141),(55,137),(0,0),(29,143),(71,143),
            (0,0),(31,147),(69,147),(0,0),(31,147),(29,143),(0,0),(69,147),
            (71,143),(0,0),(45,147),(45,143),(0,0),(55,147),(55,143),(0,0),
            (31,149),(69,149),(0,0),(33,153),(67,153),(0,0),(33,153),(31,149),
            (0,0),(67,153),(69,149),(0,0),(45,153),(45,149),(0,0),(55,153),
            (55,149),(0,0),(33,155),(67,155),(0,0),(35,159),(65,159),(0,0),
            (35,159),(33,155),(0,0),(65,159),(67,155),(0,0),(45,159),(45,155),
            (0,0),(55,159),(55,155),(0,0),(35,161),(65,161),(0,0),(37,165),
            (63,165),(0,0),(37,165),(35,161),(0,0),(63,165),(65,161),(0,0),
            (45,165),(45,161),(0,0),(55,165),(55,161),(0,0),(37,167),(63,167),
            (0,0),(39,171),(61,171),(0,0),(39,171),(37,167),(0,0),(61,171),
            (63,167),(0,0),(45,171),(45,167),(0,0),(55,171),(55,167),(0,0),
            (45,173),(55,173),(0,0),(45,173),(50,199),(0,0),(55,173),(50,199) );

            PTBRPts = 161;
            PTBR : array [1..PTBRpts,1..2] of byte = ( (0,0),
            (75,109),(27,109),(0,0),(42,200),(42,146),(32,146),(32,134),(65,134),
            (65,134),(65,146),(42,146),(47,146),(59,157),(60,199),(0,0),(31,126),
            (20,94),(81,94),(71,126),(31,126),(0,0),(20,83),(98,83),(98,90),(97,90),
            (97,97),(96,97),(96,103),(95,103),(95,107),(94,107),(94,110),(93,111),
            (93,113),(92,113),(92,116),(91,116),(91,118),(90,118),(90,120),(89,121),
            (89,122),(88,123),(88,124),(87,125),(87,126),(86,126),(86,127),(85,128),
            (85,129),(84,129),(84,130),(83,130),(83,131),(82,131),(82,132),(81,132),
            (81,134),(80,135),(80,136),(79,136),(79,138),(0,0),(24,134),(23,134),
            (23,133),(22,133),(22,132),(21,132),(21,131),(20,131),(20,130),(19,130),
            (19,129),(18,129),(18,128),(18,127),(17,127),(17,126),(16,125),(16,124),
            (15,124),(15,123),(14,123),(14,122),(13,121),(13,120),(12,118),(11,117),
            (10,114),(9,111),(8,108),(7,106),(6,106),(6,104),(5,104),(5,102),(4,101),
            (4,99),(3,99),(2,95),(2,90),(1,90),(1,82),(0,82),(0,66),(1,66),(1,63),
            (2,62),(2,60),(3,60),(3,59),(4,59),(4,58),(5,58),(5,58),(6,57),(7,57),
            (7,56),(9,56),(9,55),(11,55),(11,54),(13,54),(14,53),(17,53),(17,52),
            (21,52),(21,51),(31,51),(31,50),(58,50),(58,51),(79,51),(79,52),(85,52),
            (85,53),(87,53),(87,54),(89,54),(89,55),(91,55),(91,56),(92,56),(92,57),
            (93,57),(94,58),(95,58),(95,59),(96,59),(96,60),(97,61),(97,62),(98,63),
            (98,65),(99,65),(99,69),(100,69),(100,74),(1,74));

         var
            i : integer;
         begin {procedure DisplayNevadella}
            with Canvas do begin
               Pen.Width := 1;
               Pen.Color := Color;
               i := 1;
               if Odd(Random(500)) then begin
                  while i <= 424 do begin
                     if (Nevadella[i,1] = 0) and (Nevadella[i,2] = 0) then begin
                        inc(i);
                        MoveTo(xoff+Size*Nevadella[i,1],yoff+Size*Nevadella[i,2])
                     end
                     else LineTo(xoff+Size*Nevadella[i,1],yoff+Size*Nevadella[i,2]);
                     inc(i);
                  end {for i};
               end
               else begin
                  Pen.Width := 3;
                  while i <= PTBRpts do begin
                     if (PTBR[i,1] = 0) and (PTBR[i,2] = 0) then begin
                        inc(i);
                        MoveTo(xoff+size*succ(PTBR[i,1]),yoff+size*PTBR[i,2]);
                     end
                     else LineTo(xoff+size*succ(PTBR[i,1]),yoff+size*PTBR[i,2]);
                     inc(i);
                  end;
               end;
            end {with};
         end {procedure DisplayNevadella};


         procedure ScreenSymbol(Canvas : TCanvas; x,y : integer; Symbol : tFullSymbolDeclaration);
         begin
            ScreenSymbol(Canvas,x,y,Symbol.DrawingSymbol,Symbol.Size,Symbol.Color);
         end;


         procedure ScreenSymbol(Canvas : TCanvas; x,y : integer; Sym : tDrawingSymbol; size: integer; color : TPlatformColor);
         var
            PointsInPolyLine,pw : integer;
            PolyLinePoints : array[0..5] of tPoint;
         begin
            try
               if (X < 0) or (y < 0) then exit;
                  pw := 1;
                  if Sym in [Ex,Splat] then begin
                     pw := succ(Size) div 2;
                     if (pw = 0) then pw := 1;
                  end;
                  if Sym in [VertLine] then pw := Size;

                  Canvas.Pen.Width := pw;
                  Canvas.Pen.Color := ConvertPlatformColorToTColor(Color);

                  if Sym in [FilledCircle,FilledBox,FilledDiamond,FilledUpTri,FilledDownTri] then begin
                     Canvas.Brush.Color := ConvertPlatformColorToTColor(Color);
                     Canvas.Brush.Style := bsSolid;
                  end
                  else Canvas.Brush.Style := bsClear;
                  case Sym of
                     FilledBox,Box : Canvas.Rectangle(x-size,y-size,x+size,y+size);
                     FilledCircle,
                     Circle  : Canvas.Ellipse(X+Size,Y+Size,X-Size,Y-Size);
                     Dot     : Canvas.Pixels[x,y] := ConvertPlatformColorToTColor(Color);
                     Cross   : begin
                                  Canvas.MoveTo(x+Size,y);  Canvas.LineTo(x-Size,y);
                                  Canvas.MoveTo(x,y+Size);  Canvas.LineTo(x,y-Size);
                               end;
                     VertLine : begin
                                  Canvas.MoveTo(x,y+Size);  Canvas.LineTo(x,y-Size);
                               end;
                     EX      : begin
                                  Canvas.MoveTo(x+Size,y+Size); Canvas.LineTo(x-Size,y-Size);
                                  Canvas.MoveTo(x-Size,y+Size); Canvas.LineTo(x+Size,y-Size);
                               end;
                     FilledDiamond,
                     Diamond : begin
                                  PointsInPolyLine := 5;
                                  PolyLinePoints[0].x := x-Size;
                                  PolyLinePoints[0].y := y;
                                  PolyLinePoints[1].x := x;
                                  PolyLinePoints[1].y := y+size;
                                  PolyLinePoints[2].x := x+size;
                                  PolyLinePoints[2].y := y;
                                  PolyLinePoints[3].x := x;
                                  PolyLinePoints[3].y := y-size;
                                  PolyLinePoints[4].x := x-size;
                                  PolyLinePoints[4].y := y;
                                  Canvas.Polygon(slice(PolyLinePoints,PointsInPolyLine))
                               end;
                     Splat   : begin
                                  Canvas.MoveTo(x+Size,y);  Canvas.LineTo(x-Size,y);
                                  Canvas.MoveTo(x,y+Size);  Canvas.LineTo(x,y-Size);
                                  Canvas.MoveTo(x+Size,y+Size); Canvas.LineTo(x-Size,y-Size);
                                  Canvas.MoveTo(x-Size,y+Size); Canvas.LineTo(x+Size,y-Size);
                               end;
                     FilledUpTri,
                     UpTri   : begin
                                  PointsInPolyLine := 4;
                                  PolyLinePoints[0].x := x-Size;
                                  PolyLinePoints[0].y := y+Size;
                                  PolyLinePoints[1].x := x;
                                  PolyLinePoints[1].y := y-size;
                                  PolyLinePoints[2].x := x+size;
                                  PolyLinePoints[2].y := y+size;
                                  PolyLinePoints[3].x := x-size;
                                  PolyLinePoints[3].y := y+Size;
                                  Canvas.Polygon(slice(PolyLinePoints,PointsInPolyLine))
                               end;
                     FilledDownTri,
                     DownTri : begin
                                  PointsInPolyLine := 4;
                                  PolyLinePoints[0].x := x-Size;
                                  PolyLinePoints[0].y := y-Size;
                                  PolyLinePoints[1].x := x;
                                  PolyLinePoints[1].y := y+size;
                                  PolyLinePoints[2].x := x+size;
                                  PolyLinePoints[2].y := y-size;
                                  PolyLinePoints[3].x := x-size;
                                  PolyLinePoints[3].y := y-Size;
                                  Canvas.Polygon(slice(PolyLinePoints,PointsInPolyLine))
                               end;
                  end {case};
                  Canvas.MoveTo(x,y);
            finally
               Canvas.Brush.Style := bsClear;
            end;
         end;


         function WinExecAndWait32(FileName : ANSIstring; Wait : boolean = true; Log : boolean = true) : integer;
         var
           zAppName : array[0..1024] of char;
           zCurDir : array[0..1024] of char;
           WorkDir : ANSIstring;
           StartupInfo : TStartupInfo;
           ProcessInfo : TProcessInformation;
         begin
            {$IfDef RecordShellExecute} if Log and (not HeavyDutyProcessing) and (not DEMMergeInProgress) then WriteLineToDebugFile('WinExecAndWait32 in, cmd=' + FileName ); {$EndIf}
            if Wait then ShowHourglassCursor;
            StrPCopy(zAppName,FileName);
            GetDir(0,WorkDir);
            StrPCopy(zCurDir,WorkDir);
            FillChar(StartupInfo,Sizeof(StartupInfo),#0);
            StartupInfo.cb := Sizeof(StartupInfo);
            StartupInfo.dwFlags := STARTF_USESHOWWINDOW;

            if MDDef.ShowWinExec then StartupInfo.wShowWindow := SW_Show
            else StartupInfo.wShowWindow := SW_Hide;

            if not CreateProcess(nil,
                zAppName,                                    { pointer to command line ANSIstring }
                nil,                                         { pointer to process security attributes }
                nil,                                         { pointer to thread security attributes }
                false,                                       { handle inheritance flag }
                CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, { creation flags }
                nil,                                         { pointer to new environment block }
                nil,                                         { pointer to current directory name }
                StartupInfo,                                 { pointer to STARTUPINFO }
                ProcessInfo) then begin                      { pointer to PROCESS_INF }
                   Result := -1
            end
            else begin
                if Wait then WaitforSingleObject(ProcessInfo.hProcess,INFINITE);
                Result := 0;
            end;
            ApplicationProcessMessages;
            if Wait then ShowDefaultCursor;
            {$IfDef RecordShellExecute}
               if Log and (not HeavyDutyProcessing) and (not DEMMergeInProgress) and (Result <> 0) then WriteLineToDebugFile('WinExecAndWait32 out, result=' + IntToStr(Result));
            {$EndIf}
         end;


         function ExecuteFile(const FileName : ANSIstring; Params : ANSIstring = ''; DefaultDir : ANSIstring = ''): THandle;
         var
           zFileName, zParams, zDir: array[0..2048] of char;
           tDir : ANSIstring;
           Len  : integer;
           ShowCmd : Integer;
         begin
           tDir := DefaultDir;
           Len := length(tDir);
           if (len > 3) and (tDir[len] = '\') then Delete(tDir,len,1);
           {$IfDef RecordShellExecute} if (not HeavyDutyProcessing) then WriteLineToDebugFile('ExecuteFile ' + FileName + '  Parmams=' + Params + '  DefaultDir=' + DefaultDir); {$EndIf}
           if MDDef.ShowWinExec then ShowCmd := SW_Show
           else ShowCmd := SW_Hide;
           Result := ShellAPI.ShellExecute(Application.MainForm.Handle, nil,StrPCopy(zFileName, FileName),StrPCopy(zParams, Params),StrPCopy(zDir, tDir), ShowCmd);
         end;

         var
            ProgressCaption : ANSIstring;

         procedure StartCount(Title : ShortString);
         begin
            if (PetProgF = Nil) then PetProgF := TPetProgF.Create(Application)
            else PetProgF.Visible := true;
            PetProgF.Gauge1.Visible := true;
            PetProgF.Caption := Title;
            ProgressCaption := Title;
            PetProgF.Gauge1.Progress := 0;
            ShowHourglassCursor;
         end;

         procedure UpdateCount(HowFar : LongInt);
         begin
            if WantShowProgress and (PETProgF <> Nil) then begin
               PetProgF.Caption := ProgressCaption + '  ' + IntToStr(HowFar);
               ApplicationProcessMessages;
            end;
         end;

         procedure EndCount;
         begin
            if WantShowProgress and (PETProgF <> Nil) then begin
               PetProgF.Visible := false;
               ShowDefaultCursor;
            end;
         end;


      procedure StartThreadTimers(Capt : ShortString = ''; MaxThreads : integer = 8; Enable1 : boolean = false);
      var
         i : integer;
      begin
         if (ThreadTimers = Nil) then begin
            {$IfDef ThreadTimerCreation} WriteLineToDebugFile('Creating ThreadTimers'); {$EndIf}
            ThreadTimers := TThreadTimerForm.Create(Application);
         end;
         ThreadTimers.Caption := Capt;
         ThreadTimers.OverallGauge9.Visible := true;
         for i := 1 to MaxThreads do ThreadTimers.EnableGauge(i,false);
         if Enable1 then ThreadTimers.EnableGauge(1, true);
         ThreadTimers.ClientHeight := 70 + MaxThreads * 30;
         ThreadTimers.Gauge1.Visible := MaxThreads > 0;
         ThreadTimers.Gauge2.Visible := MaxThreads > 1;
         ThreadTimers.Gauge3.Visible := MaxThreads > 2;
         ThreadTimers.Gauge4.Visible := MaxThreads > 3;
         ThreadTimers.Gauge5.Visible := MaxThreads > 4;
         ThreadTimers.Gauge6.Visible := MaxThreads > 5;
         ThreadTimers.Gauge7.Visible := MaxThreads > 6;
         ThreadTimers.Gauge8.Visible := MaxThreads > 7;
         ThreadTimers.StatusBar1.Panels[1].Text := 'Start ' + TimeToStr(Now);
         ThreadTimers.Show;
      end;


      procedure EndThreadTimers;
      begin
         if (ThreadTimers <> Nil) then begin
            ThreadTimers.Close;
            ThreadTimers := Nil;
            {$IfDef ThreadTimerCreation} WriteLineToDebugFile('Destroying ThreadTimers'); {$EndIf}
            ApplicationProcessMessages;
         end;
      end;

      procedure StartSingleThreadTimer(Capt : string35 = '');
      begin
         StartThreadTimers(Capt,1,true);
      end;
	  
	  
	  
procedure ReadDefault(Prompt : ShortString; var IntVal : byte);
var
   Error : integer;
begin
   {$IfDef VCL}
      with PETMARCommonForm do repeat
         Caption := 'Input byte (0-255)';
         Panel1.Caption := Prompt;
         Edit1.Text := IntToStr(IntVal);
         PETMARCommonForm.FormActivate(Nil);
         ShowModal;
         Val(Edit1.Text,IntVal,Error);
      until (Error = 0);
   {$EndIf}
end;


procedure ReadDefault(Prompt : ShortString; var IntVal : int16);
var
   Error : integer;
begin
   {$IfDef VCL}
      with PETMARCommonForm do repeat
         Caption := 'Input Integer';
         Panel1.Caption := Prompt;
         Edit1.Text := IntToStr(IntVal);
         PETMARCommonForm.FormActivate(Nil);
         ShowModal;
         Val(Edit1.Text,IntVal,Error);
      until (Error = 0);
   {$EndIf}
end;


procedure ReadDefault(Prompt : ShortString; var IntVal : int32);
var
   Error : integer;
begin
   {$IfDef VCL}
      with PETMARCommonForm do repeat
         Caption := 'Input Integer';
         Panel1.Caption := Prompt;
         Edit1.Text := IntToStr(IntVal);
         PETMARCommonForm.FormActivate(Nil);
         ShowModal;
         Val(Edit1.Text,IntVal,Error);
      until (Error = 0);
   {$EndIf}
end;


procedure ReadDefault(Prompt : ShortString; var IntVal : int64);
var
   Error : integer;
begin
   {$IfDef VCL}
      with PETMARCommonForm do repeat
         Caption := 'Input Integer';
         Panel1.Caption := Prompt;
         Edit1.Text := IntToStr(IntVal);
         PETMARCommonForm.FormActivate(Nil);
         ShowModal;
         Val(Edit1.Text,IntVal,Error);
      until (Error = 0);
   {$EndIf}
end;


procedure ReadDefault(Prompt : ShortString;  var RealVal : float64);
begin
   {$IfDef VCL}
      with PETMARCommonForm do repeat
         Caption := 'Input Real Number';
         Panel1.Caption := Prompt;
         Edit1.Text := RealToString(RealVal,-18,-8);
         PETMARCommonForm.FormActivate(Nil);
         ShowModal;
      until CheckEditString(Edit1.Text,RealVal);
   {$EndIf}
end;


procedure ReadDefault(Prompt : ShortString;  var RealVal : float32);
var
   fl : float64;
begin
   fl := RealVal;
   ReadDefault(Prompt,fl);
   RealVal := fl;
end;

procedure ReadDefault(Prompt : ShortString;  var WordVal : word);
var
   fl : int64;
begin
   fl := WordVal;
   ReadDefault(Prompt,fl);
   WordVal := fl;
end;


function GetFileFromDirectory(InMessage : ANSIstring; Mask : ANSIstring; var FileWanted : ANSIString) : boolean;
var
   Masks : ANSIstring;
   i     : byte;
begin
   {$IfDef VCL}
      i := 1;
      if StrUtils.AnsiContainsText(Mask,'|') then Masks := Mask
      else Masks := 'Specified files(' + Mask + ')|' + Mask + '|' + ' All files (*.*)|*.*';
      Result := GetFileMultipleMask(InMessage,Masks,FileWanted,i);
   {$EndIf}
end;




procedure StartProgress(Title : ShortString; ProgressTop : integer = -99; ProgressLeft : integer = -99);
begin
   {$IfDef VCL}
      WantOut := false;
      if WantShowProgress then begin
         if (PetProgF = Nil) then PetProgF := TPetProgF.Create(Application) ;
         PetProgF.Visible := true;
         PetProgF.Caption := Title;
         PetProgF.Gauge1.Progress := 0;
         PetProgF.Gauge1.Visible := true;
         PetProgF.BitBtn1.Visible := false;
         {$IfDef ShowProgressBarSize} WriteLineToDebugFile('Start progress, form height=' + IntToStr(PetProgF.Height ));   {$EndIf}
         if (ProgressTop >= 0) then begin
            PetProgF.Top := ProgressTop;
            PetProgF.Left := ProgressLeft;
         end;
         if PetProgF.NeedToCheckPlacement then CheckFormPlacement(PetProgF);
      end;
      ShowHourglassCursor;
   {$EndIf}
end;

procedure StartProgressAbortOption(Title : ShortString; ProgressTop : integer = -99; ProgressLeft : integer = -99);
begin
   {$IfDef VCL}
      if WantShowProgress then begin
         StartProgress(Title,ProgressTop,ProgressLeft);
         PetProgF.SetUpForAborting;
      end;
   {$EndIf}
end;


procedure UpdateProgressBar(HowFar : float64);
begin
   {$IfDef VCL}
      if WantShowProgress and (PETProgF <> Nil) and (not Math.IsNAN(HowFar)) and (not Math.IsInfinite(HowFar)) then begin
         PetProgF.Gauge1.Progress := round(100.0 * HowFar);
         if HeavyDutyProcessing or MDDef.ProgressBarAlwaysUpperLeftCorner then begin
            //on large screen, lets you web surf and see the progress in screen corner
            PetProgF.Top := 10;
            PetProgF.Left := 10;
         end;
         if WantOut then begin
            EndProgress;
         end;
      end;
   {$EndIf}
end;


procedure EndProgress;
begin
   {$IfDef VCL}
      {$IfDef ShowProgressBarSize} WriteLineToDebugFile('End Progress, form height=' + IntToStr(PetProgF.Height )); {$EndIf}
      if WantShowProgress and (PETProgF <> Nil) then begin
         PetProgF.Visible := false;
      end;
      ShowDefaultCursor;
   {$EndIf}
end;



procedure StopSplashing;
begin
   {$IfDef ExSplash}
   {$Else}
      {$IfDef VCL}
         if (MDSplashForm <> Nil) then begin
            MDStopSplashing;
            MDSplashForm := Nil;
         end;
      {$EndIf}
      ApplicationProcessMessages;
   {$EndIf}
end;


function AnswerIsYes(Prompt : ANSIstring) : boolean;
var
   x,y : integer;
begin
   {$IfDef VCL}
      x := Mouse.CursorPos.X - 25;
      y := Mouse.CursorPos.Y - 75;
      if (x < 10) then x := 10;
      if (y < 10) then y := 10;
      if x > Screen.Monitors[0].Width - 250 then x := Screen.Monitors[0].Width - 250;
      if y > Screen.Monitors[0].Height - 250 then y := Screen.Monitors[0].Height - 250;
      Result := MessageDlgPos(Prompt + '?',mtConfirmation,[mbYes,mbNo],0,x,y) = ID_Yes;
   {$EndIf}
end;

procedure MessageToContinue(Mess : ANSIstring; SaveOutPut : boolean = true);
var
   TStr : tStringList;
   tMess : string;
begin
   {$IfDef VCL}
      if SaveOutPut then begin
         TStr := tStringList.Create;
         while length(Mess) > 0 do TStr.Add(BeforeSpecifiedCharacterANSI(Mess,#13,false,true));
         if (TStr.Count > 20) then begin
            ShowInNotepadPlusPlus(TStr,'');
         end
         else begin
            StringListToContinue(SaveOutPut,TStr);
            TStr.Free;
         end;
      end
      else begin
         tMess := Mess;
         MessageDlg(tMess,mtInformation,[mbOK],0);
      end;
   {$EndIf}
end;


function BitmapSizeString(Bitmap : tMyBitmap) : shortstring;
begin
   Result := ' Bitmap size: ' + BitmapSize(Bitmap);
end;

procedure OpenRecycleBin;
var
    recycleBinPIDL: PItemIDList;
    execInfo: TShellExecuteInfo;
    Handle : tHandle;
begin
    SHGetSpecialFolderLocation(Handle, CSIDL_BITBUCKET, recycleBinPIDL);
    execInfo.cbSize := Sizeof(execInfo) ;
    execInfo.fMask := SEE_MASK_IDLIST;
    execInfo.Wnd := Handle;
    execInfo.lpVerb := nil;
    execInfo.lpFile := nil;
    execInfo.lpParameters := nil;
    execInfo.lpDirectory := nil;
    execInfo.nShow := SW_SHOWNORMAL;
    execInfo.hInstApp:=0;
    execInfo.lpIDList := recycleBinPIDL;
    ShellExecuteEx(@execInfo) ;
end;


procedure GetValidDBfieldname(var fName2 : shortstring);
begin
    repeat
       Petmar.GetString('Field name (<= 10 chars)',fName2,true,DBaseFieldNameChars);
    until length(fname2) <= 10;
end;




function FileCanBeOpened(fName : PathStr) : boolean;
//  1/16/2022 this did not work in opening a database
var
   f : tHandle;
begin
   repeat
     try
         f := FileOpen(fName,fmOpenRead);
         Result := true;
     except
         on EFOpenError do begin
           if AnswerIsYes('Cannot open ' + fName + '; Close it in other program') then begin

           end
           else begin
              Result := false;
              exit;
           end;
         end;
     end;
   until Result;
   FileClose(f);
end;



function GetVolumeName(DriveLetter: ANSIChar): shortstring;
//https://www.swissdelphicenter.ch/en/showcode.php?id=153
var
  dummy : DWORD;
  buffer : array[0..MAX_PATH] of Char;
  oldmode : LongInt;
begin
  oldmode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    GetVolumeInformation(PChar(DriveLetter + ':\'),buffer,SizeOf(buffer),nil,dummy,dummy,nil,0);
    Result := StrPas(buffer);
  finally
    SetErrorMode(oldmode);
  end;
end;


procedure UnblockFile(fName : PathStr);
begin
   {$IfDef VCL}
      if FileExists(fName) then ExecuteFile('type nul > ' + fName + ':Zone.Identifier','','');
   {$EndIf}
end;


procedure WriteOpenHandlestoDebugLog(Where : shortString);

      function GetOpenHandles : DWORD; inline;
      //from https://stackoverflow.com/questions/4966900/delphi-causes-of-system-error-1158-no-more-system-handles-for-current-process
      begin
         if not GetProcessHandleCount(GetCurrentProcess,Result) then RaiseLastOSError;
      end;

begin
   WriteLineToDebugFile(Where + '  handles open=' + IntToStr(GetOpenHandles));
end;



procedure AddSuffixOrPrefixToFiles(PrefixWanted : boolean);
const
   aPath : Pathstr = '';
var
   FilesWanted : tStringList;
   j : integer;
   version,what : shortstring;
   fName,NewName : PathStr;
begin
   repeat
      if PrefixWanted then What := 'prefix' else what := 'suffix';
      GetDOSPath('files to add ' + What,aPath);
      ShowHourglassCursor;
      FilesWanted := tStringList.Create;
      FindMatchingFiles(aPath,'*.*',FilesWanted,2);
      if (FilesWanted.Count > 0) then begin
         Version := LastSubDir(ExtractFilePath(FilesWanted.Strings[0])) + '_';
         Petmar.GetString('file name ' + What,Version,false,ValidDosFileNameChars);
         for j := 0 to pred(FilesWanted.Count) do begin
            fName := FilesWanted.Strings[j];
            if not StrUtils.AnsiContainsText(UpperCase(ExtractFileNameNoExt(fName)),UpperCase(version)) then begin
               if PrefixWanted then NewName := ExtractFilePath(fName) + Version + ExtractFileNameNoExt(fName) + ExtractFileExt(fName)
               else NewName := ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + Version + ExtractFileExt(fName);
               RenameFile(fName, NewName);
            end;
         end;
      end;
      FilesWanted.Free;
      ShowDefaultCursor;
   until not AnswerIsYes('Another directory');
end;





		 
		 
