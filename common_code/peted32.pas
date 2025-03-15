unit peted32;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}


{$Define NoSearchReplace}

interface

uses
   Windows, Classes, Graphics, Controls, Menus,Messages,
   System.UITypes,
   Forms, StdCtrls, Dialogs, ExtCtrls,//Printers,
   PETMAR_types, Buttons, ToolWin, ComCtrls;

type
  TPetEditf = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    FindDialog1: TFindDialog;
    Search1: TMenuItem;
    Find1: TMenuItem;
    Replace1: TMenuItem;
    FindNext1: TMenuItem;
    ReplaceDialog1: TReplaceDialog;
    FontDialog1: TFontDialog;
    Screen1: TMenuItem;
    ToolBar1: TToolBar;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton7: TSpeedButton;
    RichEdit1: TRichEdit;
    NewFile1: TMenuItem;
    FileOpen1: TMenuItem;
    Addlinenumbers1: TMenuItem;
    SpeedButton1: TSpeedButton;
    procedure Addlinenumbers1Click(Sender: TObject);
    procedure NewFile1Click(Sender: TObject);
    procedure FileOpenClick(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure FileExitClick(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure Find(Sender : TObject);
    procedure FindNext1Click(Sender: TObject);
    procedure Replace1Click(Sender: TObject);
    procedure Replace(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Screen1Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    procedure WMClearListeningWindow(var Msg : TMessage); message WM_ClearListeningWindow;
  public
     FileInWindow : PathStr;
     ListenToClear : boolean;
     procedure NoOpenNewOptions;
  end;


implementation

uses
   Petmar,SysUtils,DEMdefs;

{$R *.DFM}
const
  { Default word delimiters are any character except the core alphanumerics. }
  WordDelimiters: set of AnsiChar = [#0..#255] - ['a'..'z','A'..'Z','1'..'9','0'];


{$IfDef NoSearchReplace}
{$Else}

{ SearchMemo scans the text of a TEdit, TMemo, or other TCustomEdit-derived
  component for a given search string.  The search starts at the current
  caret position in the control.  The Options parameter determines whether the
  search runs forward (frDown) or backward from the caret position, whether
  or not the text comparison is case sensitive, and whether the matching
  string must be a whole word.  If text is already selected in the control,
  the search starts at the 'far end' of the selection (SelStart if searching
  backwards, SelEnd if searching forwards).  If a match is found, the
  control's text selection is changed to select the found text and the
  function returns True.  If no match is found, the function returns False. }

{ SearchBuf is a lower-level search routine for arbitrary text buffers.  Same
  rules as SearchMemo above.  If a match is found, the function returns a
  pointer to the start of the matching string in the buffer.  If no match,
  the function returns nil. }




function SearchBuf(Buf: PAnsiChar; BufLen: Integer; SelStart, SelLength: Integer; SearchString : String; Options: TFindOptions): PAnsiChar;
var
  SearchCount,I : Integer;
  C             : AnsiChar;
  Direction     : Shortint;
  CharMap       : array [AnsiChar] of AnsiChar;


  function FindNextWordStart(var BufPtr: pANSIchar): Boolean;
  begin                   { (True XOR N) is equivalent to (not N) }
    //Result := False;    { (False XOR N) is equivalent to (N)    }
     { When Direction is forward (1), skip non delimiters, then skip delimiters. }
     { When Direction is backward (-1), skip delims, then skip non delims }
    while (SearchCount > 0) and
          ((Direction = 1) xor (BufPtr^ in WordDelimiters)) do begin
      Inc(BufPtr, Direction);
      Dec(SearchCount);
    end;
    while (SearchCount > 0) and
          ((Direction = -1) xor (BufPtr^ in WordDelimiters)) do begin
      Inc(BufPtr, Direction);
      Dec(SearchCount);
    end;
    Result := SearchCount > 0;
    if Direction = -1 then begin   { back up one char, to leave ptr on first non delim }
      Dec(BufPtr, Direction);
      Inc(SearchCount);
    end;
  end;

begin
  Result := nil;
  if BufLen <= 0 then Exit;
  if frDown in Options then begin
    Direction := 1;
    Inc(SelStart, SelLength);  { start search past end of selection }
    SearchCount := BufLen - SelStart - Length(SearchString);
    if SearchCount < 0 then Exit;
    if Longint(SelStart) + SearchCount > BufLen then Exit;
  end
  else begin
    Direction := -1;
    Dec(SelStart, Length(SearchString));
    SearchCount := SelStart;
  end;
  if (SelStart < 0) or (SelStart > BufLen) then Exit;
  Result := @Buf[SelStart];

  { Using a Char map array is faster than calling AnsiUpper on every character }
  for C := Low(CharMap) to High(CharMap) do CharMap[C] := C;

  if not (frMatchCase in Options) then begin
    AnsiUpperBuff(pANSIchar(@CharMap), sizeof(CharMap));
    AnsiUpperBuff(@SearchString[1], Length(SearchString));
  end;

  while SearchCount > 0 do begin
    if frWholeWord in Options then
      if not FindNextWordStart(Result) then Break;
    I := 0;
    while (CharMap[Result[I]] = SearchString[I+1]) do begin
      Inc(I);
      if I >= Length(SearchString) then begin
        if (not (frWholeWord in Options)) or (SearchCount = 0) or
           (Result[I] in WordDelimiters) then
          Exit;
        Break;
      end;
    end;
    Inc(Result, Direction);
    Dec(SearchCount);
  end;
  Result := nil;
end;


function SearchMemo(Memo: TCustomEdit; const SearchString: String; Options: TFindOptions): Boolean;
var
  Buffer, P: PAnsiChar;
  Size: Word;
begin
  Result := False;
  if (Length(SearchString) = 0) then Exit;
  Size := Memo.GetTextLen;
  if (Size = 0) then Exit;
  Buffer := StrAlloc(Size + 1);
  try
    Memo.GetTextBuf(Buffer, Size + 1);
    P := SearchBuf(Buffer, Size, Memo.SelStart, Memo.SelLength,
                   SearchString, Options);
    if P <> nil then begin
      Memo.SelStart := P - Buffer;
      Memo.SelLength := Length(SearchString);
      Result := True;
    end;
  finally
    StrDispose(Buffer);
  end;
end;

{$EndIf}

procedure TPetEditf.NoOpenNewOptions;
begin
   SpeedButton5.Visible := false;
   SpeedButton7.Visible := false;
   NewFile1.Visible := false;
   FileOpen1.Visible := false;
   SpeedButton4.Left := 0;
end;


procedure TPetEditf.WMClearListeningWindow;
begin
   if ListenToClear then Close;
end;

procedure TPetEditf.NewFile1Click(Sender: TObject);
begin
  RichEdit1.Clear;
  OpenDialog1.Filename := '';
  Caption := 'Text Editor - [Untitled]';
end;


procedure TPetEditf.FileOpenClick(Sender: TObject);
begin
   with OpenDialog1 do if Execute then begin
      RichEdit1.PlainText := (UpperCase(ExtractFileExt(FileName)) <> '.RTF');
      RichEdit1.Lines.LoadFromFile(FileName);
      Caption := ExtractFilename(FileName) + ' Edit window';
   end;
end;

procedure TPetEditf.Save1Click(Sender: TObject);
begin
   OpenDialog1.Filename := FileInWindow;
   if (OpenDialog1.Filename <> '') then RichEdit1.Lines.SaveToFile(OpenDialog1.Filename)
   else SaveAs1Click(Sender);
end;

procedure TPetEditf.SaveAs1Click(Sender: TObject);
begin
  if FileExists(FileInWindow) then begin
     SaveDialog1.Filename := FileInWindow;
     SaveDialog1.InitialDir := ExtractFilePath(FileInWindow);
  end;
  with SaveDialog1 do if Execute then begin
      RichEdit1.Lines.SaveToFile(Filename);
      Caption := ExtractFilename(FileName);
      OpenDialog1.Filename := Filename;
      FileInWindow := FileName;
    end;
end;

procedure TPetEditf.Find1Click(Sender: TObject);
begin
   FindDialog1.Execute;
   FindNext1.Enabled := True;
end;

procedure TPetEditf.Find(Sender: TObject);
begin
   {$IfDef NoSearchReplace}
   {$Else}
     with Sender as TFindDialog do
       if not SearchMemo(RichEdit1, FindText, Options) then ShowMessage('Cannot find "' + FindText + '".');
   {$EndIf}
end;

procedure TPetEditf.Replace1Click(Sender: TObject);
begin
   {$IfDef NoSearchReplace}
   {$Else}
     ReplaceDialog1.Execute;
   {$EndIf}
end;

procedure TPetEditf.FindNext1Click(Sender: TObject);
begin
   {$IfDef NoSearchReplace}
   {$Else}
     Find(FindDialog1);
   {$EndIf}
end;

{ Replace and ReplaceAll call this routine. }
procedure TPetEditf.Replace(Sender: TObject);
{$IfDef NoSearchReplace}
begin
{$Else}
var
  Found: Boolean;
begin
  with ReplaceDialog1 do begin
    if AnsiCompareText(RichEdit1.SelText, FindText) = 0 then RichEdit1.SelText := ReplaceText;
    Found := SearchMemo(RichEdit1, FindText, Options);
    while Found and (frReplaceAll in Options) do begin
      RichEdit1.SelText := ReplaceText;
      Found := SearchMemo(RichEdit1, FindText, Options);
    end;
    if (not Found) and (frReplace in Options) then ShowMessage('Cannot find "' + FindText + '".');
  end;
{$EndIf}
end;

procedure TPetEditf.Addlinenumbers1Click(Sender: TObject);
var
   i : integer;
begin
   for I := 0 to pred(RichEdit1.Lines.Count) do
      RichEdit1.Lines.Strings[i] := IntegerToString(succ(I),5) + '--' + RichEdit1.Lines.Strings[i];
end;

procedure TPetEditf.FileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TPetEditf.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  MsgResult : Word;
begin
  CanClose := true;
  if RichEdit1.Modified then begin
     MsgResult := MessageDlg(Format('File %s has been modified. Save file?', [OpenDialog1.Filename]), mtWarning, mbYesNoCancel, 0);
     case MsgResult of
        mrYes : Save1Click(Sender);
        //mrNo : CanClose := True;
        mrCancel : CanClose := False;
     end;
  end;
end;

procedure TPetEditf.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
   Self := Nil;
end;


procedure TPetEditf.FormCreate(Sender: TObject);
begin
   Toolbar1.Visible := MDDef.ShowMainToolbar;
   FileInWindow := '';
   {$IfDef NoSearchReplace}
   {$Else}
      Search1.Visible := false;
   {$EndIf}
end;


procedure TPetEditf.Screen1Click(Sender: TObject);
begin
   FontDialog1.Font := RichEdit1.Font;
   if FontDialog1.Execute then begin
      RichEdit1.Font := FontDialog1.Font;
      RichEdit1.Repaint;
   end;
end;


procedure TPetEditf.SpeedButton1Click(Sender: TObject);
begin
   Save1Click(Sender);
end;

procedure TPetEditf.SpeedButton4Click(Sender: TObject);
begin
   RichEdit1.CopyToClipboard;
end;

procedure TPetEditf.SpeedButton5Click(Sender: TObject);
begin
   NewFile1Click(Sender);
end;

procedure TPetEditf.SpeedButton7Click(Sender: TObject);
begin
   FileOpenClick(Sender);
end;

initialization
finalization   
end.
