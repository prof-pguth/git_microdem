unit peted32;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}


{$Define NoSearchReplace}

interface

uses
   Windows, Classes, Graphics, Controls, Menus,Messages,
   System.UITypes,
   Forms, StdCtrls, Dialogs, ExtCtrls,
   PETMAR_types, Buttons, ToolWin, ComCtrls;

type
  TPetEditf = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    //FindDialog1: TFindDialog;
    //Search1: TMenuItem;
    //Find1: TMenuItem;
    //Replace1: TMenuItem;
    //FindNext1: TMenuItem;
    //ReplaceDialog1: TReplaceDialog;
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
    //procedure Find1Click(Sender: TObject);
   // procedure Replace(Sender: TObject);
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
