unit get_thumbnails;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TThumbnailForm = class(TForm)
    RadioGroup1: TRadioGroup;
    Edit1: TEdit;
    Label1: TLabel;
    BitBtn1: TBitBtn;
    Edit2: TEdit;
    CheckBox1: TCheckBox;
    HelpBtn: TBitBtn;
    OKBtn: TBitBtn;
    CheckBox2: TCheckBox;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure AlbumInventory;

implementation

{$R *.dfm}

uses
   Petmar,Petmar_types,DEMDefs,PETImage,
   nevadia_main;

var
   ThumbnailDir    : PathStr;

procedure AlbumInventory;
var
   ThisFile : tStringList;
   OutName,
   fName   : PathStr;
   j,Loop : integer;
   HTMLForm : boolean;
   DoTable : boolean;
   //NPics        : integer;
   Mask : shortstring;
   theFiles : tStringList;
   bName : NameStr;
   Ext,OutExt : ExtStr;
   ThumbnailForm: TThumbnailForm;
begin
   ThumbnailForm := TThumbnailForm.Create(Application);
   with ThumbnailForm do begin
      Edit1.Text := IntToStr(MDDef.tnHeight);
      Edit2.Text := ThumbnailDir;
      ShowModal;
      CheckEditString(Edit1.Text,MDDef.tnHeight);
      ThumbNailDir := Edit2.Text;
      HTMLForm := CheckBox1.Checked;
      OutExt := RadioGroup1.Items[RadioGroup1.ItemIndex];
      DoTable := CheckBox2.Checked;
   end;

   if HTMLForm then begin
      ThisFile := tStringList.Create;
      ThisFile.Add(StartHTMLString);
      ThisFile.Add('<head>');
      ThisFile.Add('<p align="center"><font size="6"><strong>Photo Album</strong></font></p>');
      ThisFile.Add('</head>');
      ThisFile.Add('<p>');
      ThisFile.Add('<b>Click for full size picture</b><p><p>');
      ThisFile.Add('<body bgcolor="#00FFFF">');
      if DoTable then ThisFile.Add(StartTableString);
   end;

   //NPics := 0;
   for Loop := 1 to 5 do begin
      case Loop of
         1 : Mask := '*.jpg';
         2 : Mask := '*.png';
         3 : Mask := '*.gif';
         4 : Mask := '*.tif';
         5 : Mask := '*.bmp';
      end;
      TheFiles := nil;
      Petmar.FindMatchingFiles(ThumbNailDir,Mask,TheFiles);
      StartProgress(Mask);
      for j := 0 to pred(TheFiles.Count) do begin
         UpdateProgressBar(j/ TheFiles.Count);
         fName := TheFiles.Strings[j];
         FSplit(fName,ThumbNailDir,bName,Ext);
         if Copy(bName,1,3) <> 'TN-' then begin
            wmDEM.StatusBar1.Panels[0].Text := bName;
            OutName := ThumbNailDir + 'TN-' + bName + OutExt;
            CreateThumbNail(fname,OutName,MDDef.tnHeight,MDDef.tnQuality);
            if HTMLForm then begin
               if DoTable then ThisFile.Add(StartRowString + StartColumnString);
               ThisFile.Add('<a href="' + ExtractFileName(fName) + '"> <img src="' + ExtractFileName(OutName) + '"></a>');
               if DoTable then begin
                  ThisFile.Add(EndColumnString + StartColumnString + ExtractFileNameNoExt(fName) + EndColumnString + EndRowString);
               end;
            end;
            //inc(NPics);
            ApplicationProcessMessages;
         end;
      end;
      EndProgress;
   end;
   if HTMLForm then begin
      if DoTable then ThisFile.Add(EndTableString);
      ThisFile.Add('</p></body>' + EndHTMLString);
      ThisFile.SaveToFile(ThumbNailDir + 'index.htm');
      ThisFile.Free;
      ExecuteFile(ThumbNailDir + 'index.htm', '', '');
   end;
   wmDEM.StatusBar1.Panels[0].Text := '';
end;


procedure TThumbnailForm.BitBtn1Click(Sender: TObject);
begin
   Petmar.GetDOSPath('Path for thumbnails',ThumbnailDir);
   Edit2.Text := ThumbnailDir;
end;


procedure TThumbnailForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TThumbnailForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html/thumbnails.htm');
end;

procedure TThumbnailForm.OKBtnClick(Sender: TObject);
begin
   Close;
end;


initialization
   ThumbnailDir := '';
finalization;
end.
