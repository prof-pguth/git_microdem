{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing Exif metadata in JPEG files  }
{ Version 1.1.2 (2011-01-23)                                                           }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is ExifListForm.pas.                                               }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2011 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordEXIF}
{$EndIf}

unit xifListForm;
{
  Demonstrates listing both standard and MakerNote tags - see ExifListFrame.pas for the
  important code. Note that the approach taken to the two tag types is different. In the
  standard tag case, how tags are interpreted is hardcoded at compile time. In the
  MakerNote case, in contrast, tag interpretation is done on the fly using an INI file
  (MakerNotes.ini), albeit with the intial detection of the basic MakerNote type being
  done by CCR.Exif.pas.
}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, IniFiles, Controls, Forms, Dialogs, StrUtils,
  ExtDlgs, StdCtrls, ExtCtrls, ComCtrls, Buttons, ActnList, System.Actions,
  xif.Demos,
  xifListFrame,
  {$IfDef ExMaps}
  {$Else}
  DEMMapf,
  {$EndIf}
  Petmar,Petmar_types;

type
  TfrmExifList = class(TForm)
    dlgOpen: TOpenPictureDialog;
    ActionList: TActionList;
    actOpen: TAction;
    dlgSave: TSavePictureDialog;
    PageControl: TPageControl;
    tabOriginal: TTabSheet;
    tabResaved: TTabSheet;
    panBtns: TPanel;
    btnOpen: TBitBtn;
    btnExit: TBitBtn;
    btnCopy: TBitBtn;
    actCopy: TAction;
    actSelectAll: TAction;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actCopyUpdate(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
  private
    FActiveFrame, FOriginalFrame, FResavedFrame: TOutputFrame;
    FMakerNoteValueMap: TMemIniFile;
  protected
    procedure DoFileOpen(const FileName1, FileName2: string); override;
  public
     {$IfDef ExMaps}
     {$Else}
     BaseMap : tMapForm;
     {$EndIf}
     CurrentImage : integer;
     TheFiles : tStringList;
     procedure DoJPEGFile(fName : PathStr);
     function GetLatLong(var Lat, Long: float) : boolean;
  end;

var
  frmExifList: TfrmExifList;

implementation

uses
   ShellApi,
   xif,
   DEMdefs,
   Make_Tables;

{$R *.dfm}


procedure TfrmExifList.FormCreate(Sender: TObject);
begin
  PageControl.Visible := TestMode;
  FMakerNoteValueMap := TMemIniFile.Create(ExtractFilePath(Application.ExeName) + 'MakerNotes.ini');
  FOriginalFrame := TOutputFrame.Create(Self);
  FOriginalFrame.Align := alClient;
  FOriginalFrame.Name := '';
  if not TestMode then
    FOriginalFrame.Parent := Self
  else begin
    actOpen.Enabled := False;
    actOpen.Visible := False;
    ActiveControl := PageControl;
    FOriginalFrame.Parent := tabOriginal;
    FResavedFrame := TOutputFrame.Create(Self);
    FResavedFrame.Align := alClient;
    FResavedFrame.Parent := tabResaved;
  end;
  FActiveFrame := FOriginalFrame;
  SupportOpeningFiles := True;
  {$IfDef ExMaps}
  {$Else}
  BaseMap := Nil;
  {$EndIf}
  TheFiles := Nil;
end;

procedure TfrmExifList.FormDestroy(Sender: TObject);
begin
  FMakerNoteValueMap.Free;
end;


function TfrmExifList.GetLatLong(var Lat, Long: float) : boolean;
var
  ExifData: TExifData;
  Item: TListItem;
  Strings: TStringList;
  LatStr,LongStr : ANSIstring;
begin
   FActiveFrame.GetLatLong(LatStr,LongStr);
   {$IfDef RecordEXIF}
   WriteLineToDebugFile('TfrmExifList.GetLatLong in;  lat=' + LatStr + '   long=' + LongStr,true);
   {$EndIf}
   Result := (latStr <> '') and (LongStr <> '');
   if Result then begin
      Lat := StrToFloat(petmar_types.BeforeSpecifiedCharacter(LatStr,'°',true,true));
      Delete(LatStr,1,2);
      Lat := Lat + StrToFloat(petmar_types.BeforeSpecifiedCharacter(LatStr,' ',true,true)) / 60;
      petmar_types.BeforeSpecifiedCharacter(LatStr,'d',true,true);
      Delete(LatStr,1,1);
      Lat := Lat + StrToFloat(petmar_types.BeforeSpecifiedCharacter(LatStr,' ',true,true)) / 3600;
      if StrUtils.AnsiContainsText(LatStr,'south') then Lat := -Lat;

      Long := StrToFloat(petmar_types.BeforeSpecifiedCharacter(LongStr,'°',true,true));
      Delete(LongStr,1,2);
      Long := Long + StrToFloat(petmar_types.BeforeSpecifiedCharacter(LongStr,' ',true,true)) / 60;
      petmar_types.BeforeSpecifiedCharacter(LongStr,'d',true,true);
      Delete(LongStr,1,1);
      Long := Long + StrToFloat(petmar_types.BeforeSpecifiedCharacter(LongStr,' ',true,true)) / 3600;
      if StrUtils.AnsiContainsText(LongStr,'west') then Long := -Long;
   end;
end;

procedure TfrmExifList.DoJPEGFile(fName : PathStr);
var
   Lat,Long : float;
   //OrientationStr : ANSIString;
begin
   OpenFile(fName);
   //FActiveFrame.GetOrientation(OrientationStr);
  {$IfDef ExMaps}
  {$Else}
   if (BaseMap <> Nil) then begin
     GetLatLong(Lat, Long);
     BaseMap.DoFastMapRedraw;
     BaseMap.MapDraw.MapSymbolAtLatLongDegree(BaseMap.Image1.Canvas,Lat,Long,FilledBox,4,clRed);
   end;
   {$EndIf}
end;

procedure TfrmExifList.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage = tabOriginal then
    FActiveFrame := FOriginalFrame
  else
    FActiveFrame := FResavedFrame;
end;

procedure TfrmExifList.actCopyExecute(Sender: TObject);
begin
  FActiveFrame.CopyToClipboard;
end;

procedure TfrmExifList.actCopyUpdate(Sender: TObject);
begin
  actCopy.Enabled := FActiveFrame.CanCopyToClipboard;
end;

procedure TfrmExifList.actOpenExecute(Sender: TObject);
begin
  {$IfDef RecordEXIF}
  WriteLineToDebugFile('TfrmExifList.actOpenExecute',true);
  {$EndIf}
  if dlgOpen.Execute then begin
     {$IfDef RecordEXIF}
     WriteLineToDebugFile('picked=' + dlgOpen.FileName);
     {$EndIf}
     DoJPEGFile(dlgOpen.FileName);
     ImageDirectory := ExtractFilePath(dlgOpen.FileName);
     BitBtn1Click(nil);
  end;
end;

procedure TfrmExifList.actSelectAllExecute(Sender: TObject);
begin
  FActiveFrame.SelectAll;
end;


procedure TfrmExifList.BitBtn1Click(Sender: TObject);
var
  List2 : tStringList;
  i : integer;
begin
  {$IfDef RecordEXIF}
  WriteLineToDebugFile('TfrmExifList.BitBtn1Click',true);
  {$EndIf}
   if (Sender <> Nil) then Petmar.GetDOSPath('with EXIF files',ImageDirectory);
   if (TheFiles <> Nil) then begin
      TheFiles.Free;
      TheFiles := Nil;
   end;

   FindMatchingFiles(ImageDirectory,'*.jpg',TheFiles,5);
   List2 := Nil;
   FindMatchingFiles(ImageDirectory,'*.jpeg',List2,5);
   for i := 0 to pred(List2.Count) do TheFiles.Add(List2.Strings[i]);
   List2.Destroy;
   if (TheFiles.Count = 0) then MessageToContinue('No JPEGs in ' + ImageDirectory)
   else begin
     CurrentImage := 0;
     DoJPEGFile(TheFiles.Strings[0]);
     BitBtn2.Enabled := true;
     BitBtn3.Enabled := true;
     BitBtn4.Enabled := true;
   end;
end;


procedure TfrmExifList.BitBtn2Click(Sender: TObject);
begin
   inc(CurrentImage);
   if CurrentImage = TheFiles.Count then CurrentImage := 0;
   DoJPEGFile(TheFiles.Strings[CurrentImage]);
end;

procedure TfrmExifList.BitBtn3Click(Sender: TObject);
begin
   dec(CurrentImage);
   if CurrentImage < 0 then CurrentImage := pred(TheFiles.Count);
   DoJPEGFile(TheFiles.Strings[CurrentImage]);
end;

procedure TfrmExifList.BitBtn4Click(Sender: TObject);
var
   fName : PathStr;
   Dir, Lat,Long : float;
   NumLoc,NumNoLoc,
   i : integer;
   MyTable : tMyData;
   TStr : shortstring;
   Locs : tStringList;
begin
   fName := ImageDirectory + 'photo_locations' + DefaultDBExt;
   {if not FileExists(fName) then} Make_Tables.MakePhotoIndex(fName);
   NumLoc := 0;
   NumNoLoc := 0;
   Locs := tStringList.Create;
   MyTable := tMyData.Create(fName);
   for i := 0 to pred(TheFiles.Count) do begin
      fName := TheFiles.Strings[i];
      OpenFile(fName);
      if GetLatLong(Lat, Long) then begin
         MyTable.Insert;
         MyTable.SetFieldByNameAsFloat('LAT', Lat);
         MyTable.SetFieldByNameAsFloat('LONG', Long);
         MyTable.SetFieldByNameAsString('IMAGE', fName);
         if FActiveFrame.GetViewDirection(Dir) then MyTable.SetFieldByNameAsFloat('AZIMUTH',Dir);
         TStr := MyTable.GetFieldByNameAsString('LAT') + '/' + MyTable.GetFieldByNameAsString('LONG');
         Locs.Add(Tstr);
         MyTable.Post;
         inc(NumLoc);
      end
      else inc(NumNoLoc);
   end;
   MyTable.Destroy;
   MessageToContinue('Images with location: ' + IntToStr(NumLoc) + MessLineBreak +
                            'Images w/o  location: ' + IntToStr(NumNoLoc) + MessLineBreak +
                            'Different locations: ' + IntToStr(Locs.Count));
   Locs.Destroy;
end;


procedure TfrmExifList.btnExitClick(Sender: TObject);
begin
   Close;
end;


procedure TfrmExifList.DoFileOpen(const FileName1, FileName2: string);
begin
  FOriginalFrame.LoadFromFile(FileName1, FMakerNoteValueMap);
  if FileName2 = '' then Exit;
  FResavedFrame.LoadFromFile(FileName2, FMakerNoteValueMap);
  tabOriginal.Caption := ExtractFileName(FileName1);
  tabResaved.Caption := ExtractFileName(FileName2);
end;


initialization
finalization
  {$IfDef RecordEXIF}
  WriteLineToDebugFile('RecordEXIF active in xifListForm');
  {$EndIf}
end.
