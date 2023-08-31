{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing Exif metadata in JPEG files  }
{ Version 1.5.1                                                                        }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is JpegDumpForm.pas.                                               }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2012 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}

unit JpegDumpForm;
{
  Demonstrates using the enumerator functions of CCR.Exif.JPEGUtils to parse a JPEG
  file's structure - see JpegDumpOutputFrame.pas for the actual parsing code.
  While the implementation of JPEGHeader may look a bit funky if you're unused to
  the details of Delphi's for/in loop support, its use should be straightforward.
}
interface

{$I nevadia_defines.inc}

uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end DB declarations

  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtDlgs,
  Buttons, StdCtrls, ExtCtrls, ComCtrls, ActnList, StdActns,
  System.Math,System.Actions,
  CCR.Exif.Demos,
  JpegDumpOutputFrame,
  DEMMapF,Petmar_types, Petmar;

type
  TNewfrmJpegDump = class(TForm)
    panBtns: TPanel;
    btnOpen: TBitBtn;
    btnExit: TBitBtn;
    dlgOpen: TOpenPictureDialog;
    ActionList: TActionList;
    EditSelectAll1: TEditSelectAll;
    actOpen: TAction;
    dlgSave: TSavePictureDialog;
    actReload: TAction;
    PageControl: TPageControl;
    tabOriginal: TTabSheet;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    BitBtn3: TBitBtn;
    procedure btnExitClick(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
  private
    FOriginalFrame  : TNewOutputFrame;
  protected
    procedure DoFileOpen(const FileName1 : string); override;
  public
     TheFiles : tStringList;
     CurrentImage : integer;
     MapOwner : tMapForm;
  end;


procedure StartExif;
function GetPhotoInfo(fName : PathStr) : shortstring;
procedure MakePhotoDB(fName : PathStr);


implementation

uses
   ShellApi, CCR.Exif,
   Make_tables,DEMDefs;

{$R *.dfm}


function GetPhotoInfo(fName : PathStr) : shortstring;
var
  NewfrmJpegDump :   TNewfrmJpegDump;
begin
  NewfrmJpegDump := TNewfrmJpegDump.Create(Application);
  NewfrmJpegDump.OpenFile(fName);
  Result := NewfrmJpegDump.FOriginalFrame.FocalLength + ' ISO=' + NewfrmJpegDump.FOriginalFrame.ISO + '  f_stop=' + NewfrmJpegDump.FOriginalFrame.fStop  + '  shutter=' + NewfrmJpegDump.FOriginalFrame.Shutter;
  NewfrmJpegDump.Close;
end;


procedure MakePhotoDB(fName : PathStr);
var
  NewfrmJpegDump : TNewfrmJpegDump;
begin
  NewfrmJpegDump := TNewfrmJpegDump.Create(Application);
  NewfrmJpegDump.FOriginalFrame.Enabled := false;
  NewfrmJpegDump.FOriginalFrame.Memo.Enabled := false;
  PhotoDir := fName;
  NewfrmJpegDump.BitBtn2Click(nil);
  NewfrmJpegDump.Close;
end;



procedure StartExif;
var
  NewfrmJpegDump : TNewfrmJpegDump;
begin
  NewfrmJpegDump := TNewfrmJpegDump.Create(Application);
end;

procedure TNewfrmJpegDump.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TNewfrmJpegDump.FormCreate(Sender: TObject);
begin
   FOriginalFrame := TNewOutputFrame.Create(Self);
   FOriginalFrame.Align := alClient;
   FOriginalFrame.Name := '';
   FOriginalFrame.Parent := Self;
   SupportOpeningFiles := True;
   MapOwner := nil;
end;


procedure TNewfrmJpegDump.actOpenExecute(Sender: TObject);
begin
   if dlgOpen.Execute then begin
      OpenFile(dlgOpen.FileName);
      PhotoDir := ExtractFilePath(dlgOpen.FileName);
   end;
end;

procedure TNewfrmJpegDump.BitBtn1Click(Sender: TObject);
var
  List2 : tStringList;
  i : integer;
begin
  {$IfDef RecordEXIF}
  WriteLineToDebugFile('TfrmExifList.BitBtn1Click',true);
  {$EndIf}
   if (Sender <> Nil) then Petmar.GetDOSPath('with EXIF files',PhotoDir);

   if (TheFiles <> Nil) then begin
      TheFiles.Free;
      TheFiles := Nil;
   end;

   FindMatchingFiles(PhotoDir,'*.jpg',TheFiles,5);
   List2 := Nil;
   FindMatchingFiles(PhotoDir,'*.jpeg',List2,5);
   for i := 0 to pred(List2.Count) do TheFiles.Add(List2.Strings[i]);
   List2.Destroy;

   for i := pred(TheFiles.Count) downto 0 do begin
      if UpperCase(Copy(ExtractFileName(TheFiles.Strings[i]),1,3)) = 'TN-' then begin
         TheFiles.Delete(i);
      end;
   end;
   if (TheFiles.Count = 0) then MessageToContinue('No JPEGs in ' + PhotoDir)
   else begin
     CurrentImage := 0;
     OpenFile(TheFiles.Strings[CurrentImage]);
   end;
end;

procedure TNewfrmJpegDump.BitBtn2Click(Sender: TObject);
var
   fName,fName2 : PathStr;
   NumLoc,NumNoLoc,i : integer;
   HasPosition : boolean;
   MyTable : tMyData;
   EV100 : float64;
   TStr : shortstring;
   Locs : tStringList;
begin
   if (TheFiles = Nil) then begin
      BitBtn1Click(Sender);
   end;

   fName2 := PhotoDir + 'photo_index' + DefaultDBExt;
   Make_Tables.MakePhotoIndex(fName2);
   NumLoc := 0;
   NumNoLoc := 0;
   Locs := tStringList.Create;
   Locs.Sorted := true;
   Locs.Duplicates := dupIgnore;
   MyTable := tMyData.Create(fName2);
   StartProgress('Make DB, pix=' + IntToStr(TheFiles.Count));

   for i := 0 to pred(TheFiles.Count) do begin
      if (i mod 10 = 0) then UpdateProgressBar(i/TheFiles.Count);

      fName := TheFiles.Strings[i];
      OpenFile(fName);
      try
         HasPosition := (FOriginalFrame.PhotoLat > -998) and (FOriginalFrame.PhotoLong > -998);
      except
         on exception do HasPosition := false;
      end;
      if HasPosition then inc(NumLoc) else inc(NumNoLoc);

      if CheckBox1.Checked or HasPosition then begin
         MyTable.Insert;
         if HasPosition then begin
            try
               MyTable.SetFieldByNameAsFloat('LAT', FOriginalFrame.PhotoLat);
               MyTable.SetFieldByNameAsFloat('LONG', FOriginalFrame.PhotoLong);
               TStr := RealToString(FOriginalFrame.PhotoLat,-12,-8) + '/' + RealToString(FOriginalFrame.PhotoLong,-12,-8);
               Locs.Add(Tstr);
            except
               on exception do begin end;
            end;
         end;
         if (FOriginalFrame.GPSDateStamp <> '') then MyTable.SetFieldByNameAsString('GPS_DATE', FOriginalFrame.GPSDateStamp)
         else MyTable.SetFieldByNameAsString('GPS_DATE', FOriginalFrame.DateTime);
         if (not CheckBox2.Checked) then begin
             Delete(fName,1,length(PhotoDir));
            //fName := ExtractFileName(fName);
         end;

         MyTable.SetFieldByNameAsString('IMAGE', fName);
         MyTable.SetFieldByNameAsString('AZIMUTH',FOriginalFrame.PhotoAzimuth);
         MyTable.SetFieldByNameAsString('ALTITUDE',FOriginalFrame.PhotoAltitude);
         MyTable.SetFieldByNameAsString('FOCAL_LEN',FOriginalFrame.FocalLength);
         MyTable.SetFieldByNameAsInteger('ORIENT',FOriginalFrame.Orient);
         MyTable.SetFieldByNameAsString('ISO',FOriginalFrame.ISO);
         MyTable.SetFieldByNameAsString('F_STOP',FOriginalFrame.fStop);
         MyTable.SetFieldByNameAsString('FOCAL_35',FOriginalFrame.Focal35);
         MyTable.SetFieldByNameAsString('SHUTTER',FOriginalFrame.Shutter);
         MyTable.SetFieldByNameAsString('CAMERA',FOriginalFrame.CameraName);

         EV100 := ln(sqr(StrToFloat(FOriginalFrame.fStop)) / FOriginalFrame.Speed) / ln(2)   + ln(100 / StrToFloat(FOriginalFrame.ISO)) / ln(2);
         MyTable.SetFieldByNameAsFloat('EV100',EV100);
         MyTable.Post;
      end;
   end;
   MyTable.Destroy;
   EndProgress;
   if (Sender <> Nil) then MessageToContinue('Images with location: ' + IntToStr(NumLoc) + MessLineBreak + '  w/o  location: ' + IntToStr(NumNoLoc) + MessLineBreak + 'Unique locations: ' + IntToStr(Locs.Count));
   Locs.Destroy;
   if (MapOwner <> Nil) then MapOwner.OpenDBonMap('',fName2,true);
end;


procedure TNewfrmJpegDump.BitBtn3Click(Sender: TObject);
begin
   if CurrentImage < TheFiles.Count-2 then inc (CurrentImage);
   OpenFile(TheFiles.Strings[CurrentImage]);
end;


procedure TNewfrmJpegDump.btnExitClick(Sender: TObject);
begin
   Close;
end;

procedure TNewfrmJpegDump.CheckBox3Click(Sender: TObject);
begin
   FOriginalFrame.Verbose := CheckBox3.Checked;
end;

procedure TNewfrmJpegDump.DoFileOpen(const FileName1 : string);
begin
  FOriginalFrame.LoadFromFile(FileName1);
  if actReload.Visible then actReload.Enabled := True;
  tabOriginal.Caption := ExtractFileName(FileName1);
end;



end.
