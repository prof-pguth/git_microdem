unit stereo_viewer;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordStereoViewerProblems}
{$EndIf}

interface

uses
  Windows, SysUtils,  Classes, Graphics,  Forms,
  Petmar_types, Menus, Vcl.Dialogs, Vcl.ExtDlgs, Vcl.Controls, Vcl.ExtCtrls;

type
  TStereoViewerForm = class(TForm)
    Image1: TImage;
    Image2: TImage;
    MainMenu1: TMainMenu;
    Flipimages1: TMenuItem;
    Anaglyph1: TMenuItem;
    Twinview1: TMenuItem;
    Save1: TMenuItem;
    SaveJPSstereo1: TMenuItem;
    Save2: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure FormResize(Sender: TObject);
    procedure Flipimages1Click(Sender: TObject);
    procedure Anaglyph1Click(Sender: TObject);
    procedure Twinview1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure SaveJPSstereo1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    fName1,fName2 : PathStr;
    procedure ShowPix;
  end;


procedure ShowStereoPair(file1,file2 : PathStr);


implementation

{$R *.dfm}


uses
   DEMDefs,PetImage, petmar;


procedure ShowStereoPair(file1,file2 : PathStr);
var
  StereoViewerForm : TStereoViewerForm;
begin
  StereoViewerForm := TStereoViewerForm.Create(Application);
  if (File1 = '') and StereoViewerForm.OpenPictureDialog1.Execute then File1 := StereoViewerForm.OpenPictureDialog1.FileName;
  if (File2 = '') and StereoViewerForm.OpenPictureDialog1.Execute then File2 := StereoViewerForm.OpenPictureDialog1.FileName;
  StereoViewerForm.fName1 := file1;
  StereoViewerForm.fName2 := file2;
  StereoViewerForm.ShowPix;
  StereoViewerForm.Show;
end;


procedure TStereoViewerForm.Save1Click(Sender: TObject);
begin
   PetImage.SaveImageAsBMP(Image1);
end;

procedure TStereoViewerForm.SaveJPSstereo1Click(Sender: TObject);
var
   BMP1,BMP2,BMP3 : tMyBitmap;
   fName : PathStr;
begin
   fName := MDTempDir + 'three3.jps';
   if GetNew3DGraphicsFileName('3D image',fName) then begin
       BMP1 := LoadBitmapFromFile(fName1);
       BMP2 := LoadBitmapFromFile(fName2);
       CreateBitmap(BMP3,2*Bmp1.Width,bmp1.Height);
       Bmp3.Canvas.Draw(0,0,bmp1);
       Bmp3.Canvas.Draw(BMP1.Width,0,bmp2);
       PetImage.SaveBitmap(BMP3,fName);
       BMP1.Free;
       BMP2.Free;
       BMP3.Free;
   end;
end;

procedure TStereoViewerForm.ShowPix;

   procedure Load(fName : PathStr; Image : tImage; Left : integer);
   var
      Bitmap : tMyBitmap;
   begin
      Bitmap := LoadBitmapFromFile(fName);
      Image.Left := Left + ((ClientWidth div 2) - Bitmap.Width) div 2;
      Image.Top := (ClientHeight - Bitmap.Height) div 2;
      Image.Width := Bitmap.Width;
      Image.Height := Bitmap.Height;
      Image.Picture.Graphic := Bitmap;
      Bitmap.Free;
   end;

begin
   if FileExists(fName1) and FileExists(fName2) then begin
      WindowState := wsMaximized;
      Color := clBlack;
      Load(fName1,Image1,0);
      Load(fName2,Image2,(ClientWidth div 2) );
      {$IfDef RecordStereoViewerProblems}
         WriteLineToDebugFile('TStereoViewerForm.ShowPix');
         WriteLineToDebugFile('ClientWidth = ' + IntToStr(ClientWidth) +  '  Image1.Width = ' + IntToStr(Image1.Width));
         WriteLineToDebugFile('Image1.Left = ' + IntToStr(Image1.Left) + '   Image1.Top = ' + IntToStr(Image1.Top));
      {$EndIf}
   end;
end;


procedure TStereoViewerForm.Twinview1Click(Sender: TObject);
begin
   ShowPix;
   TwinView1.Enabled := false;
   Anaglyph1.Enabled := true;
   Image2.Visible := true;
   Save1.Enabled := false;
end;

procedure TStereoViewerForm.FormResize(Sender: TObject);
begin
   if (Sender <> Anaglyph1) then ShowPix;
   {$IfDef RecordStereoViewerProblems}
      WriteLineToDebugFile('TStereoViewerForm.FormResize');
      WriteLineToDebugFile('ClientWidth = ' + IntToStr(ClientWidth));
      WriteLineToDebugFile('Image1.Left = ' + IntToStr(Image1.Left));
      WriteLineToDebugFile('Image1.Top = ' + IntToStr(Image1.Top));
      WriteLineToDebugFile('Image1.Width = ' + IntToStr(Image1.Width));
   {$EndIf}
end;


procedure TStereoViewerForm.Flipimages1Click(Sender: TObject);
var
   tStr : PathStr;
begin
   tStr := fName1;
   fName1 := fName2;
   fName2 := tStr;
   ShowPix;
end;


procedure TStereoViewerForm.Anaglyph1Click(Sender: TObject);
var
   Bitmap3 : tMyBitmap;
begin
   Bitmap3 := AnaglyphFromTwoBitmaps(fName1,FName2);
    Image1.Left := (ClientWidth - Bitmap3.Width) div 2;
    if (Image1.Left < 0) then Image1.Left := 0;
    Image1.Top := (ClientHeight - Bitmap3.Height) div 2;
    Image1.Width := Bitmap3.Width;
    Image1.Height := Bitmap3.Height;
    Image1.Picture.Graphic := Bitmap3;
    Image2.Visible := false;

    {$IfDef RecordStereoViewerProblems}
       WriteLineToDebugFile('TStereoViewerForm.Anaglyph1Click');
       WriteLineToDebugFile('ClientWidth = ' + IntToStr(ClientWidth));
       WriteLineToDebugFile('Image1.Left = ' + IntToStr(Image1.Left));
       WriteLineToDebugFile('Image1.Top = ' + IntToStr(Image1.Top));
       WriteLineToDebugFile('Image1.Width = ' + IntToStr(Image1.Width));
    {$EndIf}

   Bitmap3.Free;
   TwinView1.Enabled := true;
   Anaglyph1.Enabled := false;
   Save1.Enabled := true;
end;

initialization
finalization
  {$IfDef RecordStereoViewerProblems} WriteLineToDebugFile('RecordStereoViewerProblems active in stereo_viewer'); {$EndIf}
end.
