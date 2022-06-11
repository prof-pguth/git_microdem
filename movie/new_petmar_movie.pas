unit new_petmar_movie;


////////////////////////////////////////////////////////////////////////////////
// Adpated from                                                               //
// Project:	TGIFImage demo application.                                       //
// Description:	Simple GIF Animation Builder.                               //
// Copyright	(c) 1997-2008 Anders Melander.                                 //
//		All rights reserved.                                                    //
//  http://melander.dk/articles/gifanimate/                                   //
////////////////////////////////////////////////////////////////////////////////

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$IfDef Debug}
      {$Define RecordMovie}
   {$Else}
   {$EndIf}
{$EndIf}



interface

uses
  GIFImg,ClipBrd,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Vcl.Imaging.pngimage, Vcl.Buttons,
  Petmar_types;

type
  TFormAnimate = class(TForm)
    ScrollBoxSource: TScrollBox;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Panel1: TPanel;
    PanelPreview: TPanel;
    ImageAnimate: TImage;
    Panel3: TPanel;
    ButtonAnimate: TButton;
    ButtonSave: TButton;
    SaveDialog: TSaveDialog;
    GroupBox1: TGroupBox;
    CheckBoxCrop: TCheckBox;
    CheckBoxMerge: TCheckBox;
    CheckBoxPalette: TCheckBox;
    StatusBar1: TStatusBar;
    Label1: TLabel;
    Edit1: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    CheckBox1: TCheckBox;
    procedure ButtonAnimateClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MovieList : tStringList;
    MovieName, MoviePath : PathStr;
  end;


procedure CreateNewMovie(fName : PathStr = ''; CreateNew : boolean = false);


implementation

uses
   Petmar,PetImage,
   DEMdefs;


{$R *.DFM}


procedure CreateNewMovie;
var
  FormAnimate : TFormAnimate;
begin
   {$IfDef RecordMovie} WriteLineToDebugFile('CreateNewMovie in'); {$EndIf}
   FormAnimate := TFormAnimate.Create(Application);
   if CreateNew then begin
      FormAnimate.BitBtn4Click(Nil);
      FormAnimate.Show;
   end
   else begin
      if (fName = '') then begin
         FormAnimate.BitBtn2Click(Nil);
      end
      else with FormAnimate do begin
         MovieList.LoadFromFile(fName);
         MovieName := fName;
         MoviePath := ExtractFilePath(fName);
      end;

      FormAnimate.ButtonAnimateClick(Nil);
      FormAnimate.Show;
   end;
end;


procedure TFormAnimate.FormCreate(Sender: TObject);
begin
   MovieName := DEMDefs.MovieDir;
   MovieList := tStringList.Create;
   Edit1.Text := IntToStr(MDDef.GIFDefaultDelay);
   CheckBox1.Checked := MDDef.GIFfileLabels;

  // Enable GIF animation and transparency
  GIFImageDefaultAnimate := True;
  GIFImageDefaultTransparent := True;

  // Avoid animation flickering by double buffering the TImage parent
  PanelPreview.DoubleBuffered := True;
end;


procedure TFormAnimate.BitBtn1Click(Sender: TObject);
begin
   ClipBoard.Assign(ImageAnimate.Picture);
end;


procedure TFormAnimate.BitBtn2Click(Sender: TObject);
var
   DefExt : byte;
begin
   DefExt := 0;
   if GetFileMultipleMask('movie','Any movie|*.MOV',MovieName,DefExt) then begin
      MovieList.LoadFromFile(MovieName);
      MoviePath := ExtractFilePath(MovieName);
   end;
end;

procedure TFormAnimate.BitBtn3Click(Sender: TObject);
begin
   ModalEditWindow(MovieName,'Reorder frames in the movie');
   MovieList.LoadFromFile(MovieName);
   ButtonAnimateClick(Sender);
end;


procedure TFormAnimate.BitBtn4Click(Sender: TObject);
var
   fName,OldName : pathStr;
   i : integer;
   FilesWanted : tStringList;
   DefaultFilter : byte;
begin
   DefaultFilter := 1;
   FilesWanted := tStringList.Create;
   FilesWanted.Add(ExtractFilePath(LastDataBase));
   if GetMultipleFiles('Movie images','Any image format|*.png;*.jpg;*.jp2;*.gif;*.bmp;*.tif;*.tiff;*.kml;*.kmz|PNG images|*.png|TIFF images|*.tif;*.tiff',FilesWanted,DefaultFilter) then begin
      MovieList.Clear;
      for i := 0 to pred(FilesWanted.Count) do begin
         FName := FilesWanted.Strings[i];
         OldName := fName;
         CleanUpFileName(fname);
         RenameFile(OldName,Fname);
         fName := ExtractFileName(fName);
         MovieList.Add(fName);
      end;
      MovieDir := ExtractFilePath(OldName);
      MovieName := MovieDir + 'movie.mov';
      MovieList.SaveToFile(MovieName);
   end;
   FilesWanted.Destroy;
   ButtonAnimateClick(Nil);
end;



procedure TFormAnimate.ButtonAnimateClick(Sender: TObject);
var
  GIF: TGIFImage;
  MovieX,MovieY : integer;

           function TransparentIndex(Frame: TGIFFrame): byte;
           begin
             // Use lower left pixel as transparent color
             Result := Frame.Pixels[0, Frame.Image.Height-1];
           end;

           function AddBitmap(GIF: TGIFImage; Source: TGraphic; Transparent: boolean): TGIFFrame;
           var
             GCE: TGIFGraphicControlExtension;
             LoopExt: TGIFAppExtNSLoop;
           begin
             // Preview image being added (for user feedback)
             ImageAnimate.Picture.Assign(Source);
             ImageAnimate.Update;

             // Add source image to animation
             Result := GIF.Add(Source);

             // Netscape Loop extension must be the first extension in the first frame!
             if (GIF.Images.Count = 1) then begin
               LoopExt := TGIFAppExtNSLoop.Create(Result);
               LoopExt.Loops := 0; // Number of loops (0 = forever)
             end;

             // Add Graphic Control Extension
             GCE := TGIFGraphicControlExtension.Create(Result);
             GCE.Delay := MDDef.GIFDefaultDelay div 10; // Animation delay (30 = 300 mS)
             if (Transparent) then begin
               GCE.Transparent := True;
               GCE.TransparentColorIndex := TransparentIndex(Result);
             end;
           end;

           function SizeOfGIF(GIF: TGIFImage): integer;
           var
             Stream: TStream;
           begin
             Stream := TMemoryStream.Create;
             try
               GIF.SaveToStream(Stream);
               Result := Stream.Size;
             finally
               Stream.Free;
             end;
           end;

           procedure LoadAndLabel(anImage : tImage; fname : pathStr; First : boolean = false);
           var
              bmp : tMyBitmap;
              Tstr : shortstring;
           begin
              bmp := tMyBitmap.Create;
              bmp := LoadBitmapFromFile(fname);
              if First then begin
                 Moviex := bmp.Width;
                 MovieY := bmp.Height;
              end
              else begin
                 bmp.Width := MovieX;
                 bmp.Height := MovieY;
              end;
              if MDDef.GIFfileLabels then begin
                 bmp.Canvas.Font.Size := 14;
                 bmp.Canvas.Font.Style := [fsBold];
                 TStr := ExtractFileNameNoExt(fname);
                 bmp.Canvas.TextOut((bmp.width - bmp.Canvas.TextWidth(Tstr)) div 2,0,TStr);
              end;
              anImage.Picture.Graphic := bmp;
              bmp.Free;
              AddBitmap(GIF,anImage.Picture.Graphic, false);
           end;


var
  i,Frames: integer;
  fName : PathStr;
  OptimizeOptions: TGIFOptimizeOptions;
  Size, SizeOptimized: integer;
begin
  if MovieList.Count > 0 then begin
     Screen.Cursor := crHourGlass;
     try
       GIF := TGIFImage.Create;
       try
         Frames := 0;
         for i := 0 to MovieList.Count-1 do begin
            fName := MovieList.Strings[i];
            if FileExists(MoviePath + fName) then Fname := MoviePath + fName
            else Fname := MovieDir + fName;
            if FileExists(fName) then begin
               inc(Frames);

               case i mod 6 of
                  0 : LoadAndLabel(Image1,fName,i=0);
                  1 : LoadAndLabel(Image2,fName);
                  2 : LoadAndLabel(Image3,fName);
                  3 : LoadAndLabel(Image4,fName);
                  4 : LoadAndLabel(Image5,fName);
                  5 : LoadAndLabel(Image6,fName);
               end;

            end
            else begin
               {$IfDef RecordMovie} WriteLineToDebugFile('Missing file: ' + fName); {$EndIf}
            end;
         end;
         if Frames > 0 then begin
            // Calculate size of GIF before optimization
            Size := SizeOfGIF(GIF);

            // Optimize Color map...
            if (CheckBoxPalette.Checked) then
              GIF.OptimizeColorMap;

            // Optimize GIF frames...
            OptimizeOptions := [];
            if (CheckBoxMerge.Checked) then include(OptimizeOptions, ooMerge);
            if (CheckBoxCrop.Checked) then include(OptimizeOptions, ooCrop);
            if (OptimizeOptions <> []) then GIF.Optimize(OptimizeOptions, rmNone, dmNearest, 0);

            // Calculate size of GIF after optimization
            SizeOptimized := SizeOfGIF(GIF);

            // Display statistics
            StatusBar1.Panels[0].Text :='Size: ' + SmartMemorySizeBytes(Size);
            StatusBar1.Panels[1].Text := 'Optimized: ' + SmartMemorySizeBytes(SizeOptimized);
            StatusBar1.Panels[2].Text := Format('Saved: %.1n%%', [(Size-SizeOptimized)*100/Size]);

            // Display animation
            ImageAnimate.Picture.Assign(GIF);
         end
         else begin
            MessageToContinue('Movie list has ' + IntToStr(MovieList.Count) + ' but none were found');
         end;
       finally
         GIF.Free;
       end;
       ButtonSave.Enabled := True;
     finally
       Screen.Cursor := crDefault;
     end;
   end
   else begin
      MessageToContinue('No files in movie list');
   end;
end;


procedure TFormAnimate.ButtonSaveClick(Sender: TObject);
begin
  // Set file extension and filter to GIF (or whatever is currently displayed)
  SaveDialog.DefaultExt := GraphicExtension(TGraphicClass(ImageAnimate.Picture.Graphic.ClassType));
  SaveDialog.Filter := GraphicFilter(TGraphicClass(ImageAnimate.Picture.Graphic.ClassType));
  // Prompt for filename
  if (SaveDialog.Execute) then
    ImageAnimate.Picture.SaveToFile(SaveDialog.Filename);
  if UpperCase(ExtractFilePath(SaveDialog.Filename)) = UpperCase(MDTempDir) then MessageToContinue('The file will be purged the next time the program starts; save elsewhere if you want the file to remain');
end;


procedure TFormAnimate.CheckBox1Click(Sender: TObject);
begin
   MDDef.GIFfileLabels := CheckBox1.Checked;
end;

procedure TFormAnimate.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDDef.GIFDefaultDelay);
end;



end.

