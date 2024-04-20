unit petimage_form;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}


{$IfDef RecordProblems} //normally only defined for debugging specific problems
   //{$Define RecordImageOverlayProblems}
   //{$Define RecordImageLoadProblems}
   //{$Define RecordImageResize}
   //{$Define RecordBitmapEdit}
   //{$Define RecordBlendBitmaps}
   //{$Define RecordGetImagePartOfBitmap}
   //{$Define RecordJPEG}
   //{$Define RecordPNG}
   //{$Define RecordClosing}
   //{$Define RecordBitmapProblems}
   //{$Define RecordIHSmerges}
   //{$Define RecordRoamOnMapProblems}
{$EndIf}


//{$Define NoPatternFloodFill}


interface

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

  System.UITypes,
  Windows,  SysUtils, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Menus, ExtCtrls,{Printers,}JPEG,ClipBrd,
  Buttons, ToolWin, ComCtrls,
  PETMAR,Petmar_types,PetImage,
  {$IfDef RegisterPhoto}    //unclear if all the code for this is still available, and if it would run
     DEMPersW,
     DEMCoord,
  {$EndIf}
  ExtDlgs, StdCtrls;

type
  TImageDisplayForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Close1: TMenuItem;
    New1: TMenuItem;
    OpenDialog1: TOpenDialog;
    ScrollBox1: TScrollBox;
    Text1: TMenuItem;
    Add1: TMenuItem;
    Saveimage1: TMenuItem;
    Flood1: TMenuItem;
    Fill1: TMenuItem;
    Recolor1: TMenuItem;
    Blackandwhite1: TMenuItem;
    ColorstoWhite1: TMenuItem;
    ColorstoWhite2: TMenuItem;
    Negative1: TMenuItem;
    Specifiedcolors1: TMenuItem;
    ColorDialog1: TColorDialog;
    ColorDialog2: TColorDialog;
    Replaceimage1: TMenuItem;
    Removegrays1: TMenuItem;
    Calculate1: TMenuItem;
    Distance1: TMenuItem;
    Leavesinglecolor1: TMenuItem;
    Panel1: TPanel;
    Options1: TMenuItem;
    ShowPosition1: TMenuItem;
    Showcolors1: TMenuItem;
    ToolBar1: TToolBar;
    SpeedButton2: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    Colorize1: TMenuItem;
    Red1: TMenuItem;
    Green1: TMenuItem;
    Blue1: TMenuItem;
    MergeRGBseparates1: TMenuItem;
    MakeGrayscale1: TMenuItem;
    Getcolorpalette1: TMenuItem;
    Rotate1: TMenuItem;
    Specifiedangle1: TMenuItem;
    Linetohorizontal1: TMenuItem;
    Linetovertical1: TMenuItem;
    Image1: TImage;
    ClipboardSpeedButton: TSpeedButton;
    StatusBar1: TStatusBar;
    OpenPictureDialog1: TOpenPictureDialog;
    Subdueimage1: TMenuItem;
    Pastefromclipboard1: TMenuItem;
    Colorcrosssections1: TMenuItem;
    N1: TMenuItem;
    Pickhorizon1: TMenuItem;
    Refresh1: TMenuItem;
    RedrawSpeedButton12: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    Zoom1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Pastefromclipboard2: TMenuItem;
    N2: TMenuItem;
    Batchprocess1: TMenuItem;
    Panel2: TPanel;
    TrackBar1: TTrackBar;
    Panel3: TPanel;
    Label1: TLabel;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    CancelBtn: TSpeedButton;
    BitBtn1: TBitBtn;
    Alphablending1: TMenuItem;
    Overlaynewimagefromclipboard1: TMenuItem;
    N3: TMenuItem;
    SpeedButton6: TSpeedButton;
    V: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    ZoomInSpeedButton4: TSpeedButton;
    ZoomOutSpeedButton5: TSpeedButton;
    SpeedButton11: TSpeedButton;
    ransparentGIFS1: TMenuItem;
    Strecchgrayscale1: TMenuItem;
    N4: TMenuItem;
    Dilateimage1: TMenuItem;
    Erodeimage1: TMenuItem;
    Reviewdirectory1: TMenuItem;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton17: TSpeedButton;
    N90degrees1: TMenuItem;
    N180degrees1: TMenuItem;
    N270degrees1: TMenuItem;
    Skeletonize1: TMenuItem;
    SpeedButton3: TSpeedButton;
    Copyimagetoclipboard1: TMenuItem;
    Removewhitemargins1: TMenuItem;
    Makethisupperleftcorner1: TMenuItem;
    Makethislowerrightcorner1: TMenuItem;
    Removenongraypixels1: TMenuItem;
    Reloadimage1: TMenuItem;
    ComboBox1: TComboBox;
    Picksolidcolor1: TMenuItem;
    Solid1: TMenuItem;
    Pickthispointforsolidcolor1: TMenuItem;
    Saveimageas1: TMenuItem;
    PutEXIFdataintoJPEGS1: TMenuItem;
    Belowthispoint1: TMenuItem;
    Rightofthispoint1: TMenuItem;
    Leftofthispoint1: TMenuItem;
    Abovethispoint1: TMenuItem;
    Horizontalpalette1: TMenuItem;
    Convertwhitetonearwhite1: TMenuItem;
    Smoothingfilter1: TMenuItem;
    ools1: TMenuItem;
    Cyan1: TMenuItem;
    Yellow1: TMenuItem;
    Magenta1: TMenuItem;
    Startregionselection1: TMenuItem;
    Subsetpictures1: TMenuItem;
    PopupMenu2: TPopupMenu;
    Subsetthispicture1: TMenuItem;
    Usethissubsetonmultiplebitmaps1: TMenuItem;
    BitBtn2: TBitBtn;
    Forceaspectratio1: TMenuItem;
    Anyaspectratio1: TMenuItem;
    Pickaspectratio1: TMenuItem;
    Saveimage2: TMenuItem;
    CopytColortoclipboard1: TMenuItem;
    Changecolumns1: TMenuItem;
    MakeRGBandgrayscaleseparates1: TMenuItem;
    Savesubset1: TMenuItem;
    MakeIHSseparates1: TMenuItem;
    procedure Overlaynewimagefromclipboard1Click(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure SpeedButton13Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure SpeedButton12Click(Sender: TObject);
    procedure Alphablending1Click(Sender: TObject);
    procedure Batchprocess1Click(Sender: TObject);
    procedure Pastefromclipboard2Click(Sender: TObject);
    procedure Zoom1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Image1DblClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Add1Click(Sender: TObject);
    procedure Saveimage1Click(Sender: TObject);
    procedure Fill1Click(Sender: TObject);
    procedure ColorstoWhite2Click(Sender: TObject);
    procedure ColorstoWhite1Click(Sender: TObject);
    procedure Negative1Click(Sender: TObject);
    procedure Specifiedcolors1Click(Sender: TObject);
    procedure Replaceimage1Click(Sender: TObject);
    procedure Removegrays1Click(Sender: TObject);
    procedure Distance1Click(Sender: TObject);
    procedure Leavesinglecolor1Click(Sender: TObject);
    procedure ShowPosition1Click(Sender: TObject);
    procedure Showcolors1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Red1Click(Sender: TObject);
    procedure Green1Click(Sender: TObject);
    procedure Blue1Click(Sender: TObject);
    procedure MergeRGBseparates1Click(Sender: TObject);
    procedure MakeGrayscale1Click(Sender: TObject);
    procedure Getcolorpalette1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Specifiedangle1Click(Sender: TObject);
    procedure Linetohorizontal1Click(Sender: TObject);
    procedure Linetovertical1Click(Sender: TObject);
    procedure ClipboardSpeedButtonClick(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Subdueimage1Click(Sender: TObject);
    procedure Pastefromclipboard1Click(Sender: TObject);
    procedure Colorcrosssections1Click(Sender: TObject);
    procedure Pickhorizon1Click(Sender: TObject);
    procedure Refresh1Click(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure MakeRGBseparates1Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure VClick(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure ZoomInSpeedButton4Click(Sender: TObject);
    procedure ZoomOutSpeedButton5Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure ransparentGIFS1Click(Sender: TObject);
    procedure Strecchgrayscale1Click(Sender: TObject);
    procedure Dilateimage1Click(Sender: TObject);
    procedure Erodeimage1Click(Sender: TObject);
    procedure Reviewdirectory1Click(Sender: TObject);
    procedure SpeedButton14Click(Sender: TObject);
    procedure SpeedButton15Click(Sender: TObject);
    procedure SpeedButton16Click(Sender: TObject);
    procedure SpeedButton17Click(Sender: TObject);
    procedure N90degrees1Click(Sender: TObject);
    procedure N180degrees1Click(Sender: TObject);
    procedure N270degrees1Click(Sender: TObject);
    procedure Skeletonize1Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure Copyimagetoclipboard1Click(Sender: TObject);
    procedure Removewhitemargins1Click(Sender: TObject);
    procedure Makethisupperleftcorner1Click(Sender: TObject);
    procedure Makethislowerrightcorner1Click(Sender: TObject);
    procedure Removenongraypixels1Click(Sender: TObject);
    procedure Reloadimage1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Picksolidcolor1Click(Sender: TObject);
    procedure Pickthispointforsolidcolor1Click(Sender: TObject);
    procedure Saveimageas1Click(Sender: TObject);
    procedure PutEXIFdataintoJPEGS1Click(Sender: TObject);
    procedure Belowthispoint1Click(Sender: TObject);
    procedure Rightofthispoint1Click(Sender: TObject);
    procedure Leftofthispoint1Click(Sender: TObject);
    procedure Abovethispoint1Click(Sender: TObject);
    procedure Horizontalpalette1Click(Sender: TObject);
    procedure Convertwhitetonearwhite1Click(Sender: TObject);
    procedure Smoothingfilter1Click(Sender: TObject);
    procedure ools1Click(Sender: TObject);
    procedure MakeRGBgrayscales1Click(Sender: TObject);
    procedure Cyan1Click(Sender: TObject);
    procedure Yellow1Click(Sender: TObject);
    procedure Magenta1Click(Sender: TObject);
    procedure Subsetthispicture1Click(Sender: TObject);
    procedure Usethissubsetonmultiplebitmaps1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Anyaspectratio1Click(Sender: TObject);
    procedure Pickaspectratio1Click(Sender: TObject);
    procedure Forceaspectratio1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure Saveimage2Click(Sender: TObject);
    procedure CopytColortoclipboard1Click(Sender: TObject);
    procedure Changecolumns1Click(Sender: TObject);
    procedure MakeRGBandgrayscaleseparates1Click(Sender: TObject);
    procedure Savesubset1Click(Sender: TObject);
    procedure MakeIHSseparates1Click(Sender: TObject);
  private
    { Private declarations }
     BaseTopBitmap,TopBitmap : tMyBitmap;  //need two, one to modify and the other as the original
     procedure Recolor(Method : RecolorMethod);
     procedure RotateBitmapByAngle(Angle : float64; WhiteBackground : boolean = true);
     procedure ShiftAlphaBlending;
     procedure SetBlendingButtons(Setting : boolean);
     procedure RotateImage(Angle: int16);
     procedure SolidToEdge(StartX,StartY,EndX,EndY : integer);
     procedure MakeNewImageForm(Method : RecolorMethod; fName : PathStr);
  public
    { Public declarations }
     ShiftImageX,ShiftImageY,LocationInFilesInDir,
     RightClickX,RightClickY,
     ImageBlowUp, sx,sy, EndX,EndY,
     XBMPSize,YBMPSize : integer;
     ShiftRotateAngle,ShiftImageBlowUp : float64;
     LoadedFileName : PathStr;
     CanCloseItself,AllowScrollbars,
     ImageMouseIsDown,
     ForceAspect,
     ForceScrollBars,SizeCorrect : boolean;
     MovingBMP : PathStr;
     FilesInDir : tStringList;
     {$IfDef ExImageOverlays}
     {$Else}
        Overlays : tMyData;
     {$EndIf}
     OverlayOpaqueBMP,{BaseBMP,}EditBMP : tMyBitmap;
     {$IfDef RegisterPhoto}
        LastLat,LastLong : float64;
     {$EndIf}

      BigBM_files : PathStr;
      BigBM_Capt : shortstring;

     procedure LoadImage(var FName : PathStr; PickSize : boolean = false); overload;
     procedure LoadImage(var bitmap : tMyBitmap; PickSize : boolean = false); overload;
     procedure SaveThumbnail(FName : PathStr; ThumbNailHeight : integer);
     procedure ResizeImage(xsize,ysize : integer);
     {$IfDef ExImageOverlays}
     {$Else}
        procedure CreateOverlays;
        procedure AddImageOverlay(x,y,width,height : integer; fName : PathStr; Plot : boolean);
        procedure DrawOverlays;
     {$EndIf}
     procedure NoRedrawOrReload;
     procedure StartAlphaBlending(fName : PathStr);
  end;

procedure DisplayBitmap(fName : PathStr; TheTitle : shortString = ''; StayAtop : boolean = false; FewChoices : boolean = false); overload;
procedure DisplayBitmap(bmp : tMyBitmap; TheTitle : shortString = ''; StayAtop : boolean = false; FewChoices : boolean = false);  overload;

procedure PopUpDefaultHorizontalLegendOnBitmap(Min,Max : float64;  Units : shortstring; Legend : tLegendColors; ChloroplethScheme : shortstring = '');
procedure PutMyBitmapIntoImage(fname : PathStr; Image : tImage);
procedure AlphaMatchBitmaps(Bitmap,Bitmap2 : tMyBitmap);

function MakeBigBitmap(var theFiles : tStringList; Capt : shortstring; SaveName : PathStr = ''; Cols : integer = -1; Legend : PathStr ='') : TImageDisplayForm;

procedure DifferenceTwoBitmaps;

function OpenImageEditor : TImageDisplayForm;

var
   LastPhotoRoamX,
   LastPhotoRoamY : integer;
   ImageDoingWhat : tImageDoingWhat;

implementation

{$R *.DFM}


uses
{$IfDef NoPatternFloodFill}
{$Else}
   Zipatone,
{$EndIf}

{$IfDef ExZipatone}
{$Else}
  sc_ColLith,
{$EndIf}


{$IfDef ExGraphs}
{$Else}
   BaseGraf,
{$EndIf}

{$IfDef ExTiff}
{$Else}
   Geotiff,
{$EndIf}

{$IfDef ExGIF}
{$Else}
   GIFImg,
{$EndIf}

{$IfDef ExPNG}
{$Else}
   PNGImage,
{$EndIf}

  {$IfDef VCL}
     PETFont,
  {$EndIf}

  {$IfDef RegisterPhoto}
     Register_Photo,
  {$EndIf}

   {$IfDef ExDBCreate}
   {$Else}
      DataBaseCreate,
   {$EndIf}

   {$IfDef ExGIS}
   {$Else}
      Make_tables, DEMDataBase,
   {$EndIf}

   {$IfDef ExExif}
   {$Else}
     ccr.exif,
   {$EndIf}

   DEMDefs,
   PETMath,
   PetDBUtils,
   KML_Creator,
   Nevadia_Main;

type
   trgbArray =  array[0..MaxScreenXMax] of TRGBTriple;
   prgbArray =  ^trgbArray;

var
   FirstX,FirstY,LastX,LastY : integer;


function MakeBigBitmap(var theFiles : tStringList; Capt : shortstring; SaveName : PathStr = ''; Cols : integer = -1; Legend : PathStr ='') : TImageDisplayForm;
var
   bigbmp,LegBMP : tMyBitmap;
   fName : PathStr;
   AskCols : boolean;
   x,y : integer;
begin
   {$IfDef RecordBigBitmap} WriteLineToDebugFile('MakeBigBitmap in'); {$EndIf}
   Result := nil;
   if (TheFiles <> Nil) and (TheFiles.Count > 0) then begin
     AskCols := (Cols < 0);
     if AskCols then begin
        Cols := MDDef.BigBM_nc;
     end;
     BigBMP := CombineBitmaps(Cols, theFiles, Capt);
     if (BigBMP <> nil) then begin
        if (Capt <> '') then begin
           BigBmp.Canvas.Font.Size := 24;
           BigBmp.Canvas.Font.Style := [fsBold];
           x := BigBmp.Width - 10 - BigBmp.Canvas.TextWidth(Capt);
           y := BigBmp.Height - 10 - BigBmp.Canvas.TextHeight(Capt);
           BigBmp.Canvas.TextOut(x,y,Capt);
        end;
        if (Legend <> '') then begin
            LegBMP := LoadBitmapFromFile(Legend);
            BigBmp.Height := BigBmp.Height + LegBMP.Height + 8;
            y := BigBmp.Height - LegBMP.Height - 4;
            x := (BigBmp.Width - LegBMP.Width) div 2;
            BigBmp.Canvas.Draw(X,y,LegBmp);
            LegBMP.Free;
        end;

         Result := TImageDisplayForm.Create(Application);
         if (SaveName <> '') then begin
            {$IfDef RecordBigBitmap} WriteLineToDebugFile('MakeBigBitmap save in ' + SaveName); {$EndIf}
            SaveBitmap(BigBmp,SaveName);
            //Result.Caption := ExtractFileNameNoExt(SaveName);
            Result.LoadImage(SaveName,true);
         end
         else begin
            Result.LoadImage(BigBmp,true);
         end;
         fName := Petmar.NextFileNumber(MDtempDir,'big_bmp_files','.txt');
         theFiles.SaveToFile(fName);
         Result.BigBM_Capt := Capt;
         Result.BigBM_files := fName;
         Result.ChangeColumns1.Visible := true;
         if AskCols then Result.Changecolumns1Click(nil)
         else Result.RedrawSpeedButton12Click(Nil);
         BigBMP.Free;
         {$IfDef RecordBigBitmap} WriteLineToDebugFile('MakeBigBitmap out, imageform created'); {$EndIf}
      end;
      theFiles.Destroy;
   end
   else begin
      {$IfDef RecordBigBitmap} WriteLineToDebugFile('Nothing in string list'); {$EndIf}
   end;
end;


function OpenImageEditor : TImageDisplayForm;
begin
   StopSplashing;
   Result := TImageDisplayForm.Create(Application);
   Result.LoadedFileName := ImageDir;
   Result.Replaceimage1Click(Nil);
end;


procedure DoBitmapDifference(bmp1,bmp2 : tMyBitmap; var Difference : float64; Display : boolean = true);
var
   xstart,ystart,xend,yend,
   x,y,ChangeCount : integer;
   MaxDiff,Red1,Red2,Green1,Green2,Blue1,Blue2,Red3 : byte;
   Diffs : array[0..255] of integer;
   BMP3 : tMyBitmap;
   BMPMemory,BMPMemory2,BMPMemory3 : tBMPMemory;
   results : tStringList;
begin
      for x := 0 to 255 do Diffs[x] := 0;
      (*
      if GoGrayscale then begin
         MakeTheBitmapGrayScale(bmp1);
         MakeTheBitmapGrayScale(bmp2);
      end;
      *)
      ChangeCount := 0;
      BMPMemory := tBMPMemory.Create(Bmp1);
      BMPMemory2 := tBMPMemory.Create(Bmp2);
      if Display then begin
         CloneBitmap(bmp1,bmp3);
         bmp3.Canvas.Draw(0,0,bmp2);
         DisplayBitmap(bmp1);
         DisplayBitmap(bmp2);
         BMPMemory3 := tBMPMemory.Create(Bmp3);
      end;

      xstart := 660;
      ystart := 1280;
      xend := 3900;
      yend := 2950;

      for y := ystart to yend do begin
         for x := xstart to xend do begin
            BMPMemory.GetPixelRGB(x,y,Red1,Green1,Blue1);
            BMPMemory2.GetPixelRGB(x,y,Red2,Green2,Blue2);

            (*
            if GoGrayscale then begin
               Gray1 := RGBtoGrayscale(Red1,Green1,Blue1);
               Gray2 := RGBtoGrayscale(Red2,Green2,Blue2);
               Gray3 := abs(Gray1-Gray2);
            end
            else Gray3 := 0;
            *)
            Red3 := abs(Red1-Red2);
            MaxDiff := Red3;
            inc(Diffs[MaxDiff]);
            if (MaxDiff < MDDef.ImageDiffThreshhold) then begin
               if Display then BMPMemory3.SetPixelRGB(x,y,0,0,0);
            end
            else begin
               inc(ChangeCount);
            end;
         end;
      end;
      if Display then begin
         for x := 1 to MDDef.ErosionCycles do ErodeTheImage(bmp3,clBlack);
         results := tStringList.Create;
         Results.Add(' Difference   Count');
         for x := 0 to 255 do
            if Diffs[x] <> 0 then
               Results.Add(IntegerToString(x,5) + IntegerToString(Diffs[x],12));
         DisplayAndPurgeStringList(Results,'Difference Histogram');
         BMPMemory3.Destroy;
         DisplayBitmap(bmp3);
         bmp3.Free;
      end;
      Difference := 100.0 * ChangeCount / (succ(xend-xstart) * succ(yend-ystart));
      BMPMemory.Destroy;
      BMPMemory2.Destroy;
end;



procedure DifferenceTwoBitmaps;
var
   Difference : float64;
   fName1,fName2,Dir : PathStr;
   Bmp1,BMP2 : tMyBitmap;
   Results,
   TheFiles : tStringList;
  i: Integer;
begin
   {$IfDef RecordProblems} WriteLineToDebugFile('DifferenceTwoBitmaps in'); {$EndIf}
   ReadDefault('Image Difference threshhold',MDDef.ImageDiffThreshhold);

   if AnswerIsYes('Directory') then begin
      Dir := 'C:\Temp\fox\evening_26July\';
      GetDOSPath('with jpg images',Dir);
      TheFiles := nil;
      Petmar.FindMatchingFiles(Dir,'*.jpg',TheFiles);
      fName1 := TheFiles.Strings[0];
      bmp1 := LoadBitmapFromFile(fName1);
      Results := tStringList.Create;
      Results.Add('IMAGE,DIFFERENCE');
      StartProgress('Motion');
      for i := 1 to pred(TheFiles.Count) do begin
         if (i mod 50 = 1) then begin
            UpdateProgressBar(i/TheFiles.Count);
            {$IfDef RecordProblems} WriteLineToDebugFile(IntToStr(i)); {$EndIf}
         end;
         fName2 := TheFiles.Strings[i];
         bmp2 := LoadBitmapFromFile(fName2);
         DoBitmapDifference(bmp1,bmp2,Difference,false);
         bmp1.Canvas.Draw(0,0,bmp2);
         bmp2.Free;
         Results.Add(ExtractFileName(fName2) + ',' + RealToString(Difference,-12,2));
      end;
      TheFiles.Destroy;
      bmp1.Free;
      EndProgress;
      fName1 := Dir + 'frame-differences.dbf';
      StringList2CSVtoDB(Results,fName1,true);
      {$IfDef RecordProblems} WriteLineToDebugFile('DifferenceTwoBitmaps out');     {$EndIf}
   end
   else begin
      GetFileFromDirectory('Image 1 to compare',GraphicsFilters,fName1);
      GetFileFromDirectory('Image 2 to compare',GraphicsFilters,fName2);
      bmp1 := LoadBitmapFromFile(fName1);
      bmp2 := LoadBitmapFromFile(fName2);

      if (bmp1.Width <> bmp2.Width) or (bmp1.Height <> bmp2.Height) then begin
         MessageToContinue('size mismath');
      end
      else begin
         ReadDefault('Erosion cycles',MDDef.ErosionCycles);
         DoBitmapDifference(bmp1,bmp2,Difference);
         MessageToContinue(RealToString(Difference,-12,2));
      end;
      bmp1.Free;
      bmp2.Free;
      {$IfDef RecordProblems} WriteLineToDebugFile('DifferenceTwoBitmaps out'); {$EndIf}
   end;
end;


{$IfDef ExExif}
{$Else}
procedure SaveJPEGWithExif(FileWithEXIF,NewFileName : PathStr; var Bitmap : tMyBitmap);
var
   Jpeg : TJpegImageEx;
begin
   Jpeg := TJpegImageEx.Create;
   try
      Jpeg.LoadFromFile(FileWithExif);
      Jpeg.Assign(Bitmap, [jaPreserveMetadata]);
      Jpeg.SaveToFile(NewFileName);
   finally
      Jpeg.Free;
   end;
end;
{$EndIf}


procedure PopUpDefaultHorizontalLegendOnBitmap(Min,Max : float64;  Units : shortstring; Legend : tLegendColors; ChloroplethScheme : shortstring = '');
var
   Bitmap : tMyBitmap;
begin
   Bitmap := DefaultHorizontalLegendOnBitmap(Min,Max,Units,'',Legend,ChloroplethScheme);
   DisplayBitmap(Bitmap,'Legend');
   Bitmap.Free;
end;


procedure DisplayBitmap(fName : PathStr; TheTitle : shortString = ''; StayAtop : boolean = false; FewChoices : boolean = false);
var
   img : TImageDisplayForm;
begin
   {$IfDef RecordBitmapEdit} WriteLineToDebugFile('DisplayBitmap in, fname=' + fName); {$EndIf}
   img := TImageDisplayForm.Create(Application);
   img.LoadImage(fName);
   if StayAtop then img.FormStyle := fsStayOnTop;
   if FewChoices then begin
      img.Toolbar1.Visible := false;
      img.File1.Visible := false;
      img.Calculate1.Visible := false;
      img.Flood1.Visible := false;
      img.Recolor1.Visible := false;
      img.Rotate1.Visible := false;
      img.Options1.Visible := false;
      img.Text1.Visible := false;
      img.Zoom1.Visible := false;
   end;

   if (TheTitle <> '') then img.Caption := theTitle;
   if (img.ClientHeight > 600) then img.ClientHeight := 600;
   if (img.ClientWidth > 800) then img.ClientWidth := 800;
   {$IfDef RecordBitmapEdit} WriteLineToDebugFile('DisplayBitmap out'); {$EndIf}
end;


procedure DisplayBitmap(bmp : tMyBitmap; TheTitle : shortString = ''; StayAtop : boolean = false; FewChoices : boolean = false);
var
   fName2 : PathStr;
begin
   fName2 := NextFileNumber(MDTempDir, 'bmp-pet', '.bmp');
   bmp.SaveToFile(fName2);
   BMP.Free;
   DisplayBitmap(fName2,TheTitle, StayAtop, FewChoices);
end;


procedure PutMyBitmapIntoImage(fname : PathStr; Image : tImage);
var
   Bitmap : tMyBitmap;
begin
   Bitmap := LoadBitmapFromFile(fName);
   Image.Picture.Graphic := Bitmap;
   Bitmap.Free;
end;


procedure AlphaMatchBitmaps(Bitmap,Bitmap2 : tMyBitmap);
var
   Result : TImageDisplayForm;
   fName,fName2 : PathStr;
begin
   Result := TImageDisplayForm.Create(Application);
   fName := MDTempDir + 'Baseblending.bmp';
   PetImage.SaveBitmap(Bitmap,fName);
   Result.LoadImage(fName);
   fName2 := MDTempDir + 'blending.bmp';
   PetImage.SaveBitmap(Bitmap2,fName2);
   Result.StartAlphaBlending(fName2);
end;


procedure TImageDisplayForm.TrackBar1Change(Sender: TObject);
var
   DrawBitmap : tMyBitmap;
begin
  {$IfDef ExOpacity}
  {$Else}
      if TrackBar1.Enabled then begin
         CloneBitmap(EditBMP,DrawBitmap);
         DrawBitmap.Canvas.Draw(0,0,EditBMP);
         DrawBitmap := BlendBitmaps(DrawBitmap,TopBitmap,TrackBar1.Position/TrackBar1.Max);
         Image1.Picture.Graphic := DrawBitmap;
         Label1.Caption := 'Opacity: ' + IntToStr(Round(100 * TrackBar1.Position/TrackBar1.Max)) + '%';
      end;
      DrawBitmap.Free;
   {$EndIf}
end;


procedure TImageDisplayForm.Reviewdirectory1Click(Sender: TObject);
var
   tPath : PathStr;
begin
   SpeedButton14.Visible := true;
   SpeedButton15.Visible := true;
   SpeedButton16.Visible := true;
   tPath := ExtractFilePath(LoadedFileName);
   FilesInDir := Nil;
   PetMar.FindMatchingFiles(tPath,'*.jpg',FilesInDir);
   LocationInFilesInDir := 0;
   tPath := FilesInDir.Strings[LocationInFilesInDir];
   LoadImage(tPath);
end;


procedure TImageDisplayForm.Rightofthispoint1Click(Sender: TObject);
begin
   SolidToEdge(RightClickX,0,pred(EditBMP.Width),pred(EditBMP.Height));
end;


procedure TImageDisplayForm.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
{$IfDef RegisterPhoto}
var
   Lat,Long,Pitch,DistOut,Azimuth : float64;
   Mess2,Mess3,Mess4 : ShortString;
   x1,y1,x2,y2 : integer;
{$EndIf}
begin
   wmdem.StatusBar1.Panels[1].Text := 'x=' + IntToStr(x) + '  & y=' + IntToStr(y);
   wmdem.StatusBar1.Panels[2].Text := ColorString(Image1.Canvas.Pixels[x,y]);

   {$IfDef RegisterPhoto}
      if (PhotoRegForm <> Nil) and (PhotoRegForm.DEMPersF <> Nil) then begin
         PhotoRegForm.DEMPersF.LastX := x;
         PhotoRegForm.DEMPersF.LastY := y;
         PhotoRegForm.DEMPersF.FindLatLong(Lat,Long,Pitch,DistOut,Azimuth,Mess1,Mess2,Mess3,Mess4);

         PhotoRegForm.DEMPersF.Image1.Canvas.Pen.Color := clRed;
         PhotoRegForm.DEMPersF.Image1.Canvas.Pen.Mode := pmNotXor;
         PhotoRegForm.DEMPersF.Image1.Canvas.Pen.Width := 3;
         if (LastPhotoRoamX > -1) then CrossWithHole(PhotoRegForm.DEMPersF.Image1.Canvas,LastPhotoRoamX,LastPhotoRoamY);
         CrossWithHole(PhotoRegForm.DEMPersF.Image1.Canvas,x,y);
         LastPhotoRoamX := x;
         LastPhotoRoamY := y;

         if ImageMouseIsDown and (ImageDoingWhat = DigitizeOnPhoto) then begin
            {$IfDef RecordRoamOnMapProblems} WriteLineToDebugFile('TImageFm.Image1MouseMove: ' + LatLongDegreeToString(Lat,Long)); {$EndIf}
            if (Lat < 99) then with DEMGlb[1].SelectionMap,MapDraw do begin
               Petmar.ScreenSymbol(Self.Image1.Canvas,x,y,FilledBox,2,clRed);
               LatLongDegreeToScreen(Lat,Long,x1,y1);
               Petmar.ScreenSymbol(DEMGlb[1].SelectionMap.Image1.Canvas,x1,y1,FilledBox,2,clRed);
               {$IfDef RecordRoamOnMapProblems} WriteLineToDebugFile('x1=' + IntToStr(x1) + '   y1=' + IntToStr(y1) + '  x2=' + IntToStr(x2) + '   y2=' + IntToStr(y2)  ); {$EndIf}
            end;
            LastLat := Lat;
            LastLong := Long;
         end;
      end;
   {$EndIf}

   if ImageMouseIsDown and (ImageDoingWhat = imDoingNothing) then begin
      ScrollBox1.HorzScrollBar.Position := ScrollBox1.HorzScrollBar.Position + SX - X;
      ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position + SY - Y;
      exit;
   end;

   if (ImageDoingWhat in [DraggingBMP]) then begin
       Image1.Canvas.Pen.Mode := pmNotXor;
       Image1.Canvas.Pen.Width := 3;
       Image1.Canvas.Pen.Color := clRed;
       Image1.Canvas.Rectangle(LastX,LastY,LastX+XBMPSize,LastY+YBMPSize);
       Image1.Canvas.Rectangle(X,Y,X+XBMPSize,Y+YBMPSize);
   end;

   if (ImageDoingWhat = imPickingBox) and ImageMouseIsDown then begin
       SetRedrawMode(Image1);
       if ForceAspect then x := FirstX + ((y-FirstY) * MDDef.XPixaspect div MDDef.YPixaspect);
       Image1.Canvas.Rectangle(FirstX,FirstY,x,y);
       Image1.Canvas.Rectangle(FirstX,FirstY,Lastx,Lasty);
       EndX := x;
       EndY := y;
       wmdem.StatusBar1.Panels[3].Text := 'Subset: ' + IntToStr(x-FirstX) + 'x' + IntToStr(y-FirstY);
   end;

   if ImageDoingWhat in [imSecondDistancePoint,SecondHorizontalRotate,SecondVerticalRotate] then begin
       Image1.Canvas.Pen.Mode := pmNotXor;
       Image1.Canvas.Pen.Width := 3;
       Image1.Canvas.Pen.Color := clRed;
       Image1.Canvas.MoveTo(FirstX,FirstY);
       Image1.Canvas.LineTo(Lastx,Lasty);
       Image1.Canvas.MoveTo(FirstX,FirstY);
       Image1.Canvas.LineTo(x,y);
   end;
   LastX := x;
   LastY := y;
end;


procedure TImageDisplayForm.SpeedButton14Click(Sender: TObject);
var
   fName : PathStr;
begin
   if (LocationInFilesInDir > 0) then begin
      dec(LocationInFilesInDir);
      fName := FilesInDir.Strings[LocationInFilesInDir];
      LoadImage(fName);
   end;
end;

procedure TImageDisplayForm.SpeedButton15Click(Sender: TObject);
var
   fName : PathStr;
begin
  if (LocationInFilesInDir < pred(FilesInDir.Count)) then begin
     inc(LocationInFilesInDir);
     fName := FilesInDir.Strings[LocationInFilesInDir];
     LoadImage(fName);
     Caption := ExtractFileName(fname) + '  ' + IntToStr(LocationInFilesInDir) + '/' + IntToStr(pred(FilesInDir.Count));
  end;
end;


procedure TImageDisplayForm.SpeedButton16Click(Sender: TObject);
var
   fName : PathStr;
begin
   SysUtils.DeleteFile(FilesInDir.Strings[LocationInFilesInDir]);
   FilesInDir.Delete(LocationInFilesInDir);
   fName := FilesInDir.Strings[LocationInFilesInDir];
   LoadImage(fName);
end;

procedure TImageDisplayForm.SpeedButton17Click(Sender: TObject);
var
   UL : boolean;
begin
   UL := AnswerIsYes('Upper left corner (vice lower left)');
   KML_Creator.MakeLegendOverlay(Self.Caption,Image1,UL);
end;


procedure TImageDisplayForm.Fill1Click(Sender: TObject);
begin
{$IfDef ExZipatone}
{$Else}
   Fill1.Checked := not Fill1.Checked;
   if Fill1.Checked then begin
      ReadPatternFile;
      ImageDoingWhat := DoingFloodFill;
      PatternF := Tpatternf.Create(Self);
      PatternF.FormStyle := fsMDIChild;
   end
   else begin
      ImageDoingWhat := imDoingNothing;
      PatternF.Close
   end;
{$EndIf}
end;


procedure TImageDisplayForm.StartAlphaBlending(fName : PathStr);
begin
   Panel2.Height := 41;
   ClientHeight := ClientHeight + Panel2.Height;
   TopBitmap := LoadBitmapFromFile(fName);
   BaseTopBitmap := LoadBitmapFromFile(fName);
   TopBitmap.Width := EditBMP.Width;
   TopBitmap.Height := EditBMP.Height;
   Label1.Caption := 'Opacity: ' + IntToStr(Round(100 * TrackBar1.Position/TrackBar1.Max)) + '%';
   SetBlendingButtons(true);
end;


procedure TImageDisplayForm.Strecchgrayscale1Click(Sender: TObject);
begin
   Recolor(StretchGrayscale);
end;


procedure TImageDisplayForm.SetBlendingButtons(Setting : boolean);
begin
    SpeedButton6.Visible := Setting;
    V.Visible := Setting;
    SpeedButton9.Visible := Setting;
    SpeedButton10.Visible := Setting;
    ZoomInSpeedButton4.Visible := Setting;
    ZoomOutSpeedButton5.Visible := Setting;
    TrackBar1.Enabled := Setting;
end;


procedure TImageDisplayForm.Getcolorpalette1Click(Sender: TObject);
{$IfDef ExGIS}
begin
{$Else}
const
   MaxPaletteForDetail = 1024;
var
   x,y,c,nc,dbnum    : integer;
   p0 : pRGB;
   aLine : Ansistring;
   fName : PathStr;
   Palette : tStringList;
   Hist :  array[0..MaxPaletteForDetail] of integer;
   color : array[0..MaxPaletteForDetail] of tRGBTriple;
begin
  {$IfDef VCL}
   Palette := tStringList.Create;
   Palette.Sorted := true;
   Palette.Duplicates := dupIgnore;
   StartProgress('Get palette');
   with EditBMP.Canvas do begin
      for y := 0 to pred(EditBMP.Height) do begin
         if (y mod 25 = 0) then UpdateProgressBar(y/EditBMP.Height);
         p0 := EditBMP.Scanline[y];
         for x := 0 to pred(EditBMP.Width) do begin
            Palette.Add(IntToStr(p0[x].rgbtRed) + ',' + IntToStr(p0[x].rgbtGreen) + ',' + IntToStr(p0[x].rgbtBlue));
         end;
      end;
   end;
   EndProgress;
   nc := Palette.Count;
   if (nc < MaxPaletteForDetail) then begin
      for x := 0 to pred(nc) do begin
         aLine := Palette.Strings[x];
         color[x].rgbtRed := StrToInt(petmar_types.BeforeSpecifiedCharacterANSI(aLine,',',true,true));
         color[x].rgbtGreen := StrToInt(petmar_types.BeforeSpecifiedCharacterANSI(aLine,',',true,true));
         color[x].rgbtBlue := StrToInt(aLine);
         Hist[x] := 0;
      end;

      StartProgress('Get histogram');
      with EditBMP.Canvas do begin
         for y := 0 to pred(EditBMP.Height) do begin
            if (y mod 25 = 0) then UpdateProgressBar(y/EditBMP.Height);
            p0 := EditBMP.Scanline[y];
            for x := 0 to pred(EditBMP.Width) do begin
               for c := 0 to pred(nc) do begin
                  if SameColor(p0[x],Color[c]) then begin
                     inc(hist[c]);
                     Break;
                  end;
               end;
            end;
         end;
      end;
      EndProgress;
      Palette.Clear;
      Palette.Sorted := false;
      Palette.Add('NAME,COLOR,RED,GREEN,BLUE,COUNT');
      for c := 0 to pred(nc) do begin
          Palette.Add('Entry ' + IntToStr(c) +',' + IntToStr(ConvertPlatformColorToTColor(Color[c])) + ',' +
             IntToStr(Color[c].rgbtRed) + ',' + IntToStr(Color[c].rgbtGreen) + ',' + IntToStr(Color[c].rgbtBlue) + ',' + IntToStr(Hist[c]));
      end;
      fName := NextFileNumber(MDTempDir, 'palette_','.csv');
      Palette.SaveToFile(fName);
      DEMDataBase.OpenNumberedGISDataBase(dbNum,fName,true);
      Palette.Free;
   end
   else DisplayAndPurgeStringList(Palette,'RGB palette ' + IntegerToString(nc) + ' entries');
   {$EndIf}
{$EndIf}
end;


procedure TImageDisplayForm.ShiftAlphaBlending;
var
  tb : tMyBitmap;
begin
   FreeAndNil(TopBitmap);
   CreateBitmap(TopBitmap,EditBMP.Height,EditBMP.Width);
   CreateBitmap(tb,round(ShiftImageBlowUp * BaseTopBitmap.Width),round(ShiftImageBlowUp * BaseTopBitmap.Height));
   Tb.Canvas.StretchDraw(rect(0,0,tb.width,tb.height),BaseTopBitmap);
   //Tb.Canvas.StretchDraw(rect(0,0,tb.width,tb.height),TopBitmap);
   if (abs(ShiftRotateAngle) > 0.0001) then tb := RotateBitmap(tb,ShiftRotateAngle,true);
   TopBitmap.Canvas.Draw(ShiftImageX,ShiftImageY,tb);
   tb.free;
   TrackBar1Change(Nil);
end;



procedure TImageDisplayForm.FormResize(Sender: TObject);
begin
   TrackBar1.Width := ClientWidth - 248;
   if SizeCorrect then exit;

   ScrollBox1.HorzScrollBar.Visible := AllowScrollbars and (ForceScrollBars or (Image1.Width > ClientWidth + 2));
   ScrollBox1.VertScrollBar.Visible := AllowScrollbars and (ForceScrollBars or (Image1.Height + Panel1.Height + Toolbar1.height + StatusBar1.Height > ClientHeight + 2));

   {$IfDef RecordImageResize}
      WriteLineToDebugFile('TImageFm.FormResize');
      WriteLineToDebugFile('  Image size: ' + ImageSize(Image1));
      WriteLineToDebugFile('  Client size: ' + IntToStr(ClientWidth) + ' x ' + IntToStr(ClientHeight));
      if ScrollBox1.HorzScrollBar.Visible and ScrollBox1.VertScrollBar.Visible then WriteLineToDebugFile('   Both scrollbars visible')
      else if not (ScrollBox1.HorzScrollBar.Visible and ScrollBox1.VertScrollBar.Visible) then WriteLineToDebugFile('   No scrollbars visible')
      else WriteLineToDebugFile('   One scrollbar visible');
   {$EndIf}
end;


procedure TImageDisplayForm.Alphablending1Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := LoadedFileName;
   {$IfDef VCL}
      if GetGraphicsFileName('',fName) then StartAlphaBlending(fName);
   {$EndIf}
end;


procedure TImageDisplayForm.Anyaspectratio1Click(Sender: TObject);
begin
   ForceAspect := false;
   ImageDoingWhat := imPickingBox;
end;

procedure TImageDisplayForm.NoRedrawOrReload;
begin
   RedrawSpeedButton12.Visible := false;
   Refresh1.Visible := false;
   New1.Visible := false;
   ReplaceImage1.Visible := false;
end;


procedure TImageDisplayForm.ools1Click(Sender: TObject);
begin
   if (MDdef.ProgramOption <> DragonPlotProgram) then begin
      wmdem.Tools1Click(Sender);
   end;
end;

procedure TImageDisplayForm.Overlaynewimagefromclipboard1Click(Sender: TObject);
var
   DragBitmap : tMyBitmap;
begin
   ImageDoingWhat  := DraggingBMP;
   DragBitmap := tMyBitmap.Create;
   MovingBMP := MDTempDir + 'dragging.bmp';
   DragBitmap.Assign(ClipBoard);
   PetImage.SaveBitmap(DragBitmap,MovingBMP);
   DragBitmap.Free;
end;


procedure TImageDisplayForm.Pickaspectratio1Click(Sender: TObject);
begin
   ForceAspect := true;
   ReadDefault('Photo width',MDDef.XPixAspect);
   ReadDefault('Photo height',MDDef.YPixAspect);
   ForceAspectRatio1.Caption := 'Aspect ratio ' + IntToStr(MDDef.XPixAspect) + 'x' + IntToStr(MDDef.YPixAspect);
   ImageDoingWhat := imPickingBox;
end;


{$IfDef ExImageOverlays}
{$Else}

      procedure TImageDisplayForm.CreateOverlays;
      var
        fName : PathStr;
      begin
         if (Overlays = Nil) then begin
            fName := MDTempDir + 'imageoverlay' + DefaultDBExt;
            MakeImageOverlayTable(fName);
            Overlays := tMyData.Create(fName);
         end;
      end;


      procedure TImageDisplayForm.Cyan1Click(Sender: TObject);
      begin
         MakeNewImageForm(TurnCyan,LoadedFileName);
      end;

      procedure TImageDisplayForm.AddImageOverlay(x,y,width,height : integer; fName : PathStr; Plot : boolean);
      var
         ch : AnsiChar;
      begin
         Overlays.ApplyFilter('FILENAME=' + QuotedStr(fname));
         if (Overlays.RecordCount > 0) then begin
            Overlays.Edit;
         end
         else begin
            Overlays.Insert;
         end;
         Overlays.SetFieldByNameAsInteger('X',x);
         Overlays.SetFieldByNameAsInteger('Y',y);
         Overlays.SetFieldByNameAsInteger('WIDTH',X + Width);
         Overlays.SetFieldByNameAsInteger('HEIGHT',Y + HEIGHT);
         Overlays.SetFieldByNameAsString('FILENAME',fName);
         if Plot then ch := 'Y' else ch := 'N';
         Overlays.SetFieldByNameAsString('PLOT',ch);
         Overlays.Post;
         Overlays.ApplyFilter('');
      end;


      procedure TImageDisplayForm.DrawOverlays;
      var
         Bitmap,Bitmap2 : tMyBitmap;
      begin
         Bitmap := Nil;
         Overlays.First;
         while not Overlays.EOF do begin
            if (ptTrim(Overlays.GetFieldByNameAsString('PLOT')) = 'Y') then begin
               if Bitmap = Nil then begin
                  Bitmap := tBitmap.Create;
                  Bitmap.LoadFromFile(MDTempDir + Overlays.GetFieldByNameAsString('FILENAME'));
               end
               else begin
                  Bitmap2 := tMyBitmap.Create;
                  Bitmap2.LoadFromFile(MDTempDir + Overlays.GetFieldByNameAsString('FILENAME'));
                  Bitmap.Canvas.Draw(Overlays.GetFieldByNameAsInteger('X'),Overlays.GetFieldByNameAsInteger('Y'),Bitmap2);
                  Bitmap2.Free;
               end;
            end;
            Overlays.Next;
         end;
         Image1.Picture.Graphic := Bitmap;
         Bitmap.Free;
      end;

{$EndIf}


procedure TImageDisplayForm.ResizeImage(xsize,ysize : integer); //Bitmap2 : tMyBitmap = Nil);
var
   Bitmap : tMyBitmap;
   xs,ys : float64;
begin
   {$IfDef RecordImageResize} EditBitmap.SaveToFile(MDTempDir + 'resizeimage1.bmp'); {$EndIf}
   SizeCorrect := true;
   xs := xsize / Image1.Width;
   ys := ysize / Image1.Height;
   if (xs > ys) then xs := ys;
   xsize := round(xs * Image1.Width);
   ysize := round(xs * Image1.Height);
   CreateBitmap(Bitmap,xsize,ysize);
   Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;

   Image1.Width := xSize;
   Image1.Height := ySize;
   ClientHeight := ysize + Panel1.Height + Toolbar1.height + StatusBar1.Height;
   ClientWidth := xsize;
   SizeCorrect := false;
   FormResize(nil);
   Image1.Canvas.StretchDraw(Rect(0,0,xsize,ysize),EditBMP);
   {$IfDef RecordImageResize} EditBitmap.SaveToFile(MDTempDir + 'resizeimage2.bmp'); {$EndIf}
   {$IfDef RecordImageResize} WriteLineToDebugFile('TImageFm.ResizeImage  Image size: ' + ImeageSize(Image1) + '  Client size: ' + IntToStr(ClientWidth) + ' x ' + IntToStr(ClientHeight)); {$EndIf}
end;


procedure TImageDisplayForm.SaveThumbnail(FName : PathStr; ThumbNailHeight : integer);
begin
   SaveImageAsThumbnail(Image1,fName,ThumbNailHeight);
end;

procedure TImageDisplayForm.New1Click(Sender: TObject);
var
   NewImageForm : TImageDisplayForm;
begin
   NewImageForm := TImageDisplayForm.Create(Application);
   NewImageForm.Replaceimage1Click(Sender);
end;


procedure TImageDisplayForm.Close1Click(Sender: TObject);
begin
   Close;
end;

procedure TImageDisplayForm.ransparentGIFS1Click(Sender: TObject);
var
   StampBitmap : tMyBitmap;
   DefFilter : byte;
   i : integer;
   FilesWanted : tStringList;
   fName : PathStr;
begin
   DefFilter := 1;
   FilesWanted := tStringList.Create;
   if Petmar.GetMultipleFiles('Files for transparent GIF','GIF|*.gif',FilesWanted,DefFilter) then begin
      for i := 0 to pred(FilesWanted.Count) do begin
         fName := FilesWanted.Strings[i];
         StampBitmap :=  LoadBitmapFromFile(fName);
         GetImagePartOfBitmap(StampBitmap);
         SaveBitmap(StampBitmap,fName);
         StampBitmap.Free
      end;
   end;
   FilesWanted.Free;
end;


procedure TImageDisplayForm.Erodeimage1Click(Sender: TObject);
begin
   Recolor(ErodeImage);
end;

procedure TImageDisplayForm.Forceaspectratio1Click(Sender: TObject);
begin
   ForceAspect := true;
   ImageDoingWhat := imPickingBox;
end;

procedure TImageDisplayForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   {$IfDef RecordClosing} WriteLineToDebugFile('TImageFm.FormClose in'); {$EndIf}
   Action := caFree;
   CancelBtnClick(Sender);
   {$IfDef RecordClosing} WriteLineToDebugFile('TImageFm.FormClose CancelBtnClicked'); {$EndIf}

   if (TopBitmap <> Nil) then TopBitmap.Free;
   if (BaseTopBitmap <> Nil) then BaseTopBitmap.Free;
   if (EditBMP <> Nil) then EditBMP.Free;
   {$IfDef ExImageOverlays}
   {$Else}
      if (Overlays <> Nil) then begin
         {$IfDef RecordClosing} WriteLineToDebugFile('TImageFm.FormClose overlays start'); {$EndIf}
         Overlays.ApplyFilter('');
         Overlays.First;
         while not Overlays.eof do begin
            Sysutils.DeleteFile(MDTempDir + Overlays.GetFieldByNameAsString('FILENAME'));
            Overlays.Next;
         end;
         Overlays.Destroy;
         SysUtils.DeleteFile(MDTempDir + 'imageoverlay' + DefaultDBExt);
      end;
   {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('TImageFm.FormClose out'); {$EndIf}
end;


procedure TImageDisplayForm.Image1DblClick(Sender: TObject);
var
   color : TColor;
   r,g,b : byte;
   y : integer;
   Dist  : float64;
   Angle : integer;
   Bitmap : tMyBitmap;
   {$IfDef ExGraphs}
   {$Else}
      ThisGraph : BaseGraf.TThisBaseGraph;
      rfileGray,rfileRed,rfileGreen,rfileBlue  : file;
      v : tGraphPoint32;
   {$EndIf}
   {$IfDef NoPatternFloodFill}
   {$Else}
      CurrentPattern : tPatternRecord;
   {$EndIf}
begin
   {$IfDef RegisterPhoto}
      ImageMouseIsDown := false;
      if (ImageDoingWhat = PickRegistrationPoint) then begin
         RegPhotoForm.StringGrid1.Cells[0,RegPhotoForm.PointDoing] := IntToStr(LastX);
         RegPhotoForm.StringGrid1.Cells[1,RegPhotoForm.PointDoing] := IntToStr(LastY);
         Petmar.ScreenSymbol(Image1.Canvas,LastX,LastY,Cross,5,clRed);
         Image1.Canvas.Font.Color := clRed;
         Image1.Canvas.TextOut(LastX+5,LastY+5,IntToStr(RegPhotoForm.PointDoing));
      end
      else if (PhotoRegForm <> Nil) and (PhotoRegForm.DEMPersF <> Nil) and (ImageDoingWhat = LocatePoints) then begin
         PhotoRegForm.DEMPersF.LastX := Lastx;
         PhotoRegForm.DEMPersF.LastY := Lasty;
         Image1.Canvas.Pen.Mode := pmNotXor;
         ScreenSymbol(Image1.Canvas,LastX,LastY,FilledBox,4,clRed);
         PhotoRegForm.DEMPersF.Image1DblClick(Nil);
         ScreenSymbol(Image1.Canvas,Lastx,Lasty,FilledBox,4,clRed);
         Image1.Canvas.Pen.Mode := pmCopy;
         exit;
      end;
   {$EndIf}

   if (ImageDoingWhat = DraggingBMP) then begin
      Image1.Canvas.Pen.Mode := pmNotXor;
      Image1.Canvas.Pen.Width := 3;
      Image1.Canvas.Pen.Color := clRed;
      Image1.Canvas.Rectangle(LastX,LastY,LastX+XBMPSize,LastY+YBMPSize);
      Bitmap := tMyBitmap.Create;
      Bitmap.LoadFromFile(MovingBMP);
      Image1.Canvas.Draw(LastX,LastY,Bitmap);
      Bitmap.Free;
      ImageDoingWhat := imDoingNothing;
      exit;
   end;
   if (ImageDoingWhat = imFirstDistancePoint) then begin
      ImageDoingWhat := imSecondDistancePoint;
      FirstX := LastX;
      FirstY := LastY;
      exit;
   end;
   if (ImageDoingWhat = FirstHorizontalRotate) then begin
      ImageDoingWhat := SecondHorizontalRotate;
      FirstX := LastX;
      FirstY := LastY;
      exit;
   end;
   if ImageDoingWhat = FirstVerticalRotate then begin
      ImageDoingWhat := SecondVerticalRotate;
      FirstX := LastX;
      FirstY := LastY;
      exit;
   end;

   {$IfDef ExGraphs}
   {$Else}
      if (ImageDoingWhat = ColorCrossSection) then begin
         ThisGraph := TThisBaseGraph.Create(Application);
         ThisGraph.OpenDataFile(rfileGray);
         ThisGraph.OpenDataFile(rfileRed);
         ThisGraph.OpenDataFile(rfileGreen);
         ThisGraph.OpenDataFile(rfileBlue);
         ThisGraph.GraphDraw.MaxHorizAxis := 255;
         ThisGraph.GraphDraw.MaxVertAxis := Image1.Height;
         ThisGraph.GraphDraw.NormalCartesianY := false;
         ThisGraph.GraphDraw.VertLabel := 'Pixels';
         ThisGraph.GraphDraw.HorizLabel := 'Color Levels';
         for y := 0 to pred(Image1.Height) do begin
            Color := Image1.Canvas.Pixels[Lastx,Y];
            GetRGBfromTColor(Color,r,g,b);
            v[2] := y;
            v[1] := r;
            BlockWrite(rfileRed,v,1);
            v[1] := g;
            BlockWrite(rfileGreen,v,1);
            v[1] := b;
            BlockWrite(rfileBlue,v,1);
            v[1] := ValidByteRange(round(0.3 * r  + 0.59 * g  + 0.11 * b));
            BlockWrite(rfileGray,v,1);
         end;

         CloseFile(rfileGray);
         CloseFile(rfileRed);
         CloseFile(rfileGreen);
         CloseFile(rfileBlue);
         ThisGraph.GraphDraw.FileColors256[1] := claBlack;
         ThisGraph.GraphDraw.FileColors256[2] := claRed;
         ThisGraph.GraphDraw.FileColors256[3] := claLime;
         ThisGraph.GraphDraw.FileColors256[4] := claBlue;
         ThisGraph.AutoScaleAndRedrawDiagram(false,true);
      end;
   {$EndIf}
   
   if (ImageDoingWhat = SecondHorizontalRotate) and ((FirstX <> LastX) or (FirstY <> LastY)) then begin
      Image1.Canvas.Pen.Mode := pmNotXor;
      Image1.Canvas.Pen.Width := 3;
      Image1.Canvas.Pen.Color := clRed;
      Image1.Canvas.MoveTo(FirstX,FirstY);
      Image1.Canvas.LineTo(Lastx,Lasty);
      Angle := round(PetMath.HeadingOfLine(LastX-FirstX,LastY-FirstY));
      RotateBitmapByAngle(Angle-90);
      ImageDoingWhat := imDoingNothing;
      exit;
   end;

   if (ImageDoingWhat = SecondVerticalRotate) and ((FirstX <> LastX) or (FirstY <> LastY)) then begin
      Image1.Canvas.Pen.Mode := pmNotXor;
      Image1.Canvas.Pen.Width := 3;
      Image1.Canvas.Pen.Color := clRed;
      Image1.Canvas.MoveTo(FirstX,FirstY);
      Image1.Canvas.LineTo(Lastx,Lasty);
      Angle := round(PetMath.HeadingOfLine(LastX-FirstX,LastY-FirstY));
      RotateBitmapByAngle(180 + Angle);
      ImageDoingWhat := imDoingNothing;
      exit;
   end;

   if (ImageDoingWhat = imSecondDistancePoint) and ((FirstX <> LastX) or (FirstY <> LastY)) then begin
      Dist := sqrt(sqr(LastX-FirstX) + sqr(LastY-FirstY));
      if AnswerIsYes('Distance=' + RealToString(Dist,-8,1) + ' pixels' + MessLineBreak + MessLineBreak + 'Another distance') then begin
         ImageDoingWhat := imFirstDistancePoint;
      end
      else begin
         ImageDoingWhat := imDoingNothing;
         Distance1.Checked := false;
      end;
      Image1.Canvas.Pen.Mode := pmNotXor;
      Image1.Canvas.Pen.Width := 3;
      Image1.Canvas.Pen.Color := clRed;
      Image1.Canvas.MoveTo(FirstX,FirstY);
      Image1.Canvas.LineTo(Lastx,Lasty);
      Image1.Canvas.Pen.Mode := pmCopy;
      exit;
   end;
   if (ImageDoingWhat = DoingFloodFill) then begin
      CurrentPattern := PatternFromString('yyy');
      SimpleFillShape(Image1.Canvas,LastX,LastY,0,0,Image1.Width,Image1.Height, CurrentPattern);
   end
   else if (ImageDoingWhat = imDoingNothing) then begin
      Color := Image1.Canvas.Pixels[Lastx,LastY];
      MessageToContinue('x=' + IntToStr(LastX) + '   y=' + IntToStr(LastY) +  MessLineBreak + 'Color = ' + IntToStr(Color) + MessLineBreak + ColorString(Color),True);
   end
   else if (ImageDoingWhat = AddingText) then begin
     {$IfDef VCL}
         with FontDlg do begin
            XPic := LastX;
            YPic := LastY;
            Edit3.Text := IntegerToString(Xpic,-3);
            Edit4.Text := IntegerToString(Ypic,-3);
            Image1.Picture.SaveToFile(MDTempDir + 'Backup.BMP');
            Running := true;
            Edit2.Text := '';
            FontDlg.ShowModal;
            Running := false;
            DeleteFileIfExists(MDTempDir + 'Backup.BMP');
         end;
      {$EndIf}
   end;
end;


procedure TImageDisplayForm.Abovethispoint1Click(Sender: TObject);
begin
   SolidToEdge(0,0,0,RightClickY);
end;

procedure TImageDisplayForm.Add1Click(Sender: TObject);
begin
  {$IfDef VCL}
      Add1.Checked := not Add1.Checked;
      if Add1.Checked then begin
         ImageDoingWhat := AddingText;
         FontDlg := TFontDlg.Create(Self);
         Image1.Canvas.Font.Name := 'Times New Roman';
         FontDlg.FontDialog1.Font := Image1.Canvas.Font;
         FontDlg.OwningCanvas := Image1.Canvas;
      end
      else begin
         ImageDoingWhat := imDoingNothing;
         FontDlg.Close;
      end;
   {$EndIf}
end;


procedure TImageDisplayForm.Saveimage1Click(Sender: TObject);
begin
 {$IfDef ExExif}
 {$Else}
   if FileExtEquals(LoadedFileName,'.jpg') then SaveJPEGWithExif(LoadedFileName,LoadedFileName,EditBMP)
   else {$EndIf}
      PetImage.SaveBitmap(EditBMP,LoadedFileName);
end;


procedure TImageDisplayForm.Saveimage2Click(Sender: TObject);
begin
   Saveimageas1Click(Sender);
end;

procedure TImageDisplayForm.Saveimageas1Click(Sender: TObject);
begin
   if GetNewGraphicsFileName('saved image',LoadedFileName) then begin
      PetImage.SaveBitmap(EditBMP,LoadedFileName);
   end;
end;

procedure TImageDisplayForm.Savesubset1Click(Sender: TObject);
begin
   if GetNewGraphicsFileName('saved image subset',LoadedFileName) then begin
      LoadImage(EditBMP);
      PetImage.SaveBitmap(ExtractPartOfImage(Image1, FirstX,EndX,FirstY,EndY),LoadedFileName);
   end;
end;

procedure TImageDisplayForm.Recolor(Method : RecolorMethod);
var
   x,y : integer;
   r,g,b : byte;
   h,l,s : float64;
   BMPMemory : tBMPMemory;
begin
   StartProgress('Recolor');
   if (Method = NoGrays) then begin
      {$IfDef VCL} ReadDefault('Black limit to keep',MDDef.BlackLimit); {$EndIf}
      BitmapRemoveGrays(EditBMP,MDDef.BlackLimit);
   end
   else if (Method = NoNonGrays) then begin
      {$IfDef VCL} ReadDefault('Gray tolerance',MDDef.GrayTolerance); {$EndIf}
      BitmapRemoveNonGrays(EditBMP,MDDef.GrayTolerance);
   end
   else if (Method = MakeGrayscale) then MakeTheBitmapGrayScale(EditBMP)
   else if (Method = DilateImage) then begin
      if ColorDialog1.Execute then DilateTheImage(EditBMP,ColorDialog1.Color);
   end
   else if (Method = ErodeImage) then begin
      if ColorDialog1.Execute then ErodeTheImage(EditBMP,ColorDialog1.Color);
   end
   else if (Method = StretchGrayscale) then MakeBitmapStretchedGrayscale(EditBMP)
   else if (Method = NonWhiteToBlack) then BitmapNonWhiteToBlack(EditBMP)
   else if (Method = MakeSubdued) then MakeTheBitmapSubdued(EditBMP)
   else if (Method = SmoothFilterImage) then SmoothFilterTheImage(EditBMP)
   else if (Method = WhiteToNearWhite) then BitmapWhiteToNearWhite(EditBMP)
   else if (Method in [NegativeMethod,NonBlackToWhite, PickedColors,LeaveSingle]) then begin
      for y := 0 to pred(EditBMP.Height) do begin
         if (y mod 100 = 0) then UpdateProgressBar(y/EditBMP.Height);
         for x := 0 to pred(EditBMP.Width) do begin
            GetRGBfromTColor(EditBMP.Canvas.Pixels[x,y],r,g,b);
            case Method of
               NegativeMethod : if EditBMP.Canvas.Pixels[x,y] = clWhite then EditBMP.Canvas.Pixels[x,y] := clBlack
                                else if EditBMP.Canvas.Pixels[x,y] = clBlack then EditBMP.Canvas.Pixels[x,y] := clWhite;
               NonBlackToWhite : if EditBMP.Canvas.Pixels[x,y] <> clBlack then EditBMP.Canvas.Pixels[x,y] := clWhite;
               PickedColors    : if EditBMP.Canvas.Pixels[x,y] = ColorDialog1.Color then EditBMP.Canvas.Pixels[x,y] := ColorDialog2.Color;
               LeaveSingle     : if EditBMP.Canvas.Pixels[x,y] <> ColorDialog1.Color then EditBMP.Canvas.Pixels[x,y] := clWhite;
            end;
         end;
      end;
   end
   else begin
      BMPMemory := tBMPMemory.Create(EditBMP);
      for y := 0 to pred(EditBMP.Height) do begin
         if (y mod 100 = 0) then UpdateProgressBar(y/EditBMP.Height);
         for x := 0 to pred(EditBMP.Width) do begin
            BMPMemory.GetPixelRGB(x,y,r,g,b);
            case Method of
               TurnRed         : BMPMemory.SetPixelRGB(x,y,r,0,0);
               TurnGreen       : BMPMemory.SetPixelRGB(x,y,0,g,0);
               TurnBlue        : BMPMemory.SetPixelRGB(x,y,0,0,b);
               BlueGrayscale : BMPMemory.SetPixelRGB(x,y,b,b,b);
               RedGrayscale : BMPMemory.SetPixelRGB(x,y,r,r,r);
               GreenGrayscale : BMPMemory.SetPixelRGB(x,y,g,g,g);
               TurnMagenta        : BMPMemory.SetPixelRGB(x,y,r,0,b);
               TurnYellow      : BMPMemory.SetPixelRGB(x,y,r,g,0);
               TurnCyan     : BMPMemory.SetPixelRGB(x,y,0,g,b);
               TurnHLS_light,
               TurnHLS_hue,
               TurnHLS_sat  : begin
                                 RGBtoHLS(r,g,b,h,l,s);
                                 if Method = TurnHLS_light then begin
                                    g := round(l * 255);
                                    BMPMemory.SetPixelRGB(x,y,g,g,g);
                                 end;
                                 if Method = TurnHLS_sat then begin
                                    g := round(s * 255);
                                    BMPMemory.SetPixelRGB(x,y,g,g,g);
                                 end;
                                 if Method = TurnHLS_hue then begin
                                    if (r <> 255) or (g <> 255) or (b <> 255) then HLStoRGB(h,0.5,1,r,g,b);
                                    BMPMemory.SetPixelRGB(x,y,r,g,b);
                                 end;
                              end;
            end;
         end;
      end;
      BMPMemory.Destroy;
      if Method in [BlueGrayscale,TurnBlue] then Caption := 'Blue ' + ExtractFileName(LoadedFileName);
      if Method in [RedGrayscale,TurnRed] then Caption := 'Red ' + ExtractFileName(LoadedFileName);
      if Method in [GreenGrayscale,TurnGreen] then Caption := 'Green ' + ExtractFileName(LoadedFileName);
      if Method in [GreenGrayscale,TurnGreen] then Caption := 'Green ' + ExtractFileName(LoadedFileName);
      if Method in [TurnCyan] then Caption := 'Cyan ' + ExtractFileName(LoadedFileName);
      if Method in [TurnMagenta] then Caption := 'Magenta ' + ExtractFileName(LoadedFileName);
      if Method in [TurnYellow] then Caption := 'Yellow ' + ExtractFileName(LoadedFileName);
      if Method in [TurnHLS_light] then Caption := 'HLS lightness ' + ExtractFileName(LoadedFileName);
      if Method in [TurnHLS_sat] then Caption := 'HLS saturation ' + ExtractFileName(LoadedFileName);
      if Method in [TurnHLS_hue] then Caption := 'HLS hue ' + ExtractFileName(LoadedFileName);
   end;
   Image1.Picture.Graphic := EditBMP;
   EndProgress;
end;


procedure TImageDisplayForm.ColorstoWhite2Click(Sender: TObject);
begin
   Recolor(NonBlackToWhite);
end;

procedure TImageDisplayForm.ComboBox1Change(Sender: TObject);
var
   TStr : shortstring;
begin
   TStr := ComboBox1.Text;
   Delete(TStr,length(TStr),1);
   ImageBlowup := round(StrToFloat(TStr));
   ResizeImage(XBMPSize * ImageBlowUp div 100, YBMPSize * ImageBlowUp div 100);
end;

procedure TImageDisplayForm.Convertwhitetonearwhite1Click(Sender: TObject);
begin
   Recolor(WhiteToNearWhite);
end;

procedure TImageDisplayForm.Copyimagetoclipboard1Click(Sender: TObject);
begin
   AssignImageToClipBoard(Image1);
end;

procedure TImageDisplayForm.CopytColortoclipboard1Click(Sender: TObject);
begin
   ClipBrd.ClipBoard.AsText := IntToStr(Image1.Canvas.Pixels[Lastx,Lasty]);
end;

procedure TImageDisplayForm.ColorstoWhite1Click(Sender: TObject);
begin
   Recolor(NonWhiteToBlack);
end;

procedure TImageDisplayForm.N180degrees1Click(Sender: TObject);
begin
  RotateImage(180);
end;

procedure TImageDisplayForm.N270degrees1Click(Sender: TObject);
begin
  RotateImage(270);
end;

procedure TImageDisplayForm.RotateImage(Angle : int16);
begin
    if (Angle = 90) then Drehen90Grad(EditBMP);
    if (Angle = 180) then Drehen180Grad(EditBMP);
    if (Angle = 270) then Drehen270Grad(EditBMP);
    LoadImage(EditBMP);
end;


procedure TImageDisplayForm.N90degrees1Click(Sender: TObject);
begin
   RotateImage(90);
end;

procedure TImageDisplayForm.Negative1Click(Sender: TObject);
begin
   Recolor(NegativeMethod);
end;


procedure TImageDisplayForm.Specifiedcolors1Click(Sender: TObject);
begin
   if ColorDialog1.Execute and ColorDialog2.Execute then Recolor(PickedColors);
end;


procedure TImageDisplayForm.LoadImage(var Bitmap : tMyBitmap; PickSize : boolean = false);
begin
   Image1.Picture.Graphic := Bitmap;
   if LoadedFileName = '' then LoadedFileName := 'loaded_image.bmp';

   LoadedFileName := NextFileNumber(MDTempDir,ExtractFileNameNoExt(LoadedFileName) + '_','.bmp');
   Bitmap.SaveToFile(LoadedFileName);
   XBMPSize := Bitmap.Width;
   YBMPSize := Bitmap.Height;
   Image1.Width := XBMPsize;
   Image1.Height := YBMPsize;

   ComboBox1Change(Nil);
   {$IfDef RecordImageResize} WriteLineToDebugFile('Bitmap size ' + LoadedFile + '  ' + ImageSize(Bitmap)); {$EndIf}
   if (Not SizeCorrect) then begin
      ClientHeight := Image1.Height + Panel1.Height + Toolbar1.Height + StatusBar1.Height;
      ClientWidth := Image1.Width;
      if (ClientHeight > Forms.Screen.Height - 200) then  ClientHeight := Forms.Screen.Height - 200;
      if (ClientWidth > Forms.Screen.Width - 50) then  ClientWidth := Forms.Screen.Width - 200;
   end;
   if PickSize then begin
      ImageBlowup := 100;
      while (YBMPSize * ImageBlowUp div 100) > wmdem.ClientHeight do begin
         dec(ImageBlowup,10);
      end;
      ComboBox1.Text := IntToStr(ImageBlowUp)  + '%';
   end;
   {$IfDef RecordImageResize} WriteLineToDebugFile('TImageFm.LoadImage, Im size: ' + IntToStr(Image1.Width) + 'x' + IntToStr(Image1.Height) + '  Client size: ' + IntToStr(ClientWidth) + 'x' + IntToStr(ClientHeight)); {$EndIf}
end;


procedure TImageDisplayForm.LoadImage(var FName : PathStr; PickSize : boolean = false);
begin
   try
      {$IfDef RecordImageLoadProblems} WriteLineToDebugFile('TImageFm.LoadImage: ' + fName); {$EndIf}
      ShowHourglassCursor;
      if not FileExists(fName) then begin
         OpenPictureDialog1.FileName := fName;
         if not OpenPictureDialog1.Execute then exit;
         fName := OpenPictureDialog1.FileName;
      end;
      Caption := ExtractFileName(FName) + ' stored image';
      LoadedFileName := FName;
      EditBMP := LoadBitmapFromFile(LoadedFileName);
      XBMPSize := EditBMP.Width;
      YBMPSize := EditBMP.Height;
      LoadImage(EditBMP,PickSize);
   finally
      ShowDefaultCursor;
   end;
end;


procedure TImageDisplayForm.Replaceimage1Click(Sender: TObject);
var
   fName : PathStr;
   i : integer;
   NewImageForm : TImageDisplayForm;
begin
   If OpenPictureDialog1.Execute then begin
      MDdef.DefaultReadImageType := OpenPictureDialog1.FilterIndex;
      fName := OpenPictureDialog1.FileName;
      LoadImage(fName);
      if (OpenPictureDialog1.Files.Count > 1) then begin
         for i := 1 to pred(OpenPictureDialog1.Files.Count) do begin
            NewImageForm := TImageDisplayForm.Create(Application);
            fName := OpenPictureDialog1.Files[i];
            NewImageForm.LoadImage(fName);
         end;
      end;
   end
   else Close;
end;


procedure TImageDisplayForm.Removegrays1Click(Sender: TObject);
begin
   Recolor(NoGrays);
end;


procedure TImageDisplayForm.Removenongraypixels1Click(Sender: TObject);
begin
   Recolor(NoNonGrays);
end;

procedure TImageDisplayForm.Removewhitemargins1Click(Sender: TObject);
begin
   GetImagePartOfBitmap(EditBMP);
   LoadImage(EditBMP);
end;

procedure TImageDisplayForm.Dilateimage1Click(Sender: TObject);
begin
   Recolor(DilateImage);
end;

procedure TImageDisplayForm.Distance1Click(Sender: TObject);
begin
   Distance1.Checked := not Distance1.Checked;
   if Distance1.Checked then ImageDoingWhat := imFirstDistancePoint
   else ImageDoingWhat := imDoingNothing;
end;


procedure TImageDisplayForm.Leavesinglecolor1Click(Sender: TObject);
begin
   if ColorDialog1.Execute then Recolor(LeaveSingle);
end;


procedure TImageDisplayForm.Leftofthispoint1Click(Sender: TObject);
begin
   SolidToEdge(0,0,RightClickX,pred(EditBMP.Height));
end;

procedure TImageDisplayForm.ShowPosition1Click(Sender: TObject);
begin
   ShowPosition1.Checked := not ShowPosition1.Checked;
   if ShowColors1.Checked or ShowPosition1.Checked then Panel1.Height := 20
   else Panel1.Height := 0;
end;


procedure TImageDisplayForm.Skeletonize1Click(Sender: TObject);
begin
   SkeletonizeBitmap(Editbmp);
   Image1.Picture.Graphic := EditBmp;
end;

procedure TImageDisplayForm.Smoothingfilter1Click(Sender: TObject);
begin
   Recolor(SmoothFilterImage);
end;


procedure TImageDisplayForm.SolidToEdge(StartX,StartY,EndX,EndY : integer);
var
   x,y : integer;
   BMPMemory : tBMPMemory;
begin
   BMPMemory := tBMPMemory.Create(EditBMP);
   for y := StartY to EndY do begin
      for x := StartX to EndX do begin
         BMPMemory.SetPixelColor(x,y,MDDef.ReplaceBorderColor);
      end;
   end;
   BMPMemory.Destroy;
   LoadImage(EditBMP);
end;


procedure TImageDisplayForm.Showcolors1Click(Sender: TObject);
begin
   ShowColors1.Checked := not ShowColors1.Checked;
   if ShowColors1.Checked or ShowPosition1.Checked then Panel1.Height := 20
   else Panel1.Height := 0;
end;


procedure TImageDisplayForm.SpeedButton10Click(Sender: TObject);
begin
   ShiftImageY := ShiftImageY - 1;
   ShiftAlphaBlending;
end;

procedure TImageDisplayForm.SpeedButton11Click(Sender: TObject);
begin
   ShiftRotateAngle := 5;
   ReadDefault('rotate (deg)',ShiftRotateAngle);
   ShiftAlphaBlending;
end;

procedure TImageDisplayForm.SpeedButton12Click(Sender: TObject);
var
   Bitmap : tMyBitmap;
begin
  {$IfDef ExOpacity}
  {$Else}
  if TrackBar1.Enabled then begin
      Bitmap := BlendBitmaps(TopBitmap,EditBMP,TrackBar1.Position/255);
      SaveBitmap(Bitmap);
      Bitmap.Free;
  end;
  {$EndIf}
end;

procedure TImageDisplayForm.SpeedButton13Click(Sender: TObject);
var
   Bitmap : tMyBitmap;
begin
  {$IfDef ExOpacity}
  {$Else}
   if TrackBar1.Enabled then begin
      Bitmap := BlendBitmaps(TopBitmap,EditBMP,TrackBar1.Position/255);
      Clipboard.Assign(Bitmap);
      Bitmap.Free;
   end;
  {$EndIf}
end;


procedure TImageDisplayForm.SpeedButton2Click(Sender: TObject);
begin
   Saveimage1Click(Sender);
end;


procedure TImageDisplayForm.SpeedButton3Click(Sender: TObject);
begin
  FormStyle := fsStayOntop;
end;

procedure TImageDisplayForm.SpeedButton4Click(Sender: TObject);
begin
   Add1Click(Sender);
end;


procedure TImageDisplayForm.SpeedButton6Click(Sender: TObject);
begin
   ShiftImageX := ShiftImageX + 1;
   ShiftAlphaBlending;
end;

procedure TImageDisplayForm.Red1Click(Sender: TObject);
begin
   Recolor(TurnRed);
end;


procedure TImageDisplayForm.Green1Click(Sender: TObject);
begin
   Recolor(TurnGreen);
end;


procedure TImageDisplayForm.Horizontalpalette1Click(Sender: TObject);
{$IfDef ExGIS}
begin
{$Else}
var
   x,y,dbNum : integer;
   p0    : pRGB;
   fName : PathStr;
   Palette : tStringList;
begin
  {$IfDef VCL}
   Palette := tStringList.Create;
   Palette.Add('N,R,G,B');
   with EditBMP.Canvas do begin
      y := 5;
      p0 := EditBMP.Scanline[y];
      for x := 0 to pred(EditBMP.Width) do begin
         Palette.Add(IntToStr(x) + ',' + IntToStr(p0[x].rgbtRed) + ',' + IntToStr(p0[x].rgbtGreen) + ',' + IntToStr(p0[x].rgbtBlue));
      end;
   end;
   fName := NextFileNumber(MDTempDir, 'palette_','.csv');
   Palette.SaveToFile(fName);
   DEMDataBase.OpenNumberedGISDataBase(dbNum,fName,true);
   Palette.Free;
   {$EndIf}
{$EndIf}
end;


procedure TImageDisplayForm.Batchprocess1Click(Sender: TObject);
var
   FilesWanted : TStringList;
   i: integer;
   fName : PathStr;
   DefaultFilter  : byte;
begin
   FilesWanted := tStringList.Create;
   FilesWanted.Add(ProgramRootDir);
   DefaultFilter := 0;
   if Petmar.GetMultipleFiles('for color modification','Any file|*.*',FilesWanted,DefaultFilter) then begin
      if ColorDialog1.Execute and ColorDialog2.Execute then begin
         for i  := 0 to pred(FilesWanted.Count) do begin
            fName := FilesWanted[i];
            LoadImage(fName);
            Recolor(PickedColors);
            SaveImageAsBMP(Image1,FilesWanted[i]);
         end;
      end;
   end;
   FilesWanted.Free;
end;

procedure TImageDisplayForm.Belowthispoint1Click(Sender: TObject);
begin
   SolidToEdge(0,RightClickY,pred(EditBMP.Width),pred(EditBMP.Height));
end;

procedure TImageDisplayForm.BitBtn1Click(Sender: TObject);
begin
  {$IfDef ExBlendMovie}
  {$Else}
     PetImage.BlendMovie(TopBitmap,EditBMP);
  {$EndIf}
end;

procedure TImageDisplayForm.BitBtn2Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := NextFileNumber(ImageDir, 'screen_capture_', '.jpg');
   PetImage.ScreenShot(fName);
end;

procedure TImageDisplayForm.Blue1Click(Sender: TObject);
begin
   Recolor(TurnBlue);
end;


function MergeRGBBitmaps(var RedBitmap,GreenBitmap,BlueBitmap : tMyBitmap; PurgeBMPs : boolean = true) : tMyBitmap;
var
   RedMem,BlueMem,GreenMem,MergeMem : tBMPMemory;
   x,y : integer;
begin
   CreateBitmap(Result,RedBitmap.Width,RedBitmap.Height);
   MergeMem := tBMPMemory.Create(Result);
   RedMem := tBMPMemory.Create(RedBitmap);
   GreenMem := tBMPMemory.Create(GreenBitmap);
   BlueMem := tBMPMemory.Create(BlueBitmap);

   StartProgress('Merge');
   for x := 0 to pred(Result.Width) do begin
      UpdateProgressBar(x/Result.Width);
      for y := 0 to pred(Result.Height) do begin
         MergeMem.SetRedChannel(x,y,RedMem.RedChannel(x,y));
         MergeMem.SetGreenChannel(x,y,GreenMem.GreenChannel(x,y));
         MergeMem.SetBlueChannel(x,y,BlueMem.BlueChannel(x,y));
      end;
   end;
   RedMem.Destroy;
   GreenMem.Destroy;
   BlueMem.Destroy;
   MergeMem.Destroy;
   if PurgeBMPs then begin
      RedBitmap.Destroy;
      GreenBitmap.Destroy;
      BlueBitmap.Destroy;
   end;
end;


function MergeRGBFiles(RedName,GreenName,BlueName : PathStr ) : tMyBitmap;
var
   RedBitmap,GreenBitmap,BlueBitmap : tMyBitmap;
begin
   RedBitmap := LoadBitmapFromFile(RedName);
   GreenBitmap := LoadBitmapFromFile(GreenName);
   BlueBitmap := LoadBitmapFromFile(BlueName);
   Result := MergeRGBBitmaps(RedBitmap,GreenBitmap,BlueBitmap);
end;


procedure TImageDisplayForm.MergeRGBseparates1Click(Sender: TObject);
var
   RedName,GreenName,BlueName,fName : PathStr;
   MergeBitmap : tMyBitmap;
begin
   if GetGraphicsFileName('red color', RedName) and GetGraphicsFileName('green color', GreenName) and GetGraphicsFileName('blue color', BlueName) then begin
      MergeBitmap := MergeRGBFiles(RedName,GreenName,BlueName);
      fName := MDTempDir + 'working.bmp';
      PetImage.SaveBitmap(MergeBitmap,fName);
      LoadImage(fName);
      EndProgress;
   end;
end;

procedure TImageDisplayForm.Magenta1Click(Sender: TObject);
begin
   MakeNewImageForm(TurnMagenta,LoadedFileName);
end;

procedure TImageDisplayForm.MakeGrayscale1Click(Sender: TObject);
begin
   Recolor(MakeGrayscale);
end;


procedure TImageDisplayForm.MakeIHSseparates1Click(Sender: TObject);
begin
   MakeNewImageForm(TurnHLS_sat,LoadedFileName);
   MakeNewImageForm(TurnHLS_light,LoadedFileName);
   MakeNewImageForm(TurnHLS_hue,LoadedFileName);
end;

procedure TImageDisplayForm.MakeRGBandgrayscaleseparates1Click(Sender: TObject);
begin
   MakeRGBgrayscales1Click(Sender);
   MakeRGBseparates1Click(Sender);
end;

procedure TImageDisplayForm.FormCreate(Sender: TObject);
var
   Bitmap : tMyBitmap;
begin
   {$IfDef ExImageOverlays}
   {$Else}
       Overlays := Nil;
   {$EndIf}
   TopBitmap := Nil;
   BaseTopBitmap := Nil;
   EditBMP := Nil;
   SizeCorrect := false;
   ForceScrollBars := false;
   AllowScrollBars := true;
   {$IfDef RegisterPhoto}
      ImageMouseIsDown := false;
   {$EndIf}
   CanCloseItself := true;
   ImageBlowUp := 100;

   ShiftImageX := 0;
   ShiftImageY := 0;
   ShiftImageBlowUp := 1;
   ShiftRotateAngle := 0;

   Panel1.Height := 0;
   Panel2.Height := 0;

   StatusBar1.Height := 0;
   LoadedFileName := '';

   ForceAspectRatio1.Caption := 'Aspect ratio ' + IntToStr(MDDef.XPixAspect) + 'x' + IntToStr(MDDef.YPixAspect);

  {$IfDef RegisterPhoto}
  {$Else}
     SpeedButton7.Visible := false;
     SpeedButton8.Visible := false;
  {$EndIf}

   Image1.Stretch := true;             //Required for Delphi 6 "feature"
   CreateBitmap(Bitmap,ClientWidth - 5,ClientHeight - Panel1.Height - Toolbar1.Height - 5);
   Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
   BigBM_files := '';
   BigBM_Capt := '';
   SetBlendingButtons(false);
end;


procedure TImageDisplayForm.Specifiedangle1Click(Sender: TObject);
const
   Angle : float64 = 45;
var
   WhiteBackground : boolean;
begin
   ReadDefault('Rotation angle (' + DegSym + ', + = CW)', Angle);
   WhiteBackground := AnswerIsYes('White background');
   RotateBitmapByAngle(Angle,WhiteBackground);
end;

procedure TImageDisplayForm.RotateBitmapByAngle(Angle : float64; WhiteBackground : boolean = true);
var
   BigBitmap : tMyBitmap;
   Diagonal : integer;
   Miss : tColor;
begin
   wmDEM.StatusBar1.Panels[0].Text := 'Rotate by ' + RealToString(Angle,-12,-2) + DegSym;
   ResizeImage(XBMPSize,YBMPSize);   CopyImageToBitmap(Image1,EditBMP);
   Diagonal := round(sqrt(sqr(EditBMP.Height) + sqr(EditBMP.Width)));
   CreateBitmap(BigBitmap,Diagonal,Diagonal);

   if WhiteBackground then Miss := clWhite else Miss := clBlack;
   BigBitmap.Canvas.Pen.Color := Miss;
   BigBitmap.Canvas.Brush.Color := Miss;
   BigBitmap.Canvas.Brush.Style := bsSolid;
   BigBitmap.Canvas.Rectangle(0,0,Diagonal,Diagonal);

   BigBitmap.Canvas.Draw( (Diagonal-EditBMP.Width) div 2,  (Diagonal-EditBMP.Height) div 2, EditBMP);

   ShowHourglassCursor;
   EditBMP := RotateBitmap(BigBitmap, Angle,true,WhiteBackground);
   LoadImage(EditBMP);
   BigBitmap.Free;
   ShowDefaultCursor;
end;

procedure TImageDisplayForm.Linetohorizontal1Click(Sender: TObject);
begin
   ImageDoingWhat := FirstHorizontalRotate;
end;

procedure TImageDisplayForm.Linetovertical1Click(Sender: TObject);
begin
   ImageDoingWhat := FirstVerticalRotate;
end;


procedure TImageDisplayForm.CancelBtnClick(Sender: TObject);
begin
  {$IfDef ExOpacity}
  {$Else}
      if (BaseTopBitmap <> Nil) then FreeAndNil(BaseTopBitmap);
      if (TopBitmap <> Nil) then FreeAndNil(TopBitmap);
      Panel2.Height := 0;
      BaseTopBitmap := Nil;
      TopBitmap := Nil;
   {$EndIf}
end;


procedure TImageDisplayForm.Changecolumns1Click(Sender: TObject);
var
   bmp : tMyBitmap;
   files : tStringList;
   fName : PathStr;
begin
   files := tStringList.Create;
   Files.LoadFromFile(BigBM_files);
   ReadDefault('Number of Columns (images to combine =' + IntToStr(files.Count) + ')',MDDef.BigBM_Nc);
   bmp := CombineBitmaps(MDDef.BigBM_Nc,files,BigBM_Capt);
   fName := NextFileNumber(MDTempDir,'new_cols_','.bmp');
   BMP.saveToFile(fName);
   LoadImage(fName);
   bmp.free;
   Files.Free;
end;

procedure TImageDisplayForm.ClipboardSpeedButtonClick(Sender: TObject);
begin
   AssignImageToClipBoard(Image1);
end;

procedure TImageDisplayForm.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if (ImageDoingWhat = imDoingNothing) and (Button = mbLeft) then begin
      SX := X;  // X start co-ordinate, image panning
      SY := Y;  // Y start co-ordinate, image panning
      ImageMouseIsDown := true;
      Forms.Screen.Cursor := crHandPoint;
      exit;
   end;

   if (ImageDoingWhat = imPickingBox) then begin
      FirstX := x;
      FirstY := y;
      ImageMouseIsDown := true;
   end;

   {$IfDef ExImageOverlays}
   {$Else}
      if (Overlays <> Nil) then begin
         Overlays.ApplyFilter('');
         {$IfDef RecordImageOverlayProblems} WriteLineToDebugFile('Overlay records: ' + IntToStr(Overlays.RecordCount));      {$EndIf}
         Overlays.ApplyFilter('(X < ' + IntToStr(x) + ') AND (WIDTH > ' + IntToStr(x) + ') AND (Y < ' + IntToStr(y) + ') AND (HEIGHT > ' + IntToStr(y) + ')');
         {$IfDef RecordImageOverlayProblems} WriteLineToDebugFile('Overlay filter: ' + Overlays.Filter + '  Found: ' + IntToStr(Overlays.RecordCount)); {$EndIf}
         if (Overlays.RecordCount > 0) then begin
            ImageDoingWhat := DraggingBMP;
            XBMPSize := Overlays.GetFieldByNameAsInteger('WIDTH') - Overlays.GetFieldByNameAsInteger('X');
            YBMPSize := Overlays.GetFieldByNameAsInteger('HEIGHT') - Overlays.GetFieldByNameAsInteger('Y');
            LastX := x;
            LastY := y;
         end;
      end;
   {$EndIf}

   if (Panel2.Height > 0) then begin
      FirstX := x;
      FirstY := y;
      ImageMouseIsDown := true;
      Forms.Screen.Cursor := crDrag;
   end;

   if (Button = mbRight) then begin
      RightClickX := x * 100 div ImageBlowUp;
      RightClickY := y * 100 div ImageBlowUp;
      CopytColortoclipboard1.Visible := MDDef.ProgramOption = ExpertProgram;
      PopUpMenu1.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   end;
   {$IfDef RegisterPhoto}
      if (ImageDoingWhat = DigitizeOnPhoto) then begin
         LastLat := 999;
         ImageMouseIsDown := true;
      end;
   {$EndIf}
end;

procedure TImageDisplayForm.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift : TShiftState; X, Y: Integer);
begin
   if (Panel2.Height > 0) and ImageMouseIsDown then begin
      ShiftImageX := ShiftImageX + (x - FirstX);
      ShiftImageY := ShiftImageY + (Y - FirstY);
      ShiftAlphaBlending;
      ImageMouseIsDown := false;
      ShowDefaultCursor;
   end;
   if ImageMouseIsDown and (ImageDoingWhat = imDoingNothing) then begin
      ImageMouseIsDown := false;
      ShowDefaultCursor;
   end;

   if (ImageDoingWhat = imPickingBox) and ImageMouseIsDown then begin
      LastX := x;
      LastY := y;
      ImageMouseIsDown := false;
      PopUpMenu2.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   end;

   {$IfDef ExImageOverlays}
   {$Else}
      if (Overlays <> Nil) and (ImageDoingWhat = DraggingBMP) then begin
         Overlays.Edit;
         Overlays.SetFieldByNameAsInteger('X',x);
         Overlays.SetFieldByNameAsInteger('Y',y);
         Overlays.SetFieldByNameAsInteger('WIDTH',x + xbmpsize);
         Overlays.SetFieldByNameAsInteger('HEIGHT',y + ybmpsize);
         Overlays.Post;
         Overlays.ApplyFilter('');
         ImageDoingWhat := imDoingNothing;
         DrawOverlays;
      end;
   {$EndIf}

   {$IfDef RegisterPhoto} ImageMouseIsDown := false; {$EndIf}
end;


procedure TImageDisplayForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   {$IfDef RecordClosing} WriteLineToDebugFile('TImageFm.FormCloseQuery in'); {$EndIf}
   CanClose := CanCloseItself;
end;

procedure TImageDisplayForm.Subdueimage1Click(Sender: TObject);
begin
   Recolor(MakeSubdued);
end;


procedure TImageDisplayForm.Subsetthispicture1Click(Sender: TObject);
begin
   {$IfDef RecordBitmapEdit}
      WriteLineToDebugFile('Picked box, x=' + IntToStr(Firstx) + 'x' + IntToStr(EndX) + '  y=' +  IntToStr(Firsty) + 'x' + IntToStr(Endy));
      WriteLineToDebugFile('Actual box, x=' + IntToStr(Firstx * 100 div ImageBlowUp) + 'x' + IntToStr(EndX * 100 div ImageBlowUp) + '  y=' +
          IntToStr(Firsty * 100 div ImageBlowUp) + 'x' + IntToStr(Endy * 100 div ImageBlowUp));
   {$EndIf}
   ReplaceBitmapWithSubset(EditBMP,FirstX * 100 div ImageBlowUp,EndX * 100 div ImageBlowUp,FirstY * 100 div ImageBlowUp,EndY * 100 div ImageBlowUp);
   LoadImage(EditBMP);
   {$IfDef RecordBitmapEdit} WriteLineToDebugFile('Aspect ratio: ' + realToString(EditBMP.Width/EditBMP.Height,-12,-4)); {$EndIf}
end;

procedure TImageDisplayForm.Pastefromclipboard1Click(Sender: TObject);
begin
   Image1.Picture.Bitmap.Assign(ClipBoard);
   Caption := 'Image from clipboard';
   GetImagePartOfBitmap(EditBMP);
   FormResize(Nil);
end;



procedure TImageDisplayForm.Usethissubsetonmultiplebitmaps1Click(Sender: TObject);
var
   FilesWanted : TStringList;
   df : byte;
   bmp : tBitmap;
   fName : PathStr;
   i : integer;
begin
   FilesWanted := tStringList.Create;
   FilesWanted.Add(ExtractFilePath(LoadedFileName));
   df := 1;
   if GetMultipleFiles('to subset',AllowedGraphicsFilters,FilesWanted,df) then begin
      StartProgress('Subset');
      for I := 0 to pred(FilesWanted.Count) do begin
         UpdateProgressBar(i/FilesWanted.Count);
         fName := FilesWanted.Strings[i];
         bmp := LoadBitmapFromFile(fName);
         ReplaceBitmapWithSubset(EditBMP,FirstX * 100 div ImageBlowUp,EndX * 100 div ImageBlowUp,FirstY * 100 div ImageBlowUp,EndY * 100 div ImageBlowUp);
         fName := ExtractFilePath(fName) + 'sub_' + extractFileName(fName);
         SaveBitmap(bmp,fName);
      end;
      EndProgress;
   end;
   FilesWanted.Free;
end;

procedure TImageDisplayForm.VClick(Sender: TObject);
begin
   ShiftImageY := ShiftImageY + 1;
   ShiftAlphaBlending;
end;

procedure TImageDisplayForm.Yellow1Click(Sender: TObject);
begin
    MakeNewImageForm(TurnYellow,LoadedFileName);
end;

procedure TImageDisplayForm.Colorcrosssections1Click(Sender: TObject);
begin
   ImageDoingWhat := ColorCrossSection;
end;

procedure TImageDisplayForm.Pickhorizon1Click(Sender: TObject);
const
   HorizonFactor : integer = 25;
var
   x,y : integer;
   LastBlue,r,g,b : byte;
begin
   ReadDefault('Horizon factor',HorizonFactor);
   for x := 0 to pred(EditBMP.Width) do begin
      LastBlue := 0;
      for y := 0 to pred(EditBMP.Height) do begin
         GetRGBfromTColor(EditBMP.Canvas.Pixels[x,Y],r,g,b);
         if (y > 0) and (HorizonFactor + b < LastBlue) then begin
            EditBMP.Canvas.Pixels[x,Y] := clBlue;
            break;
         end;
         LastBlue := B;
      end;
   end;
   LoadImage(EditBMP);
end;

procedure TImageDisplayForm.Picksolidcolor1Click(Sender: TObject);
begin
   QueryColor(MDDef.ReplaceBorderColor);
end;

procedure TImageDisplayForm.Pickthispointforsolidcolor1Click(Sender: TObject);
begin
   MDDef.ReplaceBorderColor := ConvertTColorToPlatformColor(EditBMP.Canvas.Pixels[RightClickX,RightClickY]);
end;

procedure TImageDisplayForm.PutEXIFdataintoJPEGS1Click(Sender: TObject);
{$IfDef ExExif}
begin
{$Else}
var
   NewFileName : PathStr;
   Bitmap : tMyBitmap;
   FilterNum : byte;
   TheFiles : tstringlist;
   i : integer;
begin
   ShowHourglassCursor;
   TheFiles := tstringlist.Create;
   TheFiles.Add(ExtractFilePath(LoadedFileName));
   if Petmar.GetMultipleFiles('JPEGS to copy EXIF data','JPEG|*.jpg',TheFiles,FilterNum) then begin
      for i := 0 to pred(TheFiles.Count) do begin
         NewFileName := TheFiles.Strings[i];
         Bitmap := LoadBitmapFromFile(NewFileName);
         SaveJPEGWithExif(LoadedFileName,NewFileName,Bitmap);
         Bitmap.Free;
      end;
   end;
   TheFiles.Free;
   ShowDefaultCursor;
{$EndIf}
end;

procedure TImageDisplayForm.Refresh1Click(Sender: TObject);
begin
   RedrawSpeedButton12Click(Sender);
end;

procedure TImageDisplayForm.Reloadimage1Click(Sender: TObject);
begin
   LoadImage(LoadedFileName);
end;

procedure TImageDisplayForm.RedrawSpeedButton12Click(Sender: TObject);
begin
   LoadImage(LoadedFileName);
end;

procedure TImageDisplayForm.SpeedButton7Click(Sender: TObject);
begin
   {$IfDef RegisterPhoto}
      ImageDoingWhat := LocatePoints;
   {$EndIf}
end;

procedure TImageDisplayForm.SpeedButton8Click(Sender: TObject);
begin
  {$IfDef RegisterPhoto}
     ImageDoingWhat := DigitizeOnPhoto;
  {$EndIf}
end;

procedure TImageDisplayForm.SpeedButton9Click(Sender: TObject);
begin
   ShiftImageX := ShiftImageX - 1;
   ShiftAlphaBlending;
end;

procedure TImageDisplayForm.MakeNewImageForm(Method : RecolorMethod; fName : PathStr);
var
   NewImageForm : TImageDisplayForm;
begin
   NewImageForm := TImageDisplayForm.Create(Application);
   NewImageForm.LoadImage(fName);
   NewImageForm.Recolor(Method);
end;

procedure TImageDisplayForm.MakeRGBgrayscales1Click(Sender: TObject);
begin
   MakeNewImageForm(BlueGrayscale,LoadedFileName);
   MakeNewImageForm(RedGrayscale,LoadedFileName);
   MakeNewImageForm(GreenGrayscale,LoadedFileName);
end;

procedure TImageDisplayForm.MakeRGBseparates1Click(Sender: TObject);
begin
   MakeNewImageForm(TurnBlue,LoadedFileName);
   MakeNewImageForm(TurnRed,LoadedFileName);
   MakeNewImageForm(TurnGreen,LoadedFileName);
end;

procedure TImageDisplayForm.Makethislowerrightcorner1Click(Sender: TObject);
begin
   MakeThisLowerRightCornerOfBitmap(EditBMP,RightClickX,RightClickY);
   LoadImage(EditBMP);
end;

procedure TImageDisplayForm.Makethisupperleftcorner1Click(Sender: TObject);
begin
   MakeThisUpperLeftCornerOfBitmap(EditBMP,RightClickX,RightClickY);
   LoadImage(EditBMP);
end;


procedure TImageDisplayForm.Zoom1Click(Sender: TObject);
begin
   ReadDefault('Blowup factor (%)',ImageBlowup);
   ResizeImage(XBMPSize * ImageBlowUp div 100, YBMPSize * ImageBlowUp div 100);
end;

procedure TImageDisplayForm.ZoomInSpeedButton4Click(Sender: TObject);
begin
   ShiftImageBlowUp := ShiftImageBlowUp * 1.02;
   ShiftAlphaBlending;
end;


procedure TImageDisplayForm.ZoomOutSpeedButton5Click(Sender: TObject);
begin
   ShiftImageBlowUp := ShiftImageBlowUp * 0.98;
   ShiftAlphaBlending;
end;

procedure TImageDisplayForm.Pastefromclipboard2Click(Sender: TObject);
begin
   Pastefromclipboard1Click(Sender);
end;

initialization
   {$IfDef MessageStartUpUnitProblems} MessageToContinue('Startup petimage'); {$EndIf}
   LastPhotoRoamX := -1;
   LastPhotoRoamY := -1;
finalization
   {$IfDef RecordImageOverlayProblems} WriteLineToDebugFile('RecordImageOverlayProblems active in PetImage_form'); {$EndIf}
   {$IfDef RecordImageLoadProblems} WriteLineToDebugFile('RecordImageLoadProblems active in PetImage_form'); {$EndIf}
   {$IfDef RecordRoamOnMapProblems} WriteLineToDebugFile('RecordRoamOnMapProblems active in PetImage_form'); {$EndIf}
   {$IfDef RecordImageResize} WriteLineToDebugFile('RecordImageResize active in PetImage_form'); {$EndIf}
   {$IfDef RecordBlendBitmaps} WriteLineToDebugFile('RecordBlendBitmaps active in PetImage_form'); {$EndIf}
   {$IfDef RecordJPEG} WriteLineToDebugFile('RecordJPEG active in PetImage_form'); {$EndIf}
   {$IfDef RecordIHSmerges} WriteLineToDebugFile('RecordIHSmerges active in PetImage_form'); {$EndIf}
   {$IfDef RecordPNG} WriteLineToDebugFile('RecordPNG active in PetImage_form'); {$EndIf}
   {$IfDef RecordBitmapProblems} WriteLineToDebugFile('RecordBitmapProblems active in PetImage_form'); {$EndIf}
   {$IfDef RecordGetImagePartOfBitmap} WriteLineToDebugFile('RecordGetImagePartOfBitmap active in PetImage_form'); {$EndIf}
   {$IfDef RecordBitmapEdit} WriteLineToDebugFile('RecordBitmapEdit active in PetImage_form'); {$EndIf}
end.



