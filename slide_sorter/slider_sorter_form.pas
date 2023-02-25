unit slider_sorter_form;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2022 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define LogSlideSorter}
{$EndIf}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, StrUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ExtCtrls, FMX.Menus,
  Petmar_db;

const
   pmThreeEqual = 1;
   pmThreeBig = 2;
   pmFourEqual = 3;

type
  TSlideSorterForm = class(TForm)
    LeftPanel1: TPanel;
    RightPanel2: TPanel;
    CenterPanel3: TPanel;
    Panel4: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Panel5: TPanel;
    Button5: TButton;
    Button6: TButton;
    Panel6: TPanel;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    ImageViewer1: TImageViewer;
    ImageViewer2: TImageViewer;
    ImageViewer3: TImageViewer;
    Button4: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button13: TButton;
    Button14: TButton;
    PopupMenu1: TPopupMenu;
    MenuItem1: TMenuItem;
    SpeedButton1: TSpeedButton;
    FarRightPanel1: TPanel;
    Panel2: TPanel;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Label4: TLabel;
    Button19: TButton;
    Button20: TButton;
    ImageViewer4: TImageViewer;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ImageViewer2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ImageViewer2MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Single);
    procedure ImageViewer2MouseUp(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Single);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure ImageViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ImageViewer3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ImageViewer4MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ImageViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Single);
    procedure ImageViewer3MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Single);
    procedure ImageViewer4MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Single);
    procedure ImageViewer1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ImageViewer3MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ImageViewer4MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure ImageViewer2Click(Sender: TObject);
    procedure ImageViewer1DblClick(Sender: TObject);
    procedure ImageViewer3DblClick(Sender: TObject);
    procedure ImageViewer4DblClick(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
  private
    { Private declarations }
    LastMouseX,LastMouseY,
    StartMouseX,StartMouseY : Single;
    MouseIsDown : boolean;
    procedure CenterImageViewer(ImageViewer : tImageViewer);
    procedure MouseMove(ImageViewer : tImageViewer; x,y : single);
    procedure ImageViewerMouseDown(ImageViewer : tImageViewer; X, Y: Single);
  public
    { Public declarations }
    CurThumbnail,
    Advance,
    OnSlide : integer;
    OpenZoom,
    Working : boolean;
    PhotoDB : tMyData;
    fName : array[1..4] of Shortstring;
    FileNames : tStringList;
    procedure LoadPictures;
    procedure DeleteAFile(Num : integer);
  end;


procedure StartSlideSorter;


implementation

{$R *.fmx}


uses
   {$IfDef ExExif} {$Else} ccr.exif, {$EndIf}
   ccr.exif,
   Petmar,Petmar_types,PetImage,DEMdefs, petimage_form,
   JpegDumpForm,
   Nevadia_main;



procedure StartSlideSorter;
var
  SlideSorterForm : TSlideSorterForm;
   TheFiles : tStringList;
   fName,fName2 : PathStr;

         procedure FindFiles;
         var
            i : integer;
         begin
            TheFiles := Nil;
            Petmar.FindMatchingFiles(PhotoDir,'*.*',TheFiles,6);
            for i := Pred(TheFiles.Count) downto 0 do begin
                if (not PetImage.ValidImageFileName(TheFiles.Strings[i])) then TheFiles.Delete(i);
            end;
         end;

      procedure CheckType(Key : shortstring);
      var
         I : Integer;
      begin
         for I := 0 to pred(TheFiles.Count) do begin
            fName := UpperCase(ExtractFileNameNoExt(TheFiles.Strings[i]));
            if (length(Fname) = 8) and StrUtils.AnsiContainsText(ExtractFileNameNoExt(fName),Key) then begin
               RenamePhotoJPEGS(PhotoDir,Key);
               TheFiles.Free;
               FindFiles;
               exit;
            end;
         end;
      end;


begin
   if (PhotoDir = '') then PhotoDir := MainMapData;

   if GetDosPath('Photo directory',PhotoDir) then begin
      FindFiles;
      if (TheFiles.Count > 0) then begin
         CheckType('DSC');
         CheckType('P');
         TheFiles.SaveToFile(MDTempDir + 'slide_sorter.txt');
         fName2 := PhotoDir + 'photo_index' + DefaultDBExt;
         if not FileExists(fName2) then MakePhotoDB(PhotoDir);
         SlideSorterForm := TSlideSorterForm.Create(Application);
         SlideSorterForm.Show;
         SlideSorterForm.WindowState := TWindowState.wsMaximized;
         SlideSorterForm.PhotoDB := tMyData.Create(fName2);
         SlideSorterForm.LoadPictures;
         SlideSorterForm.Caption := SlideSorterForm.Caption + '  ' + PhotoDir;
      end;
      TheFiles.Free;
   end;
end;



procedure TSlideSorterForm.DeleteAFile(Num : integer);
var
   FileName : string;
begin
   if (Num < FileNames.Count) then begin
      FileName := FileNames[Num];
      File2Trash(FileName);
   end;
end;


procedure TSlideSorterForm.MouseMove(ImageViewer : tImageViewer; x,y : single);
begin
   if MouseIsDown then begin
      ImageViewer.ScrollBy(x-StartMouseX,Y-StartMousey);
      StartMouseX := x;
      StartMousey := y;
   end;
   LastMouseX := x;
   LastMouseY := y;
end;

procedure TSlideSorterForm.ImageViewerMouseDown(ImageViewer : tImageViewer; X, Y: Single);
begin
   StartMouseX := x;
   StartMouseY := y;
   MouseIsDown := true;
end;


procedure TSlideSorterForm.CenterImageViewer(ImageViewer : tImageViewer);
var
   xs,ys : single;
begin
   //WriteLineToDebugFile('Initial ViewPort  x=' +  RealToString(ImageViewer.ViewPortPosition.x,-12,-2)  + '  y=' +  RealToString(ImageViewer.ViewPortPosition.y,-12,-2) );
   xs := round( (0.5 * ImageViewer.ClientWidth - LastMouseX));
   ys := round( (0.5 * ImageViewer.ClientHeight - LastMouseY));
   //WriteLineToDebugFile('   Move ViewPort  x=' +  RealToString(xs,-12,-2)  + '  y=' +  RealToString(ys,-12,-2) );
   ImageViewer.ScrollBy(xs,ys);
   ImageViewer.BitmapScale := 1;   //ImageViewer2.BitmapScale * 2;
   //WriteLineToDebugFile('   Later ViewPort  x=' +  RealToString(ImageViewer.ViewPortPosition.x,-12,-2)  + '  y=' +  RealToString(ImageViewer.ViewPortPosition.y,-12,-2) );
   //WriteLineToDebugFile('    Last mouse, x=' +  RealToString(LastMouseX,-12,-2)  + '  y=' +  RealToString(LastMouseY,-12,-2) + '  Scale: ' + RealToString( ImageViewer.BitmapScale,-12,-2));
end;



procedure TSlideSorterForm.Button13Click(Sender: TObject);
begin
  Button13.Enabled := false;
  if (MDDef.PictureMode = pmFourEqual) then  inc(OnSlide,4*Advance)
  else inc(OnSlide,3*Advance);
  LoadPictures;
  Button13.Enabled := true;
end;

procedure TSlideSorterForm.Button14Click(Sender: TObject);
begin
   Button14.Enabled := false;
   DeleteAFile(OnSlide);
   DeleteAFile(OnSlide+1);
   DeleteAFile(OnSlide+2);
   if (MDDef.PictureMode = pmFourEqual) then DeleteAFile(OnSlide+3);
   LoadPictures;
   Button14.Enabled := true;
end;

procedure TSlideSorterForm.Button16Click(Sender: TObject);
begin
   FileNames.Delete(OnSlide+3);
   LoadPictures;
end;

procedure TSlideSorterForm.Button17Click(Sender: TObject);
begin
   DeleteAFile(OnSlide+3);
   LoadPictures;
end;

procedure TSlideSorterForm.Button1Click(Sender: TObject);
begin
  if (MDDef.PictureMode = pmFourEqual) then dec(OnSlide,4*Advance)
  else dec(OnSlide,3*Advance);
  LoadPictures;
end;

procedure TSlideSorterForm.Button2Click(Sender: TObject);
begin
   FileNames.Delete(OnSlide);
   LoadPictures;
end;

procedure TSlideSorterForm.Button3Click(Sender: TObject);
begin
   DeleteAFile(OnSlide);
   LoadPictures;
end;

procedure TSlideSorterForm.Button5Click(Sender: TObject);
begin
   FileNames.Delete(OnSlide+1);
   LoadPictures;
end;

procedure TSlideSorterForm.Button6Click(Sender: TObject);
begin
   DeleteAFile(OnSlide+1);
   LoadPictures;
end;

procedure TSlideSorterForm.Button7Click(Sender: TObject);
begin
  inc(OnSlide);
  LoadPictures;
end;

procedure TSlideSorterForm.Button8Click(Sender: TObject);
begin
   FileNames.Delete(OnSlide+2);
   LoadPictures;
end;

procedure TSlideSorterForm.Button9Click(Sender: TObject);
begin
   DeleteAFile(OnSlide+2);
   LoadPictures;
end;


procedure TSlideSorterForm.FormActivate(Sender: TObject);
begin
   LoadPictures;
   OnSlide := 1;
   LoadPictures;
end;

procedure TSlideSorterForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Action :=  TCloseAction.caFree;
    FileNames.Destroy;
    PhotoDB.Destroy;
end;

procedure TSlideSorterForm.FormCreate(Sender: TObject);
begin
   Working := false;
   MouseIsDown := false;
   OpenZoom := false;
   fName[1] := '';
   fName[2] := '';
   fName[3] := '';
   fName[4] := '';
   FileNames := tStringList.Create;
   FileNames.LoadFromFile(MDTempDir + 'slide_sorter.txt');
   OnSlide := 0;
   FormResize(nIl);
   Advance := 1;
end;


procedure TSlideSorterForm.FormResize(Sender: TObject);
begin
   if (MDDef.PictureMode = pmThreeEqual) then begin
     LeftPanel1.Width := ClientWidth div 3;
     RightPanel2.Width := ClientWidth div 3;
     CenterPanel3.Width := ClientWidth div 3;
     LeftPanel1.Align := tAlignLayout.FitLeft;
     RightPanel2.Align := tAlignLayout.FitRight;
     CenterPanel3.Align := tAlignLayout.HorzCenter;
     FarRightPanel1.Visible := false;
   end
   else if (MDDef.PictureMode = pmFourEqual) then begin
     FarRightPanel1.Visible := true;
     LeftPanel1.Align := tAlignLayout.None;
     RightPanel2.Align := tAlignLayout.None;
     FarRightPanel1.Align := tAlignLayout.None;
     CenterPanel3.Align := tAlignLayout.None;

     LeftPanel1.Position.x := 0;
     LeftPanel1.Position.y := 0;
     CenterPanel3.Position.x := ClientWidth div 2;
     CenterPanel3.Position.y := 0;

     RightPanel2.Position.x := 0;
     RightPanel2.Position.y := ClientHeight div 2;
     FarRightPanel1.Position.x := ClientWidth div 2;
     FarRightPanel1.Position.y := ClientHeight div 2;

     LeftPanel1.Width := ClientWidth div 2;
     RightPanel2.Width := ClientWidth div 2;
     CenterPanel3.Width := ClientWidth div 2;
     FarRightPanel1.Width := ClientWidth div 2;

     LeftPanel1.Height := ClientHeight div 2;
     RightPanel2.Height := ClientHeight div 2;
     CenterPanel3.Height := ClientHeight div 2;
     FarRightPanel1.Height := ClientHeight div 2;
   end
   else if (MDDef.PictureMode = pmThreeBig) then begin
     LeftPanel1.Width := ClientWidth div 10;
     RightPanel2.Width := ClientWidth div 10;
     CenterPanel3.Width := 8* ClientWidth div 10;
     LeftPanel1.Align := tAlignLayout.FitLeft;
     RightPanel2.Align := tAlignLayout.FitRight;
     CenterPanel3.Align := tAlignLayout.HorzCenter;
     FarRightPanel1.Visible := false;
   end;
end;


procedure TSlideSorterForm.ImageViewer1DblClick(Sender: TObject);
begin
   CenterImageViewer(ImageViewer1);
end;

procedure TSlideSorterForm.ImageViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
   ImageViewerMouseDown(ImageViewer1, X, Y);
end;


procedure TSlideSorterForm.ImageViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
   MouseMove(ImageViewer1,x,y);
end;

procedure TSlideSorterForm.ImageViewer1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
   MouseIsDown := false;
end;


procedure TSlideSorterForm.ImageViewer2Click(Sender: TObject);
begin
   CenterImageViewer(ImageViewer2);
end;

procedure TSlideSorterForm.ImageViewer2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
   ImageViewerMouseDown(ImageViewer2, X, Y);
end;

procedure TSlideSorterForm.ImageViewer2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
   MouseMove(ImageViewer2,2,y);
end;


procedure TSlideSorterForm.ImageViewer2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
   MouseIsDown := false;
end;


procedure TSlideSorterForm.ImageViewer3DblClick(Sender: TObject);
begin
   CenterImageViewer(ImageViewer3);
end;

procedure TSlideSorterForm.ImageViewer3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
   ImageViewerMouseDown(ImageViewer3, X, Y);
end;

procedure TSlideSorterForm.ImageViewer3MouseMove(Sender: TObject;  Shift: TShiftState; X, Y: Single);
begin
   MouseMove(ImageViewer3,x,y);
end;

procedure TSlideSorterForm.ImageViewer3MouseUp(Sender: TObject;  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
   MouseIsDown := false;
end;

procedure TSlideSorterForm.ImageViewer4DblClick(Sender: TObject);
begin
   CenterImageViewer(ImageViewer4);
end;

procedure TSlideSorterForm.ImageViewer4MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
   ImageViewerMouseDown(ImageViewer4, X, Y);
end;

procedure TSlideSorterForm.ImageViewer4MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
   MouseMove(ImageViewer4,x,y);
end;

procedure TSlideSorterForm.ImageViewer4MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
   MouseIsDown := false;
end;

function LoadBitmapFromFile(fName : PathStr) : tBitmap;
var
   ExifData: TExifData;
   Ext : ExtStr;
begin
   Result := tBitmap.Create;
   if FileExists(fName) then begin
       Result.LoadFromFile(fName);
       Ext := UpperCase(ExtractFileExt(fName));
       if ExtEquals(Ext, '.JPG') or ExtEquals(Ext, '.JPEG') or ExtEquals(Ext, '.JPE') then begin
           ExifData := TExifData.Create;
           ExifData.EnsureEnumsInRange := False; //as we use case statements rather than array constants, no need to keep this property set to True
           ExifData.LoadFromGraphic(fName);
           if (not ExifData.Empty) then begin
              if (ExifData.Orientation in [toLeftBottom,toBottomRight]) then Result.Rotate(180);
              if (ExifData.Orientation = toRightTop) then Result.Rotate(90);
              if (ExifData.Orientation = toLeftBottom) then Result.Rotate(270);
           end;
           ExifData.Destroy;
       end;
   end;
end;


procedure TSlideSorterForm.LoadPictures;

         procedure LoadEm(SlideNum,Curslide : integer; Viewer : tImageViewer; theLabel : tLabel);
         var
            Bitmap : tBitmap;
            TStr : shortstring;
         begin
            while (SlideNum < FileNames.Count) and (SlideNum >= 0) and (not FileExists(FileNames[SlideNum])) do FileNames.Delete(SlideNum);
            if (SlideNum < FileNames.Count) and (SlideNum >= 0) then begin
               if fName[CurSlide] <>  FileNames[SlideNum] then begin
                  fName[CurSlide] := FileNames[SlideNum];
                  Bitmap := LoadBitmapFromFile(FileNames[SlideNum]);
                  theLabel.width := ClientWidth;

                  PhotoDB.ApplyFilter('IMAGE=' + QuotedStr(FileNames[SlideNum]));
                  if (PhotoDB.RecordCount = 1) then begin
                     TStr := '  ' + PhotoDB.GetFieldByNameAsString('CAMERA') +'  focal=' + PhotoDB.GetFieldByNameAsString('FOCAL_LEN') + '  35mm=' + PhotoDB.GetFieldByNameAsString('FOCAL_35') +
                             '  ISO=' + PhotoDB.GetFieldByNameAsString('ISO') + '  f_stop=' + PhotoDB.GetFieldByNameAsString('F_STOP') +  '  shutter=' + PhotoDB.GetFieldByNameAsString('SHUTTER') +
                             '  EV100=' + PhotoDB.GetFieldByNameAsString('EV100');
                  end;
                  TStr := IntToStr(SlideNum+1) + '/' + IntToStr(FileNames.Count) + ' ' + ExtractFileName(FileNames[SlideNum]) + TStr;
                  theLabel.Text := TStr;
                  Viewer.Bitmap := Bitmap;
                  //Viewer.BestFit;
                  if OpenZoom then CenterImageViewer(Viewer)
                  else Viewer.BestFit;
                  Bitmap.Free;
               end;
            end
            else begin
               Viewer.Bitmap.Width := 0;
               Viewer.Bitmap.Height := 0;
               theLabel.Text :=  '';
            end;
         end;

begin
   if Working then exit;
   Working := true;
   if (FileNames.Count > 0) then begin
      if MDDef.PictureMode in [pmThreeBig] then begin
         if (OnSlide > FileNames.Count - 2) then OnSlide := FileNames.Count - 2;
         if (OnSlide < -1) then OnSlide := -1;
      end
      else if MDDef.PictureMode in [pmThreeEqual] then begin
         if (OnSlide > FileNames.Count - 2) then OnSlide := FileNames.Count - 2;
         if (OnSlide < 0) then OnSlide := 0;
      end
      else if MDDef.PictureMode in [pmFourEqual] then begin
         if (OnSlide > FileNames.Count - 3) then OnSlide := FileNames.Count - 3;
         if (OnSlide < 0) then OnSlide := 0;
      end
      else begin
         if (OnSlide > FileNames.Count - 1) then OnSlide := FileNames.Count - 1;
         if (OnSlide < -1) then OnSlide := -1;
      end;
      LoadEm(OnSlide,1,ImageViewer1,Label1);
      LoadEm(OnSlide+1*Advance,2,ImageViewer2,Label2);
      LoadEm(OnSlide+2*Advance,3,ImageViewer3,Label3);
      if FarRightPanel1.Visible then LoadEm(OnSlide+3*Advance,4,ImageViewer4,Label4);
  end;
   Working := false;
end;


procedure TSlideSorterForm.MenuItem10Click(Sender: TObject);
begin
    Advance := 1;
end;

procedure TSlideSorterForm.MenuItem11Click(Sender: TObject);
begin
   Advance := 5;
end;

procedure TSlideSorterForm.MenuItem12Click(Sender: TObject);
begin
   Advance := 10;
end;

procedure TSlideSorterForm.MenuItem14Click(Sender: TObject);
begin
   OpenZoom := not OpenZoom;
end;

procedure TSlideSorterForm.MenuItem15Click(Sender: TObject);
begin
   MDDef.PictureMode := pmThreeBig;
   FormResize(Nil);
   OnSlide := -1;
   repeat
      inc(OnSlide);
      LoadPictures;
      Delay(250);
   until Onslide =  FileNames.Count - 2;

end;

procedure TSlideSorterForm.MenuItem1Click(Sender: TObject);
begin
  OnSlide := 0;
  LoadPictures;
end;

procedure TSlideSorterForm.MenuItem2Click(Sender: TObject);
begin
    LoadFileToClipboard(fName[CurThumbnail]);
end;

procedure TSlideSorterForm.MenuItem3Click(Sender: TObject);
begin
   MDDef.PictureMode := pmThreeEqual;
   FormResize(Nil);
end;

procedure TSlideSorterForm.MenuItem4Click(Sender: TObject);
begin
   MDDef.PictureMode := pmFourEqual;
   FormResize(Nil);
end;

procedure TSlideSorterForm.MenuItem5Click(Sender: TObject);
begin
   MDDef.PictureMode := pmThreeBig;
   FormResize(Nil);
end;

procedure TSlideSorterForm.MenuItem6Click(Sender: TObject);
begin
   {$IfDef LogSlideSorter}   WriteLineToDebugFile('Call editor, fname=' + fName[CurThumbnail]);   {$EndIf}

   Petimage_form.DisplayBitmap(fName[CurThumbnail],'',true);
end;

procedure TSlideSorterForm.MenuItem8Click(Sender: TObject);
begin
   Close;
   wmdem.Slidesorter1Click(Nil);
end;

procedure TSlideSorterForm.SpeedButton1Click(Sender: TObject);
var
  FP: TPointF;
begin
   CurThumbnail := 2;
  //Initialize the coordinates to the origin of the button control.
  FP.X := 0;
  FP.Y := 0;
  //Transposes coordinates in context of  form.
  FP := SpeedButton1.LocalToAbsolute(FP);
  //Transposes the coordinates in the context of the screen.
  FP := ClientToScreen(FP);
  //Display popup menu at computed coordinates.
  PopupMenu1.Popup(FP.X, FP.Y);
end;


initialization
finalization
   {$IfDef LogSlideSorter}   WriteLineToDebugFile('LogSlideSorter active in slider_sorter_form');   {$EndIf}
end.
