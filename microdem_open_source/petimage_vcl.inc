{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{     include file for pet_image.pas     }
{________________________________________}


function IsJPEG(Ext : ExtStr) : boolean;
begin
    Result := ExtEquals(Ext, '.JPEG') or ExtEquals(Ext, '.JPG') or ExtEquals(Ext, '.JPE');
end;

function ValidImageFileExt(Ext : ExtStr) : boolean;
begin
   Result := ExtEquals(Ext, '.BMP') or
      IsJPEG(Ext) or ExtEquals(Ext, '.JPS')
      {$IfDef ExPNG}  {$Else} or ExtEquals(Ext, '.PNG') or ExtEquals(Ext,'.PNS') {$EndIf}
      {$IfDef ExGIF}  {$Else} or ExtEquals(Ext, '.GIF') {$EndIf}
      {$IfDef ExTiff} {$Else} or ExtEquals(Ext, '.TIF') {$EndIf};
end;

function ValidImageFileName(fName : PathStr) : boolean; overload;
begin
  Result := ValidImageFileExt(ExtractFileExt(fName));
end;


function GraphicsFilters : shortstring;
begin
   Result := 'BMP|*.bmp|JPEG (quick)|*.jpg|JPEG (deliberate)|*.jpg'  {$IfDef ExGIF} {$Else} + '|GIF|*.gif' {$EndIf} {$IfDef ExPNG} {$Else} + '|PNG|*.png' {$EndIf};
end;

function ThreeDeeGraphicsFilters : shortstring;
begin
   Result := 'Stereo PNG|*.PNS|Stereo JPEG|*.JPS';
end;


function AllowedGraphicsFilters : shortstring;
begin
   Result := 'Graphics files|*.bmp;*.jpg;*.jpeg;*.jpe;*.tif;*.tiff' {$IfDef ExGIF} {$Else} + ';*.gif' {$EndIf} {$IfDef ExPNG} {$Else} + ';*.png' {$EndIf};
end;


   procedure RenamePhotoJPEGS(PhotoDir : PathStr = ''; NameContains : shortString = '');
   var
      OldName,NewName,BaseName : PathStr;
      Ext : extstr;
      i,SubDirs : integer;
      TheFiles : tStringList;
      TStr : shortString;
      Ask : boolean;
   begin
      {$IfDef RecordFileOps} WriteLineToDebugFile('Twmdem.RenameJPEGswithcreationtime1Click in'); {$EndIf}
      Ask := (PhotoDir = '');
      if (PhotoDir <> '') or GetDosPath('files to rename',PhotoDir) then begin
         SubDirs := 1;
         Ext := '.jpg';
         if (NameContains = '') then NameContains := 'DSC_';
         if Ask then begin
            Petmar.ReadDefault('subdirectoy level',SubDirs);
            Petmar.GetString('file extension',Ext,false,ValidDosFileNameChars);
            Petmar.GetString('name starts with',NameContains,false,ValidDosFileNameChars);
         end;
         TheFiles := nil;
         Petmar.FindMatchingFiles(PhotoDir,'*' + Ext,TheFiles,SubDirs);
         StartProgress('Rename');
         for i := 0 to pred(TheFiles.Count) do begin
            if (i mod 10 = 0) then UpdateProgressBar(i/TheFiles.Count);
            OldName := TheFiles.Strings[i];
            if StrUtils.AnsiContainsText(Copy(ExtractFileNameNoExt(OldName),1,length(NameContains)),NameContains) then begin
               if (NameContains[length(NameContains)] <> '_') then TStr := '_' else TStr := '';
               BaseName := NameContains + TStr + FileTimeFromFileName(OldName);
               NewName := ExtractFilePath(OldName) + BaseName + Ext;
               while FileExists(NewName) do begin
                  NewName := Petmar.NextFileNumber(ExtractFilePath(OldName),BaseName + '_',Ext);
               end;
               {$IfDef RecordFileOps} WriteLineToDebugFile('OldName=' + Oldname + ' new name=' + ExtractFileName(NewName)); {$EndIf}
               System.SysUtils.RenameFile(OldName,NewName);
            end;
         end;
         TheFiles.Free;
         EndProgress;
      end;
      {$IfDef RecordFileOps} WriteLineToDebugFile('Twmdem.RenameJPEGswithcreationtime1Click out'); {$EndIf}
   end;


   procedure SetRedrawMode(Image1 : tImage);  inline;
   begin
      Image1.Canvas.Pen.Mode := pmNotXor;
      Image1.Canvas.Pen.Color := clRed;
      Image1.Canvas.Pen.Width := 2;
      Image1.Canvas.Brush.Style := bsClear;
   end;


      procedure ReplaceBitmapWithSubset(var Bitmap : tMyBitmap; Left,Right,Top,Bottom : integer);
      var
         NewBitmap : tMyBitmap;
      begin
         CreateBitmap(NewBitmap,Bitmap.Width,Bitmap.Height);
         NewBitmap.Canvas.Draw(0,0,Bitmap);
         Bitmap.Free;
         CreateBitmap(Bitmap,Right - Left, Bottom - Top);
         ClearBitmap(Bitmap,clNearWhite);
         Bitmap.Canvas.CopyRect(Rect(0,0,Bitmap.Width,Bitmap.Height),NewBitmap.Canvas,Rect(Left,Top,Right,Bottom));
         NewBitmap.Free;
      end;


      procedure MakeThisLowerRightCornerOfBitmap(var Bitmap : tMyBitmap; x,y : integer);
      begin
         ReplaceBitmapWithSubset(Bitmap,0,x,0,y);
      end;


      procedure MakeThisUpperLeftCornerOfBitmap(var Bitmap : tMyBitmap;x,y : integer);
      begin
         ReplaceBitmapWithSubset(Bitmap,x,pred(Bitmap.Width),y,pred(Bitmap.Height));
      end;

   function LoadBitmapFromFile(fName : PathStr) : tMyBitmap;
   var
      Ext    : ExtStr;
      MyJPEG : TJPEGImage;
      {$IfDef ExGIF}
      {$Else}
         GIF : TGIFImage;
      {$EndIf}
      {$IfDef ExExif}
      {$Else}
         ExifData: TExifData;
      {$EndIf}

      {$IfDef ExPNG}
      {$Else}
         MyPNG : TPNGImage;
      {$EndIf}
      {$IfDef ExTiff}
      {$Else}
         Success : boolean;
         TiffImage : tTiffImage;
      {$EndIf}
   begin
      if (not FileExists(fName)) or (fName = '') then  begin
         Result := Nil;
         exit;
      end;
      {$IfDef RecordBitmapProblems} WriteLineToDebugFile('in LoadBitmapFromFile ' + '  fname=' + fName);  {$EndIf}

      Ext := ExtractFileExt(fName);

      {$IfDef ExTiff}
      {$Else}
         if ExtEquals(Ext, '.tif') or ExtEquals(Ext, '.tiff') then begin
            TiffImage := tTIFFImage.CreateGeotiff(false,true,FName,Success);
            Result := TiffImage.DisplayInBitmap;
            TiffImage.Destroy;
            exit;
         end;
      {$EndIf}

      Result := tMyBitmap.Create;

      if ExtEquals(Ext, '.BMP') then begin
         Result.LoadFromFile(FName);
      end;

      if ExtEquals(Ext, '.JPG') or ExtEquals(Ext, '.JPEG') or ExtEquals(Ext, '.JPE') then begin
         try
            MyJPEG := TJPEGImage.Create;
            MyJPEG.LoadFromFile(FName);
            MyJPEG.Performance := jpBestQuality;
            Result.Assign(MyJPEG);
            MyJPEG.Free;

            {$IfDef ExExif}
            {$Else}
                ExifData := TExifData.Create;
                ExifData.EnsureEnumsInRange := False; //as we use case statements rather than array constants, no need to keep this property set to True
                ExifData.LoadFromGraphic(fName);
                if (not ExifData.Empty) then begin
                   if ExifData.Orientation in [toBottomRight,toRightTop,toLeftBottom] then begin
                      if (ExifData.Orientation in [toLeftBottom,toBottomRight]) then PetImage.Drehen180Grad(Result);
                      if (ExifData.Orientation = toRightTop) then PetImage.Drehen90Grad(Result);
                      if (ExifData.Orientation = toLeftBottom) then PetImage.Drehen270Grad(Result);
                   end;
                end;
                ExifData.Destroy;
             {$EndIf}
         except
            on Exception do raise;
         end;
      end;

      {$IfDef ExPNG}
      {$Else}
         if ExtEquals(Ext, '.png') then begin
            try
              MyPNG := TPNGImage.Create;
              MyPNG.LoadFromFile(fName);
              Result.Assign(MyPNG);
              MyPNG.Free;
            except
               on Exception do raise;
            end;
         end;
      {$EndIf}


      {$IfDef ExGIF}
      {$Else}
         if ExtEquals(Ext, '.gif') then begin
            GIF := TGIFImage.Create;
            GIF.LoadFromFile(fName);
            Result.Assign(GIF);
            GIF.Free;
         end;
      {$EndIf}

      Result.PixelFormat := pf24bit;
   end;


   procedure ClearBitmap(BM : tMyBitmap; Color : tColor);
   begin
      bm.Canvas.Brush.Color := Color;
      bm.Canvas.Pen.Color := Color;
      bm.Canvas.Rectangle(0,0,Bm.Width,Bm.Height);
   end;

function GetGraphicsFileName(WhatFor : shortstring; var fName : PathStr) : boolean;
begin
   if WhatFor = '' then WhatFor := 'Image';
   Result := Petmar.GetFileFromDirectory(WhatFor, AllowedGraphicsFilters,fName);
end;


function GetNewGraphicsFileName(WhatFor : shortstring; var fName : PathStr) : boolean;
begin
   Result := Petmar.GetFileNameDefaultExtSaveExt(WhatFor,GraphicsFilters,FName,MDdef.DefaultSaveImageType,false);
end;


function GetNew3DGraphicsFileName(WhatFor : shortstring; var fName : PathStr) : boolean;
var
   aType : integer;
begin
   aType := 1;
   Result := Petmar.GetFileNameDefaultExtSaveExt(WhatFor,ThreeDeeGraphicsFilters,FName,aType,false);
end;


procedure ConvertMyBitmapToJPEG(var FName : PathStr);
var
   MyBMP  : tMyBitmap;
begin
   MyBMP := LoadBitmapFromFile(fName);
   SaveBitmap(MyBMP,ChangeFileExt(fName,'.jpg'));
   MyBMP.Free;
   DeleteFileIfExists(FName);
end;


procedure SaveImageAsThumbnail(Image1 : tImage; FName : PathStr; ThumbNailHeight : integer);
var
   MyBMP,NewBMP : tMyBitmap;
begin
   CopyImageToBitmap(Image1,MyBmp);
   CreateBitmap(NewBMP,round(MyBMP.Width * ThumbNailHeight / MyBMP.Height),ThumbNailHeight);
   NewBMP.Canvas.StretchDraw(Rect(0,0,NewBMP.Width,NewBMP.Height),MyBMP);
   SaveBitmap(NewBMP,fName);
   NewBMP.Free;
   MyBMP.Free;
end;


procedure CloneImageToBitmap(Image1 : tImage; var CloneBitmap : tMyBitmap; MakeItBlack : boolean = false);
begin
   CreateBitmap(CloneBitmap,Image1.Width,Image1.Height);
   if MakeItBlack then begin
      CloneBitmap.Canvas.Pen.Color := clBlack;
      CloneBitmap.Canvas.Brush.Color := clBlack;
      CloneBitmap.Canvas.Brush.Style := bsSolid;
      CloneBitmap.Canvas.Rectangle(0,0,pred(Image1.Width),pred(Image1.Height));
   end;
end;


function CopyImageToBitmap(var Image1 : tImage; var Bitmap : tMyBitmap; ClearWhite : boolean = false) : boolean;
var
  MenuStr : shortstring;
begin
   Result := false;
   if (Image1 <> Nil) and (Image1.Picture.Graphic <> Nil) then begin
      try
         {$IfDef RecordBitmapProblems} WriteLineToDebugFile('CopyImageToBitmap, Image dimensions:' + ImageHeight(Image1) + '  Picture dimensions:' + IntToStr(Image1.Picture.Graphic.Width) + 'x' + IntToStr(Image1.Picture.Graphic.Height));         {$EndIf}
         PetImage.CreateBitmap(Bitmap,Image1.Picture.Graphic.Width,Image1.Picture.Graphic.Height);
         Bitmap.Canvas.Draw(0,0,Image1.Picture.Graphic);
         if ClearWhite then begin
            GetImagePartOfBitmap(Bitmap);
         end;
         Result := true;
      except
         MenuStr := 'Bitmap too large to copy (' + ImageSize(Image1) + ')';
         MessageToContinue(MenuStr);
         {$IfDef RecordBitmapProblems} WriteLineToDebugFile(MenuStr); {$EndIf}
         Bitmap.Free;
         Bitmap := nil;
      end;
   end;
end;


procedure AssignImageToClipBoard(Image1 : tImage; Margins : byte = 0);
var
   bmp,bigbmp : tMyBitmap;
begin
   if Margins = 0  then Clipboard.Assign(Image1.Picture)
   else begin
      CopyImageToBitmap(Image1,bmp);
      CreateBitmap(bigbmp,bmp.width + 2 *margins, bmp.height + 2 * margins);
      BigBmp.Canvas.Draw(Margins,Margins,bmp);
      Clipboard.Assign(Bigbmp);
      bigbmp.Free;
      bmp.Free;
   end;
end;


procedure SaveImageAsBMP(Image1 : tImage; SaveName : PathStr = ''); overload;
var
   MyBMP : tMyBitmap;
begin
   if CopyImageToBitmap(Image1,MyBMP) then begin
      SaveBitmap(MyBMP,SaveName);
      MyBMP.Free;
   end
end;


procedure LoadBitmapInImage(Image1 : tImage; fName : PathStr);
var
   BMP : tMyBitmap;
begin
   BMP := LoadBitmapFromFile(fName);
   Image1.Picture.Graphic := BMP;
   BMP.free;
end;


procedure PlotDipSymbol(Bitmap : tMyBitmap; x,y,VectorSize,Dip,Strike,DipDirect : integer;
    ThisIs : tStructureType; LabelValues : boolean; PenColor : tPlatformColor; PenWidth : integer );
var
   xt,yt,xt2,yt2 : integer;
   TStr  : ShortString;
begin
   {$IfDef RecordDipStrike} WriteLineToDebugFile('PlotDipSymbol at x=' + IntToStr(x) + '  y=' + IntToStr(y)); {$EndIf}

   with Bitmap.Canvas do  begin
      Brush.Style := bsClear;
      Pen.Width := PenWidth;
      Pen.Color := ConvertPlatformColorToTColor(PenColor);
      if (Dip = 0) then begin
         if (ThisIs = aBedding) then begin
            Ellipse(x-VectorSize,y-VectorSize,X+VectorSize,Y+VectorSize);
            DrawLine(Bitmap,x,y-VectorSize,x,y+VectorSize);
            DrawLine(Bitmap,x-VectorSize,y,x+VectorSize,y);
         end;
      end
      else begin
         xt := round(x + VectorSize * SinDeg(Strike));
         yt := round(y - VectorSize * CosDeg(Strike));
         MoveTo(xt,yt);

         xt := round(x - VectorSize * SinDeg(Strike));
         yt := round(y + VectorSize * CosDeg(Strike));
         LineTo(xt,yt);
         if (ThisIs = aBedding) then begin
            xt := round(x + VectorSize div 3 * SinDeg(DipDirect));
            yt := round(y - VectorSize div 3 * CosDeg(DipDirect));
            {$IfDef RecordDipStrike}
            WriteLineToDebugFile('End dip symbol at x=' + IntToStr(xt) + '  y=' + IntToStr(yt));
            {$EndIf}
            DrawLine(Bitmap,x,y,xt,yt);
            if (Dip = 90) then begin
               xt := round(x + VectorSize div 3 * SinDeg(DipDirect+180));
               yt := round(y - VectorSize div 3 * CosDeg(DipDirect+180));
               DrawLine(Bitmap,x,y,xt,yt);
               exit;
            end;
         end;
         if (ThisIs = aFault) then begin
            xt := round(x + VectorSize*3 div 2 * SinDeg(DipDirect));
            yt := round(y - VectorSize*3 div 2 * CosDeg(DipDirect));
            DrawLine(Bitmap,x,y,xt,yt);
            x := round(xt - VectorSize div 2* CosDeg(DipDirect-125));
            y := round(yt - VectorSize div 2* SinDeg(DipDirect-125));
            DrawLine(Bitmap,x,y,xt,yt);
            x := round(xt + VectorSize div 2* CosDeg(DipDirect+125));
            y := round(yt + VectorSize div 2* SinDeg(DipDirect+125));
            DrawLine(Bitmap,x,y,xt,yt);
         end;
         if (ThisIs = aJoint) then begin
            xt := round(x + VectorSize div 4 * SinDeg(Strike));
            yt := round(y - VectorSize div 4 * CosDeg(Strike));
            MoveTo(xt,yt);
            xt := round(xt + VectorSize div 4 * SinDeg(DipDirect));
            yt := round(yt - VectorSize div 4 * CosDeg(DipDirect));
            LineTo(xt,yt);
            xt2 := round(x - VectorSize  div 4 * SinDeg(Strike));
            yt2 := round(y + VectorSize  div 4 * CosDeg(Strike));
            MoveTo(xt2,yt2);
            xt2 := round(xt2 + VectorSize div 4 * SinDeg(DipDirect));
            yt2 := round(yt2 - VectorSize div 4 * CosDeg(DipDirect));
            LineTo(xt2,yt2);
            LineTo(xt,yt);
            if (Dip = 90) then begin
               xt := round(x + VectorSize div 4 * SinDeg(Strike));
               yt := round(y - VectorSize div 4 * CosDeg(Strike));
               MoveTo(xt,yt);
               xt := round(xt + VectorSize div 4 * SinDeg(DipDirect+180));
               yt := round(yt - VectorSize div 4 * CosDeg(DipDirect+180));
               LineTo(xt,yt);
               xt2 := round(x - VectorSize  div 4 * SinDeg(Strike));
               yt2 := round(y + VectorSize  div 4 * CosDeg(Strike));
               MoveTo(xt2,yt2);
               xt2 := round(xt2 + VectorSize div 4 * SinDeg(DipDirect+180));
               yt2 := round(yt2 - VectorSize div 4 * CosDeg(DipDirect+180));
               LineTo(xt2,yt2);
               LineTo(xt,yt);
               exit;
            end;
         end;
         if (ThisIs = aFoliation) then begin
            xt := round(x + VectorSize div 3 * SinDeg(Strike));
            yt := round(y - VectorSize div 3 * CosDeg(Strike));
            MoveTo(xt,yt);
            xt := round(x + VectorSize div 3 * SinDeg(DipDirect));
            yt := round(y - VectorSize div 3 * CosDeg(DipDirect));
            LineTo(xt,yt);
            xt2 := round(x - VectorSize div 3 * SinDeg(Strike));
            yt2 := round(y + VectorSize div 3 * CosDeg(Strike));
            LineTo(xt2,yt2);
            if Dip = 90 then begin
               xt := round(x + VectorSize div 3 * SinDeg(Strike));
               yt := round(y - VectorSize div 3 * CosDeg(Strike));
               MoveTo(xt,yt);
               xt := round(x + VectorSize div 3 * SinDeg(DipDirect+180));
               yt := round(y - VectorSize div 3 * CosDeg(DipDirect+180));
               LineTo(xt,yt);
               xt2 := round(x - VectorSize div 3 * SinDeg(Strike));
               yt2 := round(y + VectorSize div 3 * CosDeg(Strike));
               LineTo(xt2,yt2);
               exit;
            end;
         end;

         TStr := IntToStr(Dip);
         Font.Color := ConvertPlatformColorToTColor(PenColor);
         Font.Style := [fsBold];
         case VectorSize of
            8..12 : Font.Size := 8;
            13..15 : Font.Size := 10;
            16..20 : Font.Size := 12;
            else Font.Size := 15;
         end;

         if LabelValues then begin
            {$IfDef RecordDipStrike} WriteLineToDebugFile('DipDirect=' + IntToStr(DipDirect)); {$EndIf}
            if DipDirect in [0..200] then xt := xt+3
            else xt := xt-3-TextWidth(TStr);

            if (DipDirect in [0..90]) or ( (DipDirect >=270) and (DipDirect <=360) ) then begin
               yt := yt-TextHeight(TStr);
            end
            else if (DipDirect in [91..160]) or ( (DipDirect >=201) and (DipDirect <=269) ) then begin
               yt := yt+2;
            end
            else begin
               yt := yt + 2;
            end;

            TextOut(xt,yt,TStr);
            {$IfDef RecordDipStrike} WriteLineToDebugFile('Label at x=' + IntToStr(xt) + '  y=' + IntToStr(yt)); {$EndIf}
         end;
      end;
   end;
end;


procedure FillScanlineAddresses(Bitmap : tMyBitmap; var P3 : tScreenPRGB);
var
   j : integer;
begin
   for j := 0 to pred(Bitmap.Height) do P3[j] := Bitmap.ScanLine[j];
end;


procedure DilateTheImage(var Bitmap1 : tMyBitmap; ImageColor : tColor);
var
   x,y : integer;
   p1 :  tScreenPRGB;
   p2 : prgb;
   Bitmap2 : tMyBitmap;
   ImageRGBtriple : tRGBtriple;
begin
   ImageRGBtriple := ConvertTColorToPlatformColor(ImageColor);
   CreateBitmap(Bitmap2,Bitmap1.Width,Bitmap1.Height);
   Bitmap2.Canvas.Draw(0,0,Bitmap1);
   FillScanlineAddresses(Bitmap1,P1);

   for y := 1 to pred(pred(Bitmap1.Height)) do begin
      P2 := BitMap2.ScanLine[y];
      for x := 1 to pred(pred(Bitmap1.Width)) do begin
         if SameColor(p2[x],ImageRGBtriple) then begin
            p1[pred(y)][pred(x)] := ImageRGBtriple;
            p1[pred(y)][x] := ImageRGBtriple;
            p1[pred(y)][succ(x)] := ImageRGBtriple;
            p1[y][pred(x)] := ImageRGBtriple;
            p1[y][succ(x)] := ImageRGBtriple;
            p1[succ(y)][pred(x)] := ImageRGBtriple;
            p1[succ(y)][x] := ImageRGBtriple;
            p1[succ(y)][succ(x)] := ImageRGBtriple;
         end;
      end;
   end;
   FreeAndNil(Bitmap2);
end;


procedure GrayscaleNegative(bmp : tMyBitmap);
var
   x,y : integer;
   p0 : pRGB;
begin
   for y := 0 to pred(BMP.Height) do begin
      p0 := BMP.ScanLine[y];
      for x := 0 to pred(BMP.Width) do begin
         p0[x].rgbtBlue := 255-p0[x].rgbtBlue;
         p0[x].rgbtRed := 255-p0[x].rgbtRed;
         p0[x].rgbtGreen := 255-p0[x].rgbtGreen;
      end;
   end;
end;

procedure ThreshholdGrayscale(bmp : tMyBitmap; DownToZero,Upto255 : integer);
var
   x,y : integer;
   p0 : pRGB;
begin
   for y := 0 to pred(BMP.Height) do begin
      p0 := BMP.ScanLine[y];
      for x := 0 to pred(BMP.Width) do begin
         if p0[x].rgbtBlue <= DownToZero then begin
            p0[x].rgbtBlue := 0;
            p0[x].rgbtRed := 0;
            p0[x].rgbtGreen := 0;
         end
         else if p0[x].rgbtBlue >= UpTo255 then begin
            p0[x].rgbtBlue := 255;
            p0[x].rgbtRed := 255;
            p0[x].rgbtGreen := 255;
         end;
      end;
   end;
end;


procedure MakeGraphicsFileNegative(fName : PathStr);
var
   bmp : tMyBitmap;
begin
   bmp := tMyBitmap.Create;
   BMP.LoadFromFile(fname);
   GrayscaleNegative(bmp);
   PetImage.SaveBitmap(bmp,fName);
   bmp.free;
end;


procedure MakeBitmapNegative(var BMP : tMyBitmap; RGBTriple : tRGBTriple);
var
   x,y : integer;
   p0 : pRGB;
begin
   for y := 0 to pred(BMP.Height) do  begin
      p0 := BMP.ScanLine[y];
      for x := 0 to pred(BMP.Width) do  begin
         if SameColor(p0[x],RGBTripleWhite) then p0[x] := RGBTriple
         else p0[x] := RGBTripleWhite;
      end;
   end;
end;

procedure ChangeBitmapBlackWhite(var BMP : tMyBitmap; BlackReplace,WhiteReplace : tRGBTriple);
var
   x,y : integer;
   p0 : pRGB;
begin
   for y := 0 to pred(BMP.Height) do begin
      p0 := BMP.ScanLine[y];
      for x := 0 to pred(BMP.Width) do begin
         if SameColor(p0[x],RGBTripleWhite) then p0[x] := WhiteReplace
         else p0[x] := BlackReplace;
      end;
   end;
end;


procedure AssignBitmapToClipBoard(bm1 : tMyBitmap);
begin
   Clipboard.Assign(bm1);
end;


procedure GetImagePartOfBitmap(var Bitmap : tMyBitmap; WhiteBoundary : boolean = true; WhiteBoundarySize : integer = 2);
var
   NewBitmap : tMyBitmap;
   Left,Right,Top,Bottom : integer;
begin
   {$IfDef RecordGetImagePartOfBitmap} WriteLineToDebugFile('GetImagePartOfBitmap in'); {$EndIf}
   FindImagePartOfBitmap(Bitmap,Left,Right,Top,Bottom);
   if (Right > Left) and (Bottom > Top) then begin
      if (Left > 0) or (Right < Bitmap.Width) or (Top < Bitmap.Height) or (Bottom > 0) then begin
         CreateBitmap(NewBitmap,Bitmap.Width,Bitmap.Height);
         NewBitmap.Canvas.Draw(0,0,Bitmap);
         Bitmap.Free;
         if not WhiteBoundary then WhiteBoundarySize := 0;
         CreateBitmap(Bitmap,(Right - Left)+succ(WhiteBoundarySize * 2), (Bottom - Top)+succ(WhiteBoundarySize * 2));
         ClearBitmap(Bitmap,clNearWhite);
         Bitmap.Canvas.CopyRect(Rect(WhiteBoundarySize,WhiteBoundarySize,Bitmap.Width-WhiteBoundarySize,Bitmap.Height-WhiteBoundarySize),NewBitmap.Canvas,Rect(Left,Top,Right,Bottom));
         NewBitmap.Free;
      end;
   end;
   {$IfDef RecordGetImagePartOfBitmap} WriteLineToDebugFile('Final' + BitmapSizeString(Bitmap)); {$EndIf}
end;

{start of image rotation}
      //http://www.efg2.com/Lab/ImageProcessing/Unit7.TXT

      procedure SpiegelnHorizontal(Bitmap:tMyBitmap);
      var
        i,j,w :  INTEGER;
         RowIn :  pRGBArray;
         RowOut:  pRGBArray;
      begin
          w := bitmap.width*sizeof(TRGBTriple);
          Getmem(rowin,w);
          for j := 0 to Bitmap.Height-1 do begin
            move(Bitmap.Scanline[j]^,rowin^,w);
            rowout := Bitmap.Scanline[j];
            for i := 0 to Bitmap.Width-1 do rowout[i] := rowin[Bitmap.Width-1-i];
          end;
          bitmap.Assign(bitmap);
          Freemem(rowin);
      end;


      procedure SpiegelnVertikal(Bitmap : tMyBitmap);
      var
         j,w  : INTEGER;
         help : tMyBitmap;
      begin
          help := tMyBitmap.Create;
          help.Width       := Bitmap.Width;
          help.Height      := Bitmap.Height;
          help.PixelFormat := Bitmap.PixelFormat;
          w := Bitmap.Width*sizeof(TRGBTriple);
          for j := 0 to Bitmap.Height-1 do move(Bitmap.Scanline[j]^,Help.Scanline[Bitmap.Height - 1 - j]^,w);
          Bitmap.Assign(help);
          help.free;
      end;

      type
         THelpRGB = packed record
                      rgb    : TRGBTriple;
                      dummy  : byte;
                   end;

      procedure Drehen270Grad(Bitmap:tMyBitmap);
      var
          aStream : TMemorystream;
          header  : tBitmapINFO;
          dc      : hDC;
          P       : ^THelpRGB;
          x,y,b,h : Integer;
          RowOut:  pRGBArray;
      BEGIN
         aStream := TMemoryStream.Create;
         aStream.SetSize(Bitmap.Height*Bitmap.Width * 4);
         with header.bmiHeader do begin
           biSize := SizeOf(tBitmapINFOHEADER);
           biWidth := Bitmap.Width;
           biHeight := Bitmap.Height;
           biPlanes := 1;
           biBitCount := 32;
           biCompression := 0;
           biSizeimage := aStream.Size;
           biXPelsPerMeter := 1;
           biYPelsPerMeter := 1;
           biClrUsed := 0;
           biClrImportant :=0;
         end;
         dc := GetDC(0);
         P  := aStream.Memory;
         GetDIBits(dc,Bitmap.Handle,0,Bitmap.Height,P,header,dib_RGB_Colors);
         ReleaseDC(0,dc);
         b := bitmap.Height;  // rotate
         h := bitmap.Width;   // rotate
         bitmap.Width := b;
         bitmap.height := h;
         for y := 0 to (h-1) do begin
           rowOut := Bitmap.ScanLine[(h-1)-y];
           P  := aStream.Memory;        // reset pointer
           inc(p,y);
           for x := (b-1) downto 0 do begin
              rowout[x] := p^.rgb;
              inc(p,h);
           end;
         end;
         aStream.Free;
      end;


      procedure Drehen90Grad(Bitmap:tMyBitmap);
      var aStream : TMemorystream;
          header  : tBitmapINFO;
          dc      : hDC;
          P       : ^THelpRGB;
          x,y,b,h : Integer;
          RowOut:  pRGBArray;
      begin
         aStream := TMemoryStream.Create;
         aStream.SetSize(Bitmap.Height*Bitmap.Width * 4);
         with header.bmiHeader do begin
           biSize := SizeOf(tBitmapINFOHEADER);
           biWidth := Bitmap.Width;
           biHeight := Bitmap.Height;
           biPlanes := 1;
           biBitCount := 32;
           biCompression := 0;
           biSizeimage := aStream.Size;
           biXPelsPerMeter := 1;
           biYPelsPerMeter := 1;
           biClrUsed := 0;
           biClrImportant := 0;
         end;
         dc := GetDC(0);
         P  := aStream.Memory;
         GetDIBits(dc,Bitmap.Handle,0,Bitmap.Height,P,header,dib_RGB_Colors);
         ReleaseDC(0,dc);
         b := bitmap.Height;  // rotate
         h := bitmap.Width;   // rotate
         bitmap.Width := b;
         bitmap.height := h;
         for y := 0 to (h-1) do  begin
           rowOut := Bitmap.ScanLine[y];
           P  := aStream.Memory;        // reset pointer
           inc(p,y);
           for x := 0 to (b-1) do  begin
              rowout[x] := p^.rgb;
              inc(p,h);
           end;
         end;
         aStream.Free;
      end;


      procedure Drehen180Grad(Bitmap:tMyBitmap);
      var i,j     :  INTEGER;
          rowIn :  pRGBArray;
          rowOut:  pRGBArray;
          help  : tMyBitmap;
      begin
         help := tMyBitmap.Create;
         help.Width  := Bitmap.Width;
         help.Height := Bitmap.Height;
         help.PixelFormat := Bitmap.PixelFormat;    // only pf24bit for now
         FOR  j := 0 TO Bitmap.Height - 1 DO BEGIN
           rowIn  := Bitmap.ScanLine[j];
           rowOut := help.ScanLine[Bitmap.Height - j - 1];
           FOR i := 0 TO Bitmap.Width - 1 DO rowOut[Bitmap.Width - i - 1] := rowIn[i]
         END;
         bitmap.assign(help);
         help.free;
      end;


{From: "Jack Sudarev" <jack.sudarev@practel.com.au> To: <EarlGlynn@att.net>  Subject: Rotate bitmap  Date: Thursday, June 22, 2000 10:23 PM  "Rotate bitmap using scanline" }


function Ceil(x : float64) : integer;
begin
  Result := succ(Trunc(x));
end;


//Bilinear interpolation
function GetSmoothColor(iOriginal, jOriginal : Double; OriginalBitmap : tMyBitmap): TRGBTriple;
var
  f0, f1, f2, f3, iFrac, jFrac : Double;
  P0, P1, P2, P3: TRGBTriple;
  P : pRGBArray;
begin
    //Get fractional parts
    iFrac := Frac(iOriginal);
    jFrac := Frac(jOriginal);
    f0 := (1 - iFrac)*(1 - jFrac);
    f1 := iFrac*(1 - jFrac);
    f2 := iFrac*jFrac;
    f3 := (1 - iFrac)*jFrac;
    //Get surrounding points
    P := OriginalBitmap.ScanLine[Trunc(jOriginal)];
    P0 := P[Trunc(iOriginal)];
    P1 := P[Ceil(iOriginal)];
    P := OriginalBitmap.ScanLine[Ceil(jOriginal)];
    P2 := P[Trunc(iOriginal)];
    P3 := P[Ceil(iOriginal)];
    //Calculate result color
    Result.rgbtRed := Round(P0.rgbtRed*f0 + P1.rgbtRed*f1 + P2.rgbtRed*f2 + P3.rgbtRed*f3);
    Result.rgbtGreen := Round(P0.rgbtGreen*f0 + P1.rgbtGreen*f1 + P2.rgbtGreen*f2 + P3.rgbtGreen*f3);
    Result.rgbtBlue := Round(P0.rgbtBlue*f0 + P1.rgbtBlue*f1 + P2.rgbtBlue*f2 + P3.rgbtBlue*f3);
end;


//Rotate bitmap to any angle
function RotateBitmap(OriginalBitmap : tMyBitmap;  Angle : float64; AntiAliasing: Boolean; WhiteBackground : boolean = true) : tMyBitmap;
var
  i, j : Integer;
  iRotationAxis, jRotationAxis,
  iOriginal, jOriginal,
  iPrime, jPrime,Miss,
  iPrimeRotated, jPrimeRotated: Integer;
  Theta,cosTheta, sinTheta,
  iPrimeRotatedA, jPrimeRotatedA,
  iOriginalA, jOriginalA: Double;
  RowOriginal,RowRotated: pRGBArray;
begin
    if WhiteBackground then Miss := 255 else Miss := 0;
    if (OriginalBitmap.PixelFormat <> pf24bit) then OriginalBitmap.PixelFormat := pf24bit; // force to 24 bits
    try
      // The size of BitmapRotated is same as BitmapOriginal.  PixelFormat
      // must also match since 24-bit GBR triplets are assumed in ScanLine.
      CloneBitmap(OriginalBitmap,Result);
      Result.Canvas.Pen.Color := Miss;
      Result.Canvas.Brush.Color := Miss;
      Result.Canvas.Brush.Style := bsSolid;
      Result.Canvas.Rectangle(0,0,Result.Width,Result.Height);

      // Axis of rotation is normally center of image
      iRotationAxis := Result.Width div 2;
      jRotationAxis := Result.Height div 2;

      // Convert degrees to radians.  Use minus sign to force clockwise rotation.
      Theta := (-Angle) * Pi/180;
      sinTheta := Sin(Theta);
      cosTheta := Cos(Theta);

      // Step through each row of rotated image.
      for j := Result.Height - 1 downto 0 do begin
        RowRotated := Result.Scanline[j];
        {
          Assume the bitmap has an even number of pixels in both dimensions and
          the axis of rotation is to be the exact middle of the image -- so this
          axis of rotation is not at the middle of any pixel.

          The transformation (i,j) to (iPrime, jPrime) puts the center of each
          pixel at odd-numbered coordinates.  The left and right sides of each
          pixel (as well as the top and bottom) then have even-numbered coordinates.

          The point (iRotationAxis, jRotationAxis) identifies the axis of rotation.

          For a 640 x 480 pixel image, the center point is (320, 240).  Pixels
          numbered (index i) 0..319 are left of this point along the "X" axis and
          pixels numbered 320..639 are right of this point.  Likewise, vertically
          pixels are numbered (index j) 0..239 above the axis of rotation and
          240..479 below the axis of rotation.

          The subtraction (i, j) - (iRotationAxis, jRotationAxis) moves the axis of
          rotation from (i, j) to (iRotationAxis, jRotationAxis), which is the
          center of the bitmap in this implementation.
        }

        jPrime := 2*(j - jRotationAxis) + 1;

        for i := Result.Width - 1 downto 0 do begin

          iPrime := 2*(i - iRotationAxis) + 1;

          // Rotate (iPrime, jPrime) to location of desired pixel
          // Note:  There is negligible difference between floating point and
          // scaled integer arithmetic here, so keep the math simple (and readable).
          if not AntiAliasing then begin
            iPrimeRotated := Round(iPrime * CosTheta - jPrime * sinTheta);
            jPrimeRotated := Round(iPrime * sinTheta + jPrime * cosTheta);

            // Transform back to pixel coordinates of image, including translation
            // of origin from axis of rotation to origin of image.
            iOriginal := (iPrimeRotated - 1) div 2 + iRotationAxis;
            jOriginal := (jPrimeRotated - 1) div 2 + jRotationAxis;

            // Make sure (iOriginal, jOriginal) is in BitmapOriginal.  If not,
            // assign white color to corner points.
            if (iOriginal >= 0) and (iOriginal <= OriginalBitmap.Width - 1) and
               (jOriginal >= 0) and (jOriginal <= OriginalBitmap.Height - 1) then begin
              // Assign pixel from rotated space to current pixel in BitmapRotated
              RowOriginal := OriginalBitmap.Scanline[jOriginal];
              RowRotated[i] := RowOriginal[iOriginal];
            end
            else begin
              RowRotated[i].rgbtBlue := Miss; // assign "corner" color
              RowRotated[i].rgbtGreen := Miss;
              RowRotated[i].rgbtRed := Miss;
            end;
          end
          else begin //Antialiasing is On

            iPrimeRotatedA := iPrime * CosTheta - jPrime * sinTheta;
            jPrimeRotatedA := iPrime * sinTheta + jPrime * cosTheta;

            // Transform back to pixel coordinates of image, including translation
            // of origin from axis of rotation to origin of image.
            iOriginalA := (iPrimeRotatedA - 1)/2 + iRotationAxis;
            jOriginalA := (jPrimeRotatedA - 1)/2 + jRotationAxis;

            // Make sure (iOriginal, jOriginal) is in BitmapOriginal.  If not,
            // assign white color to corner points.
            if (iOriginalA >= 0) and (iOriginalA <= OriginalBitmap.Width - 1) and
               (jOriginalA >= 0) and (jOriginalA <= OriginalBitmap.Height - 1) then begin
              // Assign pixel from rotated space to current pixel in BitmapRotated
              RowRotated[i] := GetSmoothColor(iOriginalA, jOriginalA, OriginalBitmap);
            end
            else begin
              RowRotated[i].rgbtRed := Miss;
              RowRotated[i].rgbtGreen := Miss;
              RowRotated[i].rgbtBlue := Miss; // assign "corner" color
            end;
          end;
        end;//for i
      end;//for j
    finally
    end;
end;

procedure RecolorBitmap(var Bitmap : Pathstr; Color : tPlatFormColor);
var
   bmp : tMyBitmap;
begin
   bmp := tMyBitmap.Create;
   bmp.LoadFromFile(Bitmap);
   RecolorBitmap(bmp,Color);
   PetImage.SaveBitmap(bmp,Bitmap);
   bmp.free;
end;


procedure RecolorBitmap(var Bitmap : tMyBitmap; Color : tPlatformColor);
{any non-white pixel will be recolored to the specified color}
var
   x,y : integer;
   p0 : prgb;
begin
   for y := 0 to pred(Bitmap.Height) do begin
      p0 := Bitmap.Scanline[y];
      for x := 0 to pred(Bitmap.Width) do begin
         if (not SameColor(P0[x],RGBTripleWhite)) then p0[x] := Color;
      end;
   end;
end;


function BlendBitmaps(src1,src2 : tMyBitmap; amount : extended) : tMyBitmap;
begin
   CreateBitMap(Result,src1.width,src1.Height);
   Result.Canvas.Draw(0,0,src1);
   BlendBitmapAtop(Result,src2,amount);
end;


procedure BlendBitmapAtop(var src1 : tMyBitmap; Overlay : tMyBitmap; amount : extended);
var
   w,h,x,y :integer;
   MemBase,MemOverlay : tBmpMemory;
   r1,g1,b1,r2,g2,b2 : byte;
begin
   if (Overlay = Nil) or (src1 = Nil) then exit;
   {$IfDef RecordBlendBitmaps}
      WriteLineToDebugFile('BlendBitmaps,  amount=' + RealToString(Amount,-12,2) + '  Source: ' + BitmapSize(src1) + '  Overlay: ' + BitmapSize(Overlay));
      src1.SaveToFile(MDtempDir + 'blend_source.bmp');
      Overlay.SaveToFile(MDtempDir + 'blend_overlay.bmp');
   {$EndIf}

   w := src1.Width;
   h := src1.Height;

   //if overlay is smaller, only try to overlay in upper left portion of source
   if Overlay.Width < w then w := Overlay.Width;
   if Overlay.Height < h then h := Overlay.Height;

   MemBase    := tBmpMemory.Create(Src1);
   MemOverlay := tBmpMemory.Create(Overlay);

   for y := 0 to (h-1) do begin
       for x := 0 to (w-1) do begin
          if not MemOverlay.SameColor(x,y,rgbTripleWhite) then begin
             MemBase.GetPixelRGB(x,y,r1,g1,b1);
             MemOverlay.GetPixelRGB(x,y,r2,g2,b2);
             r1 := ValidByteRange(r1 + round(amount * (r2-r1)));
             g1 := ValidByteRange(g1 + round(amount * (g2-g1)));
             b1 := ValidByteRange(b1 + round(amount * (b2-b1)));
             MemBase.SetPixelRGB(x,y,r1,g1,b1);
          end;
      end;
   end;
   MemBase.Destroy;
   MemOverlay.Destroy;
   {$IfDef RecordBlendBitmaps} src1.SaveToFile(MDtempDir + 'blend_result.bmp'); {$EndIf}
end;


procedure MakeBitmapThumbnail(var inBMP : tMyBitmap; tnHeight : integer);
begin
   if tnHeight < inBMP.Height then begin
      inBMP.Canvas.StretchDraw(Rect(0,0,round(tnHeight / inbmp.Height *inbmp.Width),tnHeight),inBMP);
      inBMP.Width := round(tnHeight / inbmp.Height * inbmp.Width);
      inBMP.Height := tnHeight;
   end;
end;


procedure CreateThumbNail(fname,OutName : PathStr; tnHeight,tnQuality : integer; aLabel : ShortString = '');
var
   inBMP,outBMP : tMyBitmap;
begin
   inBMP := LoadBitmapFromFile(fName);
   OutBMP := CreateThumbNailBMP(inBMP,tnHeight);
   if (aLabel <> '') then begin
      OutBMP.Canvas.Font.Size := 14;
      OutBMP.Canvas.Font.Color := clRed;
      OutBMP.Canvas.TextOut(1,1,aLabel);
   end;
   SaveBitmap(OutBMP,OutName);
   OutBMP.Free;
   InBMP.Free;
end;


procedure SkeletonizeBitmap(var Bitmap : tMyBitmap);
//http://www.planet-source-code.com/vb/scripts/ShowCode.asp?txtCodeId=1217&lngWId=7
//code by Peter Bone, an English PhD student in image processing (in 8/20/2003)
type
  TRGBTripleArray = array[0..5000] of RGBTriple;
  PRGBTripleArray = ^TRGBTripleArray;
  TPoints = array of TPoint;
Var
  LScan : PRGBTripleArray;
  Lx, Ly : integer;
  LPoints : TPoints;

    // return offset of neighbouring point
    function Neighbour(ANeighbour : byte) : TPoint;
    begin
      ANeighbour := (ANeighbour - 2) mod 8 + 2; // neighbourhood wrap-round
      Case ANeighbour of
        2 : begin Result.X := 0; Result.Y := -1; end; //    ___________
        3 : begin Result.X := 1; Result.Y := -1; end; //   | 9 | 2 | 3 |
        4 : begin Result.X := 1; Result.Y := 0; end; //   |___|___|___|
        5 : begin Result.X := 1; Result.Y := 1; end; //   | 8 | 1 | 4 |
        6 : begin Result.X := 0; Result.Y := 1; end; //   |___|___|___|
        7 : begin Result.X := -1; Result.Y := 1; end; //   | 7 | 6 | 5 |
        8 : begin Result.X := -1; Result.Y := 0; end; //   |___|___|___|
        9 : begin Result.X := -1; Result.Y := -1; end; //
      end;
    end;


    // thin binary image to a single pixel width skeleton by erosion
    procedure Skeletonize(var Points : TPoints ; AWidth, AHeight : integer);
    Var
      Lx, Ly, Lindex,lCount : integer;
      LB, LA, LN, LAN6, LAN4 : byte;
      LNOff : TPoint;
      LChanging : boolean;
      LRemove : array of boolean;
      LImage : array of array of byte;
    begin
      // create a 2D image from the points - LImage array
      // the LImage array will be larger than the actual image
      // and offset by 1 in x and y to prevent access violations
      // when accessing neighbouring points to those at the edge of the image
      SetLength(LImage, AWidth+3);
      for Lx := 0 to AWidth + 2 do begin
        SetLength(LImage[Lx], AHeight+3);
        for Ly := 0 to AHeight + 2 do  begin
          LImage[Lx][Ly] := 0;
        end;
      end;
      for Lindex := 0 to High(Points) do LImage[Points[Lindex].X+1][Points[Lindex].Y+1] := 1;

      SetLength(LRemove, length(Points));

      LChanging := True;
      LCount := 0;
      while LChanging do begin // stop if pixels are no longer being removed
        inc(LCount);
        LChanging := False;
        StartProgress('Skeletonize, loop ' + IntToStr(LCount));
        // loop through all points in the binary object
        for Lindex := 0 to High(Points) do begin
          if Lindex mod 100 = 0 then UpdateProgressBar(Lindex/High(Points));

          LRemove[Lindex] := False;

          Lx := Points[Lindex].X + 1;
          Ly := Points[Lindex].Y + 1;

          // calculate B, the sum of non-zero neighbours
          LB := 0;
          for LN := 2 to 9 do begin
            LNOff := Neighbour(LN);
            Inc(LB, LImage[Lx+LNOff.X][Ly+LNOff.Y]);
          end;

          if (LB < 2) or (LB > 6) then Continue; // failed B removal test

          // calculate A, the number of 0 -> 1 patterns around the neighbourhood
          LA := 0;
          for LN := 2 to 9 do begin
            LNOff := Neighbour(LN);
            if LImage[Lx+LNOff.X][Ly+LNOff.Y] = 0 then begin
              LNOff := Neighbour(LN+1);
              if LImage[Lx+LNOff.X][Ly+LNOff.Y] = 1 then Inc(LA);
            end;
            if LA > 1 then Break; // fails if LA <> 1 so no need to continue
          end;

          if LA <> 1 then Continue; // failed A removal test

          // calculate the A value for neighbour 6
          LAN6 := 0;
          for LN := 2 to 9 do begin
            LNOff := Neighbour(LN);
            if LImage[Lx+LNOff.X][Ly+LNOff.Y+1] = 0 then begin
              LNOff := Neighbour(LN+1);
              if LImage[Lx+LNOff.X][Ly+LNOff.Y+1] = 1 then Inc(LAN6);
            end;
          end;

          if (LImage[Lx][Ly+1] * LImage[Lx+1][Ly] * LImage[Lx-1][Ly] <> 0) and (LAN6 = 1) then Continue; // failed test

          // calculate the A value for neighbour 4
          LAN4 := 0;
          for LN := 2 to 9 do begin
            LNOff := Neighbour(LN);
            if LImage[Lx+LNOff.X+1][Ly+LNOff.Y] = 0 then begin
              LNOff := Neighbour(LN+1);
              if LImage[Lx+LNOff.X+1][Ly+LNOff.Y] = 1 then Inc(LAN4);
            end;
          end;

          // final removal test
          if (LImage[Lx][Ly-1] * LImage[Lx+1][Ly] * LImage[Lx][Ly+1] = 0) or (LAN4 <> 1) then begin
            // erosion cannot be done sequencially - so flag all points that
            // must be removed to remove them all at once after each pass
            LRemove[Lindex] := True;
            LChanging := True; // still removing pixels so continue for another pass
          end;
        end; // loop through points

        // remove the points that are flagged in the LRemove array
        if LChanging then begin
          Lindex := 0;
          while Lindex < length(Points) do begin
            if LRemove[Lindex] then begin
              // set value of image for this point to zero
              LImage[Points[Lindex].X+1][Points[Lindex].Y+1] := 0;
              // remove point from points list by moving the last element to the
              // points position and then reducing the length of the array by 1
              Points[Lindex] := Points[High(Points)];
              SetLength(Points, High(Points));
              // must do the same for this element in the LRemove array
              LRemove[Lindex] := LRemove[High(LRemove)];
              SetLength(LRemove, High(LRemove));
            end else Inc(Lindex);
          end;
        end;
      end; // passes loop
      EndProgress;
    end;


begin
  // create a list of points from the image
  SetLength(LPoints, 0);
  for Ly := 0 to Bitmap.Height - 1 do  begin
    LScan := Bitmap.ScanLine[Ly];
    for Lx := 0 to Bitmap.Width - 1 do  begin
      if LScan[Lx].rgbtBlue = 0 then begin
        SetLength(LPoints, length(LPoints) + 1);
        LPoints[High(LPoints)] := Point(Lx, Ly);
      end;
    end;
  end;

  // skeletonize list of points
  Skeletonize(LPoints, Bitmap.Width, Bitmap.Height);

  // draw skeleton back on image from skeletonized points list
  RecolorBitmap(Bitmap,claWhite);
  for Ly := 0 to High(LPoints) do  begin
    Bitmap.Canvas.Pixels[LPoints[Ly].X, LPoints[Ly].Y] := clblack;
  end;
end;


procedure BitmapNonWhiteToBlack(var Bitmap : tMyBitmap);
var
   x,y : integer;
   p1 : PRGB;
begin
   for y := 0 to pred(Bitmap.Height) do begin
      P1 := BitMap.ScanLine[y];
      for x := 0 to pred(Bitmap.Width) do begin
         if not SameColor(p1[x],RGBTripleWhite) then p1[x] := RGBTripleBlack;
      end;
   end;
end;


procedure MakeBitmapGreen(var BMP : tMyBitmap);
var
   x,y : integer;
   P0  : pRGB;
begin
   for y := 0 to pred(BMP.Height) do begin
      p0 := BMP.Scanline[y];
      for x := 0 to pred(BMP.Width) do begin
         P0[x].rgbtRed := 0;
         P0[x].rgbtBlue := 0;
      end;
   end;
end;


procedure MakeBitmapStretchedGrayscale(Bitmap : tMyBitmap);
const
   MinGray : byte = 75;
   MaxGray : byte = 200;
var
   x,y,j : integer;
   P0    : pRGB;
begin
   StartProgress('Recolor');
   ReadDefault('Min gray',MinGray);
   ReadDefault('Max gray',MaxGray);
   for y := 0 to pred(Bitmap.Height) do  begin
      p0 := Bitmap.Scanline[y];
      if y mod 25 = 0 then UpdateProgressBar(y/Bitmap.Height);
      for x := 0 to pred(Bitmap.Width) do  begin
         j := round(0.3 * P0[x].rgbtRed  + 0.59 * P0[x].rgbtGreen  + 0.11 * P0[x].rgbtBlue );
         j := ValidByteRange(MinGray + (MaxGray - MinGray) * j div 255);
         P0[x].rgbtRed := j;
         P0[x].rgbtGreen := j;
         P0[x].rgbtBlue := j;
      end;
   end;
   EndProgress;
end;


function CreateProtractor(Double,ShallowAngles : boolean; FormVertExag : float64) : tMyBitmap;
var
   i,StartX,StartY,LastEndY : integer;

   procedure DrawLine(InAngle : integer);
   var
      EndX,EndY,End2 : integer;
      Angle : float64;
      TStr : ShortString;
   begin
      Angle := ArcTan( SinDeg(InAngle) * FormVertExag / CosDeg(InAngle)) / DegToRad;
      EndX := StartX + round(100 * CosDeg(Angle));
      End2 := StartX - round(100 * CosDeg(Angle));
      EndY := StartY + round(100 * SinDeg(Angle));
      if (EndY > LastEndY + 15) then begin
         Result.Canvas.MoveTo(StartX,StartY);
         Result.Canvas.LineTo(EndX,EndY);
         TStr := IntToStr(InAngle) + '°';
         Result.Canvas.TextOut(EndX+5,EndY,TStr);
         if Double then begin
            Result.Canvas.MoveTo(StartX,StartY);
            Result.Canvas.LineTo(End2,EndY);
            Result.Canvas.TextOut(End2-5-Result.Canvas.TextWidth(TStr),EndY,TStr);
         end;
         LastEndY := EndY;
      end;
   end;

begin
   if Double then  begin
      i := 2;
      StartX := 150;
    end
    else begin
       i := 1;
       Startx := 0;
    end;

   CreateBitmap(Result,i * 150,120);
   StartY := 0;
   LastEndY := -99;
   Result.Canvas.Pen.Color := clBlack;
   Result.Canvas.Pen.Width := 1;
   Result.Canvas.Font.Name := MDDef.DefaultGraphFont.Name;
   Result.Canvas.Font.Size := MDDef.DefaultGraphFont.Size;
   Result.Canvas.Font.Style := [fsBold];
   DrawLine(0);
   if ShallowAngles then begin
      DrawLine(1);
      DrawLine(2);
   end;
   DrawLine(5);
   DrawLine(10);
   DrawLine(20);
   DrawLine(30);
   if (not ShallowAngles) then begin
      DrawLine(40);
      DrawLine(50);
      DrawLine(60);
   end;
end;


