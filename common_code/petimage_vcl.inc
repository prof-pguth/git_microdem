{$IfDef VCL}

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
   Result := 'BMP|*.bmp|JPEG (quick)|*.jpg|JPEG (deliberate)|*.jpg'
         {$IfDef ExGIF} {$Else} + '|GIF|*.gif' {$EndIf}
         {$IfDef ExPNG} {$Else} + '|PNG|*.png' {$EndIf};
end;

function ThreeDeeGraphicsFilters : shortstring;
begin
   Result := 'Stereo PNG|*.PNS|Stereo JPEG|*.JPS';
end;


function AllowedGraphicsFilters : shortstring;
begin
   Result := 'Graphics files|*.bmp;*.jpg;*.jpeg;*.jpe;*.tif;*.tiff'
         {$IfDef ExGIF} {$Else} + ';*.gif' {$EndIf}
         {$IfDef ExPNG} {$Else} + ';*.png' {$EndIf};
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
         if NameContains = '' then NameContains := 'DSC_';
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
               if NameContains[length(NameContains)] <> '_' then TStr := '_' else TStr := '';
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
         RegVar : tRegVars;
         MapProj  : tMapProjection;
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
            MapProj := Nil;
            TiffImage := tTIFFImage.CreateGeotiff(false,MapProj,RegVar,true,FName,Success);
            Result := TiffImage.DisplayInBitmap;
            TiffImage.Destroy;
            FreeAndNil(MapProj);
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
{$EndIf}
