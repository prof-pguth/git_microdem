
{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2019 Peter L. Guth   }
{                                    }
{ include file for demmapdraw        }
{____________________________________}



{$IfDef ExWMS}
{$Else}

   procedure WebMercatorLatLongToXY(Lat,Long : float64; var x,y : float64);
   begin
       //https://epsg.io/transform#s_srs=4326&t_srs=3857&x=-76.5000000&y=39.0000000
       x := long * 20037508.34 / 180;
       y := SphericalEarthAkm * 1000 * ln(Math.Tan(QuarterPi + 0.5 * Lat*DegToRad));
   end;


      function TMapDraw.CreateWMSRequest : ANSIstring;
      var
         Table : tMyData;
         x1,y1,x2,y2 : float64;
         CRSstring : shortstring;

         function BBoxString(x1,y1,x2,y2 : float64) : shortstring;
         begin
            if FormatSettings.DecimalSeparator = ',' then Result := '&BBOX=' +
                AnsiReplaceStr(FloatToStr(x1), ',', '.') + ',' +
                AnsiReplaceStr(FloatToStr(y1), ',', '.') + ',' +
                AnsiReplaceStr(FloatToStr(x2), ',', '.') + ',' +
                AnsiReplaceStr(FloatToStr(y2), ',', '.')
            else Result := Result + '&BBOX=' + FloatToStr(x1) + ',' + FloatToStr(y1) + ',' + FloatToStr(x2) + ',' + FloatToStr(y2);
         end;

      begin
         {$IfDef RecordWMS}        WriteLineToDebugFile('TMapDraw.CreateWMSRequest in');         {$EndIf}

         {$IfDef VCL}
         CheckFile(ExtractFileName(WMS_servers_fName));
         Table := tMyData.Create(WMS_servers_fName);
         if (FavoriteWMS <> '') then LastWMSService := FavoriteWMS
         else if (LastWMSService = '') then begin
            Table.ApplyFilter('USE=' + QuotedStr('Y'));
            LastWMSService := GetStringFromFieldInDataBase('WMS to display on map',Table,'SERVICE');
            FavoriteWMS := LastWMSService;
            //LastWMSLayer := LastWMSService;
         end;
         Table.ApplyFilter('SERVICE=' + QuotedStr(LastWMSService));
         CRSstring := Table.GetFieldByNameAsString('CRS');
         Result := Table.GetFieldByNameAsString('URL') + '?VERSION=' + Table.GetFieldByNameAsString('VERSION') + '&REQUEST=GetMap' + '&LAYERS=' + Table.GetFieldByNameAsString('LAYER')+ '&CRS=';
         Table.Destroy;
         {$EndIf}

         //Result := 'https://gis.apfo.usda.gov/arcgis/services/NAIP/Maryland/ImageServer/WMSServer?VERSION=1.3.0&REQUEST=GetMap&LAYERS=0&CRS=EPSG:3857';
                    //https://gis.apfo.usda.gov/arcgis/services/NAIP/Maryland/ImageServer?VERSION=1.3.0&REQUEST=GetMap&LAYERS=0&CRS=EPSG:3857&BBOX=-8518836.221259,4716067.30842037,-8510593.03918499,4724010.01586778&FORMAT=image/png&STYLES=&Width=1575&Height=1515
        if KMLcompatibleMap then begin
           {$IfDef RecordWMS}         WriteLineToDebugFile('KMLcompatibleMap, request CRS:84');        {$EndIf}
           if CRSString = 'EPSG:3857' then begin
              WebMercatorLatLongToXY(MapCorners.BoundBoxGeo.yMin,MapCorners.BoundBoxGeo.Xmin,x1,y1);
              WebMercatorLatLongToXY(MapCorners.BoundBoxGeo.YMax,MapCorners.BoundBoxGeo.Xmax,x2,y2);
              Result := Result + CRSString + BBoxString(x1,y1,x2,y2);
           end
           else begin
           //Result := Result + 'EPSG:4326';
              Result := Result + 'CRS:84' + BBoxString(MapCorners.BoundBoxGeo.XMin,MapCorners.BoundBoxGeo.Ymin,MapCorners.BoundBoxGeo.XMax,MapCorners.BoundBoxGeo.YMax);
           end;
        end
        else begin
           {$IfDef RecordWMS}  WriteLineToDebugFile('not KMLcompatibleMap, request UTM'); {$EndIf}
            //NAD83 UTM: EPSG:26917, with last two the UTM zone
            //WGS84 UTM: EPSG:32601, with last two the UTM zone, N hemi
            //WGS84 UTM: EPSG:32701, with last two the UTM zone, S hemi
           Result := Result + 'EPSG:269' + AddDayMonthLeadingZero(PrimaryMapProjection.projUTMZone) +
              BBoxString(0.1 * round(10 * MapCorners.BoundBoxUTM.xmin),0.1 * round(10 * MapCorners.BoundBoxUTM.ymin),0.1 * round(10 * MapCorners.BoundBoxUTM.xmax), 0.1 * round(10 * MapCorners.BoundBoxUTM.ymax));
        end;

        Result := Result + '&FORMAT=' + 'image/png&STYLES=' + '&Width=' + IntToStr(MapXSize) + '&Height=' + IntToStr(MapYSize);
         {$IfDef RecordWMS}   WriteLineToDebugFile('WMS request=  ' + Result);  {$Else} WriteLineToDebugFile('WMS request=  ' + Result); {$EndIf}
      end;


      procedure tMapDraw.DownloadWMSforThisMap(var fName : PathStr);
      var
         Request : ANSIString;
      begin
         Request := CreateWMSRequest;
         {$IfDef RecordWMS} WriteLineToDebugFile('DownloadWMSforThisMap, request=' + Request);     {$EndIf}
         if (LastWMSService = '') then fName := ''
         else begin
             {$IfDef Android}
             fName := System.IOUtils.TPath.Combine(MainMapData, 'wms_map' + '.jpg');
             {$Else}
             fName := NextFileNumber(MDTempDir, 'wms_map_', '.png');
             {$EndIf}
            DownloadFileFromWeb(Request,fName);
         end;
         {$IfDef RecordWMS}  WriteLineToDebugFile('DownloadWMSforThisMap, out'); {$EndIf}
      end;



      procedure tMapDraw.DrawMMSOverlay(var Bitmap : tMyBitmap);


             function TryAnotherWMSService : boolean;
             begin
                {$IfDef RecordWMS}  WriteLineToDebugFile('TryAnotherWMSService wms bitmap not found');  {$EndIf}
                Result := AnswerIsYes('No WMS coverage at ' + LastWMSService + MessLineBreak + ' Try another');
                if Result then begin
                   WMSLayerfName := '';
                   LastWMSService := '';
                   FavoriteWMS := '';
                end
                else begin
                   WMSOverlayOnMap := false;
                end;
             end;

      label
          RestartWMS;
      var
          Bitmap2 : tMyBitmap;
          afile : file;
          HTMLName : PathStr;
          Header : array[1..10] of byte;
      begin
            RestartWMS:;
            if WMSOverlayOnMap then begin
                if (WMSLayerfName = '') then begin
                  {$IfDef RecordWMS} WriteLineToDebugFile('TMapDraw WMS, grab new image');  {$EndIf}
                   DownloadWMSforThisMap(WMSLayerfName);
                end;
                {$IfDef RecordWMS}  WriteLineToDebugFile('wms layer name =' + WMSLayerfName);    {$EndIf}
                if FileExists(WMSLayerfName) then begin
                   assignFile(afile,WMSLayerfName);
                   reset(afile,1);
                   BlockRead(afile,Header,10);
                   CloseFile(aFile);
                   if (Header[1] <> 137) or (Header[2] <> 80) or (Header[3] <> 78) or (Header[4] <> 71) then begin
                      {$IfDef VCL}
                      HTMLName := ChangeFileExt(WMSLayerfName,'.htm');
                      RenameFile(WMSLayerfName, HTMLname);
                      ExecuteFile(HTMLName, '', '');
                      WMSLayerfName := '';
                      WMSOverlayOnMap := false;
                      {$EndIf}
                   end
                   else begin
                      Bitmap2 := PetImage.LoadBitmapFromFile(WMSLayerfName);
                      if IsBitmapMonochrome(Bitmap2) then begin
                         if TryAnotherWMSService then goto RestartWMS;
                      end
                      else begin
                         {$IfDef VCL}
                           BlendBitmapAtop(Bitmap,Bitmap2,MDDef.WMSOpacityValue * 0.01);
                         {$Else}
                            Bitmap.CopyFromBitmap(Bitmap2);
                         {$EndIf}
                      end;
                      if (Bitmap2 <> Nil) then Bitmap2.Free;
                   end;
                end
                else begin
                    {$IfDef VCL}
                    if AnswerIsYes('WMS request failed; do you want to see failure message in browser') then begin
                       ExecuteFile(CreateWMSRequest,'','');
                    end;
                    {$EndIf}
                    if TryAnotherWMSService then goto RestartWMS;
                end;
            end;
      end;


{$EndIf}



