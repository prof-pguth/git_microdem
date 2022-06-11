unit kml_creator;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define KMLProblems}
{$EndIf}


interface

uses
  SysUtils,  Classes, Graphics, Controls,ExtCtrls, Forms, //Dialogs,
  Winapi.Windows,
  PETMAR,Petmar_types,
  DEMMapf;

type
    tKMLCreator = class
      protected
      private
         procedure StartGroundOverlay(TheName : shortString; fName : PathStr);
         procedure EndGroundOverlay;
         procedure StartPlaceMark(Name,Desc : AnsiString);
      public
         KMLStringList : tStringList;
         KMLOutputPath : PathStr;
         oName : shortString;
         constructor Create(Path : PathStr = ''; Name : shortString = '');
         procedure AddGroundOverlay(TheName : shortString; TheMap : tMapForm; fName : PathStr);
         procedure CloseAndSaveFile(OpenGENow : boolean; var fName : PathStr);
         procedure AddFolder(Name,Desc : AnsiString);
         procedure EndFolder;
         procedure AddWorldFileOverlay(fName : PathStr; Caption : ShortString);
         procedure AddPlaceMark(Lat,Long : float64; Name,Desc : Ansistring; TimeSpan : AnsiString; z : float64 = 0; StyleURL : string35 = '');
         procedure GroundOverlayBoundingBox(LatHigh,LatLow,LongHigh,LongLow : float64);
         procedure EncodeLatLongZ(Lat,Long,z : float64);
         procedure ZipKMZ(fName : PathStr = '');
         procedure AddLineString(Name,Desc,Style : Ansistring);
         procedure EndLineString;
         procedure AddPolygonString(Name, Desc, Style: Ansistring);
         procedure EndPolygonString;
         procedure AddLogo(fName : PathStr; UpperLeft : boolean);
         procedure AddName(Name : ShortString);
         procedure AddIcon(Name : PathStr; BoundScale : boolean);
         procedure AddDescription(Desc : AnsiString);
         destructor Destroy;
    end;


procedure LoadMapInGoogleEarth(Title : ShortString; Map : tMapForm);
procedure SimpleLoadGoogleMaps(Lat,Long : float64);

procedure OverlayKMLonMap(Map : tMapForm; fName : PathStr);
procedure MakeLegendOverlay(TheName : shortString; Image1 : tImage; UpperLeft : boolean);
function GetKMLPath(Path : PathStr{; var KMLRandomBase : ShortString}) : PathStr;

procedure CreateKMLImageOverlay(Path,fName,fname2 : PathStr; North,East,West,South : float64);
procedure UnzipKMLtoKMLDir(var Dir : PathStr; fName : PathStr);


implementation

{ tKMLCreator }

uses
   {$IfDef ExGIS}
   {$Else}
      demdatabase,
   {$EndIf}
      PetImage,KML_overlay,KML_opts,DEMDefs,
   {$IfDef ExSat}
   {$Else}
      DEMEROS,
   {$EndIf}
   DEM_Indexes,
   DEMCoord;


procedure UnzipKMLtoKMLDir(var Dir : PathStr; fName : PathStr);
begin
   Dir := '';
   Dir := GetKMLPath(Dir);
   ZipMasterUnzip(fName,Dir);
end;



procedure CreateKMLImageOverlay(Path,fName,fname2 : PathStr; North,East,West,South : float64);
var
   KML : tKMLCreator;
   sName : PathStr;
begin
   KML := tKMLCreator.Create(Path,'');
   sName := KML.KMLOutputPath + fName2;
   KML.StartGroundOverlay(fName2,fName);
   KML.GroundOverlayBoundingBox(North,South,East,West);
   KML.EndGroundOverlay;
   KML.CloseAndSaveFile(true,sName);
   KML.Destroy;
end;


procedure MakeLegendOverlay(TheName : shortString; Image1 : tImage; UpperLeft : boolean);
var
   KML : tKMLCreator;
   sName : PathStr;
   bmp : tMyBitmap;
begin
   KML := tKMLCreator.Create('',TheName);
   sName := KML.KMLOutputPath + TheName;
   CopyImageToBitmap(Image1,bmp);
   bmp.SaveToFile(sName + '.png');
   bmp.Free;
   KML.AddLogo(sName + '.png',UpperLeft);
   sName := sName + '.kml';
   KML.CloseAndSaveFile(true,sName);
   KML.Destroy;
end;

procedure OverlayKMLonMap(Map : tMapForm; fName : PathStr);
var
   fName2,fName3 : PathStr;
   Bitmap : tMyBitmap;
begin
   fName2 := ChangeFileExt(fName,'.png');
   fName3 := ChangeFileExt(fName,'.pnw');
   if SysUtils.FileExists(fName2) and FileExists(fName3) then begin
      CopyImageToBitmap(Map.Image1,Bitmap);
      Map.MapDraw.DrawWorldFileImageOnMap(Bitmap,fName2);
      Map.Image1.Picture.Graphic := Bitmap;
      Bitmap.Free;
   end;
end;


procedure SimpleLoadGoogleMaps(Lat,Long : float64);
var
   url : string;
begin
   if (Lat > -990) then url := 'https://www.google.com/maps?z=16&q=' + RealToString(Lat,-18,-8) + ',' + RealToString(Long,-18,-8) + '&t=h'
   else url := 'https://maps.google.com/maps?tab=wl';
   ExecuteFile(url, '', '');
end;


procedure LoadMapInGoogleEarth(Title : ShortString; Map : tMapForm);
var
   KML : tKMLCreator;
   Ext : ExtStr;
   TStr : ShortString;
   i : integer;
   hName,fName,tName : PathStr;
   KML_over_opts : TKML_over_opts;


   procedure LoadLayer(fName : PathStr; LayerName : string35);
   var
      Bitmap : tMyBitmap;
   begin
      {$IfDef KMLProblems} WriteLineToDebugFile('Load layer: ' + fName); {$EndIf}
      if (fName <> '') then begin
         Bitmap := tMyBitmap.Create;
         Bitmap.LoadFromFile(fName);
         Map.Image1.Picture.Graphic := Bitmap;
         fName := NextFileNumber(KML.KMLOutputPath,'kml_map', Ext);
         PetImage.SaveImageAsBMP(Map.Image1,fname);
         KML.AddGroundOverlay(LayerName,Map,fName);
      end;
   end;

begin
   {$IfDef KMLProblems} WriteLineToDebugFile('LoadMapInGoogleEarth enter, ' + Map.Caption); {$EndIf}
   KML_over_opts := TKML_over_opts.Create(Application);
   KML_over_opts.CheckBox8.Enabled := (Map.MapDraw.CurrentFansTable <> 0);
   if MDDef.AskAboutKMLExport then begin
      {$IfDef KMLProblems} WriteLineToDebugFile('LoadMapInGoogleEarth about to showmodal'); {$EndIf}
      KML_over_opts.ShowModal;
   end;

   ShowHourglassCursor;

   {$IfDef KMLProblems} WriteLineToDebugFile('Options set'); {$EndIf}
   if MDDef.UseGif then Ext := '.gif'
   else Ext := '.png';
   {$IfDef KMLProblems} WriteLineToDebugFile('Picked: ' + Ext); {$EndIf}

   KML := tKMLCreator.Create('',Title);
   KML.oName := 'kml_map';

   if (KMLLogo1FileName <> '') then KML.AddLogo(KMLLogo1FileName,true);
   if (KMLLogo2FileName <> '') then KML.AddLogo(KMLLogo2FileName,false);

   if (MDDef.KMLOutputOption in [0,1]) then begin
      if MDDef.KMLExportLayers then begin
         LoadLayer(Map.MapDraw.TigerOverlayFName,'Tiger layer');
         LoadLayer(Map.MapDraw.ContourOverlayfName,'Contour layer');
         LoadLayer(Map.MapDraw.ContourOverlayfName2,'Contour layer2');
         LoadLayer(Map.MapDraw.AllFansCoverageFName,'Composite Viewshed layer');
         LoadLayer(Map.MapDraw.PLSSOverlayfName,'PLSS layer');
         LoadLayer(Map.MapDraw.GazOverlayfName,'Gazetteer layer');
         LoadLayer(Map.MapDraw.BaseMapFName,'Base map layer');
         if (Map.MapDraw.FloodLayers <> Nil) then begin
            for i := 0 to pred(Map.MapDraw.FloodLayers.Count) do begin
               hName := ExtractFileName(Map.MapDraw.FloodLayers.Strings[i]);
               Delete(hName,length(HName)-3,4);
               hName := hName + ' m';
               hName[6] := ' ';
               LoadLayer(Map.MapDraw.FloodLayers.Strings[i],hName);
            end;
         end;
      end
      else begin
         fName := Map.Caption;
         StripBlanks(fName);
         StripInvalidPathNameChars(fName);
         fName := KML.KMLOutputPath + fName + '_' + IntToStr(Random(100)) + Ext;
         PetImage.SaveImageAsBMP(Map.Image1,fname);
         KML.AddGroundOverlay(Title,Map,fName);
      end;

      if MDDef.KMLExportSeparateFans and (Map.MapDraw.CurrentFansTable <> 0) then begin
         GISdb[Map.MapDraw.CurrentFansTable].MyData.First;
         while not GISdb[Map.MapDraw.CurrentFansTable].MyData.EOF do begin
            tName := GISdb[Map.MapDraw.CurrentFansTable].MyData.GetFieldByNameAsString('IMAGE');
            if (TName <> '') then begin
               TStr := GISdb[Map.MapDraw.CurrentFansTable].MyData.GetFieldByNameAsString('NAME');
               if (TStr = '') then TStr := 'Sensor';
               KML.AddWorldFileOverlay(tName,TStr);
            end;
            GISdb[Map.MapDraw.CurrentFansTable].MyData.Next;
         end;
      end;

      if MDDef.KMLExportAllMaps then begin
         {$IfDef KMLProblems} WriteLineToDebugFile('AllMapsCheckBox3.Checked'); {$EndIf}
         {$IfDef ExSat}
         {$Else}
            for I := 1 to MaxSatAllowed do begin
               if (SatImage[i] <> Nil) and (SatImage[i].SelectionMap.Handle <> Map.Handle) and (SatImage[i].SelectionMap.MapDraw.KMLcompatibleMap ) then begin
                  fName := NextFileNumber(KML.KMLOutputPath, (SatImage[i].SceneBaseName), Ext);
                  PetImage.SaveImageAsBMP(SatImage[i].SelectionMap.Image1,fname);
                  KML.AddGroundOverlay(SatImage[i].SceneBaseName,SatImage[i].SelectionMap,fName);
               end;
            end;
         {$EndIf}
         for i := 1 to MaxDEMDataSets do begin
            if (DEMGlb[i] <> Nil) and (DEMGlb[i].SelectionMap.Handle <> Map.Handle) and (DEMGlb[i].SelectionMap.MapDraw.KMLcompatibleMap ) then begin
               fName := NextFileNumber(KML.KMLOutputPath, (DEMGlb[i].AreaName), Ext);
               PetImage.SaveImageAsBMP(DEMGlb[i].SelectionMap.Image1,fname);
               KML.AddGroundOverlay(DEMGlb[i].AreaName,DEMGlb[i].SelectionMap,fName);
            end;
         end;
      end;
      {$IfDef KMLProblems} WriteLineToDebugFile('Ground overlay added: ' + fName); {$EndIf}
   end
   else fName := NextFileNumber(KML.KMLOutputPath, 'kml_map', Ext);

   ShowHourglassCursor;
   if (MDDef.KMLOutputOption in [0,2]) then begin
      for i := 1 to DEMDefs.MaxDataBase do begin
         if (GISdb[i] <> Nil) and (GISdb[i].KMLExportable) and (GISdb[i].dbName <> '') and (MDDef.KMLDefaultDB or AnswerIsYes('Include DB ' + GISdb[i].dbName)) then begin
            {$IfDef KMLProblems} WriteLineToDebugFile('DB adding: ' + GISDB[i].dbName); {$EndIf}
            KML_opts.ConvertToKML(i,KML.KMLOutputPath,kml);
            {$IfDef KMLProblems} WriteLineToDebugFile('DB added: ' + GISDB[i].dbName); {$EndIf}
         end;
      end;
   end;
   fName := '';
   KML.CloseAndSaveFile(MDDef.KMLOpenGoogleEarth,fName);
   KML.Destroy;

   ShowDefaultCursor;
   {$IfDef KMLProblems} WriteLineToDebugFile('LoadMapInGoogleEarth out,  File saved to: ' + fName); {$EndIf}
end;


function GetKMLPath(Path : PathStr{; var KMLRandomBase : ShortString}) : PathStr;
var
   Base : PathStr;
begin
   Base := MainMapData + 'KML\';
   SafeMakeDir(Base);
   {$IfDef KMLProblems} WriteLineToDebugFile('KML output base=' + Base);  {$EndIf}
   Path := NextFilePath(Base + 'KML');
   Result := Path;
end;



constructor tKMLCreator.Create(Path : PathStr = ''; Name : shortString = '');
begin
   oName := 'md_map';
   if (Name = '') then Name := ExtractFileNameNoExt(Application.ExeName);
   {$IfDef KMLProblems} WriteLineToDebugFile('Input KML output dir=' + Path); {$EndIf}
   KMLOutputPath := GetKMLPath(Path);
   {$IfDef KMLProblems} WriteLineToDebugFile('Using KML output dir=' + KMLOutputPath );  {$EndIf}
   KMLStringList := tStringList.Create;
   KMLStringList.Add('<?xml version="1.0" encoding="UTF-8"?>');
   KMLStringList.Add('<kml xmlns="http://earth.google.com/kml/2.2">');
   KMLStringList.Add('<Document>');
   if MDDef.KMLTopLevelFolder then KMLStringList.Add('<Folder>');
   Self.AddName(Name);
   KMLStringList.Add('<open>1</open>');
end;


destructor tKMLCreator.Destroy;
begin
   KMLStringList.Free;
   inherited;
end;

procedure tKMLCreator.AddName(Name : ShortString);
begin
   if (Name <> '') then KMLStringList.Add('<name>' + Name + '</name>');
end;

procedure tKMLCreator.AddDescription(Desc : AnsiString);
begin
   if (Desc <> '') then KMLStringList.Add('<description>' + Desc + '</description>');
end;


procedure tKMLCreator.AddLogo(fName : PathStr; UpperLeft : boolean);
const
   FractionUnitsStr = ' xunits="fraction" yunits="fraction"';
var
   ch : char;
begin
   if FileExists(fName) then begin
      KMLStringList.Add('<ScreenOverlay>');
      AddName(ExtractFileNameNoExt(fName));
      AddIcon(fname,false);
      if UpperLeft then ch := '1' else ch := '0';
      KMLStringList.Add('<overlayXY x="0" y="' + ch + '" ' + FractionUnitsStr  + '/>');
      KMLStringList.Add('<screenXY x="0" y="' + ch + '" '  + FractionUnitsStr  + '/>');
      KMLStringList.Add('<rotationXY x="0" y="0" ' + FractionUnitsStr  + '/>');
      KMLStringList.Add('<size x="0" y="0" ' + FractionUnitsStr  + '/>');
      KMLStringList.Add('</ScreenOverlay>');
      CopyFile(fName,KMLOutputPath + ExtractFileName(fName));
   end
   else MessageToContinue('Logo file missing : ' + fName);
end;


procedure tKMLCreator.EncodeLatLongZ(Lat, Long, z : float64);
begin
    KMLStringList.Add(RealToString(Long,-18,-6) + ',' + RealToString(Lat,-18,-6) + ',' + RealToString(z,-18,-6) );
end;

procedure tKMLCreator.EndFolder;
begin
   KMLStringList.Add('</Folder>');
end;

procedure tKMLCreator.EndLineString;
begin
   KMLStringList.Add('</coordinates>');
   KMLStringList.Add('</LineString>');
   KMLStringList.Add('</Placemark>');
end;


procedure tKMLCreator.EndPolygonString;
begin
   KMLStringList.Add('</coordinates>');
   KMLStringList.Add('</LinearRing>');
   KMLStringList.Add('</outerBoundaryIs>');
   KMLStringList.Add('</Polygon>');
   KMLStringList.Add('</Placemark>');
end;


procedure tKMLCreator.StartPlaceMark(Name,Desc : AnsiString);
begin
   KMLStringList.Add('<Placemark>');
   if (Name <> '') then AddName(Name);
   if (Desc <> '') then AddDescription(Desc);
end;


procedure tKMLCreator.ZipKMZ(fName: PathStr = '');
{$IfDef ExZip}
begin
{$Else}
var
   TheFiles : tStringList;
   i : integer;
begin
   {$IfDef KMLProblems} WriteLineToDebugFile('tKMLCreator.ZipKMZ in, fName=' + fName); {$EndIf}
   TheFiles := tStringList.Create;
   Petmar.FindMatchingFiles(KMLOutputpath,'*.*',TheFiles);
   if (fName = '') then fName := KMLOutputpath + 'KML_file.zip';

   fName := ChangeFileExt(fName,'.zip');
   {$IfDef KMLProblems} WriteLineToDebugFile('call compressor for, fName=' + fName); {$EndIf}

   ZipMasterZipFiles(fName,TheFiles);
   for I := 0 to pred(TheFiles.Count) do SysUtils.DeleteFile(TheFiles.Strings[i]);
   TheFiles.Free;
   SysUtils.RenameFile(fname,ChangeFileExt(fName,'.kmz'));

   fName := ChangeFileExt(fName,'.kmz');

   {$IfDef KMLProblems} WriteLineToDebugFile('tKMLCreator.ZipKMZ out, fName=' + fName); {$EndIf}
{$EndIf}
end;

procedure tKMLCreator.CloseAndSaveFile;
begin
   {$IfDef KMLProblems} WriteLineToDebugFile('tKMLCreator.CloseAndSaveFile in, fName=' + fName); {$EndIf}
   if (fName = '') then fName := KMLOutputPath + oName + '.KML';
   if MDDef.KMLTopLevelFolder then KMLStringList.Add('</Folder>');
   KMLStringList.Add('</Document>');
   KMLStringList.Add('</kml>');
   KMLStringList.SaveToFile(fName);
   {$IfDef KMLProblems} WriteLineToDebugFile('tKMLCreator.CloseAndSaveFile saved ok to ' + fName); {$EndIf}
   if MDDef.ShowKMLfile then QuickOpenEditWindow(FName,'KML file',false,true);
   if MDDef.ZipKMLfiles then begin
      {$IfDef KMLProblems} WriteLineToDebugFile('tKMLCreator.CloseAndSaveFile zipping ' + fName); {$EndIf}
      fName := ChangeFileExt(fName,'.KMZ');
      ZipKMZ(fName);
   end;
   ApplicationProcessMessages;
   if OpenGENow then begin
      {$IfDef KMLProblems} WriteLineToDebugFile('tKMLCreator.CloseAndSaveFile exec, fName=' + fName); {$EndIf}
      ExecuteFile(fName, '', '');
   end;

   {$IfDef KMLProblems} WriteLineToDebugFile('tKMLCreator.CloseAndSaveFile out, fName=' + fName); {$EndIf}
end;

procedure tKMLCreator.AddFolder(Name, Desc: AnsiString);
begin
   KMLStringList.Add('<Folder>');
   AddName(Name);
   AddDescription(Desc);
end;

procedure tKMLCreator.AddIcon(Name : PathStr; BoundScale : boolean);
begin
	KMLStringList.Add('<Icon>');
	KMLStringList.Add('	<href>' + ExtractFileName(Name) + '</href>');
	if BoundScale then KMLStringList.Add('	<viewBoundScale>0.75</viewBoundScale>');
	KMLStringList.Add('</Icon>');
end;


procedure tKMLCreator.StartGroundOverlay(TheName : shortString; fName : PathStr);
begin
   KMLStringList.Add('<GroundOverlay>');
	AddName(TheName);
	KMLStringList.Add('<color>9effffff</color>');
   AddIcon(fName,true);
	KMLStringList.Add('<LatLonBox>');
end;


procedure tKMLCreator.EndGroundOverlay;
begin
	KMLStringList.Add('</LatLonBox>');
   KMLStringList.Add('</GroundOverlay>');
end;


procedure tKMLCreator.GroundOverlayBoundingBox(LatHigh,LatLow,LongHigh,LongLow : float64);
begin
   KMLStringList.Add('	<north>' + RealToString(LatHigh,-18,-6) + '</north>');
   KMLStringList.Add('	<south>' + RealToString(LatLow,-18,-6) + '</south>');
   KMLStringList.Add('	<east>' + RealToString(LongHigh,-18,-6) + '</east>');
   KMLStringList.Add('	<west>' + RealToString(LongLow,-18,-6) + '</west>');
end;


procedure tKMLCreator.AddWorldFileOverlay(fName : PathStr; Caption : ShortString);
var
   Bitmap : tMyBitmap;
   DeltaX,Xrot,YRot,DeltaY,UpLeftX,UpLeftY : float64;
begin
   if FileExists(fName) then begin
      StartGroundOverlay(Caption,fName);
      CopyFile(fName,KMLOutputPath + ExtractFileName(fName));
      Bitmap := PetImage.LoadBitmapFromFile(fName);
      GetDefaultWorldFile(fName);
      ReadWorldFileValues(fName,DeltaX,Xrot,YRot,DeltaY,UpLeftX,UpLeftY);
      GroundOverlayBoundingBox(UpLeftY,UpLeftY+pred(Bitmap.Height) * DeltaY,UpLeftX+pred(Bitmap.Width) * DeltaX,UpLeftX);
      Bitmap.Free;
      EndGroundOverlay;
   end
   else MessageToContinue('Missing ground overlay: ' + fName);
end;


procedure tKMLCreator.AddGroundOverlay(TheName : shortString; TheMap : tMapForm; fName : PathStr);
begin
   {$IfDef KMLProblems} WriteLineToDebugFile('tKMLCreator.AddGroundOverlay, TheName =' + TheName); {$EndIf}
   StartGroundOverlay(TheName,fName);
   with TheMap.MapDraw.MapCorners do GroundOverlayBoundingBox(BoundBoxGeo.ymax,BoundBoxGeo.ymin,BoundBoxGeo.xmax,BoundBoxGeo.xmin);
   EndGroundOverlay;
   if MDDef.KMLCreateWorldFiles then TheMap.MapDraw.WriteMapsWorldFile(fName);
end;


procedure tKMLCreator.AddLineString(Name, Desc,Style : Ansistring);
begin
   StartPlaceMark(Name, Desc);
   if (Style <> '') then KMLStringList.Add(Style);
   KMLStringList.Add('<LineString>');
   KMLStringList.Add('<coordinates>');
end;

procedure tKMLCreator.AddPolygonString(Name, Desc,Style : Ansistring);
begin
   StartPlaceMark(Name, Desc);
   if (Style <> '') then KMLStringList.Add(Style);
   KMLStringList.Add('<Polygon>');
   KMLStringList.Add('<tessellate>1</tessellate>');
   KMLStringList.Add('<outerBoundaryIs>');
   KMLStringList.Add('<LinearRing>');
   KMLStringList.Add('<coordinates>');
end;



procedure tKMLCreator.AddPlaceMark(Lat, Long : float64; Name, Desc : Ansistring; TimeSpan : Ansistring; z : float64 = 0; StyleURL : string35 = '');
begin
   StartPlaceMark(Name, Desc);
   if (TimeSpan <> '') then KMLStringList.Add(TimeSpan);
   if (StyleURL <> '') then KMLStringList.Add(StyleUrl);
   KMLStringList.Add('<Point>');
   if (abs(z) > 0.00001) then begin
      //required here, for every point, or it will clamp to ground
      KMLStringList.Add('<altitudeMode>absolute</altitudeMode>');
   end;
   KMLStringList.Add('<coordinates>' + RealToString(Long,-18,-6) + ',' + RealToString(Lat,-18,-6) +  ',' + RealToString(z,-18,-2) + '</coordinates>');
   KMLStringList.Add('</Point>');
   KMLStringList.Add('</Placemark>');
end;


initialization
finalization
   {$IfDef KMLProblems} WriteLineToDebugFile('KMLProblems active in kml_creator'); {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing kml_creator out'); {$EndIf}
end.








