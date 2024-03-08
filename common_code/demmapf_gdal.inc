{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}
{                                   }
{ include file for demmapf          }
{___________________________________}

procedure TMapForm.GDALinfo1Click(Sender: TObject);
{$IfDef ExGDAL}
begin
{$Else}
var
   GDALinfo : tGDALinfo;
   fName    : PathStr;
begin
     if MapDraw.DEMMap then fName := DEMGlb[Mapdraw.DEMonMap].DEMFileName
     else if ValidSatImage(MapDraw.SatOnMap) then fName := SatImage[MapDraw.SATonMap].IndexFileName
     else exit;
     GetGDALinfo(fName, GDALinfo);
     ShowInNotepadPlusPlus(GDALinfoOutputFName(fname),'GDALinfo results ' + ExtractFileName(fName));
{$EndIf}
end;



procedure TMapForm.GDALtranslatesubset1Click(Sender: TObject);
begin
   {$IfDef ExGDAL}
   {$Else}
      GDALSubsetSatImageToMatchMap(Self,GDAL_Translate_Name);
   {$EndIf}
end;



procedure TMapForm.ContoursShapefile1Click(Sender: TObject);
{$IfDef ExGDAL}
begin
{$Else}
var
   OutName,InName : PathStr;
   ContInt : float64;
begin
   {$IfDef RecordSave} WriteLineToDebugFile('TMapForm.Contoursshapefile1Click');  {$EndIf}
   if IsGDALFilePresent(GDAL_contour_name) then begin
      ContInt := 100;
      ReadDefault('contour interval (m)',ContInt);
      InName := DEMGlb[MapDraw.DEMonMap].DEMFileName;
      OutName := Petmar.NextFileNumber(MDtempdir,'contours_','.shp');
      //if AnswerIsYes('Shapefile vice DXF') then OutName := MDtempdir + 'contours.shp'
      //else OutName := MDtempdir + 'contours.dxf';
      GDALcommand(MDTempDir + 'contour.bat', GDAL_contour_name + ' -a elev ' + GeotiffDEMNameOfMap + ' ' + OutName + ' -i ' + RealToString(ContInt,-12,-2));
      LoadDataBaseFile(OutName);
   end;
{$EndIf}
end;

procedure TMapForm.GDALaspectmapHorn1Click(Sender: TObject);
begin
   GDAL_AspectMap_Horn(GeotiffDEMNameOfMap, DEMGlb[MapDraw.DEMonMap].GDAL_ScaleFactorString);
end;

procedure TMapForm.GDALaspectmapZevenbergenThorne1Click(Sender: TObject);
begin
   GDAL_AspectMap_ZT(GeotiffDEMNameOfMap, DEMGlb[MapDraw.DEMonMap].GDAL_ScaleFactorString);
end;

procedure TMapForm.GDALbilinearbicubictoUTM1Click(Sender: TObject);
begin
   {$IfDef NoExternalPrograms}
   {$Else}
      GDAL_dual_UTM(MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.GDALfillholes1Click(Sender: TObject);
begin
   {$IfDef NoExternalPrograms}
   {$Else}
      GDAL_Fill_Holes(GeotiffDEMNameOfMap);
   {$EndIf}
end;

procedure TMapForm.GDALhillshadeHorn1Click(Sender: TObject);
begin
   GDAL_HillshadeMap_Horn(GeotiffDEMNameOfMap, DEMGlb[MapDraw.DEMonMap].GDAL_ScaleFactorString);
end;


procedure TMapForm.GDALroughness1Click(Sender: TObject);
begin
   GDAL_Roughness(GeotiffDEMNameOfMap);
end;

procedure TMapForm.GDALslopemap1Click(Sender: TObject);
var
   NewGrid : integer;
begin
   NewGrid := GDAL_SlopeMap_ZT(GeotiffDEMNameOfMap, DEMGlb[MapDraw.DEMonMap].GDAL_ScaleFactorString);
   MatchAnotherDEMMap(NewGrid,MapDraw.DEMonMap);
end;


procedure TMapForm.GDALslopemapHorn1Click(Sender: TObject);
var
   NewGrid : integer;
begin
   NewGrid := GDAL_SlopeMap_Horn(GeotiffDEMNameOfMap, DEMGlb[MapDraw.DEMonMap].GDAL_ScaleFactorString);
   MatchAnotherDEMMap(NewGrid,MapDraw.DEMonMap);
end;

procedure TMapForm.GDALTPI1Click(Sender: TObject);
begin
   GDAL_TPI(GeotiffDEMNameOfMap);
end;


procedure TMapForm.GDALTRIRileyterrestrial1Click(Sender: TObject);
begin
   GDAL_TRI_Riley(GeotiffDEMNameOfMap);
end;

procedure TMapForm.GDALTRIWilsonbathymetric1Click(Sender: TObject);
begin
   GDAL_TRI_Wilson(GeotiffDEMNameOfMap);
end;

procedure TMapForm.GDALupsample1Click(Sender: TObject);
begin
   GDAL_upsample_DEM(MapDraw.DEMonMap,true);
end;

procedure TMapForm.GDALwarpsubset1Click(Sender: TObject);
begin
   {$IfDef ExGDAL}
   {$Else}
      GDALSubsetSatImageToMatchMap(Self,GDAL_warp_Name);
   {$EndIf}
end;





