{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}
{                                        }
{ include file for demmapf               }
{________________________________________}

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
   {$IfDef RecordSave} WriteLineToDebugFile('TMapForm.Contoursshapefile1Click'); {$EndIf}
   if IsGDALFilePresent(GDAL_contour_name) then begin
      ContInt := 10;
      ReadDefault('contour interval (m)',ContInt);
      InName := DEMGlb[MapDraw.DEMonMap].DEMFileName;
      OutName := Petmar.NextFileNumber(MDtempdir,DEMGlb[MapDraw.DEMonMap].AreaName + '_' + RealToString(ContInt,-8,-2) + '_contours_','.shp');
      GDALcommand(MDTempDir + 'contour.bat', GDAL_contour_name + ' -a elev ' + GeotiffDEMNameOfMap + ' ' + OutName + ' -i ' + RealToString(ContInt,-12,-2));
      LoadDataBaseFile(OutName);
   end;
{$EndIf}
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
   GDAL_HillshadeMap_Horn(true,MapDraw.DEMonMap);
end;


procedure TMapForm.GDALroughness1Click(Sender: TObject);
begin
   GDAL_Roughness(true,MapDraw.DEMonMap);
end;

procedure TMapForm.GDALslopemap1Click(Sender: TObject);
var
   NewGrid : integer;
begin
   NewGrid := GDAL_SlopeMap_ZT(true,MapDraw.DEMonMap);
   MatchAnotherDEMMap(NewGrid,MapDraw.DEMonMap);
end;


procedure TMapForm.GDALslopemapHorn1Click(Sender: TObject);
var
   NewGrid : integer;
begin
   NewGrid := GDAL_SlopeMap_Horn(true,MapDraw.DEMonMap);
   MatchAnotherDEMMap(NewGrid,MapDraw.DEMonMap);
end;

procedure TMapForm.GDALTPI1Click(Sender: TObject);
begin
   GDAL_TPI(true,MapDraw.DEMonMap);
end;


procedure TMapForm.GDALTRIRileyterrestrial1Click(Sender: TObject);
begin
   GDAL_TRI_Riley(true,MapDraw.DEMonMap);
end;

procedure TMapForm.GDALTRIWilsonbathymetric1Click(Sender: TObject);
begin
   GDAL_TRI_Wilson(true,MapDraw.DEMonMap);
end;

procedure TMapForm.GDALupsample1Click(Sender: TObject);
begin
   GDAL_upsample_DEM(True,MapDraw.DEMonMap,true);
end;

procedure TMapForm.GDALwarpsubset1Click(Sender: TObject);
begin
   {$IfDef ExGDAL}
   {$Else}
      GDALSubsetSatImageToMatchMap(Self,GDAL_warp_Name);
   {$EndIf}
end;





