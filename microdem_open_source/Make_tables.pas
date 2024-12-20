unit Make_tables;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}
   //{$Define RecordMakeTables}
{$EndIf}


interface


uses
   Sysutils,db,
   PetMar,Petmar_types,Petmar_db,DEMDefs;

const
   MaxSeriesNameLength = 35;

procedure CreateLatLongTable(fName : PathStr; HighAcc : boolean = true);
procedure CreateGeologyTable(fName : PathStr);
procedure CreateGPSTrackTable(fName : PathStr);

procedure MakeICOADSTable(fName : PathStr);
procedure MakeCliwocTable(fName : PathStr);

procedure MakeFieldRangeTable(fName : PathStr);
procedure MakePhotoIndex(fName : PathStr);
procedure MakeBasicBoundingBoxTable(fName : PathStr; NameStr : shortstring = '');
procedure MakeTopoProfileTable(fName : PathStr; FresnelFields,VegFields,VegFields2,PointCloudFields,PointCloudFields2,GrazingFields,PointCloudExtremes,VoxelFields,VoxelFields2 : boolean);

procedure MakeFresnelPointsTable(fName : PathStr);

procedure CreateXYZTable(fName : PathStr; RecSeg : boolean);
procedure CreateThalwegTable(fName : PathStr);

procedure CreateOptimalRegistrationTable(fName : PathStr);

procedure CreateTableForVATforDEM(fName : PathStr);

procedure CreateIntegratedDataBaseTable(fName : PathStr);
procedure CreateIndexSeriesFile(fName : PathStr);

procedure CreateIndexSymbologyTable(fName : PathStr; Long : boolean; ShapeType : integer = 0);
procedure CreateLatLongTimeFile(fName : PathStr);
procedure CreateImageRegistrationTable(fName : PathStr);
procedure MakeRangeCircleTable(fName : PathStr);
procedure CreateDataUseTable(fName : PathStr);
procedure MakeSF3Table(fName : PathStr);
procedure CreateWindComponentTable(fName : PathStr);
procedure CreateThalwegFile(fName : PathStr);

procedure CreateFileNameIndexTable(fName : PathStr);
procedure CreateProjectTable(fName : PathStr);

procedure CreateMilIconsTable(fName : PathStr);

procedure MakeNamePlotTable(fName : PathStr);
procedure MakeNodeTable(fName : PathStr);
procedure CreateContourLineTable(fName : PathStr);
procedure CreateLatLongZTable(fName : PathStr; IncludeZ : boolean = true; IncludeColor : boolean = false;  IncludeAltitude : boolean = false; IncludeAzimuth : boolean = false;
   IncludeDistance : boolean = false; IncludeSource : boolean = false; IncludePitch : boolean = false);
procedure CreateUTM_ZTable(fName : PathStr);
procedure Create_ZTable(fName : PathStr);

procedure CreateTigerRulesDBF(fName : PathStr);

procedure MakeDummyRecordTable(fName : PathStr);
procedure CreatPointCloudProjectTable(fName : PathStr);

procedure MakeRedistrictingTable(fName : PathStr);
procedure CreateGeomorphometryAttributesTable(fname : PathStr; IncludeName : boolean);

procedure CreateColorBreaksTable(fName : PathStr);

procedure CreateFabricRegion(fName : PathStr);
procedure CreateInflectionTable(fName : PathStr);

procedure CreateDEMCorrelationTable(fname : PathStr);

procedure MakeProfileIndexTable(fName : PathStr);
procedure CreateGridlegendTable(LegendDBFfName : PathStr; MaxLen : integer);
procedure MakeTwoStringTable(fName : PathStr; f1,f2 : ShortString; l1,l2 : integer);
procedure MakeSensorTargetTable(fName : PathStr; AddDetails : boolean = false; StringLength : integer = 15);
procedure MakePCTable(fName : PathStr; NumComponents : integer);
procedure MakePickUseTable(fName : PathStr);

procedure MakeSatelliteTable(fName : PathStr; NumBands : integer);

procedure CreateTriangleTable(fName : PathStr);
procedure CreatePointFabricFile(fname : PathStr; DoWaveLength : boolean);

procedure MakeImageOverlayTable(fName: PathStr);
procedure MakeDelauneyTable(fName : PathStr; AddDelauneyZ,AddDelauneyImage : boolean);
procedure DefineAndCreateANewTable(FName : PathStr; AddXYZPoints,AddLatAndLong,AddBoundingBox : boolean; QuickClose : boolean = false; AddName : boolean = false; AddIcon : boolean = false);

procedure CreateTableOfType(NewName : PathStr; What : shortstring);
procedure MakeGMTfile(fName : PathStr; GMTfile : boolean);
procedure DefineAndCreateDatumShiftTable(FName : PathStr);
procedure MakeAtlasParametersTable(fName : PathStr);


{$IfDef ExHydrography}
{$Else}
   procedure MakeSideScanRecordFile(fName : PathStr; Long : boolean = true);
   procedure MakeHydrographyIndex(fName : PathStr);
   procedure MakeSideScanIndex(fName : PathStr);
   procedure MakeSideScanTargetFile(fName : PathStr);
{$EndIf}

{$IfDef ExGeology}
{$Else}
   procedure CreateGeologySymbolFile(fName : PathStr);
   procedure MakeStatColGIS(fName : PathStr);
   procedure MakeEarthQuakeCentroidsFile(fName : PathStr);
   procedure CreateXYADBTable(fName : PathStr);
{$EndIf}


implementation

uses
   Databasecreate;

var
  CreateDataBase : tCreateDataBase;


procedure MakeAtlasParametersTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   CreateDataBase.AddAField('PARAMETER',ftString,16);
   CreateDataBase.AddAField('MAX_CAT',ftFloat,14,6);
   CreateDataBase.AddAField('MIN_CAT',ftFloat,14,6);
   CreateDataBase.AddAField('MAX_COLOR',ftFloat,14,6);
   CreateDataBase.AddAField('MIN_COLOR',ftFloat,14,6);
   CreateDataBase.AddAField('CLASS',ftInteger,3);
   CreateDataBase.AddAField('USE',ftString,1);
   CreateDataBase.WriteCorrectHeader;
end;

procedure CreateGPSTrackTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   CreateDataBase.AddLatLongToTable;
   CreateDataBase.AddAField('Z',ftFloat,9,3);
   CreateDataBase.AddAField('SPEED',ftFloat,9,3);
   CreateDataBase.AddAField('HEADING',ftFloat,9,3);
   CreateDataBase.AddAField('TIME',ftstring,12,3);
   CreateDataBase.WriteCorrectHeader;
end;

procedure CreateLatLongTable(fName : PathStr; HighAcc : boolean = true);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   CreateDataBase.AddLatLongToTable(true);
   CreateDataBase.AddAField('Z',ftFloat,9,3);
   CreateDataBase.WriteCorrectHeader;
end;


procedure CreateGeologyTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   CreateDataBase.AddLatLongToTable;
   CreateDataBase.AddAField('Z',ftFloat,9,3);
   CreateDataBase.AddUTMtoTable;
   CreateDataBase.AddAField('DIPSTRIKE',ftString,10,0);
   CreateDataBase.AddAField('STRIKE',ftFloat,7,2);
   CreateDataBase.AddAField('DIP',ftFloat,6,2);
   CreateDataBase.WriteCorrectHeader;
end;


procedure DefineAndCreateDatumShiftTable(FName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddLatLongToTable;
      AddAField('DATUM_1',ftString,6);
      AddAField('DATUM_2',ftString,6);
      AddUTMtoTable;
      AddAField('UTM_ZONE',ftInteger,3);
      AddAField('MGRS',ftString,18);
      AddAField('DIFF_GEO',ftFloat,7,2);
      {$IfDef NoUTMDiffsDatumShift}
      {$Else}
         AddAField('DIFF_UTM',ftFloat,7,2);
      {$EndIf}
      AddAField('DIFF_GEO_X',ftFloat,7,2);
      AddAField('DIFF_GEO_Y',ftFloat,7,2);
      {$IfDef NoUTMDiffsDatumShift}
      {$Else}
         AddAField('DIFF_UTM_X',ftFloat,7,2);
         AddAField('DIFF_UTM_Y',ftFloat,7,2);
      {$EndIf}
      WriteCorrectHeader;
   end;
end;


procedure DefineAndCreateANewTable(FName : PathStr; AddXYZPoints,AddLatAndLong,AddBoundingBox : boolean; QuickClose : boolean = false; AddName : boolean = false; AddIcon : boolean = false);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      if AddName then AddAField('NAME',ftString,36);
      if AddLatAndLong then AddLatLongToTable;
      if AddXYZPoints then AddXYZToTable;
      if AddIcon then AddAField('ICON',ftString,24);
      if AddBoundingBox then AddBoundingBoxToTable;
      AddRecNoToTable;
      WriteCorrectHeader;
   end;
end;


procedure MakeGMTfile(fName : PathStr; GMTfile : boolean);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   if GMTfile then begin
      CreateDataBase.AddAField('F_TYPE',ftString,9,0);
      CreateDataBase.AddAField('F_NAME',ftString,48,0);
   end
   else begin
      CreateDataBase.AddAField('NAME',ftString,48,0);
   end;
   CreateDataBase.WriteCorrectHeader;
end;


procedure CreateTableOfType(NewName : PathStr; What : shortstring);
var
   Table : tMyData;
   fts   : ShortString;
begin
   {$IfDef RecordMakeTables} WriteLineToDebugFile('CreateTableOfType in'); {$EndIf}
    if FileExists(TableDefinitionsFileName) then begin
       CreateDataBase := tCreateDataBase.Create(NewName);
       with CreateDataBase do begin
          Table := tMyData.Create(TableDefinitionsFileName);
          Table.ApplyFilter( 'TABLE_TYPE=' + QuotedStr(What));
          while not Table.eof do begin
             fts := (Table.GetFieldByNameAsString('TYPE'));
             AddAField(Table.GetFieldByNameAsString('NAME'),ftFromString(fts),Table.GetFieldByNameAsInteger('LENGTH'),Table.GetFieldByNameAsInteger('DECIMALS'));
             Table.Next;
          end;
          Table.Destroy;
          WriteCorrectHeader;
       end;
      {$IfDef RecordMakeTables} WriteLineToDebugFile('CreateTableOfType succeed'); {$EndIf}
    end
    else begin
       {$IfDef VCL} MessageToContinue(TableDefinitionsFileName + ' missing; cannot create ' + what); {$EndIf}
    end;
end;


procedure MakeDelauneyTable(fName : PathStr; AddDelauneyZ,AddDelauneyImage : boolean);
var
   i : integer;
begin
    CreateDataBase := tCreateDataBase.Create(fName);
    with CreateDataBase do begin
      for i := 1 to 3 do begin
        AddAField('X' + IntToStr(i),ftFloat,18,6);
        AddAField('Y' + IntToStr(i),ftFloat,18,6);
        if AddDelauneyZ then AddAField('Z' + IntToStr(i),ftFloat,8,2);
        if AddDelauneyImage then begin
           AddAField('XIM' + IntToStr(i),ftInteger,6,0);
           AddAField('YIM' + IntToStr(i),ftInteger,6,0);
        end;
      end;
      AddAField('X_LOW',ftFloat,18,6);
      AddAField('X_HI',ftFloat,18,6);
      AddAField('Y_LOW',ftFloat,18,6);
      AddAField('Y_HI',ftFloat,18,6);
      if AddDelauneyImage then begin
         AddAField('XIM_LOW',ftInteger,6,0);
         AddAField('XIM_HI',ftInteger,6,0);
         AddAField('YIM_LOW',ftInteger,6,0);
         AddAField('YIM_HI',ftInteger,6,0);
      end;
       WriteCorrectHeader;
    end;
end;


procedure MakeImageOverlayTable(fName: PathStr);
begin
    CreateDataBase := tCreateDataBase.Create(fName);
    with CreateDataBase do begin
       AddAField('PLOT',ftString,1);
       AddAField('FILENAME',ftString,16);
       AddAField('X',ftInteger,5);
       AddAField('Y',ftInteger,5);
       AddAField('HEIGHT',ftInteger,5);
       AddAField('WIDTH',ftInteger,5);
       WriteCorrectHeader;
    end;
end;


procedure CreateProjectTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddBoundingBoxToTable;
      AddAField('DATA_LAYER',ftString,12,0);
      AddAField('FILENAME',ftString,255,0);
      AddAField('MAP_TYPE',ftInteger,3);
      WriteCorrectHeader;
   end;
end;


procedure CreatPointCloudProjectTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddLatLongToTable;
      AddAField('NAME',ftString,24,0);
      AddAField('USE',ftString,24,0);
      AddAField('TYPE',ftString,12,0);
      AddAField('FILENAME',ftString,255,0);
      AddAField('FILTER',ftString,64,0);
      AddAField('COLOR',ftInteger,12,0);
      AddAField('SYM_SIZE',ftInteger,2,0);
      AddAField('NUM_PTS',ftInteger,12,0);
      AddAField('MIN_X',ftFloat,18,8);
      AddAField('MAX_X',ftFloat,18,8);
      AddAField('MIN_Y',ftFloat,18,8);
      AddAField('MAX_Y',ftFloat,18,8);
      AddAField('MIN_Z',ftFloat,18,8);
      AddAField('MAX_Z',ftFloat,18,8);
      WriteCorrectHeader;
   end;
end;

procedure MakeCliwocTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddLatLongToTable;
      AddAField('SHIP',ftString,8);
      AddAField('SPEED_KPH',ftFloat,8,2);
      AddAField('KNOTS',ftFloat,8,2);
      AddAField('HEADING',ftFloat,8,2);
      AddAField('DEC_YEAR',ftFloat,12,6);
      AddAField('MONTH',ftInteger,2,6);
      WriteCorrectHeader;
   end;
end;


{$IfDef ExGeology}
{$Else}
procedure CreateGeologySymbolFile(fName : PathStr);
begin
   if not FileExists(fName) then begin
      CreateDataBase := tCreateDataBase.Create(fName);
      with CreateDataBase do begin
         AddLatLongToTable;
         AddAField('PLOT',ftString,1,0);
         AddAField('FEATURE',ftInteger,1,0);
         AddAField('DIPSTRIKE',ftString,10,0);
         AddAField('NOTE',ftString,64,0);
         WriteCorrectHeader;
      end;
   end;
end;


procedure MakeStatColGIS(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddLatLongToTable;
      AddAField('COL_NAME',ftString,80,0);
      AddAField('NAME',ftString,35,0);
      AddAField('THICK',ftFloat,10,2);
      WriteCorrectHeader;
   end;
end;


procedure MakeEarthQuakeCentroidsFile(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddAField('EVENT_ID',ftString,16,0);
      AddAField('MONTH',ftInteger,2,0);
      AddAField('DAY',ftInteger,2,0);
      AddAField('YEAR',ftInteger,4,0);
      AddAField('TIME',ftString,10,0);
      AddLatLongToTable;
      AddAField('DEPTH',ftFloat,6,2);
      AddAField('MB',ftFloat,6,2);
      AddAField('MS',ftFloat,6,2);
      AddAField('MECH',ftString,1,0);
      AddAField('REGION',ftString,30,0);
      AddAField('FP1_STRIKE',ftInteger,3,0);
      AddAField('FP1_DIP',ftInteger,2,0);
      AddAField('FP2_STRIKE',ftInteger,3,0);
      AddAField('FP2_DIP',ftInteger,2,0);
      AddAField('PLUNGE_1',ftInteger,2,0);
      AddAField('STRIKE_1',ftInteger,3,0);
      AddAField('PLUNGE_2',ftInteger,2,0);
      AddAField('STRIKE_2',ftInteger,3,0);
      AddAField('PLUNGE_3',ftInteger,2,0);
      AddAField('STRIKE_3',ftInteger,3,0);
      WriteCorrectHeader;
   end;
end;
{$EndIf}


procedure CreateTableForVATforDEM(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddAField('VALUE',ftInteger,8,0);
      AddAField('USE',ftString,1,0);
      AddAField('N',ftInteger,8,0);
      AddAField('NAME',ftString,120,0);
      AddAField('COLOR',ftInteger,8,0);
      AddAField('N_SUB',ftInteger,8,0);
      WriteCorrectHeader;
   end;
end;


procedure MakeSatelliteTable(fName : PathStr; NumBands : integer);
var
   I : integer;
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddLatLongToTable;
      for I := 1 to NumBands do AddAField('BAND_' + IntToStr(i),ftInteger,9);
      AddAField('CLUSTER',ftInteger,2,0);
      WriteCorrectHeader;
   end;
end;


procedure MakeBasicBoundingBoxTable(fName : PathStr; NameStr : shortstring = '');
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   if (NameStr = '') then NameStr := 'NAME';
   CreateDataBase.AddAField(NameStr,ftstring,35);
   CreateDataBase.AddBoundingBoxToTable;
   CreateDataBase.WriteCorrectHeader;
end;



procedure MakeNodeTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddLatLongToTable(true);
      AddAField('NODE_ID',ftInteger,10);
      AddAField('BASIN_ID',ftString,12);
     //IDs for the streams at the node
      AddAField('DOWN',ftString,15);
      AddAField('UP_MAIN',ftString,15);
      AddAField('UP_TRIB',ftString,15);
      AddAField('UP_TRIB2',ftString,15);
    //upstream contributing areas for the main channel and tributaries
      AddAField('CONT_MAIN',ftInteger,9);
      AddAField('CONT_TRIB',ftInteger,9);
      AddAField('CONT_TRIB2',ftInteger,9);
      AddAField('CONT_TRIB2',ftInteger,9);
    //orders for the streams at the node
      AddAField('UP_MAIN_O',ftInteger,3);
      AddAField('UP_TRIB_O',ftInteger,3);
      AddAField('UP_TRIB2_O',ftInteger,3);
      AddAField('DOWN_ORD',ftInteger,3);
      WriteCorrectHeader;
   end;
end;


procedure CreateThalwegTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddAField('BASIN_ID',ftString,10);
      AddAField('THALWEG_KM',ftFloat,12,2);
      AddAField('RELIEF_TH',ftFloat,8,1);
      AddAField('SINUOSITY',ftFloat,8,3);
      AddAField('STRAHLER_O',ftInteger,2);
      AddBoundingBoxToTable;
      WriteCorrectHeader;
   end;
end;


procedure MakeSF3Table(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddAField('SF3_CODE',ftString,12);
      AddAField('SHORT_NAME',ftString,10);
      AddAField('DESCRIPT',ftString,120);
      WriteCorrectHeader;
   end;
end;


procedure MakePhotoIndex(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
       AddLatLongToTable;
       AddAField('ALTITUDE',ftFloat,8,2);
       AddAField('AZIMUTH',ftFloat,6,2);
       AddAField('GPS_DATE',ftString,20);
       AddAField('CAMERA',ftString,16);
       AddAField('FOCAL_LEN',ftString,12);
       AddAField('FOCAL_35',ftString,7);
       AddAField('ORIENT',ftInteger,2);
       AddAField('ISO',ftString,8);
       AddAField('F_STOP',ftFloat,6,2);
       AddAField('SHUTTER',ftString,16);
       AddAField('EV100',ftFloat,5,1);
       AddAField('IMAGE',ftString,255);
       WriteCorrectHeader;
   end;
end;


procedure MakeRangeCircleTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddLatLongToTable;
      AddAField('NAME',ftString,24);
      AddAField('RANGE',ftFloat,12,2);
      AddAField('COLOR',ftInteger,12);
      AddAField('LINE_WIDTH',ftInteger,3);
      AddAField('USE',ftString,1);
      WriteCorrectHeader;
   end;
end;

procedure CreateDEMCorrelationTable(fname : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddLatLongToTable;
      AddAField('N_PTS',ftInteger,12);
      AddAField('R',ftFloat,8,4);
      AddAField('AVG_DIFF',ftFloat,8,2);
      AddAField('AVG_DEV',ftFloat,8,2);
      AddAField('AVG_SLOPE',ftFloat,8,2);
      WriteCorrectHeader;
   end;
end;

procedure CreateInflectionTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddLatLongToTable;
      AddAField('INFLECT_M',ftFloat,12,2);
      AddAField('HEIGHT_M',ftFloat,12,2);
      WriteCorrectHeader;
   end;
end;

procedure CreateXYADBTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddLatLongToTable;
      AddAField('AGE_MA',ftFloat,6,2);
      AddAField('DEPTH_BASE',ftFloat,9,1);
      AddAField('BATHY_M',ftFloat,9,1);
      WriteCorrectHeader;
   end;
end;


procedure MakeFresnelPointsTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddLatLongToTable;
      AddAField('NAME',ftString,36);
      AddAField('RANGE_KM',ftFloat,12,4);
      AddAField('SENSOR_LAT',ftFloat,11,7);
      AddAField('SENSOR_LON',ftFloat,12,7);
      AddAField('SENSOR_UP',ftFloat,6,2);
      AddAField('TARGET_UP',ftFloat,6,2);
      AddAField('FREQ_MHZ',ftFloat,8,1);
      AddAField('COLOR',ftInteger,9);
      WriteCorrectHeader;
   end;
end;


procedure MakeTopoProfileTable(fName : PathStr; FresnelFields,VegFields,VegFields2,PointCloudFields,PointCloudFields2,GrazingFields,PointCloudExtremes,VoxelFields,VoxelFields2 : boolean);
begin
   {$IfDef RecordMakeTables} WriteLineToDebugFile('MakeFresnelTable in: ' + fName); {$EndIf}
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
       AddLatLongToTable;
       AddAField('ELEV_M',ftFloat,12,4);
       AddAField('RANGE_KM',ftFloat,13,5);
       AddAField('CURV_M',ftFloat,8,2);
       AddAField('LOS_HT_M',ftFloat,8,2);
       AddAField('MASK_AIR',ftFloat,8,2);
       AddAField('BLOCK_TERR',ftString,1);

       if VegFields then begin
          AddAField('BLOCK_VEG',ftString,1);
          AddAField('VEG_HT',ftFloat,7,2);
       end;
       if VegFields2 then begin
          AddAField('BLOCK_VEG2',ftString,1);
          AddAField('VEG2_HT',ftFloat,7,2);
       end;

       if VoxelFields then begin
          AddAField('VPTS_AROUND',ftInteger,4);
          AddAField('VPTS_ABOVE',ftInteger,4);
       end;

       if VoxelFields2 then begin
          AddAField('VPT2_AROUND',ftInteger,4);
          AddAField('VPT2_ABOVE',ftInteger,4);
       end;

       if PointCloudFields then begin
          AddAField('PTS_AROUND',ftInteger,4);
          AddAField('PTS_ABOVE',ftInteger,4);
          if PointCloudExtremes then begin
             AddAField('MINZ_PTCLD',ftFloat,7,2);
             AddAField('MAXZ_PTCLD',ftFloat,7,2);
             AddAField('VEGZ_PTCLD',ftFloat,7,2);
          end;
          AddAField('COLOR_LAS',ftInteger,8);
       end;

       if PointCloudFields2 then begin
          AddAField('PTS2_AROUND',ftInteger,4);
          AddAField('PTS2_ABOVE',ftInteger,4);
          AddAField('COLOR_LAS2',ftInteger,8);
       end;

       if FresnelFields then begin
          AddAField('FRESNEL1_M',ftFloat,7,2);
          AddAField('FRESNEL2_M',ftFloat,7,2);
          AddAField('INTRUDE_PC',ftFloat,7,2);
          if VegFields then AddAField('INTR_VEG',ftFloat,7,2);
       end;

       if GrazingFields then begin
          AddAField('SLOPE_3D',ftFloat,7,2);
          AddAField('ASPECT',ftFloat,5,1);
          AddAField('PITCH',ftFloat,5,2);
          AddAField('GRAZING_2D',ftFloat,6,2);
          AddAField('GRAZING_3D',ftFloat,6,2);
       end;

       if GrazingFields or MDDef.ForceCrestComputations then begin
          AddAField('SLOPE_2D',ftFloat,7,2);
       end;

       AddAField('COLOR',ftInteger,8);
       if MDDef.ForceCrestComputations then begin
          AddAField('PEAK',ftString,1);
          AddAField('PIT',ftString,1);
       end;
       WriteCorrectHeader;
   end;
   {$IfDef RecordMakeTables} WriteLineToDebugFile('MakeFresnelTable out');   {$EndIf}
end;


procedure MakePickUseTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddAField('MENU_OPTS',ftString,164,0);
      AddAField('USE',ftString,1,0);
      WriteCorrectHeader;
   end;
end;

procedure MakeFieldRangeTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddAField('FIELD_NAME',ftString,12,0);
      AddAField('FIELD_MIN',ftFloat,18,6);
      AddAField('FIELD_MAX',ftFloat,18,6);
      WriteCorrectHeader;
   end;
end;


procedure MakeSensorTargetTable(fName : PathStr; AddDetails : boolean = false; StringLength : integer = 15);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddAField('SENSOR',ftString,StringLength,0);
      AddLatLongToTable;
      AddAField('TARGET',ftString,StringLength,0);
      AddLat2Long2ToTable;
      if AddDetails then begin
          AddAField('S_RANGE',ftFloat,8,1);
          AddAField('S_HEIGHT',ftFloat,8,1);
          AddAField('S_AZIMUTH',ftFloat,8,1);
          AddAField('S_VERT_ANG',ftFloat,8,1);
          AddAField('T_HEIGHT',ftFloat,8,1);
          AddAField('VISIBLE',ftString,1,0);
          AddAField('USE',ftString,1,0);
      end;
      WriteCorrectHeader;
   end;
end;


procedure CreateColorBreaksTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddAField('MAX',ftFloat,12,6);
      AddAField('RED',ftInteger,3,0);
      AddAField('GREEN',ftInteger,3,0);
      AddAField('BLUE',ftInteger,3,0);
      AddAField('MIN',ftFloat,12,6);
      WriteCorrectHeader;
   end;
end;


procedure CreateGridlegendTable(LegendDBFfName : PathStr; MaxLen : integer);
begin
   CreateDataBase := tCreateDataBase.Create(LegendDBFfName);
   with CreateDataBase do begin
      AddAField('ID',ftInteger,5,0);
      AddAField('NAME',ftString,Maxlen,0);
      AddAField('COLOR',ftInteger,8,0);
      AddAField('NPTS',ftInteger,10,0);
      WriteCorrectHeader;
   end;
end;


procedure MakeTwoStringTable(fName : PathStr; f1,f2 : ShortString; l1,l2 : integer);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   CreateDataBase.AddAField(f1,ftString,l1,0);
   CreateDataBase.AddAField(f2,ftString,l2,0);
   CreateDataBase.AddAField('N',ftInteger,10,0);
   CreateDataBase.WriteCorrectHeader;
end;


procedure MakePCTable(fName : PathStr; NumComponents : integer);
var
   i : integer;
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   CreateDataBase.AddLatLongToTable;
   for i := 1 to NumComponents do CreateDataBase.AddAField('PC' + IntToStr(i),ftFloat,12,4);
   CreateDataBase.WriteCorrectHeader;
end;



procedure CreateGeomorphometryAttributesTable(fname : PathStr; IncludeName : boolean);
begin
   {$IfDef RecordMakeTables} WriteLineToDebugFile('CreateGeomorphometryAtributesTable in'); {$EndIf}
   CreateDataBase := tCreateDataBase.Create(fName);
   CreateDataBase.AddLatLongToTable;
   CreateDataBase.AddRecNoToTable;
   if IncludeName then begin
      CreateDataBase.AddAField('AREA',ftString,24,0);
      CreateDataBase.AddAField('NAME',ftString,24,0);
   end;

   CreateDataBase.AddAField('NPTS',ftInteger,10,0);

   if MDDef.IncludeMissingHoles then begin
      CreateDataBase.AddAField('MISSING',ftFloat,6,2);
   end;

   if MDDef.IncludeBasicElevation then begin
      CreateDataBase.AddAField('ELEV_AVG',ftFloat,8,1);
      CreateDataBase.AddAField('ELEV_STD',ftFloat,8,2);
      CreateDataBase.AddAField('ELEV_SKW',ftFloat,8,4);
      CreateDataBase.AddAField('ELEV_KRT',ftFloat,8,4);
   end;
   if MDDef.IncludeAdvancedElevation then begin
      CreateDataBase.AddAField('RELIEF',ftFloat,6,1);
      CreateDataBase.AddAField('ELEV_RELF',ftFloat,8,4);

      CreateDataBase.AddAField('LAT_ZMAX',ftFloat,10,6);
      CreateDataBase.AddAField('LONG_ZMAX',ftFloat,11,6);
      CreateDataBase.AddAField('ELEV_MAX',ftFloat,8,1);

      CreateDataBase.AddAField('LAT_ZMIN',ftFloat,10,6);
      CreateDataBase.AddAField('LONG_ZMIN',ftFloat,11,6);
      CreateDataBase.AddAField('ELEV_MIN',ftFloat,8,1);

      CreateDataBase.AddAField('DIST_DROP',ftFloat,8,4);
      CreateDataBase.AddAField('AZ_DROP',ftFloat,5,1);
      CreateDataBase.AddAField('SLOPE_DROP',ftFloat,7,3);
   end;

   if MDDef.IncludeSlopeMeasures then begin
     CreateDataBase.AddAField('SLOPE_MAX',ftFloat,7,3);
     CreateDataBase.AddAField('SLOPE_AVG',ftFloat,7,3);
     CreateDataBase.AddAField('SLOPE_STD',ftFloat,7,3);
     CreateDataBase.AddAField('SLOPE_SKW',ftFloat,7,3);
     CreateDataBase.AddAField('SLOPE_KRT',ftFloat,7,3);
   end;

   if MDDef.IncludeFabricMeasures then begin
     CreateDataBase.AddAField('SSO_PTS',ftInteger,8);
     CreateDataBase.AddAField('S1S2',ftFloat,8,4);
     CreateDataBase.AddAField('S2S3',ftFloat,8,4);
     CreateDataBase.AddAField('FABRIC_DIR',ftFloat,5,1);
     CreateDataBase.AddAField('SHAPE',ftFloat,8,4);
     CreateDataBase.AddAField('STRENGTH',ftFloat,8,4);
     CreateDataBase.AddAField('ROUGHNESS',ftFloat,10,6);
     CreateDataBase.AddAField('ASPECT_PTS',ftInteger,8);
     CreateDataBase.AddAField('ASPECT_AVG',ftFloat,5,1);
     CreateDataBase.AddAField('ASPECT_STR',ftFloat,5,3);
   end;

   if MDDef.IncludeProfCMeasures then begin
      CreateDataBase.AddAField('PROFC_AVG',ftFloat,12,6);
      CreateDataBase.AddAField('PROFC_STD',ftFloat,12,6);
      CreateDataBase.AddAField('PROFC_SKW',ftFloat,12,6);
      CreateDataBase.AddAField('PROFC_KRT',ftFloat,12,6);
   end;

   if MDDef.IncludeProfCMeasures then begin
      CreateDataBase.AddAField('PLANC_AVG',ftFloat,12,6);
      CreateDataBase.AddAField('PLANC_STD',ftFloat,12,6);
      CreateDataBase.AddAField('PLANC_SKW',ftFloat,12,6);
      CreateDataBase.AddAField('PLANC_KRT',ftFloat,12,6);
   end;

   if MDDef.IncludeOpenness then begin
      CreateDataBase.AddAField('OPN_UP_AVG',ftFloat,6,2);
      CreateDataBase.AddAField('OPN_UP_STD',ftFloat,6,2);
      CreateDataBase.AddAField('OPN_UP_SKW',ftFloat,6,2);
      CreateDataBase.AddAField('OPN_UP_KRT',ftFloat,6,2);
      CreateDataBase.AddAField('OPN_DN_AVG',ftFloat,6,2);
      CreateDataBase.AddAField('OPN_DN_STD',ftFloat,6,2);
      CreateDataBase.AddAField('OPN_DN_SKW',ftFloat,6,2);
      CreateDataBase.AddAField('OPN_DN_KRT',ftFloat,6,2);
   end;
   (*
   if IncludeWaveLength then begin
      CreateDataBase.AddAField('LEN_MEAN',ftFloat,9,3);
      CreateDataBase.AddAField('LEN_MEDIAN',ftFloat,9,3);
      CreateDataBase.AddAField('LEN_STD',ftFloat,9,3);
      CreateDataBase.AddAField('HT_MEAN',ftFloat,8,2);
      CreateDataBase.AddAField('HT_MEDIAN',ftFloat,8,2);
      CreateDataBase.AddAField('HT_STD',ftFloat,8,2);
      CreateDataBase.AddAField('HT_STD2AV',ftFloat,8,2);
      CreateDataBase.AddAField('LEN_STD2AV',ftFloat,8,2);
   end;
   *)
   if MDDef.IncludeGammaMeasures then begin
      CreateDataBase.AddAField('GAMMA_EW',ftFloat,10,6);
      CreateDataBase.AddAField('GAMMA_NS',ftFloat,10,6);
      CreateDataBase.AddAField('GAMMA_NESW',ftFloat,10,6);
      CreateDataBase.AddAField('GAMMA_NWSE',ftFloat,10,6);
   end;

   if MDDef.IncludeFractalMeasures then begin
      CreateDataBase.AddAField('FFT_FR_NS',ftFloat,8,3);
      CreateDataBase.AddAField('FFT_FR_EW',ftFloat,8,3);
      CreateDataBase.AddAField('MEM_FR_NS',ftFloat,8,3);
      CreateDataBase.AddAField('MEM_FR_EW',ftFloat,8,3);
      CreateDataBase.AddAField('VAR_FR_NS',ftFloat,8,3);
      CreateDataBase.AddAField('VAR_FR_EW',ftFloat,8,3);
      CreateDataBase.AddAField('VAR_FR_NW',ftFloat,8,3);
      CreateDataBase.AddAField('VAR_FR_NE',ftFloat,8,3);
      CreateDataBase.AddAField('TRI_PR_FR',ftFloat,8,3);
      CreateDataBase.AddAField('FFT_FR_NS2',ftFloat,8,3);
      CreateDataBase.AddAField('FFT_FR_EW2',ftFloat,8,3);
      CreateDataBase.AddAField('MEM_FR_NS2',ftFloat,8,3);
      CreateDataBase.AddAField('MEM_FR_EW2',ftFloat,8,3);
      CreateDataBase.AddAField('TRI_PR_FR2',ftFloat,8,3);
   end;
   if MDDef.IncludeBasinID then begin
      CreateDataBase.AddAField('BASIN_ID',ftString,15,0);
      CreateDataBase.AddAField('COLOR',ftInteger,8,0);
      CreateDataBase.AddAField('PERIMETR_KM',ftFloat,10,2);
   end;

   if MDDef.IncludeBasinID then begin
      CreateDataBase.AddAField('SLP_OV_30',ftFloat,6,2);
      CreateDataBase.AddAField('SLP_OV_50',ftFloat,6,2);
      CreateDataBase.AddAField('BASIN_RUGD',ftFloat,8,3);
      CreateDataBase.AddAField('RELIEF_RAT',ftFloat,8,3);
      CreateDataBase.AddAField('AREA_SQKM',ftFloat,10,1);
      CreateDataBase.AddAField('THALWEG_KM',ftFloat,8,1);
      CreateDataBase.AddAField('CHANNEL_KM',ftFloat,8,1);
      CreateDataBase.AddAField('STRAHLER_O',ftInteger,2,0);
      CreateDataBase.AddAField('BSN_NODES',ftInteger,8,0);
      CreateDataBase.AddAField('BSN_SEGS',ftInteger,8,0);
   end;

   CreateDataBase.WriteCorrectHeader;
   {$IfDef RecordMakeTables} WriteLineToDebugFile('CreateGeomorphometryAtributesTable out'); {$EndIf}
end;



procedure CreatePointFabricFile(fname : PathStr; DoWaveLength : boolean);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   CreateDataBase.AddLatLongToTable;
   CreateDataBase.AddAField('S1S2',ftFloat,8,2);
   CreateDataBase.AddAField('S2S3',ftFloat,8,2);
   CreateDataBase.AddAField('FABRIC_DIR',ftFloat,8,2);
   CreateDataBase.AddAField('FABRIC_AMP',ftFloat,8,2);
   CreateDataBase.AddAField('NPTS',ftInteger,6);
   CreateDataBase.AddAField('RELIEF',ftfloat,8,2);
   if DoWaveLength then begin
       CreateDataBase.AddAField('LEN_MEAN',ftFloat,9,3);
       CreateDataBase.AddAField('LEN_MEDIAN',ftFloat,9,3);
       CreateDataBase.AddAField('LEN_STD',ftFloat,9,3);
       CreateDataBase.AddAField('HT_MEAN',ftFloat,8,2);
       CreateDataBase.AddAField('HT_MEDIAN',ftFloat,8,2);
       CreateDataBase.AddAField('HT_STD',ftFloat,8,2);
       CreateDataBase.AddAField('HT_STD2AV',ftFloat,8,2);
       CreateDataBase.AddAField('LEN_STD2AV',ftFloat,8,2);
   end;
   CreateDataBase.WriteCorrectHeader;
end;


procedure MakeRedistrictingTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   CreateDataBase.AddAField('DISTRICT',ftString,12,0);
   CreateDataBase.AddAField('BLOCKS',ftInteger,9,0);
   CreateDataBase.AddAField('POP',ftInteger,9,0);
   CreateDataBase.AddAField('GOAL',ftFloat,6,2);
   CreateDataBase.AddAField('WHITE_PC',ftFloat,6,2);
   CreateDataBase.AddAField('BLACK_PC',ftFloat,6,2);
   CreateDataBase.AddAField('HISPAN_PC',ftFloat,6,2);
   CreateDataBase.AddAField('WHITE1',ftInteger,9,0);
   CreateDataBase.AddAField('BLACK1',ftInteger,9,0);
   CreateDataBase.AddAField('HISPANIC',ftInteger,9,0);
   CreateDataBase.WriteCorrectHeader;
end;

procedure MakeProfileIndexTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   CreateDataBase.AddAField('XY_FILE',ftString,24,0);   //full path relative to the index file
   CreateDataBase.AddAField('GOOD_PROF',ftString,4,0);
   CreateDataBase.AddAField('YEAR',ftInteger,4,0);
   CreateDataBase.AddAField('MONTH',ftInteger,2,0);
   CreateDataBase.AddAField('DAY',ftInteger,2,0);
   CreateDataBase.AddAField('PROFILE',ftString,8,0);
   CreateDataBase.AddAField('X_OFFSET',ftFloat,8,2);
   CreateDataBase.AddAField('Y_OFFSET',ftFloat,8,2);
   CreateDataBase.AddAField('LINE_WIDTH',ftInteger,2,0);
   CreateDataBase.AddAField('COLOR',ftInteger,8,0);
   CreateDataBase.WriteCorrectHeader;
end;


{$IfDef ExHydrography}
{$Else}


procedure MakeSideScanTargetFile(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddLatLongToTable(true);
      CreateDataBase.AddAField('PING',ftInteger,6,0);
      CreateDataBase.AddAField('FILENAME',ftString,120,0);
      CreateDataBase.AddAField('NOTES',ftString,255,0);
      WriteCorrectHeader;
   end;
end;


procedure MakeSideScanRecordFile(fName : PathStr; Long : boolean = true);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   CreateDataBase.AddLatLongToTable(true);
   CreateDataBase.AddAField('PING',ftInteger,9);
   CreateDataBase.AddAField('TIME',ftString,12);
   CreateDataBase.AddAField('DEPTH',ftFloat,6,2);
   CreateDataBase.AddAField('SLANT_RNG',ftInteger,3);
   if Long then begin
      CreateDataBase.AddAField('SPEED',ftFloat,6,2);
      CreateDataBase.AddAField('HEADING',ftFloat,6,1);
      CreateDataBase.AddAField('FISH_HEAD',ftFloat,6,1);
      CreateDataBase.AddAField('FISH_DEPTH',ftFloat,6,1);
      CreateDataBase.AddAField('WATER_TEMP',ftFloat,6,2);
      CreateDataBase.AddAField('PRESSURE',ftFloat,6,2);
      CreateDataBase.AddAField('FISH_PITCH',ftFloat,6,2);
      CreateDataBase.AddAField('FISH_ALT',ftFloat,6,2);
      CreateDataBase.AddAField('FISH_ROLL',ftFloat,6,2);
   end;
   CreateDataBase.AddRecNoToTable;
   CreateDataBase.WriteCorrectHeader;
end;

procedure MakeHydrographyIndex(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   CreateDataBase.AddAField('FILENAME',ftString,120,0);
   CreateDataBase.AddAField('CHIRP',ftInteger,8,0);
   CreateDataBase.AddBoundingBoxToTable;
   CreateDataBase.WriteCorrectHeader;
end;


procedure MakeSideScanIndex(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   CreateDataBase.AddAField('FILENAME',ftString,120,0);
   CreateDataBase.AddLatLongToTable(true);
   CreateDataBase.AddAField('LAT2',ftFloat,12,8);
   CreateDataBase.AddAField('LONG2',ftFloat,13,8);
   CreateDataBase.AddAField('RANGE',ftFloat,6,2);
   CreateDataBase.AddAField('HEADING',ftFloat,6,1);
   CreateDataBase.AddAField('LENGTH',ftFloat,6,1);
   CreateDataBase.AddAField('PINGS',ftInteger,6,0);
   CreateDataBase.WriteCorrectHeader;
end;

{$EndIf}


procedure CreateMilIconsTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   CreateDataBase.AddAField('UNITNAME',ftString,18);
   CreateDataBase.AddAField('PARENT',ftString,18);
   CreateDataBase.AddAField('TOP_TEXT',ftString,18);
   CreateDataBase.AddAField('BTM_TEXT',ftString,18);
   CreateDataBase.AddAField('MILSYMFONT',ftString,48);
   CreateDataBase.AddAField('TEXTFONT',ftString,48);
   CreateDataBase.AddAField('UNITLEVEL',ftInteger,2);
   CreateDataBase.AddAField('FILLLEVEL',ftInteger,2);
   CreateDataBase.AddAField('SYMFONTS',ftInteger,2);
   CreateDataBase.AddAField('TEXFONTS',ftInteger,2);
   CreateDataBase.AddAField('CHARNUM',ftInteger,3);
   CreateDataBase.AddAField('TEXTBOLD',ftInteger,1);
   CreateDataBase.AddAField('FILLCOLOR',ftInteger,10);
   CreateDataBase.AddAField('TEXTCOLOR',ftInteger,10);
   CreateDataBase.AddAField('FONTCOLOR',ftInteger,10);
   CreateDataBase.AddAField('ICON',ftString,100);
   CreateDataBase.WriteCorrectHeader;
end;


procedure CreateFileNameIndexTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   //with CreateDataBase do begin
     CreateDataBase.AddAField('FILENAME',ftString,255);   //full path name
     CreateDataBase.AddBoundingBoxToTable;                  //bounding box corners
     CreateDataBase.WriteCorrectHeader;
   //end;
end;



procedure CreateTriangleTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   //with CreateDataBase do begin
     CreateDataBase.AddAField('VERTEX_1',ftInteger,6,0);
     CreateDataBase.AddAField('VERTEX_2',ftInteger,6,0);
     CreateDataBase.AddAField('VERTEX_3',ftInteger,6,0);
     CreateDataBase.AddAField('SEGMENT',ftInteger,4,0);
     CreateDataBase.WriteCorrectHeader;
   //end;
end;

procedure CreateXYZTable(fName : PathStr; RecSeg : boolean);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   //with CreateDataBase do begin
      CreateDataBase.AddAField('X',ftFloat,18,6);
      CreateDataBase.AddAField('Y',ftFloat,18,6);
      CreateDataBase.AddAField('Z',ftFloat,18,6);
      CreateDataBase.AddRecNoToTable;
      if RecSeg then begin
         CreateDataBase.AddAField('SEGMENT',ftInteger,4,0);
      end;
      CreateDataBase.WriteCorrectHeader;
   //end;
end;


procedure CreateUTM_ZTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   //with CreateDataBase do begin
      CreateDataBase.AddUTMtoTable;
      CreateDataBase.AddAField('Z',ftFloat,8,2);
      CreateDataBase.WriteCorrectHeader;
   //end;
end;


procedure Create_ZTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   CreateDataBase.AddAField('Z',ftFloat,8,2);
   CreateDataBase.WriteCorrectHeader;
end;


procedure CreateLatLongZTable(fName : PathStr; IncludeZ : boolean = true; IncludeColor : boolean = false;
   IncludeAltitude : boolean = false; IncludeAzimuth : boolean = false; IncludeDistance : boolean = false;
   IncludeSource : boolean = false; IncludePitch : boolean = false);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
     AddLatLongToTable;
     if IncludeColor then AddRedGreenBlueFieldsToTable;
     if IncludeAltitude then AddAField('ALTITUDE',ftFloat,8,2);
     if IncludeZ then AddAField('Z',ftFloat,8,2);
     if IncludeAzimuth then AddAField('AZIMUTH',ftFloat,6,2);
     if IncludeDistance then AddAField('DISTANCE',ftFloat,10,1);
     if IncludeSource then AddAField('SOURCE',ftString,35,1);
     if IncludePitch then AddAField('PITCH',ftFloat,5,1);
     WriteCorrectHeader;
   end;
end;


procedure MakeDummyRecordTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddAField('RECORD',ftInteger,8,0);
      WriteCorrectHeader;
   end;
end;

procedure MakeNamePlotTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddAField('NAME',ftString,35,0);
      AddLineDefToTable;
      AddAField('PLOT',ftString,1,0);
      AddBoundingBoxToTable;
      WriteCorrectHeader;
   end;
end;


procedure CreateContourLineTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddAField('CONTOUR',ftInteger,5,0);
      AddAField('X1',ftInteger,5,0);
      AddAField('Y1',ftInteger,5,0);
      AddAField('X2',ftInteger,5,0);
      AddAField('Y2',ftInteger,5,0);
      WriteCorrectHeader;
   end;
end;


procedure CreateWindComponentTable(fName : PathStr);
var
   i : integer;
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddLatLongToTable;
      for i := 1 to 12 do begin
         AddAField(UpperCase(PETMAR_types.MonthName[i]) + '_U_MS',ftFloat,8,4);
         AddAField(UpperCase(PETMAR_types.MonthName[i]) + '_V_MS',ftFloat,8,4);
      end;
      WriteCorrectHeader;
   end;
end;


procedure CreateDataUseTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddAField('DATA_TYPE',ftString,12,0);
      AddAField('USE',ftString,1,0);
      WriteCorrectHeader;
   end;
end;

procedure CreateFabricRegion(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddLatLongToTable;
      AddAField('REGION_M',ftInteger,8,0);
      AddAField('ORGANIZE',ftFloat,8,4);
      AddAField('FLATNESS',ftFloat,8,4);
      AddAField('FABRIC_DIR',ftFloat,6,2);
      AddAField('RELIEF',ftFloat,9,3);
      WriteCorrectHeader;
   end;
end;


procedure CreateLatLongTimeFile(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddLatLongToTable;
      AddAField('DTG',ftString,24);
      AddAField('POLARITY',ftString,1);
      AddAField('VIS',ftString,1);
      AddAField('DIST_MILES',ftFloat,10,2);
      AddAField('AZ_DMS',ftString,12);
      AddAField('PITCH_DMS',ftString,12);
      AddAField('DIST_KM',ftFloat,10,2);
      AddAField('AZIMUTH',ftFloat,5,1);
      AddAField('PITCH',ftFloat,6,2);
      WriteCorrectHeader;
   end;
end;

procedure MakeICOADSTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddLatLongToTable;
      AddAField('YEAR',ftInteger,4,0);
      AddAField('MONTH',ftInteger,2,0);
      AddAField('DAY',ftInteger,2,0);
      AddAField('SST',ftFloat,6,1);
      AddAField('WIND_SPEED',ftFloat,6,1);
      AddAField('WIND_DIR',ftInteger,3,0);
      AddAField('WAVE_HT',ftInteger,3,0);
      AddAField('SWELL_HT',ftInteger,3,0);
      WriteCorrectHeader;
   end;
end;

procedure CreateOptimalRegistrationTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddAField('AVG_ERR',ftFloat,8,1);
      AddAField('MAX_ERR',ftFloat,8,1);
      AddAField('POINT_1',ftString,35,0);
      AddAField('POINT_2',ftString,35,0);
      AddAField('POINT_3',ftString,35,0);
      WriteCorrectHeader;
   end;
end;

procedure CreateTigerRulesDBF(fName : PathStr);
begin
    if FileExists(fName) then SysUtils.DeleteFile(fName);
    CreateDataBase := tCreateDataBase.Create(fName);
    with CreateDataBase do begin
       AddAField('CAPTION', ftString,35, 0);
       AddAField('PLOT', ftString,1, 0);
       AddAField('PIXEL_SIZE', ftInteger,8, 0);
       AddAField('FILTER', ftString,24, 0);
       AddLineDefToTable;
       WriteCorrectHeader;
    end;
end;


procedure CreateImageRegistrationTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   CreateDataBase.AddAField('POINT',ftString,35,0);
   CreateDataBase.AddLatLongToTable;
   CreateDataBase.AddAField('X_IMAGE',ftInteger,8,0);
   CreateDataBase.AddAField('Y_IMAGE',ftInteger,8,0);
   CreateDataBase.AddAField('X_UTM',ftFloat,12,2);
   CreateDataBase.AddAField('Y_UTM',ftFloat,12,2);
   CreateDataBase.AddAField('X_REF_IM',ftInteger,8,0);
   CreateDataBase.AddAField('Y_REF_IM',ftInteger,8,0);
   CreateDataBase.AddRecNoToTable;
   CreateDataBase.WriteCorrectHeader;
end;


procedure CreateThalwegFile(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   CreateDataBase.AddAField('DIST_KM',ftFloat,10,4);
   CreateDataBase.AddAField('ELEV_M',ftFloat,5,1);
   CreateDataBase.AddLatLongToTable;
   CreateDataBase.WriteCorrectHeader;
end;


procedure CreateIndexSeriesFile(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
      AddAField('DATA_TYPE',ftString,15,0);
      AddAField('SERIES',ftString,MaxSeriesNameLength,0);
      AddAField('USE',ftString,1,0);
      AddAField('NUM_FILES',ftInteger,8,0);
      AddAField('COLOR',ftInteger,8,0);
      AddAField('PIXEL_IS',ftInteger,2);
      AddAField('VERT_DATUM',ftString,20);
      AddBoundingBoxToTable;
      WriteCorrectHeader;
   end;
end;

procedure CreateIndexSymbologyTable(fName : PathStr; Long : boolean; ShapeType : integer = 0);
begin
    if FileExists(fName) then SysUtils.DeleteFile(fName);
    CreateDataBase := tCreateDataBase.Create(fName);
    with CreateDataBase do begin
      AddAField('NAME', ftString,35);
      AddAField('PLOT', ftString,1);
      AddAField('LABEL', ftString,1);
      AddAField('PLOT_ORDER', ftInteger,1);
      AddAField('PIXEL_SIZE', ftInteger,8);
      if Long then AddAField('FILENAME', ftString,120);
      AddAField('FILTER', ftString,120);
      AddAField('FIELD_NAME', ftString,12);
      if ShapeType in [0] then AddAField('SHAPE_TYPE', ftInteger,1);

      if ShapeType in [0,1,11] then AddPointSymbolDefToTable;
      if ShapeType in [0,3,13,5,15] then AddLineDefToTable;
      if ShapeType in [0,5,15] then begin
         AddAField('FILL_COLOR',ftInteger,12);
         AddAField('FILL_PAT',ftInteger,2);
         AddAField('ALT_FILL', ftString,12);
      end;

      AddAField('POINTSYM', ftString,12);
      AddAField('LAB_FIELD', ftString,10);
      AddFontDefToTable;
      WriteCorrectHeader;
   end;
end;


procedure CreateIntegratedDataBaseTable(fName : PathStr);
begin
   CreateDataBase := tCreateDataBase.Create(fName);
   with CreateDataBase do begin
       AddAField('FILENAME',ftString,120,0);  //full path name
       AddAField('SERIES',ftString,MaxSeriesNameLength,0);     //data type (DOQQ, DRG, 24KDEM, etc.)
       AddBoundingBoxToTable;                      //bounding box corners
       WriteCorrectHeader;
   end;
end;


procedure MakeWeaponsFanTable(fName : PathStr);
begin
   {$IfDef RecordMakeTables} WriteLineToDebugFile('MakeWeaponsFanTable: ' + fName); {$EndIf}
   if Not FileExists(fName) then begin
      CreateTableOfType(fName,'WeaponsFan');
      {$IfDef RecordMakeTables} WriteLineToDebugFile('  did not exist, now created'); {$EndIf}
   end;
end;


initialization
finalization
   {$IfDef RecordMakeTables} WriteLineToDebugFile('RecordMakeTables Active in make_table'); {$EndIf}
end.


