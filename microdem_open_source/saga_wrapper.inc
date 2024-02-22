{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}
{                                   }
{   include file for md_use_tools   }
{___________________________________}


//  https://sourceforge.net/p/saga-gis/wiki/Executing%20Modules%20with%20SAGA%20CMD/#:~:text=SAGA%20CMD%20is%20a%20command,application%20like%20a%20web%20server.

(*
 Algorithm 'Sink Removal' starting…
Input parameters:
{ 'DEM' : 'C:/temp/canyon_range/canyon_range_COP.tif', 'DEM_PREPROC' : 'TEMPORARY_OUTPUT', 'METHOD' : 1, 'SINKROUTE' : None, 'THRESHOLD' : False, 'THRSHEIGHT' : 100 }

ta_preprocessor "Sink Removal" -DEM "C:/Users/pguth/AppData/Local/Temp/processing_rDPKFj/f38374e4abec45d0b2951b2280afff43/canyonrangeCOP.sgrd" -METHOD 1 -THRESHOLD false -THRSHEIGHT 100.0 -DEM_PREPROC "C:/Users/pguth/AppData/Local/Temp/processing_rDPKFj/5e3826c0da1d4b1c994adef706c2d79a/DEM_PREPROC.sdat"



Algorithm 'Channel Network' starting…
Input parameters:
{ 'CHNLNTWRK' : 'TEMPORARY_OUTPUT', 'CHNLROUTE' : 'TEMPORARY_OUTPUT', 'DIV_CELLS' : 5, 'DIV_GRID' : None, 'ELEVATION' : 'C:/temp/canyon_range/canyon_range_COP.tif', 'INIT_GRID' : 'C:/temp/canyon_range/canyon_range_COP.tif', 'INIT_METHOD' : 2, 'INIT_VALUE' : 0, 'MINLEN' : 10, 'SHAPES' : 'TEMPORARY_OUTPUT', 'SINKROUTE' : None, 'TRACE_WEIGHT' : None }

ta_channels "Channel Network" -ELEVATION "C:/Users/pguth/AppData/Local/Temp/processing_rDPKFj/f38374e4abec45d0b2951b2280afff43/canyonrangeCOP.sgrd"
  -INIT_GRID "C:/Users/pguth/AppData/Local/Temp/processing_rDPKFj/f38374e4abec45d0b2951b2280afff43/canyonrangeCOP.sgrd"
  -INIT_METHOD 2 -INIT_VALUE 0.0 -DIV_CELLS 5 -MINLEN 10
  -CHNLNTWRK "C:/Users/pguth/AppData/Local/Temp/processing_rDPKFj/78fcdc4180ba4343beafaf3dc5267ff2/CHNLNTWRK.sdat" -CHNLROUTE "C:/Users/pguth/AppData/Local/Temp/processing_rDPKFj/b8eeb6cc8a244aef9058514ad6e4b7ae/CHNLROUTE.sdat" -SHAPES "C:/Users/pguth/AppData/Local/Temp/processing_rDPKFj/578df69586ac4544b9e5384bd8e7650e/SHAPES.shp"
*)


 function IsSagaCMDthere : boolean;
 begin
    Result := FileExists(MDDef.SagaCMD);
    if (not Result) then begin
       FindDriveWithFile(MDDef.SagaCMD);
       Result := FileExists(MDDef.SagaCMD);
       if (not Result) then begin
          Result := GetExistingFileName('saga_cmd.exe','*.exe',MDDef.SagaCMD);
       end;
       if Result then SaveMDdefaults;
    end;
 end;


 function SAGAMap(cmd : shortstring; OutName : shortstring; ZUnits : tElevUnit; OpenMap : boolean = true) : integer;
 begin
    if IsSagaCMDthere then begin
       {$IfDef RecordSAGA} WriteLineToDebugFile('SAGAMap in, cmd= ' + CMD); {$EndIf}
       WinExecAndWait32(cmd,true,MDDef.LogDOScommands);
       if (OutName <> '') then begin
            {$IfDef RecordSAGA} WriteLineToDebugFile('SAGAMap opening map'); {$EndIf}
            Result := OpenNewDEM(OutName,OpenMap);
            if ValidDEM(Result) and OpenMap then begin
               DEMGlb[Result].DEMheader.ElevUnits := zUnits;
               CreateDEMSelectionMap(Result,true,true,mtElevSpectrum);
            end;
       end;
       {$IfDef RecordSAGA} WriteLineToDebugFile('SAGAMap out, grid=' + IntToStr(Result)); {$EndIf}
    end
    else Result := 0;
 end;


function SAGA_LSFactor(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
//  https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_hydrology_22.html
//  area from wetness index, https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_hydrology_15.html

//  slope in radians,  https://gis.stackexchange.com/questions/313659/ls-factor-in-saga-slope-units
var
    cmd : shortstring;
    SlopeName,AreaName,TWIName : PathStr;
begin
   {$IfDef RecordSAGA} WriteLineToDebugFile('SAGA_LSFactor in, ' + InName); {$EndIf}
   if (OutName = '') then OutName := MDTempDir + 'saga_ls_factor_' + ExtractFileNameNoExt(InName) + '.tif';
   AreaName := MDTempDir + 'saga_catch_area_' + ExtractFileNameNoExt(InName) + '.tif';
   TWIName := MDTempDir + 'saga_twi_' + ExtractFileNameNoExt(InName) + '.tif';
   SlopeName := MDTempDir + 'saga_slope_rad_' + ExtractFileNameNoExt(InName) + '.tif';

   cmd := MDDef.SagaCMD + ' ta_hydrology 15 -DEM ' + InName + ' -AREA ' + AreaName + ' TWI ' + TWIName;
   {$IfDef RecordSAGA} WriteLineToDebugFile('LS_factor, ' + cmd); {$EndIf}
   WinExecAndWait32(cmd,true,MDDef.LogDOScommands);

   cmd := MDDef.SagaCMD + ' ta_morphometry 0 -ELEVATION ' + InName + ' -SLOPE ' + SlopeName;
   {$IfDef RecordSAGA} WriteLineToDebugFile('LS_factor, ' + cmd); {$EndIf}
   WinExecAndWait32(cmd,true,MDDef.LogDOScommands);

   {$IfDef RecordSAGA} WriteLineToDebugFile('LS_factor, ' + cmd); {$EndIf}
   CMD := MDDef.SagaCMD + ' ta_hydrology 22' +
         ' -SLOPE ' + SlopeName +
         ' -AREA ' + AreaName +
         ' -LS ' + OutName;
   Result := SAGAMap(cmd,OutName,Undefined,OpenMap);
   {$If Defined(RecordSAGA) or Defined(RecordSAGAresult)} WriteLineToDebugFile(ExtractFileNameNoExt(InName) + '  SAGA_LSFactor out=' + IntToStr(Result)); {$EndIf}
end;



function SAGA_FlowAccumulationParallizeable(InName : PathStr; OutName : PathStr = '') : integer;
//https://saga-gis.sourceforge.io/saga_tool_doc/9.3.1/ta_hydrology_29.html
var
    cmd : shortstring;
begin
   if (OutName = '') then OutName := MDTempDir + 'saga_flow_accumulation_' + ExtractFileNameNoExt(InName) + '.tif';
   CMD := MDDef.SagaCMD + ' ta_hydrology 29' +
         ' -DEM ' + inName +
         ' -FLOW ' + OutName;
   Result := SAGAMap(cmd,OutName,Undefined,true);
end;

(*
Usage: saga_cmd ta_hydrology 29 [-DEM ] [-FLOW ] [-UPDATE ] [-METHOD ] [-CONVERGENCE ]
  -DEM:           	Elevation
	grid, input
  -FLOW:          	Flow Accumulation
	grid, output
  -UPDATE:        	Update Frequency
	integer number
	Minimum: 0
	Default: 0
  -METHOD:        	Method
	choice
	Available Choices:
	[0] D8
	[1] Dinfinity
	[2] MFD
	Default: 2
  -CONVERGENCE:	Convergence
	floating point number
	Minimum: 0.000000
	Default: 1.100000
*)

 function SAGA_WatershedBasinsWangLiu(InName : PathStr) : integer;
// https://saga-gis.sourceforge.io/saga_tool_doc/8.3.0/ta_preprocessor_4.html
 var
    cmd : shortstring;
    FilledGrid,FlowDirections,WaterShedBasins : PathStr;
 begin
    Result := 0;
    if IsSagaCMDthere and FileExists(InName) then begin
       {$IfDef RecordSAGAFull} WriteLineToDebugFile('SagaWatershedBasinsWangLiu in, InName= ' + InName); {$EndIf}
       FilledGrid      := MDTempDir + 'saga_wang_liu_filled_grid_' + ExtractFileNameNoExt(InName) + '.tif';
       FlowDirections  := MDTempDir + 'saga_wang_liu_flow_dir_' + ExtractFileNameNoExt(InName) + '.tif';
       WaterShedBasins := MDTempDir + 'saga_wang_liu_watershed_' + ExtractFileNameNoExt(InName) + '.tif';
       //if not FileExists(ChannelName) then begin
          CMD := MDDef.SagaCMD + ' ta_preprocessor 4' +
               ' -ELEV ' + inName +
               ' -FILLED ' + FilledGrid +
               ' -FDIR ' + FlowDirections +
               ' -WSHED ' + WaterShedBasins;
          WinExecAndWait32(cmd,true,MDDef.LogDOScommands);
          {$IfDef RecordSAGAFull} WriteLineToDebugFile('SagaWatershedBasinsWangLiu out'); {$EndIf}
       //end;
       //Result := SAGAMap(cmd,InName,'',Undefined,false);
    end;
 end;


function SAGAedgeContaminationMap(InName : PathStr; OutName : PathStr = '') : integer;
//https://saga-gis.sourceforge.io/saga_tool_doc/2.1.3/ta_hydrology_13.html
//may not be useful, 2/1/2024
var
    cmd : shortstring;
begin
    if (OutName = '') then OutName := MDTempDir + 'saga_channel_' + ExtractFileNameNoExt(InName) + '.tif';
    CMD := MDDef.SagaCMD + ' ta_hydrology 13 -ELEVATION ' + inName + ' -CONTAMINATION ' + OutName;
    Result := SAGAMap(cmd,{InName,}OutName,Undefined,true);
end;


function SAGA_StrahlerOrderGrid(InName : PathStr; OutName : PathStr = '') : integer;
(*
saga_cmd ta_channels 6 [-DEM <str>] [-STRAHLER <str>]
  -DEM:<str>     	Elevation
	Grid, input
  -STRAHLER:<str>	Strahler Order
	Grid, output
*)
var
    cmd : shortstring;
begin
    if (OutName = '') then OutName := MDTempDir + 'saga_strahler_order_' + ExtractFileNameNoExt(InName) + '.tif';
    CMD := MDDef.SagaCMD + ' ta_channels 6 -DEM ' + inName + ' -STRAHLER ' + OutName;
    Result := SAGAMap(cmd,{InName,}OutName,Undefined,true);
end;


    function SagaChannelShapefile(InName : PathStr; ChannelName : PathStr = '') : integer;
    var
       cmd : shortstring;
    begin
       Result := 0;
       if IsSagaCMDthere and FileExists(InName) then begin
         {$IfDef RecordSAGAFull} WriteLineToDebugFile('SagaChannelShapefile in, InName= ' + InName); {$EndIf}
          if (ChannelName = '') then ChannelName := MDTempDir + 'saga_channel_' + ExtractFileNameNoExt(InName) + '.shp';
          if not FileExists(ChannelName) then begin
             CMD := MDDef.SagaCMD + ' ta_channels 0' +
                  ' -ELEVATION ' + inName +
                  ' -INIT_GRID ' + inName +
                  ' -CHNLNTWRK ' + MDTempDir + 'saga_channel_network_' + ExtractFileNameNoExt(InName) + '.tif' +
                  ' -CHNLROUTE ' + MDTempDir + 'saga_channel_route_' + ExtractFileNameNoExt(InName) + '.tif' +
                  ' -SHAPES ' + ChannelName;
             WinExecAndWait32(cmd,true,MDDef.LogDOScommands);
             {$IfDef RecordSAGAFull} WriteLineToDebugFile('SagaChannelShapefile in, ChannelName= ' + ChannelName); {$EndIf}
          end;
          //Result := SAGAMap(cmd,InName,'',Undefined,false);
       end;
    end;


function SagaWatershedBasins(InName : PathStr; BasinGrid : PathStr = ''; ChannelNetwork : PathStr = ''; OutName : PathStr = '') : integer;
//  https://hydrology.usu.edu/taudem/taudem5/help53/StreamReachAndWatershed.html
   // issues with display of the grids in both QGIS and MD
   // shapefiles work, but the basins produces way to many basins
//  https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_channels_1.html  --attempts to use this led to problems with the CHANNELS parameter, 1/31/2024
 var
    cmd : shortstring;
    OrderName,BasinShape,SegmentName : PathStr;
 begin
    Result := 0;
    if IsSagaCMDthere and FileExists(InName) then begin
       SagaChannelShapefile(InName);
       if (BasinGrid = '') then BasinGrid := MDTempDir + 'saga_basins_' + ExtractFileNameNoExt(InName) + '.tif';
       OrderName := MDTempDir + 'saga_strahler_order_' + ExtractFileNameNoExt(InName) + '.tif';
       BasinShape := MDTempDir + 'saga_basins_' + ExtractFileNameNoExt(InName) + '.shp';
       SegmentName := MDTempDir + 'saga_segments_' + ExtractFileNameNoExt(InName) + '.shp';

       CMD := MDDef.SagaCMD + ' ta_channels 5' +
            ' -DEM ' + inName +
            ' -ORDER ' + OrderName +
            ' -BASIN ' + BasinGrid +
            ' -SEGMENT ' + BasinShape +
            ' -BASINS ' + BasinShape;
       WinExecAndWait32(cmd,true,MDDef.LogDOScommands);
       {$IfDef RecordSAGAFull} WriteLineToDebugFile('SagaChannelShapefile in, ChannelName= ' + ChannelName); {$EndIf}
    end;
    //Result := SAGAMap(cmd,InName,'',Undefined,false);
 end;


    function SagaChannelNetwork(InName : PathStr; OutName : PathStr = ''; ShpName : PathStr = '') : integer;
    var
       cmd : shortstring;
    begin
       if (OutName = '') then OutName := MDTempDir + 'saga_channel_' + ExtractFileNameNoExt(InName) + '.tif';
       if (ShpName = '') then ShpName := MDTempDir + 'saga_channel_' + ExtractFileNameNoExt(InName) + '.shp';
       CMD := MDDef.SagaCMD + ' ta_channels "Channel Network" -ELEVATION ' + inName + ' -INIT_GRID ' + inName + ' -CHNLNTWRK ' + OutName;
       Result := SAGAMap(cmd,{InName,}OutName,Undefined,false);
       Result := 0;
    end;


    function SagaSinkRemoval(InName: PathStr; OutName : PathStr = '') : integer;
    var
       cmd : shortstring;
    begin
       if IsSagaCMDthere then begin
          {$IfDef RecordSAGA} WriteLineToDebugFile('SagaSinkRemoval in, InName= ' + InName); {$EndIf}
          if (OutName = '') then OutName := MDTempDir + 'saga_sinks_removed_' + ExtractFileNameNoExt(InName) + '.tif';
          CMD := MDDef.SagaCMD + ' ta_preprocessor "Sink Removal" -DEM ' + inName + ' -DEM_PREPROC ' + OutName;
          //TemporaryNewGeotiff := false;
          Result := SAGAMap(cmd,{InName,}outname,euMeters,false);
          {$IfDef RecordSAGA} WriteLineToDebugFile('SagaSinkRemoval out'); {$EndIf}
       end
       else Result := 0;;
    end;


    procedure SAGA_all_DEMs_remove_sinks;
    var
       i : integer;
       InName,OutName : PathStr;
    begin
        {$IfDef RecordSAGA} WriteLineToDebugFile('SagaSinkRemoval in, InName= ' + InName); {$EndIf}
        if IsSagaCMDthere then begin
           for i := 1 to MaxDEMDataSets do begin
             if ValidDEM(i) then begin
                InName := DEMGlb[i].SelectionMap.GeotiffDEMNameOfMap;
                OutName := ChangeFileExt(InName,'_no_sink.tif');
                SagaSinkRemoval(InName,OutName);
                GDALConvertImagesToGeotiff(OutName,true);
             end;
          end;
        end;
    end;


    function SagaTRIMap(InName : PathStr) : integer;
    var
       cmd : shortstring;
       OutName : shortstring;
    begin
       OutName := MDTempDir + 'saga_tri_' + ExtractFileNameNoExt(InName) + '.tif';
       CMD := MDDef.SagaCMD + ' ta_morphometry 16 -DEM ' + inName + ' -TRI ' + OutName + ' -MODE 0';
       Result := SAGAMap(cmd,{InName,}outname,euMeters);
    end;


    function SagaTPIMap(InName : PathStr) : integer;
    var
       cmd : shortstring;
       OutName : shortstring;
    begin
       OutName := MDTempDir + 'saga_tpi_' + ExtractFileNameNoExt(InName) + '.tif';
       CMD := MDDef.SagaCMD + ' ta_morphometry 18 -DEM ' + inName + ' -TPI ' + OutName;
       Result := SAGAMap(cmd,{InName,}outname,euMeters);
    end;


    function SagaVectorRuggednessMap(InName : PathStr; Radius : integer) : integer;
    var
       cmd : shortstring;
       OutName : shortstring;
    begin
       OutName := MDTempDir + 'saga_vrm_rad_' + FilterSizeStr(Radius) + '_' + ExtractFileNameNoExt(InName) + '.tif';
       CMD := MDDef.SagaCMD + ' ta_morphometry 17 -DEM ' + inName + ' -VRM ' + OutName + ' -RADIUS ' + IntToStr(Radius);
       Result := SAGAMap(cmd,{InName,}outname,Undefined);
    end;


