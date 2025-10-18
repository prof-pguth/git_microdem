{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}
{                                        }
{   include file for  demix_definitions  }
{________________________________________}


function CreateSingleLSPGrid(OpenMaps : boolean; DEM : integer; Param : shortstring) : integer;
var
   Mean,Std : float32;
   NPts : int64;
begin
   Param := UpperCase(Param);
   if (Param = 'SLOPE') then Result := CreateSlopeMap(DEM,OpenMaps)
   else if (Param = 'PLANC') then Result := WBT_PlanCurvature(OpenMaps,DEMGlb[DEM].GeotiffDEMName)
   else if (Param = 'PROFC') then Result := WBT_ProfileCurvature(OpenMaps,DEMGlb[DEM].GeotiffDEMName)
   else if (Param = 'TANGC') then Result := WBT_TangentialCurvature(OpenMaps,DEMGlb[DEM].GeotiffDEMName)
   else if (Param = 'HAND') then Result := WBT_ElevAboveStream(OpenMaps,DEMGlb[DEM].GeotiffDEMName,'','','','')
   else if (Param = 'VDEV') then Result := WBT_AvgNormVectAngDev(OpenMaps,DEMGlb[DEM].GeotiffDEMName,5)
   else if (Param = 'SSTD') then Result := WBT_SphericalStdDevOfNormals(OpenMaps,DEMGlb[DEM].GeotiffDEMName,5)
   else if (Param = 'MHILL') then Result := WBT_MultidirectionalHillshadeMap(OpenMaps,DEM)
   else if (Param = 'WETIN') then Result := WBT_WetnessIndex(OpenMaps,true,DEMGlb[DEM].GeotiffDEMName,'')
   else if (Param = 'KNSS') then Result := RUN_LSPcalculator(DEM,'--knss',OpenMaps)
   else if (Param = 'KNCC') then Result := RUN_LSPcalculator(DEM,'--kncc',OpenMaps)
   else if (Param = 'KNCS') then Result := RUN_LSPcalculator(DEM,'--kncs',OpenMaps)
   else if (Param = 'KPS') then Result := RUN_LSPcalculator(DEM,'--kps',OpenMaps)
   else if (Param = 'CONIN') then Result := SAGA_ConvergenceIndex(OpenMaps,DEMGlb[DEM].GeotiffDEMName)
   else if (Param = 'OPENU') then Result := CreateUpwardOpennessMap(OpenMaps,DEM,-99,MDDef.DEMIX_open_radials)
   else if (Param = 'OPEND') then Result := CreateDownwardOpennessMap(OpenMaps,DEM,-99,MDDef.DEMIX_open_radials)
   else if (Param = 'HILL') then Result := CreateHillshadeMap(OpenMaps,DEM)
   else if (Param = 'SLOPE') then Result := CreateSlopeMap(DEM,OpenMaps)
   else if (Param = 'RUFF') then begin
      Result := CreateRoughnessSlopeStandardDeviationMap(DEM,MDDef.DEM_ruff_window,OpenMaps);
      DEMglb[Result].ElevationStatistics(DEMglb[Result].FullDEMGridLimits,Mean,Std,NPts);
      WriteLineToDebugFile(DEMglb[Result].AreaName + '   ' + RealToString(Mean,-12,-2));
   end
   else if (Param = 'TPI') then Result := BoxCarDetrendDEM(OpenMaps,DEM,3)
   else if (Param = 'RRI') then Result := MakeRRIGrid(DEM,OpenMaps)
   else if (Param = 'MAD2K') then Result := MakeMAD2KGrid(false,DEM)
   else begin
      Result := 0;
      MessageToContinue('Not yet implemented for ' + Param);
   end;
end;


procedure CreateLSPgrids(OpenMaps : boolean; Param,AreaName : shortstring; var PointGrids,AreaGrids : tDEM_int_array);


   {$IfDef ExternalProgramFUV_SSIM}
       procedure DoExternalProgramGrids;
       begin
          if (Param = 'LS') then begin
             Success := OpenGridsCreatedByExternalProgram(OpenMaps,'wbt',AreaName,'sed_trans_',PointGrids,AreaGrids);
          end;

          if (Param = 'ROTOR') then begin
             Success := OpenGridsCreatedByExternalProgram(OpenMaps,'WBT',AreaName,'rotor_',PointGrids,AreaGrids);
          end;
       end;
    {$EndIf}


       procedure OtherProgramLSPs;
       var
          Success : boolean;
       begin
          if (Param = 'KNSS') then begin
             LSP_Calc_Grids('knss',OpenMaps);
          end;
          if (Param = 'KNCC') then begin
             LSP_Calc_Grids('kncc',OpenMaps);
          end;
          if (Param = 'KNCS') then begin
             LSP_Calc_Grids('kncs',OpenMaps);
          end;

          if (Param = 'CONIN') then begin
             Success := SAGACreateDEMIX_ConIn_Grids(OpenMaps,AreaName,'CONIN_');
          end;
       end;


      procedure WhiteBoxToolsLSPs;
      var
         i : integer;
      begin
          if (Param = 'HAND') then begin
             WBT_CreateDEMIX_HANDGrids(OpenMaps);
          end;

          if (Param = 'ACCUM') then begin
             WBT_CreateDEMIX_Flow_AccumulationGrids(false,OpenMaps);
          end;

          if (Param = 'LOGFA') then begin
             WBT_CreateDEMIX_Flow_AccumulationGrids(true,OpenMaps);
          end;

          if (Param = 'PROFC') then begin
             for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_ProfileCurvature(OpenMaps,DEMGlb[PointDEMs[i]].GeotiffDEMName);
             for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_ProfileCurvature(OpenMaps,DEMGlb[AreaDEMs[i]].GeotiffDEMName);
          end;

          if (Param = 'PLANC') then begin
             for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_PlanCurvature(OpenMaps,DEMGlb[PointDEMs[i]].GeotiffDEMName);
             for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_PlanCurvature(OpenMaps,DEMGlb[AreaDEMs[i]].GeotiffDEMName);
          end;

          if (Param = 'TANGC') then begin
             for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_TangentialCurvature(OpenMaps,DEMGlb[PointDEMs[i]].GeotiffDEMName);
             for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_TangentialCurvature(OpenMaps,DEMGlb[AreaDEMs[i]].GeotiffDEMName);
          end;

          if (Param = 'VDEV') then begin
             for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_AvgNormVectAngDev(OpenMaps,DEMGlb[PointDEMs[i]].GeotiffDEMName,5);
             for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_AvgNormVectAngDev(OpenMaps,DEMGlb[AreaDEMs[i]].GeotiffDEMName,5);
          end;

          if (Param = 'SSTD') then begin
             for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_SphericalStdDevOfNormals(OpenMaps,DEMGlb[PointDEMs[i]].GeotiffDEMName,5);
             for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_SphericalStdDevOfNormals(OpenMaps,DEMGlb[AreaDEMs[i]].GeotiffDEMName,5);
          end;

          if (Param = 'MHILL') then begin
             for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_MultidirectionalHillshadeMap(OpenMaps,PointDEMs[i]);
             for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_MultidirectionalHillshadeMap(OpenMaps,AreaDEMs[i]);
          end;

          if (Param = 'WETIN') then begin
             for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_WetnessIndex(OpenMaps,true,DEMGlb[PointDEMs[i]].GeotiffDEMName,'');
             for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_WetnessIndex(OpenMaps,true,DEMGlb[AreaDEMs[i]].GeotiffDEMName,'');
          end;
      end;


      procedure MICRODEM_LSPs;
      var
         i : integer;
      begin
          if (Param = 'HILL') then begin
             CreateDEMIXDerivedGrids('HILL_',AreaName,OpenMaps,MDDef.DEMIXsaveLSPmaps);
          end;
          if (Param = 'TPI') then begin
             CreateDEMIXDerivedGrids('TPI_',AreaName,OpenMaps,MDDef.DEMIXsaveLSPmaps);
          end;

          if (Param = 'RRI') then begin
             CreateDEMIXDerivedGrids('RRI_',AreaName,OpenMaps,MDDef.DEMIXsaveLSPmaps);
          end;

          if (Param = 'OPEND') then begin
             for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := CreateDownwardOpennessMap(OpenMaps,PointDEMs[i],-99,MDDef.DEMIX_open_radials);
             for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := CreateDownwardOpennessMap(OpenMaps,AreaDEMs[i],-99,MDDef.DEMIX_open_radials);
          end;

          if (Param = 'OPENU') then begin
             for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := CreateUpwardOpennessMap(OpenMaps,PointDEMs[i],-99,MDDef.DEMIX_open_radials);
             for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := CreateUpWardOpennessMap(OpenMaps,AreaDEMs[i],-99,MDDef.DEMIX_open_radials);
          end;

          if (Param = 'RUFF') then begin
              for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := CreateRoughnessSlopeStandardDeviationMap(PointDEMs[i],MDDef.DEM_ruff_window,OpenMaps);
              for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := CreateRoughnessSlopeStandardDeviationMap(AreaDEMs[i],MDDef.DEM_ruff_window,OpenMaps);
          end;

          if (Param = 'SLOPE') then begin
              for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := CreateSlopeMap(PointDEMs[i],OpenMaps);
              for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := CreateSlopeMap(AreaDEMs[i],OpenMaps);
          end;
      end;

begin
   {$IfDef ExternalProgramFUV_SSIM} DoExternalProgramGrids; {$EndIf}
   OtherProgramLSPs;
   WhiteBoxToolsLSPs;
   MICRODEM_LSPs;
end;
