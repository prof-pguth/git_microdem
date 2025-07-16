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
begin
   if (Param = 'SLOPE') then Result := CreateSlopeMap(DEM,OpenMaps)
   else if (Param = 'PLANC') then Result := WBT_PlanCurvature(OpenMaps,DEMGlb[DEM].GeotiffDEMName)
   else if (Param = 'OPENU') then Result := CreateUpwardOpennessMap(OpenMaps,DEM,-99,MDDef.DEMIX_open_radials)
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
