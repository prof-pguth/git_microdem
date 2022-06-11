unit block_geostats_thread;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}

removed 6/15/2016

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordBlockGeostats}
{$EndIf}


interface

uses
//needed for inline of the core DB functions
   Data.DB,

   {$IfDef UseFireDacSQLlite}
   FireDAC.Stan.ExprFuncs,
   FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
   FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
   FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
   FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Comp.Client, FireDAC.Comp.DataSet,
   FireDAC.Phys.SQLite, FireDAC.Comp.UI,   //FireDAC.VCLUI.Wait,
   {$EndIf}

   {$IfDef UseBDETables}
   dbTables,
   {$EndIf}

   {$IfDef UseTDBF}
   dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
   DBClient,
   {$EndIf}
//end core DB functions definitions

  Classes,SysUtils, Forms,
  System.Math,
  DEMDefs,PetDBUtils,Petmar_types;

type
  tOldBlockGeostats = class(TThread)
  private
    { Private declarations }
    procedure UpdateThreadProgress;
    procedure MakeTable;
    procedure DeleteTable;
  protected
  public
    WantedDEM,ProgressBarNum,PercentDone : integer;
    PointsDBF,
    dbName : PathStr;
    GridLimits : tDEMGridLimits;
    ProfileData : tMyData;
    TableName : PathStr;

    constructor Create(inDBName : PathStr; inWantedDEM,inProgessBar : integer; inGridLimits : tDEMGridLimits; inPointsDBF : PathStr = '');
    procedure Execute; override;
  end;


implementation

uses
   DEMCoord,
   DEMLOS_Draw,
   Nevadia_Main,
   Thread_timers,
   Make_tables,Read_DEM,
   Petmar,NetMainW,BaseGraf,PetMath;

{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure BlockGeostats.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ BlockGeostats }

var
   DBCounter : integer;


constructor tOldBlockGeostats.Create;
begin
  inherited Create(false);
  WantedDEM := InWantedDEM;
  GridLimits := inGridLimits;
  dbName := inDBName;
  PointsDBF := inPointsDBF;
  ProgressBarNum := inProgessBar;
  FreeOnTerminate := true;   //not DoProgress;
end;


procedure tOldBlockGeostats.UpdateThreadProgress;
begin
    if (PercentDone > 990) then begin
       EndThread(ProgressBarNum);
    end
    else ThreadTimers.UpdateThreadStats(ProgressBarNum,PercentDone);
end;


procedure tOldBlockGeostats.MakeTable;
begin
   inc(DBCounter);
   TableName := MDTempDir + 'los_' + IntToStr(DBCounter) + DefaultDBExt;

   MDDef.ForceCrestComputations := true;
   Make_Tables.MakeFresnelTable(TableName,false,false,false,false,false,false,false,false,false);
   ProfileData := tMyData.Create(TableName);
end;

procedure tOldBlockGeostats.DeleteTable;
begin
   ProfileData.Destroy;
   DeleteFileIfExists(TableName);
end;


procedure tOldBlockGeostats.Execute;
{$IfDef ExGeostats}
begin
{$Else}
label
   AllDone;
var
   StartX,StartY,XBoxGridSize,YBoxGridSize,ProgressFactor,
   XBoxes,YBoxes,x,y,ElevsNeeded,i,RecNum,xcenter,ycenter : integer;
   Table1 : tMyData;
   NPts : integer;
   //tf,
   GammaEW,GammaNS,GammaNESW,GammaNWSE,
   MinZ,MaxZ,ave,adev,sdev,svar,skew,curt : float;
   Lat,Long : float;
   NetForm : NetMainw.tNetForm;
   ThisGraph : BaseGraf.TThisBaseGraph;
   AvgElev,AvgSlope,MaxSlope,s1s2,s2s3,Shape,Strength,Upward,Downward,
   StdDevSlope,StdDevElev,ElevRange,PercentOcean,QueensAspect,RoughnessFactor  : float;
   V : ^tTrendVector;
   TheDips,TheDipDirs : VectorType;
   Missing : float;

        procedure ComputeStats;
        var
           z : float;
           SSOGridLimits : tDEMGridLimits;
        begin
           {$IfDef RecordBlockGeostats}
           WriteLineToDebugFile('ComputeStats in ' + TimeToStr(Now),true);
           {$EndIf}

           with DEMGlb[WantedDEM],Table1 do begin
              if FilledGridBox(GridLimits) then begin
                 DEMGlb[WantedDEM].ElevationMoments(GridLimits,NPts,MinZ,MaxZ,ave,adev,sdev,svar,skew,curt);
                 if (NPts >= ElevsNeeded) and (MaxZ - MinZ > 0) then  begin
                    Table1.Insert;
                    ComputeMissingData(GridLimits,Missing);
                    if Table1.FieldExists('NAME') then Table1.SetFieldByNameAsString('NAME',AreaName);
                    if Table1.FieldExists('AREA') then Table1.SetFieldByNameAsString('AREA',LastSubDir(DEMFileName));

                    Table1.SetFieldByNameAsFloat('LAT',Lat);
                    Table1.SetFieldByNameAsFloat('LONG',Long);
                    inc(RecNum);
                    Table1.SetFieldByNameAsFloat('REC_NO',RecNum);
                    Table1.SetFieldByNameAsInteger('NPTS',Npts);

                    if MDDef.IncludeBasicElevation then begin
                      Table1.SetFieldByNameAsFloat('ELEV_AVG',ave);
                      CarefullySetFloat('ELEV_STD',sdev,0.001);
                      CarefullySetFloat('ELEV_SKW',skew,0.001);
                      CarefullySetFloat('ELEV_KRT',curt,0.001);
                    end;

                    if MDDef.IncludeAdvancedElevation then                begin
                       Table1.SetFieldByNameAsFloat('RELIEF',MaxZ - MinZ);
                       Table1.SetFieldByNameAsFloat('ELEV_MAX',MaxZ);
                       z := (ave-MinZ) / (MaxZ - MinZ);
                       CarefullySetFloat('ELEV_RELF',z,0.001);
                    end;

                     if MDDef.IncludeSlopeMeasures then                 begin
                        DEMGlb[WantedDEM].SlopeMoments(GridLimits,NPts,MinZ,MaxZ,ave,adev,sdev,svar,skew,curt);
                        CarefullySetFloat('SLOPE_AVG',ave,0.001);
                        CarefullySetFloat('SLOPE_STD',sdev,0.001);
                        CarefullySetFloat('SLOPE_SKW',skew,0.001);
                        CarefullySetFloat('SLOPE_KRT',curt,0.001);
                        CarefullySetFloat('SLOPE_MAX',MaxZ,0.001);
                     end;

                    if MDDef.IncludeBasinID then                begin
                       Table1.SetFieldByNameAsString('BASIN_ID',AreaName);
                       Table1.SetFieldByNameAsFloat('SLP_OV_30',Over30PercentSlope);
                       Table1.SetFieldByNameAsFloat('SLP_OV_50',Over50PercentSlope);
                    end;

                     if MDDef.IncludePlanCMeasures then                 begin
                         DEMGlb[WantedDEM].PlanCMoments(GridLimits,NPts,MinZ,MaxZ,ave,adev,sdev,svar,skew,curt);
                         CarefullySetFloat('PLANC_AVG',ave,0.000001);
                         CarefullySetFloat('PLANC_STD',sdev,0.000001);
                         CarefullySetFloat('PLANC_SKW',skew,0.000001);
                         CarefullySetFloat('PLANC_KRT',curt,0.000001);
                     end;

                     if MDDef.IncludeProfCMeasures then                 begin
                        DEMGlb[WantedDEM].ProfCMoments(GridLimits,NPts,MinZ,MaxZ,ave,adev,sdev,svar,skew,curt);
                        CarefullySetFloat('PROFC_AVG',ave,0.000001);
                        CarefullySetFloat('PROFC_STD',sdev,0.000001);
                        CarefullySetFloat('PROFC_SKW',skew,0.000001);
                        CarefullySetFloat('PROFC_KRT',curt,0.000001);
                     end;

                     if MDDef.IncludeMissingHoles then                  begin
                        Table1.SetFieldByNameAsFloat('MISSING',Missing);
                     end;

                     if MDDef.IncludeOpenness then begin
                        if DEMGlb[WantedDEM].FigureOpenness(xcenter,ycenter,MDDef.OpenGridBoxSize,Upward,Downward) then                         begin
                           Table1.SetFieldByNameAsFloat('OPEN_UP',Upward);
                           Table1.SetFieldByNameAsFloat('OPEN_DOWN',Downward);
                        end;
                     end;

                     {$IfDef ExComplexGeostats}
                     {$Else}
                     if MDDef.IncludeGammaMeasures then                     begin
                         VariogramGamma(GridLimits,GammaEW,GammaNS,GammaNESW,GammaNWSE);
                         CarefullySetFloat('GAMMA_EW',GammaEW,0.0001);
                         CarefullySetFloat('GAMMA_NS',GammaNS,0.0001);
                         CarefullySetFloat('GAMMA_NESW',GammaNESW,0.0001);
                         CarefullySetFloat('GAMMA_NWSE',GammaNWSE,0.0001);
                     end;
                     {$EndIf}

                     if (MDDef.IncludeFabricMeasures or MDDef.IncludeWavelength) then                     begin
                       NetForm := Nil;
                       ThisGraph := Nil;
                       New(v);
                       SSOGridLimits.XGridLow := xCenter - round(MDDef.SSOBoxSizeMeters /DEMGLB[WantedDEM].AverageXSpace);
                       SSOGridLimits.XGridHigh := xCenter + round(MDDef.SSOBoxSizeMeters /DEMGLB[WantedDEM].AverageXSpace);
                       SSOGridLimits.YGridLow := yCenter - round(MDDef.SSOBoxSizeMeters /DEMGLB[WantedDEM].AverageYSpace);
                       SSOGridLimits.YGridHigh := YCenter + round(MDDef.SSOBoxSizeMeters /DEMGLB[WantedDEM].AverageYSpace);

                       if DEMGlb[WantedDEM].SSOComputations(SSOGridLimits,
                             AvgElev,AvgSlope,MaxSlope,s1s2,s2s3,Shape,Strength,StdDevSlope,StdDevElev,
                             ElevRange,PercentOcean,QueensAspect,RoughnessFactor,
                             V^,TheDips,TheDipDirs,NPts,false) and (s1s2 < 32000) then begin
                          if MDDef.IncludeFabricMeasures then begin
                              Table1.SetFieldByNameAsFloat('S1S2',S1S2);
                              Table1.SetFieldByNameAsFloat('STRENGTH',Strength);

                              Table1.SetFieldByNameAsFloat('S2S3',S2S3);
                              Table1.SetFieldByNameAsFloat('SHAPE',Shape);

                              CarefullySetFloat('ROUGHNESS',RoughnessFactor,0.0001);
                              if (TheDipDirs[3] > 180) then TheDipDirs[3] := TheDipDirs[3] - 180;
                              CarefullySetFloat('FABRIC_DIR',TheDipDirs[3],0.1);
                          end;
                          (*
                          if MDDef.IncludeWavelength then
                          begin
                              if GetPerpendicularLineEnd(Lat,Long,5000,TheDipDirs[3],Lat1,Long1,Lat2,Long2) then
                              begin
                                 //Synchronize(MakeTable);
                                 MDDef.ForceCrestComputations := true;
                                 LOSComputeOnly(ProfileData,DEMGLB[WantedDEM],0,lat1,long1,lat2,long2,0,0);
                                 FindWavelengthStats(ProfileData,WavelengthMean,WavelengthMedian,WavelengthStdDev,
                                          HeightMean,HeightMedian,HeightStd);
                                 if (WaveLengthMean > 0.01) then
                                 begin
                                    Table1.SetFieldByNameAsFloat('LEN_MEAN',WavelengthMean);
                                    Table1.SetFieldByNameAsFloat('LEN_MEDIAN',WavelengthMedian);
                                    Table1.SetFieldByNameAsFloat('LEN_STD',WavelengthStdDev);
                                    Table1.SetFieldByNameAsFloat('HT_MEAN',HeightMean);
                                    Table1.SetFieldByNameAsFloat('HT_MEDIAN',HeightMedian);
                                    Table1.SetFieldByNameAsFloat('HT_STD',HeightStd);
                                    Table1.SetFieldByNameAsFloat('HT_STD2AV',HeightStd/HeightMean);
                                    Table1.SetFieldByNameAsFloat('LEN_STD2AV',WavelengthStdDev/WavelengthMean);
                                 end;
                                 TableName := ProfileData.FullTableName;
                                 Synchronize(DeleteTable);
                              end;
                           end;
                           *)
                       end {if SSOComputations};
                       Dispose(v);
                     end {if (NPts >= MDDef.MinPointsForSSO) and IncludeFabricMeasures};
                    Table1.Post;
                  end {if (NPts >= ElevsNeeded) and (MaxZ - MinZ > 0)};
              end; {if FilledGridBox}
          end;
        end;

begin
  {$IfDef RecordBlockGeostats}
  WriteLineToDebugFile('BlockGeostats.Execute in ' + TimeToStr(Now),true,true);
  {$EndIf}
    if (WantedDEM = 0) then    begin
       {$IfDef RecordBlockGeostats}
       WriteLineToDebugFile('(WantedDEM = 0)');
       {$EndIf}
       Table1 := tMyData.Create(dbName);
       RecNum := 0;
       for i := 1 to MaxDEMDataSets do       begin
          PercentDone := round(100 * i/MaxDEMDataSets);
          Synchronize(UpdateThreadProgress);
          if (DEMGlb[i] <> Nil) then with DEMGlb[i] do begin
            ElevsNeeded := round(0.01 * MDDef.GeomorphElevsNeededPercent * HeadRecs.NumCol * HeadRecs.NumRow);
            if (ElevsNeeded > 10000) then ElevsNeeded := 10000;

            WantedDEM := i;
            DEMCenterPoint(Lat,Long);

            GridLimits := DEMGlb[WantedDEM].FullDEMGridLimits;
            ComputeStats;
          end;
       end;
       goto AllDone;
   end
   else with DEMGlb[WantedDEM] do   begin
      {$IfDef RecordBlockGeostats}
      WriteLineToDebugFile('WantedDEM = ' + IntToStr(WantedDEM));
      {$EndIf}
      GetBoxGridSize(XBoxGridSize,YBoxGridSize);
      ElevsNeeded := round(0.01 * MDDef.GeomorphElevsNeededPercent * XBoxGridSize * YBoxGridSize);

      if not FilledGridBox(GridLimits) then exit;
      Table1 := tMyData.Create(dbName);
      {$IfDef RecordBlockGeostats}
      WriteLineToDebugFile('PointsDBF = EmptyString');
      {$EndIf}
      MDDef.StatSampleIncr := 1;
      if MDDef.EntireDEMGeostats then      begin
         GridLimits := DEMGlb[WantedDEM].FullDEMGridLimits;
         DEMGlb[WantedDEM].DEMGridToLatLongDegree((GridLimits.XGridLow+GridLimits.XGridHigh) div 2,(GridLimits.YGridLow+GridLimits.YGridHigh) div 2, Lat,Long);
         ComputeStats;
      end
      else       begin
          with GridLimits do          begin
             XBoxes := (XGridHigh - XGridLow) div XBoxGridSize;
             YBoxes := (YGridHigh - YGridLow) div YBoxGridSize;
          end;
          if (xboxes > 0) and (yboxes > 0) then           begin
             if (xboxes > 10) then ProgressFactor := 4
             else if (xboxes > 4) then ProgressFactor := 2
             else ProgressFactor := 1;
             StartX := GridLimits.XGridLow;
             StartY := GridLimits.YGridLow;
             try
                try
                   SkipMenuUpdating := true;
                   x := 0;
                   while (x <= XBoxes) do begin
                      PercentDone := round(100 * x/XBoxes);
                      Synchronize(UpdateThreadProgress);
                      GridLimits.XGridLow := StartX + x * XBoxGridSize;
                      GridLimits.XGridHigh := StartX + succ(x) * XBoxGridSize;
                      y := 0;
                      while (y <= YBoxes) do begin
                          GridLimits.YGridLow := StartY + y * YBoxGridSize;
                          GridLimits.YGridHigh := StartY + succ(Y) * YBoxGridSize;
                          xCenter := (GridLimits.XGridLow+GridLimits.XGridHigh) div 2;
                          yCenter := (GridLimits.YGridLow+GridLimits.YGridHigh) div 2;
                          DEMGlb[WantedDEM].DEMGridToLatLongDegree(xCenter,yCenter, Lat,Long);
                          ComputeStats;
                          inc(y,MDDef.StatSampleIncr);
                      end {while y};
                      inc(x,MDDef.StatSampleIncr);
                   end {while x};
                except
                    on Exception do begin
                    end;
                end;
             finally
             end;
         end;
      end;
      //end;
  AllDone:;
      Table1.Destroy;
      PercentDone := 999;
      Synchronize(UpdateThreadProgress);
      {$IfDef RecordBlockGeostats}
      WriteLineToDebugFile('BlockGeostats.Execute out');
      {$EndIf}
   end {if};
{$EndIf}
end;



initialization
   DBCounter := 0;
finalization
end.
