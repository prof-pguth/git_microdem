unit pit_and_spire;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}

//PLG note, 2/25/2023: other than pit and spire, the other routines have not been recently tested and may not work well.  Use at your own risk

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      {$Define RecordPitsSpires}
   {$EndIf}
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,  Forms, Dialogs,  Buttons,
  DEMDefs,DEMMapf,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Controls;

type
  TPitSpireForm = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    RadioGroup1: TRadioGroup;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    TabSheet3: TTabSheet;
    Label10: TLabel;
    Label11: TLabel;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Label12: TLabel;
    Label13: TLabel;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    TabSheet4: TTabSheet;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Edit14: TEdit;
    Edit15: TEdit;
    Edit16: TEdit;
    BitBtn6: TBitBtn;
    BitBtn8: TBitBtn;
    Label17: TLabel;
    Edit17: TEdit;
    BitBtn5: TBitBtn;
    CheckBox4: TCheckBox;
    Label18: TLabel;
    Edit18: TEdit;
    Label20: TLabel;
    Edit20: TEdit;
    TabSheet5: TTabSheet;
    Label21: TLabel;
    Edit21: TEdit;
    BitBtn10: TBitBtn;
    CheckBox1: TCheckBox;
    Edit22: TEdit;
    Label22: TLabel;
    Edit23: TEdit;
    Label23: TLabel;
    Edit24: TEdit;
    BitBtn11: TBitBtn;
    CheckBox8: TCheckBox;
    Edit25: TEdit;
    BitBtn12: TBitBtn;
    Label24: TLabel;
    Edit26: TEdit;
    RadioGroup2: TRadioGroup;
    Label25: TLabel;
    Edit27: TEdit;
    Label26: TLabel;
    Edit28: TEdit;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Edit29: TEdit;
    Edit30: TEdit;
    Edit31: TEdit;
    Label31: TLabel;
    Edit32: TEdit;
    PeakRoofBtn: TBitBtn;
    TabSheet6: TTabSheet;
    BitBtn15: TBitBtn;
    RadioGroup3: TRadioGroup;
    BitBtn16: TBitBtn;
    BitBtn17: TBitBtn;
    Flatroadbtn: TBitBtn;
    Edit33: TEdit;
    Label32: TLabel;
    BitBtn14: TBitBtn;
    BitBtn18: TBitBtn;
    GroupBox3: TGroupBox;
    Label19: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    BitBtn2: TBitBtn;
    BitBtn7: TBitBtn;
    Edit19: TEdit;
    Edit2: TEdit;
    Edit1: TEdit;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    Label3: TLabel;
    BitBtn3: TBitBtn;
    Edit3: TEdit;
    Edit4: TEdit;
    BitBtn9: TBitBtn;
    GroupBox1: TGroupBox;
    Label6: TLabel;
    Label1: TLabel;
    BitBtn13: TBitBtn;
    BitBtn4: TBitBtn;
    Edit6: TEdit;
    Edit5: TEdit;
    CheckBox2: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure Edit21Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Edit22Change(Sender: TObject);
    procedure Edit23Change(Sender: TObject);
    procedure Edit24Change(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure Edit25Change(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure PeakRoofBtnClick(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure FlatroadbtnClick(Sender: TObject);
    procedure Edit28Change(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateValues;
    procedure DrawBuildingEdges;
    procedure DrawWalls;
  public
    { Public declarations }
     MapOwner : tMapForm;
     UseDEM,
     SpiresDB,RoofDB,EdgeDB,WallDB,TrendGrid,RoadNeighGrid,PeakedDB,SlopeGrid,AspectGrid,PeakRoofGrid,DropOffGrid : integer;
     GridLimits : tGridLimits;
  end;


implementation

{$R *.dfm}

uses
   {$IfDef ExGIS}
   {$Else}
   demdatabase,
   {$EndIf}
   DEMCoord,Petmar,Petmar_types,demdef_routines,
   Mask_opts2,Make_grid,
   DEM_Manager,
   BaseMap,
   DEMstat,
   PetImage,Petmath;



procedure TPitSpireForm.DrawWalls;
var
   Col,Row,xp,yp,WallDB : integer;
   znw,zw,zsw,zn,z,zs,zne,ze,zse : float32;
   Lat,Long : float64;
   WallResults : tStringList;
   fName : PathStr;
begin
   {$IfDef RecordPitsSpires} WriteLineToDebugFile('TMapForm.DrawWalls in'); {$EndIf}
      if MDDef.OverWriteFeatureDBs then CloseAndNilNumberedDB(WallDB);
      WallResults := tStringList.Create;
      WallResults.Add('LAT,LONG');

       StartProgressAbortOption('Search walls');
       Col := GridLimits.XGridLow;
       while Col <= GridLimits.XGridHigh do begin
          UpdateProgressBar((Col-GridLimits.XGridLow)/(GridLimits.XGridHigh-GridLimits.XGridLow));
          Row := GridLimits.YGridLow;
          while Row <= GridLimits.YGridHigh do begin
             if DEMGlb[MapOwner.MapDraw.DEMonMap].SurroundedPointElevs(Col,Row,znw,zw,zsw,zn,z,zs,zne,ze,zse,MDDef.WallRegion) then begin
                if (((z-zs) >= MDDef.AcrossWallHeight) and ((z-zn) >= MDDef.AcrossWallHeight) and (abs(z-ze) <= MDDef.AlongWallHeight) and (abs(z-zw) <= MDDef.AlongWallHeight)) or
                   (((z-ze) >= MDDef.AcrossWallHeight) and ((z-zw) >= MDDef.AcrossWallHeight) and (abs(z-zn) <= MDDef.AlongWallHeight) and (abs(z-zs) <= MDDef.AlongWallHeight)) or
                   (((z-znw) >= MDDef.AcrossWallHeight) and ((z-zse) >= MDDef.AcrossWallHeight) and (abs(z-zne) <= MDDef.AlongWallHeight) and (abs(z-zsw) <= MDDef.AlongWallHeight))  or
                   (((z-zne) >= MDDef. AcrossWallHeight) and ((z-zsw) >= MDDef.AcrossWallHeight) and (abs(z-znw) <= MDDef.AlongWallHeight) and (abs(z-zse) <= MDDef.AlongWallHeight)) then begin
                        //inc(NumWall);
                        DEMGlb[MapOwner.MapDraw.DEMonMap].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                        WallResults.Add(RealToString(Lat,-18,8) + ',' + RealToString(Long,-18,8) );
                        MapOwner.MapDraw.DEMGridToScreen(Col,Row,xp,yp);
                end;
             end;
             inc(Row);
          end;
          inc(Col);
       end;
       EndProgress;

      {$IfDef RecordPitsSpires} WriteLineToDebugFile('Searches over');{$EndIf}

      Memo1.Lines.Add('');
      Memo1.Lines.Add('Walls: ' + IntToStr(pred(WallResults.Count)));
      Memo1.Lines.Add('');

      if (WallResults.Count > 1) then begin
         fName := MDTempDir + DEMGlb[MapOwner.MapDraw.DEMonMap].AreaName + '_walls'+'.csv';
         WallDB := MapOwner.DisplayAndPurgeStringListDB(WallResults,fName,true);
         GISdb[Walldb].AssignSymbol(MDdef.WallSymbol);
      end
      else WallResults.Free;
   {$IfDef RecordPitsSpires} WriteLineToDebugFile('TMapForm.DrawWalls out');{$EndIf}
end;


procedure TPitSpireForm.BitBtn10Click(Sender: TObject);
begin
   if (TrendGrid <> 0) then CloseSingleDEM(TrendGrid);
   Make_grid.CreateAnOrganizationMap(MapOwner.MapDraw.DEMonMap);
   TrendGrid := NewTrendDirGrid;
end;


procedure TPitSpireForm.BitBtn11Click(Sender: TObject);
var
   z,z2 : float32;
   Col,Row,x,y,n,
   XBoxSize,YBoxSize : integer;
begin
   if (RoadNeighGrid <> 0) then CloseSingleDEM(RoadNeighGrid);
   UpdateValues;

   RoadNeighGrid := DEMGlb[MapOwner.MapDraw.DEMonMap].CloneAndOpenGridSetMissing(ByteDEM,'Road neighbors',euUndefined);

   XBoxSize := round(MDDef.RoadTrendRegion / 2 / DEMGlb[RoadNeighGrid].AverageXSpace);
   YBoxSize := round(MDDef.RoadTrendRegion / 2 / DEMGlb[RoadNeighGrid].AverageYSpace);
   StartProgress('Roads');
   Col := GridLimits.XGridLow;
    while Col <= GridLimits.XGridHigh do begin
       UpdateProgressBar((Col-GridLimits.XGridLow)/(GridLimits.XGridHigh-GridLimits.XGridLow));
       Row := GridLimits.YGridLow;
       while Row <= GridLimits.YGridHigh do begin
         if DEMGlb[TrendGrid].GetElevMeters(Col,Row,z) then begin
            N := 0;
            for x := Col-XBoxSize to Col+XBoxSize do begin
               for y := Row-YBoxSize to Row+YBoxSize do begin
                  if DEMGlb[TrendGrid].GetElevMeters(x,y,z2) and (abs(z-z2) < MDDef.RoadTrendSensitivity) then inc(n);
               end;
            end;
            DEMGlb[RoadNeighGrid].SetGridElevation(Col,Row,n);
         end;
         inc(Row);
      end;
      inc(Col);
   end;
   EndProgress;
   DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.BackToWandering;
   DEMGlb[RoadNeighGrid].SetUpMap(RoadNeighGrid,true,mtElevSpectrum);
end;


procedure TPitSpireForm.BitBtn12Click(Sender: TObject);
var
   Lat,Long,Lat2,Long2 : float64;
   z,z2 : float32;
   Col,Row,x,y,n,xc,yc,xp,yp,PixWide,
   XBoxSize,YBoxSize : integer;
   Bitmap : tMyBitmap;
begin
   if (RoadNeighGrid <> 0) then CloseSingleDEM(RoadNeighGrid);
   DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.N11view1Click(Nil);
   UpdateValues;
   CheckEditString(Edit26.Text,PixWide);

   RoadNeighGrid := DEMGlb[MapOwner.MapDraw.DEMonMap].CloneAndOpenGridSetMissing(SmallIntDEM,'Road neighbors',euUndefined);
   if RoadNeighGrid <> 0 then begin
      XBoxSize := round(MDDef.RoadTrendRegion / 2 / DEMGlb[RoadNeighGrid].AverageXSpace);
      YBoxSize := round(MDDef.RoadTrendRegion / 2 / DEMGlb[RoadNeighGrid].AverageYSpace);
      xc := XBoxSize + 2;
      yc := YBoxSize + 2;
       StartProgress('Roads');
       Col := GridLimits.XGridLow;
       while Col <= GridLimits.XGridHigh do begin
          UpdateProgressBar((Col-GridLimits.XGridLow)/(GridLimits.XGridHigh-GridLimits.XGridLow));
          Row := GridLimits.YGridLow;
          while Row <= GridLimits.YGridHigh do begin
             if DEMGlb[TrendGrid].GetElevMeters(Col,Row,z) then begin
                PetImage.CloneImageToBitmap(DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.Image1,Bitmap);
                DEMGlb[MapOwner.MapDraw.DEMonMap].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                VincentyPointAtDistanceBearing(Lat,Long,MDDef.RoadTrendRegion,z,Lat2,Long2);
                DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.MapDraw.LatLongDegreeToScreen(Lat2,Long2,xc,yc);
                Bitmap.Canvas.MoveTo(xc,yc);
                Bitmap.Canvas.Pen.Width := PixWide;
                VincentyPointAtDistanceBearing(Lat,Long,MDDef.RoadTrendRegion,-z,Lat2,Long2);
                DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.MapDraw.LatLongDegreeToScreen(Lat2,Long2,xc,yc);
                Bitmap.Canvas.LineTo(xc,yc);

               N := 0;
               for x := Col-XBoxSize to Col+XBoxSize do begin
                  for y := Row-YBoxSize to Row+YBoxSize do begin
                     DEMGlb[TrendGrid].SelectionMap.MapDraw.DEMGridToScreen(Col,Row,xp,yp);
                     if Bitmap.Canvas.Pixels[xp,yp] <> clWhite then begin
                        if DEMGlb[TrendGrid].GetElevMeters(x,y,z2) and (abs(z-z2) < MDDef.RoadTrendSensitivity) then inc(n);
                     end;
                  end;
               end;
               DEMGlb[RoadNeighGrid].SetGridElevation(Col,Row,n);
               Bitmap.Free;
            end;
            inc(Row);
         end;
         inc(Col);
      end;
      EndProgress;
      DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.BackToWandering;
      DEMGlb[RoadNeighGrid].SetUpMap(RoadNeighGrid,true,mtElevSpectrum);
   end;
end;


procedure TPitSpireForm.BitBtn13Click(Sender: TObject);
var
   DEM,db : integer;
   fName : PathStr;
   PeakResults : tStringList;
   gl : tGridLimits;
begin
   {$IfDef RecordPitsSpires} WriteLineToDebugFile('TPitSpireForm.BitBtn13Click (peaks) in'); {$EndIf}
   UpdateValues;
   Memo1.Lines.Add('');
   Memo1.Lines.Add('Peak radius: ' + RealToString(MDDef.PeakRadius,-8,-2) + ' m');
   Memo1.Lines.Add('Peak height: ' + RealToString(MDDef.PeakHeight,-8,-2) + ' m');

   PeakResults := tStringList.Create;
   PeakResults.Add('DEM,LAT,LONG,PEAK_M,RELIEF_M');
   if CheckBox2.Checked then begin
      for DEM := 1 to MaxDEMDataSets do begin
         if ValidDEM(DEM) then begin
            gl := DEMGlb[DEM].SpecifyDEMGridLimitsFromLatLong(DEMGlb[MapOwner.MapDraw.DEMonMap].DEMBoundBoxGeo.YMin,DEMGlb[MapOwner.MapDraw.DEMonMap].DEMBoundBoxGeo.XMin,
               DEMGlb[MapOwner.MapDraw.DEMonMap].DEMBoundBoxGeo.YMax,DEMGlb[MapOwner.MapDraw.DEMonMap].DEMBoundBoxGeo.XMax);
            FindPeaks(DEM,gl,PeakResults,Memo1);
         end;
      end;
   end
   else begin
      FindPeaks(MapOwner.MapDraw.DEMonMap,GridLimits,PeakResults,Memo1);
   end;

   if (PeakResults.Count > 2) then begin
      fName := NextFileNumber(MDTempDir,'peaks_rad_' + IntToStr(Round(MDDef.PeakRadius)) + '_ht_' + IntToStr(Round(MDDef.PeakHeight)) + '_v_', '.dbf');
      db := MapOwner.StringListToLoadedDatabase(PeakResults,fname);
      GISdb[db].dbOpts.Symbol := MDdef.PeakSymbol;
      if CheckBox2.Checked then begin
         GISdb[db].dbOpts.DBAutoShow := dbasColorByString;
         GISdb[db].dbOpts.FloatColorField := 'DEM';
      end;
      {$IfDef RecordPitsSpires} WriteLineToDebugFile('off to plot ' + fName); {$EndIf}
      GISdb[db].RedrawLayerOnMap;
      {$IfDef RecordPitsSpires} WriteLineToDebugFile('TPitSpireForm.BitBtn13Click end'); {$EndIf}
   end;
end;

procedure TPitSpireForm.BitBtn14Click(Sender: TObject);
begin
   {$IfDef RecordPitsSpires} WriteLineToDebugFile('TPitSpireForm.BitBtn14Click in'); {$EndIf}
   UpdateValues;
   MapOwner.Forceredraw1Click(Nil);
   DrawWalls;
   {$IfDef RecordPitsSpires} WriteLineToDebugFile('TPitSpireForm.BitBtn14Click out'); {$EndIf}
end;

procedure TPitSpireForm.PeakRoofBtnClick(Sender: TObject);
var
   PeakRoofsOnMap,Col,Row,NPts : integer;
   SpireResults : tStringList;
   fName : PathStr;
   Lat,Long : float64;
   Slopes,Aspects : tElevBoxArray;
   x,y: Integer;
   TStr : ShortString;
begin
   DEMDef_routines.SaveBackupDefaults;
   MDDef.SlopeAlg := smEightNeighborsUnweighted;
   MDDef.GeomorphMapsFullDEM := true;
   UpdateValues;

   if (SlopeGrid = 0) then SlopeGrid := MakeSingleNewDerivativeMap('S',MapOwner.MapDraw.DEMonMap,0,false);
   if (AspectGrid = 0) then AspectGrid := MakeSingleNewDerivativeMap('A',MapOwner.MapDraw.DEMonMap,0,false);

   if MDDef.OverWriteFeatureDBs then begin
      CloseAndNilNumberedDB(PeakedDB);
      if (PeakRoofGrid <> 0) then CloseSingleDEM(PeakRoofGrid);
   end;

   if RadioGroup3.ItemIndex in [0,2] then begin
      if (Sender = Flatroadbtn) then TStr := 'Flat road neighbors'
      else TStr := 'Peak roof neighbors';
      PeakRoofGrid := DEMGlb[MapOwner.MapDraw.DEMonMap].CloneAndOpenGridSetMissing(ByteDEM,TStr,euIntCode);
  end;

   if RadioGroup3.ItemIndex in [1,2] then begin
      SpireResults := tStringList.Create;
      SpireResults.Add('LAT,LONG,SLOPE,ASPECT,NEIGHBORS');
   end;

    PeakRoofsOnMap := 0;
     Col := GridLimits.XGridLow;
     StartProgress('Roofs');
     while Col <= GridLimits.XGridHigh do begin
        UpdateProgressBar((Col-GridLimits.XGridLow)/(GridLimits.XGridHigh-GridLimits.XGridLow));
        Row := GridLimits.YGridLow;
        while Row <= GridLimits.YGridHigh do begin
           if DEMGlb[SlopeGrid].GetElev3x3Meters(Col,Row,Slopes) and DEMGlb[AspectGrid].GetElev3x3Meters(Col,Row,Aspects) then begin
              if ( (Sender = PeakRoofBtn) and (Slopes[0,0] >= MDDef.PeakRoofMinSlope)) or
                 ( (Sender = Flatroadbtn) and (Slopes[0,0] <= MDDef.BuildingMaxSlope))  then begin
                NPts := 0;
                for x := -1 to 1 do begin
                   for y := -1 to 1 do begin
                      if (abs(slopes[x,y] - slopes[0,0]) < MDDef.PeakRoofSlopeTol) and (abs(Aspects[x,y] - Aspects[0,0]) < MDDef.PeakRoofAspectTol) then
                         inc(NPts);
                   end {for y};
                end {for x};
                dec(NPts);  //since we did not check for x=y in the nested loops
                if RadioGroup3.ItemIndex in [0,2] then begin
                   if MDDef.FuzzyMatches then begin
                      DEMGlb[PeakRoofGrid].SetGridElevation(Col,Row,NPts);
                      if (NPts > MDDef.PeakRoofNeighbors) then inc(PeakRoofsOnMap);
                   end
                   else if (NPts > MDDef.PeakRoofNeighbors) then begin
                      DEMGlb[PeakRoofGrid].SetGridElevation(Col,Row,MDdef.MaskCode);
                      inc(PeakRoofsOnMap);
                   end;
                end;

                if (RadioGroup3.ItemIndex in [1,2]) and  (NPts > MDDef.PeakRoofNeighbors) then begin
                   DEMGlb[SlopeGrid].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                   SpireResults.Add( RealToString(Lat,-18,8) + ',' + RealToString(Long,-18,8) + ',' +
                       RealToString(slopes[0,0],-18,2) + ',' + RealToString(aspects[0,0],-12,-2) + ',' + IntToStr(NPts));
                   if RadioGroup3.ItemIndex in [2] then inc(PeakRoofsOnMap);
                end {if};
              end;
           end {if};
           inc(Row);
       end {while row};
       inc(Col);
    end {for col};
    EndProgress;

    if RadioGroup3.ItemIndex in [0,2] then begin
       if MDDef.MaskMapShow in [0,2] then begin
          DEMGlb[PeakRoofGrid].SetUpMap(PeakRoofGrid,true,mtElevSpectrum);
       end
       else begin
          DEMGlb[PeakRoofGrid].CheckMaxMinElev;
       end;
       if MDDef.MaskMapShow in [1,2] then begin
           DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.GridpointsfromsecondDEMAssignAndDraw(PeakRoofGrid);
       end;
    end;

    if RadioGroup3.ItemIndex in [1,2] then  begin
       fName := NextFileNumber(MDTempDir, 'Peak_roofs','.csv');
       PeakedDB := DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.DisplayAndPurgeStringListDB(SpireResults,fName);
    end;

    Memo1.Lines.Add('');
    if (Sender = Flatroadbtn)  then Memo1.Lines.Add('Max slope: ' + RealToString(MDDef.BuildingMaxSlope,-12,2) + '%')
    else Memo1.Lines.Add('Min slope: ' + RealToString(MDDef.PeakRoofMinSlope,-12,2) + '%');
    Memo1.Lines.Add('Slope tolearance: ' + RealToString(MDDef.PeakRoofSlopeTol,-12,1) + DegSym);
    Memo1.Lines.Add('Aspect tolearance: ' + RealToString(MDDef.PeakRoofAspectTol,12,-1) + DegSym);
    Memo1.Lines.Add('Nearest neighbors: ' + IntToStr(MDDef.PeakRoofNeighbors));
    Memo1.Lines.Add(TStr + ' in DEM: ' + IntToStr(PeakRoofsOnMap));
    Memo1.Lines.Add('');
    DEMDef_routines.RestoreBackupDefaults;
end;


procedure TPitSpireForm.BitBtn15Click(Sender: TObject);
var
   Col,Row,OnMap : integer;
   SpireResults : tStringList;
   fName : PathStr;
   znw,zw,zsw,zn,z,zs,zne,ze,zse : float32;
   Lat,Long : float64;

        procedure Corner(zw,znw,zn : float64; Dir : shortstring);
        begin
           z := MaxFloat(zw,znw,zn);
           if z > MDDef.BuildingMinHeight then begin
               DEMGlb[MapOwner.MapDraw.DEMonMap].DEMGridToLatLongDegree(Col,Row,Lat,Long);
               inc(OnMap);
               SpireResults.Add( RealToString(Lat,-18,8) + ',' + RealToString(Long,-18,8) + ',' +  RealToString(z,-8,-2) + ',' + Dir);
           end;
        end;

begin
   {$IfDef RecordPitsSpires} WriteLineToDebugFile('TPitSpireForm.BitBtn15Click in (corners)');{$EndIf}
      UpdateValues;
      if MDDef.OverWriteFeatureDBs then CloseAndNilNumberedDB(SpiresDB);
      SpireResults := tStringList.Create;
      SpireResults.Add('LAT,LONG,HEIGHT,CORNER');
      OnMap := 0;
      StartProgress('Corners');

       Col := GridLimits.XGridLow;
       while Col <= GridLimits.XGridHigh do begin
          UpdateProgressBar((Col-GridLimits.XGridLow)/(GridLimits.XGridHigh-GridLimits.XGridLow));
          Row := GridLimits.YGridLow;
          while Row <= GridLimits.YGridHigh do begin
             if DEMGlb[MapOwner.MapDraw.DEMonMap].IsSurroundedPoint(Col,Row) then begin
                 DEMGlb[MapOwner.MapDraw.DEMonMap].GetNineElevMeters(Col,Row,znw,zw,zsw,zn,z,zs,zne,ze,zse);
                 znw := z - znw;
                 zn := z - zn;
                 zne := z - zne;
                 zsw := z - zsw;
                 zs := z - zs;
                 zse := z - zse;
                 ze := z - ze;
                 zw := z - zw;
                 Corner(zw,znw,zn,'NW');
                 Corner(zn,zne,ze,'NE');
                 Corner(ze,zse,zs,'SE');
                 Corner(zs,zsw,zw,'SW');
             end;
            inc(Row);
         end;
         inc(Col);
      end;
      EndProgress;
      fName := Petmar.NextFileNumber(MDTempDir, DEMGlb[MapOwner.MapDraw.DEMonMap].AreaName + '_bldg_corner_','.csv');
      DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.StringListToLoadedDatabase(SpireResults,fName);
      Memo1.Lines.Add('');
      Memo1.Lines.Add('Bldg height >= ' + RealToString(MDDef.BuildingMinHeight,-8,-2) + ' m');
      Memo1.Lines.Add('Corners in DEM: ' + IntToStr(OnMap));
      Memo1.Lines.Add('');
   {$IfDef RecordPitsSpires} WriteLineToDebugFile('TPitSpireForm.BitBtn15Click in out'); {$EndIf}
end;

procedure TPitSpireForm.BitBtn16Click(Sender: TObject);
begin
   GridMaskOptions;
end;

procedure TPitSpireForm.BitBtn17Click(Sender: TObject);
begin
   Memo1.Clear;
end;

procedure TPitSpireForm.BitBtn18Click(Sender: TObject);
begin
   {$IfDef RecordPitsSpires} WriteLineToDebugFile('TPitSpireForm.BitBtn18lick in');{$EndIf}
   UpdateValues;
   MapOwner.Forceredraw1Click(Nil);
   {$IfDef ExVegDensity}
   {$Else}
      DEMGlb[MapOwner.MapDraw.DEMonMap].VegDensityLayers[1].FindPowerLines(MapOwner,Memo1);
   {$EndIf}
   {$IfDef RecordPitsSpires} WriteLineToDebugFile('TPitSpireForm.BitBtn18lick out'); {$EndIf}
end;

procedure TPitSpireForm.BitBtn2Click(Sender: TObject);
begin
   PickSymbol(BitBtn2,MDDef.SpireSymbol,'Spires');
end;

procedure TPitSpireForm.BitBtn3Click(Sender: TObject);
begin
   PickSymbol(BitBtn3,MDDef.PitSymbol,'Pits');
end;

procedure TPitSpireForm.BitBtn4Click(Sender: TObject);
begin
   PickSymbol(BitBtn4,MDDef.PeakSymbol,'Peaks');
end;


procedure TPitSpireForm.BitBtn5Click(Sender: TObject);
begin
   UpdateValues;
   DrawBuildingEdges;
end;

procedure TPitSpireForm.BitBtn6Click(Sender: TObject);
begin
   PickSymbol(BitBtn6,MDDef.WallSymbol,'Walls');
end;


procedure TPitSpireForm.BitBtn7Click(Sender: TObject);
var
   Col,Row,dx,dy,x,SpiresOnMap,NumLower : integer;
   SpireResults : tStringList;
   fName : PathStr;
   SpireHeightM : float32;
   Lat,Long : float64;
begin
   {$IfDef RecordPitsSpires} WriteLineToDebugFile('TPitSpireForm.BitBtn1Click in (spires)'); {$EndIf}
   UpdateValues;
   if MDDef.OverWriteFeatureDBs then CloseAndNilNumberedDB(SpiresDB);
   SpireResults := tStringList.Create;
   SpireResults.Add('LAT,LONG,HEIGHT,RADIUS_M,NUM_LOWER');

   dx := round(MDDef.SpireRadius / DEMGlb[MapOwner.MapDraw.DEMonMap].AverageXSpace);
   dy := round(MDDef.SpireRadius / DEMGlb[MapOwner.MapDraw.DEMonMap].AverageYSpace);
   SpiresOnMap := 0;
   StartProgressAbortOption('Spires');

   Col := GridLimits.XGridLow;
   while Col <= GridLimits.XGridHigh do begin
      UpdateProgressBar((Col-GridLimits.XGridLow)/(GridLimits.XGridHigh-GridLimits.XGridLow));
      Row := GridLimits.YGridLow;
      while Row <= GridLimits.YGridHigh do begin
        for x := 1 to dx do begin
           if DEMGlb[MapOwner.MapDraw.DEMonMap].IsSpire(Col,Row,x,x,SpireHeightM,NumLower) then begin
              DEMGlb[MapOwner.MapDraw.DEMonMap].DEMGridToLatLongDegree(Col,Row,Lat,Long);
              SpireResults.Add(RealToString(Lat,-18,8) + ',' + RealToString(Long,-18,8) + ',' + RealToString(SpireHeightM,-18,-2) + ',' + RealToString(x* DEMGlb[MapOwner.MapDraw.DEMonMap].AverageXSpace,-12,-2) + ',' + IntToStr(NumLower));
              inc(SpiresOnmap);
              break;
           end;
        end;
        inc(Row);
     end;
     inc(Col);
     if WantOut then break;
   end;
   EndProgress;
   fName := MDTempDir + DEMGlb[MapOwner.MapDraw.DEMonMap].AreaName + '_spires_rad_' + RealToString(MDDef.SpireRadius,-4,-1) + '_ht_' + RealToString(MDDef.SpireHeight,-8,-1) + '_v' +'.csv';
   SpiresDB := DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.DisplayAndPurgeStringListDB(SpireResults,fName);
   GISdb[SpiresDB].AssignSymbol(MDdef.SpireSymbol);

   Memo1.Lines.Add('');
   Memo1.Lines.Add('Spire height >= ' + RealToString(MDDef.SpireHeight,-8,-2) + ' m');
   Memo1.Lines.Add('Max spire radius: ' + RealToString(MDDef.SpireRadius,-8,-2) + ' m');
   Memo1.Lines.Add('Point tolerance: ' + IntToStr(MDDef.SpireNeighborTolerance));
   Memo1.Lines.Add('Spires on map: ' + IntToStr(SpiresOnMap));
   Memo1.Lines.Add('');
   {$IfDef RecordPitsSpires} WriteLineToDebugFile('TPitSpireForm.BitBtn1Click in out'); {$EndIf}
end;


procedure TPitSpireForm.BitBtn8Click(Sender: TObject);
label
   OnGround,OnGround2;
var
   SpireResults : tStringList;
   Col,Row,dx,dy,x,y,SpiresOnMap,GridCand,sg,NumGoodSlope,NumTotal : integer;
   fName : PathStr;
   TStr : shortString;
   Lat,Long: float64;
   Aspect,z,BuildingHeight,bs,dropoff  : float32;
   rad : array[1..4] of float64;


   function CouldBeRoof(Col,Row,dx,dy : integer) : boolean;
   label
      Found1,Found2,Found3,Found4;
   var
      x,y : integer;
      s1 : float64;
      TheMinElev,TheMaxElev,avgZ : float32;
      zb  : array[1..4] of float32;
      Limits : tGridLimits;
      SlpAsp : tSlopeAspectRec;
   begin
      Result := false;
      if DEMGlb[MapOwner.MapDraw.DEMonMap].GetSlopeAndAspect(Col,Row,SlpAsp) then begin
         if (SlpAsp.SlopePercent <= MDDef.BuildingMaxSlope) then begin
           z := SlpAsp.z;
           if (RadioGroup2.ItemIndex = 0) then begin
               Limits.XGridLow := Col-dx;
               Limits.XGridHigh := Col + dx;
               Limits.YGridLow := Row - dy;
               Limits.YGridHigh := Row + dy;
              DEMGlb[MapOwner.MapDraw.DEMonMap].BoxAreaExtremeElevations(Limits,TheMinElev,TheMaxElev,avgz);
              BuildingHeight := z - TheMinElev;
              Result := (BuildingHeight > MDDef.BuildingMinHeight);
           end
           else begin
              //go east
              for x := Col + 1 to Col + dx do begin
                if DEMGlb[MapOwner.MapDraw.DEMonMap].GetSlopeAndAspect(x,Row,SlpAsp) then begin
                   zb[1] := z - SlpAsp.z;
                   s1 := SlpAsp.SlopePercent;
                 //if DEMGlb[MapOwner.MapDraw.DEMonMap].GetElevMeters(x,Row,zb[1]) then begin
                    //s1 := DEMGlb[MapOwner.MapDraw.DEMonMap].SlopePerCent(x,Row);

                    //zb[1] := z - zb[1];
                    if (zb[1] > MDDef.BuildingMinHeight) then begin
                       Rad[1] := (x - Col) * DEMGlb[MapOwner.MapDraw.DEMonMap].AverageXSpace;
                       if DEMGlb[MapOwner.MapDraw.DEMonMap].GetSlopeAndAspect(pred(x),Row,SlpAsp) then begin
                          //s2 := DEMGlb[MapOwner.MapDraw.DEMonMap].SlopePerCent(pred(x),Row);
                          if (SlpAsp.SlopePercent < MDDef.MinRoofEdgeSlope) then exit;
                          goto Found1;
                       end
                       else exit;
                    end
                    else if (s1 > MDDef.BuildingMaxSlope) then exit;
                 end;
              end;
              exit;
              Found1:;
              //go west
              for x := Col - 1 downto Col - dx do begin
                 if DEMGlb[MapOwner.MapDraw.DEMonMap].GetElevMeters(x,Row,zb[2]) then begin
                    s1 := DEMGlb[MapOwner.MapDraw.DEMonMap].SlopePerCent(x,Row);
                    zb[2] := z - zb[2];
                    if (zb[2] > MDDef.BuildingMinHeight) then begin
                       Rad[2] := (Col - x) * DEMGlb[MapOwner.MapDraw.DEMonMap].AverageXSpace;
                       if (DEMGlb[MapOwner.MapDraw.DEMonMap].SlopePerCent(succ(x),Row) < MDDef.MinRoofEdgeSlope) then exit;
                       goto Found2;
                    end
                    else if (s1 > MDDef.BuildingMaxSlope) then exit;
                 end;
              end;
              exit;
              Found2:;
              //go south
              for y := Row - 1 downto Row - dy do begin
                 if DEMGlb[MapOwner.MapDraw.DEMonMap].GetElevMeters(Col,y,zb[3]) then begin
                    s1 := DEMGlb[MapOwner.MapDraw.DEMonMap].SlopePerCent(Col,y);
                    zb[3] := z - zb[3];
                    if (zb[3] > MDDef.BuildingMinHeight) then begin
                       Rad[3] := (Row - y) * DEMGlb[MapOwner.MapDraw.DEMonMap].AverageYSpace;
                       if (DEMGlb[MapOwner.MapDraw.DEMonMap].SlopePerCent(Col,succ(y)) < MDDef.MinRoofEdgeSlope) then exit;
                       goto Found3;
                    end
                    else if (s1 > MDDef.BuildingMaxSlope) then exit;
                 end;
              end;
              exit;
              Found3:;
              //go north
              for y := Row + 1 to Row + dy do begin
                 if DEMGlb[MapOwner.MapDraw.DEMonMap].GetElevMeters(Col,y,zb[4]) then begin
                    s1 := DEMGlb[MapOwner.MapDraw.DEMonMap].SlopePerCent(Col,y);
                    zb[4] := z - zb[4];
                    if (zb[4] > MDDef.BuildingMinHeight) then begin
                       Rad[4] := (y - Row) * DEMGlb[MapOwner.MapDraw.DEMonMap].AverageYSpace;
                       if (DEMGlb[MapOwner.MapDraw.DEMonMap].SlopePerCent(Col,Pred(y)) < MDDef.MinRoofEdgeSlope) then exit;
                       goto Found4;
                    end
                    else if (s1 > MDDef.BuildingMaxSlope) then exit;
                 end;
              end;
              exit;
              Found4:;
              Rad[1] := rad[1] + Rad[2];  //EW spacing
              Rad[3] := rad[3] + rad[4];  //NS spacing
              MinOfPairFirst(Rad[1],Rad[3]);
              if (Rad[1] < MDDef.MinBldgSize) then exit;
              BuildingHeight := Petmath.MaxinArray(4,zb);
              Result := true;
           end;

            if RadioGroup2.ItemIndex = 0 then begin
               TStr := RealToString(x* DEMGlb[MapOwner.MapDraw.DEMonMap].AverageXSpace,-12,-2);
            end
            else begin
               TStr := RealToString(rad[1],-12,-2) +  ',' + RealToString(rad[3],-12,-2);
            end;
            DEMGlb[MapOwner.MapDraw.DEMonMap].DEMGridToLatLongDegree(Col,Row,Lat,Long);
            SpireResults.Add(RealToString(Lat,-18,8) + ',' + RealToString(Long,-18,8) + ',' + RealToString(BuildingHeight,-18,2) + ',' + TStr {+ ',' + RealToString(Slope,-12,-2)});
            inc(SpiresOnmap);
         end;
      end;
   end;


   procedure MakeCandidateGrid(a1,a2,a3,a4 : float64);
   var
      Col,Row,N : integer;
      Slope,Aspect : float32;
   begin
      GridCand := DEMGlb[MapOwner.MapDraw.DEMonMap].CloneAndOpenGridSetMissing(ByteDEM,'Roof_class',euIntCode);
      DEMGlb[GridCand].ElevationMultiple := 1;
       Col := GridLimits.XGridLow;
       StartProgress('Roofs');
       while Col <= GridLimits.XGridHigh do begin
          UpdateProgressBar((Col-GridLimits.XGridLow)/(GridLimits.XGridHigh-GridLimits.XGridLow));
          Row := GridLimits.YGridLow;
          while Row <= GridLimits.YGridHigh do begin
            if DEMGlb[SlopeGrid].GetElevMeters(Col,Row,Slope) then begin
               if (Slope > MDDef.MinRoofEdgeSlope) then begin
                  if DEMGlb[AspectGrid].GetElevMeters(Col,Row,Aspect) then begin
                     if ((a1 < a2) and (Aspect > a1) and (Aspect < a2)) or ((a1 > a2) and ((Aspect > a1) or (Aspect < a2))) then DEMGlb[GridCand].SetGridElevation(Col,Row,5)
                     else if (Aspect > a3) and (Aspect < a4) then DEMGlb[GridCand].SetGridElevation(Col,Row,3);
                  end;
               end
               else if Slope < MDDef.BuildingMaxSlope then begin
                   DEMGlb[GridCand].SetGridElevation(Col,Row,1);
               end {if slope compare};
            end {if get slope};
            inc(Row);
         end {while row};
         inc(Col);
      end {while col};
      EndProgress;
      n := 0;
      DEMGlb[GridCand].CheckMaxMinElev;
      fName := MDTempDir +'cand.dem';
      DEMGlb[GridCand].WriteNewFormatDEM(fname);
   end;



begin
   UpdateValues;
   DEMDef_routines.SaveBackupDefaults;
   MDDef.SlopeAlg := smSteepestNeighbor;

   if (RadioGroup2.ItemIndex = 2) then begin
      if (SlopeGrid = 0) then SlopeGrid := MakeSingleNewDerivativeMap('S',MapOwner.MapDraw.DEMonMap,0,false);
      if (AspectGrid = 0) then AspectGrid := MakeSingleNewDerivativeMap('A',MapOwner.MapDraw.DEMonMap,0,false);
      if (DropOffGrid = 0) then DropOffGrid := MakeSingleNewDerivativeMap('d',MapOwner.MapDraw.DEMonMap,round(MDDef.MinBldgSize),false);

      if MDDef.OverWriteFeatureDBs then begin
         if (PeakRoofGrid <> 0) then CloseSingleDEM(PeakRoofGrid);
      end;

      PeakRoofGrid := DEMGlb[MapOwner.MapDraw.DEMonMap].CloneAndOpenGridSetMissing(ByteDEM,'Roofs', euIntCode);
      MakeCandidateGrid(220,320,40,140);
      StartProgress('Roofs EW');
      Row := GridLimits.YGridLow;
      while Row <= GridLimits.YGridHigh do begin
         UpdateProgressBar((Row-GridLimits.YGridLow)/(GridLimits.YGridHigh-GridLimits.YGridLow));
         Col := GridLimits.XGridLow;
         while Col <= GridLimits.XGridHigh do begin
            if DEMGlb[GridCand].GetElevMeters(Col,Row,Aspect) then begin
               if round(Aspect) = 5 then begin
                  repeat
                      NumGoodSlope := 0;
                      NumTotal := 0;
                      sg := Col;
                      repeat
                         inc(Col);
                         if DEMGlb[GridCand].GetElevMeters(Col,Row,Aspect) then begin
                            inc(NumTotal);
                            if (Aspect = 1) then inc(NumGoodSlope);
                         end;
                      until (round(Aspect) in [3,5]) or (Col > GridLimits.XGridHigh);

                      x := sg;
                      repeat
                         inc(x);
                         if DEMGlb[DropOffGrid].GetElevMeters(x,Row,Dropoff) then begin
                             if (DropOff < MDDef.BuildingMinHeight) then goto OnGround;
                         end;
                      until ((succ(x)-sg) * DEMGlb[GridCand].AverageXSpace > MDDef.MinBldgSize) or (x > GridLimits.XGridHigh);

                      bs := (Col-sg) * DEMGlb[GridCand].AverageXSpace;
                      if (bs >= MDDef.MinBldgSize) and (bs <= MDDef.MaxBldgSize) then begin
                         for x := sg to Col do DEMGlb[PeakRoofGrid].SetGridElevation(x,Row,1);
                      end;
                  until (round(Aspect) in [3,5]) or (Col > GridLimits.XGridHigh);
               end;
            end;
            OnGround:;
            inc(Col);
         end {while row};
         inc(Row);
      end {while col};
      EndProgress;
      CloseSingleDEM(GridCand);
      DEMGlb[PeakRoofGrid].CheckMaxMinElev;
      DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.GridpointsfromsecondDEMAssignAndDraw(PeakRoofGrid);
      MessageToContinue('EW done');

      MakeCandidateGrid(310,50,130,230);
      StartProgress('Roofs NS');
      Col := GridLimits.XGridLow;
      while Col <= GridLimits.XGridHigh do begin
         UpdateProgressBar((Col-GridLimits.XGridLow)/(GridLimits.XGridHigh-GridLimits.XGridLow));
         Row := GridLimits.YGridLow;
         while Row <= GridLimits.YGridHigh do begin

            if DEMGlb[GridCand].GetElevMeters(Col,Row,Aspect) then begin
               if round(Aspect) = 5 then begin
                  repeat
                      NumGoodSlope := 0;
                      NumTotal := 0;
                      sg := Row;
                      repeat
                         inc(Row);
                         if DEMGlb[GridCand].GetElevMeters(Col,Row,Aspect) then begin
                            inc(NumTotal);
                            if (Aspect = 1) then inc(NumGoodSlope);
                         end;
                      until (round(Aspect) in [3,5]) or (Row > GridLimits.YGridHigh);
                      y := sg;
                      repeat
                         inc(y);
                         if DEMGlb[DropOffGrid].GetElevMeters(Col,y,Dropoff) then begin
                             if DropOff < MDDef.BuildingMinHeight then goto OnGround2;
                         end;
                      until (succ(y-sg) * DEMGlb[GridCand].AverageYSpace > MDDef.MinBldgSize) or (y > GridLimits.YGridHigh);


                      bs := (Row-sg) * DEMGlb[GridCand].AverageYSpace;
                      if DEMGlb[DropOffGrid].GetElevMeters(Col,Row,Dropoff) then begin
                         if (bs >= MDDef.MinBldgSize) and (bs <=  MDDef.MaxBldgSize) then begin
                            for x := sg to Row do DEMGlb[PeakRoofGrid].SetGridElevation(Col,x,1);
                         end;
                      end;
                      OnGround2:;
                  until (round(Aspect) in [3]) or (Row > GridLimits.YGridHigh);
               end;
            end;
            inc(Row);
         end {while row};
         inc(Col);
      end {while col};
      EndProgress;
      CloseSingleDEM(GridCand);

      DEMGlb[PeakRoofGrid].CheckMaxMinElev;
      DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.GridpointsfromsecondDEMAssignAndDraw(PeakRoofGrid);
      MessageToContinue('NS done');

   end
   else begin
       if MDDef.OverWriteFeatureDBs then CloseAndNilNumberedDB(RoofDB);
       SpireResults := tStringList.Create;

       if RadioGroup2.ItemIndex = 0 then TStr := 'RADIUS_M'
       else TStr := 'RAD1_M,RAD2_M';
       SpireResults.Add('LAT,LONG,HEIGHT,' + TStr + ',SLOPE');

       dx := round(MDDef.RoofRegion / DEMGlb[MapOwner.MapDraw.DEMonMap].AverageXSpace);
       dy := round(MDDef.RoofRegion / DEMGlb[MapOwner.MapDraw.DEMonMap].AverageYSpace);

        SpiresOnMap := 0;
         Col := GridLimits.XGridLow;
         StartProgress('Roofs');
         while Col <= GridLimits.XGridHigh do begin
            UpdateProgressBar((Col-GridLimits.XGridLow)/(GridLimits.XGridHigh-GridLimits.XGridLow));
            Row := GridLimits.YGridLow;
            while Row <= GridLimits.YGridHigh do begin
               if (RadioGroup2.ItemIndex = 0) then begin
                  for x := 1 to dx do begin
                    if CouldBeRoof(Col,Row,x,x) then break;
                  end {for x};
               end
               else CouldBeRoof(Col,Row,dx,dy);
              inc(Row);
           end {while row};
           inc(Col);
        end {for col};
        EndProgress;

        fName := NextFileNumber(MDTempDir, DEMGlb[MapOwner.MapDraw.DEMonMap].AreaName + '_roof_rad_' + IntToStr(MDDef.RoofRegion) + '_ht_' + IntToStr(round(MDDef.BuildingMinHeight)) {+ '_slp_' + RealToString(Slope,-8,-2)} + 'v','.csv');
        RoofDB := DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.DisplayAndPurgeStringListDB(SpireResults,fName);
        Memo1.Lines.Add('');
        Memo1.Lines.Add('Flat roof algorithm ' + IntToStr(RadioGroup2.ItemIndex));
        Memo1.Lines.Add('Roof height >= ' + RealToString(MDDef.BuildingMinHeight,-8,-2) + ' m');
        Memo1.Lines.Add('Max roof radius: ' + RealToString(MDDef.RoofRegion,-8,-2) + ' m');
        Memo1.Lines.Add('Roofs in DEM: ' + IntToStr(SpiresOnMap));
        Memo1.Lines.Add('');
    end;
    DEMDef_routines.RestoreBackupDefaults;
end;



procedure TPitSpireForm.BitBtn9Click(Sender: TObject);
var
   DEM,db : integer;
   fName : PathStr;
   PitResults : tStringList;
begin
   {$IfDef RecordPitsSpires} WriteLineToDebugFile('TPitSpireForm.BitBtn9Click in'); {$EndIf}
   UpdateValues;
   Memo1.Lines.Add('');
   Memo1.Lines.Add('Pit depth: ' + RealToString(MDDef.PitHeight,-8,-2) + ' m');
   Memo1.Lines.Add('Pit radius: ' + RealToString(MDDef.PitRadius,-8,-2) + ' m');
   Memo1.Lines.Add('');

   PitResults := tStringList.Create;
   PitResults.Add('DEM,LAT,LONG,PIT_BASE_M,RELIEF_M');

   if CheckBox2.Checked then begin
      for DEM := 1 to MaxDEMDataSets do begin
         if ValidDEM(DEM) then begin
            FindPits(DEM,DEMGlb[DEM].FullDEMGridLimits,PitResults,Memo1);
         end;
      end;
   end
   else begin
      FindPits(UseDEM,DEMGlb[UseDEM].FullDEMGridLimits,PitResults,Memo1);
   end;

   if (PitResults.Count > 2) then begin
      fName := NextFileNumber(MDTempDir,'pits_rad_' + IntToStr(Round(MDDef.PitRadius)) + '_ht_' + IntToStr(Round(MDDef.PitHeight)) + '_v_', '.csv');
      PitResults.SaveToFile('c:\temp\pits.csv');
      db := MapOwner.DisplayAndPurgeStringListDB(PitResults,fName,true);
      GISdb[db].dbOpts.Symbol := MDdef.PitSymbol;
      if CheckBox2.Checked then begin
         GISdb[db].dbOpts.DBAutoShow := dbasColorByString;
         GISdb[db].dbOpts.FloatColorField := 'DEM';
      end;
      {$IfDef RecordPitsSpires} WriteLineToDebugFile('off to plot ' + fName); {$EndIf}
      GISdb[db].RedrawLayerOnMap;
      {$IfDef RecordPitsSpires} WriteLineToDebugFile('end GIS display'); {$EndIf}
   end;
end;

procedure TPitSpireForm.CancelBtnClick(Sender: TObject);
begin
   Close;
end;

procedure TPitSpireForm.CheckBox1Click(Sender: TObject);
begin
    MDDef.LimitSSODirByStrength := CheckBox1.Checked;
end;

procedure TPitSpireForm.CheckBox4Click(Sender: TObject);
begin
   MDdef.OverWriteFeatureDBs := CheckBox4.Checked;
end;

procedure TPitSpireForm.CheckBox8Click(Sender: TObject);
begin
    MDDef.LimitSSODirByFlatness := CheckBox8.Checked;
end;

procedure TPitSpireForm.DrawBuildingEdges;
var
   Col,Row,NumLower,PointsFound : integer;
   Lat,Long : float64;
   z,znw,zw,zsw,zn,zs,zne,ze,zse : float32;
   SpireResults : tStringList;
   fName : PathStr;
begin
   {$IfDef RecordPitsSpires} WriteLineToDebugFile('TPitSpireForm.DrawBuildingEdges'); {$EndIf}
   SpireResults := tStringList.Create;
   SpireResults.Add('LAT,LONG,NUM_LOWER');

   with MapOwner,MapDraw,DEMGlb[DEMonMap] do begin
      if MDDef.OverWriteFeatureDBs then CloseAndNilNumberedDB(EdgeDB);

      PointsFound := 0;
      StartProgressAbortOption('Buidling edges');
         Col := GridLimits.XGridLow;
         while (Col <= GridLimits.XGridHigh) do begin
            UpdateProgressBar((Col-GridLimits.XGridLow)/(GridLimits.XGridHigh-GridLimits.XGridLow));
            Row := GridLimits.YGridLow;
            while (Row <= GridLimits.YGridHigh) do begin
              if SurroundedPointElevs(Col,Row, znw,zw,zsw,zn,z,zs,zne,ze,zse) then begin
                 NumLower := 0;
                 if (z - znw > MDDef.BuildingMinHeight) and (z - znw < MDDef.BuildingMaxHeight) then inc(NumLower);
                 if (z - zw > MDDef.BuildingMinHeight) and (z - zw < MDDef.BuildingMaxHeight) then inc(NumLower);
                 if (z - zne > MDDef.BuildingMinHeight) and (z - zne < MDDef.BuildingMaxHeight) then inc(NumLower);
                 if (z - ze > MDDef.BuildingMinHeight) and (z - ze < MDDef.BuildingMaxHeight) then inc(NumLower);
                 if (z - zn > MDDef.BuildingMinHeight) and (z - zn < MDDef.BuildingMaxHeight) then inc(NumLower);
                 if (z - zsw > MDDef.BuildingMinHeight) and (z - zsw < MDDef.BuildingMaxHeight) then inc(NumLower);
                 if (z - zs > MDDef.BuildingMinHeight) and (z - zs < MDDef.BuildingMaxHeight) then inc(NumLower);
                 if (z - zse > MDDef.BuildingMinHeight) and (z - zse < MDDef.BuildingMaxHeight) then inc(NumLower);
                 if (NumLower >= MDDef.BuildingMinNumLower) and (NumLower <= MDDef.BuildingMaxNumLower) then begin
                    DEMGridToLatLongDegree(Col,Row,Lat,Long);
                    SpireResults.Add(RealToString(Lat,-18,8) + ',' + RealToString(Long,-18,8) + ',' + IntToStr(NumLower));
                    inc(PointsFound);
                 end {if};
              end;
              inc(Row,1);
            end {while row};
            inc(Col,1);
            if WantOut then break;
         end {while col};
      Memo1.Lines.Add('');
      Memo1.Lines.Add('Roof height >= ' + RealToString(MDDef.BuildingMinHeight,-8,-2) + ' m');
      Memo1.Lines.Add('Points lower >=' + IntToStr(MDDef.BuildingMinNumLower) + '  and <='+ IntToStr(MDDef.BuildingMaxNumLower));
      Memo1.Lines.Add('Building edges found: ' + IntToStr(PointsFound));
      EndProgress;
      fName := MDTempDir + DEMGlb[DEMonMap].AreaName + '_ht_' + IntToStr(round(MDDef.BuildingMinHeight)) + '_to_' + IntToStr(round(MDDef.BuildingMaxHeight)) + '_v' +'.csv';
      EdgeDB := DEMGlb[DEMonMap].SelectionMap.DisplayAndPurgeStringListDB(SpireResults,fName);
      //EdgeDB := LastDBLoaded;
   end {with};
end;


procedure TPitSpireForm.Edit21Change(Sender: TObject);
begin
   CheckEditString(Edit21.Text,MDDef.GeomorphBoxSizeMeters);
end;

procedure TPitSpireForm.Edit22Change(Sender: TObject);
begin
   CheckEditString(Edit22.Text,MDDef.MinSSOStrength);
end;

procedure TPitSpireForm.Edit23Change(Sender: TObject);
begin
   CheckEditString(Edit23.Text,MDDef.RoadTrendRegion);
end;

procedure TPitSpireForm.Edit24Change(Sender: TObject);
begin
   CheckEditString(Edit24.Text,MDDef.RoadTrendSensitivity);
end;

procedure TPitSpireForm.Edit25Change(Sender: TObject);
begin
    CheckEditString(Edit25.Text,MDDef.MaxSSOFlat);
end;

procedure TPitSpireForm.Edit28Change(Sender: TObject);
begin
   CloseSingleDEM(DropOffGrid);
end;

procedure TPitSpireForm.FlatroadbtnClick(Sender: TObject);
begin
   PeakRoofBtnClick(Sender);
end;

procedure TPitSpireForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
   CloseSingleDEM(SlopeGrid);
   CloseSingleDEM(AspectGrid);
   CloseSingleDEM(DropoffGrid);
end;

procedure TPitSpireForm.FormCreate(Sender: TObject);
begin
    SpiresDB := 0;
    RoofDB := 0;
    EdgeDB := 0;
    WallDB := 0;

    TrendGrid := 0;
    RoadNeighGrid := 0;
    DropOffGrid := 0;

    PeakedDB  := 0;
    SlopeGrid  := 0;
    AspectGrid  := 0;

    Edit1.Text := RealToString(MDDef.SpireRadius,-12,-1);
    Edit2.Text := RealToString(MDDef.SpireHeight,-12,-1);
    Edit3.Text := RealToString(MDDef.PitRadius,-12,-1);
    Edit4.Text := RealToString(MDDef.PitHeight,-12,-1);
    Edit5.Text := RealToString(MDDef.PeakRadius,-12,-1);
    Edit6.Text := RealToString(MDDef.PeakHeight,-12,-1);

    Edit7.Text := RealToString(MDDef.BuildingMinHeight,-12,-2);
    Edit18.Text := RealToString(MDDef.BuildingMaxHeight,-12,-2);
    Edit8.Text := IntToStr(MDDef.BuildingMinNumLower);
    Edit9.Text := IntToStr(MDDef.BuildingMaxNumLower);

    Edit10.Text := IntToStr(MDDef.PL_LastVacantVoxel);
    Edit11.Text := IntToStr(MDDef.PL_FirstVacantVoxel);
    Edit12.Text := IntToStr(MDDef.PL_MaxOccupiedVoxels);
    Edit13.Text := IntToStr(MDDef.PL_NeighborsRequired);

    Edit14.Text := RealToString(MDDef.AcrossWallHeight,-12,2);
    Edit15.Text := RealToString(MDDef.AlongWallHeight,-12,2);
    Edit16.Text := IntToStr(MDDef.WallRegion);
    Edit19.Text := IntToStr(MDDef.SpireNeighborTolerance);
    Edit20.Text := RealToString(MDDef.BuildingMaxSlope,-12,-2);
    Edit21.Text := IntToStr(MDDef.GeomorphBoxSizeMeters);
    Edit22.Text := RealToString(MDDef.MinSSOStrength,-8,-2);
    Edit23.Text := IntToStr(MDDef.RoadTrendRegion);
    Edit24.Text := IntToStr(MDDef.RoadTrendSensitivity);
    Edit25.Text :=  RealToString(MDDef.MaxSSOFlat,-8,-2);
    Edit27.Text :=  RealToString(MDDef.MinRoofEdgeSlope,-8,-2);
    Edit28.Text :=  RealToString(MDDef.MinBldgSize,-8,-2);
    Edit29.Text := RealToString(MDDef.PeakRoofMinSlope,-12,-2);
    Edit30.Text := RealToString(MDDef.PeakRoofSlopeTol,-12,-2);
    Edit31.Text := RealToString(MDDef.PeakRoofAspectTol,-12,-2);
    Edit32.Text := IntToStr(MDDef.PeakRoofNeighbors);
    Edit33.Text :=  RealToString(MDDef.MaxBldgSize,-8,-2);

    CheckBox1.Checked := MDDef.LimitSSODirByStrength;
    CheckBox4.Checked := MDdef.OverWriteFeatureDBs;

    CheckBox5.Checked := MDDef.PL_PlotOccupied;
    CheckBox6.Checked := MDDef.PL_PlotFirstNeighbor;
    CheckBox7.Checked := MDDef.PL_PlotSecondNeighbor;
    CheckBox8.Checked := MDDef.LimitSSODirByFlatness;

    SymbolOnButton(BitBtn2,MDDef.SpireSymbol);
    SymbolOnButton(BitBtn4,MDDef.PeakSymbol);
    SymbolOnButton(BitBtn3,MDDef.PitSymbol);
    SymbolOnButton(BitBtn6,MDDef.WallSymbol);

    if MDDef.GeomorphMapsFullDEM then RadioGroup1.ItemIndex := 0
    else RadioGroup1.ItemIndex := 1;
    Petmar.CheckFormPlacement(Self);
end;


procedure TPitSpireForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\pit_spire.htm');
end;

procedure TPitSpireForm.UpdateValues;
begin
   MDDef.PL_PlotOccupied := CheckBox5.Checked;
   MDDef.PL_PlotFirstNeighbor := CheckBox6.Checked;
   MDDef.PL_PlotSecondNeighbor := CheckBox7.Checked;

   CheckEditString(Edit1.Text,MDDef.SpireRadius);
   CheckEditString(Edit2.Text,MDDef.SpireHeight);
   CheckEditString(Edit3.Text,MDDef.PitRadius);
   CheckEditString(Edit4.Text,MDDef.PitHeight);
   CheckEditString(Edit5.Text,MDDef.PeakRadius);
   CheckEditString(Edit6.Text,MDDef.PeakHeight);

   CheckEditString(Edit7.Text,MDDef.BuildingMinHeight);
   CheckEditString(Edit18.Text,MDDef.BuildingMaxHeight);
   CheckEditString(Edit8.Text,MDDef.BuildingMinNumLower);
   CheckEditString(Edit9.Text,MDDef.BuildingMaxNumLower);

   CheckEditString(Edit10.Text,MDDef.PL_LastVacantVoxel);
   CheckEditString(Edit11.Text,MDDef.PL_FirstVacantVoxel);
   CheckEditString(Edit12.Text,MDDef.PL_MaxOccupiedVoxels);
   CheckEditString(Edit13.Text,MDDef.PL_NeighborsRequired);

   CheckEditString(Edit14.Text,MDDef.AcrossWallHeight);
   CheckEditString(Edit15.Text,MDDef.AlongWallHeight);
   CheckEditString(Edit16.Text,MDDef.WallRegion);
   CheckEditString(Edit17.Text,MDDef.RoofRegion);
   CheckEditString(Edit19.Text,MDDef.SpireNeighborTolerance);
   CheckEditString(Edit20.Text,MDDef.BuildingMaxSlope);
   CheckEditString(Edit27.Text,MDDef.MinRoofEdgeSlope);
   CheckEditString(Edit28.Text,MDDef.MinBldgSize);
   CheckEditString(Edit29.Text,MDDef.PeakRoofMinSlope);
   CheckEditString(Edit30.Text,MDDef.PeakRoofSlopeTol);
   CheckEditString(Edit31.Text,MDDef.PeakRoofAspectTol);
   CheckEditString(Edit32.Text,MDDef.PeakRoofNeighbors);
   CheckEditString(Edit33.Text,MDDef.MaxBldgSize);

   if (RadioGroup1.ItemIndex = 0) then  begin
      GridLimits := DEMGlb[MapOwner.MapDraw.DEMonMap].FullDEMGridLimits;
   end
   else begin
      GridLimits := DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.MapDraw.MapAreaDEMGridLimits;
   end;

end;

initialization
finalization
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing pit_and_spire out'); {$EndIf}
   {$IfDef RecordPitsSpires} WriteLineToDebugFile('RecordPitsSpires active in pit_and_spire'); {$EndIf}
end.

