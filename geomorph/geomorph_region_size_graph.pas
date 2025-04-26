unit geomorph_region_size_graph;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}



{$I nevadia_defines.inc}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}



interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  BaseGraf,DEMDefs,DEMDef_routines,Petmar_types;


type
   tRegionSizeParameter = (rsRelief,rsOpenUp,rsOpenDown,rsOpenDiff,rsSummit,rsBaseLevel,rsGeoRelief,rsDropoff,rsElevRelf,
      rsElevMoment,rsSlopeMoment{,rsPlanCurveMoment,rsProfCurveMoment});

type
  Tregionsizeform = class(TForm)
    RadioGroup1: TRadioGroup;
    Label1: TLabel;
    Edit1: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    RadioGroup2: TRadioGroup;
    Edit2: TEdit;
    Label2: TLabel;
    Edit3: TEdit;
    Label3: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Param : tRegionSizeParameter;
    CurDEM : integer;
    Lat,Long : float64;
  function GeomorphParameterVersusRegion(aLat,aLong : float64) : TThisBaseGraph;
end;


procedure MakeGraphsByRegionSize(CurDEM : integer; Lat,Long : float64);

var
   regionsizeform: Tregionsizeform;


implementation

{$R *.dfm}

uses
   DEMcoord,DEMstat,Petmar,Petmath,DEMMapf,
   BaseMap,
   Nevadia_Main;


procedure MakeGraphsByRegionSize;
begin
   regionsizeform := Tregionsizeform.Create(Application);
   regionsizeform.Lat := Lat;
   regionsizeform.Long := Long;
   regionsizeform.CurDEM := CurDEM;
   regionsizeform.Show;
end;




function Tregionsizeform.GeomorphParameterVersusRegion(aLat,aLong : float64) : TThisBaseGraph;
var
   MomentVar : tMomentVar;
   Col,Row,RegionM  : Integer;
   SumRelief : float64;
   rFile : file;
   TStr1,TStr2 : string35;
   v : tGraphPoint32;
   zvs : ^bfarray32;


      function PerformPointComputations(Col,Row : integer) : boolean;
      var
         Relief,Summit,BaseLevel,GeoRelief,Dropoff,Elev_relf,z : float32;
         Upward,Downward,Relief1 : float64;
         SlopeAspectRec : tSlopeAspectRec;
         PixelsNS,PixelsEW,PixelsDia : integer;
      begin
         if Param in [rsOpenUp,rsOpenDown,rsOpenDiff] then begin
            PixelsNS := round(RegionM / DEMGlb[CurDEM].AverageYSpace);
            PixelsEW := round(RegionM / DEMGlb[CurDEM].AverageXSpace);
            PixelsDia := round(RegionM / DEMGlb[CurDEM].AverageDiaSpace);
            Result := DEMGlb[CurDEM].FigureOpenness(Col,Row,PixelsNS,PixelsEW,PixelsDia,Upward,Downward);
            if Param in [rsOpenUp] then Relief1 := Upward
            else if Param in [rsOpenDown] then Relief1 := Downward
            else Relief1 := Upward-Downward;
         end
         else if Param in [rsElevMoment,rsSlopeMoment{,rsPlanCurveMoment,rsProfCurveMoment}] then begin
             if Param in [rsElevMoment] then begin
                Result := DEMGlb[CurDEM].GetElevMeters(Col,Row,z);
             end
             else begin
                if Param in [rsSlopeMoment] then begin
                   Result := DEMGlb[CurDEM].GetSlopeAndAspect(MDDef.SlopeCompute,Col,Row,SlopeAspectRec);
                   z := SlopeAspectRec.SlopePercent;
                end
                {$IfDef MultipleCurvatureMethods}
                   else if DEMGlb[CurDEM].IsSurroundedPoint(Col,Row) then  begin
                       Result := true;
                       if DEMGlb[CurDEM].GetEvansParams(Col,Row,MDDef.WoodRegionRadiusPixels,MaxSlp,SlopeCurvature,PlanCurvature,crossc,MaxCurve,MinCurve) then begin
                          if Param in [rsPlanCurveMoment] then z := PlanCurvature;
                          if Param in [rsProfCurveMoment] then z := SlopeCurvature;
                       end;
                   end
                {$EndIf}
                else Result := false;
             end;

              if Result then begin
                zvs^[MomentVar.NPts] := z;
                inc(MomentVar.NPts);
              end;
             exit;
         end
         else begin
            Result := DEMGlb[CurDEM].QuickRelief(Col,Row,RegionM,Relief,Summit,BaseLevel,GeoRelief,Dropoff,Elev_relf);
            if (Param = rsSummit) then Relief1 := Summit
            else if (Param = rsRelief) then Relief1 := Relief
            else if (Param = rsBaseLevel) then Relief1 := BaseLevel
            else if (Param = rsGeoRelief) then Relief1 := GeoRelief
            else if (Param = rsDropoff) then Relief1 := Dropoff
            else if (Param = rsElevRelf) then Relief1 := Elev_relf;
         end;
         if Result then begin
            inc(MomentVar.NPts);
            SumRelief := SumRelief + Relief1;
         end;
      end;

var
   x,y,MinX,MaxX,MinY,MaxY,NewMinX,NewMaxX,NewMinY,NewMaxY : integer;
   First : boolean;
begin
   Param := tRegionSizeParameter(RadioGroup1.ItemIndex);
   Lat := aLat;
   Long := aLong;
   DEMGlb[CurDEM].LatLongDegreeToDEMGridInteger(Lat,Long,Col,Row);
   if Param in [rsElevMoment,rsSlopeMoment{,rsPlanCurveMoment,rsProfCurveMoment}] then begin
      New(zvs);
   end;

   case RadioGroup2.ItemIndex of
      0 : TStr1 := 'average';
      1 : TStr1 := 'std dev';
      2 : TStr1 := 'skewness';
      3 : TStr1 := 'kurtosis';
   end;

   if Param = rsRelief then begin
      TStr1 := 'Relief';
      TStr2 := TStr1 + ' (m)';
   end
   else if Param = rsOpenUp then begin
      TStr1 := 'Upward openness';
      TStr2 := TStr1 + '°';
   end
   else if Param = rsOpenDown then begin
      TStr1 := 'Downward openness';
      TStr2 := TStr1 + '°';
   end
   else if Param = rsOpenDiff then begin
      TStr1 := 'Difference openness';
      TStr2 := TStr1 + '°';
   end
   else if Param = rsSummit then begin
      TStr1 := 'Summit';
      TStr2 := TStr1 + ' (m)';
   end
   else if Param = rsBaseLevel  then begin
      TStr1 := 'Base level';
      TStr2 := TStr1 + ' (m)';
   end
   else if Param = rsGeoRelief  then begin
      TStr1 := 'Geophysical relief';
      TStr2 := TStr1 + ' (m)';
   end
   else if Param = rsDropoff  then begin
      TStr1 := 'Dropoff';
      TStr2 := TStr1 + ' (m)';
   end
   {$IfDef MultipleCurvatureMethods}
      else if Param = rsPlanCurveMoment then begin
         TStr1 := 'Plan Curvature ' + TStr1;
         TStr2 := TStr1;
      end
      else if Param = rsProfCurveMoment then begin
         TStr1 := 'Profile Curvature ' + TStr1;
         TStr2 := TStr1;
      end
   {$EndIf}
   else if Param = rsElevMoment then begin
      TStr1 := 'Elevation ' + TStr1;
      TStr2 := TStr1 + ' (m)';
   end
   else if Param = rsSlopeMoment then begin
      TStr1 := 'Slope ' + TStr1;
      TStr2 := TStr1 + ' (%)';
   end;

   Result := TThisBaseGraph.Create(Application);
   with Result,GraphDraw do begin
      MaxHorizAxis := MDdef.LastBoxSize;
      MaxVertAxis := (DEMGlb[CurDEM].DEMheader.MaxElev - DEMGlb[CurDEM].DEMheader.MinElev);
      PadAxis(MinVertAxis,MaxVertAxis);
      Caption := TStr1 + ' versus Region Size';  // +  TStr;
      HorizLabel := 'Region Box Size (m)';
      VertLabel := TStr2;
      SetUpGraphForm;
      OpenPointFile(rfile,Result.Symbol);
   end;
   First := true;
   StartProgressAbortOption(TStr1);
   MomentVar.NPts := 0;
   SumRelief := 0;
   RegionM := MDDef.FirstBoxSize;
   while (RegionM < MDDef.LastBoxSize) do begin
      UpdateProgressBar(RegionM/MDDef.LastBoxSize);
      if First then begin
         Minx := Col - round(0.5*MDDef.FirstBoxSize / DEMGlb[CurDEM].AverageXSpace);
         MaxX := Col + round(0.5*MDDef.FirstBoxSize / DEMGlb[CurDEM].AverageXSpace);
         MinY := Row - round(0.5*MDDef.FirstBoxSize / DEMGlb[CurDEM].AverageYSpace);
         MaxY := Row + round(0.5*MDDef.FirstBoxSize / DEMGlb[CurDEM].AverageYSpace);
         First := false;
            x := Minx;
            while x <= MaxX do begin
               y := MinY;
               while Y <= MaxY do begin
                  PerformPointComputations(Col,Row);
                  inc(y);
               end {while Row};
               inc(x);
            end {while Col};
      end
      else begin
         //strip on left of analysis region
         NewMinX := Col - round(0.5*RegionM / DEMGlb[CurDEM].AverageXSpace);
         for y := MinY to MaxY do PerformPointComputations(NewMinX,y);
         //new strip on right of analysis region
         NewMaxX := Col + round(0.5*RegionM / DEMGlb[CurDEM].AverageXSpace);
         for y := MinY to MaxY do PerformPointComputations(NewMAxX,y);
         //new strips on Bottom
         NewMinY := Row - round(0.5*RegionM / DEMGlb[CurDEM].AverageYSpace);
         if NewMinY <> MinY then begin
            for y := NewMinY to pred(MinY) do
               for x := NewMinX to NewMaxX do PerformPointComputations(x,y);
         end;
         //new strips on top
         NewMaxY := Row + round(0.5*RegionM / DEMGlb[CurDEM].AverageYSpace);
         if NewMaxY <> MaxY then begin
            for y := NewMinY to pred(MinY) do
               for x := NewMinX to NewMaxX do PerformPointComputations(x,y);
         end;
         (*
         Minx := NewMinX;
         MaxX := NewMaxX;
         MinY := NewMinY;
         MaxY := NewMaxY;
         *)
      end;

      if (MomentVar.NPts > 0) then begin
         if Param in [rsElevMoment,rsSlopeMoment{,rsPlanCurveMoment,rsProfCurveMoment}] then begin
            moment(zvs^,MomentVar,msBeforeMedian);
            case RadioGroup2.ItemIndex of
               0 : v[2] := MomentVar.mean;
               1 : v[2] := MomentVar.std_dev;
               2 : v[2] := MomentVar.skew;
               3 : v[2] := MomentVar.curt;
            end;
         end
         else begin
           v[2] := SumRelief / MomentVar.NPts;
         end;
         v[1] := RegionM;
         BlockWrite(rfile,v,1);
      end;
      RegionM := RegionM + round(2 * DEMGlb[CurDEM].AverageXSpace);
   end;
   CloseFile(rfile);
   Result.AutoScaleAndRedrawDiagram;
   wmdem.FormPlacementInCorner(Result,lpNEMap);
   EndProgress;
   if Param in [rsElevMoment,rsSlopeMoment{,rsPlanCurveMoment,rsProfCurveMoment}] then begin
      Dispose(zvs);
   end;
   {$IfDef RecordGeostat} WriteLineToDebugFile('GeomorphParameterVersusRegion out'); {$EndIf}
end;


procedure Tregionsizeform.RadioGroup1Click(Sender: TObject);
begin
   GeomorphParameterVersusRegion(Lat,Long);
end;

procedure Tregionsizeform.RadioGroup2Click(Sender: TObject);
begin
   GeomorphParameterVersusRegion(Lat,Long);
end;


procedure Tregionsizeform.BitBtn1Click(Sender: TObject);
begin
   GeomorphParameterVersusRegion(Lat,Long);
end;


procedure Tregionsizeform.BitBtn2Click(Sender: TObject);
begin
   ChangeDEMNowDoing(GeomorphPointGraph);
end;

procedure Tregionsizeform.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDDef.LastBoxSize);
end;

procedure Tregionsizeform.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDDef.FirstBoxSize);
end;

procedure Tregionsizeform.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,MDDef.BoxSizeIncr);
end;

procedure Tregionsizeform.FormCreate(Sender: TObject);
begin
   Edit1.Text := MDDef.LastBoxSize.ToString;
   Edit2.Text := IntToStr(MDDef.FirstBoxSize);
   Edit3.Text := IntToStr(MDDef.BoxSizeIncr);
   wmDEM.FormPlacementInCorner(self);
end;


initialization
   regionsizeform := Nil;
finalization
end.
