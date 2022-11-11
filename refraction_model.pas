unit refraction_model;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2022 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordTimeSteps}
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Math,
  Dialogs,
  System.UITypes,
  DEMMapf,Petmar_types, StdCtrls, Buttons;

type
  TRefractionForm = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Refre: TBitBtn;
    BitBtn3: TBitBtn;
    Edit3: TEdit;
    Label3: TLabel;
    Edit4: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Edit5: TEdit;
    Label6: TLabel;
    Edit6: TEdit;
    BitBtn4: TBitBtn;
    Label7: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn3Click(Sender: TObject);
    procedure RefreClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
  private
    { Private declarations }
     procedure WaveLengthCalculation;
  public
    { Public declarations }
      WavePeriod : float64;
      TheMap : tMapForm;
      Wavelength : array[0..1000] of float32;
  end;

procedure CreateWaveRefraction(MapOwner : tMapForm);

implementation

{$R *.dfm}

uses
   Petmath,Petmar,BaseGraf,DEMDefs,BaseMap,DEMCoord;

const
   NumDepthSteps = 1000;
var
   Coords   : ^tdCoords;
   DistBetweenOrthogonals,NumOrthogonals,StepsBetweenCrests : integer;
   TimeStep,TotalTime : float64;
   Az  : array[0..500] of float64;


procedure TRefractionForm.BitBtn1Click(Sender: TObject);
var
   ThisGraph : tThisBaseGraph;
   v : array[1..2] of float32;
   rfile : file;
   x : integer;
begin
   WaveLengthCalculation;
   ThisGraph := TThisBaseGraph.Create(Application);
   ThisGraph.SetUpGraphForm;
   ThisGraph.Caption := 'Wavelength';
   if Sender = BitBtn1 then ThisGraph.GraphDraw.HorizLabel := 'Wavelength (m)'
   else ThisGraph.GraphDraw.HorizLabel := 'Speed (m/sec)';
   ThisGraph.GraphDraw.VertLabel := 'Depth (m)';
   ThisGraph.GraphDraw.NormalCartesianY := false;
   ThisGraph.OpenPointFile(rfile,ThisGraph.Symbol);

   for x := 1 to NumDepthSteps do begin
       v[2] := x;
       if (Sender = BitBtn1) then v[1] := WaveLength[x]
       else v[1] := WaveLength[x] / WavePeriod;
       BlockWrite(rfile,v,1);
   end;
   CloseFile(rfile);
   ThisGraph.AutoScaleAndRedrawDiagram;
end;



procedure TRefractionForm.BitBtn3Click(Sender: TObject);
begin
   Close;
end;

procedure TRefractionForm.BitBtn4Click(Sender: TObject);
var
   i,k  : integer;
   z1,z2 : float32;
   Lat1,Long1,Lat2,Long2,Dist    : float64;
begin
   ShowHourglassCursor;
   for k := 1 to StepsBetweenCrests do begin
      TotalTime := TotalTime + TimeStep/60/60;
      for i := 0 to NumOrthogonals do if Az[i] > -998 then begin
         VincentyPointAtDistanceBearing(Coords^[i].Lat,Coords^[i].Long,0.25*DistBetweenOrthogonals,Az[i] - 90,Lat1,Long1);
         VincentyPointAtDistanceBearing(Coords^[i].Lat,Coords^[i].Long,0.25*DistBetweenOrthogonals,Az[i] + 90,Lat2,Long2);

         if DEMGlb[TheMap.MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat1,Long1,z1) and (z1 < 0) and
            DEMGlb[TheMap.MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat2,Long2,z2) and (z1 < 0) then begin
            //d := round(abs(z));

            Dist := sqrt(9.8 * abs(z1)) * TimeStep;
            VincentyPointAtDistanceBearing(Lat1,Long1,Dist,Az[i],Lat1,Long1);

            Dist := sqrt(9.8 * abs(z2)) * TimeStep;
            VincentyPointAtDistanceBearing(Lat2,Long2,Dist,Az[i],Lat2,Long2);
            Coords^[i].Lat := 0.5 * (Lat1+ Lat2);
            Coords^[i].Long := 0.5 * (Long1+ Long2);
            VincentyCalculateDistanceBearing(Lat1,long1,Lat2,Long2,Dist,Az[i]);
            Az[i] := Az[i] - 90;
         end
         else Az[1] := -999;
      end;
   end;

   for i := 0 to NumOrthogonals do if Az[i] > -998 then begin
      TheMap.MapDraw.MapSymbolAtLatLongDegree(TheMap.Image1.Canvas,Coords^[i].Lat,Coords^[i].Long,MDDef.KeyLocationSymbol);
   end;
   Label7.Caption := 'hrs=' + RealToString(TotalTime,-12,3);
   ShowDefaultCursor;
end;

procedure TRefractionForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Dispose(Coords);
   Action := caFree;
end;


procedure TRefractionForm.FormCreate(Sender: TObject);
begin
   New(Coords);
end;


procedure TRefractionForm.RefreClick(Sender: TObject);
var
   i  : integer;
   StartLat,StartLong,WavePropogation : float64;
begin
   WaveLengthCalculation;
   CheckEditString(Edit2.Text,TimeStep);
   CheckEditString(Edit3.Text,StepsBetweenCrests);
   CheckEditString(Edit4.Text,NumOrthogonals);
   CheckEditString(Edit5.Text,DistBetweenOrthogonals);
   CheckEditString(Edit6.Text,WavePropogation);
   StartLat := -30;
   StartLong := -90;

   VincentyPointAtDistanceBearing(StartLat,StartLong,NumOrthogonals div 2 * DistBetweenOrthogonals,WavePropogation-90, Coords^[0].Lat,Coords^[0].Long);

   for I := 1 to NumOrthogonals do begin
      VincentyPointAtDistanceBearing(Coords^[pred(i)].Lat,Coords^[pred(i)].Long,DistBetweenOrthogonals,WavePropogation+90, Coords^[i].Lat,Coords^[i].Long);
   end;

   for i := 0 to NumOrthogonals do begin
      Az[i] := WavePropogation;
      TheMap.MapDraw.MapSymbolAtLatLongDegree(TheMap.Image1.Canvas,Coords^[i].Lat,Coords^[i].Long,MDDef.KeyLocationSymbol);
   end;
   BitBtn4Click(Sender);
end;



procedure TRefractionForm.WaveLengthCalculation;
{iteratively calculate WaveLength at given depths from period; done only once, with results stored}
const
   Tol = 0.05;
var
   Depth : integer;
   Lo,c,
   TwoPiDepth,Th,v,
   Estimate,NewEstimate : float64;
begin
   CheckEditString(Edit1.Text,WavePeriod);
   Estimate := 9.8 / TwoPi * Sqr(WavePeriod);
   Lo := 9.8 / TwoPi * Sqr(WavePeriod);
   NewEstimate := Estimate;
   StartProgress('Wave length model');
   for Depth := NumDepthSteps  downto 1 do begin
      if (Depth mod 5 = 0) then UpDateProgressBar((NumDepthSteps - Depth) / NumDepthSteps);
      if Depth < 0.0152 * Lo then begin
         c := sqrt(9.8 * Depth);
         WaveLength[Depth] := c * WavePeriod;
      end
      else begin
         TwoPiDepth := TwoPi * Depth;
         repeat
            WaveLength[Depth] := NewEstimate;
            v := TwoPiDepth / NewEstimate;
            Th := TanH(v);
            NewEstimate := Estimate * Th;
         until abs(WaveLength[Depth] - NewEstimate) < Tol;
      end;
   end {for Column};
   WaveLength[0] := 0;
   EndProgress;
end;


procedure CreateWaveRefraction(MapOwner : tMapForm);
var
  RefractionForm: TRefractionForm;
begin
   RefractionForm := TRefractionForm.Create(Application);
   RefractionForm.TheMap := MapOwner;
   RefractionForm.Show;
end;


initialization
finalization
   {$IfDef RecordTimeSteps}  WriteLineToDebugFile('RecordTimeSteps active in refraction_model'); {$EndIf}
end.
