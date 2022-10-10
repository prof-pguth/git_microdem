unit fresnel_block_form;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}
   //{$Define RecordFresnelProblems}
{$EndIf}


interface

uses
//needed for inline of core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
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
//end for inline of the core DB functions

  Windows, Messages, SysUtils, Classes, Graphics,  Forms,
  Dialogs,  Buttons,
  System.Math,
  demdefs,petmar_types,DEMMapf, Vcl.StdCtrls, Vcl.Controls;

type
  TFres_blockf = class(TForm)
    Memo1: TMemo;
    Sensor: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Edit1: TEdit;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    procedure BitBtn4Click(Sender: TObject);
    procedure SensorClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    AntLat,AntLong : float64;
    LastLat,LastLong : float64;
    FresDB : integer;
    BaseMap : tMapForm;
    wfan : tWeaponsFan;
  end;


procedure FresnelBlockage(Map : tMapForm; FirstAntenna : boolean; Lat,Long : float64);


implementation

{$R *.dfm}

uses
   DEMLOSw,DEMLos_draw,petmar,demdef_routines,DEMCoord, PETImage,PetDBUtils,Make_tables,
   {$IfDef ExViewshed}
   {$Else}
   DEMWeapn,
   {$EndIf}
   {$IfDef ExGIS}
   {$Else}
   demdatabase,
   {$EndIf}
  BaseMap;

var
  Fres_blockf : TFres_blockf;


procedure FresnelBlockage(Map : tMapForm; FirstAntenna : boolean; Lat,Long : float64);
{$IfDef ExFresnel}
begin
{$Else}
var
   LosResult : tLOSResult;
   fName : PathStr;
   Dist,Bearing : float64;
begin
   {$IfDef RecordFresnelProblems} WriteLineToDebugFile('FresnelBlockage in');  {$EndIf}
    if (Fres_blockf = Nil) then begin
      {$IfDef RecordFresnelProblems} WriteLineToDebugFile('Create form'); {$EndIf}
       Fres_blockf := TFres_blockf.Create(Application);
       MDDef.wf.FanCurvAlg := vcRadioLineOfSight;
       Fres_blockf.BaseMap := Map;
       Fres_blockf.Edit1.Text := RealToString(MDDef.FresnelFreq,-12,-2);
       InitializeWeaponsFan(Fres_blockf.wFan);
       Fres_blockf.Show;
    end;

    if FirstAntenna then with Fres_blockf do begin
       Fres_blockf.AntLat := Lat;
       Fres_blockf.AntLong := Long;
       Fres_blockf.Memo1.Lines.Add('First antenna: ' + LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod));
       wFan.W_Lat := lat;
       wFan.W_Long := long;
       BaseMap.MapDraw.MapSymbolAtLatLongDegree(BaseMap.Image1.Canvas,Lat,Long,FilledBox,5,claLime);
       fName := Petmar.NextFileNumber(MDTempDir, DEMGlb[BaseMap.MapDraw.DEMonMap].AreaName + '_fresnel_',DefaultDBExt);
       MakeFresnelPointsTable(fName);
       Fres_blockf.FresDB := BaseMap.LoadDataBaseFile(fName);
       //Fres_blockf.FresDB := LastDBLoaded;
       GISdb[FresDB].dbOpts.DBAutoShow := dbasColorField;
       GISdb[FresDB].MyData.Insert;
       GISdb[FresDB].MyData.SetFieldByNameAsFloat('LAT',Lat);
       GISdb[FresDB].MyData.SetFieldByNameAsFloat('LONG',Long);
       GISdb[FresDB].MyData.SetFieldByNameAsInteger('COLOR',clBlack);
       GISdb[FresDB].MyData.SetFieldByNameAsString('NAME','Sensor');
       GISdb[FresDB].MyData.Post;
    end
    else with Fres_blockf do begin
       {$IfDef RecordFresnelProblems} WriteLineToDebugFile('Second point'); {$EndIf}
       LastLat := Lat;
       LastLong := Long;
       LOSResult := LOSComputeOnly(Fres_blockf.BaseMap.MapDraw.DEMonMap,DEMGlb[Fres_blockf.BaseMap.MapDraw.DEMonMap].VegGrid[1],
             AntLat,AntLong,Lat,Long,MDDef.ObsAboveGround,MDDef.TargetAboveGround,Fres_blockf.FresDB);
       //Table.Destroy;
       BaseMap.MapDraw.MapSymbolAtLatLongDegree(BaseMap.Image1.Canvas,Lat,Long,FilledBox,3,FresnelZoneColor(LosResult));
       Fres_blockf.Memo1.Lines.Add('  ' + LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod) + '  ' + FresnelZoneResult(LosResult));
       GISdb[FresDB].MyData.Insert;
       GISdb[FresDB].MyData.SetFieldByNameAsFloat('LAT',Lat);
       GISdb[FresDB].MyData.SetFieldByNameAsFloat('LONG',Long);
       GISdb[FresDB].MyData.SetColorFromPlatformColor(FresnelZoneColor(LosResult));
       GISdb[FresDB].MyData.SetFieldByNameAsString('NAME',FresnelZoneResult(LosResult));

       GISdb[FresDB].MyData.SetFieldByNameAsFloat('SENSOR_LAT',wFan.W_Lat);
       GISdb[FresDB].MyData.SetFieldByNameAsFloat('SENSOR_LON',wFan.W_Long);
       GISdb[FresDB].MyData.SetFieldByNameAsFloat('SENSOR_UP',MDDef.ObsAboveGround);
       GISdb[FresDB].MyData.SetFieldByNameAsFloat('TARGET_UP',MDDef.TargetAboveGround);
       VincentyCalculateDistanceBearing(wFan.W_Lat,wFan.W_Long,Lat,Long,Dist,Bearing);
       GISdb[FresDB].MyData.SetFieldByNameAsFloat('RANGE_KM',0.001 * Dist);
       GISdb[FresDB].MyData.SetFieldByNameAsFloat('FREQ_MHZ',MDDef.FresnelFreq);

       GISdb[FresDB].MyData.Post;
    end;
   {$IfDef RecordFresnelProblems} WriteLineToDebugFile('FresnelBlockage out'); {$EndIf}
{$EndIf}
end;


procedure TFres_blockf.BitBtn1Click(Sender: TObject);
begin
    Fres_blockf.BaseMap.MapDraw.AddFanToMap(wFan);
end;


procedure TFres_blockf.BitBtn2Click(Sender: TObject);
begin
   ChangeDEMNowDoing(FirstFresnelPoint);
end;

procedure TFres_blockf.BitBtn3Click(Sender: TObject);
begin
   StartLOS(True,JustWandering,BaseMap.MapDraw.DEMonMap,AntLat,AntLong,LastLat,LastLong,BaseMap,true);
end;

procedure TFres_blockf.BitBtn4Click(Sender: TObject);
{$IfDef ExFresnel}
begin
{$Else}
var
   Bitmap : tMyBitmap;
   Lat,Long : float64;
   x,y : Integer;
   Table : tMyData;
   LosResult : tLOSResult;
begin
   PetImage.CloneImageToBitmap(Fres_blockf.BaseMap.Image1,Bitmap);
   StartProgress('Map');
   for y := 0 to pred(Bitmap.Height) do begin
      UpdateProgressBar(y/Bitmap.Height);
      for x := 0 to pred(Bitmap.Width) do begin
         Fres_blockf.BaseMap.MapDraw.ScreenToLatLongDegree(x,y,Lat,Long);
         LOSResult := LOSComputeOnly(Fres_blockf.BaseMap.MapDraw.DEMonMap,DEMGlb[Fres_blockf.BaseMap.MapDraw.DEMonMap].VegGrid[1], AntLat,AntLong,Lat,Long,
            MDDef.ObsAboveGround,MDDef.TargetAboveGround,Fres_blockf.FresDB);
         Table.Destroy;
         Bitmap.Canvas.Pixels[x,y] := ConvertPlatformColorToTColor(FresnelZoneColor(LosResult));
      end;
   end;
   EndProgress;
   Fres_blockf.BaseMap.IHSmergeOntoMap(Bitmap);
{$EndIf}
end;


procedure TFres_blockf.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDDef.FresnelFreq);
end;


procedure TFres_blockf.SensorClick(Sender: TObject);
begin
   GetWeaponParameters(1,wFan);
end;

initialization
   Fres_blockf := Nil;
finalization
   {$IfDef RecordFresnelProblems} WriteLineToDebugFile('RecordFresnelProblems active in Fresnel_block_form'); {$EndIf}
end.
