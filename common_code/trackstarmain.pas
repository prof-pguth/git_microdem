unit trackstarmain;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordSatTrack}
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,ExtCtrls, Buttons,
  Petmar_types,DEMMapf,NetMainW, Grids;

type
  TSatTractForm = class(TForm)
    Timer1: TTimer;
    Panel2: TPanel;
    Edit1: TEdit;
    Label2: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
     NetBMP : tMyBitmap;
  public
    { Public declarations }
     fn1        : PathStr;
     MinApart   : integer;
     MapOwner   : tMapForm;
     NetForm    : tNetForm;
     ObsLat,ObsLong,ObsElev : float64;
     BaseBitMap : tMyBitmap;
  end;

var
  SatTractForm : TSatTractForm;

implementation

{$R *.DFM}

uses
   {$IfDef ExTrackSat}
   {$Else}
      Support,
      SGP_In,
      SGP_Time,SGP_Math,
      SGP_Conv,Solar,
      SGP4SDP4,SGP_Obs,
   {$EndIf}
   PETMAR,
   PETMath,
   DEMDefs,
   BaseMap,
   DEMCoord,
   Sun_position,
   Nevadia_main, PETImage;

var
  nr_sats : integer;
  jtime1,dt  : double;


procedure TSatTractForm.Button1Click(Sender: TObject);
var
   wYear,wmonth,wDay : word;
begin
  if FileExists(fn1) then begin
     nr_sats := Input_Satellite_Data(fn1);
     DecodeDate(now,wYear,wmonth,wDay);

     jtime1 := PETMath.JulDay(wMonth,wDay,wYear);
     MinApart := 15;
     dt := MinApart / 60 / 24;

     PetImage.CopyImagetoBitmap(MapOwner.Image1,BaseBitmap);
     if (Sender = Nil) then Timer1Timer(Sender)
     else Timer1.Enabled := true;
   end
   else begin
      MessageToContinue('Missing ephemeris ' + fn1);
      Close;
   end;
end;


procedure TSatTractForm.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   Timer1.Enabled := false;
   Action := caFree;
   if ObsLat < -999 then begin
      MapOwner.Closable := true;
      MapOwner.Close;
   end;
   NetBMP.Free;
end;


procedure TSatTractForm.FormCreate(Sender: TObject);
begin
   wmdem.FormPlacementInCorner(Self,lpNEMap);
   fn1 := ProgramRootDir + 'gps-ops.txt';
   NetForm := Nil;
   NetBMP := Nil;
   ObsLat := -9999;
   ObsLong := -9999;
   ObsElev  := -9999;
end;


procedure TSatTractForm.FormResize(Sender: TObject);
begin
   StringGrid1.DefaultColWidth := ClientWidth div StringGrid1.ColCount;
end;


procedure TSatTractForm.Timer1Timer(Sender: TObject);
var
   Passes,i,j,xpic,ypic,Month,Day,Year,xd,yd,k : integer;
   sat_pos,sat_gd,sat_vel : vector;
   Lat,Long,Distance,Bearing,dz,Angle : float64;
   NewBitmap : tMyBitmap;
   TStr : ShortString;
   aSymbol : tFullSymbolDeclaration;
begin
{$IfDef RecordSatTrack}
   WriteLineToDebugFile('TSatTractForm.Timer1Timer in');
{$EndIf}
    if CheckBox1.Checked then Passes := 2 else Passes := 1;
    CloneBitmap(BaseBitmap,NewBitmap);
    NewBitmap.Canvas.Draw(0,0,BaseBitmap);
    if (NetForm <> Nil) then begin
       NetForm.Image1.Picture.Graphic := NetBMP;
       Caption := 'Sky map from ' + LatLongDegreeToString(ObsLat,ObsLong,MDDef.OutPutLatLongMethod);
    end;
    StringGrid1.Cells[0,0] := 'SATELLITE';
    StringGrid1.Cells[1,0] := 'LAT';
    StringGrid1.Cells[2,0] := 'LONG';
    StringGrid1.Cells[3,0] := 'ALT_KM';
    StringGrid1.Cells[4,0] := 'AZIMUTH';
    StringGrid1.Cells[5,0] := 'PITCH';
    k := 1;
    for j := 1 to Passes do begin
       {$IfDef RecordSatTrack}
       WriteLineToDebugFile('j=' + IntToStr(j),true);
       {$EndIf}
       for i := 1 to nr_sats do begin
          {$IfDef RecordSatTrack}
          WriteLineToDebugFile('i=' + IntToStr(i) + '/' + IntToStr(nr_sats));
          {$EndIf}
           Convert_Satellite_Data(i);
           SGP(jtime1,sat_pos,sat_vel);
           Convert_Sat_State(sat_pos,sat_vel);
           Calculate_LatLonAlt(sat_pos,jtime1,sat_gd);

           Lat := sat_gd[1] / DegToRad;
           Long := sat_gd[2] / DegToRad;
           Long := Long + 180;
           while Long > 180 do Long := Long - 360;
           MapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,XPic,YPic);
           if (j = Passes) then begin
              StringGrid1.Cells[0,k] := sat_name[i];
              StringGrid1.Cells[1,k] := RealToString(Lat,10,6);
              StringGrid1.Cells[2,k] := RealToString(Long,11,6);
              StringGrid1.Cells[3,k] := RealToString(sat_gd[3],8,1);
              if (ObsLat > -999) then begin
                 VincentyCalculateDistanceBearing(ObsLat,ObsLong,Lat,Long,Distance,Bearing);
                 dz := 1000 * sat_gd[3] - ObsElev - DropEarthCurve(Distance);
                 Angle := arcTan(dz/Distance) / DegToRad;
                 if (Angle > 0) then begin
                    if (NetForm <> Nil) then begin
                       aSymbol.DrawingSymbol := FilledBox;
                       aSymbol.SIZE := 3;
                       aSymbol.Color := claLime;
                       NetForm.nd.PlotPointOnNet(LinePlot,Angle,Bearing,aSymbol,xd,yd);
                    end;
                    StringGrid1.Cells[4,k] := RealToString(Bearing,8,1);
                    StringGrid1.Cells[5,k] := RealToString(Angle,8,1);
                    inc(k);
                 end;
              end
              else begin
                  inc(k);
              end;

              ScreenSymbol(NewBitmap.Canvas,XPic,YPic,Box,3,claRed);
              TStr := Copy(sat_name[i],14,6);

              NewBitmap.Canvas.Font.Color := clRed;
              NewBitmap.Canvas.TextOut(XPic+5,YPic+5,TStr);
           end
           else ScreenSymbol(BaseBitmap.Canvas,XPic,YPic,Box,2,claSilver);
        end;
        {$IfDef RecordSatTrack}
        WriteLineToDebugFile('done i loop');
        {$EndIf}
     end;
     StringGrid1.RowCount := k;
     StringGrid1.ColCount := 6;

     MapOwner.Image1.Picture.Graphic := NewBitmap;
     NewBitmap.Free;
     CalDat(Trunc(Jtime1),Month,Day,Year);
     //Hours := 24 * Frac(JTime1 + 0.000045);
     //TStr := IntegerToString(Trunc(Hours),2) + IntegerToString(round(Frac(Hours) * 60),2);
     for i := 1 to 3 do if TStr[i] = ' ' then TStr[i] := '0';
     Label1.Caption := IntToStr(Month) + '/' + IntToStr(Day) + '/' + IntToStr(Year) + '  ' + TStr + ' UTC';
     jtime1 := jtime1 + dt;
{$IfDef RecordSatTrack}
   WriteLineToDebugFile('TSatTractForm.Timer1Timer out');
{$EndIf}
end;


procedure TSatTractForm.Edit1Change(Sender: TObject);
begin
    CheckEditString(Edit1.Text,MinApart);
    dt := MinApart / 60 / 24;
end;

procedure TSatTractForm.BitBtn1Click(Sender: TObject);
begin
   Timer1.Enabled := false;
   BitBtn1.Enabled := false;
   BitBtn2.Enabled := true;
end;


procedure TSatTractForm.BitBtn2Click(Sender: TObject);
begin
   Timer1.Enabled := true;
   BitBtn2.Enabled := false;
   BitBtn1.Enabled := true;
end;


procedure TSatTractForm.BitBtn4Click(Sender: TObject);
begin
   BitBtn1Click(nil);
   Petmar.GetFileFromDirectory('satellite orbital elements','*.txt',fn1);
   MapOwner.DoFastMapRedraw;
   FreeAndNil(BaseBitmap);
   PetImage.CopyImagetoBitmap(MapOwner.Image1,BaseBitmap);
   Button1Click(nil);
   BitBtn2Click(nil);
end;



procedure TSatTractForm.BitBtn5Click(Sender: TObject);
begin
   if (NetForm = Nil) then begin
      NetForm := HorizonBlockingGraph(MapOwner,ObsLat,ObsLong,false,false);
   end;
end;


initialization
finalization
{$IfDef RecordSatTrack}
   WriteLineToDebugFile('RecordSatTrack active in trackstarmain');
{$EndIf}

end.
