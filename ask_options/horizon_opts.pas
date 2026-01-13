unit horizon_opts;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
  DEMMapf,Petmar_types,Petmar;

type
  THorizonOptions = class(TForm)
    BitBtn1: TBitBtn;
    OKBtn: TBitBtn;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    GroupBox1: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    CheckBox7: TCheckBox;
    Edit11: TEdit;
    Edit10: TEdit;
    Edit9: TEdit;
    HelpBtn: TBitBtn;
    GroupBox2: TGroupBox;
    Edit6: TEdit;
    Label8: TLabel;
    Edit8: TEdit;
    Label7: TLabel;
    Edit7: TEdit;
    Label6: TLabel;
    Edit5: TEdit;
    Label5: TLabel;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Edit12: TEdit;
    Label12: TLabel;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtn11: TBitBtn;
    GroupBox3: TGroupBox;
    Edit4: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Label4: TLabel;
    Label1: TLabel;
    Edit1: TEdit;
    GroupBox4: TGroupBox;
    CheckBox8: TCheckBox;
    CheckBox1: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox9: TCheckBox;
    BitBtn10: TBitBtn;
    CheckBox10: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
  private
    { Private declarations }
     procedure CheckOptions;
  public
    { Public declarations }
     MapOwner : tMapForm;
     Lat,Long : float64;
     NeedRedraw : boolean;
  end;


implementation

{$R *.dfm}

uses
   {$IfDef ExMICRONET}
   {$Else}
      NetOpts,
   {$EndIf}
   {$IfDef ExGeography}
   {$Else}
      sun_position,
   {$EndIf}
   {$IfDef ExPointCloud}
   {$Else}
      Point_Cloud_Options,
   {$EndIf}
   Nevadia_Main,
   PetImage, PetMath,petmar_db,
   DEMCoord,
   DEMDefs;


procedure THorizonOptions.BitBtn2Click(Sender: TObject);
begin
   CheckOptions;
   if (MapOwner <> nil) then begin
      HorizonBlockingGraph(MapOwner,Lat,Long,MDDef.HorizonVertAngleGraph,MDDef.HorizonDistanceGraph);
      MapOwner.DoFastMapRedraw;
   end;
   if MDDef.DaylightDuration then SunRiseSunSet(Lat,Long,MDDef.ShowToday,false);
   NeedRedraw := false;
end;

procedure THorizonOptions.BitBtn3Click(Sender: TObject);
begin
   {$IfDef ExMICRONET}
   {$Else}
      MicronetOptions(Nil);
   {$EndIf}
end;

procedure THorizonOptions.BitBtn5Click(Sender: TObject);
var
   Jd : integer;
begin
   CheckOptions;
   JD := MDDef.FirstSunLightDay;
   while jd <= MDDef.LastSunLightDay do begin
      HoursSolarIlluminationGrid(MapOwner,jd);
      inc(jd,MDDef.SunlightMapInterval);
   end;
end;

procedure THorizonOptions.BitBtn6Click(Sender: TObject);
begin
   CheckOptions;
   HoursSolarIlluminationGrid(MapOwner,MDDef.SingleJulianDay);
end;


procedure THorizonOptions.BitBtn7Click(Sender: TObject);
begin
   SunRiseSunSet(Lat,Long,false,false);
end;

procedure THorizonOptions.BitBtn8Click(Sender: TObject);
begin
   SunRiseSunSet(Lat,Long,false,true);
end;


procedure THorizonOptions.BitBtn9Click(Sender: TObject);
begin
   GetGridParameters;
end;

procedure THorizonOptions.FormCreate(Sender: TObject);
begin
   MapOwner := nil;
   NeedRedraw := false;
   Edit1.Text := RealToString(MDDef.HorizonLength,-12,0);
   Edit2.Text := RealToString(MDDef.HorizonRadialMultiple,-12,2);
   Edit3.Text := RealToString(MDDef.HorizonIncrement,-12,2);
   Edit4.Text := RealToString(MDDef.ObsAboveGround,-12,2);
   Edit9.Text := RealToString(MDDef.AzToSat,-12,2);
   Edit10.Text := RealToString(MDDef.ElevToSat,-12,2);
   Edit11.Text := IntToStr(MDDef.StatSampleIncr);
   Edit5.Text := IntToStr(MDDef.FirstSunLightDay);
   Edit6.Text := IntToStr(MDDef.LastSunLightDay);
   Edit7.Text := IntToStr(MDDef.SunlightPrecision);
   Edit8.Text := IntToStr(MDDef.SingleJulianDay);
   Edit12.Text := IntToStr(MDDef.SunlightMapInterval);
   CheckBox1.Checked := MDDef.InvertSkyline;
   CheckBox2.Checked := MDDef.HorizonDistanceGraph;
   CheckBox3.Checked := MDDef.HorizonVertAngleGraph;
   CheckBox4.Checked := MDDef.HorizonSkyMap;
   CheckBox5.Checked := MDDef.VerifyTimeZone;
   CheckBox6.Checked := MDDef.SolarPathMap;
   CheckBox7.Checked := MDDef.ShowMaskedToSat;
   CheckBox9.Checked := MDDef.DaylightDuration;
   Petmar.ColorLineWidthBitBtn(BitBtn1,MDDef.HorizonColor,MDDef.HorizonWidth);
   CheckFormPlacement(Self);
end;


procedure THorizonOptions.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\horizon_opts.htm');
end;


procedure THorizonOptions.BitBtn10Click(Sender: TObject);
begin
   CheckOptions;
   HoursSolarIlluminationGrid(MapOwner,AnnualJulianDay(2019,3,21),'Equinoxes_hours_direct_sunlight');
   HoursSolarIlluminationGrid(MapOwner,AnnualJulianDay(2019,6,21),'June_solstice_hours_direct_sunlight');
   HoursSolarIlluminationGrid(MapOwner,AnnualJulianDay(2019,12,21),'Dec_solstice_hours_direct_sunlight');
end;

procedure THorizonOptions.BitBtn11Click(Sender: TObject);
var
   BlockAngle,BlockLength,BlockLat,BlockLong,MaxLength : float64;
   z : float32;
   x,y : integer;
   BMP : tMyBitmap;
   p0 : pRGB;
   fName : PathStr;
   Results : tStringList;
begin
   CloneImageToBitmap(MapOwner.Image1,Bmp);
   StartProgressAbortOption('Geostationary satellite');
   Results := tStringList.Create;
   Results.Add('LAT,LONG,BLOCK_DIST,BLOCK_ANG');
   y := 0;
   while (y < MapOwner.MapDraw.MapYSize) do begin
      p0 := BMP.ScanLine[y];
      if (y mod 25 = 0) then UpdateProgressBar(y/ MapOwner.MapDraw.MapYSize);
      x := 0;
      while (x <  MapOwner.MapDraw.MapXSize) do begin
         DEMGlb[MapOwner.MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z);
         MaxLength := (DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.MaxElev - z) / TanDeg(MDDef.ElevToSat);
         DEMGlb[MapOwner.MapDraw.DEMonMap].HorizonBlocking(Lat,Long,MDDef.AzToSat,MaxLength,MDDef.ObsAboveGround,BlockAngle,BlockLength,BlockLat,BlockLong,MDDef.wf.StraightAlgorithm);
         if (MDDef.ShowMaskedToSat and (BlockAngle > MDDef.ElevToSat)) or ((not MDDef.ShowMaskedToSat) and (BlockAngle <> MDDef.ElevToSat)) then begin
            if (Results <> Nil) then begin
               Results.Add(RealToString(Lat,-12,-7) + ',' + RealToString(Long,-12,-7)  + ',' + RealToString(0.001 * BlockLength,-12,-3)  + ',' + RealToString(BlockAngle,-12,-3));
            end
            else begin
               if (MDDef.StatSampleIncr = 1) then p0[x] := rgbTripleRed
               else Petmar.ScreenSymbol(bmp.Canvas,x,y,FilledBox,2,claRed);
            end;
         end;
         inc(x,MDDef.StatSampleIncr);
         if WantOut then break;
      end;
      inc(y,MDDef.StatSampleIncr);
      if WantOut then break;
   end;
   if (Results <> Nil) then begin
      fName := Petmar.NextFileNumber(MDTempDir, 'Sat_block_', DefaultDBExt);
      MapOwner.StringListToLoadedDatabase(Results,fName);
   end
   else MapOwner.IHSmergeOntoMap(bmp);
   EndProgress;
end;

procedure THorizonOptions.BitBtn1Click(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('Horizon',BitBtn1,MDDef.HorizonColor,MDDef.HorizonWidth);
   NeedRedraw := true;
end;


procedure THorizonOptions.CheckBox1Click(Sender: TObject);
begin
   NeedRedraw := true;
end;

procedure THorizonOptions.CheckBox2Click(Sender: TObject);
begin
   NeedRedraw := true;
end;

procedure THorizonOptions.CheckBox3Click(Sender: TObject);
begin
   NeedRedraw := true;
end;

procedure THorizonOptions.CheckBox4Click(Sender: TObject);
begin
   NeedRedraw := true;
end;

procedure THorizonOptions.CheckOptions;
begin
  Petmar.CheckEditString(Edit1.Text,MDDef.HorizonLength);
  Petmar.CheckEditString(Edit2.Text,MDDef.HorizonRadialMultiple);
  Petmar.CheckEditString(Edit3.Text,MDDef.HorizonIncrement);
  Petmar.CheckEditString(Edit4.Text,MDDef.ObsAboveGround);
  Petmar.CheckEditString(Edit5.Text,MDDef.FirstSunlightDay);
  Petmar.CheckEditString(Edit6.Text,MDDef.LastSunLightDay);
  Petmar.CheckEditString(Edit7.Text,MDDef.SunlightPrecision);
  Petmar.CheckEditString(Edit8.Text,MDDef.SingleJulianDay);
  Petmar.CheckEditString(Edit9.Text,MDDef.AzToSat);
  Petmar.CheckEditString(Edit10.Text,MDDef.ElevToSat);
  Petmar.CheckEditString(Edit11.Text,MDDef.StatSampleIncr);
  MDDef.InvertSkyline := CheckBox1.Checked;
  MDDef.HorizonDistanceGraph := CheckBox2.Checked;
  MDDef.HorizonVertAngleGraph := CheckBox3.Checked;
  MDDef.HorizonSkyMap := CheckBox4.Checked;
  MDDef.VerifyTimeZone := CheckBox5.Checked;
  MDDef.SolarPathMap := CheckBox6.Checked;
  MDDef.ShowMaskedToSat := CheckBox7.Checked;
  MDDef.ShowSolstices := CheckBox8.Checked;
  MDDef.DaylightDuration := CheckBox9.Checked;
  MDDef.ShowToday := CheckBox10.Checked;
end;

procedure THorizonOptions.Edit1Change(Sender: TObject);
begin
   NeedRedraw := true;
end;

procedure THorizonOptions.Edit2Change(Sender: TObject);
begin
   NeedRedraw := true;
end;

procedure THorizonOptions.Edit3Change(Sender: TObject);
begin
   NeedRedraw := true;
end;

procedure THorizonOptions.Edit4Change(Sender: TObject);
begin
   NeedRedraw := true;
end;

procedure THorizonOptions.OKBtnClick(Sender: TObject);
begin
  CheckOptions;
  Close;
end;


end.
