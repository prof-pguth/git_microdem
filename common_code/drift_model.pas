unit drift_model;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordDriftModel}
   //{$Define RecordMonteCarloDetails}
   //{$Define RecordMonteCarlo}
   //{$Define ShowTideInterpolation}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,Math,
  Dialogs, Grids, DBGrids, DB, StdCtrls, Buttons, ExtCtrls, ComCtrls,
  Petmar_types,Petmar_db,DEMMapf;

type
  TDrifting_form = class(TForm)
    Panel2: TPanel;
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    TabSheet3: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    RadioGroup2: TRadioGroup;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit6: TEdit;
    RadioGroup1: TRadioGroup;
    O: TTabSheet;
    Label3: TLabel;
    Label4: TLabel;
    Edit4: TEdit;
    Edit3: TEdit;
    CheckBox1: TCheckBox;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    HelpBtn: TBitBtn;
    TabSheet1: TTabSheet;
    TabSheet4: TTabSheet;
    ComboBox1: TComboBox;
    BitBtn9: TBitBtn;
    CheckBox8: TCheckBox;
    BitBtn10: TBitBtn;
    Edit8: TEdit;
    Label7: TLabel;
    TabSheet5: TTabSheet;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    BitBtn11: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn13: TBitBtn;
    CheckBox5: TCheckBox;
    RadioGroup3: TRadioGroup;
    TabSheet6: TTabSheet;
    Label9: TLabel;
    Label10: TLabel;
    Edit9: TEdit;
    Edit10: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    Edit11: TEdit;
    BitBtn14: TBitBtn;
    BitBtn15: TBitBtn;
    Label13: TLabel;
    Edit12: TEdit;
    BitBtn17: TBitBtn;
    GroupBox1: TGroupBox;
    CheckBox3: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox4: TCheckBox;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RadioGroup2Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
  private
    { Private declarations }
    procedure InitializeTides(Ask : boolean);
    procedure GetVectorMagMult(var VectorMagMult: float64);
  public
    { Public declarations }
     Drift_DB : integer;
     TideTable : tMyData;
     CoriolisDeflection,WindCurrentFactor : float64;
     MapActive,
     RandomUncertain : boolean;
     theBaseMap : tMapForm;
     ShipSpeedStdDev,ShipHeadingStdDev : float64;
  end;


procedure LoadDriftModel(BaseMap : tMapForm; dbNum : integer =0);

implementation

uses
   {$IfDef ExGIS}
   {$Else}
   demdatabase,
   {$EndIf}
   DEMDefs,BaseMap,DEMCoord, DEM_Manager,
   Petmar,PetDBUtils,Petmath,PetImage,Thread_timers,
   nevadia_main;

{$R *.dfm}


procedure LoadDriftModel(BaseMap : tMapForm; dbNum : integer = 0);
var
  Drifting_form : TDrifting_form;
begin
   Drifting_form := TDrifting_form.Create(Application);
   Drifting_form.show;
   Drifting_form.TheBaseMap := BaseMap;
   if (dbNum = 0) then begin
      dbNum := BaseMap.OpenDBonMap('Drift model','');
      //dbNum := LastDBLoaded;
   end;

   GISdb[dbNum].AddFieldToDataBase(ftFloat,'WC_SPEED',8,2);
   GISdb[dbNum].AddFieldToDataBase(ftFloat,'WC_DIR',8,2);
   Drifting_form.Drift_DB := dbNum;
   Drifting_form.Caption := 'Drift model: ' + GISdb[Drifting_form.Drift_db].dbName;
   Drifting_form.TabSheet5.TabVisible := GISdb[Drifting_form.Drift_db].MyData.FieldExists('ADJ_SPEED') and GISdb[Drifting_form.Drift_db].MyData.FieldExists('ADJ_DIR');
   Drifting_form.TideTable := Nil;
   Drifting_form.InitializeTides(false);
   GISdb[Drifting_form.Drift_DB].dbTablef.ShowStatus;
end;


procedure TDrifting_form.BitBtn11Click(Sender: TObject);
begin
   InitializeTides(true);
end;

procedure TDrifting_form.BitBtn12Click(Sender: TObject);
begin
   if (ComboBox1.ItemIndex > 1) then ComboBox1.ItemIndex := ComboBox1.ItemIndex -1
   else ComboBox1.ItemIndex := pred(ComboBox1.Items.Count);
   ComboBox1Change(Sender);
end;

procedure TDrifting_form.BitBtn13Click(Sender: TObject);
begin
   if (ComboBox1.ItemIndex < pred(ComboBox1.Items.Count)) then ComboBox1.ItemIndex := ComboBox1.ItemIndex + 1
   else ComboBox1.ItemIndex := 0;
   ComboBox1Change(Sender);
end;



procedure TDrifting_form.BitBtn14Click(Sender: TObject);
var
   NumRuns : integer;
   i,DensityGrid : Integer;
   Lat,Long,gridSize : float64;
   Results : tStringList;
   fName : PathStr;
begin
  {$IfDef RecordMonteCarlo} WriteLinetoDebugFile('TDrifting_form.BitBtn14Click (run Monte Carlo) in'); {$EndIf}
   RandomUncertain := true;
   MapActive := false;
   GISdb[Drift_DB].EmpSource.Enabled := false;

   CheckEditString(Edit9.Text,ShipSpeedStdDev);
   CheckEditString(Edit10.Text,ShipHeadingStdDev);
   CheckEditString(Edit11.Text,NumRuns);
   CheckEditString(Edit12.Text,GridSize);

   Results := tStringList.Create;
   Results.Add('LAT,LONG');
   StartSingleThreadTimer('Monte Carlo');
   for i := 1 to NumRuns do begin
       if (i mod 25 = 1) then begin
          ThreadTimers.UpdateThreadStats(9,round(100 * i/NumRuns),IntToStr(i) + ' runs ' + TimeToStr(Now));
       end;
       BitBtn1Click(Sender);
       GISdb[Drift_DB].MyData.Last;
       if GISdb[Drift_DB].ValidLatLongFromTable(Lat,Long) then Results.Add(RealToString(Lat,-18,8) + ',' + RealToString(Long,-18,8));
   end;
   EndThreadTimers;
   fName := Petmar.NextFileNumber(MDTempDir,GISdb[Drift_db].dbName + '_monte_carlo_points_','.csv');
   GISdb[Drift_DB].theMapOwner.DisplayAndPurgeStringListDB(Results,fName);
   {$IfDef RecordMonteCarlo} WriteLinetoDebugFile('Call create density grid'); {$EndIf}
   DensityGrid := GISdb[Drift_DB].dbTablef.CreateGrid(cgPointDensity,GridSize);

   fName := Petmar.NextFileNumber(MDTempDir,GISdb[Drift_db].dbName + '_monte_carlo_density_','.csv');
   {$IfDef RecordMonteCarlo} WriteLinetoDebugFile('Call density grid to DBF ' + fName); {$EndIf}
   DEMGlb[DensityGrid].SelectionMap.SaveDEMtoDBF(fName,'PT_DENSITY',1,false,false);
   CloseSingleDEM(DensityGrid);
   fName := ChangeFileExt(fName,'.dbf');
   {$IfDef RecordMonteCarlo} WriteLinetoDebugFile('Call open DBF ' + fName); {$EndIf}

   TheBaseMap.OpenDBonMap('',fName);
   GISdb[Drift_DB].PlotFieldOnMap('PT_DENSITY');
   RandomUncertain := false;
   MapActive := true;
   GISdb[Drift_DB].dbTablef.ShowStatus;
   {$IfDef RecordMonteCarlo} WriteLinetoDebugFile('Monte Carlo out'); {$EndIf}
end;


procedure TDrifting_form.BitBtn15Click(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('Resultant vector',BitBtn15,MDDef.ResultantColor,MDdef.ResultantWidth);
end;

procedure TDrifting_form.BitBtn17Click(Sender: TObject);
var
   Lat,Long,Speed,Heading,MinSpeed,MaxSpeed,SpeedIncrement,
   MinHeading,MaxHeading,HeadingIncrement : float64;
   Results : tStringList;
   fName : PathStr;
begin
  with GISdb[Drift_DB] do begin
     MinSpeed := 0.4;
     MaxSpeed := 1.6;
     SpeedIncrement := 0.2;
     MinHeading := -20;
     MaxHeading := 80;
     HeadingIncrement := 20;
     Results := tStringList.Create;
     Results.Add('LAT,LONG,NAME,ICON');
     Heading := MinHeading;
     while Heading <= MaxHeading + 0.5 do begin
        FillFieldWithValue('SHIP_DIR',RealToString(Heading,-8,0));
        Speed := MinSpeed;
        while Speed <= MaxSpeed + 0.05 do begin
           Speed := Speed + SpeedIncrement;
           FillFieldWithValue('SHIP_SPEED',RealToString(Speed,-8,-2));
           BitBtn1Click(Nil);
           if GISdb[Drift_DB].ValidLatLongFromTable(lat,Long) then
               Results.Add(RealToString(Lat,-12,-6) + ',' + RealToString(Long,-12,-6) + ',' +   RealToString(Speed,-6,1) + ' kts @ ' + RealToString(Heading,-6,0) + DegSym + ',' + 'ship_icon_2' + OverlayFExt);
        end;
        Heading := Heading + HeadingIncrement;
     end;
     fName := MDTempDir + 'model_sensitivity.csv';
     theBaseMap.DisplayAndPurgeStringListDB(Results,fName);
  end;
end;


procedure TDrifting_form.InitializeTides(Ask : boolean);
var
   TheItems : tStringList;
begin
   if (TideTable <> Nil) then TideTable.Destroy;
   TideTable := Nil;
   if Ask or (not FileExists(OceanTideFName)) then Petmar.GetFileFromDirectory('Tide currents',DefaultDBMask,OceanTideFName);

   if FileExists(OceanTideFName) then begin
      TideTable := tMyData.Create(OceanTideFName,dbmCDS);
      TheItems := tStringList.Create;
      TheItems.Sorted := true;
      TheItems.Duplicates := DupIgnore;
      while not TideTable.eof do begin
        TheItems.Add(TideTable.GetFieldByNameAsString('DATE') + '-' + TideTable.GetFieldByNameAsString('TIME'));
        TideTable.Next;
      end;
      ComboBox1.Items := TheItems;
      ComboBox1.Text := TheItems[0];
      FreeAndNil(TheItems);
   end;
   BitBtn9.Enabled := (TideTable <> Nil);
   BitBtn10.Enabled := (TideTable <> Nil);
   CheckBox5.Checked := (TideTable <> Nil);
   CheckBox5.Enabled := (TideTable <> Nil);
end;


procedure TDrifting_form.GetVectorMagMult(var VectorMagMult : float64);
//needed for tide plotting, and the motion vectors
//gets scaling factor
var
   Lat,Long,Lat2,Long2,Distance : float64;
   x,y,xp,yp : integer;
begin
   GISdb[Drift_DB].MyData.First;
   GISdb[Drift_DB].ValidLatLongFromTable(Lat,Long);
   GISdb[Drift_DB].TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);

   //get the length of a 10 knot vector, to pick the vector size
   Distance := 10 * 0.5148 * 3600;   //knots to meters per hour
   VincentyPointAtDistanceBearing(Lat,Long,Distance,90,Lat2,Long2);
   GISdb[Drift_DB].TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat2,Long2,xp,yp);
   VectorMagMult := 0.05 * sqrt(sqr(x-xp) + sqr(y-yp));
end;



procedure TDrifting_form.BitBtn1Click(Sender: TObject);
var
   MovieList : tStringList;
   VectorMagMult : float64;


      procedure DrawPositions(Color : tPlatformColor; LineWidth : integer; ConstantUncertain : boolean; SpeedUncertain,AzimuthUncertain : float64; UpdateMemo : boolean; LabelPts : boolean; PlotVectors : boolean);
      var
         Lat,Long,StartLat,StartLong,TideDir,TideSpeed,Duration,Distance,
         WindCurrentSpeed,WindCurrentDirection,Dist,Bearing,LastDist,
         TLat,TLong,TDir,TSpeed,xspeed,yspeed,XIncr,YIncr : float64;
         WindSpeed,WindDir,Delta,ShipDir,ShipSpeed : float32;
         MovieFrame,Day,Month,Year,x,y,i : integer;
         FirstPosition : boolean;
         TimeStr,DateStr : ShortString;
         fName : PathStr;
         BaseFrame,Bitmap : tMyBitmap;
         lats,longs,xs,ys : array[1..4] of float64;

                procedure AssignValues(Index : integer);
                begin
                   lats[Index] := TLat;
                   longs[Index] := TLong;
                   xs[Index] := xspeed;
                   ys[Index] := YSpeed;
                end;

               procedure LabelTime;
               begin
                  if MapActive and LabelPts and (not RandomUncertain) then begin
                     GISdb[Drift_DB].TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
                     TimeStr := GISdb[Drift_DB].MyData.GetFieldByNameAsString('TIME');
                     {$IfDef RecordDriftModel} WriteLineToDebugFile(TimeStr + '  Lable time: ' + LatLongDegreeToString(Lat,Long,DecDegrees)); {$EndIf}
                     GISdb[Drift_DB].theMapOwner.Image1.Canvas.TextOut(x+2,y+2,TimeStr);
                  end;
               end;


      begin
         {$IfDef RecordDriftModel} WriteLineToDebugFile('Speed uncertainty=' + RealToString(SpeedUncertain,-18,-2) + '  Azimuth uncertainty=' + RealToString(AzimuthUncertain,-18,-2)); {$EndIf}

         MovieFrame := 0;
         TideDir := 0;
         TideSpeed := 0;
         WindCurrentSpeed := 0;

         FirstPosition := true;
         GISdb[Drift_DB].MyData.First;
         GISdb[Drift_DB].ValidLatLongFromTable(Lat,Long);
         GISdb[Drift_DB].TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
         LabelTime;

         if MapActive then begin
            GetVectorMagMult(VectorMagMult);
            GISdb[Drift_DB].theMapOwner.Image1.Canvas.Pen.Color := ConvertPlatformColorToTColor(Color);
            GISdb[Drift_DB].theMapOwner.Image1.Canvas.Pen.Width := LineWidth;
            if ConstantUncertain then begin
               GISdb[Drift_DB].theMapOwner.Image1.Canvas.MoveTo(x,y);
            end;
         end;

         repeat
            GISdb[Drift_DB].GetFloat32FromTableLinkPossible('SHIP_DIR',ShipDir);
            GISdb[Drift_DB].GetFloat32FromTableLinkPossible('SHIP_SPEED',ShipSpeed);
            {$IfDef RecordDriftModel} WriteLineToDebugFile('++++++++++++++++++++++Time step: ' + GISdb[Drift_DB].MyData.GetFieldByNameAsString('TIME')); {$EndIf}
            if (not CheckBox3.Checked) or (GISdb[Drift_DB].MyData.GetFieldByNameAsString('WIND_SPEED') <> '') then begin
                  if RandomUncertain then begin
                     ShipSpeed := ShipSpeed + Math.RandG(0,ShipSpeedStdDev);
                     if (ShipSpeed < 0) then ShipSpeed := 0;
                     ShipDir := ShipDir + Math.RandG(0,ShipHeadingStdDev);
                     {$IfDef RecordMonteCarloDetails} WriteLineToDebugFile('speed=' + RealToString(ShipSpeed,-18,2) + '   heading=' + RealToString(ShipDir,-18,2)); {$EndIf}
                  end;

                  if CheckBox10.Checked then begin
                     if GISdb[Drift_DB].GetFloat32FromTableLinkPossible('ADJ_SPEED',Delta) then begin
                        ShipSpeed := ShipSpeed + Delta;
                        if (ShipSpeed < 0) then ShipSpeed := 0;
                     end;
                  end;
                  if CheckBox11.Checked then begin
                     if GISdb[Drift_DB].GetFloat32FromTableLinkPossible('ADJ_DIR',Delta) then ShipDir := ShipDir + Delta;
                  end;

                  StartLat := Lat;
                  StartLong := Long;
                  Duration := 1;

                  {$IfDef RecordDriftModel} WriteLineToDebugFile('Starting Location: ' + LatLongDegreeToString(Lat,Long,DecDegrees)); {$EndIf}
                  if CheckBox2.Checked then begin  //tide current
                     if CheckBox5.Checked then begin
                        if GISdb[Drift_DB].MyData.GetFieldByNameAsString('DAY_WEEK') = 'Thursday' then Day := 23
                        else if GISdb[Drift_DB].MyData.GetFieldByNameAsString('DAY_WEEK') = 'Friday' then Day := 24
                        else if GISdb[Drift_DB].MyData.GetFieldByNameAsString('DAY_WEEK') = 'Saturday' then Day := 25
                        else Day := 26;

                        Month := 9;
                        Year := 1779;

                        TimeStr := GISdb[Drift_DB].MyData.GetFieldByNameAsString('TIME');
                        if (TimeStr = '2400') then begin
                           TimeStr := '00:00';
                           inc(Day);
                        end
                        else begin
                           if Length(TimeStr) = 3 then begin
                              System.Insert(':',TimeStr,2);
                              TimeStr := '0' + TimeStr;
                           end
                           else System.Insert(':',TimeStr,3);
                        end;
                        DateStr :=  IntToStr(Day) + '/0' + IntToStr(Month) + '/' + IntToStr(Year);
                        TideTable.ApplyFilter( 'DATE=' + QuotedStr(DateStr) + ' AND TIME=' + QuotedStr(TimeStr) + ' AND LAT < ' + RealToString(Lat + 0.16667,-12,5) + ' AND LAT > ' + RealToString(Lat - 0.16667,-12,5) +
                            ' AND LONG < ' + RealToString(Long + 0.25,-12,5) +  ' AND LONG > ' + RealToString(Long - 0.25,-12,5));

                        {$IfDef ShowTideInterpolation} WriteLineToDebugFile('Filter=' + TideTable.Filter); {$EndIf}

                            for I := 1 to 4 do begin
                               lats[I] := -99;
                               longs[I] := -99;
                               xs[I] := -99;
                               ys[I] := -99;
                            end;

                        if (TideTable.RecordCount = 4) then begin
                           while not TideTable.eof do begin
                               //  4   3
                               //  1   2

                               TLat := TideTable.GetFieldByNameAsFloat('LAT');
                               TLong := TideTable.GetFieldByNameAsFloat('LONG');
                               TDir := TideTable.GetFieldByNameAsFloat('DIRECTION');
                               TSpeed := TideTable.GetFieldByNameAsFloat('SPEED');
                               xspeed := TSpeed * SinDeg(TDir);
                               yspeed := TSpeed * CosDeg(TDir);

                               {$IfDef ShowTideInterpolation} WriteLineToDebugFile(RealToString(tLat,12,4) + RealToString(tLong,12,4) + RealToString(TDir,8,1) + RealToString(TSpeed,5,2) + RealToString(xspeed,6,2) + RealToString(yspeed,6,2));                              {$EndIf}

                               if (TLat < Lat) and (TLong < Long) then AssignValues(1)
                               else if (TLat < Lat) and (TLong > Long) then AssignValues(2)
                               else if (TLat > Lat) and (TLong > Long) then AssignValues(3)
                               else if (TLat > Lat) and (TLong < Long) then AssignValues(4)
                               else MessageToContinue('Tide Problem');
                               TideTable.Next;
                           end;
                           if Lats[4] < -98 then begin
                              lats[4] := lats[3];
                              longs[4] := long;
                              xs[4] := xs[3];
                              ys[4] := ys[3];
                           end;

                           if Lats[1] < -98 then begin
                              lats[1] := lats[2];
                              longs[1] := long;
                              xs[1] := xs[2];
                              ys[1] := ys[2];
                           end;

                            XIncr := (Long - Longs[1]) / 0.25;
                            YIncr := (Lat - Lats[1]) / 0.16667;
                            xspeed := (  (1-XIncr) * (1-YIncr) * xs[1]
                                 +    (1-XIncr) *    YIncr  * xs[4]
                                 +       XIncr  * (1-YIncr) * xs[2]
                                 +       XIncr  *    YIncr  * xs[3]);
                            yspeed := (  (1-XIncr) * (1-YIncr) * ys[1]
                                 +    (1-XIncr) *    YIncr  * ys[4]
                                 +       XIncr  * (1-YIncr) * ys[2]
                                 +       XIncr  *    YIncr  * ys[3]);

                            TideSpeed := sqrt(sqr(xspeed) + sqr(yspeed)) * 1.94384449 {convert m/s to knots};
                            TideDir := HeadingOfLine(xspeed,yspeed);
                        end
                        else begin  //out of box, so just get closest tide station
                           TideTable.ApplyFilter( 'DATE=' + QuotedStr(DateStr) + ' AND TIME=' + QuotedStr(TimeStr));
                           LastDist := 38e38;
                           while not TideTable.eof do begin
                              TLat := TideTable.GetFieldByNameAsFloat('LAT');
                              TLong := TideTable.GetFieldByNameAsFloat('LONG');
                              VincentyCalculateDistanceBearing(Lat,Long,tLat,tLong,Dist,Bearing);
                              if (Dist < LastDist) then begin
                                 LastDist := Dist;
                                 TideDir := TideTable.GetFieldByNameAsFloat('DIRECTION');
                                 TideSpeed := TideTable.GetFieldByNameAsFloat('SPEED')* 1.94384449;
                              end;
                              TideTable.Next;
                           end;
                        end;
                        {$IfDef ShowTideInterpolation} WriteLineToDebugFile(RealToString(Lat,12,4) + RealToString(Long,12,4) +  RealToString(TideDir,8,1) + RealToString(TideSpeed,5,2)); {$EndIf}
                     end
                     else begin
                        {$IfDef RecordDriftModel} WriteLineToDebugFile('Tides from DB'); {$EndIf}
                        TideSpeed := GISdb[Drift_DB].MyData.GetFieldByNameAsFloat('TIDE_SP2');
                        TideDir := GISdb[Drift_DB].MyData.GetFieldByNameAsFloat('TIDE_DIR2');
                     end;
                     Distance := TideSpeed * 0.5148 * 3600;  //knots to meters per hour
                     VincentyPointAtDistanceBearing(Lat,Long,Distance,TideDir,Lat,Long);
                     {$IfDef RecordDriftModel} WriteLineToDebugFile('Location after tides: ' + LatLongDegreeToString(Lat,Long,DecDegrees)); {$EndIf}
                  end
                  else begin
                     {$IfDef RecordDriftModel} WriteLineToDebugFile('No Tides'); {$EndIf}
                  end;

                  if CheckBox4.Checked then begin //speed uncertainty
                     if (RadioGroup1.ItemIndex = 0) then Distance := (ShipSpeed + SpeedUncertain) * 0.5148 * 3600
                     else Distance := (1 + 0.01 * SpeedUncertain) * ShipSpeed * 0.5148 * 3600;  //knots to meters per hour
                     if (Distance < 0) then Distance := 0;
                     VincentyPointAtDistanceBearing(Lat,Long,Distance,ShipDir+AzimuthUncertain,Lat,Long);
                     {$IfDef RecordDriftModel} WriteLineToDebugFile('Location after speed uncertainty: ' + LatLongDegreeToString(Lat,Long,DecDegrees)); {$EndIf}
                  end;

                  if CheckBox3.Checked and GISdb[Drift_DB].GetFloat32FromTableLinkPossible('WIND_SPEED',WindSpeed) and GISdb[Drift_DB].GetFloat32FromTableLinkPossible('WIND_TO',WindDir) then begin
                     WindCurrentSpeed := WindSpeed * WindCurrentFactor;
                     WindCurrentDirection := WindDir + CoriolisDeflection;
                     Distance := WindCurrentSpeed * 0.5148 * 3600;   //knots to meters per hour
                     VincentyPointAtDistanceBearing(Lat,Long,Distance,WindCurrentDirection,Lat,Long);
                     {$IfDef RecordDriftModel} WriteLineToDebugFile('Location after wind current: ' + LatLongDegreeToString(Lat,Long,DecDegrees)); {$EndIf}
                  end;

                  if MapActive then begin
                    if ConstantUncertain then begin
                       GISdb[Drift_DB].TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
                       GISdb[Drift_DB].theMapOwner.Image1.Canvas.LineTo(x,y);
                    end
                    else begin
                       CopyImageToBitmap(GISdb[Drift_DB].theMapOwner.Image1,Bitmap);
                       if (not RandomUncertain) and PlotVectors then begin
                          if CheckBox7.Checked then begin
                             PlotOrientedLine(Bitmap,x,y,VectorMagMult*WindSpeed,WindDir,MDDef.WindColor,MDDef.WindWidth,true,false);
                          end;

                          if (RadioGroup3.ItemIndex = 0) then begin
                             if CheckBox2.Checked then begin
                                PlotOrientedLine(Bitmap,x,y,VectorMagMult*TideSpeed,TideDir,MDDef.TideColor,MDDef.TideWidth,true,false);
                             end;
                             if CheckBox3.Checked then begin
                                PlotOrientedLine(Bitmap,x,y,VectorMagMult*WindCurrentSpeed,WindCurrentDirection,MDDef.WindCurrentColor,MDDef.WindCurrentWidth ,true,false);
                             end;
                             if CheckBox4.Checked then begin
                                PlotOrientedLine(Bitmap,x,y,VectorMagMult*ShipSpeed,ShipDir,MDDef.ShipColor,MDDef.ShipWidth,true,false);
                             end;
                          end
                          else begin
                             ArrowEndX := x;
                             ArrowEndY := y;
                             if CheckBox2.Checked then PlotOrientedLine(Bitmap,ArrowEndX,ArrowEndY,VectorMagMult*TideSpeed,TideDir,MDDef.TideColor,MDDef.TideWidth,true,false,true);
                             if CheckBox4.Checked then PlotOrientedLine(Bitmap,ArrowEndX,ArrowEndY,VectorMagMult*ShipSpeed,ShipDir,MDDef.ShipColor,MDDef.ShipWidth,true,false,true);
                             if CheckBox3.Checked then PlotOrientedLine(Bitmap,ArrowEndX,ArrowEndY,VectorMagMult*WindCurrentSpeed,WindCurrentDirection,MDDef.WindCurrentColor,MDDef.WindCurrentWidth ,true,false,true);
                          end;
                       end;
                       GISdb[Drift_DB].theMapOwner.Image1.Picture.Graphic := Bitmap;
                       Bitmap.Free;
                       GISdb[Drift_DB].TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
                    end;
                  end;

                 if UpdateMemo then begin
                     GISdb[Drift_DB].MyData.Edit;
                     if FirstPosition then begin
                        FirstPosition := false;
                     end;

                     if CheckBox3.Checked then begin
                        GISdb[Drift_DB].MyData.SetFieldByNameAsFloat('WC_DIR',WindCurrentDirection);
                        GISdb[Drift_DB].MyData.SetFieldByNameAsFloat('WC_SPEED',WindCurrentSpeed);
                     end;

                     if CheckBox5.Checked then begin
                        GISdb[Drift_DB].MyData.SetFieldByNameAsFloat('TIDE_DIR',TideDir);
                        GISdb[Drift_DB].MyData.SetFieldByNameAsFloat('TIDE_SPEED',TideSpeed);
                     end;

                     VincentyCalculateDistanceBearing(StartLat,StartLong,Lat,Long,Dist,Bearing);
                     GISdb[Drift_db].MyData.SetFieldByNameAsFloat('CMG_SPEED',Dist* 0.000539956803); //meters/hr to knots
                     GISdb[Drift_db].MyData.SetFieldByNameAsFloat('CMG_DIR',Bearing);
                 end;

               if (Sender = BitBtn2) then begin
                  Petmar.ScreenSymbol( GISdb[Drift_DB].theMapOwner.Image1.Canvas,x,y,FilledBox,3,claRed);
                  inc(MovieFrame);
                  FName := 'drift' + IntToStr(MovieFrame) + MovieFileExt;
                  CopyImageToBitmap(GISdb[Drift_DB].theMapOwner.Image1,BaseFrame);
                  GISdb[Drift_DB].theMapOwner.Image1.Canvas.Font.Color := clRed;
                  GISdb[Drift_DB].theMapOwner.Image1.Canvas.Font.Size := 16;
                  GISdb[Drift_DB].theMapOwner.Image1.Canvas.TextOut(0,0,GISdb[Drift_DB].MyData.GetFieldByNameAsString('DAY_WEEK') + '  ' + GISdb[Drift_DB].MyData.GetFieldByNameAsString('TIME'));
                  GISdb[Drift_DB].theMapOwner.Image1.Picture.SaveToFile(DEMdefs.MovieDir + fName);
                  MovieList.Add(fName);
                  GISdb[Drift_DB].theMapOwner.Image1.Picture.Graphic := BaseFrame;
                  BaseFrame.Free;
               end;
            end;
            GISdb[Drift_DB].MyData.Next;
            GISdb[Drift_DB].MyData.Edit;
            GISdb[Drift_DB].MyData.SetFieldByNameAsFloat('LAT',Lat);
            GISdb[Drift_DB].MyData.SetFieldByNameAsFloat('LONG',Long);
            LabelTime;
            GISdb[Drift_DB].MyData.Next;
            if not GISdb[Drift_DB].MyData.eof then GISdb[Drift_DB].MyData.Prior;
            {$IfDef RecordDriftModel} WriteLineToDebugFile('Location at end of timestep: ' + LatLongDegreeToString(Lat,Long,DecDegrees)); {$EndIf}
         until GISdb[Drift_DB].MyData.eof;
         //GISdb[Drift_DB].MyData.Post;
      end;

var
   AzUncert,SpUncert : float64;
   fName : PathStr;
begin
   if MapActive and (not RandomUncertain) then begin
      GISdb[Drift_DB].EmpSource.Enabled := false;
      GISdb[Drift_DB].theMapOwner.DoFastMapRedraw;
   end;
   if (Sender = BitBtn2) then begin   //make movie
      MovieList := tStringList.Create;
      DrawPositions(claRed,3,false,0,0,true,false,true);
      fName := 'drift_motion.mov';
      MovieList.SaveToFile(DEMdefs.MovieDir + fName);
      PetImage.MakeMovie(fName);
      MovieList.Free;
   end
   else if (Sender = BitBtn14) then begin   //Monte Carlo simulation
      DrawPositions(MDDef.ResultantColor,MDDef.ResultantWidth,true,0,0,false,false,false);
   end
   else begin
      AzUncert := 0;
      SpUncert := 0;
      if (RadioGroup2.ItemIndex in [1,3]) then CheckEditString(Edit1.Text,AzUncert);
      if (RadioGroup2.ItemIndex in [2,3]) then begin
         if (RadioGroup1.ItemIndex = 0) then CheckEditString(Edit2.Text,SpUncert)
         else CheckEditString(Edit6.Text,SpUncert);
      end;

      if (RadioGroup2.ItemIndex <> 0) and (not RandomUncertain) then begin
         {$IfDef RecordDriftModel} WriteLineToDebugFile('Draw uncertainty'); {$EndIf}
         DrawPositions(claDarkGrey,1,true, SpUncert,AzUncert,false,false,false);
         DrawPositions(claDarkGrey,1,true, SpUncert,-AzUncert,false,false,false);
         DrawPositions(claDarkGrey,1,true, -SpUncert,AzUncert,false,false,false);
         DrawPositions(claDarkGrey,1,true, -SpUncert,-AzUncert,false,false,false);
      end;
      if CheckBox1.Checked {LabelTimes} then begin
         {$IfDef RecordDriftModel} WriteLineToDebugFile('=====================================Label times'); {$EndIf}
         DrawPositions(MDDef.ResultantColor,MDDef.ResultantWidth,false,0,0,false,true,false);
      end;
      if CheckBox6.Checked {MotionVectors} then begin
         {$IfDef RecordDriftModel} WriteLineToDebugFile('=====================================Motion Vectors'); {$EndIf}
         DrawPositions(MDDef.ResultantColor,MDDef.ResultantWidth,false,0,0,false,false,true);
      end;
      {$IfDef RecordDriftModel} WriteLineToDebugFile('=====================================Resultant Vectors'); {$EndIf}
      DrawPositions(MDDef.ResultantColor,MDDef.ResultantWidth,false,0,0,true,false,true);     //resultant vectors
   end;
   if MapActive and (not RandomUncertain) then begin
      GISdb[Drift_DB].EmpSource.Enabled := true;
      GISdb[Drift_DB].dbTablef.ShowStatus;
   end;
end;


procedure TDrifting_form.BitBtn10Click(Sender: TObject);
{$IfDef ExMovies}
begin
{$Else}
var
   i,td : integer;
   MovieList : tStringList;
   fName : PathStr;
begin
   CheckEditString(Edit8.Text,td);
   MovieList := tStringList.Create;
   fName := DEMdefs.MovieDir + 'tide_animation.mov';

   for i := 0 to pred(ComboBox1.Items.Count) do begin
      ComboBox1.Text := ComboBox1.Items.Strings[i];
      BitBtn9Click(Sender);
      fName := 'time_' + intToStr(i) + MovieFileExt;
      PetImage.SaveImageAsBMP(GISdb[Drift_DB].theMapOwner.Image1,DEMdefs.MovieDir +fname);
      MovieList.Add(fName);
      Petmar.Delay(td);
   end;
   fName := 'tide_animation.mov';
   MovieList.SaveToFile(DEMdefs.MovieDir + fName);
   PetImage.MakeMovie(fName);
   MovieList.Free;
{$EndIf}
end;


procedure TDrifting_form.BitBtn2Click(Sender: TObject);
begin
   BitBtn1Click(Sender);
end;

procedure TDrifting_form.BitBtn3Click(Sender: TObject);
begin
  Petmar.PickLineSizeAndColor('Ship headway',BitBtn3,MDDef.ShipColor,MDDef.ShipWidth);
end;

procedure TDrifting_form.BitBtn4Click(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('Tide current',BitBtn4,MDDef.TideColor,MDDef.TideWidth);
end;

procedure TDrifting_form.BitBtn5Click(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('Wind current',BitBtn5,MDDef.WindCurrentColor,MDDef.WindCurrentWidth);
end;


procedure TDrifting_form.BitBtn6Click(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('Wind current',BitBtn6,MDDef.WindColor,MDDef.WindWidth);
end;

procedure TDrifting_form.BitBtn7Click(Sender: TObject);
var
   Speed : float32;
begin
//  with GISdb[Drift_DB] do begin
      Speed := 1;
      ReadDefault('Speed',Speed);
      GISdb[Drift_DB].FillFieldWithValue('SHIP_SPEED',RealToString(Speed,-8,-2));
      BitBtn1Click(Sender);
//  end;
end;

procedure TDrifting_form.BitBtn8Click(Sender: TObject);
var
   Heading : float32;
begin
  //with GISdb[Drift_DB] do begin
      Heading := 1;
      ReadDefault('Heading',Heading);
      GISdb[Drift_DB].FillFieldWithValue('SHIP_DIR',RealToString(Heading,-8,-2));
      BitBtn1Click(Sender);
  //end;
end;

procedure TDrifting_form.BitBtn9Click(Sender: TObject);
var
   Lat,Long,Speed,Dir,VectorMagMult : float64;
   x,y : integer;
   TStr : AnsiString;
   Bitmap : tMyBitmap;
begin
   with GISdb[Drift_DB],MyData do begin
      TheMapOwner.DoFastMapRedraw;
      GetVectorMagMult(VectorMagMult);
      {$IfDef RecordDriftModel}
        TideTable.ApplyFilter('');
        WriteLineToDebugFile('UnFiltered,   Tide records: ' + IntToStr(TideTable.RecordCount));
      {$EndIf}
      TStr := ComboBox1.Text;
      TideTable.ApplyFilter( 'DATE=' + QuotedStr(Petmar_Types.BeforeSpecifiedCharacterANSI(TStr,'-',true,true)) + ' AND TIME=' + QuotedStr(TStr));
      {$IfDef RecordDriftModel} WriteLineToDebugFile('Filter =' + TideTable.Filter + '    Tide records: ' + IntToStr(TideTable.RecordCount)); {$EndIf}
      TideTable.First;
      CopyImageToBitmap(TheMapOwner.Image1,Bitmap);
      while not TideTable.eof do begin
          Lat := TideTable.GetFieldByNameAsFloat('LAT');
          Long := TideTable.GetFieldByNameAsFloat('LONG');
          TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
          Speed := TideTable.GetFieldByNameAsFloat('SPEED') * 1.94384449;
          Dir := TideTable.GetFieldByNameAsFloat('DIRECTION');
          if TheMapOwner.MapDraw.OnScreen(x,y) then begin
             PlotOrientedLine(Bitmap,x,y,round(VectorMagMult*Speed),round(Dir),MDDef.TideColor,dbOpts.LineWidth,true,false);
             if CheckBox8.Checked then begin
                if Dir < 90 then y := Y + 5 else y := y - 5;
                Bitmap.Canvas.TextOut(x+5,y,RealToString(Speed,-6,2) + ' @' + RealToString(Dir,-6,0) + DegSym);
             end;
          end;
         TideTable.Next;
      end;
      PlotOrientedLine(Bitmap,5,5,round(VectorMagMult*1.0),90,MDDef.TideColor,dbOpts.LineWidth,true,false);
      Bitmap.Canvas.Font.Size := 14;
      Bitmap.Canvas.Font.Style := [fsBold];
      Bitmap.Canvas.TextOut(round(VectorMagMult*1.0)+ 15,5,'1 knot');
      Bitmap.Canvas.TextOut(10,25,ComboBox1.Text);
      TheMapOwner.Image1.Picture.Graphic := Bitmap;
      Bitmap.Free;
   end;
end;


procedure TDrifting_form.CheckBox1Click(Sender: TObject);
begin
   BitBtn1Click(Sender);
end;

procedure TDrifting_form.CheckBox2Click(Sender: TObject);
begin
   BitBtn1Click(Sender);
end;


procedure TDrifting_form.CheckBox3Click(Sender: TObject);
begin
   BitBtn1Click(Sender);
end;

procedure TDrifting_form.CheckBox4Click(Sender: TObject);
begin
   BitBtn1Click(Sender);
end;

procedure TDrifting_form.CheckBox6Click(Sender: TObject);
begin
   BitBtn1Click(Sender);
end;

procedure TDrifting_form.CheckBox7Click(Sender: TObject);
begin
   BitBtn1Click(Sender);
end;


procedure TDrifting_form.CheckBox9Click(Sender: TObject);
begin
   BitBtn1Click(Sender);
end;

procedure TDrifting_form.ComboBox1Change(Sender: TObject);
begin
   BitBtn9Click(Sender);
end;

procedure TDrifting_form.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,WindCurrentFactor);
   if (WindCurrentFactor < 0) then  begin
      MessageToContinue('Must be positive');
      WindCurrentFactor := abs(WindCurrentFactor);
   end;
   WindCurrentFactor := 0.01 * WindCurrentFactor;
end;

procedure TDrifting_form.Edit4Change(Sender: TObject);
begin
    CheckEditString(Edit4.Text,CoriolisDeflection);
end;

procedure TDrifting_form.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
   if (TideTable <> Nil) then TideTable.Destroy;
end;


procedure TDrifting_form.FormCreate(Sender: TObject);
begin
   RandomUncertain := false;
   MapActive := true;
   CoriolisDeflection := 15;
   WindCurrentFactor := 0.03;
   RadioGroup2Click(Sender);
   Petmar.ColorLineWidthBitBtn(BitBtn3,MDDef.ShipColor,MDDef.ShipWidth);
   Petmar.ColorLineWidthBitBtn(BitBtn4,MDDef.TideColor,MDDef.TideWidth);
   Petmar.ColorLineWidthBitBtn(BitBtn5,MDDef.WindCurrentColor,MDDef.WindCurrentWidth);
   Petmar.ColorLineWidthBitBtn(BitBtn6,MDDef.WindColor,MDDef.WindWidth);
   Petmar.ColorLineWidthBitBtn(BitBtn15,MDDef.ResultantColor,MDDef.ResultantWidth);
   Drift_DB := 0;
   wmdem.FormPlacementInCorner(Self);
end;


procedure TDrifting_form.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\drift\drift_model.htm');
end;


procedure TDrifting_form.RadioGroup1Click(Sender: TObject);
begin
  RadioGroup2Click(Sender);
end;

procedure TDrifting_form.RadioGroup2Click(Sender: TObject);
begin
   Label1.Enabled := RadioGroup2.ItemIndex in [1,3];
   Edit1.Enabled := Label1.Enabled;
   Label2.Enabled := (RadioGroup2.ItemIndex in [2,3]) and (RadioGroup1.ItemIndex = 0);
   Edit2.Enabled := Label2.Enabled;
   Label5.Enabled := (RadioGroup2.ItemIndex in [2,3]) and (RadioGroup1.ItemIndex = 1);
   Edit6.Enabled := Label5.Enabled;
end;


procedure TDrifting_form.RadioGroup3Click(Sender: TObject);
begin
   BitBtn1Click(Sender);
end;

initialization
finalization
  {$IfDef RecordDriftModel} WriteLineToDebugFile('RecordDriftModel active in drift_model'); {$EndIf}
  {$IfDef ShowTideInterpolation} WriteLineToDebugFile('ShowTideInterpolation active in drift_model'); {$EndIf}
  {$IfDef RecordMonteCarlo} WriteLineToDebugFile('RecordMonteCarlo active in drift_model'); {$EndIf}
end.


