unit basin_flooding;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordFloodBasin}
   //{$Define RecordFloodingProblems}
{$EndIf}

interface

uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}


  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,  Dialogs, StdCtrls, Buttons, Grids,
  Vcl.ExtCtrls, Vcl.ComCtrls,
  System.UItypes,
  Petmar_types, DEMMapf;

type
  TFloodingForm = class(TForm)
    StringGrid1: TStringGrid;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    CancelBtn: TBitBtn;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    FloodButton: TBitBtn;
    HelpBtn: TBitBtn;
    RadioGroup1: TRadioGroup;
    CheckBox2: TCheckBox;
    Edit5: TEdit;
    Edit6: TEdit;
    GroupBox1: TGroupBox;
    Edit3: TEdit;
    Label3: TLabel;
    Edit2: TEdit;
    Label2: TLabel;
    Edit4: TEdit;
    Label6: TLabel;
    RadioGroup2: TRadioGroup;
    BitBtn4: TBitBtn;
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FloodButtonClick(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure FloodTheBasin(StartX,StartY : integer; ReservoirTop : float64; var MaxDepth,AverageDepth,Area,Volume : float64; SaveFile : boolean);
    procedure Edit5Change(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
  private
    function CreateFloodBitmap(ReservoirTop: float64; xp, yp: integer): tMyBitmap;
    function CreateNewFloodBitmap(ReservoirTop: float64): tMyBitmap;
    function CreateNewFloodOverlay(ReservoirTop: float64): tMyBitmap;
    { Private declarations }
  public
    { Public declarations }
     MapForm : tMapForm;
     FloodingFileName : PathStr;
     xg,yg : integer;
     z1 : float64;
     FloodWaterColor : tColor;
  end;


procedure BasinFlooding(MapForm : tMapForm; xg,yg : integer);



implementation

{$R *.dfm}

uses
   Petmar,Petmath,PetImage,
   PetDBUtils,
   BaseMap,
   DEM_Manager,
   DEMDef_Routines,
   DEMCoord,DEMDefs;


procedure BasinFlooding(MapForm : tMapForm; xg,yg : integer);
var
  FloodingForm : TFloodingForm;
  sl : tStringList;
  Lat,Long : Petmar_types.float64;
begin
   {$IfDef RecordFloodBasin} WriteLineToDebugFile('BasinFlooding in  xg=' + intToStr(xg) + '   yg=' + IntToStr(yg)); {$EndIf}
   FloodingForm := TFloodingForm.Create(Application);
   FloodingForm.MapForm := MapForm;
   FloodingForm.RadioGroup1.Enabled := MapForm.MapDraw.DEMonMap <> 0;
   FloodingForm.xg := xg;
   FloodingForm.yg := yg;
   if (xg >= 0) and (yg >= 0) then begin
      FloodingForm.FloodingFileName := MDTempDir + 'flooding_starts' + DefaultDBExt;
      DEMGlb[MapForm.MapDraw.DEMonMap].DEMGridToLatLongDegree(xg,yg,Lat,Long);
      sl := tStringList.Create;
      sl.Add('LAT,LONG');
      sl.Add(RealToString(Lat,-12,-8) + ',' + RealToString(Long,-12,-8));
      StringList2CSVtoDB(sl,FloodingForm.FloodingFileName,true);
   end;
   FloodingForm.Edit1.Text := RealToString(MDDef.ReservoirTop,-8,-2);
   FloodingForm.Edit2.Text := RealToString(MDDef.ReservoirLowestLevel,-8,-2);
   FloodingForm.Edit3.Text := RealToString(MDDef.FloodStep,-8,-2);
   FloodingForm.Edit4.Text := RealToString(MDDef.ReservoirTop,-8,-2);
   FloodingForm.Show;
end;


procedure TFloodingForm.FloodTheBasin(StartX,StartY : integer; ReservoirTop : float64; var MaxDepth,AverageDepth,Area,Volume : float64; SaveFile : boolean);
var
   NPts,x,y,xp,yp,BasinDEM,its   : integer;
   XDEM,YDEM,z,zt : float32;
   Changed : boolean;
   Bitmap,Bitmap2 : tMyBitmap;
   fName : PathStr;
   P0 : PRGB;
   Table : tMyData;

         procedure CheckPoint(x,y : integer);
         var
            z1,z2 : float32;
            zi : integer;
         begin
            if DEMGlb[MapForm.MapDraw.DEMonMap].GetElevMeters(x,y,z2) and DEMGlb[BasinDEM].GetElevMeters(x,y,z1) then begin
               zi := round(z1);
               if (not (zi in [1,2])) and (z2 <= ReservoirTop) then begin
                  Changed := true;
                  DEMGlb[BasinDEM].SetGridElevation(x,y,1);
               end;
            end;
         end;

begin
   {$IfDef RecordFloodingProblems} WriteLinetoDebugFile('TMapForm.FloodTheBasin in'); {$EndIf}
   BasinDEM := 0;
   fName := MDTempDir + 'flooding.dem';
   if FileExists(fName) and LoadNewDEM(BasinDem,fName,false) then begin
      for x := 0 to pred(DEMGlb[BasinDEM].DEMheader.NumCol) do begin
         for y := 0 to pred(DEMGlb[BasinDEM].DEMheader.NumRow) do begin
            if not DEMGlb[BasinDEM].MissingDataInGrid(x,y) then
               DEMGlb[BasinDEM].SetGridElevation(X,Y,1);
         end;
      end;
   end
   else begin
      BasinDEM := DEMGlb[MapForm.MapDraw.DEMonMap].CloneAndOpenGridSetMissing(ByteDEM,DEMGlb[MapForm.MapDraw.DEMonMap].AreaName + ' flooding',euUndefined);
   end;
   {$IfDef RecordFloodingProblems} WriteLineToDebugFile('Initializaiton over ');  {$EndIf}

   ShowHourglassCursor;
   if FileExists(FloodingFileName) then begin
      {$IfDef RecordFloodingProblems} WriteLineToDebugFile('Flooding starts with ' + FloodingFileName); {$EndIf}
      Table := tMyData.Create(FloodingFileName);
      while not Table.Eof do begin
         DEMGlb[BasinDEM].LatLongDegreeToDEMGridInteger(Table.GetFieldByNameAsFloat('LAT'),Table.GetFieldByNameAsFloat('LONG'),Xp,Yp);
         DEMGlb[BasinDEM].SetGridElevation(xp,yp,1);
         Table.Next;
      end;
      Table.Destroy;
   end
   else DEMGlb[BasinDEM].SetGridElevation(StartX,StartY,1);

   its := 0;
   repeat
      inc(its);
      StatusBar1.Panels[0].Text := IntToStr(its) + ' iterations...' + RealToString(ReservoirTop,6,1);
      Changed := false;
      for x := 1 to (DEMGlb[MapForm.MapDraw.DEMonMap].DEMheader.NumCol - 2) do begin
         for y := 1 to (DEMGlb[MapForm.MapDraw.DEMonMap].DEMheader.NumRow - 2) do begin
            if DEMGlb[BasinDEM].GetElevMeters(x,y,zt) and (round(zt) = 1) then begin
               DEMGlb[BasinDEM].SetGridElevation(x,y,2);
               CheckPoint(pred(x),pred(y));
               CheckPoint(pred(x),y);
               CheckPoint(pred(x),succ(y));
               CheckPoint(succ(x),pred(y));
               CheckPoint(succ(x),y);
               CheckPoint(succ(x),succ(y));
               CheckPoint(x,pred(y));
               CheckPoint(x,succ(y));
            end {if y};
         end {for y};
      end {for x};
   until (not Changed);

   {$IfDef RecordFloodingProblems} WriteLinetoDebugFile('flooding done'); {$EndIf}

   NPts := 0;
   AverageDepth := 0;
   MaxDepth := 0;
   for x := 0 to pred(DEMGlb[BasinDEM].DEMheader.NumCol) do begin
      for y := 0 to pred(DEMGlb[BasinDEM].DEMheader.NumRow) do begin
         if DEMGlb[BasinDEM].GetElevMeters(x,y,zt) and (round(zt) = 2) then begin
            inc(NPts);
            DEMGlb[MapForm.MapDraw.DEMonMap].GetElevMeters(x,y,z);
            z := ReservoirTop - z;
            if (z > MaxDepth) then MaxDepth := z;
            DEMGlb[BasinDEM].SetGridElevation(x,y,z);
            AverageDepth := AverageDepth + z;
         end
         else DEMGlb[BasinDEM].SetGridMissing(x,y);
      end;
   end;
   if SaveFile then DEMGlb[BasinDEM].WriteNewFormatDEM(fName);
   {$IfDef RecordFloodingProblems} WriteLineToDebugFile('Basin DEM saved'); {$EndIf}

   AverageDepth := AverageDepth / NPts;
   Area := NPts * DEMGlb[MapForm.MapDraw.DEMonMap].AverageXSpace * DEMGlb[MapForm.MapDraw.DEMonMap].AverageYSpace;
   Volume := Area * AverageDepth;

   CopyImageToBitmap(MapForm.Image1,BitMap);
   CloneBitmap(Bitmap,Bitmap2);
   if (MapForm.mapDraw.MapType <> mtGrayReflect) then MakeTheBitmapGrayScale(Bitmap);

   if ShowSatProgress then StartProgressAbortOption('Merge');

   for y := 0 to pred(MapForm.MapDraw.MapYSize) do with MapForm.MapDraw do begin
      if (y mod 10 = 0) and ShowSatProgress then UpdateProgressBar(y/MapYSize);
      P0 := BitMap2.ScanLine[y];
      for x := 0 to pred(MapXSize) do begin
         ScreenToDEMGrid(X,Y,XDEM,YDEM);
         xp := round(Xdem);
         yp := round(Ydem);
         if not DEMGlb[BasinDEM].MissingDataInGrid(xp,yp) then P0[x] := MDdef.WaterColor;
      end;
   end;

   IHSMergePurgeBitmaps(Bitmap,Bitmap2);
   MapForm.Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
   if ShowSatProgress then EndProgress;
   ShowDefaultCursor;

   CloseSingleDEM(BasinDEM);
   StatusBar1.Panels[0].Text := '';
   {$IfDef RecordFloodingProblems} WriteLineToDebugFile('TMapForm.FloodTheBasin out');  {$EndIf}
end;


function TFloodingForm.CreateFloodBitmap(ReservoirTop : float64; xp,yp : integer) : tMyBitmap;
//create white bitmap the same size as the map
//set all points above the flood elevation to green
//from all the starting points (one per point in the water that is separated by a bridge or peninsula from the others), flood fill to a green boundary, with blue fill
//turn all the green to white, as those points are above the water
//in blue the bitmap now has all the flooded points, which are below the top of the flooded level, and which have a clear path to the water
var
   x,y : integer;
   z : float32;
   Table : tMyData;
   p0 : pRGB;
   c : tRGBTriple;
begin
   CloneImageToBitmap(MapForm.Image1,Result);
   if (not FileExists(FloodingFileName)) then BitBtn3Click(nil);
   if FileExists(FloodingFileName) then begin
      ShowHourglassCursor;
      c := ConvertTColorToPlatformColor(clGreen);
      for y := 0 to pred(MapForm.MapDraw.MapYSize) do begin
         p0 := Result.Scanline[y];
         for x := 0 to pred(MapForm.MapDraw.MapXSize) do begin
            if MapForm.MapDraw.ScreenToElev(x,y,z) and (z >= ReservoirTop) then begin
               p0[x] := c;
            end;
         end;
      end;

      Table := tMyData.Create(FloodingFileName);
      while not Table.Eof do begin
         MapForm.MapDraw.LatLongDegreeToScreen(Table.GetFieldByNameAsFloat('LAT'),Table.GetFieldByNameAsFloat('LONG'),Xp,Yp);
         Result.Canvas.Brush.Color := clBlue;
         Result.Canvas.FloodFill(xp,yp,clGreen,fsBorder);
         Table.Next;
      end;
      Table.Destroy;

      for y := 0 to pred(MapForm.MapDraw.MapYSize) do begin
         p0 := Result.Scanline[y];
         for x := 0 to pred(MapForm.MapDraw.MapXSize) do begin
            if SameColor(c,p0[x]) then begin
               p0[x] := RGBTripleWhite;
            end;
         end;
      end;
      ShowDefaultCursor;
   end;
end;


function TFloodingForm.CreateNewFloodBitmap(ReservoirTop : float64) : tMyBitmap;
var
   x,y : integer;
   z : float32;
   p0 : pRGB;
begin
   CloneImageToBitmap(MapForm.Image1,Result);
   ShowHourglassCursor;
   for y := 0 to pred(MapForm.MapDraw.MapYSize) do begin
      p0 := Result.Scanline[y];
      for x := 0 to pred(MapForm.MapDraw.MapXSize) do begin
         if MapForm.MapDraw.ScreenToElev(x,y,z) and (z >= ReservoirTop) then p0[x] := claWhite
         else p0[x] := ConvertTColorToPlatFormColor(FloodWaterColor);
      end;
   end;
   ShowDefaultCursor;
end;


function TFloodingForm.CreateNewFloodOverlay(ReservoirTop : float64) : tMyBitmap;
var
   x,y : integer;
   z : float32;
   p0 : pRGB;
begin
   CopyImageToBitmap(MapForm.Image1,Result);
   ShowHourglassCursor;
   for y := 0 to pred(MapForm.MapDraw.MapYSize) do begin
      p0 := Result.Scanline[y];
      for x := 0 to pred(MapForm.MapDraw.MapXSize) do begin
         if MapForm.MapDraw.ScreenToElev(x,y,z) and (z < ReservoirTop) then p0[x] := ConvertTColorToPlatFormColor(FloodWaterColor);
      end;
   end;
   MapForm.MapDraw.DrawMapOverlays(Result);
   ShowDefaultCursor;
end;



procedure TFloodingForm.Edit5Change(Sender: TObject);
var
   MLLWW_ft : float32;
begin
   CheckEditString(Edit5.Text,MLLWW_ft);
   Edit1.Text := RealToString(FeetToMeters*MLLWW_ft - MDDef.TideGaugeOffset,-12,-3);
end;


procedure TFloodingForm.Edit6Change(Sender: TObject);
begin
   CheckEditString(Edit6.Text,MDDef.TideGaugeOffset);
   Edit5Change(Sender);
   //Edit1.Text := RealToString(FeetToMeters*MLLWW_ft - MDDef.TideGaugeOffset,-12,-3);
end;

procedure TFloodingForm.BitBtn1Click(Sender: TObject);
var
   fName : PathStr;
   MovieList : tStringList;
   z1 : float64;
begin
   {$IfDef RecordFloodBasin} WriteLineToDebugFile('TFloodingForm.BitBtn1Click in ReservoirTop=' + Edit1.Text + '  ReservoirLowestLevel=' + Edit2.Text + '  ReservoirTop=FloodStep' + Edit3.Text); {$EndIf}
   StringGrid1.RowCount := 1;
   CheckEditString(Edit4.Text,MDDef.ReservoirTop);
   CheckEditString(Edit2.Text,MDDef.ReservoirLowestLevel);
   CheckEditString(Edit3.Text,MDDef.FloodStep);
   z1 := MDDef.ReservoirLowestLevel;
   if (MapForm.MapDraw.FloodLayers = Nil) then MapForm.MapDraw.FloodLayers := tStringList.Create
   else MapForm.MapDraw.FloodLayers.Clear;
   MovieList := tStringList.Create;
   while (z1 <= MDDef.ReservoirTop) do begin
      if (RadioGroup2.ItemIndex = 1) then Edit5.Text := RealToString(z1,-8,-2);
      Self.StatusBar1.Panels[0].Text := RealToString(z1,-8,-2) + ' m';
      if (MDDef.FloodAlg = 2) then MapForm.DoFastMapRedraw;
      fName := 'Level' + RealToString(z1,-8,-2);
      {$IfDef RecordFloodBasin} WriteLineToDebugFile('start ' + fName); {$EndIf}
      FloodButtonClick(Sender);
      {$IfDef RecordFloodBasin} WriteLineToDebugFile('over ' + fName); {$EndIf}
      MapForm.SaveBitmapForMovie(MovieDir + fName,MovieList);
      CopyFile(MDTempDir + 'flooding.dem',DEMDefs.WriteDEMDir + 'flood_' + RealToString(z1,-8,2) + '.dem');
      z1 := z1 + MDDef.FloodStep;
   end;
   fName := 'flooding.Mov';
   {$IfDef RecordFloodBasin} WriteLineToDebugFile('Run movie=' + fName); {$EndIf}
   MovieList.SaveToFile(DEMdefs.MovieDir + fName);
   MovieList.Free;
   {$IfDef ExMovies}
   {$Else}
      PetImage.MakeMovie(fName);
   {$EndIf}
   SysUtils.DeleteFile(MDTempDir + 'flooding.dem');
   Self.StatusBar1.Panels[0].Text := '';
   {$IfDef RecordFloodBasin} WriteLineToDebugFile('TFloodingForm.BitBtn1Click out'); {$EndIf}
end;


procedure TFloodingForm.BitBtn2Click(Sender: TObject);
var
   fName : PathStr;
   Results : tStringList;
begin
   FName := ProgramRootDir;
   if GetFileNameDefaultExt('HTML file with results','HTML file|*.htm',FName) then begin
      Results := tStringList.Create;
      Results.Add(StringGridToHTMLTable(StringGrid1));
      Results.SaveToFile(fName);
      Results.Free;
   end;
end;

procedure TFloodingForm.BitBtn3Click(Sender: TObject);
begin
   if (FloodingFileName = '') then FloodingFileName := DBDir;
   Petmar.GetFileFromDirectory('Starting points',DefaultDBMask,FloodingFileName);
end;

procedure TFloodingForm.BitBtn4Click(Sender: TObject);
begin
   Petmar.QueryTColor(BitBtn4,FloodWaterColor);
end;

procedure TFloodingForm.CancelBtnClick(Sender: TObject);
begin
   {$IfDef RecordFloodBasin} WriteLineToDebugFile('TFloodingForm.CancelBtnClick'); {$EndIf}
   Close;
end;


procedure TFloodingForm.FloodButtonClick(Sender: TObject);
var
  AverageDepth,MaxDepth,Area,Volume,z1  :float64;
  Row : integer;
  Bitmap : tMyBitmap;
  fName : PathStr;
  TStr : shortstring;

           procedure LabelBitmap;
           begin
               if CheckBox1.Checked then begin
                  MapForm.Image1.Canvas.Pen.Color := ConvertPlatformColorToTColor(MDDef.WaterColor);
                  MapForm.Image1.Canvas.Font.Name := 'Verdana';
                  MapForm.Image1.Canvas.Font.Size := 14;
                  MapForm.Image1.Canvas.TextOut(5,5,TStr);
               end;
           end;

begin
   {$IfDef RecordFloodBasin} WriteLineToDebugFile('TFloodingForm.FloodButtonClick in'); {$EndIf}
   CheckEditString(Edit1.Text,z1);
   if (Edit5.Text = '') then TStr := '' else TStr := '  ' + Edit5.Text + ' ft MLLW';
   TStr := 'Water: ' + RealToString(z1,-8,-2) + ' m NAVD' + TStr;

   SaveBackupDefaults;
   if {(not MapForm.MapDraw.DEMMap) or}(MDDef.FloodAlg = 1) then begin
      {$IfDef RecordFloodBasin} WriteLineToDebugFile('MDDef.FloodAlg = 1'); {$EndIf}
      MapForm.DoFastMapRedraw;
      Bitmap := CreateFloodBitmap(z1,xg,yg);
      fName := DEMdefs.MovieDir + TStr + OverlayFExt;
      PetImage.SaveBitmap(Bitmap,fName);
      if (MapForm.MapDraw.FloodLayers <> Nil) then  begin
         MapForm.MapDraw.FloodLayers.Add(fName);
         MapForm.MapDraw.WriteMapsWorldFile(fName);
      end;
      MapForm.IHSmergeOntoMap(Bitmap);
   end
   else if (MDDef.FloodAlg = 3) then begin
      Bitmap := CreateNewFloodBitmap(z1);
      MapForm.Image1.Picture.Graphic := Bitmap;
      Bitmap.Free;
      MDDef.UseGif := true;
   end
   else if (MDDef.FloodAlg = 4) then begin
      Bitmap := CreateNewFloodOverlay(z1);
      MapForm.Image1.Picture.Graphic := Bitmap;
      Bitmap.Free;
      //MDDef.UseGif := true;
   end
   else if (MDDef.FloodAlg = 0) then begin
      {$IfDef RecordFloodBasin} WriteLineToDebugFile('MDDef.FloodAlg = 0'); {$EndIf}
      MapForm.MapDraw.MapType := mtBlueGreenReflect;
      MDDef.CurrentSeaLevel := z1;
      MapForm.DoBaseMapRedraw;
   end
   else begin
      {$IfDef RecordFloodBasin} WriteLineToDebugFile('MDDef.FloodAlg = 2'); {$EndIf}
      FloodTheBasin(xg,yg,z1,MaxDepth,AverageDepth,Area,Volume,Sender = BitBtn1);
      Row := StringGrid1.RowCount;
      StringGrid1.RowCount := Row + 1;
      StringGrid1.FixedRows := 1;
      StringGrid1.DefaultColWidth := (StringGrid1.Width - 20) div 5;
      StringGrid1.Cells[0,Row] := RealToString(z1,-8,1);
      StringGrid1.Cells[1,Row] := SmartAreaFormat(Area);
      StringGrid1.Cells[2,Row] := SmartVolumeFormat(Volume,MDDef.EnglishDistanceUnits);
      StringGrid1.Cells[3,Row] := RealToString(AverageDepth,-8,1);
      StringGrid1.Cells[4,Row] := RealToString(MaxDepth,-8,1);
   end;
   LabelBitmap;
   if Checkbox2.Checked then begin
      {$IfDef RecordFloodBasin} WriteLineToDebugFile('KML export start'); {$EndIf}
      MapForm.Caption := TStr;
      MapForm.QuickbasemaptoGoogleEarth1Click(Sender);
      {$IfDef RecordFloodBasin} WriteLineToDebugFile('KML export end'); {$EndIf}
   end;
   RestoreBackupDefaults;
   {$IfDef RecordFloodBasin} WriteLineToDebugFile('TFloodingForm.FloodButtonClick out'); {$EndIf}
end;


procedure TFloodingForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TFloodingForm.FormCreate(Sender: TObject);
begin
   {$IfDef RecordFloodBasin} WriteLineToDebugFile('TFloodingForm.FormCreate in'); {$EndIf}
   StringGrid1.ColCount := 5;
   StringGrid1.RowCount := 1;
   RadioGroup1.ItemIndex := MDDef.FloodAlg;
   Edit6.Text := RealToString(MDDef.TideGaugeOffset,-12,-3);

   StringGrid1.Cells[0,0] := 'Pool Elev';
   StringGrid1.Cells[1,0] := 'Area';
   StringGrid1.Cells[2,0] := 'Volume';
   StringGrid1.Cells[3,0] := 'Avg Depth';
   StringGrid1.Cells[4,0] := 'Max Depth';
   FloodWaterColor := clBlue;
   ColorBitBtn(BitBtn4,clBlue);
   Show;
end;

procedure TFloodingForm.HelpBtnClick(Sender: TObject);
begin
   //if SimpleFloodModel then DisplayHTMLTopic('html\quick_sea_level_rise.htm') else
   DisplayHTMLTopic('html\basin_flooding.htm');
end;


procedure TFloodingForm.RadioGroup1Click(Sender: TObject);
begin
   MDDef.FloodAlg := RadioGroup1.ItemIndex;
   StringGrid1.Visible := RadioGroup1.ItemIndex = 2;                        
   BitBtn3.Visible := RadioGroup1.ItemIndex in [0,1];
   BitBtn2.Visible := RadioGroup1.ItemIndex = 2;
   {$IfDef RecordFloodBasin} WriteLineToDebugFile('TFloodingForm.RadioGroup1Click, FloodAlg=' + IntToStr(MDDef.FloodAlg)); {$EndIf}
end;

initialization
finalization
   {$IfDef RecordFloodBasin} WriteLineToDebugFile('RecordFloodBasin active in basin_flooding'); {$EndIf}
   {$IfDef RecordFloodingProblems} WriteLineToDebugFile('RecordFloodingProblems active in basin_flooding'); {$EndIf}
end.
