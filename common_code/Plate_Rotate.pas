unit plate_rotate;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordPlateRotations}
{$EndIf}


interface

uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,

   {$IfDef UseFireDacSQLlite}
      FireDAC.Stan.ExprFuncs,
      FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
      FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
      FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
      FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Comp.Client, FireDAC.Comp.DataSet,
      FireDAC.Phys.SQLite, FireDAC.Comp.UI,
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

   {$IfDef ExGIS}
   {$Else}
      demdatabase,
   {$EndIf}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  System.UItypes,
  PETMath,BaseGraf,
  PETMAR,Petmar_types,DEMMapf,ExtCtrls, ComCtrls;

type
  TPickRotationForm = class(TForm)
    Memo1: TMemo;
    GroupBox3: TGroupBox;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Plate1ComboBox: TComboBox;
    Plate2ComboBox: TComboBox;
    ModelComboBox: TComboBox;
    Panel1: TPanel;
    BitBtn12: TBitBtn;
    Plate3ComboBox: TComboBox;
    Label16: TLabel;
    Panel2: TPanel;
    RadioGroup1: TRadioGroup;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Edit5: TEdit;
    Edit4: TEdit;
    Edit3: TEdit;
    Edit2: TEdit;
    Edit1: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label11: TLabel;
    TrackBar1: TTrackBar;
    Label12: TLabel;
    CheckBox4: TCheckBox;
    TrackBar2: TTrackBar;
    CheckBox5: TCheckBox;
    GroupBox1: TGroupBox;
    BitBtn4: TBitBtn;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox1: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    Panel4: TPanel;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    ComboBox8: TComboBox;
    ComboBox9: TComboBox;
    ComboBox10: TComboBox;
    ComboBox11: TComboBox;
    BitBtn14: TBitBtn;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    BitBtn16: TBitBtn;
    BitBtn17: TBitBtn;
    BitBtn18: TBitBtn;
    GroupBox2: TGroupBox;
    TotalPolesComboBox: TComboBox;
    Label6: TLabel;
    TimeComboBox: TComboBox;
    Label9: TLabel;
    ContinentComboBox: TComboBox;
    Label10: TLabel;
    BitBtn9: TBitBtn;
    HelpBtn: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn13: TBitBtn;
    procedure TrackBar2Change(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure Plate1ComboBoxChange(Sender: TObject);
    procedure ModelComboBoxChange(Sender: TObject);
    procedure TotalPolesComboBoxChange(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure CheckBox10Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure ComboBox7Change(Sender: TObject);
    procedure ComboBox8Change(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
  private
    procedure DrawTJmotion(vx1,vy1,vx2,vy2 : float32; Boundary : shortString; LowerPlate : integer; AzText : shortstring; Color : tColor);
    { Private declarations }
  public
    { Public declarations }
     xv1,xv2,yv1,yv2,xv3,yv3 : float32;
     P1P2Color,P1P3Color,P2P3Color : tcolor;
     MapForm : DEMMapf.tMapForm;
     PlatesDB,
     MotionTable,
     PoleTable,
     BoundariesDB,
     RotLineSize,DesiredRecord : integer;
     RotLineColor : tPlatformColor;
     Redrawing : boolean;
     BaseBitmap : tMyBitmap;
     ThisGraph : tThisBaseGraph;
     procedure GetRotationMatrix;
     procedure ComputeRotationMatrix;
  end;

var
   PickRotationForm : Plate_Rotate.TPickRotationForm;
   RotationData : VectorType;
   PlateRotationMatrix : MatrixType;
   RotationMa,RotationRate : float64;


implementation

{$R *.dfm}


uses

   Nevadia_Main,
   Map_Overlays,DEMesrishapefile,
   DEMMapDraw,
   DEMDefs,PETdbUtils, PETImage;


procedure TPickRotationForm.BitBtn1Click(Sender: TObject);
var
   Bitmap : tMyBitmap;
   RecNo : integer;
begin
   Redrawing := true;
   MapForm.DoFastMapRedraw;
   if not CopyImageToBitmap(MapForm.Image1,Bitmap) then exit;
   Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(RotLineColor);
   Bitmap.Canvas.Pen.Width := RotLineSize;
   ShowHourglassCursor;
   GISdb[PlatesDB].MyData.ApplyFilter('NAME=' + QuotedStr(UpperCase(ContinentComboBox.Text)));
   if (GISdb[PlatesDB].MyData.RecordCount > 0) then begin
      GetRotationMatrix;
      RecNo := GISdb[PlatesDB].MyData.RecNo;
      GISdb[PlatesDB].aShapeFile.RotateAndPlotSingleRecord(MapForm.MapDraw,Bitmap,RecNo,PlateRotationMatrix);
   end;

   MapForm.Image1.Picture.Graphic := Bitmap;
   MapForm.SetFocus;
   Bitmap.Free;
   ShowDefaultCursor;
   BitBtn8Click(Sender);
   Redrawing := false;
end;


procedure TPickRotationForm.BitBtn2Click(Sender: TObject);
begin
   PickLineSizeAndColor('',BitBtn2,RotLineColor,RotLineSize);
end;


procedure TPickRotationForm.FormCreate(Sender: TObject);
var
   i : integer;
   DataThere : tStringList;
begin
   if not FileExists(CurrentMotionsFile) then DownloadFileFromWeb(WebDataDownLoadDir + ExtractFileName(CurrentMotionsFile),CurrentMotionsFile);

   RotLineSize := 3;
   RotLineColor := claPurple;
   DesiredRecord := 1;
   PlatesDB := 0;
   BoundariesDB := 0;
   Redrawing := false;
   ThisGraph := Nil;
   P1P2Color := clLime;
   P1P3Color := clBlue;
   P2P3Color := clAqua;
   ColorBitBtn(BitBtn16,P1P2Color);
   ColorBitBtn(BitBtn17,P1P3Color);
   ColorBitBtn(BitBtn18,P2P3Color);

   Petmar.ColorLineWidthBitBtn(BitBtn2,RotLineColor,RotLineSize);
   OpenNumberedGISDataBase(MotionTable,CurrentMotionsFile);
   OpenNumberedGISDataBase(PoleTable,PlatePolesFile);

    DataThere := GISDB[MotionTable].MyData.UniqueEntriesInDB('MODEL');
    for i := 0 to pred(DataThere.Count) do ModelComboBox.Items.Add(DataThere.Strings[i]);
    ModelComboBox.Text := MDDef.PlateModel;
    DataThere.Free;
    ModelComboBoxChange(Nil);

    DataThere := GISDB[PoleTable].MyData.UniqueEntriesInDB('MODEL');

    for i := 0 to pred(DataThere.Count) do TotalPolesComboBox.Items.Add(DataThere.Strings[i]);
    TotalPolesComboBox.Text := TotalPolesComboBox.Items[0];
    DataThere.Free;
    TotalPolesComboBoxChange(Nil);

    CheckBox1.Checked := MDDef.PlateVelocityDiagram;
    CheckBox2.Checked := MDDef.PlateRotateContCrust;
    CheckBox3.Checked := MDDef.PlateRotateBoundaries;
    CheckBox4.Checked := MDDef.PlateLabelVectors;
    CheckBox5.Checked := MDDef.ResultantPlateVectors;
    CheckBox8.Checked := MDDef.PlateTectonicVelocity;
    CheckBox9.Checked := MDDef.PlateNumbers;
    CheckBox10.Checked := MDDef.PlateVectors;

    RadioGroup1Click(Sender);
    TrackBar1.Position := MDDef.RotateVectMult;
    ClientHeight := Panel1.Top + Panel1.Height;
    wmDEM.FormPlacementInCorner(self,lpNEMap);
end;


procedure TPickRotationForm.BitBtn3Click(Sender: TObject);
begin
   Close;
end;


procedure TPickRotationForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   CloseAndNilNumberedDB(MotionTable);
   CloseAndNilNumberedDB(PoleTable);

   if (MapForm <> Nil) then begin
      MapForm.Closable := true;
      MapForm.Close;
      MapForm := Nil;
   end;

   ApplicationProcessMessages;
   Action := caFree;
end;


procedure TPickRotationForm.BitBtn4Click(Sender: TObject);
begin
    GISdb[PlatesDB].LayerIsOn := MDDef.PlateRotateBoundaries;
    GISdb[BoundariesDB].LayerIsOn := MDDef.PlateRotateContCrust;

   MapForm.DoFastMapRedraw;
   MapForm.SetFocus;
end;


procedure TPickRotationForm.BitBtn5Click(Sender: TObject);
var
   Bitmap : tMyBitmap;
   i : integer;
begin
   {$IfDef RecordPlateRotations} WriteLineToDebugFile('TPickRotationForm.BitBtn5Click in, cont=' + ContinentComboBox.Text + '  TheDB=' + GISdb[PlatesDB].MyData.TableName);  {$EndIf}
   if (ContinentComboBox.Text <> '') then begin
      MapForm.DoFastMapRedraw;
      ShowHourglassCursor;
      GISdb[PoleTable].MyData.ApplyFilter('PLATE = ' + QuotedStr(ContinentComboBox.Text)  + ' AND MODEL = ' + QuotedStr(TotalPolesComboBox.Text));
      i := 0;
      GISdb[PlatesDB].MyData.ApplyFilter('NAME=' + QuotedStr(UpperCase(ContinentComboBox.Text)));
      if (GISdb[PlatesDB].MyData.RecordCount > 0) then begin
         DesiredRecord := GISdb[PlatesDB].MyData.RecNo;
         {$IfDef RecordPlateRotations}  WriteLineToDebugFile('  DesiredRecord=' + IntToStr(DesiredRecord));{$EndIf}
         if not CopyImageToBitmap(MapForm.Image1,Bitmap) then exit;
         Bitmap.Canvas.Pen.Width := RotLineSize;
         while not GISdb[PoleTable].MyData.Eof do begin
            RotationData[1] := GISdb[PoleTable].MyData.GetFieldByNameAsFloat('POLE_LAT');
            RotationData[2] := GISdb[PoleTable].MyData.GetFieldByNameAsFloat('POLE_LONG');
            RotationData[3] := GISdb[PoleTable].MyData.GetFieldByNameAsFloat('OMEGA');
            ComputeRotationMatrix;
            Bitmap.Canvas.Pen.Color := RainbowColorFunct(i,0,GISdb[PoleTable].MyData.RecordCount);
            inc(i);
            GISdb[PlatesDB].aShapeFile.RotateAndPlotSingleRecord(MapForm.MapDraw,Bitmap,DesiredRecord,PlateRotationMatrix);
            GISdb[PoleTable].MyData.Next;
         end;
         MapForm.Image1.Picture.Graphic := Bitmap;
         MapForm.SetFocus;
         Bitmap.Free;
      end
      else MessageToContinue('No plate outline');
      ShowDefaultCursor;
   end
   else MessageToContinue('Select continent');
end;


procedure TPickRotationForm.BitBtn6Click(Sender: TObject);
var
   Bitmap : tMyBitmap;
begin
   {$IfDef RecordPlateRotations} WriteLineToDebugFile('TPickRotationForm.BitBtn6Click'); {$EndIf}
   MapForm.DoFastMapRedraw;
   if not CopyImageToBitmap(MapForm.Image1,Bitmap) then exit;
   if (Sender <> BitBtn9) then begin
      Bitmap.Canvas.Pen.Color := ConvertPlatformColorTotColor(RotLineColor);
      ShowHourglassCursor;
   end
   else begin
      Bitmap.Canvas.Draw(0,0,BaseBitmap);
      Bitmap.Canvas.Pen.Color := BaseBitmap.Canvas.Pen.Color
   end;
   Bitmap.Canvas.Pen.Width := RotLineSize;

   GISdb[PoleTable].MyData.ApplyFilter('TIME = ' + TimeComboBox.Text + ' AND MODEL = ' + QuotedStr(TotalPolesComboBox.Text));
   {$IfDef RecordPlateRotations}  WriteLineToDebugFile('PoleTable filter=' + GISdb[PoleTable].MyData.Filter);{$EndIf}
   while not GISdb[PoleTable].MyData.Eof do begin
      RotationData[1] := GISdb[PoleTable].MyData.GetFieldByNameAsFloat('POLE_LAT');
      RotationData[2] := GISdb[PoleTable].MyData.GetFieldByNameAsFloat('POLE_LONG');
      RotationData[3] := GISdb[PoleTable].MyData.GetFieldByNameAsFloat('OMEGA');
      ComputeRotationMatrix;

      GISdb[PlatesDB].MyData.ApplyFilter('NAME=' + QuotedStr(UpperCase(GISdb[PoleTable].MyData.GetFieldByNameAsString('PLATE'))));
      if (GISdb[PlatesDB].MyData.RecordCount > 0) then GISdb[PlatesDB].aShapeFile.RotateAndPlotSingleRecord(MapForm.MapDraw,Bitmap,GISdb[PlatesDB].MyData.RecNo,PlateRotationMatrix);
      GISdb[PoleTable].MyData.Next;
   end;

   MapForm.Image1.Picture.Graphic := Bitmap;
   MapForm.SetFocus;
   Bitmap.Free;
   ShowDefaultCursor;
end;


procedure TPickRotationForm.DrawTJmotion(vx1,vy1,vx2,vy2 : float32; Boundary : shortString; LowerPlate : integer; AzText : shortstring; Color : tColor);
var
   x,y,Az : float32;
   Bitmap : tMyBitmap;
begin
   CopyImageToBitmap(ThisGraph.Image1,Bitmap);
   Bitmap.Canvas.Pen.Color := color;
   Bitmap.Canvas.Pen.Width := 3;
   DrawLine(Bitmap,ThisGraph.GraphDraw.GraphX(vx1),ThisGraph.GraphDraw.GraphY(vy1),ThisGraph.GraphDraw.GraphX(vx2),ThisGraph.GraphDraw.GraphY(vy2));
  if (Boundary = 'Ridge') or (Boundary = 'Transform fault') then begin
     x := 0.5 * (vx1 + vx2);
     y := 0.5 * (vy1 + vy2);
     Az := HeadingOfLine((vx2-vx1),(vy2-vy1));
     if (Boundary = 'Ridge') then Az := Az + 90;
  end;

  if (Boundary = 'Trench') then begin
     Az := StrToFloat(AzText);
     if LowerPlate = 0 then begin
        x := vx2;
        y := vy2;
     end
     else begin
        x := vx1;
        y := vy1;
     end;
  end;

  PlotOrientedLine(Bitmap,ThisGraph.GraphDraw.GraphX(x),ThisGraph.GraphDraw.GraphY(y),ThisGraph.GraphDraw.XWindowSize,round(AZ),ConvertTColorToPlatformColor(Color),1);
  PlotOrientedLine(Bitmap,ThisGraph.GraphDraw.GraphX(x),ThisGraph.GraphDraw.GraphY(y),ThisGraph.GraphDraw.XWindowSize,round(AZ+180),ConvertTColorToPlatformColor(Color),1);

  ThisGraph.Image1.Picture.Graphic := bitmap;
  Bitmap.Free;
end;

procedure TPickRotationForm.ComboBox6Change(Sender: TObject);
begin
   ComboBox9.Enabled := ComboBox6.Text = 'Trench';
   Edit6.Enabled := ComboBox6.Text = 'Trench';
end;

procedure TPickRotationForm.ComboBox7Change(Sender: TObject);
begin
   ComboBox10.Enabled := ComboBox7.Text = 'Trench';
   Edit7.Enabled := ComboBox7.Text = 'Trench';
end;

procedure TPickRotationForm.ComboBox8Change(Sender: TObject);
begin
   ComboBox11.Enabled := ComboBox8.Text = 'Trench';
   Edit8.Enabled := ComboBox8.Text = 'Trench';
end;

procedure TPickRotationForm.ComputeRotationMatrix;
begin
   RotationMatrix(RotationData[1] * DegToRad, RotationData[2] * DegToRad, RotationData[3] * DegToRad, PlateRotationMatrix);
end;

procedure TPickRotationForm.GetRotationMatrix;
begin
   CheckEditString(Edit1.Text,RotationData[1]);
   CheckEditString(Edit2.Text,RotationData[2]);
   CheckEditString(Edit3.Text,RotationData[3]);
   CheckEditString(Edit4.Text,RotationRate);
   CheckEditString(Edit5.Text,RotationMa);
   ComputeRotationMatrix;
end;


procedure TPickRotationForm.BitBtn7Click(Sender: TObject);
begin
   GetRotationMatrix;
   ChangeDEMNowDoing(GetDriftVectors);
   MapForm.SetFocus;
end;


procedure TPickRotationForm.RadioGroup1Click(Sender: TObject);


         procedure FillPlateOutlinesAvailable;
         var
            DataThere : tStringList;
            i : integer;
         begin
             ContinentComboBox.Items.Clear;
             if (RadioGroup1.ItemIndex in [2]) then begin
                GISdb[PlatesDB].MyData.First;
                DataThere := tStringList.Create;
                DataThere.Sorted := true;
                while not GISdb[PlatesDB].MyData.EOF do begin
                   DataThere.Add(GISdb[PlatesDB].MyData.GetFieldByNameAsString('NAME'));
                   GISdb[PlatesDB].MyData.Next;
                end;
             end
             else begin
                if (GISdb[PoleTable] <> Nil) then begin
                   DataThere := GISdb[PoleTable].MyData.UniqueEntriesInDB('PLATE');
                   DataThere.Sorted := true;
                end;
             end;
             for i := 0 to pred(DataThere.Count) do ContinentComboBox.Items.Add(DataThere.Strings[i]);
             DataThere.Free;
             ContinentComboBox.Text := ContinentComboBox.Items[0];
         end;


begin
   Panel3.Visible := RadioGroup1.ItemIndex in [2];
   Panel4.Visible := RadioGroup1.ItemIndex in [4];
   GroupBox2.Visible := RadioGroup1.ItemIndex in [1];
   GroupBox3.Visible := RadioGroup1.ItemIndex in [0,2,3,4];

   ContinentComboBox.Enabled := RadioGroup1.ItemIndex in [1,2];
   ModelComboBox.Enabled := RadioGroup1.ItemIndex in [0,3,4];
   Label9.Enabled := RadioGroup1.ItemIndex in [1,2];
   BitBtn1.Enabled := RadioGroup1.ItemIndex in [1,2];

   BitBtn5.Enabled := RadioGroup1.ItemIndex = 1;
   BitBtn6.Enabled := RadioGroup1.ItemIndex = 1;
   BitBtn9.Enabled := RadioGroup1.ItemIndex = 1;

   BitBtn7.Enabled := RadioGroup1.ItemIndex = 0;

   Edit1.Enabled := RadioGroup1.ItemIndex = 2;
   Edit2.Enabled := RadioGroup1.ItemIndex = 2;
   Edit3.Enabled := RadioGroup1.ItemIndex = 2;
   BitBtn8.Enabled := RadioGroup1.ItemIndex in [0,2];
   BitBtn10.Enabled := RadioGroup1.ItemIndex = 2;

   BitBtn2.Enabled := RadioGroup1.ItemIndex in [0,2,3];
   BitBtn13.Enabled := RadioGroup1.ItemIndex in [0,3,4];

   BitBtn1.Enabled := RadioGroup1.ItemIndex in [2];

   BitBtn12.Enabled := RadioGroup1.ItemIndex in [0,3,4];
   Plate1ComboBox.Enabled := RadioGroup1.ItemIndex in [0,3,4];

   Plate2ComboBox.Enabled := RadioGroup1.ItemIndex in [3,4];

   Label13.Enabled := RadioGroup1.ItemIndex in [0,1,3,4];
   Label15.Enabled := RadioGroup1.ItemIndex in [3,4];
   Label16.Enabled := RadioGroup1.ItemIndex in [4];
   Label6.Enabled := RadioGroup1.ItemIndex in [1];
   Label14.Enabled := RadioGroup1.ItemIndex in [0,3,4];

   Trackbar2.Enabled := RadioGroup1.ItemIndex <> 4;

   Label7.Enabled := RadioGroup1.ItemIndex in [1,3,4];
   Label8.Enabled := RadioGroup1.ItemIndex in [1,3,4];
   Edit4.Enabled := RadioGroup1.ItemIndex in [3];
   Edit5.Enabled := RadioGroup1.ItemIndex in [];

   Plate3ComboBox.Enabled := RadioGroup1.ItemIndex in [4];

   TotalPolesComboBox.Enabled := RadioGroup1.ItemIndex in [1];
   Label10.Enabled := TotalPolesComboBox.Enabled;

   CheckBox1.Enabled := RadioGroup1.ItemIndex in [1,3,4];    //velocity diagram
   CheckBox4.Enabled := RadioGroup1.ItemIndex in [0,1,3,4];  //label velocities
   CheckBox5.Enabled := RadioGroup1.ItemIndex in [0,1,3,4];  //resultant vector

   if RadioGroup1.ItemIndex in [0] then GroupBox3.Caption := 'Current plate motion';
   if RadioGroup1.ItemIndex in [1] then GroupBox3.Caption := 'Past total poles rotation';
   if RadioGroup1.ItemIndex in [2] then GroupBox3.Caption := 'Arbitrary rotation';
   if RadioGroup1.ItemIndex in [3] then GroupBox3.Caption := 'Plate boundary motion';
   if RadioGroup1.ItemIndex in [4] then GroupBox3.Caption := 'Triple junction motion';

   TimeComboBox.Enabled := RadioGroup1.ItemIndex in [1];
   FillPlateOutlinesAvailable;
   ClientHeight := Panel1.Top + Panel1.Height;
   ChangeDEMNowDoing(JustWandering);
end;


procedure TPickRotationForm.Edit5Change(Sender: TObject);
begin
   if (Edit5.Text <> '') then begin
      CheckEditString(Edit5.Text,RotationMa);
      Edit3.Text := RealToString(RotationRate * RotationMa,-8,-2);
      Edit4.Text := RealToString(RotationRate,-8,-4);
      GetRotationMatrix;
   end;
end;


procedure TPickRotationForm.Edit1Change(Sender: TObject);
begin
   GetRotationMatrix;
end;

procedure TPickRotationForm.Edit2Change(Sender: TObject);
begin
   GetRotationMatrix;
end;

procedure TPickRotationForm.Edit3Change(Sender: TObject);
begin
   GetRotationMatrix;
end;


procedure TPickRotationForm.BitBtn8Click(Sender: TObject);
var
   xp,yp : integer;
   Lat,Long : float64;
begin
   if RadioGroup1.ItemIndex in [0] then begin
      Lat := GISdb[MotionTable].MyData.GetFieldByNameAsFloat('LAT');
      Long := GISdb[MotionTable].MyData.GetFieldByNameAsFloat('LONG');
   end
   else begin
      CheckEditString(Edit1.Text,Lat);
      CheckEditString(Edit2.Text,Long);
   end;
   if (Long > 180) then Long := Long - 360;
   MapForm.MapDraw.LatLongDegreeToScreen(Lat,Long,xp,yp);
   if MapForm.MapDraw.OnScreen(xp,yp) then Petmar.ScreenSymbol(MapForm.Image1.Canvas,xp,yp,Cross,5,claLime);
   MapForm.MapDraw.LatLongDegreeToScreen(-Lat,Long+180,xp,yp);
   if MapForm.MapDraw.OnScreen(xp,yp) then Petmar.ScreenSymbol(MapForm.Image1.Canvas,xp,yp,Cross,5,claRed);
end;

procedure TPickRotationForm.BitBtn9Click(Sender: TObject);
{$IfDef ExMovies}
begin
{$Else}
var
   i : integer;
   MovieList : tStringList;
   fName : PathStr;
begin
   MapForm.DoFastMapRedraw;
   if not CopyImageToBitmap(MapForm.Image1,BaseBitmap) then exit;
   MovieList := tStringList.Create;
   StartProgress('Drift movie');
   for i := pred(TimeComboBox.Items.Count) downto 0 do begin
      UpdateProgressBar((TimeComboBox.Items.Count-i)/TimeComboBox.Items.Count);
      TimeComboBox.Text := TimeComboBox.Items[i];
      BaseBitmap.Canvas.Pen.Color := RainbowColorFunct(i,0,GISdb[PoleTable].MyData.RecordCount);
      BitBtn6Click(Sender);
      FName := 'World at ' + TimeComboBox.Text + ' Ma ';
      MapForm.Caption := fName;
      FName := fName + MovieFileExt;
      MapForm.Image1.Picture.SaveToFile(DEMdefs.MovieDir + fName);
      MovieList.Add(fName);
  end;

  EndProgress;
  BaseBitmap.Canvas.Pen.Width := 3;
  GISdb[PoleTable].MyData.First;
   while (not GISdb[PoleTable].MyData.Eof) do begin
      GISdb[PlatesDB].MyData.ApplyFilter('NAME=' + QuotedStr(UpperCase(GISdb[PoleTable].MyData.GetFieldByNameAsString('PLATE'))));
      if (GISdb[PlatesDB].MyData.RecordCount > 0) then
         GISdb[PlatesDB].aShapeFile.PlotSingleRecordMap(MapForm.MapDraw,BaseBitmap,GISdb[PlatesDB].MyData.RecNo);
      GISdb[PoleTable].MyData.Next;
   end;
   MapForm.Image1.Picture.Graphic := BaseBitmap;

   FName := 'World at 0 Ma' + MovieFileExt;
   MapForm.Image1.Picture.SaveToFile(DEMdefs.MovieDir + fName);
   MovieList.Add(fName);

   fName := 'platemotion.mov';
   MovieList.SaveToFile(DEMdefs.MovieDir + fName);
   PetImage.MakeMovie(fName);

   BaseBitmap.Free;
   MovieList.Free;
{$EndIf}
end;


procedure TPickRotationForm.CheckBox10Click(Sender: TObject);
begin
    MDDef.PlateVectors := CheckBox10.Checked;
end;

procedure TPickRotationForm.CheckBox1Click(Sender: TObject);
begin
    MDDef.PlateVelocityDiagram := CheckBox1.Checked;
end;


procedure TPickRotationForm.CheckBox2Click(Sender: TObject);
begin
   if (PlatesDB <> 0) then begin
      MDDef.PlateRotateBoundaries := CheckBox2.Checked;
      if CheckBox2.Checked then GISdb[PlatesDB].dbOpts.DBAutoShow := 1 else GISdb[PlatesDB].dbOpts.DBAutoShow := 0;
      BitBtn4Click(nil);
   end;
end;

procedure TPickRotationForm.CheckBox3Click(Sender: TObject);
begin
   if (BoundariesDB <> 0) then begin
      MDDef.PlateRotateContCrust := CheckBox3.Checked;
      if CheckBox3.Checked then GISdb[BoundariesDB].dbOpts.DBAutoShow := 1 else GISdb[BoundariesDB].dbOpts.DBAutoShow := 0;
      BitBtn4Click(nil);
   end;
end;

procedure TPickRotationForm.CheckBox4Click(Sender: TObject);
begin
   MDDef.PlateLabelVectors := CheckBox4.Checked;
end;

procedure TPickRotationForm.CheckBox5Click(Sender: TObject);
begin
    MDDef.ResultantPlateVectors := CheckBox5.Checked;
end;

procedure TPickRotationForm.CheckBox6Click(Sender: TObject);
begin
   MapForm.MapDraw.MakeMapGrayscale := CheckBox6.Checked;
   MapForm.MapDraw.SubdueBase := CheckBox6.Checked;
   MapForm.MapDraw.GrayscaleSubdueOverlays := CheckBox6.Checked;
   MapForm.DoFastMapRedraw;
end;

procedure TPickRotationForm.CheckBox7Click(Sender: TObject);
begin
   Map_Overlays.AddOrSubtractOverlay(MapForm,ovoWorldOutlines,CheckBox7.Checked);
   MapForm.DoFastMapRedraw;
end;

procedure TPickRotationForm.CheckBox8Click(Sender: TObject);
begin
   MDDef.PlateTectonicVelocity := checkBox8.Checked;
end;

procedure TPickRotationForm.CheckBox9Click(Sender: TObject);
begin
   MDDef.PlateNumbers := CheckBox9.Checked;
end;

procedure TPickRotationForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\plate_tectonics\rotation_control.htm');
end;

procedure TPickRotationForm.BitBtn10Click(Sender: TObject);
begin
   ChangeDEMNowDoing(PickEulerPole);
end;

procedure TPickRotationForm.BitBtn12Click(Sender: TObject);
begin
   if RadioGroup1.ItemIndex = 0 then ChangeDEMNowDoing(GetDriftVectors)
   else ChangeDEMNowDoing(GetSpreadingRate);
end;

procedure TPickRotationForm.BitBtn13Click(Sender: TObject);
begin
   EditMyFont(MDDef.LegendFont);
end;

procedure TPickRotationForm.BitBtn14Click(Sender: TObject);
begin
   if (ThisGraph <> nil) then begin
      ThisGraph.RedrawDiagram11Click(Nil);
      DrawTJmotion(xv1,yv1,xv2,yv2,ComboBox6.Text,ComboBox9.ItemIndex,Edit6.Text,P1P2Color);
      DrawTJmotion(xv1,yv1,xv3,yv3,ComboBox7.Text,ComboBox10.ItemIndex,Edit7.Text,P1P3Color);
      DrawTJmotion(xv2,yv2,xv3,yv3,ComboBox8.Text,ComboBox11.ItemIndex,Edit8.Text,P2P3Color);
   end;
end;

procedure TPickRotationForm.BitBtn16Click(Sender: TObject);
begin
   QueryTColor(BitBtn16,P1P2Color);
end;

procedure TPickRotationForm.BitBtn17Click(Sender: TObject);
begin
   QueryTColor(BitBtn17,P1P3Color);
end;

procedure TPickRotationForm.BitBtn18Click(Sender: TObject);
begin
   QueryTColor(BitBtn18,P2P3Color);
end;

procedure TPickRotationForm.Plate1ComboBoxChange(Sender: TObject);


   procedure SetUpValues(Table : tMyData);
   begin
       Edit1.Text := Table.GetFieldByNameAsString('LAT');
       Edit2.Text := Table.GetFieldByNameAsString('LONG');
       Edit3.Text := Table.GetFieldByNameAsString('OMEGA');
       Edit4.Text := '';
       Edit5.Text := '';
       GetRotationMatrix;
   end;


begin
    if (RadioGroup1.ItemIndex in [0,2,3,4]) then begin
       GISdb[MotionTable].MyData.ApplyFilter( 'MODEL = ' + QuotedStr(ModelComboBox.Text) + ' AND PLATE_NAME= ' + QuotedStr(Plate1ComboBox.Text));
       if (GISdb[MotionTable].MyData.RecordCount = 1) then SetUpValues(GISdb[MotionTable].MyData);
   end
   else begin
      GISdb[PoleTable].MyData.ApplyFilter('TIME = ' + QuotedStr(TimeComboBox.Text) + ' AND PLATE = ' + QuotedStr(Plate1ComboBox.Text));
      {$IfDef RecordPlateRotations} WriteLineToDebugFile(GISdb[PoleTable].MyData.Filter); {$EndIf}
      if (GISdb[PoleTable].MyData.RecordCount = 1) then SetUpValues(GISdb[PoleTable].MyData);
   end;
end;


procedure TPickRotationForm.ModelComboBoxChange(Sender: TObject);
var
   DataThere : tStringList;

   procedure FillComboBox(Box : tComboBox; Num : integer);
   var
      i : integer;
   begin
      Box.Items.Clear;
      for i := 0 to pred(DataThere.Count) do Box.Items.Add(DataThere.Strings[i]);
      for i := 0 to pred(DataThere.Count) do if (DataThere.Strings[i] = Box.Text) then exit;
      Box.Text := '';
   end;

begin
    {$IfDef RecordPlateRotations} WriteLineToDebugFile('TPickRotationForm.ModelComboBoxChange in ' + ModelComboBox.Text); {$EndIf}
    MDDef.PlateModel := ModelComboBox.Text;
    GISdb[MotionTable].MyData.ApplyFilter( 'MODEL=' + QuotedStr(ModelComboBox.Text));
    DataThere := GISdb[MotionTable].MyData.UniqueEntriesInDB('PLATE_NAME');
    {$IfDef RecordPlateRotations}  WriteLineToDebugFile(' continents in model= ' + IntToStr(DataThere.Count));  {$EndIf}
    FillComboBox(Plate1ComboBox,0);
    FillComboBox(Plate2ComboBox,1);
    FillComboBox(Plate3ComboBox,2);
    DataThere.Free;
end;


procedure TPickRotationForm.TotalPolesComboBoxChange(Sender: TObject);
var
   DataThere : tStringList;
   i         : integer;
   Values    : array[0..100] of float64;
begin
    GISdb[PoleTable].MyData.ApplyFilter( 'MODEL=' + QuotedStr(TotalPolesComboBox.Text));
    DataThere := GISdb[PoleTable].MyData.UniqueEntriesInDB('TIME');
    for i := 0 to pred(DataThere.Count) do Values[i] := StrToFloat(DataThere.Strings[i]);
    HeapSort(DataThere.Count,Values);
    TimeComboBox.Items.Clear;
    for i := 0 to pred(DataThere.Count) do TimeComboBox.Items.Add(FloatToStr(Values[i]));
    TimeComboBox.Text := TimeComboBox.Items[0];
    DataThere.Free;
end;


procedure TPickRotationForm.TrackBar1Change(Sender: TObject);
begin
   MDDef.RotateVectMult := TrackBar1.Position;
end;

procedure TPickRotationForm.TrackBar2Change(Sender: TObject);
begin
   Edit3.Text := IntToStr(TrackBar2.Position);
   if (DEMNowDoing = PickEulerPole) and (not Redrawing) then BitBtn1Click(Sender);
   Self.SetFocus;
end;

initialization
finalization
   {$IfDef RecordPlateRotations} WriteLineToDebugFile('RecordPlateRotations active in plate_rotate'); {$EndIf}
end.
