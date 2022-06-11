unit three_point_problem;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordThreePoint}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms, Dialogs,StdCtrls, Buttons,
  System.UITypes,
  Petmar_types, petmar_db,DEMMapf,NetMainW;

type
  TThreePointer = class(TForm)
    BitBtn1: TBitBtn;
    Memo1: TMemo;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    Edit1: TEdit;
    Strike: TLabel;
    Label1: TLabel;
    Edit2: TEdit;
    ComboBox1: TComboBox;
    BitBtn7: TBitBtn;
    HelpBtn: TBitBtn;
    RedrawSpeedButton12: TSpeedButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox4: TCheckBox;
    Label2: TLabel;
    Edit3: TEdit;
    TraceContactBitBtn12: TBitBtn;
    Button1: TButton;
    BitBtn22: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn8: TBitBtn;
    DataBaseSpeedButton28: TSpeedButton;
    procedure BitBtn4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);

    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure TraceContactBitBtn12Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BitBtn22Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure DataBaseSpeedButton28Click(Sender: TObject);
  private
    procedure ClearValues;
    procedure UpdateColor;
    { Private declarations }
  public
    { Public declarations }
    aMapOwner : tMapForm;
    ThreePointNet : TNetForm;
    PlanarDevDEM,
    LocalContactDB,
    CurrentColorNumber : integer;
    xDEMg1,yDEMg1 : float64;
    CurrentDip,CurrentStrike,CurrentDipDir : float32;
    PointUpdate : boolean;
    function SetDipAndStrike : boolean;
    procedure EnableOptions;
    procedure ThicknessAtLatLong(Lat,long : float64);
    procedure DrawPlaneAtPoint(Lat,Long,Dip,Strike,DipDir : float64);
  end;


procedure OpenStructuralGeologyForm(inMapOwner : tMapForm);

var
  ThreePointer : TThreePointer;

implementation

{$R *.dfm}


uses
   Nevadia_main,DEMDefs,Make_Tables,DEMCoord,Petmath,Petmar,Petmar_geology,
   DEMdips,DEM_Manager,DEMDataBase;


procedure OpenStructuralGeologyForm;
begin
   if (ThreePointer = Nil) then begin
      ThreePointer := TThreePointer.Create(Application);
      ThreePointer.aMapOwner := inMapOwner;
      ThreePointer.CheckBox4.Checked := MDDef.AutoIncGeoColor;
      ThreePointer.CurrentColorNumber := 0;
      ThreePointer.Show;
      MDDef.GeoContactColor := LineColors10[ThreePointer.CurrentColorNumber];
   end;
end;


procedure CloseStructuralGeologyForm;
begin
   ThreePointer.Close;
   ThreePointer := Nil;
end;


function TThreePointer.SetDipAndStrike : boolean;
begin
   Result := (Edit1.Text <> '') and (Edit2.Text <> '') and (ComboBox1.Text <> '');
   if Result then begin
      CheckEditString(Edit1.Text,BroadCastPlaneData.aStrike);
      CheckEditString(Edit2.Text,BroadCastPlaneData.Dip);
      QuadrantDipDirection(BroadCastPlaneData.DipDir,BroadCastPlaneData.aStrike,ComboBox1.Text);
      CheckEditString(Edit1.Text,CurrentStrike);
      CheckEditString(Edit2.Text,CurrentDip);
      QuadrantDipDirection(CurrentDipDir,BroadCastPlaneData.aStrike,ComboBox1.Text);
   end;
end;


procedure TThreePointer.ThicknessAtLatLong(Lat, long: float64);
var
   z,Thick : float32;
begin
   if ValidDEM(PlanarDevDEM) then begin
      if DEMGlb[PlanarDevDEM].GetElevFromLatLongDegree(Lat,Long,z) then begin
         CheckEditString(Edit2.Text,BroadCastPlaneData.Dip);
         Thick := cosDeg(BroadCastPlaneData.Dip) * z;
         aMapOwner.OverlayContourFromSecondDEM(PlanarDevDEM,z,MDDef.GeoContactColor);
         Memo1.Lines.Add('');
         Memo1.Lines.Add('Thickness from parallel plane');
         Memo1.Lines.Add(LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod) + '  ' + RealToString(Thick,-12,1) + ' m' );
      end;
   end;
end;

procedure TThreePointer.BitBtn22Click(Sender: TObject);
begin
    if (StructureOptions = Nil) then StructureOptions := TStructureOptions.Create(Application);
    StructureOptions.ShowModal;
    MDDef.StructGeologyShowNet := StructureOptions.CheckBox1.Checked;
    MDDef.StructGeologyLabelVals := StructureOptions.LabelValueCheckBox.Checked;
end;

procedure TThreePointer.BitBtn2Click(Sender: TObject);
begin
   TraceContactBitBtn12Click(Sender);
end;

procedure TThreePointer.BitBtn3Click(Sender: TObject);
begin
   ChangeDEMNowDoing(SeekingThickness);
end;


procedure TThreePointer.DrawPlaneAtPoint(Lat,Long,Dip,Strike,DipDir : float64);
var
   xDEMg1,yDEMg1 : float64;
   i : integer;

   procedure TraceOnAMap(TheMap : tMapForm);
   begin
       if (TheMap.MapDraw.DEMonMap <> 0) then begin
          DEMGlb[TheMap.MapDraw.DEMonMap].LatLongDegreeToDEMGrid(Lat,Long,xDEMg1,yDEMg1);
          BroadCastPlaneData.xglo := round(xDEMg1 - MDDef.ThreePointExtrap / DEMGlb[TheMap.MapDraw.DEMonMap].AverageXSpace);
          BroadCastPlaneData.xghi := round(xDEMg1 + MDDef.ThreePointExtrap / DEMGlb[TheMap.MapDraw.DEMonMap].AverageXSpace);
          BroadCastPlaneData.yglo := round(yDEMg1 - MDDef.ThreePointExtrap / DEMGlb[TheMap.MapDraw.DEMonMap].AverageYSpace);
          BroadCastPlaneData.yghi := round(yDEMg1 + MDDef.ThreePointExtrap / DEMGlb[TheMap.MapDraw.DEMonMap].AverageYSpace);
          TheMap.ThreadCheckPlane(not SetDipAndStrike,xDEMg1,yDEMg1,Dip,Strike,DipDir);
       end;
   end;


begin
   //needed to be called from DEMMapf
    if MDDef.TraceContactsOnDEM then begin
       for i := 0 to pred(WMDEM.MDIChildCount) do begin
         if WMDEM.MDIChildren[i] is tMapForm then TraceOnAMap(WMDEM.MDIChildren[i] as tMapForm);
       end;
    end;
    if MDDef.ShowContactsOnStereoNet then begin
       if (ThreePointNet = Nil) then begin
          ThreePointNet := TNetForm.Create(Application);
          ThreePointNet.nd.Closable := false;
          ThreePointNet.nd.NewNet;
       end;
       ThreePointNet.nd.GreatCircleOnNet(Dip,DipDir,MDDef.GeoContactWidth,MDDef.GeoContactColor);
       ThreePointNet.UpdateDisplay;
    end;
end;


procedure TThreePointer.BitBtn4Click(Sender: TObject);
begin
  CloseStructuralGeologyForm;
end;


procedure TThreePointer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   ThreePointNet.nd.Closable := true;
   Action := caFree;
   ThreePointNet.Close;
end;


procedure TThreePointer.EnableOptions;
begin
   BitBtn3.Enabled := (PlanarDevDEM <> 0);
   BitBtn8.Enabled := (PlanarDevDEM <> 0);
end;

procedure TThreePointer.FormCreate(Sender: TObject);
begin
   ColorLineWidthBitBtn(BitBtn1,MDDef.GeoContactColor,MDDef.GeoContactWidth);
   CheckBox1.Checked := MDDef.TraceContactsOnDEM;
   CheckBox2.Checked := MDDef.ShowContactsOnStereoNet;
   Edit3.Text := IntToStr(MDDef.ThreePointExtrap);
   wmdem.FormPlacementInCorner(Self);
   ThreePointNet := Nil;
   PlanarDevDEM := 0;
   LocalContactDB := 0;
   PointUpdate := true;
   EnableOptions;
end;


procedure TThreePointer.TraceContactBitBtn12Click(Sender: TObject);
var
   fName : PathStr;
begin
   ClearValues;
   UpdateColor;
   ChangeDEMNowDoing(TraceContact);
   if (Sender = TraceContactBitBtn12) then begin
      fName := Petmar.NextFileNumber(MDTempDir, 'line_trace',DefaultDBExt);
      Make_Tables.CreateLatLongTable(fName);
      aMapOwner.LineTable := aMapOwner.LoadDataBaseFile(fName);
      Button1.Enabled := true;
   end;
   ShowDefaultCursor;
end;

procedure TThreePointer.BitBtn1Click(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('Geologic contact',BitBtn1,MDDef.GeoContactColor,MDDef.GeoContactWidth);
end;


procedure TThreePointer.UpdateColor;
begin
  if MDDef.AutoIncGeoColor then begin
     inc(CurrentColorNumber);
     if (CurrentColorNumber > 10) then CurrentColorNumber := 0;
     MDDef.GeoContactColor := LineColors10[CurrentColorNumber];
     ColorLineWidthBitBtn(BitBtn1,MDDef.GeoContactColor,MDDef.GeoContactWidth);
  end;
end;

procedure TThreePointer.CheckBox1Click(Sender: TObject);
begin
   MDDef.TraceContactsOnDEM := CheckBox1.Checked;
end;

procedure TThreePointer.CheckBox2Click(Sender: TObject);
begin
   MDDef.ShowContactsOnStereoNet := CheckBox2.Checked;
end;


procedure TThreePointer.CheckBox4Click(Sender: TObject);
begin
   MDDef.AutoIncGeoColor := ThreePointer.CheckBox4.Checked;
end;

procedure TThreePointer.ClearValues;
begin
   Edit1.Text := '';
   Edit2.Text := '';
   ComboBox1.Text := '';
end;

procedure TThreePointer.DataBaseSpeedButton28Click(Sender: TObject);
begin
   {$IfDef RecordThreePoint} WriteLinetoDebugFile('TThreePointer.DataBaseSpeedButton28Click in'); {$EndIf}
   if (LocalContactDB <> 0) then CloseAndNilNumberedDB(LocalContactDB);
   LocalContactDB := aMapOwner.OpenDBonMap('Contact','',true);
   if GISdb[LocalContactDB].ItsAPointDB then begin
      PointUpdate := false;
      GISdb[LocalContactDB].DBTableF.hreepointproblem1Click(Sender);
      PointUpdate := true;
      aMapOwner.OverlayContourFromSecondDEM(PlanarDevDEM,0,MDDef.GeoContactColor);
   end
   else begin
      MessageToContinue('Requires a points DB');
      CloseAndNilNumberedDB(LocalContactDB);
   end;
   {$IfDef RecordThreePoint} WriteLinetoDebugFile('TThreePointer.DataBaseSpeedButton28Click out'); {$EndIf}
end;

procedure TThreePointer.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,MDDef.ThreePointExtrap);
end;

procedure TThreePointer.BitBtn5Click(Sender: TObject);
begin
   ClearValues;
   UpdateColor;
   ChangeDEMNowDoing(SeekingFirstThreePoint);
end;


procedure TThreePointer.BitBtn6Click(Sender: TObject);
begin
   SetDipAndStrike;
   UpdateColor;
   ChangeDEMNowDoing(SeekingPlaneContact);
end;


procedure TThreePointer.BitBtn7Click(Sender: TObject);
begin
   ClearValues;
   ChangeDEMNowDoing(FirstBearingPoint);
end;


procedure TThreePointer.BitBtn8Click(Sender: TObject);
const
   z : float64 = 250;
begin
   ReadDefault('Thickness (m, + above, - below)',z);
   z := z / cosDeg(BroadCastPlaneData.Dip);
   aMapOwner.OverlayContourFromSecondDEM(PlanarDevDEM,z,MDDef.GeoContactColor);
   UpdateColor;
end;

procedure TThreePointer.Button1Click(Sender: TObject);
begin
   if (aMapOwner.LineTable <> 0) then aMapOwner.ThreePointProblems(aMapOwner.LineTable,true);
   if (PlanarDevDEM <> 0) then aMapOwner.OverlayContourFromSecondDEM(PlanarDevDEM,1,MDDef.GeoContactColor);
   UpdateColor;
end;

procedure TThreePointer.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\structural_geology\struct_geology_form.htm');
end;


procedure TThreePointer.RedrawSpeedButton12Click(Sender: TObject);
begin
   aMapOwner.DoFastMapRedraw;
end;


initialization
finalization
   {$IfDef RecordThreePoint} WriteLineToDebugFile('RecordThreePoint active in three_point_problem');  {$EndIf}
end.
