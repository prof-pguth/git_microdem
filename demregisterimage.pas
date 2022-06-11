unit demregisterimage;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM                }
{ PETMAR Trilobite Breeding Ranch }
{_________________________________}

{$I nevadia_defines.inc}

removed May 2020

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   // {$Define RecordSatRegisterProblems}
   // {$Define RecordSatCoords}
   // {$Define FitUTM}
{$EndIf}


interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,DB, DBGrids,
  Forms, Dialogs, Menus, Grids, ExtCtrls, StdCtrls, ComCtrls, Buttons,
  DEMDefs,DEMMapf,Petmar_types,Petmar_db,Petmar,DEMEROSM,DEMDatum;

type
  TImageRegForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    Panel1: TPanel;
    Panel2: TPanel;
    BitBtn11: TBitBtn;
    HelpBtn: TBitBtn;
    BitBtn12: TBitBtn;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    PopupMenu1: TPopupMenu;
    Deletecontrolpoint1: TMenuItem;
    Movecontrolpointtoend1: TMenuItem;
    BitBtn13: TBitBtn;
    PopupMenu2: TPopupMenu;
    UTMworldfile1: TMenuItem;
    Geographicworldfile1: TMenuItem;
    Rotationregistrationfile1: TMenuItem;
    CheckBox1: TCheckBox;
    BitBtn9: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn14: TBitBtn;
    TINmapping1: TMenuItem;
    BitBtn16: TBitBtn;
    Edit1: TEdit;
    N1: TMenuItem;
    ExportmaptoGoogleEarth1: TMenuItem;
    Reprojectmap1: TMenuItem;
    BitBtn17: TBitBtn;
    Findoptimalrotationregistration1: TMenuItem;
    l1: TMenuItem;
    N2: TMenuItem;
    BitBtn18: TBitBtn;
    XYRotation1: TMenuItem;
    BitBtn19: TBitBtn;
    BitBtn20: TBitBtn;
    ShiftprimeMeridian1: TMenuItem;
    Ferro1: TMenuItem;
    Ferro2: TMenuItem;
    CheckBox2: TCheckBox;
    Insertlatlongfromclipboard1: TMenuItem;
    Insertimagecoordinatesfromclipboard1: TMenuItem;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    BitBtn7: TBitBtn;
    Simpleregistrations1: TMenuItem;
    Reprojectmap2: TMenuItem;
    KMLandGeotiff1: TMenuItem;
    GDALtranslate1: TMenuItem;
    GDALtranslastetoUTM1: TMenuItem;
    procedure BitBtn18Click(Sender: TObject);
    procedure l1Click(Sender: TObject);
    procedure Findoptimalrotationregistration1Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure Reprojectmap1Click(Sender: TObject);
    procedure ExportmaptoGoogleEarth1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure DBGrid1CellClick(Column: TColumn);
    procedure Deletecontrolpoint1Click(Sender: TObject);
    procedure Movecontrolpointtoend1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure UTMworldfile1Click(Sender: TObject);
    procedure Geographicworldfile1Click(Sender: TObject);
    procedure Rotationregistrationfile1Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure TINmapping1Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure XYRotation1Click(Sender: TObject);
    procedure BitBtn19Click(Sender: TObject);
    procedure BitBtn20Click(Sender: TObject);
    procedure Ferro2Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Insertlatlongfromclipboard1Click(Sender: TObject);
    procedure Insertimagecoordinatesfromclipboard1Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure KMLandGeotiff1Click(Sender: TObject);
    procedure GDALtranslate1Click(Sender: TObject);
    procedure GDALtranslastetoUTM1Click(Sender: TObject);
  private
    { Private declarations }
     procedure DefineDatumHere;
     procedure FitLatLong(var Lata,Latb,Longa,Longb : float64);
     procedure FitUTM(var xutma,xutmb,yutma,yutmb : float64);
     procedure DoXYRegistriation(Pt1 : integer = 1; Pt2 : integer = 2; Pt3 : integer = 3);
     procedure DeleteIncompleteLines;
     procedure LatLongGraphs;
     procedure PostUTMFromLatLong(Lat, Long: float64);
     procedure AddLatLongUTM(Lat,Long : float64);
     function FindPointMissregistration(var Distance,Bearing : float64) : boolean;
  public
    { Public declarations }
     GazDB,PointDoing,ImageNumber : integer;
     QuadTickSize : float64;
     RegisteringMapDatum : tEllipsoidConstants;
     LatHemi : AnsiChar;
     ReferenceImage,GISNum : integer;
     QuadCoords,
     Stretching,
     SaveImageCoords : boolean;
     ImageName : PathStr;
     RegisteringMap : tMapForm;
     ImageToRegister : TSatelliteForm;
     Symbol : tFullSymbolDeclaration;
     procedure ShowRegPoints;
     procedure AddImageCoords(xsatg1,ysatg1 : float64);
     procedure AddGroundCoords(Lat,Long : float64; Name : shortstring = '');
     procedure ExtrapolatePoint(Lat,Long : float64; xim,yim : integer);
     procedure AddUTMCoords(x,y : float64);
  end;


procedure ImageRegistrationStartup(ImageForm : TSatelliteForm; SatInWindow : integer);


var
   ImageRegForm : TImageRegForm;


implementation

{$R *.DFM}


uses
   Nevadia_Main,
   PETMath, PetdbUtils,
   DEM_Tin,
   Read_DEM,
   DEMDef_routines,
   DEMEROS,DEMDataBase,
   BaseGraf,
   PetImage,
   MD_use_tools,
   GetLatLn, Make_tables, basemap, DEM_indexes;

var
   LastLat,LastLong : float64;


procedure ImageRegistrationStartup(ImageForm : TSatelliteForm; SatInWindow : integer);
var
   fName : PathStr;
begin
   {$IfDef RecordSatRegisterProblems}  WriteLineToDebugFile('ImageRegistrationStartup in');  {$EndIf}
   ImageRegForm := DEMRegisterImage.TImageRegForm.Create(Application);
   ImageRegForm.Caption := 'Registering ' + ExtractFileName(SatImage[SatInWindow].IndexFileName);
   ImageRegForm.ImageName := SatImage[SatInWindow].IndexFileName;
   ImageRegForm.ImageNumber := SatInWindow;
   ImageRegForm.Visible := true;
   ImageRegForm.ImageToRegister := ImageForm;
   ImageRegForm.CheckBox1.Checked := MDDef.LabelRegisterPoints;

   fName := ChangeFileExt(ImageRegForm.ImageName,DefaultDBExt);
   if not FileExists(fName) then begin
      {$IfDef RecordSatRegisterProblems}  WriteLineToDebugFile('DBF did not exist');  {$EndIf}
      Make_tables.CreateImageRegistrationTable(fName);
   end;

   OpenNumberedGISDataBase(ImageRegForm.GISNum,fName);

   ImageRegForm.BitBtn16.Visible := (SatImage[SatInWindow].RegVars.Registration <> RegNone) or (GISdb[ImageRegForm.GISNum].MyData.RecordCount >= 3);
   ImageRegForm.BitBtn18.Enabled := SatImage[SatInWindow].SatTINRegistration <> Nil;
   GISdb[ImageRegForm.GISNum].MyData.AssignEmpSource(ImageRegForm.DataSource1);
   ImageRegForm.ShowRegPoints;
   {$IfDef RecordSatRegisterProblems} WriteLineToDebugFile('ImageRegistrationStartup out, NumPts=' + IntToStr(GISDB[ImageRegForm.GISNum].MyData.RecordCount));  {$EndIf}
end;


procedure TImageRegForm.AddImageCoords(xsatg1,ysatg1 : float64);
var
   lat,long,BoxSize : float64;
   NS,ES : boolean;
   TStr : shortstring;
begin
   {$IfDef RecordSatCoords}  WriteLineToDebugFile('TImageRegForm.AddImageCoords   x=' + IntToStr(round(xSatg1)) + '   y=' + IntToStr(round(ySatg1)));  {$EndIf}
   if Stretching then begin
      GISdb[GISNum].MyData.Insert;
      GISdb[GISNum].MyData.SetFieldByNameAsInteger('X_IMAGE',round(xSatg1));
      GISdb[GISNum].MyData.SetFieldByNameAsInteger('Y_IMAGE',round(ySatg1));
      NS := ySatG1 < SatImage[ImageNumber].NumSatRow div 2;
      ES := xSatG1 > SatImage[ImageNumber].NumSatCol div 2;
      if NS then begin
         if ES then begin
            TStr := 'NE corner box';
            ImageToRegister.ScrollBox1.HorzScrollBar.Position := ImageToRegister.Image1.Width;
            ImageToRegister.ScrollBox1.VertScrollBar.Position := ImageToRegister.Image1.Height;
         end
         else begin
            TStr := 'NW corner box';
            ImageToRegister.ScrollBox1.HorzScrollBar.Position := ImageToRegister.Image1.Width;
            ImageToRegister.ScrollBox1.VertScrollBar.Position := 0;
         end;
      end
      else begin
         if ES then begin
            TStr := 'SE corner box';
            ImageToRegister.ScrollBox1.HorzScrollBar.Position := 0;
            ImageToRegister.ScrollBox1.VertScrollBar.Position := ImageToRegister.Image1.Height;
         end
         else begin
            TStr := 'SW corner box';
         end;
      end;
      GISdb[GISNum].MyData.SetFieldByNameAsString('POINT',TStr);
      GISdb[GISNum].MyData.Post;
   end
   else begin;
      GISdb[GISNum].MyData.Edit;
      GISdb[GISNum].MyData.SetFieldByNameAsInteger('X_IMAGE',round(xSatg1));
      GISdb[GISNum].MyData.SetFieldByNameAsInteger('Y_IMAGE',round(ySatg1));
      if QuadCoords then GISdb[GISNum].MyData.SetFieldByNameAsString('POINT','Graticule intersection');
      GISdb[GISNum].MyData.Post;
      if QuadCoords then begin
         SatImage[ImageNumber].SatGridToLatLongDegree(xSatg1,ysatg1,Lat,Long);
         QuadTickNearestHere(Lat,Long,QuadTickSize);
         AddGroundCoords(Lat,Long);
         BitBtn3Click(Nil);
      end;
      if (GazDB <> 0) and (GISdb[GazDB] <> Nil) then begin
         CheckEditString(Edit1.Text,BoxSize);
         SatImage[ImageNumber].SatGridToLatLongDegree(xSatg1,ysatg1,Lat,Long);
         GISdb[GazDB].QueryGeoBox(Lat+BoxSize,Long-BoxSize,Lat-BoxSize,Long+BoxSize,true);
      end;
      if MDDef.RapidCycle then BitBtn5Click(Nil);
   end;
end;


procedure TImageRegForm.AddLatLongUTM(Lat,Long : float64);
begin
    GISdb[GISNum].MyData.SetFieldByNameAsFloat('LAT',Lat);
    GISdb[GISNum].MyData.SetFieldByNameAsFloat('LONG',Long);
    PostUTMFromLatLong(Lat,Long);
end;


procedure TImageRegForm.AddGroundCoords(Lat,Long : float64; Name : shortstring = '');
begin
    GISdb[GISNum].MyData.Last;
    GISdb[GISNum].MyData.Edit;
    GISdb[GISNum].MyData.SetFieldByNameAsString('POINT',Name);
    AddLatLongUTM(Lat,Long);
    wmdem.StatusBar1.Panels[3].Text := 'Last point: ' + LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod);
    GISdb[GISNum].MyData.Post;
end;

procedure TImageRegForm.AddUTMCoords(x,y : float64);
begin
    GISdb[GISNum].MyData.Last;
    GISdb[GISNum].MyData.Edit;
    GISdb[GISNum].MyData.SetFieldByNameAsFloat('X_UTM',x);
    GISdb[GISNum].MyData.SetFieldByNameAsFloat('Y_UTM',y);
    GISdb[GISNum].MyData.Post;
end;


procedure TImageRegForm.ExtrapolatePoint(Lat,Long : float64; xim,yim : integer);
begin
    GISdb[GISNum].MyData.Insert;
    GISdb[GISNum].MyData.SetFieldByNameAsString('POINT','Extrapoloated');
    AddLatLongUTM(Lat,Long);
    GISdb[GISNum].MyData.SetFieldByNameAsFloat('X_IMAGE',xim);
    GISdb[GISNum].MyData.SetFieldByNameAsFloat('Y_IMAGE',yim);
    GISdb[GISNum].MyData.Post;
end;


procedure TImageRegForm.DefineDatumHere;
begin
   LatHemi := MDDef.DefaultLatHemi;
   RegisteringMapDatum := tEllipsoidConstants.Create;
   RegisteringMapDatum.DefineDatumFromUTMZone(MDdef.PreferPrimaryDatum,MDDef.DefaultUTMZone,'TImageRegForm.DefineDatumHere');
   Label1.Caption := MDdef.PreferPrimaryDatum + '   UTM Zone: ' + IntToStr(MDDef.DefaultUTMZone) + '  (' + UTMZoneExtent(RegisteringMapDatum.projUTMZone) + ')';
end;


procedure TImageRegForm.FormCreate(Sender: TObject);
begin
   Petmar.CheckFormPlacement(Self);
   RegisteringMap := Nil;
   DefineDatumHere;
   Symbol.DrawingSymbol := FilledBox;
   Symbol.Color := claRed;
   Symbol.Size := 2;
   GazDB := 0;
   ReferenceImage := 0;
   QuadTickSize := -99;
   Petmar.SymbolOnButton(BitBtn10,Symbol);
   ColorLineWidthBitBtn(BitBtn7,MDDef.DelaunayLineColor,MDDef.DelaunayLineThick);

   SaveImageCoords := MDDef.SatImageCoords;
   MDDef.SatImageCoords := true;
   CheckBox2.Checked := MDDef.RapidCycle;
   QuadCoords := false;
   Stretching := false;
end;


procedure TImageRegForm.GDALtranslastetoUTM1Click(Sender: TObject);
begin
   GDALregister(false,GISNum,ImageName,LatHemi);
end;

procedure TImageRegForm.GDALtranslate1Click(Sender: TObject);
begin
    GDALregister(true,GISnum,ImageName,LatHemi);
end;


procedure TImageRegForm.Geographicworldfile1Click(Sender: TObject);
var
   Lata,Latb,Longa,Longb : float64;
   wfName : PathStr;
begin
   {$IfDef RecordSatRegisterProblems}  WriteLineToDebugFile('TImageRegForm.Geographicworldfile1Click in');   {$EndIf}
   DeleteIncompleteLines;
   FitLatLong(Lata,Latb,Longa,Longb);
   SaveWorldFile(ImageName,LongB,-LatB,Longa,Lata,MDdef.PreferPrimaryDatum,MDdef.DefaultUTMZone,LatHemi);
   wfName := SatImage[ImageToRegister.SatInWindow].IndexFileName;
   GetDefaultWorldFile(wfName);
   with SatImage[ImageToRegister.SatInWindow] do ReadWorldFile(DigitizeDatum,wfName,RegVars);
   BitBtn16.Visible := true;
end;

procedure TImageRegForm.BitBtn1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(PickImagePointForRegistration);
end;


procedure TImageRegForm.BitBtn20Click(Sender: TObject);
begin
   BaseGraf.GraphDoing := BaseGraf.gdGraphRegister;
   WMDEM.StatusBar1.Panels[0].Text := 'Graph point for registration';
end;

procedure TImageRegForm.BitBtn2Click(Sender: TObject);
begin
   ChangeDEMNowDoing(PickUTMPointForRegistration);
end;


procedure TImageRegForm.BitBtn3Click(Sender: TObject);
begin
   GISdb[GISNum].MyData.Last;
   if (GISdb[GISNum].MyData.GetFieldByNameAsString('X_IMAGE') <> '') and (GISdb[GISNum].MyData.GetFieldByNameAsString('Y_IMAGE') <> '') and
      ((GISdb[GISNum].MyData.GetFieldByNameAsString('LAT') <> '') or (GISdb[GISNum].MyData.GetFieldByNameAsString('X_UTM') <> '')) then begin
       GISdb[GISNum].MyData.Insert;
       GISdb[GISNum].MyData.Post;
   end;
end;


procedure TImageRegForm.FitUTM(var xutma,xutmb,yutma,yutmb : float64);
var
   i,n : integer;
   x,y : array[1..100] of float32;
   siga,sigb,r : float64;
begin
   n := GISdb[GISNum].MyData.RecordCount;
   GISdb[GISNum].MyData.First;
   for i := 1 to n do begin
      x[i] :=  GISdb[GISNum].MyData.GetFieldByNameAsFloat('X_IMAGE');
      y[i] :=  GISdb[GISNum].MyData.GetFieldByNameAsFloat('X_UTM');
      GISdb[GISNum].MyData.Next;
   end;
   fit(x,y,n,xutma,xutmb,siga,sigb,r);

   GISdb[GISNum].MyData.First;
   for i := 1 to n do begin
      x[i] := GISdb[GISNum].MyData.GetFieldByNameAsFloat('Y_IMAGE');
      y[i] := GISdb[GISNum].MyData.GetFieldByNameAsFloat('Y_UTM');
      GISdb[GISNum].MyData.Next;
   end;
   fit(x,y,n,yutma,yutmb,siga,sigb,r);
end;


procedure TImageRegForm.FitLatLong(var Lata,Latb,Longa,Longb : float64);
var
   PtsUsed : integer;
   x,y : array[1..100] of float32;
   siga,sigb,r : float64;
begin
   GISdb[GISNum].MyData.First;
   PtsUsed := 0;
   while not GISdb[GISNum].MyData.EOF  do begin
      if (GISdb[GISNum].MyData.GetFieldByNameAsString('LAT') <> '') then begin
         inc(PtsUsed);
         x[PtsUsed] := GISdb[GISNum].MyData.GetFieldByNameAsFloat('Y_IMAGE');
         y[PtsUsed] := GISdb[GISNum].MyData.GetFieldByNameAsFloat('LAT');
      end;
      GISdb[GISNum].MyData.Next;
   end;
   fit(x,y,PtsUsed,Lata,Latb,siga,sigb,r);

   PtsUsed := 0;
   GISdb[GISNum].MyData.First;
   while not GISdb[GISNum].MyData.EOF do begin
      if (GISdb[GISNum].MyData.GetFieldByNameAsString('LONG') <> '') then begin
         inc(PtsUsed);
         x[PtsUsed] := GISdb[GISNum].MyData.GetFieldByNameAsFloat('X_IMAGE');
         y[PtsUsed] := GISdb[GISNum].MyData.GetFieldByNameAsFloat('LONG');
      end;
      GISdb[GISNum].MyData.Next;
   end;
   fit(x,y,PtsUsed,Longa,Longb,siga,sigb,r);
end;


procedure TImageRegForm.CheckBox2Click(Sender: TObject);
begin
   MDDef.RapidCycle := CheckBox1.Checked;
end;

procedure TImageRegForm.CheckBox3Click(Sender: TObject);
begin
   ShowRegPoints;
end;

procedure TImageRegForm.CheckBox4Click(Sender: TObject);
begin
   ShowRegPoints;
end;


procedure TImageRegForm.Reprojectmap1Click(Sender: TObject);
begin
   ImageToRegister.Forceredraw1Click(nil);
   ImageToRegister.KMLReprojectImage(true,false);
end;


function TImageRegForm.FindPointMissregistration(var Distance,Bearing : float64) : boolean;
var
   Lat,Long,Lat2,Long2: float64;
begin
    if (GISdb[GISNum].MyData.RecordCount < 3) then begin
       MessageToContinue('Requires at least three points');
    end
    else begin
        Result := (GISdb[GISNum].MyData.GetFieldByNameAsString('X_IMAGE') <> '') and (GISdb[GISNum].MyData.GetFieldByNameAsString('Y_IMAGE')  <> '') and
                    (GISdb[GISNum].MyData.GetFieldByNameAsString('X_UTM')  <> '') and (GISdb[GISNum].MyData.GetFieldByNameAsString('Y_UTM')  <> '');
        if Result then begin
          SatImage[ImageToRegister.SatInWindow].SatGridToLatLongDegree(GISdb[GISNum].MyData.GetFieldByNameAsInteger('X_IMAGE'),
               GISdb[GISNum].MyData.GetFieldByNameAsInteger('Y_IMAGE'),Lat,Long);
          if (GISdb[GISNum].MyData.GetFieldByNameAsString('LAT') <> '') and (GISdb[GISNum].MyData.GetFieldByNameAsString('LONG') <> '') then begin
             Lat2 := GISdb[GISNum].MyData.GetFieldByNameAsFloat('LAT');
             Long2 := GISdb[GISNum].MyData.GetFieldByNameAsFloat('LONG');
             DEMDatum.VincentyCalculateDistanceBearing(Lat2,Long2,Lat,Long,Distance,Bearing);
          end
          else Result := false;
        end;
    end;
end;

procedure TImageRegForm.Rotationregistrationfile1Click(Sender: TObject);
var
   i,x,y : integer;
   xr,yr,xc,yc,
   Distance,Bearing : float64;
   Results : tStringList;
begin
   {$IfDef RecordSatRegisterProblems} WriteLineToDebugFile('TImageRegForm.Rotationregistrationfile1Click in'); {$EndIf}
   DeleteIncompleteLines;
   if (GISdb[GISNum].MyData.RecordCount >= 3) then begin
      DoXYRegistriation;

      if (GISdb[GISNum].MyData.RecordCount > 3) then begin
         Results := tStringList.Create;
         Results.Add('Registration errors');
         Results.Add('');
         Results.Add('Point    Distance (m)    Bearing');
         Results.Add('----------------------------------');
         i := 3;
         while not GISdb[GISNum].MyData.EOF do begin
            inc(i);
            if FindPointMissregistration(Distance,Bearing) then begin
                Results.Add(IntegerToString(i,4) + RealToString(Distance,14,2) + RealToString(Bearing,13,1));
            end
            else begin
                x := GISdb[GISNum].MyData.GetFieldByNameAsInteger('X_IMAGE');
                y := GISdb[GISNum].MyData.GetFieldByNameAsInteger('Y_IMAGE');
                xr := GISdb[GISNum].MyData.GetFieldByNameAsFloat('X_UTM');
                yr := GISdb[GISNum].MyData.GetFieldByNameAsFloat('Y_UTM');
                SatImage[ImageNumber].SatGridToUTM(x,y,xc,yc);
                Results.Add(IntegerToString(i,4) + RealToString(sqrt(sqr(xc-xr) + sqr(yc+yr)),12,2) + ' pixels');
            end;
            GISdb[GISNum].MyData.Next;
         end;
         Petmar.DisplayAndPurgeStringList(Results,'Registration errors');
      end;
   end
   else MessageToContinue('Register 3 points first');
end;


procedure TImageRegForm.Ferro2Click(Sender: TObject);
var
   Shift : float64;
begin
   Shift := -17.666666666666667;
   if (Sender <> Ferro2) then GetAngle('Angle to add',Shift,MDDef.GraticuleUnits);
   GISdb[GISNum].MyData.First;
   while not GISdb[GISNum].MyData.EOF do begin
      GISdb[GISNum].MyData.Edit;
      GISdb[GISNum].MyData.SetFieldByNameAsFloat('LONG', GISdb[GISNum].MyData.GetFieldByNameAsFloat('LONG') + Shift);
      GISdb[GISNum].MyData.Next;
   end;
end;

procedure TImageRegForm.Findoptimalrotationregistration1Click(Sender: TObject);
var
   GISNum2,Pt1,Pt2,Pt3,i,Num : integer;
   Pt1Name,Pt2Name,Pt3Name : shortstring;
   Distance,Bearing,Sum,Max : float64;
   Table : tMyData;
   fName : PathStr;
   GIS : TGISdataBaseModule;
begin
   DataSource1.Enabled := false;
   fName := MDTempDir +  'opt_reg' + DefaultDBExt;
   CreateOptimalRegistrationTable(fName);
   Table := tMyData.Create(fName);
   for Pt1 := 1 to (GISdb[GISNum].MyData.RecordCount - 2) do begin
      for Pt2 := succ(Pt1) to (GISdb[GISNum].MyData.RecordCount - 1) do begin
         for Pt3 := succ(Pt2) to (GISdb[GISNum].MyData.RecordCount) do begin
            DoXYRegistriation(Pt1,Pt2,Pt3);
            i := 0;
            Max := 0;
            Sum := 0;
            Num := 0;
            GISdb[GISNum].MyData.First;
            while not GISdb[GISNum].MyData.EOF do begin
                inc(i);
                if i = PT1 then Pt1Name := GISdb[GISNum].MyData.GetFieldByNameAsString('POINT')
                else if i = PT2 then Pt2Name := GISdb[GISNum].MyData.GetFieldByNameAsString('POINT')
                else if i = PT3 then Pt3Name := GISdb[GISNum].MyData.GetFieldByNameAsString('POINT')
                else if FindPointMissregistration(Distance,Bearing) then begin
                      if Distance > Max then Max := Distance;
                      Sum := Sum + Distance;
                      Inc(Num);
                end;
               GISdb[GISNum].MyData.Next;
            end;
            Table.Insert;
            Table.SetFieldByNameAsFloat('AVG_ERR',Sum/Num);
            Table.SetFieldByNameAsFloat('MAX_ERR',Max);
            Table.SetFieldByNameAsString('POINT_1',Pt1Name);
            Table.SetFieldByNameAsString('POINT_2',Pt2Name);
            Table.SetFieldByNameAsString('POINT_3',Pt3Name);
            Table.Post;
         end;
      end;
   end;
   Table.Destroy;
   DataSource1.Enabled := true;
   GIS := Nil;
   OpenNumberedGISDatabase(GISNum2,fName,true);
end;


procedure TImageRegForm.PostUTMFromLatLong(Lat,Long : float64);
var
   xutm,yutm : float64;
begin
   if GISdb[GISNum].MyData.FieldExists('X_UTM') then begin
      RegisteringMapDatum.LatLongDegreetoUTM(Lat,Long,xutm,yutm);
      GISdb[GISNum].MyData.SetFieldByNameAsFloat('X_UTM',xutm);
      GISdb[GISNum].MyData.SetFieldByNameAsFloat('Y_UTM',yutm);
   end;
end;

procedure TImageRegForm.BitBtn6Click(Sender: TObject);
var
   Lat,Long : float64;
begin
   GetMapParameters(LatHemi,MDdef.DefaultUTMZone,MDdef.PreferPrimaryDatum);
   DefineDatumHere;
   GISdb[GISNum].MyData.First;
   while not GISdb[GISNum].MyData.EOF do begin
      if GISdb[GISNum].ValidLatLongFromTable(Lat,Long) then begin
         GISdb[GISNum].MyData.Edit;
         PostUTMFromLatLong(Lat,Long);
      end;
      GISdb[GISNum].MyData.Next;
   end;
end;


procedure TImageRegForm.BitBtn5Click(Sender: TObject);
var
   Lat,Long : float64;
begin
   if MDDef.RapidCycle then begin
      GetLatLn.GetLatLongDefault(RegisteringMapDatum,'Registation Point',LastLat,LastLong);
      Lat := LastLat;
      Long := LastLong;
   end
   else GetLatLn.GetLatLong(RegisteringMapDatum,'Registation Point',Lat,Long);
   GISdb[GISNum].MyData.Last;
   GISdb[GISNum].MyData.Edit;
   if RadioGroup1.ItemIndex in [0,3] then GISdb[GISNum].MyData.SetFieldByNameAsFloat('LAT',Lat);
   if RadioGroup1.ItemIndex in [0,4] then GISdb[GISNum].MyData.SetFieldByNameAsFloat('LONG',Long);
   if RadioGroup1.ItemIndex in [0,1,2] then PostUTMFromLatLong(Lat,Long);
   GISdb[GISNum].MyData.Post;
   BitBtn16.Visible := (SatImage[ImageNumber].RegVars.Registration <> RegNone) or (GISdb[ImageRegForm.GISNum].MyData.RecordCount >= 3);
   if MDDef.RapidCycle then BitBtn3Click(Sender);
end;


procedure TImageRegForm.ShowRegPoints;
var
   x,y,i : integer;
   TStr : ShortString;
begin
   {$IfDef RecordSatRegisterProblems}  WriteLineToDebugFile('TImageRegForm.ShowRegPoints in');  {$EndIf}
   if GISNum = 0 then exit;
   
   ImageToRegister.Forceredraw1Click(nil);

   LoadMyFontIntoWindowsFont(GISdb[GISNum].dbOpts.GisLabelFont1,ImageToRegister.Image1.Canvas.Font);
   GISdb[GISNum].MyData.First;
   i := 1;
   While not GISdb[GISNum].MyData.EOF do begin
      if (GISdb[GISNum].MyData.GetFieldByNameAsString('X_IMAGE') <> '') and (GISdb[GISNum].MyData.GetFieldByNameAsString('Y_IMAGE') <> '') then begin
         x := ImageToRegister.XSatToScreen(GISdb[GISNum].MyData.GetFieldByNameAsInteger('X_IMAGE'));
         y := ImageToRegister.YSatToScreen(GISdb[GISNum].MyData.GetFieldByNameAsInteger('Y_IMAGE'));
         if CheckBox4.Checked then SatImage[ImageToRegister.SatInWindow].SatTINRegistration.ShowTriangulationOnSatForm(ImageToRegister);

         if CheckBox3.Checked then ScreenSymbol(ImageToRegister.Image1.Canvas,x,y,Symbol);
         if MDDef.LabelRegisterPoints then begin
            TStr := GISdb[GISNum].MyData.GetFieldByNameAsString('POINT');
            if GISdb[GISNum].MyData.GetFieldByNameAsString('LAT') <> '' then
               TStr := TStr + ' ' + LatLongDegreeToString(GISdb[GISNum].MyData.GetFieldByNameAsFloat('LAT'),
                     GISdb[GISNum].MyData.GetFieldByNameAsFloat('LONG'),ShortDegrees);
            ImageToRegister.Image1.Canvas.TextOut(x+5,y+5,TStr);
         end;

        if (ReferenceImage <> 0) then with SatImage[ReferenceImage] do begin
           SelectionMap.MapDraw.GridToScreen(GISdb[GISNum].MyData.GetFieldByNameAsInteger('X_REF_IM'),GISdb[GISNum].MyData.GetFieldByNameAsInteger('Y_REF_IM'),x,y);
           if i in [1..4] then begin
              ScreenSymbol(SelectionMap.Image1.Canvas,x,y,Symbol.DrawingSymbol,2*Symbol.Size,Symbol.Color);
           end
           else begin
              ScreenSymbol(SelectionMap.Image1.Canvas,x,y,Symbol);
           end;
        end;
      end;
      GISdb[GISNum].MyData.Next;
      inc(i);
   end;
   ShowDefaultCursor;
   {$IfDef RecordSatRegisterProblems} WriteLineToDebugFile('TImageRegForm.ShowRegPoints out');  {$EndIf}
end;

procedure TImageRegForm.UTMworldfile1Click(Sender: TObject);
var
   xutma,xutmb,yutma,yutmb : float64;
   wfName : PathStr;
begin
   {$IfDef RecordSatRegisterProblems} WriteLineToDebugFile('TImageRegForm.UTMworldfile1Click in');  {$EndIf}
   DeleteIncompleteLines;
   FitUTM(xutma,xutmb,yutma,yutmb);
   SaveWorldFile(ImageName,xutmb,-yutmb,xutma,yutma,MDdef.PreferPrimaryDatum,MDdef.DefaultUTMZone,LatHemi);
   wfName := SatImage[ImageToRegister.SatInWindow].IndexFileName;
   GetDefaultWorldFile(wfName);
   with SatImage[ImageToRegister.SatInWindow] do ReadWorldFile(DigitizeDatum,wfName,RegVars);
   BitBtn16.Visible := true;
end;

procedure TImageRegForm.XYRotation1Click(Sender: TObject);
begin
   Rotationregistrationfile1Click(Sender);
end;

procedure TImageRegForm.BitBtn9Click(Sender: TObject);
begin
   EditMyFont(GISdb[GISNum].dbOpts.GisLabelFont1);
   ShowRegPoints;
end;


procedure TImageRegForm.CheckBox1Click(Sender: TObject);
begin
   MDDef.LabelRegisterPoints := CheckBox1.Checked;
   ShowRegPoints;
end;

procedure TImageRegForm.BitBtn10Click(Sender: TObject);
begin
   Petmar.PickSymbol(BitBtn10,Symbol);
   ShowRegPoints;
end;


procedure TImageRegForm.DoXYRegistriation(Pt1 : integer = 1; Pt2 : integer = 2; Pt3 : integer = 3);
var
   LogReg : tLogRegistration;

   procedure GetPoints(i,Pt : integer);
   var
      j : integer;
   begin
       GISdb[GISNum].MyData.First;
       while not GISdb[GISNum].MyData.eof do begin
          if GISdb[GISNum].MyData.GetFieldByNameAsInteger(RecNoFName) = Pt then begin
             for j := 1 to pred(Pt) do Next;
             LogReg.MapX[i] := GISdb[GISNum].MyData.GetFieldByNameAsInteger('X_IMAGE');
             LogReg.MapY[i] := GISdb[GISNum].MyData.GetFieldByNameAsInteger('Y_IMAGE');
             LogReg.CornerX[i] := round(GISdb[GISNum].MyData.GetFieldByNameAsFloat('X_UTM'));
             LogReg.CornerY[i] := round(GISdb[GISNum].MyData.GetFieldByNameAsFloat('Y_UTM'));
             exit;
          end;
          GISdb[GISNum].MyData.Next;
       end;
   end;

begin
    if GISdb[GISNum].MyData.GetFieldByNameAsString('LONG') = '' then RegisteringMapDatum.projUTMZone := 0
    else RegisteringMapDatum.projUTMZone := GetUTMZone(GISdb[GISNum].MyData.GetFieldByNameAsFloat('LONG'));
    GetPoints(1,Pt1);
    GetPoints(2,Pt2);
    GetPoints(3,Pt3);
    DEMEros.WriteXYImageRegistrationFile(ChangeFileExt(ImageName,'.xy'),MDDef.DefaultLatHemi,RegisteringMapDatum, LogReg);
    wmDEM.SetPanelText(0,'');
    SatImage[ImageToRegister.SatInWindow].TryToLoadRegistration(ExtractFilePath(ImageName));
end;

procedure TImageRegForm.ExportmaptoGoogleEarth1Click(Sender: TObject);
begin
   ImageToRegister.Forceredraw1Click(nil);
   ImageToRegister.KMLReprojectImage(true,false);
end;

procedure TImageRegForm.BitBtn11Click(Sender: TObject);
begin
   Rotationregistrationfile1.Visible :=  GISdb[GISNum].MyData.RecordCount >= 3;
   Reprojectmap1.Visible := (SatImage[ImageRegForm.ImageNumber].SatTINRegistration <> Nil);
   ExportmaptoGoogleEarth1.Visible := (SatImage[ImageRegForm.ImageNumber].SatTINRegistration <> Nil);
   PopupMenu2.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure TImageRegForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\image_reg.htm');
end;


procedure TImageRegForm.Insertimagecoordinatesfromclipboard1Click(Sender: TObject);
begin
   if ClipBoard_Image_Coords then begin
      GISdb[GISNum].MyData.Edit;
      GISdb[GISNum].MyData.SetFieldByNameAsInteger('X_IMAGE',Clipboard_ImageX);
      GISdb[GISNum].MyData.SetFieldByNameAsInteger('Y_IMAGE',Clipboard_ImageY);
      GISdb[GISNum].MyData.Post;
   end;
   ClipBoard_Image_Coords := false;
end;


procedure TImageRegForm.Insertlatlongfromclipboard1Click(Sender: TObject);
begin
   if ClipBoard_Coords then begin
      GISdb[GISNum].MyData.Edit;
      AddLatLongUTM(Clipboard_Lat,ClipBoard_Long);
      if (ClipBoard_Name <> '')then GISdb[GISNum].MyData.SetFieldByNameAsString('POINT',ClipBoard_Name);
      GISdb[GISNum].MyData.Post;
      ClipBoard_Coords := false;
   end;
end;


procedure TImageRegForm.KMLandGeotiff1Click(Sender: TObject);
begin
   ImageToRegister.Forceredraw1Click(nil);
   ImageToRegister.KMLReprojectImage(true,true);
end;


procedure TImageRegForm.l1Click(Sender: TObject);
begin
   LatLongGraphs;
end;


procedure TImageRegForm.LatLongGraphs;

   procedure MakeGraph(Xfield,YField : ShortString; XName,YName : shortstring);
   var
      ThisGraph : tThisBaseGraph;
      rfile     :  file;
      v         : array[1..2] of float64;
   begin
      ThisGraph := TThisBaseGraph.Create(Application);
      ThisGraph.OpenPointFile(rfile,ThisGraph.Symbol);
      GISdb[GISNum].MyData.First;
      while not GISdb[GISNum].MyData.EOF do begin
         v[1] := GISdb[GISNum].MyData.GetFieldByNameAsFloat(XField);
         v[2] := GISdb[GISNum].MyData.GetFieldByNameAsFloat(YField);
         BlockWrite(rfile,v,1);
         GISdb[GISNum].MyData.Next;
      end;
      closeFile(rfile);
      with ThisGraph,GraphDraw do begin
         HorizLabel := XName;
         VertLabel := YName;
         GraphDraw.Symbol[1].Size := 3;
         Caption := 'Registration points versus image coordinates';
         ThisGraph.SetUpGraphForm;
         ThisGraph.AutoScaleAndRedrawDiagram;
         ThisGraph.Left := 0;
         ThisGraph.Top := 0;
      end;
   end;


begin
   MakeGraph('LAT','Y_IMAGE','LAT','Y_IMAGE');
   MakeGraph('LONG','X_IMAGE','LONG','X_IMAGE');
end;


procedure TImageRegForm.TINmapping1Click(Sender: TObject);
{$IfDef ExTin}
begin
{$Else}
var
   fName : PathStr;
   Dir : DirStr;
   bName : NameStr;
   Ext : ExtStr;
begin
   {$IfDef RecordSatRegisterProblems}  WriteLineToDebugFile('TImageRegForm.TINmapping1Click in'); {$EndIf}
   if (SatImage[ImageRegForm.ImageNumber].SatTINRegistration <> Nil) then begin
      SatImage[ImageRegForm.ImageNumber].SatTINRegistration.Destroy;
      SatImage[ImageRegForm.ImageNumber].SatTINRegistration := Nil;
   end;
   DeleteIncompleteLines;
   LatLongGraphs;
   fSplit(ImageRegForm.ImageName,Dir,bName,Ext);
   fName := Dir + 'tin-' + bName + DefaultDBExt;
   DeleteFileIfExists(fName);
   fName := ChangeFileExt(ImageRegForm.ImageName, DefaultDBExt);
   SatImage[ImageRegForm.ImageNumber].SatTINRegistration := tTIN.Create(nil,fName,true,true);
   SatImage[ImageToRegister.SatInWindow].SatTINRegistration.ShowTriangulationOnSatForm(ImageToRegister);
   ImageRegForm.BitBtn18.Enabled := true;
{$EndIf}
end;



procedure TImageRegForm.BitBtn12Click(Sender: TObject);
begin
   if AnswerIsYes('Confirm clear all registration points') then begin
       GISdb[GISNum].MyData.First;
       while not GISdb[GISNum].MyData.EOF do begin
          GISdb[GISNum].MyData.Edit;
          GISdb[GISNum].MyData.Delete;
       end;
   end;
end;


procedure TImageRegForm.BitBtn13Click(Sender: TObject);
var
   fName : PathStr;
   KML : tStringList;
   I,maxLength   : Integer;
   Adding : boolean;
   Str : AnsiString;
   bName : ShortString;
   Lat,Long : float64;
begin
   fName := '';
   if Petmar.GetFileFromDirectory('KML file','*.KML',fName) then begin
      KML := tStringList.Create;
      KML.LoadFromFile(fName);
      Adding := false;
      MaxLength := GISdb[GISNum].MyData.GetFieldLength('POINT');
      for I := 0 to pred(KML.count) do begin
         Str := ptTrim(KML.Strings[i]);
         if UpperCase(str) = '<PLACEMARK>' then begin
            Adding := true;
         end;
         if Adding then begin
            if Uppercase(Copy(Str,1,6)) = '<NAME>' then begin
               Delete(Str,1,6);
               bName := Petmar_Types.BeforeSpecifiedCharacterANSI(Str,'<');
               GISdb[GISNum].MyData.Insert;
               GISdb[GISNum].MyData.SetFieldByNameAsString('POINT',bName);
            end;
            if Uppercase(Copy(Str,1,13)) = '<COORDINATES>' then begin
               Delete(Str,1,13);
               Long := StrToFloat(Petmar_Types.BeforeSpecifiedCharacterANSI(Str,',',true,true));
               Lat := StrToFloat(Petmar_Types.BeforeSpecifiedCharacterANSI(Str,',',true,true));
               if Adding then begin
                  AddLatLongUTM(Lat,Long);
                  GISdb[GISNum].MyData.Post;
                  Adding := false;
               end;
            end;
         end;
         if UpperCase(Str) = '</PLACEMARK>' then begin
            Adding := false;
         end;
      end;
      KML.Free;
   end;
end;


procedure TImageRegForm.BitBtn14Click(Sender: TObject);
var
   Lat,Long  : float64;
   TStr : ShortString;
begin
   QuadCoords := false;
   if (GazDB = 0) or (GISdb[GazDB] = Nil) then OpenGazetteer(GazDB,'',nil)
   else begin
      if GISdb[GazDB].ValidLatLongFromTable(Lat,Long) then begin
         GISdb[GISNum].MyData.Last;
         GISdb[GISNum].MyData.Edit;
         if GISdb[GISNum].MyData.GetFieldByNameAsString('POINT') = '' then begin
            if GISdb[GazDB].MyData.FieldExists('NAME') then TStr := 'NAME'
            else if GISdb[GazDB].MyData.FieldExists('FEATURE') then TStr := 'FEATURE'
            else TStr := '';
            if (TStr <> '') then GISdb[GISNum].MyData.SetFieldByNameAsString('POINT',GISdb[GazDB].MyData.GetFieldByNameAsString(TStr));
         end;
         GISdb[GISNum].MyData.SetFieldByNameAsFloat('LAT',Lat);
         GISdb[GISNum].MyData.SetFieldByNameAsFloat('LONG',Long);
         if RadioGroup1.ItemIndex in [0,1,2] then PostUTMFromLatLong(Lat,Long);
         GISdb[GISNum].MyData.Post;
      end;
   end;
end;



procedure TImageRegForm.BitBtn16Click(Sender: TObject);
var
   i : integer;
   Lat,Long,Distance,Bearing : float64;
   Lats,Longs : array[1..4] of float64;
   xs,ys : array[1..4] of float64;
   Results : tStringList;
   Problem : boolean;
begin   //Quad map setup
   if (SatImage[ImageNumber].RegVars.Registration <> RegNone) then begin
      SatImage[ImageNumber].DeleteRegistrationFiles;
   end;
   MDDef.RapidCycle := false;
   CheckBox2.Checked := false;
   if (GISdb[GISNum].MyData.RecordCount >= 4) then begin
      with SatImage[ImageNumber] do begin
         Geographicworldfile1Click(Nil);
         GISdb[GISNum].MyData.First;
         for I := 1 to 4 do begin
            Lats[i] := GISdb[GISNum].MyData.GetFieldByNameAsFloat('LAT');
            Longs[i] := GISdb[GISNum].MyData.GetFieldByNameAsFloat('LONG');
            xs[i] := GISdb[GISNum].MyData.GetFieldByNameAsFloat('X_IMAGE');
            ys[i] := GISdb[GISNum].MyData.GetFieldByNameAsFloat('Y_IMAGE');
            GISdb[GISNum].MyData.Next;
         end;
         HeapSort(4,Lats);
         HeapSort(4,Longs);
         Results := tStringList.Create;
         if abs(Lats[1]-Lats[2]) > 0.01 then Results.Add('Two points do not lie on the top parallel');
         if abs(Lats[3]-Lats[4]) > 0.01 then Results.Add('Two points do not lie on the bottom parallel');
         if abs(Longs[1]-Longs[2]) > 0.01 then Results.Add('Two points do not lie on the left meridian');
         if abs(Longs[3]-Longs[4]) > 0.01 then Results.Add('Two points do not lie on the right meridian');
         Problem := (Results.Count > 0);

         Results.Add('NW Corner: ' + LatLongDegreeToString(Lats[4],Longs[1],ShortDegrees));
         Results.Add('SE Corner: ' + LatLongDegreeToString(Lats[1],Longs[4],ShortDegrees));

         DEMDatum.VincentyCalculateDistanceBearing(Lats[2],Longs[1],Lats[2],Longs[2],Distance,Bearing);
         Results.Add('Image x pixel size (m): ' + RealToString( Distance/(xs[2]-xs[1]),-12,-2));

         DEMDatum.VincentyCalculateDistanceBearing(Lats[2],Longs[1],Lats[1],Longs[1],Distance,Bearing);
         Results.Add('Image y pixel size (m): ' + RealToString( Distance/(ys[2]-ys[1]),-12,2));
         if Problem then MessageToContinue('Corners are probably wrong; check results carefully');

         Petmar.DisplayAndPurgeStringList(Results,'Quadrangle information');
      end;

      BitBtn7Click(Sender);
      if (QuadTickSize > 0) then QuadTickSize := QuadTickSize * 60;

      ReadDefault('Tick interval (Minutes)',QuadTickSize);
      QuadTickSize := QuadTickSize / 60;

      if (GISdb[GISNum].MyData.RecordCount > 4) and AnswerIsYes('Recompute interior points') then begin
         GISdb[GISNum].MyData.First;
         for I := 1 to 4 do GISdb[GISNum].MyData.Next;
         while not GISdb[GISNum].MyData.EOF do begin
            GISdb[GISNum].MyData.Edit;
            Lat := Lats[2] -  (GISdb[GISNum].MyData.GetFieldByNameAsInteger('Y_IMAGE') - ys[1]) / (ys[2] - ys[1]) * (Lats[2] - Lats[1]);
            Lat := QuadTickSize * round(Lat / QuadTickSize);
            Long := Longs[1] +  (GISdb[GISNum].MyData.GetFieldByNameAsInteger('X_IMAGE') - xs[1]) / (xs[2] - xs[1]) * (Longs[2] - Longs[1]);
            Long := QuadTickSize * round(Long / QuadTickSize);
            {$IfDef RecordSatRegisterProblems}
            WriteLineToDebugFile('ImageCoords=' + IntToStr(GISdb[GISNum].MyData.GetFieldByNameAsInteger('X_IMAGE')) + 'x' + IntToStr(GISdb[GISNum].MyData.GetFieldByNameAsInteger('Y_IMAGE')));
            WriteLineToDebugFile(LatLongDegreeToString(Lat,Long));
            {$EndIf}
            AddLatLongUTM(Lat,Long);
            GISdb[GISNum].MyData.Next;
         end;
      end;
      QuadCoords := true;
      ChangeDEMNowDoing(PickImagePointForRegistration);
   end;
end;


procedure TImageRegForm.BitBtn17Click(Sender: TObject);
begin
   ChangeDEMNowDoing(PickImagePointForRegistration);
   Stretching := true;
   ImageToRegister.ScrollBox1.HorzScrollBar.Position := 0;
   ImageToRegister.ScrollBox1.VertScrollBar.Position := 0;
end;


procedure TImageRegForm.BitBtn18Click(Sender: TObject);
begin
   {$IfDef ExTin}
   {$Else}
      SatImage[ImageToRegister.SatInWindow].SatTINRegistration.ShowTriangulationOnSatForm(ImageToRegister);
      EROSDoingWhat := PickTINTriangle;
      wmdem.StatusBar1.Panels[0].Text := 'Pick triangle for extrapolation';
   {$EndIf}
end;

procedure TImageRegForm.BitBtn19Click(Sender: TObject);
var
   x,y : float64;
begin
   x := 0;
   y := 0;
   ReadDefault('x coord',x);
   ReadDefault('y coord',y);

   GISdb[GISNum].MyData.Last;
   GISdb[GISNum].MyData.Edit;
   GISdb[GISNum].MyData.SetFieldByNameAsFloat('X_UTM',x);
   GISdb[GISNum].MyData.SetFieldByNameAsFloat('Y_UTM',y);
   GISdb[GISNum].MyData.Post;
   BitBtn16.Visible := (SatImage[ImageNumber].RegVars.Registration <> RegNone) or (GISdb[ImageRegForm.GISNum].MyData.RecordCount >= 3);
end;


procedure TImageRegForm.DBGrid1CellClick(Column: TColumn);
begin
   PopupMenu1.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure TImageRegForm.Deletecontrolpoint1Click(Sender: TObject);
begin
   GISdb[GISNum].MyData.Edit;
   GISdb[GISNum].MyData.Delete;
   ImageToRegister.Forceredraw1Click(nil);
   ShowRegPoints;
end;


procedure TImageRegForm.DeleteIncompleteLines;
begin
   GISdb[GISNum].MyData.First;
   While not GISdb[GISNum].MyData.EOF do begin
      if (GISdb[GISNum].MyData.GetFieldByNameAsString('X_IMAGE') = '') or ((GISdb[GISNum].MyData.GetFieldByNameAsString('X_UTM') = '') and (GISdb[GISNum].MyData.GetFieldByNameAsString('LONG') = '')) then begin
         GISdb[GISNum].MyData.Edit;
         GISdb[GISNum].MyData.Delete;
      end
      else GISdb[GISNum].MyData.Next;
   end;
end;

procedure TImageRegForm.Movecontrolpointtoend1Click(Sender: TObject);
begin
   GISdb[GISNum].MyData.CopyRecordToEndOfTable(true);
end;


procedure TImageRegForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   CloseAndNilNumberedDB(GISNum);
   MDDef.SatImageCoords := SaveImageCoords;
   ChangeDEMNowDoing(JustWandering);
   Self := Nil;
end;


procedure TImageRegForm.BitBtn7Click(Sender: TObject);
begin
    Petmar.PickLineSizeAndColor('Delaunay triangles',BitBtn7,MDDef.DelaunayLineColor,MDDef.DelaunayLineThick);
    ShowRegPoints;
end;


initialization
   {$IfDef MessageStartupProblems}  MessageToContinue('demregisterimage'); {$EndIf}

   LastLat := 0;
   LastLong := 0;
finalization
   {$IfDef RecordSatRegisterProblems}  WriteLineToDebugFile('RecordSatRegisterProblems active in demregisterimage'); {$EndIf}
   {$IfDef RecordSatCoords}  WriteLineToDebugFile('RecordSatCoords active in demregisterimage'); {$EndIf}
   {$IfDef FitUTM}  WriteLineToDebugFile('FitUTM active in demregisterimage'); {$EndIf}
end.



