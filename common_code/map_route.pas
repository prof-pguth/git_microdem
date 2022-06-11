unit map_route;


{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
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

  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  System.Math,
  Dialogs, StdCtrls, Buttons,
  Petmar_types,DEMMapf, ExtCtrls;

type
  TMapRtForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Edit1: TEdit;
    BitBtn5: TBitBtn;
    Label5: TLabel;
    Edit2: TEdit;
    RadioGroup1: TRadioGroup;
    CheckBox1: TCheckBox;
    Edit3: TEdit;
    Edit4: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    BitBtn6: TBitBtn;
    HelpBtn: TBitBtn;
    Memo1: TMemo;
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
     procedure UpdateForm;
  public
    { Public declarations }
    StartLat,EndLat,StartLong,EndLong,Distance,Bearing,PtSpacing,dz : float64;
    MapOwner : tMapForm;
    DoTsunami : boolean;
    NPts : integer;
  end;


procedure OverlayMapRoutes(inMapOwner : tMapForm; inDoTsunami : boolean);


implementation

{$R *.dfm}


uses
  Petmar,Petmath,DEMDefs,BaseMap, DEMCoord,Make_tables, PetDBUtils,DEMLOSw,
  getlatln;


procedure OverlayMapRoutes(inMapOwner : tMapForm; inDoTsunami : boolean);
var
  MapRtForm : TMapRtForm;
begin
    MapRtForm := TMapRtForm.Create(Application);
    MapRtForm.DoTsunami := inDoTsunami;
    if inDoTsunami then begin
       MapRtForm.Caption := 'Tsunami ' + MapRtForm.Caption;
    end
    else begin
       MapRtForm.ClientWidth := 330;
    end;
    MapRtForm.Show;
    MapRtForm.MapOwner := inMapOwner;
    MapRtForm.CheckBox1.Enabled := (inMapOwner.MapDraw.DEMonMap <> 0);
end;


procedure TMapRtForm.BitBtn1Click(Sender: TObject);
begin
   GetLatLn.GetLatLongDefault(MapOwner.MapDraw.PrimMapProj,'Start of rhumb line',StartLat,StartLong);
   UpdateForm;
end;


procedure TMapRtForm.BitBtn2Click(Sender: TObject);
begin
   GetLatLn.GetLatLongDefault(MapOwner.MapDraw.PrimMapProj,'End of rhumb line',EndLat,EndLong);
   UpdateForm;
end;


procedure TMapRtForm.BitBtn3Click(Sender: TObject);
begin
   if ClipBoard_Coords then begin
      StartLat := Clipboard_Lat;
      StartLong := ClipBoard_Long;
      UpdateForm;
   end;
end;


procedure TMapRtForm.BitBtn4Click(Sender: TObject);
begin
   if ClipBoard_Coords then begin
      EndLat := Clipboard_Lat;
      EndLong := ClipBoard_Long;
      UpdateForm;
   end;
end;


procedure TMapRtForm.BitBtn5Click(Sender: TObject);
var
   Lat2,Long2,CurDist,AvgSpeed : float64;
   fName : PathStr;
   Table : tMyData;


   procedure InsertPoint(Lat,Long,dz : float64);
   var
      z : float32;
   begin
       Table.Insert;
       Table.SetFieldByNameAsFloat('LAT',lat);
       Petmath.LongitudeAngleInRange(Long);

       Table.SetFieldByNameAsFloat('LONG',long);
       Table.SetFieldByNameAsFloat('DISTANCE',Dz);
       if (MapOwner.MapDraw.DEMonMap <> 0) then begin
          if (DEMGLB[MapOwner.MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z)) then begin
              Table.SetFieldByNameAsFloat('Z',z);
              if DoTsunami then begin
                 z := sqrt(9.8 * abs(z));
                 Table.SetFieldByNameAsFloat('SPEED_MS',z);
                 z := z * 0.001 * 60 * 60;
                 Table.SetFieldByNameAsFloat('SPEED_KMHR',z);
                 z := dz * 0.001 / z;
                 Table.SetFieldByNameAsFloat('TIME_HR',z);
              end;
          end
          else begin
             //WriteLineToDebugFile(LatLongDegreeToString(Lat,Long,DecDegrees) + '  z=' + RealToString(z,-12,2));
          end;
       end;
       Table.Post;
   end;


begin
   with MapOwner do begin
       fName := Petmar.NextFileNumber(MDTempDir, 'rhumb_line',DefaultDBExt);
       Make_Tables.CreateLatLongZTable(fName,true,false,false,false,true,false);
       Table := tMyData.Create(fName);

       if DoTsunami then  begin
          Table.InsureFieldPresentAndAdded(ftFloat,'SPEED_KMHR',8,2);
          Table.InsureFieldPresentAndAdded(ftFloat,'SPEED_MS',8,2);
          Table.InsureFieldPresentAndAdded(ftFloat,'TIME_HR',9,6);
       end;

       CurDist := 0;
       InsertPoint(StartLat,StartLong,0);
       repeat
          CurDist := CurDist + dz;
          if CurDist < Distance then begin
             VincentyPointAtDistanceBearing(StartLat,StartLong,CurDist,Bearing,Lat2,Long2);
             InsertPoint(Lat2,Long2,dz);
          end
          else begin
              InsertPoint(EndLat,EndLong,Distance-(CurDist-dz));
          end;
       until (CurDist > Distance);

       if DoTsunami then begin
          AvgSpeed := Table.FieldSum('SPEED_KMHR')/Table.RecordCount;
          Memo1.Lines.Add('Points: ' + IntToStr(NPts));
          Memo1.Lines.Add('Spacing (m)=' + RealToString(dz,-12,-2));
          Memo1.Lines.Add('Avg depth (m): ' + RealToString(Table.FieldSum('Z')/Table.RecordCount,8,3));
          Memo1.Lines.Add('Hours: ' + RealToString(Table.FieldSum('TIME_HR'),8,3));
          Memo1.Lines.Add('Avg Speed (km/hr): ' + RealToString(AvgSpeed,8,3));
          Memo1.Lines.Add('Time avg speed: ' + RealToString(0.001 * Distance/AvgSpeed,8,3));
          Table.ApplyFilter( 'Z>0');
          if (Table.RecordCount > 0) then begin
             Memo1.Lines.Add('Path on land for ' + IntToStr(Table.RecordCount) + ' points');
          end;
          Memo1.Lines.Add('');
       end;
       Table.Destroy;
       LoadDataBaseFile(fName);
    end;
    if CheckBox1.Checked and (MapOwner.MapDraw.DEMonMap <> 0) then begin
       StartLOS(true,JustWandering,MapOwner.MapDraw.DEMonMap,StartLat,StartLong,Endlat,EndLong,MapOwner);
    end;
end;

procedure TMapRtForm.BitBtn6Click(Sender: TObject);
var
   Dist,Az : float64;
begin
   CheckEditString(Edit3.Text,Dist);
   CheckEditString(Edit4.Text,Az);
   VincentyPointAtDistanceBearing(StartLat,StartLong,Dist,Az,EndLat,EndLong);
   UpdateForm;
end;

procedure TMapRtForm.Edit1Change(Sender: TObject);
begin
   if Edit1.Enabled then begin
      Petmar.CheckEditString(Edit1.Text,dz);
      NPts := succ(trunc(Distance / dz));
      Edit2.Text := IntToStr(NPts);
   end;
end;

procedure TMapRtForm.Edit2Change(Sender: TObject);
begin
   if Edit2.Enabled then begin
      CheckEditString(Edit2.Text,NPts);
      dz := Distance / pred(NPts);
      Edit1.Text := RealToString(dz,-18,-2);
   end;
end;

procedure TMapRtForm.Edit3Change(Sender: TObject);
begin
   BitBtn6.Enabled := (Edit3.Text <> '') and (Edit4.Text <> '') and (StartLat > -90);
end;

procedure TMapRtForm.Edit4Change(Sender: TObject);
begin
   Edit3Change(Sender);
end;

procedure TMapRtForm.UpdateForm;
begin
   if (StartLat > -90) then begin
      Label1.Caption := LatLongDegreeToString(StartLat,StartLong,MDDef.OutPutLatLongMethod);
   end;
   if (EndLat > -90) then begin
      Label2.Caption := LatLongDegreeToString(EndLat,EndLong,MDDef.OutPutLatLongMethod);
   end;
   if (StartLat > -90) and (EndLat > -90) then begin
      VincentyCalculateDistanceBearing(StartLat,StartLong,EndLat,EndLong,Distance,Bearing);
      Label3.Caption := 'Distance: ' + PetMar.SmartDistanceMetersFormat(Distance) + '  aziumuth=' + RealToString(Bearing,-8,2) + DegSym;
      Edit1Change(Nil);
      Edit2Change(Nil);
      BitBtn5.Enabled := true;
   end;
end;



procedure TMapRtForm.FormCreate(Sender: TObject);
begin
   StartLat := -99;
   EndLat := -99;
end;

procedure TMapRtForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\rhumb_lines.htm');
end;

procedure TMapRtForm.RadioGroup1Click(Sender: TObject);
begin
   Edit1.Enabled := RadioGroup1.ItemIndex = 0;
   Edit2.Enabled := RadioGroup1.ItemIndex = 1;
end;



end.
