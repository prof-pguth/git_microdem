unit survey_lines;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  DEMMapf,Petmar_types;

type
  TGetTracjksForm = class(TForm)
    BitBtn1: TBitBtn;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Edit6: TEdit;
    Label7: TLabel;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MapOwner : tMapForm;
    Heading,AlongTrack,BackTrack,LineSpacing,
    Lat,Long : float64;
    LinesRight,LinesLeft : integer;
    LinePoints : tStringList;
  end;


procedure GetSurveyTracks(aMapOwner : tMapForm);

implementation

{$R *.dfm}

Uses
   GetLatLn,Petmar,DEMdefs,BaseMap,Petmath;


procedure GetSurveyTracks(aMapOwner : tMapForm);
var
  GetTracjksForm : TGetTracjksForm;
begin
   GetTracjksForm := TGetTracjksForm.Create(Application);
   GetTracjksForm.MapOwner := aMapOwner;
   if ClipBoard_Coords then begin;
      GetTracjksForm.Lat := Clipboard_Lat;
      GetTracjksForm.Long := ClipBoard_Long;
      GetTracjksForm.LinePoints := tStringList.Create;
      GetTracjksForm.BitBtn1Click(nil);
   end;
   GetTracjksForm.ShowModal;
end;


procedure TGetTracjksForm.BitBtn1Click(Sender: TObject);
var
   xp,yp : integer;
begin
  if Sender <> Nil then GetLatLn.GetLatLong(MapOwner.MapDraw.PrimMapProj,'starting point',Lat,Long);
  Label7.Caption := LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod);
  MapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,xp,yp);
  Petmar.ScreenSymbol(MapOwner.Image1.Canvas,xp,yp,MDDef.DefGISSymbol);
end;


procedure TGetTracjksForm.BitBtn2Click(Sender: TObject);
var
   Heading,Heading2 : float64;

    procedure DrawLine(LineNum : integer; Lat1,Long1,Lat2,Long2 : float64);
    var
       xp,yp :integer;
    begin
       LinePoints.Add('Line ' + IntToStr(LineNum) + ',' + RealToString(Lat1,-12,-7) + ',' + RealToString(Long1,-12,-7) + ',' +
                   RealToString(Lat2,-12,-7) + ',' + RealToString(Long2,-12,-7) + ',' + RealToString(Heading,-12,-1) + ',' + RealToString(Heading2,-12,-1)) ;
       MapOwner.Image1.Canvas.Pen.Width := 3;
       MapOwner.Image1.Canvas.Pen.Color := clRed;

       MapOwner.MapDraw.LatLongDegreeToScreen(Lat1,Long1,xp,yp);
       MapOwner.Image1.Canvas.MoveTo(xp,yp);
       MapOwner.MapDraw.LatLongDegreeToScreen(Lat2,Long2,xp,yp);
       MapOwner.Image1.Canvas.LineTo(xp,yp);
    end;


var
   Lat1,Lat2,Long1,Long2 : float64;
   aLat1,aLat2,aLong1,aLong2 : float64;
   i,j,LineNum : integer;
begin
   MapOwner.DoFastMapRedraw;
   LinePoints.Clear;
   LinePoints.Add('NAME,LAT,LONG,LAT2,LONG2,HEADING1,HEADING2');
   CheckEditString(Edit1.Text,Heading);
   CheckEditString(Edit2.Text,AlongTrack);
   CheckEditString(Edit3.Text,BackTrack);
   CheckEditString(Edit4.Text,LineSpacing);
   CheckEditString(Edit5.Text,LinesRight);
   CheckEditString(Edit6.Text,LinesLeft);
   Heading2 := Petmath.FindCompassAngleInRangeFloat64(Heading+180);

   VincentyPointAtDistanceBearing(Lat,Long,AlongTrack,Heading,Lat1,Long1);
   VincentyPointAtDistanceBearing(Lat,Long,BackTrack,Heading + 180,Lat2,Long2);
   LineNum := 0;
   for i := -LinesLeft to LinesRight do begin
      inc(LineNum);
      if i < 0 then j := -90 else j := 90;
      VincentyPointAtDistanceBearing(Lat1,Long1,abs(i) * LineSpacing,Heading+j,aLat1,aLong1);
      VincentyPointAtDistanceBearing(Lat2,Long2,abs(i) * LineSpacing,Heading+j,aLat2,aLong2);
      DrawLine(LineNum,aLat1,aLong1,aLat2,aLong2);
   end;
end;


procedure TGetTracjksForm.BitBtn3Click(Sender: TObject);
var
   fName : PathStr;
begin
   if (LinePoints.Count > 0) then begin
      fName := PetMar.NextFileNumber(MDtempDir,'tracks_','.csv');
      MapOwner.StringListToLoadedDatabase(LinePoints,fName);
      LinePoints := tStringList.Create;
   end;
end;


initialization
finalization
end.
