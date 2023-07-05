unit computations;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, ComCtrls,
  Dialogs, StdCtrls, Buttons,
  Petmar_types;

type
  TCompForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    RadioGroup1: TRadioGroup;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Label3: TLabel;
    Label4: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label6: TLabel;
    Label5: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    HelpBtn: TBitBtn;
    CancelBtn: TBitBtn;
    DateTimePicker1: TDateTimePicker;
    Edit5: TEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RadioGroup1Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Lat1,Long1,Lat2,Long2 : float64;
  end;

procedure DoComputations;


implementation

{$R *.dfm}

uses
   {$IfDef ExGeography}
   {$Else}
      sun_position, moon_montenbruk_pfleger,
   {$EndIf}
   GetLatLn,Petmar,DEMDefs, BaseMap,demmagvar;


procedure DoComputations;
var
   CompForm: TCompForm;
begin
   CompForm := TCompForm.Create(Application);
   with CompForm do begin
       Lat1 := 0;
       Long1 := 0;
       Lat2 := 0;
       Long2 := 0;
       Edit3.Text := '1500';
       Edit4.Text := '2007';
       DateTimePicker1.Date := Now;
       Label1.Caption := LatLongDegreeToString(Lat1,Long1,MDdef.OutPutLatLongMethod);
       Label2.Caption := LatLongDegreeToString(Lat2,Long2,MDdef.OutPutLatLongMethod);
       RadioGroup1Click(Nil);
       Show;
   end;
end;


procedure TCompForm.BitBtn1Click(Sender: TObject);
begin
   GetLatLongDefault(WGS84DatumConstants, 'Point 1',Lat1,Long1);
   Label1.Caption := LatLongDegreeToString(Lat1,Long1,MDdef.OutPutLatLongMethod);
end;


procedure TCompForm.BitBtn2Click(Sender: TObject);
begin
   GetLatLongDefault(WGS84DatumConstants, 'Point 2',Lat2,Long2);
   Label2.Caption := LatLongDegreeToString(Lat2,Long2,MDdef.OutPutLatLongMethod);
end;


procedure TCompForm.BitBtn3Click(Sender: TObject);
var
   Alt,Time,Distance,Bearing,Lat,Long,Dec,DIP,TI,GV : float64;
   Year, Month, Day : word;
   Duration : integer;
begin
   if (RadioGroup1.ItemIndex = 0) then begin
      VincentyCalculateDistanceBearing(Lat1,Long1,Lat2,Long2,Distance,Bearing);
      Memo1.Lines.Add('From: ' + Label1.Caption);
      Memo1.Lines.Add('To: ' + Label2.Caption);
      Memo1.Lines.Add('  Distance: ' + SmartDistanceMetersFormat(Distance));
      Memo1.Lines.Add('  Azimuth:' + RealToString(Bearing,7,1) + DegSym);
   end;
   if (RadioGroup1.ItemIndex = 1) then begin
      CheckEditString(Edit1.Text,Distance);
      CheckEditString(Edit2.Text,Bearing);

      VincentyPointAtDistanceBearing(Lat1,Long1,Distance,Bearing,Lat,Long);
      Memo1.Lines.Add('From: ' + Label1.Caption);
      Memo1.Lines.Add('  Distance: ' + SmartDistanceMetersFormat(Distance));
      Memo1.Lines.Add('  Azimuth:' + RealToString(Bearing,7,1) + DegSym);
      Memo1.Lines.Add('Is: ' + LatLongDegreeToString(Lat,Long,MDdef.OutPutLatLongMethod));
   end;
   if (RadioGroup1.ItemIndex = 2) then begin
      CheckEditString(Edit3.Text,Alt);
      CheckEditString(Edit4.Text,Time);
      DEMMagVar.MagVr1(ALT,Lat1,Long1,TIME,DEC,DIP,TI,GV);
      Memo1.Lines.Add('World Magnetic Model 2005');
      Memo1.Lines.Add(LatLongDegreeToString(Lat1,Long1,MDdef.OutPutLatLongMethod));
      Memo1.Lines.Add('Alt' + RealToString(Alt,10,1) + ' m');
      Memo1.Lines.Add('Year' + RealToString(Time,9,3));
      Memo1.Lines.Add('Dec' + RealToString(Dec,10,2) + DegSym);
      Memo1.Lines.Add('Dip' + RealToString(Dip,10,2) + DegSym);
      Memo1.Lines.Add('TI' + RealToString(TI,11,0));
   end;
   {$IfDef ExGeography}
   {$Else}
      if (RadioGroup1.ItemIndex = 3) then begin
         SunRiseSunSet(Lat1,Long2,true,true);
      end;
      if (RadioGroup1.ItemIndex = 4) then begin
         DecodeDate(DateTimePicker1.Date,Year, Month, Day);
         CheckEditString(Edit5.Text,Duration);
         moon_montenbruk_pfleger.MoonRise(Month,Day,Year,Duration,Lat1,Long1);
      end;
   {$EndIf}
   Memo1.Lines.Add(' ');
end;


procedure TCompForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;


procedure TCompForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\computations.htm');
end;

procedure TCompForm.RadioGroup1Click(Sender: TObject);
begin
   BitBtn2.Enabled := RadioGroup1.ItemIndex = 0;

   Edit1.Enabled := RadioGroup1.ItemIndex = 1;
   Edit2.Enabled := RadioGroup1.ItemIndex = 1;
   Label3.Enabled := RadioGroup1.ItemIndex = 1;
   Label4.Enabled := RadioGroup1.ItemIndex = 1;

   Edit3.Enabled := RadioGroup1.ItemIndex = 2;
   Edit4.Enabled := RadioGroup1.ItemIndex = 2;
   Label5.Enabled := RadioGroup1.ItemIndex = 2;
   Label6.Enabled := RadioGroup1.ItemIndex = 2;

   Edit5.Enabled := RadioGroup1.ItemIndex = 4;
   DateTimePicker1.Enabled := RadioGroup1.ItemIndex = 4;
end;


initialization
finalization
end.
