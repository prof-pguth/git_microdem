unit uk_os_converter;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define ShowProjection}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, ComCtrls,
  DEMDefs,Petmar_types,BaseMap;

type
  TCoordConvertForm = class(TForm)
    RichEdit1: TRichEdit;
    BitBtn4: TBitBtn;
    HelpBtn: TBitBtn;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    BitBtn1: TBitBtn;
    TabSheet2: TTabSheet;
    Label6: TLabel;
    Label7: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Parameters: TTabSheet;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    BitBtn5: TBitBtn;
    Edit10: TEdit;
    TabSheet4: TTabSheet;
    RadioGroup1: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    procedure CommonConversion(Lat, Long : float64);
    { Private declarations }
  public
    { Public declarations }
     This_projection : BaseMap.tMapProjection;
     FromMap,ToMap : tMapProjection;
     procedure ShowParams;
  end;


implementation

{$R *.dfm}

uses
   PETMAR,PETMATH,GetLatLn;

   
procedure TCoordConvertForm.ShowParams;
begin
   Edit5.Text := RealToString(This_projection.ProjMapScale,-18,-6);
   Edit6.Text := RealToString(This_projection.false_east,-18,-6);
   Edit7.Text := RealToString(This_projection.false_north,-18,-6);
   Edit8.Text := RealToString(This_projection.a,-18,-6);
   Edit10.Text := RealToString(This_projection.Long0/DegToRad,-18,-6);
   Label11.Caption := 'e=' +  RealToString(e,-18,-6) + ' && e²=' + RealToString(This_projection.e2,-18,-6) ;
end;


procedure TCoordConvertForm.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
   RadioGroup1.ItemIndex := ord(MDDef.OutPutLatLongMethod);
   This_projection := CreateUKOSprojection;
   {$IfDef ShowProjection} This_projection.WriteProjectionParametersToDebugFile('TUKOSConvertForm.FormCreate');  {$EndIf}
   ShowParams;
   FromMap := tMapProjection.Create;
   ToMap := tMapProjection.Create;
end;


procedure TCoordConvertForm.FormDestroy(Sender: TObject);
begin
   This_projection.Free;
   FromMap.Free;
   ToMap.Free;
end;


procedure TCoordConvertForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\drift\proj_conv.htm');
end;

procedure TCoordConvertForm.RadioGroup1Click(Sender: TObject);
begin
   MDDef.OutPutLatLongMethod := tLatLongMethod(RadioGroup1.ItemIndex);
end;


procedure TCoordConvertForm.CommonConversion(Lat,Long : float64);
var
   OutLat,OutLong,xutm,yutm  : float64;
begin
   ToMap.DefineDatumFromUTMZone('WGS84',GetUTMZone(long),HemiFromLat(Lat),'TUKOSConvertForm.CommonConversion');
   if (This_projection.PName <> PolarStereographicEllipsoidal) then begin
      if (This_projection.PName = UK_OS) then FromMap.DefineDatumFromUTMZone('OGB-A',GetUTMZone(long),'N','TUKOSConvertForm.CommonConversion UKOS')
      else if (This_projection.PName = Finn_GK) then FromMap.DefineDatumFromUTMZone('KKJ',GetUTMZone(long),'N','TUKOSConvertForm.CommonConversion KKJ');
      if (FromMap.h_datumcode <> ToMap.h_datumcode) then MolodenskiyTransformation(Lat,Long,OutLat,OutLong,FromMap,ToMap);
      RichEdit1.Lines.Add('WGS84:  ' + LatLongDegreeToString(OutLat,OutLong,MDDef.OutPutLatLongMethod));
   end;
   ToMap.ForwardProjectDegrees(Lat,Long,XUTM,YUTM);
   RichEdit1.Lines.Add('UTM zone ' + IntToStr(ToMap.projUTMZone) + ':  x=' + RealToString(xutm,-12,0) + '  y=' + RealToString(yutm,-12,0));
   RichEdit1.Lines.Add('');
end;


procedure TCoordConvertForm.BitBtn1Click(Sender: TObject);
var
   x_spcs,y_spcs,Lat,Long : float64;
begin
   CheckEditString(Edit1.Text,x_spcs);
   CheckEditString(Edit2.Text,y_spcs);
   This_projection.InverseProjectDegrees(x_spcs,y_spcs,Lat,Long);
   RichEdit1.Lines.Add('Local datum:  x=' + RealToString(x_spcs,-12,1) + '   &  y=' + RealToString(y_spcs,-12,1));
   RichEdit1.Lines.Add('Local datum: ' + LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod));
   CommonConversion(Lat,Long);
end;


procedure TCoordConvertForm.BitBtn2Click(Sender: TObject);
var
   Lat,Long,x,y : float64;
begin
   CheckEditString(Edit3.Text,Lat);
   CheckEditString(Edit4.Text,Long);
   RichEdit1.Lines.Add('Local datum: ' + LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod));
   This_projection.ForwardProjectRadians(Lat*DegToRad,Long*DegToRad,x,y);
   RichEdit1.Lines.Add('Local datum:  x=' + RealToString(x,-12,1) + '   &  y=' + RealToString(y,-12,1));
   CommonConversion(Lat,Long);
end;


procedure TCoordConvertForm.BitBtn3Click(Sender: TObject);
var
   Lat,Long : float64;
begin
   CheckEditString(Edit3.Text,Lat);
   CheckEditString(Edit4.Text,Long);
   GetLatLn.GetLatLongDefaultNoDatum('Lat/long to convert',Lat,Long);
   Edit3.Text := RealToString(Lat,-18,-8);
   Edit4.Text := RealToString(Long,-18,-8);
end;


procedure TCoordConvertForm.BitBtn4Click(Sender: TObject);
begin
   RichEdit1.Lines.Clear;
end;


procedure TCoordConvertForm.BitBtn5Click(Sender: TObject);
var
   f : float64;
begin
   CheckEditString(Edit5.Text,This_projection.ProjMapScale);
   CheckEditString(Edit6.Text,This_projection.false_east);
   CheckEditString(Edit7.Text,This_projection.false_north);
   CheckEditString(Edit8.Text,This_projection.a);
   CheckEditString(Edit9.Text,f);
   CheckEditString(Edit10.Text,This_projection.Long0);
   This_projection.Long0 := This_projection.Long0 * DegToRad;
   This_projection.e2 := 2*f-sqr(f);
   This_projection.e := sqrt(This_projection.e2);
   This_projection.SetDatumAndTMConstants;
end;


initialization
finalization
end.
