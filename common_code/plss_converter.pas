unit plss_converter;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 5/13/2018       }
{_________________________________}

{$I nevadia_defines.inc}


{$IfDef RecordProblems}   //these are normally only defined for debugging specific problems
  //{$Define RecordPLSSProblems}
{$EndIf}


interface

uses
  System.UITypes,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TPLSSConvertForm = class(TForm)
    Memo1: TMemo;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    OKBtn: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn5: TBitBtn;
    procedure OKBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}


uses
   DEM_PLSS,Get_PLss,DEM_indexes,DEMDefs,Petmath,Petmar,GetLatLn,BaseMap,US_properties,
   {$IfDef ExSats}
   {$Else}
   DEMEros,
   {$EndIf}
   Nevadia_Main,
   Petmar_types,
   DEM_Manager,
   DEMCoord,DEMCnvrt;


procedure TPLSSConvertForm.OKBtnClick(Sender: TObject);
begin
   Close;
end;

procedure TPLSSConvertForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;


procedure TPLSSConvertForm.FormCreate(Sender: TObject);
begin
   wmDEM.FormPlacementInCorner(self);
end;

procedure TPLSSConvertForm.BitBtn1Click(Sender: TObject);
var
   GeoName,PLSSString : ShortString;
   Lat,Long : float64;
begin
   {$IfDef ExSats}
   {$Else}
   CloseAllImagery;
   {$EndIf}
   Self.Visible := false;
   if GetPLSSLocation(PLSSString,Lat,Long,DEMGlb[1].SelectionMap) then begin
      {$IfDef RecordPLSSProblems}
      WriteLineToDebugFile('TPLSSConvertForm.BitBtn1Click: '  + PLSSString + '   ' + LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod));
      {$EndIf}
      Memo1.Lines.Add(PLSSString + ' = ' + LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod));
      if US_properties.GetCounty(Lat,Long,GeoName) then Memo1.Lines.Add(GeoName);
      Memo1.Lines.Add('');
   end;
   Self.Visible := true;
end;



procedure TPLSSConvertForm.BitBtn2Click(Sender: TObject);
var
   Lat,Long : float64;
   GeoName : ShortString;
begin
   {$IfDef ExSats}
   {$Else}
   CloseAllImagery;
   {$EndIf}
   Self.Visible := false;
   GetLatLong(WGS84DatumConstants,'to convert', Lat,Long);
   ShowHourglassCursor;
   {$IfDef RecordPLSSProblems}
   WriteLineToDebugFile('TPLSSConvertForm.BitBtn2Click: ' + LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod));
   {$EndIf}

   Memo1.Lines.Add(LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod) + ' = ' + PLSSLocation(Lat,Long));
   if US_properties.GetCounty(Lat,Long,GeoName) then Memo1.Lines.Add(GeoName);
   Memo1.Lines.Add('');
   Self.Visible := true;
   ShowDefaultCursor;
end;



procedure TPLSSConvertForm.BitBtn3Click(Sender: TObject);
begin
   DEMCnvrt.ConvertCoordinates(Memo1);
end;

procedure TPLSSConvertForm.BitBtn5Click(Sender: TObject);
begin
   Memo1.Lines.Clear;
end;

initialization
finalization
   {$IfDef RecordClosingProblems}
   WriteLineToDebugFile('Closing plss_converter in', true);
   {$EndIf}

   {$IfDef RecordPLSSProblems}
   WriteLineToDebugFile('RecordPLSSProblems active in plss_converter');
   {$EndIf}

   {$IfDef RecordClosingProblems}
   WriteLineToDebugFile('Closing plss_converter out');
   {$EndIf}
end.
