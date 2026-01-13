unit pick_limits;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
    //{$Define RecordGetGridLimits}
{$EndIf}

interface

uses
   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
   Dialogs, StdCtrls, Buttons,
   DEMCoord,DEMMapf,DEMDefs,Petmar_types;

type
  TpicklimitsForm = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    HelpBtn: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
  private
    { Private declarations }
    procedure LabelLimits;
  public
    { Public declarations }
    TheDEM : tDEMDataSet;
    BaseMap : tMapForm;
    GridLimits : tGridLimits;
    EditChanges : boolean;
  end;

procedure PickLimits(DEM : tDEMDataSet; inMap : tMapForm; var inGridLimits : tGridLimits);  //overload;


implementation

{$R *.dfm}

uses
   Petmar, getlatln, basemap;


procedure PickLimits(DEM : tDEMDataSet; inMap : tMapForm; var inGridLimits : tGridLimits);
var
   PicklimitsForm : TpicklimitsForm;
begin
    PicklimitsForm := TpicklimitsForm.Create(Application);
    PickLimitsForm.EditChanges := true;
    PickLimitsForm.Caption := 'Limits: ' + DEM.AreaName;
    PickLimitsForm.TheDEM := DEM;
    PickLimitsForm.BaseMap := inMap;
    PickLimitsForm.GridLimits := InGridLimits;
    PickLimitsForm.LabelLimits;

    {$IfDef RecordGetGridLimits} WriteLineToDebugFile('PickLimits in,  left=' + intToStr(Left) + '  right=' + intToStr(Right) + '  bottom=' + intToStr(Bottom)+ '  top=' + intToStr(Top)); {$EndIf}
    PickLimitsForm.ShowModal;

    inGridLimits := PickLimitsForm.GridLimits;
    PickLimitsForm.Free;
    {$IfDef RecordGetGridLimits} WriteLineToDebugFile('PickLimits out'); {$EndIf}
end;


procedure TpicklimitsForm.LabelLimits;
begin
    Edit1.Text := IntToStr(GridLimits.YGridHigh);
    Edit3.Text := IntToStr(GridLimits.XGridLow);
    Edit2.Text := IntToStr(GridLimits.YGridLow);
    Edit4.Text := IntToStr(GridLimits.XGridHigh);
    Label5.Caption := TheDEM.DEMLocationString(GridLimits.XGridLow,GridLimits.YGridHigh);
    Label6.Caption := TheDEM.DEMLocationString(GridLimits.XGridHigh,GridLimits.YGridLow);
    Label6.Left := 275 - Label6.Width;
    Label7.Caption := IntToStr(Succ(GridLimits.XGridHigh-GridLimits.XGridLow)) + ' cols x ' + IntToStr(Succ(GridLimits.YGridHigh - GridLimits.YGridLow)) + ' rows';
    EditChanges := true;
    BaseMap.OutlineGridLimitsOnMap(GridLimits);
    {$IfDef RecordGetGridLimits} WriteLineToDebugFile('TpicklimitsForm.LabelLimits in,  NW corner: ' + Label5.Caption + 'SE corner: ' + Label6.Caption + '  ' + Label7.Caption); {$EndIf}
end;

procedure TpicklimitsForm.BitBtn1Click(Sender: TObject);
begin
   Close;
end;

procedure TpicklimitsForm.BitBtn2Click(Sender: TObject);
begin
   BitBtn3Click(Sender);
end;

procedure TpicklimitsForm.BitBtn3Click(Sender: TObject);
var
   Margin : integer;
begin
   GridLimits := TheDEM.FullDEMGridLimits;
   if (Sender = BitBtn3) then begin
      Margin := 250;
      ReadDefault('Margin to exclude',Margin);
      if (2 * Margin > TheDEM.DEMheader.NumCol) or (2 * Margin > TheDEM.DEMheader.NumRow) then begin
         MessageToContinue('Margin too large for DEM size');
      end
      else with GridLimits do begin
         YGridHigh := YGridHigh - Margin;
         YGridLow := YGridLow + Margin;
         XGridLow := XGridLow + Margin;
         XGridHigh := XGridHigh - Margin;
      end;
      {$IfDef RecordGetGridLimits} WriteLineToDebugFile('Set margin at ' + IntToStr(Margin));  {$EndIf}
   end
   else if (Sender = BitBtn4) then begin
      GridLimits := BaseMap.MapDraw.MapAreaDEMGridLimits;
      {$IfDef RecordGetGridLimits} WriteLineToDebugFile('Set current map area,   left=' + intToStr(Left) + '  right=' + intToStr(Right) + '  bottom=' + intToStr(Bottom)+ '  top=' + intToStr(Top)); {$EndIf}
   end
   else begin
      {$IfDef RecordGetGridLimits}  WriteLineToDebugFile('Set entire DEM, left=' + intToStr(Left) + '  right=' + intToStr(Right) + '  bottom=' + intToStr(Bottom)+ '  top=' + intToStr(Top));   {$EndIf}
   end;
   LabelLimits;
end;


procedure TpicklimitsForm.BitBtn4Click(Sender: TObject);
begin
   BitBtn3Click(Sender);
end;

procedure TpicklimitsForm.BitBtn5Click(Sender: TObject);
var
   Lat,Long : float64;
begin
   CheckEditString(Edit1.Text,GridLimits.YGridHigh);
   CheckEditString(Edit3.Text,GridLimits.XGridLow);
   TheDEM.DEMGridToLatLongDegree(GridLimits.XGridLow,GridLimits.YGridHigh,Lat,Long);
   GetLatLn.GetLatLongDefault(BaseMap.MapDraw.PrimMapProj,'NW corner of map area',Lat,Long);
   TheDEM.LatLongDegreeToDEMGridInteger(Lat,Long,GridLimits.XGridLow,GridLimits.YGridHigh);
   Edit1.Text := IntToStr(Round(GridLimits.YGridHigh));
   Edit3.Text := IntToStr(Round(GridLimits.XGridLow));
   LabelLimits;
end;

procedure TpicklimitsForm.BitBtn6Click(Sender: TObject);
var
   Lat,Long{,Bottom,Right} : float64;
begin
   CheckEditString(Edit2.Text,GridLimits.YGridLow);
   CheckEditString(Edit4.Text,GridLimits.XGridHigh);
   TheDEM.DEMGridToLatLongDegree(GridLimits.XGridHigh,GridLimits.YGridHigh,Lat,Long);
   GetLatLn.GetLatLongDefault(BaseMap.MapDraw.PrimMapProj,'SE corner of map area',Lat,Long);
   TheDEM.LatLongDegreeToDEMGridInteger(Lat,Long,GridLimits.XGridHigh,GridLimits.YGridLow);
   Edit2.Text := IntToStr(Round(GridLimits.YGridLow));
   Edit4.Text := IntToStr(Round(GridLimits.XGridHigh));
   LabelLimits;
end;


procedure TpicklimitsForm.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,GridLimits.YGridHigh);
   LabelLimits;
end;

procedure TpicklimitsForm.Edit2Change(Sender: TObject);
begin
    CheckEditString(Edit2.Text,GridLimits.YGridLow);
    LabelLimits;
end;

procedure TpicklimitsForm.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,GridLimits.XGridLow);
   LabelLimits;
end;

procedure TpicklimitsForm.Edit4Change(Sender: TObject);
begin
   CheckEditString(Edit4.Text,GridLimits.XGridHigh);
   LabelLimits;
end;

procedure TpicklimitsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;


procedure TpicklimitsForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\pick_limits.htm');
end;


initialization
finalization
    {$IfDef RecordGetGridLimits} WriteLineToDebugFile('RecordGetGridLimits active in pick_limits'); {$EndIf}
end.
