unit dem_map_scale;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  PETMAR,Petmar_types,DEMDefs,BaseMap;

type
  TMapScaledForm = class(TForm)
    Label1: TLabel;
    ComboBox1: TComboBox;
    Label2: TLabel;
    Edit1: TEdit;
    Label3: TLabel;
    Edit2: TEdit;
    Label4: TLabel;
    Edit3: TEdit;
    HelpBtn: TBitBtn;
    BitBtn1: TBitBtn;
    Label5: TLabel;
    SpeedButton1: TSpeedButton;
    Label6: TLabel;
    SpeedButton2: TSpeedButton;
    procedure BitBtn1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    DPI,WantMapXSize,WantMapYSize : integer;
    WantLat,WantLong,
    OldCenterLat,OldCenterLong,
    xmeters,ymeters,scale : float64;
    MapDatumConstants : tMapProjection;
    procedure ShowMapSize;
  end;

var
  MapScaledForm: TMapScaledForm;


implementation

{$R *.dfm}

uses
   PETMath,GetLatLn;

procedure TMapScaledForm.BitBtn1Click(Sender: TObject);
begin
   Close;
end;

procedure TMapScaledForm.ShowMapSize;
var
   TStr : shortstring;
   i,err : integer;
begin
   SpeedButton2.Enabled := OldCenterLat > -99;
   Tstr := ComboBox1.Text;
   for i := length(Tstr) downto 1 do  if TStr[i] = ',' then Delete(TStr,i,1);
   Val(TStr,Scale,err);
   CheckEditString(Edit1.Text,DPI);
   CheckEditString(Edit2.Text,WantMapXSize);
   CheckEditString(Edit3.Text,WantMapYSize);
   xmeters := WantMapXSize / DPI * 2.54 / 100 * Scale;
   ymeters := WantMapYSize / DPI * 2.54 / 100 * Scale;
   Label5.Caption := 'Map covers: ' + RealToString(0.001*xmeters,-12,2) + ' x ' + RealToString(0.001*ymeters,-12,2) + ' km';
   Label6.Caption := 'Center: ' +  MapDatumConstants.PreferLocationString(WantLat,WantLong);
end;

procedure TMapScaledForm.ComboBox1Change(Sender: TObject);
begin
   ShowMapSize;
end;

procedure TMapScaledForm.Edit1Change(Sender: TObject);
begin
   ShowMapSize;
end;

procedure TMapScaledForm.Edit2Change(Sender: TObject);
begin
   ShowMapSize;
end;

procedure TMapScaledForm.Edit3Change(Sender: TObject);
begin
   ShowMapSize;
end;

procedure TMapScaledForm.SpeedButton1Click(Sender: TObject);
begin
   GetLatLongDefault(MapDatumConstants,'Recentering',WantLat,WantLong);
   ShowMapSize;
end;


procedure TMapScaledForm.SpeedButton2Click(Sender: TObject);
begin
    WantLat := OldCenterLat;
    WantLong := OldCenterLong;
    ShowMapSize;
end;

procedure TMapScaledForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\pick_map_center_scale.htm');
end;


end.
