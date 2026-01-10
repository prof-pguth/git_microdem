unit compare_geo_utm;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IFDEF DEBUG}
{$ELSE}
{$ENDIF}



interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  Tcompare_geo_utm_geomorphometry = class(TForm)
  private
    { Private declarations }
    procedure OpenDEMs;
  public
    { Public declarations }
       GeoDEM,UTMDEM,GeoSlope,UTMslope : integer;
  end;

var
  compare_geo_utm_geomorphometry: Tcompare_geo_utm_geomorphometry;


procedure CompareGeoUTM;

implementation

{$R *.dfm}

uses
   Petmar,petmar_types,DEMDefs,Make_grid,DEM_Manager,DEMcoord,DEMStat;


procedure CompareGeoUTM;
var
   compare_geo_utm_geomorphometry : Tcompare_geo_utm_geomorphometry;
begin
    compare_geo_utm_geomorphometry := Tcompare_geo_utm_geomorphometry.Create(Application);
    compare_geo_utm_geomorphometry.show;
    compare_geo_utm_geomorphometry.OpenDEMs;
end;

{ Tcompare_geo_utm_geomorphometry }

procedure Tcompare_geo_utm_geomorphometry.OpenDEMs;
var
   fName : PathStr;
   r : float32;
begin
    fName := 'J:\aa_new_zealand\qgis_utm_reprojection_ref_1sec_point.tif';
    GeoDEM := OpenNewDEM(fName);
    GeoSlope := CreateSlopeMap(GeoDEM);
    fName := 'J:\aa_new_zealand\qgis_utm_reprojection_ref_30m.tif';
    UTMDEM := OpenNewDEM(fName);
    UTMslope := CreateSlopeMap(UTMDEM);

    GridScatterGram(DEMglb[GeoDEM].FullDEMGridLimits,r,GeoSlope,UTMSlope);
    GridScatterGram(DEMglb[UTMDEM].FullDEMGridLimits,r,UTMSlope,GeoSlope);
end;



end.
