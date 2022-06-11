unit projcton_for_directory;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2015 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}

no longer used, Jan 2020

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  petmar,Petmar_types, Vcl.Buttons;

type
  TGet_dir_projection = class(TForm)
    RadioGroup1: TRadioGroup;
    OKBtn: TBitBtn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure GetProjectionForDirectory(Dir : DirStr);



implementation

{$R *.dfm}

uses
   Nevadia_Main;


procedure GetProjectionForDirectory(Dir : DirStr);
var
  Get_dir_projection: TGet_dir_projection;
begin
    Get_dir_projection := TGet_dir_projection.Create(Application);
    IDDirToMark := Dir;
    Get_dir_projection.ShowModal;
end;



procedure TGet_dir_projection.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TGet_dir_projection.OKBtnClick(Sender: TObject);
begin
    case RadioGroup1.ItemIndex of
       0 : wmdem.NLCD20011Click(wmdem.XYZshapefile1);
       1 : wmdem.NLCD20011Click(wmdem.Latlong1);
       2 : wmdem.NLCD20011Click(wmdem.UTM1);
       3 : wmdem.NLCD20011Click(wmdem.SPCS1);
    end;
   Close;
end;

end.
