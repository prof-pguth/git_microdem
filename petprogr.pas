unit Petprogr;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}


interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Gauges, StdCtrls, Buttons;

type
  TPetProgF = class(TForm)
    Gauge1: TGauge;
    BitBtn1: TBitBtn;
    procedure FormDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
     NeedToCheckPlacement : boolean;
     procedure SetUpForAborting;
  end;

var
  PetProgF : TPetProgF;


implementation

{$R *.DFM}

uses
   PETMAR;


procedure TPetProgF.SetUpForAborting;
begin
   Height := 150;
   BitBtn1.Visible := true;
   Repaint;
end;


procedure TPetProgF.FormDblClick(Sender: TObject);
begin
   Close;
end;


procedure TPetProgF.FormCreate(Sender: TObject);
begin
   Height := 100;
   BitBtn1.Visible := false;
   Visible := true;
   PETMAR.WantOut := false;
   NeedToCheckPlacement := false;
end;


procedure TPetProgF.BitBtn1Click(Sender: TObject);
begin
   PETMAR.WantOut := true;
end;


procedure TPetProgF.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caHide;
end;


procedure TPetProgF.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   CanClose := false;
end;


initialization
finalization
end.
