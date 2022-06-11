unit photoalbummain;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, ExtCtrls, StdCtrls,
  Petmar_types,PETMAR;

type
  TAlbumForm = class(TForm)
    MainMenu1: TMainMenu;
    StatusBar1: TStatusBar;
    Image1: TImage;
    Inventory2: TMenuItem;
    RichEdit1: TRichEdit;
    procedure Alldirectories1Click(Sender: TObject);
    procedure Inventory2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

uses
   Nevadia_main,PETImage,get_thumbnails,DEMDefs;


{$R *.DFM}



procedure TAlbumForm.Alldirectories1Click(Sender: TObject);
begin
   //Inventory1Click(Sender);
end;

procedure TAlbumForm.Inventory2Click(Sender: TObject);
begin
   Get_thumbnails.AlbumInventory;
end;



initialization
   ThumbnailDir := '';
   HTMLForm := false;
   FormatIndex := 1;
finalization
end.
