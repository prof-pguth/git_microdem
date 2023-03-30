unit map_splitter;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Petmar_types;

type
  Tsplitter_form = class(TForm)
    Image1: TImage;
    Splitter1: TSplitter;
    Image2: TImage;
    procedure Splitter1Moved(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    RightBMP : tMyBitmap;
    LeftWidth,RightWidth : integer;
  public
    { Public declarations }
    procedure LoadRight;
  end;


procedure ImageSplitHorizontal(fName1,fname2 : PathStr);


implementation

{$R *.dfm}

uses
   Petmar,PetImage;


procedure ImageSplitHorizontal(fName1,fname2 : PathStr);
var
  splitter_form: Tsplitter_form;
begin
   if FileExists(fName1) and FileExists(fName2) then begin
      splitter_form := Tsplitter_form.Create(Application);
      splitter_form.Caption := ExtractFileNameNoExt(fName1) + '  |||||  ' + ExtractFileNameNoExt(fName2);
      splitter_form.RightBMP := LoadBitmapFromFile(fName2);
      splitter_form.ClientHeight := splitter_form.RightBMP.height;
      splitter_form.ClientWidth := splitter_form.RightBMP.Width;
      splitter_form.Image1.Picture.LoadFromFile(fName1);
      splitter_form.LoadRight;
   end;
end;



{ Tsplitter_form }

procedure Tsplitter_form.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure Tsplitter_form.LoadRight;
begin
   LeftWidth := Splitter1.Left;
   RightWidth := ClientWidth - Splitter1.Left;
   Image2.Canvas.CopyRect(Rect(0,0,RightWidth,RightBmp.Height),RightBMP.Canvas,Rect(LeftWidth,0,RightBMP.Width,RightBmp.Height));
end;

procedure Tsplitter_form.Splitter1Moved(Sender: TObject);
begin
   LoadRight;
end;

end.
