// Copyright (C) 2000, Earl F. Glynn, efg's Computer Lab, Overland Park, KS.
// All Rights Reserved Worldwide.
// May be used freely for non-commercial use.
// Reproduction for profit requires permission.

unit ScreenSave;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Spin, ExtDlgs;

type
  TFormSave = class(TForm)
    BitBtn1: TBitBtn;
    ImageUnicode: TImage;
    LabelUnicode: TLabel;
    LabelUnicode1: TLabel;
    LabelUnicode2: TLabel;
    LabelUnicode3: TLabel;
    LabelUnicode4: TLabel;
    SpinEditSize: TSpinEdit;
    Label1: TLabel;
    SavePictureDialog: TSavePictureDialog;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    procedure FormActivate(Sender: TObject);
    procedure SpinEditSizeChange(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    w:  WideChar;
    PROCEDURE DisplayUnicodeGraphic;
  public
    { Public declarations }
  end;

var
  FormSave: TFormSave;

implementation
{$R *.DFM}

uses
  Petmar,Petmar_types,Petimage,   //PLG additions
  ScreenUnicode,
  UnicodeLibrary;

//========================================================================

PROCEDURE TFormSave.DisplayUnicodeGraphic;
  VAR
    Bitmap:  TBitmap;
    Size  :  TSize;    // don't use this here
BEGIN
  Bitmap := GetUnicodeBitmap(FormUnicode.UnicodeFont, w, SpinEditSize.Value, Size,FormUnicode.UnicodeColor);
  (* Ciarán Ó Duibhín 2007/08/30 *)
  TRY
    ImageUnicode.Picture.Graphic := Bitmap;
  FINALLY
    Bitmap.Free
  END;
END {DisplayUnicodeGraphic};


procedure TFormSave.FormActivate(Sender: TObject);
begin
  FormSave.Caption := 'Save';

  LabelUnicode1.Caption := FormUnicode.LabelUnicode1.Caption;
  LabelUnicode2.Caption := FormUnicode.LabelUnicode2.Caption;
  LabelUnicode3.Caption := FormUnicode.LabelUnicode3.Caption;
  LabelUnicode4.Caption := FormUnicode.LabelUnicode4.Caption;

  w :=  Widechar( 16*16*16*(FormUnicode.StringGridSector.Row-1) +
                     16*16*(FormUnicode.StringGridSector.Col-1)     +
                        16*(FormUnicode.StringGridCharacter.Row-1) +
                           (FormUnicode.StringGridCharacter.Col-1) );
  SpinEditSize.Value := 512;
  DisplayUnicodeGraphic;
  SavePictureDialog.InitialDir := ExtractFilePath( ParamStr(0) )
end;


procedure TFormSave.SpinEditSizeChange(Sender: TObject);
begin
  DisplayUnicodeGraphic
end;


procedure TFormSave.BitBtn2Click(Sender: TObject);
begin
   Petimage.SaveImageAsBMP(ImageUnicode);
end;

procedure TFormSave.BitBtn3Click(Sender: TObject);
begin
   AssignImageToClipBoard(ImageUnicode);
end;



initialization
Finalization
end.
