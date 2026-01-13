{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


//probably only relatively minor changes from source, to get bitmap for use as map icon

// Copyright (C) 2000, Earl F. Glynn, efg's Computer Lab, Overland Park, KS.
// All Rights Reserved Worldwide.
// May be used freely for non-commercial use.
// Reproduction for profit requires permission.

// Thanks to Mike Lischke for letting me review his source code to
// his Unicode viewer.  www.lischke-online.de, public@lischke-online.de

unit ScreenUnicode;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, ExtCtrls, ComCtrls, Vcl.Buttons;

type
  TFormUnicode = class(TForm)
    StringGridSector: TStringGrid;
    StringGridCharacter: TStringGrid;
    LabelSector: TLabel;
    LabelSectorRow: TLabel;
    LabelSectorColumn: TLabel;
    LabelCharacter: TLabel;
    LabelCharacterRow: TLabel;
    LabelCharacterColumn: TLabel;
    LabelUnicode: TLabel;
    LabelUnicode1: TLabel;
    LabelUnicode2: TLabel;
    LabelUnicode3: TLabel;
    LabelUnicode4: TLabel;
    LabelBlockSelector: TLabel;
    ComboBoxBlock: TComboBox;
    ButtonClear: TButton;
    ImageUnicodeString: TImage;
    ButtonBackspace: TButton;
    ButtonCopyToClipboard: TButton;
    CheckBoxAdd: TCheckBox;
    LabelPixels: TLabel;
    LabelPixelsValue: TLabel;
    ComboBoxFonts: TComboBox;  (* Ciarán Ó Duibhín 2007/08/30 *)
    LabelFont: TLabel;
    ColorDialog1: TColorDialog;
    BitBtn1: TBitBtn;
    Image1: TImage;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    TrackBar1: TTrackBar;
    Image2: TImage;
    CheckBox1: TCheckBox;         (* Ciarán Ó Duibhín 2007/08/30 *)
    procedure StringGridSectorDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure StringGridSectorClick(Sender: TObject);
    procedure StringGridCharacterDrawCell(Sender: TObject; ACol,ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure StringGridCharacterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxBlockChange(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonBackspaceClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonCopyToClipboardClick(Sender: TObject);
    procedure ComboBoxFontsChange(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);  (* Ciarán Ó Duibhín  2007/08/30 *)
  private
    UnicodeRangeFrom    :  WORD;
    UnicodeRangeTo      :  WORD;
    UnicodeString       :  WideString;
  public
    UnicodeFont: string;  (* Ciarán Ó Duibhín  2007/08/30 *)
    UnicodeColor : tColor;
    PROCEDURE DisplayUnicodeString;
    PROCEDURE DisplayUnicodeGraphic;
    PROCEDURE SetUnicodeBlockFromSelection;
    PROCEDURE UpdateEverything;
  public
    { Public declarations }
  end;

var
  FormUnicode : TFormUnicode;

implementation
{$R *.DFM}

  USES
    Petmar,Petmar_types,Petimage,   //PLG additions
    UnicodeLibrary;  // Unicode Blocks

  CONST
    BackgroundColor    = clWhite;
    SelectedBlockColor = clYellow;
    SelectedCellColor  = clBlue;

  PROCEDURE CenterText(CONST Canvas:  TCanvas;  CONST Rect:  TRect; CONST s:  STRING);
  BEGIN
    Canvas.TextRect(Rect,(Rect.Left + Rect.Right  - Canvas.TextWidth(s))  DIV 2,(Rect.Top  + Rect.Bottom - Canvas.TextHeight(s)) DIV 2,s)
  END {CenterText};


  PROCEDURE TFormUnicode.DisplayUnicodeString;
    VAR
      Bitmap:  TBitmap;
      Size  :  TSize;
  BEGIN
    Bitmap := TBitmap.Create;
    TRY
      Bitmap.Width  := ImageUnicodeString.Width;
      Bitmap.Height := ImageUnicodeString.Height;
      Bitmap.Canvas.Brush.Color := clWhite;
      Bitmap.Canvas.FillRect(Bitmap.Canvas.ClipRect);

      Bitmap.Canvas.Font.Name := UnicodeFont;
      Bitmap.Canvas.Font.Height := Bitmap.Height;

      // "scroll left" as necessary to make sure most recent character fits
      GetTextExtentPoint32W(Bitmap.Canvas.Handle, pWideChar(UnicodeString),Length(UnicodeString), size);
      WHILE size.cx > Bitmap.Width DO  BEGIN
        Delete(UnicodeString, 1, 1);
        GetTextExtentPoint32W(Bitmap.Canvas.Handle, pWideChar(UnicodeString),Length(UnicodeString), size)
      END;

      TextOutW(Bitmap.Canvas.Handle, 0, 0, pWideChar(UnicodeString), Length(UnicodeString));
      ImageUnicodeString.Picture.Graphic := Bitmap
    FINALLY
      Bitmap.Free
    END;

    IF Length(UnicodeString) = 0 THEN BEGIN
      ButtonClear.Enabled     := FALSE;
      ButtonBackspace.Enabled := FALSE
    END
    ELSE BEGIN
      ButtonClear.Enabled     := TRUE;
      ButtonBackspace.Enabled := TRUE
    END

  END {DisplayUnicodeString};


  PROCEDURE TFormUnicode.DisplayUnicodeGraphic;
    VAR
      Bitmap  :  TBitmap;
      Size    :  TSize;
      w       :  WideChar;
  BEGIN
     w :=  Widechar(16*16*16*(StringGridSector.Row-1) +
                       16*16*(StringGridSector.Col-1)     +
                          16*(StringGridCharacter.Row-1) +
                             (StringGridCharacter.Col-1) );

    Bitmap := GetUnicodeBitmap(UnicodeFont, w, Image2.Height, Size,UnicodeColor);
    (* Ciarán Ó Duibhín 2007/08/30 *)
    TRY
      Image2.Picture.Graphic := Bitmap
    FINALLY
      Bitmap.Free
    END;

    Bitmap := GetUnicodeBitmap(UnicodeFont, w, TrackBar1.Position, Size,UnicodeColor);
    (* Ciarán Ó Duibhín 2007/08/30 *)
    TRY
       if CheckBox1.Checked then GetImagePartOfBitmap(Bitmap);
      Image1.Picture.Graphic := Bitmap;
    FINALLY
      Bitmap.Free
    END;
    LabelPixelsValue.Caption := IntToStr(Size.cx) + '-by-' + IntToStr(Size.cy);
  END {DisplayUnicodeGraphic};


  PROCEDURE TFormUnicode.SetUnicodeBlockFromSelection;
    VAR
      index:  INTEGER;
      w    :  WORD;
  BEGIN
    w :=  16*16*16*(StringGridSector.Row-1) +
             16*16*(StringGridSector.Col-1)     +
                16*(StringGridCharacter.Row-1) +
                   (StringGridCharacter.Col-1);

    index := 0;
    WHILE (w > UnicodeLibrary.UnicodeBlock[index].StartCode) AND
          (w > UnicodeLibrary.UnicodeBlock[index].StopCode)  DO BEGIN
      INC(index)
    END;

    IF  (w >= UnicodeLibrary.UnicodeBlock[index].StartCode) AND
        (w <= UnicodeLibrary.UnicodeBlock[index].StopCode)
    THEN BEGIN
      ComboBoxBlock.ItemIndex := index;
      UnicodeRangeFrom := UnicodeLibrary.UnicodeBlock[index].StartCode;
      UnicodeRangeTo   := UnicodeLibrary.UnicodeBlock[index].StopCode
    END
    ELSE BEGIN
      ComboBoxBlock.ItemIndex := -1;  // unknown block
      UnicodeRangeFrom := w;          // "range" is this single character
      UnicodeRangeTo   := w
    END
  END {SetUnicodeBlockFromSelection};


  PROCEDURE TFormUnicode.UpdateEverything;
  BEGIN
    // 1.  Force repaint of StringGrids
    StringGridSector.Invalidate;
    StringGridCharacter.Invalidate;

    // 2.  Update graphic
    DisplayUnicodeGraphic;

    // 3.  Update all hex labels
    LabelSectorRow.Caption    := IntToHex(StringGridSector.Row-1, 1);
    LabelSectorColumn.Caption := IntToHex(StringGridSector.Col-1, 1);

    LabelCharacterRow.Caption    := IntToHex(StringGridCharacter.Row-1, 1);
    LabelCharacterColumn.Caption := IntToHex(StringGridCharacter.Col-1, 1);

    LabelUnicode1.Caption := LabelSectorRow.Caption;
    LabelUnicode2.Caption := LabelSectorColumn.Caption;
    LabelUnicode3.Caption := LabelCharacterRow.Caption;
    LabelUnicode4.Caption := LabelCharacterColumn.Caption;
  END {UpdateEverything};


procedure TFormUnicode.StringGridSectorDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
  VAR
    s          :  STRING;
    SectorIndex:  WORD;
begin
  IF   ARow = 0 THEN BEGIN
    StringGridSector.Canvas.Brush.Color := clBtnFace;
    StringGridSector.Canvas.FillRect (Rect);

    IF   ACol > 0 THEN BEGIN
      s := IntToHex(ACol-1,1);
      StringGridSector.Canvas.Font.Name := 'Arial';
      StringGridSector.Canvas.Font.Color := LabelSectorColumn.Font.Color;
      StringGridSector.Canvas.Font.Height := StringGridSector.DefaultRowHeight;
      CenterText(StringGridSector.Canvas, Rect, s)
    END
  END
  ELSE BEGIN
    IF   ACol = 0 THEN BEGIN
      StringGridSector.Canvas.Brush.Color := clBtnFace;
      StringGridSector.Canvas.FillRect (Rect);

      IF   ARow > 0 THEN BEGIN
        s := IntToHex(ARow-1,1);
        StringGridSector.Canvas.Font.Name := 'Arial';
        StringGridSector.Canvas.Font.Color := LabelSectorRow.Font.Color;
        StringGridSector.Canvas.Font.Height := StringGridSector.DefaultRowHeight;
        CenterText(StringGridSector.Canvas, Rect, s)
      END
    END
    ELSE BEGIN
      SectorIndex := 16*(ARow - 1) + (ACol - 1);
      IF (SectorIndex >= UnicodeRangeFrom SHR 8) AND
         (SectorIndex <= UnicodeRangeTo   SHR 8)
      THEN StringGridSector.Canvas.Brush.Color := SelectedBlockColor
      ELSE StringGridSector.Canvas.Brush.Color := BackgroundColor;

      StringGridSector.Canvas.FillRect (Rect);

      IF (ARow = StringGridSector.Row) AND
         (ACol = StringGridSector.Col)
      THEN BEGIN
        StringGridSector.Canvas.Brush.Color := SelectedCellColor;
        StringGridSector.Canvas.Pen.Color   := SelectedCellColor;
        StringGridSector.Canvas.Rectangle(Rect.Left+2, Rect.Top+2, Rect.Right-2, Rect.Bottom-2)
      END;
    END
  END
end;


procedure TFormUnicode.TrackBar1Change(Sender: TObject);
begin
   DisplayUnicodeGraphic;
end;

procedure TFormUnicode.StringGridSectorClick(Sender: TObject);
begin
  SetUnicodeBlockFromSelection;
  UpdateEverything
end;


procedure TFormUnicode.StringGridCharacterDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
  VAR
    CharacterIndex:  WORD;
    s             :  STRING;
    SectorBase    :  WORD;
    size          :  TSize;
    w             :  WideChar;
begin
  IF   ARow = 0    THEN BEGIN
    StringGridCharacter.Canvas.Brush.Color := clBtnFace;
    StringGridCharacter.Canvas.FillRect (Rect);

    IF   ACol > 0  THEN BEGIN
      s := IntToHex(ACol-1,1);
      StringGridCharacter.Canvas.Font.Name := 'Arial';
      StringGridCharacter.Canvas.Font.Color := LabelCharacterColumn.Font.Color;
      StringGridCharacter.Canvas.Font.Height := StringGridCharacter.DefaultRowHeight;
      CenterText(StringGridCharacter.Canvas, Rect, s)
    END
  END
  ELSE
    IF   ACol = 0  THEN BEGIN
      StringGridCharacter.Canvas.Brush.Color := clBtnFace;
      StringGridCharacter.Canvas.FillRect (Rect);

      IF   ARow > 0  THEN BEGIN
        s := IntToHex(ARow-1,1);
        StringGridCharacter.Canvas.Font.Name := 'Arial';
        StringGridCharacter.Canvas.Font.Color := LabelCharacterRow.Font.Color;
        StringGridCharacter.Canvas.Font.Height := StringGridCharacter.DefaultRowHeight;
        CenterText(StringGridCharacter.Canvas, Rect, s)
      END
    END
    ELSE BEGIN
      SectorBase := 256 * ( 16*(StringGridSector.Row - 1) + (StringGridSector.Col - 1) );
      CharacterIndex := SectorBase + 16*(ARow-1) + (ACol-1);

      IF   (ARow = StringGridCharacter.Row) AND
           (ACol = StringGridCharacter.Col)
      THEN StringGridCharacter.Canvas.Brush.Color := SelectedCellColor
      ELSE BEGIN
        IF   (CharacterIndex >= UnicodeRangeFrom) AND
             (CharacterIndex <= UnicodeRangeTo)
        THEN StringGridCharacter.Canvas.Brush.Color := SelectedBlockColor
        ELSE StringGridCharacter.Canvas.Brush.Color := BackgroundColor;
      END;
      StringGridCharacter.Canvas.FillRect(Rect);

      w :=  WideChar(16*16*16*(StringGridSector.Row-1) +
                        16*16*(StringGridSector.Col-1)     +
                           16*(ARow-1) +
                              (ACol-1) );

      StringGridCharacter.Canvas.Font.Name := UnicodeFont;
      StringGridCharacter.Canvas.Font.Height := StringGridCharacter.DefaultRowHeight;

      GetTextExtentPoint32W(StringGridCharacter.Canvas.Handle, @w, 1, size);

      TextOutW(StringGridCharacter.Canvas.Handle, (Rect.Left + Rect.Right - size.cx) DIV 2, Rect.Top, @w, 1)
    END

end;


procedure TFormUnicode.StringGridCharacterClick(Sender: TObject);
  VAR
      w:  WideChar;
BEGIN
  IF CheckBoxAdd.Checked  THEN BEGIN
    w :=  Widechar( 16*16*16*(StringGridSector.Row-1) +
                        16*16*(StringGridSector.Col-1)     +
                           16*(StringGridCharacter.Row-1) +
                              (StringGridCharacter.Col-1) );
    SetLength(UnicodeString, Length(UnicodeString)+1);
    UnicodeString[Length(UnicodeString)] := w;
    DisplayUnicodeString;
  END;

  SetUnicodeBlockFromSelection;
  UpdateEverything;
end;


procedure TFormUnicode.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   action := caFree;
end;

procedure TFormUnicode.FormCreate(Sender: TObject);
  CONST
    NamesListFile = 'NamesList.TXT';
  VAR
    filename     :  STRING;
    i            :  INTEGER;
begin
  UnicodeFont := 'Courier New';
  ComboBoxFonts.Clear;
  ComboBoxFonts.Items.AddStrings (Screen.Fonts);
  ComboBoxFonts.ItemIndex := Screen.Fonts.IndexOf (UnicodeFont);

  filename := ExtractFilePath(ParamStr(0)) + NamesListFile;
  IF FileExists(filename) THEN BEGIN
    UnicodeLibrary.LoadNamesListFile(filename);
    FormUnicode.Caption := UnicodeLibrary.UnicodeFilename
  END
  ELSE {ShowMessage('Missing file ' + NamesListFile + '.'#$0A + 'Info about each Unicode will not be available.')};

  FOR i := Low(UnicodeLibrary.UnicodeBlock) TO High(UnicodeLibrary.UnicodeBlock) DO BEGIN
    ComboBoxBlock.Items.Add(UnicodeLibrary.UnicodeBlock[i].BlockName);
  END;

  // At startup, show some interesting Unicode characters
  SetLength(UnicodeString, 19);
  UnicodeString[ 1] := WideChar($0152);
  UnicodeString[ 2] := WideChar($03E0);
  UnicodeString[ 3] := WideChar($0416);
  UnicodeString[ 4] := Widechar($0539);
  UnicodeString[ 5] := WideChar($0634);
  UnicodeString[ 6] := WideChar($0950);
  UnicodeString[ 7] := WideChar($0B10);
  UnicodeString[ 8] := WideChar($0B86);
  UnicodeString[ 9] := WideChar($0C0B);
  UnicodeString[10] := WideChar($0D60);
  UnicodeString[11] := WideChar($0E12);
  UnicodeString[12] := WideChar($0EDD);
  UnicodeString[13] := WideChar($0F00);
  UnicodeString[14] := WideChar($10C5);
  UnicodeString[15] := WideChar($1124);
  UnicodeString[16] := WideChar($20A9);
  UnicodeString[17] := WideChar($2103);
  UnicodeString[18] := WideChar($3020);
  UnicodeString[19] := WideChar($FFFD);

  ComboBoxBlock.ItemIndex := 0;
  UnicodeRangeFrom := UnicodeLibrary.UnicodeBlock[0].StartCode;
  UnicodeRangeTo   := UnicodeLibrary.UnicodeBlock[0].StopCode;
  UnicodeColor := clBlack;
  UpdateEverything;
  DisplayUnicodeString
end;


procedure TFormUnicode.ComboBoxBlockChange(Sender: TObject);
  var
    b    :  BYTE;
    index:  INTEGER;
begin
  index := ComboBoxBlock.ItemIndex;
  UnicodeRangeFrom := UnicodeLibrary.UnicodeBlock[index].StartCode;
  UnicodeRangeTo   := UnicodeLibrary.UnicodeBlock[index].StopCode;

  StringGridSector.OnClick := NIL;      // Block artificial "click" events
  b := UnicodeRangeFrom DIV 256;
  StringGridSector.Row := 1 + b DIV 16;
  StringGridSector.Col := 1 + b MOD 16;
  StringGridSector.OnClick :=  StringGridSectorClick;

  StringGridCharacter.OnClick := NIL;   // Block artificial "click" events
  b := UnicodeRangeFrom MOD 256;
  StringGridCharacter.Row := 1 + b DIV 16;
  StringGridCharacter.Col := 1 + b MOD 16;
  StringGridCharacter.OnClick := StringGridCharacterClick;

  UpdateEverything
end;


procedure TFormUnicode.ButtonClearClick(Sender: TObject);
begin
  SetLength(UnicodeString, 0);
  DisplayUnicodeString
end;


procedure TFormUnicode.BitBtn1Click(Sender: TObject);
begin
   ColorDialog1.Color := UnicodeColor;
   if ColorDialog1.Execute then UnicodeColor := ColorDialog1.Color;
   UpdateEverything;
end;

procedure TFormUnicode.BitBtn2Click(Sender: TObject);
begin
   Petimage.SaveImageAsBMP(Image1);
end;

procedure TFormUnicode.BitBtn3Click(Sender: TObject);
begin
   AssignImageToClipBoard(Image1);
end;

procedure TFormUnicode.ButtonBackspaceClick(Sender: TObject);
begin
  SetLength(UnicodeString, Length(UnicodeString)-1);
  DisplayUnicodeString
end;


procedure TFormUnicode.ButtonSaveClick(Sender: TObject);
begin
end;


// See Chapter 10, "Clipboard Manipulation Functions," in
// "The Tomes of Delphi 3:  Win32 Core API" for additional information.
// In particular, look at the Button1Click on p. 449.
procedure TFormUnicode.ButtonCopyToClipboardClick(Sender: TObject);
begin
   AssignImageToClipBoard(ImageUnicodeString);
end;


//  Ciarán Ó Duibhín 2007/08/30
procedure TFormUnicode.ComboBoxFontsChange(Sender: TObject);
begin
   UnicodeFont := ComboBoxFonts.Items [ComboBoxFonts.ItemIndex];
   UpdateEverything;
end;


end.
