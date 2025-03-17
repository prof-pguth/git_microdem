{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

//probably no changes from source (needed for other unit, to get bitmap of Unicode for map icons


// Copyright (C) 2000, Earl F. Glynn, efg's Computer Lab, Overland Park, KS.
// All Rights Reserved Worldwide.
// May be used freely for non-commercial use.
// Reproduction for profit requires permission.

UNIT UnicodeLibrary;

INTERFACE

  USES
    Windows,  // TSize
    Graphics, // TBitmap
    Classes;  // TStringList

  TYPE
    TUnicodeBlock =  RECORD
      StartCode :  WORD;
      StopCode  :  WORD;
      BlockName :  String
    END;

  CONST
    //  From ftp://ftp.unicode.org/Public/UNIDATA/Blocks.txt
    //  Hard-coded so these will always be present.
    //  Could search "@@" entries in NamesList.Txt for the same info.
    UnicodeBlockCount = 88;
    UnicodeBlock:  ARRAY[0..UnicodeBlockCount-1] of TUnicodeBlock =
     ((StartCode: $0000;  StopCode: $007F;  BlockName: 'Basic Latin'),
      (StartCode: $0080;  StopCode: $00FF;  BlockName: 'Latin-1 Supplement'),
      (StartCode: $0100;  StopCode: $017F;  BlockName: 'Latin Extended-A'),
      (StartCode: $0180;  StopCode: $024F;  BlockName: 'Latin Extended-B'),
      (StartCode: $0250;  StopCode: $02AF;  BlockName: 'IPA Extensions'),
      (StartCode: $02B0;  StopCode: $02FF;  BlockName: 'Spacing Modifier Letters'),
      (StartCode: $0300;  StopCode: $036F;  BlockName: 'Combining Diacritical Marks'),
      (StartCode: $0370;  StopCode: $03FF;  BlockName: 'Greek'),
      (StartCode: $0400;  StopCode: $04FF;  BlockName: 'Cyrillic'),
      (StartCode: $0530;  StopCode: $058F;  BlockName: 'Armenian'),
      (StartCode: $0590;  StopCode: $05FF;  BlockName: 'Hebrew'),
      (StartCode: $0600;  StopCode: $06FF;  BlockName: 'Arabic'),
      (StartCode: $0700;  StopCode: $074F;  BlockName: 'Syriac'),
      (StartCode: $0780;  StopCode: $07BF;  BlockName: 'Thaana'),
      (StartCode: $0900;  StopCode: $097F;  BlockName: 'Devanagari'),
      (StartCode: $0980;  StopCode: $09FF;  BlockName: 'Bengali'),
      (StartCode: $0A00;  StopCode: $0A7F;  BlockName: 'Gurmukhi'),
      (StartCode: $0A80;  StopCode: $0AFF;  BlockName: 'Gujarati'),
      (StartCode: $0B00;  StopCode: $0B7F;  BlockName: 'Oriya'),
      (StartCode: $0B80;  StopCode: $0BFF;  BlockName: 'Tamil'),
      (StartCode: $0C00;  StopCode: $0C7F;  BlockName: 'Telugu'),
      (StartCode: $0C80;  StopCode: $0CFF;  BlockName: 'Kannada'),
      (StartCode: $0D00;  StopCode: $0D7F;  BlockName: 'Malayalam'),
      (StartCode: $0D80;  StopCode: $0DFF;  BlockName: 'Sinhala'),
      (StartCode: $0E00;  StopCode: $0E7F;  BlockName: 'Thai'),
      (StartCode: $0E80;  StopCode: $0EFF;  BlockName: 'Lao'),
      (StartCode: $0F00;  StopCode: $0FFF;  BlockName: 'Tibetan'),
      (StartCode: $1000;  StopCode: $109F;  BlockName: 'Myanmar'),
      (StartCode: $10A0;  StopCode: $10FF;  BlockName: 'Georgian'),
      (StartCode: $1100;  StopCode: $11FF;  BlockName: 'Hangul Jamo'),
      (StartCode: $1200;  StopCode: $137F;  BlockName: 'Ethiopic'),
      (StartCode: $13A0;  StopCode: $13FF;  BlockName: 'Cherokee'),
      (StartCode: $1400;  StopCode: $167F;  BlockName: 'Unified Canadian Aboriginal Syllabics'),
      (StartCode: $1680;  StopCode: $169F;  BlockName: 'Ogham'),
      (StartCode: $16A0;  StopCode: $16FF;  BlockName: 'Runic'),
      (StartCode: $1780;  StopCode: $17FF;  BlockName: 'Khmer'),
      (StartCode: $1800;  StopCode: $18AF;  BlockName: 'Mongolian'),
      (StartCode: $1E00;  StopCode: $1EFF;  BlockName: 'Latin Extended Additional'),
      (StartCode: $1F00;  StopCode: $1FFF;  BlockName: 'Greek Extended'),
      (StartCode: $2000;  StopCode: $206F;  BlockName: 'General Punctuation'),
      (StartCode: $2070;  StopCode: $209F;  BlockName: 'Superscripts and Subscripts'),
      (StartCode: $20A0;  StopCode: $20CF;  BlockName: 'Currency Symbols'),
      (StartCode: $20D0;  StopCode: $20FF;  BlockName: 'Combining Marks for Symbols'),
      (StartCode: $2100;  StopCode: $214F;  BlockName: 'Letterlike Symbols'),
      (StartCode: $2150;  StopCode: $218F;  BlockName: 'Number Forms'),
      (StartCode: $2190;  StopCode: $21FF;  BlockName: 'Arrows'),
      (StartCode: $2200;  StopCode: $22FF;  BlockName: 'Mathematical Operators'),
      (StartCode: $2300;  StopCode: $23FF;  BlockName: 'Miscellaneous Technical'),
      (StartCode: $2400;  StopCode: $243F;  BlockName: 'Control Pictures'),
      (StartCode: $2440;  StopCode: $245F;  BlockName: 'Optical Character Recognition'),
      (StartCode: $2460;  StopCode: $24FF;  BlockName: 'Enclosed Alphanumerics'),
      (StartCode: $2500;  StopCode: $257F;  BlockName: 'Box Drawing'),
      (StartCode: $2580;  StopCode: $259F;  BlockName: 'Block Elements'),
      (StartCode: $25A0;  StopCode: $25FF;  BlockName: 'Geometric Shapes'),
      (StartCode: $2600;  StopCode: $26FF;  BlockName: 'Miscellaneous Symbols'),
      (StartCode: $2700;  StopCode: $27BF;  BlockName: 'Dingbats'),
      (StartCode: $2800;  StopCode: $28FF;  BlockName: 'Braille Patterns'),
      (StartCode: $2E80;  StopCode: $2EFF;  BlockName: 'CJK Radicals Supplement'),
      (StartCode: $2F00;  StopCode: $2FDF;  BlockName: 'Kangxi Radicals'),
      (StartCode: $2FF0;  StopCode: $2FFF;  BlockName: 'Ideographic Description Characters'),
      (StartCode: $3000;  StopCode: $303F;  BlockName: 'CJK Symbols and Punctuation'),
      (StartCode: $3040;  StopCode: $309F;  BlockName: 'Hiragana'),
      (StartCode: $30A0;  StopCode: $30FF;  BlockName: 'Katakana'),
      (StartCode: $3100;  StopCode: $312F;  BlockName: 'Bopomofo'),
      (StartCode: $3130;  StopCode: $318F;  BlockName: 'Hangul Compatibility Jamo'),
      (StartCode: $3190;  StopCode: $319F;  BlockName: 'Kanbun'),
      (StartCode: $31A0;  StopCode: $31BF;  BlockName: 'Bopomofo Extended'),
      (StartCode: $3200;  StopCode: $32FF;  BlockName: 'Enclosed CJK Letters and Months'),
      (StartCode: $3300;  StopCode: $33FF;  BlockName: 'CJK Compatibility'),
      (StartCode: $3400;  StopCode: $4DB5;  BlockName: 'CJK Unified Ideographs Extension A'),
      (StartCode: $4E00;  StopCode: $9FFF;  BlockName: 'CJK Unified Ideographs'),
      (StartCode: $A000;  StopCode: $A48F;  BlockName: 'Yi Syllables'),
      (StartCode: $A490;  StopCode: $A4CF;  BlockName: 'Yi Radicals'),
      (StartCode: $AC00;  StopCode: $D7A3;  BlockName: 'Hangul Syllables'),
      (StartCode: $D800;  StopCode: $DB7F;  BlockName: 'High Surrogates'),
      (StartCode: $DB80;  StopCode: $DBFF;  BlockName: 'High Private Use Surrogates'),
      (StartCode: $DC00;  StopCode: $DFFF;  BlockName: 'Low Surrogates'),
      (StartCode: $E000;  StopCode: $F8FF;  BlockName: 'Private Use'),
      (StartCode: $F900;  StopCode: $FAFF;  BlockName: 'CJK Compatibility Ideographs'),
      (StartCode: $FB00;  StopCode: $FB4F;  BlockName: 'Alphabetic Presentation Forms'),
      (StartCode: $FB50;  StopCode: $FDFF;  BlockName: 'Arabic Presentation Forms-A'),
      (StartCode: $FE20;  StopCode: $FE2F;  BlockName: 'Combining Half Marks'),
      (StartCode: $FE30;  StopCode: $FE4F;  BlockName: 'CJK Compatibility Forms'),
      (StartCode: $FE50;  StopCode: $FE6F;  BlockName: 'Small Form Variants'),
      (StartCode: $FE70;  StopCode: $FEFE;  BlockName: 'Arabic Presentation Forms-B'),
      (StartCode: $FEFF;  StopCode: $FEFF;  BlockName: 'Specials'),
      (StartCode: $FF00;  StopCode: $FFEF;  BlockName: 'Halfwidth and Fullwidth Forms'),
      (StartCode: $FFF0;  StopCode: $FFFF;  BlockName: 'Specials')
     );

  VAR
    UnicodeFilename:  STRING;

  // Normally from ftp://ftp.unicode.org/Public/UNIDATA/NamesList.Txt
  PROCEDURE LoadNamesListFile(CONST Filename:  STRING);
  PROCEDURE GetUnicodeInfo(CONST Unicode:  WORD;
                           VAR Block, Subblock:  STRING;
                           VAr InfoList:  TStringList);
  FUNCTION  UnicodeNamesString:  String;
  FUNCTION GetUnicodeBitmap(CONST UnicodeFont: string; CONST w:  WideChar; CONST BitmapSize:  INTEGER; VAR Size:  TSize; UnicodeColor : tColor):  TBitmap;
   (* Ciarán Ó Duibhín 2007/08/30 *)

IMPLEMENTATION

  USES
    SysUtils;  // StrToInt

  CONST
    TAB = #$09;

  TYPE
    TUnicodeIndex =  RECORD
        BlockName   :  INTEGER;
        SubBlockName:  INTEGER;
        SubBlockAdd :  INTEGER;
        Description :  INTEGER;
        Count       :  WORD;
      END;

  VAR
    UnicodeList    :  TStringList;             // Keep this local to unit
    UnicodeIndex   :  ARRAY OF TUnicodeIndex;  // indices into UnicodeList


  // Parse NamesList file into separate records for each Uniocde
  PROCEDURE SetUnicodeIndices;
    VAR
      count            :  INTEGER;
      i                :  INTEGER;
      IndexSubblockName:  INTEGER;
      IndexSubblockAdd :  INTEGER;
      s                :  STRING;
      Unicode          :  INTEGER;

      PROCEDURE SetBlockNameIndex(CONST i:  INTEGER; CONST s:  STRING);
        VAR
          BlockStart:  INTEGER;
          BlockStop :  INTEGER;
          index     :  INTEGER;
      BEGIN
        BlockStart := StrToInt('$' + COPY(s,1,4));
        BlockStop  := StrToInt('$' + COPY(s, LENGTH(s)-3, 4));
        FOR index := BlockStart TO BlockStop DO
          UnicodeIndex[index].BlockName := i
      END {SetBlockNameIndex};

  BEGIN
    count             :=  0;
    IndexSubblockName := -1;
    IndexSubblockAdd  := -1;
    Unicode           := -1;
    FOR i := 0 TO UnicodeList.Count-1 DO  BEGIN
      s := UnicodeList.Strings[i];
      IF  (LENGTH(s) > 0) THEN BEGIN
        CASE s[1] OF
          '0'..'9', 'A'..'F':
            BEGIN
              IF   Unicode >= 0
              THEN UnicodeIndex[Unicode].Count := Count;

              Unicode := StrToInt('$' + COPY(s,1,4));
              UnicodeIndex[Unicode].Description  := i;
              UnicodeIndex[Unicode].SubblockName := IndexSubblockName;
              UnicodeIndex[Unicode].SubBlockAdd  := IndexSubblockAdd;
              Count := 1;
            END;

          '@':
            BEGIN
              IF   Unicode >= 0
              THEN UnicodeIndex[Unicode].Count := Count;


              IF   COPY(s,1,4) = '@@@' + TAB
              THEN UnicodeFilename := COPY(s, 5, LENGTH(s)-4)
              ELSE
                IF   COPY(s,1,5) = '@@@+' + TAB
                THEN UnicodeFilename := UnicodeFilename + ', ' +
                                        COPY(s, 6, LENGTH(s)-5)
                ELSE
                  IF   COPY(s,1,3) = '@@' + TAB
                  THEN BEGIN
                    SetBlockNameIndex(i, COPY(s,4,LENGTH(s)-3));
                    IndexSubblockName := -1;
                    IndexSubblockAdd  := -1;
                  END
                  ELSE
                    IF  COPY(s,1,2) = '@' + TAB
                    THEN BEGIN
                      IndexSubblockName := i;
                      IndexSubblockAdd  := -1
                    END
                    ELSE
                      IF   COPY(s,1,3) = '@+' + TAB
                      THEN IndexSubblockAdd := i
            END;

          TAB:
            BEGIN
              INC(Count);
            END

        END
      END
    END
  END {SetUnicodeIndices};


  PROCEDURE LoadNamesListFile(CONST Filename: STRING);
    VAR
      s     :  STRING;
      Stream:  TMemoryStream;
  BEGIN
    Stream := TMemoryStream.Create;
    TRY
      Stream.LoadFromFile(Filename);
      SetLength(s, Stream.Size);
      Stream.Read(s[1], Stream.Size);
    FINALLY
      Stream.Free
    END;

    UnicodeList.Text := s;
    SetUnicodeIndices;
  END {LoadNamesListFile};


  PROCEDURE  GetUnicodeInfo(CONST Unicode:  WORD;
                            VAR Block, Subblock:  STRING;
                            VAR InfoList:  TStringList);
    VAR
      index:  INTEGER;
      j    :  INTEGER;
      s    :  STRING;
      t    :  STRING;
      Start:  INTEGER;
  BEGIN
    InfoList := TStringList.Create;   // calling program must free

    // Block can be defined even when nothing else is known
    IF   UnicodeIndex[Unicode].BlockName > 0   THEN BEGIN
      s := UnicodeList.Strings[ UnicodeIndex[Unicode].BlockName ];
      Delete(s, 1, 3);
      t := COPY(s, 1, 4) + '-';
      Delete(s, 1, 5);
      index := POS(TAB, s);
      t := t + COPY(s, index+1, LENGTH(s)-index) + ':  ';
      Delete(s, index, LENGTH(s)-index+1);
      Block := t + s;
    END
    ELSE Block := '<unknown>';

    // Subblock name or Unicode information may or may not exist
    IF   UnicodeIndex[Unicode].Count = 0
    THEN Subblock := ''
    ELSE BEGIN
      s := '';
      // May not be defined
      IF   UnicodeIndex[Unicode].SubblockName > 0
      THEN BEGIN
        s := UnicodeList.Strings[ UnicodeIndex[Unicode].SubblockName ];
        s := COPY(s, 4, LENGTH(s)-3)
      END;

      IF   UnicodeIndex[Unicode].SubblockAdd > 0  THEN BEGIN
        t := UnicodeList.Strings[ UnicodeIndex[Unicode].SubblockAdd ];
        t := COPY(t, 5, LENGTH(t)-4)
      END;
      Subblock := s + '; ' + t;

      Start := UnicodeIndex[Unicode].Description;
      FOR j := Start TO Start + UnicodeIndex[Unicode].Count - 1 DO  BEGIN
        s := UnicodeList.Strings[j];
        index := POS(TAB, s);
        InfoList.Add(COPY(s, index+1, LENGTH(s)-index) )
      END;

    END

  END {UnicodeInfo};


  FUNCTION UnicodeNamesString:  String;
  BEGIN
    RESULT := UnicodeList.Text
  END {UnicodeNamesString};


  FUNCTION GetUnicodeBitmap(CONST UnicodeFont: string; CONST w:  WideChar; CONST BitmapSize:  INTEGER; VAR Size:  TSize; UnicodeColor : tColor):  TBitmap;
  BEGIN
    RESULT := TBitmap.Create;
    RESULT.Height := BitmapSize;
    RESULT.Width  := BitmapSize;
    RESULT.Canvas.Brush.Color := clWhite;
    RESULT.Canvas.FillRect( RESULT.Canvas.ClipRect );

    RESULT.Canvas.Font.Name := UnicodeFont;
    RESULT.Canvas.Font.Height := BitmapSize;
    RESULT.Canvas.Font.Color := UnicodeColor;

    GetTextExtentPoint32W(RESULT.Canvas.Handle, @w, 1, Size);

    TextOutW(RESULT.Canvas.Handle,
            (RESULT.Width - size.cx) DIV 2, 0,   // center left-to-right
            @w, 1)
  END {GetUnicodeBitmap};



///////////////////////////////////////////////////////////////////////////

  PROCEDURE InitializeUnicodeIndices;
    VAR
      i:  INTEGER;
  BEGIN
    // Use -1 as default "not available"
    FOR i := Low(WORD) TO High(WORD) DO    BEGIN
      WITH UnicodeIndex[i] DO    BEGIN
        BlockName    := -1;
        SubblockName := -1;
        SubblockAdd  := -1;
        Description  := -1;
        Count        := 0
      END
    END
  END {InitializeUnicodeIndices};


INITIALIZATION
  UnicodeFilename := '';
  UnicodeList := TStringList.Create;
  SetLength(UnicodeIndex, High(WORD)+1);  // entries 0..High(WORD)
  InitializeUnicodeIndices;
FINALIZATION
  UnicodeList.Free
END.
