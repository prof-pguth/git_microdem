inherited SlopeCategoryEditor: TSlopeCategoryEditor
  Left = 792
  Top = 317
  BorderIcons = [biSystemMenu]
  Caption = 'Set Slope Categories'
  ClientHeight = 451
  ClientWidth = 392
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00BBBB
    BBBBB00000999999990001111111BBBBBBBBBBBBB00999999990001111110000
    BBBBBBBBBB00999999990011111133300BBBBBBBBBB009999998888888813333
    0000BBBBBBBB00999998FFFFFF813333333000BBBBBBB0099998F6666F813333
    3333300BBBBBBB009998F6666F810333333333000BBBBBB00098F6666F810000
    0033333300BBBBBBB008F6666F80AAAAA0033333300BBBBBBB08F6666F80AAAA
    AA03333333000BBBBBB8F6666F89AAAAAA003333333300BBBBB8F6666F898888
    88888888888888888888F6666F898FFFFFFFFFFFFFFFFFFFFFFFF6666F898EEE
    EEEEEEEEEEEEEEEEEEEEE6666F898EEEEEEEEEEEEEEEEEEEEEEEF6666F898FFF
    FFFFFFFFFFFFFFFFFFFFF6666F8988888888888888888888888FF6666F802222
    000AAAAAAAA00333333FF6666F8B22222200AAAAAAAA0033333FF6666F8B2222
    22200AAAAAAAA003333FF6666F8B2222222200AAAAAAAA03333FF666F88B0222
    22222000AAAAAA00333FF666F8BB00022222222000AAAAA0033FF666F8BBCC00
    02222222200AAAAA00FF6666F8BBCCCC002222222200AAAAA0FF6666F80BCCCC
    C00022222220AAAAAAFF6666F80B00CCCCC0002222200AAAAAFF666FF800400C
    CCCCC022222200AAAAFF666F88334400CCCCC00222222000AAFF666F83334440
    00CCCC002222222000FFFFFF83334444400CCCC00022222220AAAA0033330000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  StyleElements = [seFont, seClient, seBorder]
  ExplicitWidth = 408
  ExplicitHeight = 490
  TextHeight = 15
  inherited Bevel1: TBevel
    Width = 265
    Height = 425
    ExplicitWidth = 265
    ExplicitHeight = 425
  end
  object Image1: TImage [1]
    Left = 24
    Top = 48
    Width = 73
    Height = 361
    OnDblClick = Image1DblClick
    OnMouseMove = Image1MouseMove
  end
  object Label1: TLabel [2]
    Left = 288
    Top = 312
    Width = 56
    Height = 15
    Caption = 'Categories'
  end
  inherited OKBtn: TButton
    Left = 284
    ExplicitLeft = 284
  end
  inherited CancelBtn: TButton
    Left = 284
    ExplicitLeft = 284
  end
  object HelpBtn: TButton
    Left = 284
    Top = 68
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 2
    OnClick = HelpBtnClick
  end
  object Button1: TButton
    Left = 288
    Top = 248
    Width = 75
    Height = 25
    Caption = 'Defaults'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 288
    Top = 336
    Width = 57
    Height = 23
    TabOrder = 4
    OnChange = Edit1Change
  end
  object StringGrid2: TStringGrid
    Left = 192
    Top = 16
    Width = 73
    Height = 393
    ColCount = 1
    FixedCols = 0
    RowCount = 15
    ScrollBars = ssNone
    TabOrder = 5
    OnClick = StringGrid2Click
    OnMouseMove = StringGrid2MouseMove
  end
  object StringGrid1: TStringGrid
    Left = 112
    Top = 16
    Width = 73
    Height = 393
    ColCount = 1
    FixedCols = 0
    RowCount = 15
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    ScrollBars = ssNone
    TabOrder = 6
    OnClick = StringGrid1Click
    OnMouseMove = StringGrid1MouseMove
  end
  object Button2: TButton
    Left = 288
    Top = 280
    Width = 75
    Height = 25
    Caption = 'DEMIX'
    TabOrder = 7
    OnClick = Button2Click
  end
  object ColorDialog1: TColorDialog
    Left = 312
    Top = 152
  end
end
