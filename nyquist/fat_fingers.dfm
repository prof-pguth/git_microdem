object fat_fingers_form: Tfat_fingers_form
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'Fat fingers'
  ClientHeight = 328
  ClientWidth = 690
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 215
    Width = 3
    Height = 15
    Caption = ' '
  end
  object Label2: TLabel
    Left = 8
    Top = 239
    Width = 3
    Height = 15
    Caption = ' '
  end
  object Label3: TLabel
    Left = 8
    Top = 263
    Width = 3
    Height = 15
    Caption = ' '
  end
  object SaveSpeedButton: TSpeedButton
    Left = 89
    Top = 288
    Width = 25
    Height = 25
    Hint = 'Save image'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      0400000000000001000000000000000000001000000010000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333330070
      7700333333337777777733333333008088003333333377F73377333333330088
      88003333333377FFFF7733333333000000003FFFFFFF77777777000000000000
      000077777777777777770FFFFFFF0FFFFFF07F3333337F3333370FFFFFFF0FFF
      FFF07F3FF3FF7FFFFFF70F00F0080CCC9CC07F773773777777770FFFFFFFF039
      99337F3FFFF3F7F777F30F0000F0F09999937F7777373777777F0FFFFFFFF999
      99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
      99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
      93337FFFF7737777733300000033333333337777773333333333}
    NumGlyphs = 2
    OnClick = SaveSpeedButtonClick
  end
  object Label4: TLabel
    Left = 8
    Top = 194
    Width = 3
    Height = 15
    Caption = ' '
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 185
    Height = 177
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 104
    Top = 319
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 1
    OnClick = BitBtn1Click
  end
  object StringGrid1: TStringGrid
    Left = 199
    Top = 8
    Width = 497
    Height = 321
    ColCount = 12
    DefaultColWidth = 40
    RowCount = 12
    TabOrder = 2
  end
  object BitBtn2: TBitBtn
    Left = 8
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Target'
    TabOrder = 3
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 8
    Top = 319
    Width = 75
    Height = 25
    Caption = 'Restart'
    TabOrder = 4
  end
end
