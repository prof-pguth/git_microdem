object PLSSform: TPLSSform
  Left = 432
  Top = 273
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'PLSS display options'
  ClientHeight = 367
  ClientWidth = 335
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 89
    Top = 187
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 125
    Top = 85
    Width = 3
    Height = 13
  end
  object Label3: TLabel
    Left = 89
    Top = 211
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label4: TLabel
    Left = 91
    Top = 242
    Width = 32
    Height = 13
    Caption = 'Label4'
  end
  object Label5: TLabel
    Left = 89
    Top = 273
    Width = 32
    Height = 13
    Caption = 'Label5'
  end
  object Label6: TLabel
    Left = 96
    Top = 304
    Width = 32
    Height = 13
    Caption = 'Label6'
  end
  object RedrawSpeedButton12: TSpeedButton
    Left = 191
    Top = 331
    Width = 25
    Height = 25
    Hint = 'Force redraw'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
      FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
      CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
      FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
    OnClick = RedrawSpeedButton12Click
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 16
    Width = 137
    Height = 17
    Caption = 'Show quarter sections'
    TabOrder = 0
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 32
    Width = 97
    Height = 17
    Caption = 'Show sections'
    TabOrder = 1
  end
  object CheckBox3: TCheckBox
    Left = 8
    Top = 48
    Width = 97
    Height = 17
    Caption = 'Show townships'
    TabOrder = 2
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 327
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 3
    OnClick = OKBtnClick
    IsControl = True
  end
  object CheckBox4: TCheckBox
    Left = 8
    Top = 80
    Width = 97
    Height = 17
    Caption = 'Label sections'
    TabOrder = 4
  end
  object CheckBox5: TCheckBox
    Left = 8
    Top = 96
    Width = 105
    Height = 17
    Caption = 'Label townships'
    TabOrder = 5
  end
  object BitBtn1: TBitBtn
    Left = 136
    Top = 8
    Width = 41
    Height = 25
    TabOrder = 6
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 136
    Top = 32
    Width = 41
    Height = 25
    TabOrder = 7
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 136
    Top = 56
    Width = 41
    Height = 25
    TabOrder = 8
    OnClick = BitBtn3Click
  end
  object Edit1: TEdit
    Left = 184
    Top = 8
    Width = 73
    Height = 21
    TabOrder = 9
  end
  object Edit2: TEdit
    Left = 184
    Top = 32
    Width = 73
    Height = 21
    TabOrder = 10
  end
  object Edit3: TEdit
    Left = 184
    Top = 56
    Width = 73
    Height = 21
    TabOrder = 11
  end
  object CheckBox6: TCheckBox
    Left = 190
    Top = 105
    Width = 90
    Height = 17
    Caption = 'Smart scaling'
    TabOrder = 12
  end
  object BitBtn4: TBitBtn
    Left = 8
    Top = 175
    Width = 75
    Height = 25
    Caption = 'PLSS 1'
    TabOrder = 13
    OnClick = BitBtn4Click
  end
  object CheckBox7: TCheckBox
    Left = 8
    Top = 112
    Width = 177
    Height = 17
    Caption = 'Township/Range/Section labels'
    TabOrder = 14
  end
  object CheckBox8: TCheckBox
    Left = 8
    Top = 128
    Width = 113
    Height = 17
    Caption = 'Quarters in labels'
    TabOrder = 15
  end
  object CheckBox9: TCheckBox
    Left = 191
    Top = 128
    Width = 97
    Height = 17
    Caption = 'Autoload PLSS'
    TabOrder = 16
  end
  object BitBtn5: TBitBtn
    Left = 91
    Top = 327
    Width = 77
    Height = 28
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 17
    OnClick = BitBtn5Click
    IsControl = True
  end
  object CheckBox10: TCheckBox
    Left = 8
    Top = 151
    Width = 97
    Height = 17
    Caption = 'Lots in labels'
    TabOrder = 18
  end
  object BitBtn6: TBitBtn
    Left = 8
    Top = 206
    Width = 75
    Height = 25
    Caption = 'PLSS 2'
    TabOrder = 19
    OnClick = BitBtn6Click
  end
  object BitBtn7: TBitBtn
    Left = 8
    Top = 237
    Width = 75
    Height = 25
    Caption = 'PLSS 3'
    TabOrder = 20
    OnClick = BitBtn7Click
  end
  object BitBtn8: TBitBtn
    Left = 8
    Top = 268
    Width = 75
    Height = 25
    Caption = 'PLSS 4'
    TabOrder = 21
    OnClick = BitBtn8Click
  end
  object BitBtn9: TBitBtn
    Left = 8
    Top = 296
    Width = 75
    Height = 25
    Caption = 'PLSS 5'
    TabOrder = 22
    OnClick = BitBtn9Click
  end
  object CheckBox11: TCheckBox
    Left = 192
    Top = 152
    Width = 97
    Height = 17
    Caption = 'PLSS to RAM'
    TabOrder = 23
  end
  object BitBtn10: TBitBtn
    Left = 266
    Top = 28
    Width = 50
    Height = 25
    Caption = 'Font'
    TabOrder = 24
    OnClick = BitBtn10Click
  end
  object BitBtn11: TBitBtn
    Left = 266
    Top = 59
    Width = 50
    Height = 25
    Caption = 'Font'
    TabOrder = 25
    OnClick = BitBtn11Click
  end
  object BitBtn12: TBitBtn
    Left = 241
    Top = 331
    Width = 75
    Height = 25
    Caption = 'Default'
    TabOrder = 26
    OnClick = BitBtn12Click
  end
end
