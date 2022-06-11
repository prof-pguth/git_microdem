object GraphSettingsForm: TGraphSettingsForm
  Left = 534
  Top = 262
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Graph Modifications'
  ClientHeight = 424
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 16
    Top = 8
    Width = 345
    Height = 274
    Shape = bsFrame
    IsControl = True
  end
  object XMinLabel: TLabel
    Left = 24
    Top = 69
    Width = 31
    Height = 13
    Caption = 'Min x'
  end
  object XMaxLabel: TLabel
    Left = 24
    Top = 44
    Width = 38
    Height = 13
    Caption = 'Max x '
  end
  object YMinLabel: TLabel
    Left = 24
    Top = 164
    Width = 35
    Height = 13
    Caption = 'Min y '
  end
  object YMaxLabel: TLabel
    Left = 23
    Top = 131
    Width = 38
    Height = 13
    Caption = 'Max y '
  end
  object Label2: TLabel
    Left = 163
    Top = 20
    Width = 36
    Height = 13
    Caption = 'Month'
  end
  object Label3: TLabel
    Left = 241
    Top = 20
    Width = 31
    Height = 13
    Caption = 'Year '
  end
  object Day: TLabel
    Left = 88
    Top = 16
    Width = 23
    Height = 13
    Caption = 'Day'
  end
  object Colors: TLabel
    Left = 24
    Top = 200
    Width = 36
    Height = 13
    Caption = 'Colors'
    Visible = False
  end
  object Label1: TLabel
    Left = 80
    Top = 200
    Width = 31
    Height = 13
    Caption = 'Min z'
    Visible = False
  end
  object Label4: TLabel
    Left = 200
    Top = 200
    Width = 34
    Height = 13
    Caption = 'Max z'
    Visible = False
  end
  object Label5: TLabel
    Left = 23
    Top = 288
    Width = 64
    Height = 13
    Caption = 'Left margin'
  end
  object Label6: TLabel
    Left = 103
    Top = 288
    Width = 64
    Height = 13
    Caption = 'Top margin'
  end
  object Label7: TLabel
    Left = 191
    Top = 288
    Width = 81
    Height = 13
    Caption = 'Bottom margin'
  end
  object RedrawSpeedButton12: TSpeedButton
    Left = 312
    Top = 380
    Width = 37
    Height = 28
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
  object Label8: TLabel
    Left = 24
    Top = 344
    Width = 122
    Height = 13
    Caption = 'Lower left corner text'
  end
  object OKBtn: TBitBtn
    Left = 16
    Top = 380
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 4
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 123
    Top = 380
    Width = 77
    Height = 27
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 5
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 223
    Top = 380
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 6
    OnClick = HelpBtnClick
    IsControl = True
  end
  object XMinEdit: TEdit
    Left = 80
    Top = 65
    Width = 57
    Height = 21
    TabOrder = 0
  end
  object XMaxEdit: TEdit
    Left = 80
    Top = 39
    Width = 57
    Height = 21
    TabOrder = 1
  end
  object YMinEdit: TEdit
    Left = 80
    Top = 164
    Width = 57
    Height = 21
    TabOrder = 2
  end
  object YMaxEdit: TEdit
    Left = 80
    Top = 131
    Width = 57
    Height = 21
    TabOrder = 3
  end
  object XLabelEdit: TEdit
    Left = 20
    Top = 13
    Width = 118
    Height = 21
    TabOrder = 7
  end
  object YLabelEdit: TEdit
    Left = 20
    Top = 104
    Width = 117
    Height = 21
    TabOrder = 8
  end
  object Edit1: TEdit
    Left = 156
    Top = 39
    Width = 59
    Height = 21
    TabOrder = 9
  end
  object Edit2: TEdit
    Left = 156
    Top = 65
    Width = 59
    Height = 21
    TabOrder = 10
  end
  object Edit3: TEdit
    Left = 234
    Top = 39
    Width = 59
    Height = 21
    TabOrder = 11
  end
  object Edit4: TEdit
    Left = 234
    Top = 65
    Width = 59
    Height = 21
    TabOrder = 12
  end
  object BitBtn1: TBitBtn
    Left = 152
    Top = 96
    Width = 85
    Height = 27
    Caption = 'Grid Size'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      33333FFFFFFFFFFFFFFF000000000000000077777777777777770FFFFFFFFFFF
      FFF07F3FF3FF3FF3FFF70F00F00F00F000F07F773773773777370FFFFFFFFFFF
      FFF07F3FF3FF3FF3FFF70F00F00F00F000F07F773773773777370FFFFFFFFFFF
      FFF07F3FF3FF3FF3FFF70F00F00F00F000F07F773773773777370FFFFFFFFFFF
      FFF07F3FF3FF3FF3FFF70F00F00F00F000F07F773773773777370FFFFFFFFFFF
      FFF07FFFFFFFFFFFFFF70CCCCCCCCCCCCCC07777777777777777088CCCCCCCCC
      C8807FF7777777777FF700000000000000007777777777777777333333333333
      3333333333333333333333333333333333333333333333333333}
    NumGlyphs = 2
    TabOrder = 13
    OnClick = BitBtn1Click
  end
  object ComboBox1: TComboBox
    Left = 152
    Top = 129
    Width = 145
    Height = 21
    TabOrder = 14
    Items.Strings = (
      'Full Grid'
      'Part Grid'
      'No Grid'
      'X Full Grid Only'
      'X Part Grid Only'
      'X Time, Y Full Grid '
      'X Time, Y Part Grid ')
  end
  object ComboBox2: TComboBox
    Left = 152
    Top = 168
    Width = 145
    Height = 21
    TabOrder = 15
    Text = 'ComboBox2'
    Items.Strings = (
      'Cum normal (5-95%)'
      'Cum normal (1-99%)'
      'Cum normal (0.1--99.9%)'
      'Cum normal (0.01--99.99%)')
  end
  object CheckBox1: TCheckBox
    Left = 160
    Top = 168
    Width = 97
    Height = 17
    Caption = 'Annual cycle'
    TabOrder = 16
    Visible = False
  end
  object Edit5: TEdit
    Left = 128
    Top = 200
    Width = 57
    Height = 21
    TabOrder = 17
    Visible = False
  end
  object Edit6: TEdit
    Left = 240
    Top = 200
    Width = 57
    Height = 21
    TabOrder = 18
    Visible = False
  end
  object CheckBox2: TCheckBox
    Left = 31
    Top = 250
    Width = 129
    Height = 17
    Caption = 'Reverse y axis'
    TabOrder = 19
  end
  object Edit7: TEdit
    Left = 31
    Top = 304
    Width = 65
    Height = 21
    TabOrder = 20
  end
  object Edit8: TEdit
    Left = 111
    Top = 304
    Width = 57
    Height = 21
    TabOrder = 21
  end
  object Edit9: TEdit
    Left = 191
    Top = 304
    Width = 57
    Height = 21
    TabOrder = 22
  end
  object BitBtn2: TBitBtn
    Left = 284
    Top = 302
    Width = 65
    Height = 25
    Caption = 'Font'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333333333333333333333FFF33FFFFF33333300033000
      00333337773377777333333330333300033333337FF33777F333333330733300
      0333333377FFF777F33333333700000073333333777777773333333333033000
      3333333337FF777F333333333307300033333333377F777F3333333333703007
      33333333377F7773333333333330000333333333337777F33333333333300003
      33333333337777F3333333333337007333333333337777333333333333330033
      3333333333377333333333333333033333333333333733333333333333333333
      3333333333333333333333333333333333333333333333333333}
    NumGlyphs = 2
    TabOrder = 23
    OnClick = BitBtn2Click
  end
  object ComboBox3: TComboBox
    Left = 144
    Top = 32
    Width = 145
    Height = 21
    TabOrder = 24
    Text = 'ComboBox3'
    Visible = False
    OnChange = ComboBox3Change
  end
  object CheckBox3: TCheckBox
    Left = 195
    Top = 227
    Width = 97
    Height = 17
    Caption = '1:1 line'
    TabOrder = 25
  end
  object CheckBox4: TCheckBox
    Left = 304
    Top = 48
    Width = 45
    Height = 17
    Caption = 'Log'
    TabOrder = 26
  end
  object CheckBox5: TCheckBox
    Left = 31
    Top = 227
    Width = 109
    Height = 17
    Caption = 'Reverse x axis'
    TabOrder = 27
  end
  object CheckBox6: TCheckBox
    Left = 195
    Top = 250
    Width = 133
    Height = 17
    Caption = '1:1 x-y axis scaling'
    TabOrder = 28
  end
  object Edit10: TEdit
    Left = 152
    Top = 341
    Width = 236
    Height = 21
    TabOrder = 29
  end
end
