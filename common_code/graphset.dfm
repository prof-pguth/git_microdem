object GraphSettingsForm: TGraphSettingsForm
  Left = 534
  Top = 262
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Graph Modifications'
  ClientHeight = 452
  ClientWidth = 586
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  TextHeight = 13
  object Label2: TLabel
    Left = 371
    Top = 21
    Width = 36
    Height = 13
    Caption = 'Month'
  end
  object Label3: TLabel
    Left = 449
    Top = 21
    Width = 31
    Height = 13
    Caption = 'Year '
  end
  object Label5: TLabel
    Left = 15
    Top = 320
    Width = 64
    Height = 13
    Caption = 'Left margin'
  end
  object Label6: TLabel
    Left = 95
    Top = 320
    Width = 64
    Height = 13
    Caption = 'Top margin'
  end
  object Label7: TLabel
    Left = 183
    Top = 317
    Width = 81
    Height = 13
    Caption = 'Bottom margin'
  end
  object RedrawSpeedButton12: TSpeedButton
    Left = 298
    Top = 410
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
    Left = 16
    Top = 376
    Width = 122
    Height = 13
    Caption = 'Lower left corner text'
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 412
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 115
    Top = 412
    Width = 77
    Height = 27
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 215
    Top = 412
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    OnClick = HelpBtnClick
    IsControl = True
  end
  object Edit2: TEdit
    Left = 364
    Top = 66
    Width = 59
    Height = 21
    TabOrder = 3
  end
  object Edit3: TEdit
    Left = 442
    Top = 40
    Width = 59
    Height = 21
    TabOrder = 4
  end
  object Edit4: TEdit
    Left = 442
    Top = 66
    Width = 59
    Height = 21
    TabOrder = 5
  end
  object BitBtn1: TBitBtn
    Left = 364
    Top = 138
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
    TabOrder = 6
    OnClick = BitBtn1Click
  end
  object CheckBox1: TCheckBox
    Left = 371
    Top = 93
    Width = 97
    Height = 17
    Caption = 'Annual cycle'
    TabOrder = 7
    Visible = False
  end
  object Edit7: TEdit
    Left = 23
    Top = 336
    Width = 65
    Height = 21
    TabOrder = 8
  end
  object Edit8: TEdit
    Left = 103
    Top = 336
    Width = 57
    Height = 21
    TabOrder = 9
  end
  object Edit9: TEdit
    Left = 183
    Top = 336
    Width = 57
    Height = 21
    TabOrder = 10
  end
  object BitBtn2: TBitBtn
    Left = 276
    Top = 334
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
    TabOrder = 11
    OnClick = BitBtn2Click
  end
  object CheckBox3: TCheckBox
    Left = 352
    Top = 198
    Width = 97
    Height = 17
    Caption = '1:1 line'
    TabOrder = 12
  end
  object CheckBox6: TCheckBox
    Left = 432
    Top = 198
    Width = 133
    Height = 17
    Caption = '1:1 x-y axis scaling'
    TabOrder = 13
  end
  object Edit10: TEdit
    Left = 144
    Top = 373
    Width = 236
    Height = 21
    TabOrder = 14
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 230
    Width = 169
    Height = 73
    Caption = 'Form size'
    TabOrder = 15
    object Label9: TLabel
      Left = 16
      Top = 24
      Width = 34
      Height = 13
      Caption = 'Width'
    end
    object Label10: TLabel
      Left = 15
      Top = 43
      Width = 38
      Height = 13
      Caption = 'Height'
    end
    object Edit11: TEdit
      Left = 72
      Top = 16
      Width = 78
      Height = 21
      TabOrder = 0
      Text = 'Edit11'
    end
    object Edit12: TEdit
      Left = 72
      Top = 43
      Width = 77
      Height = 21
      TabOrder = 1
      Text = 'Edit12'
    end
  end
  object CheckBox7: TCheckBox
    Left = 424
    Top = 336
    Width = 97
    Height = 17
    Caption = 'Scroll graph'
    TabOrder = 16
    OnClick = CheckBox7Click
  end
  object Edit1: TEdit
    Left = 368
    Top = 40
    Width = 55
    Height = 21
    TabOrder = 17
    Text = 'Edit1'
  end
  object BitBtn3: TBitBtn
    Left = 356
    Top = 246
    Width = 153
    Height = 25
    Caption = 'Bigger point symbols'
    TabOrder = 18
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 356
    Top = 278
    Width = 153
    Height = 25
    Caption = 'Smaller point symbols'
    TabOrder = 19
    OnClick = BitBtn4Click
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 313
    Height = 185
    ActivePage = TabSheet1
    TabOrder = 20
    object TabSheet1: TTabSheet
      Caption = 'X axis'
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
      object CheckBox4: TCheckBox
        Left = 23
        Top = 88
        Width = 45
        Height = 17
        Caption = 'Log'
        TabOrder = 0
      end
      object XMinEdit: TEdit
        Left = 80
        Top = 65
        Width = 57
        Height = 21
        TabOrder = 1
      end
      object XMaxEdit: TEdit
        Left = 80
        Top = 39
        Width = 57
        Height = 21
        TabOrder = 2
      end
      object XLabelEdit: TEdit
        Left = 3
        Top = 12
        Width = 252
        Height = 21
        TabOrder = 3
      end
      object CheckBox5: TCheckBox
        Left = 23
        Top = 111
        Width = 109
        Height = 17
        Caption = 'Reverse x axis'
        TabOrder = 4
      end
      object ComboBox3: TComboBox
        Left = 43
        Top = 134
        Width = 145
        Height = 21
        TabOrder = 5
        Text = 'ComboBox3'
        Visible = False
        OnChange = ComboBox3Change
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Left Y axis'
      ImageIndex = 1
      object YMaxLabel: TLabel
        Left = 23
        Top = 40
        Width = 38
        Height = 13
        Caption = 'Max y '
      end
      object YMinLabel: TLabel
        Left = 23
        Top = 59
        Width = 35
        Height = 13
        Caption = 'Min y '
      end
      object YLabelEdit: TEdit
        Left = 3
        Top = 3
        Width = 252
        Height = 21
        TabOrder = 0
      end
      object YMaxEdit: TEdit
        Left = 67
        Top = 37
        Width = 57
        Height = 21
        TabOrder = 1
      end
      object YMinEdit: TEdit
        Left = 64
        Top = 64
        Width = 57
        Height = 21
        TabOrder = 2
      end
      object CheckBox2: TCheckBox
        Left = 20
        Top = 91
        Width = 129
        Height = 17
        Caption = 'Reverse y axis'
        TabOrder = 3
      end
      object ComboBox2: TComboBox
        Left = 19
        Top = 133
        Width = 145
        Height = 21
        TabOrder = 4
        Text = 'ComboBox2'
        Items.Strings = (
          'Cum normal (5-95%)'
          'Cum normal (1-99%)'
          'Cum normal (0.1--99.9%)'
          'Cum normal (0.01--99.99%)')
      end
      object CheckBox8: TCheckBox
        Left = 20
        Top = 110
        Width = 153
        Height = 17
        Caption = 'Y axis string labels'
        TabOrder = 5
        OnClick = CheckBox8Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Right Y axis'
      ImageIndex = 2
      object Label11: TLabel
        Left = 31
        Top = 48
        Width = 38
        Height = 13
        Caption = 'Max y '
      end
      object Label12: TLabel
        Left = 31
        Top = 67
        Width = 35
        Height = 13
        Caption = 'Min y '
      end
      object Edit13: TEdit
        Left = 11
        Top = 11
        Width = 252
        Height = 21
        TabOrder = 0
      end
      object Edit14: TEdit
        Left = 72
        Top = 72
        Width = 57
        Height = 21
        TabOrder = 1
      end
      object Edit15: TEdit
        Left = 75
        Top = 45
        Width = 57
        Height = 21
        TabOrder = 2
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Colors'
      ImageIndex = 3
      object Label4: TLabel
        Left = 28
        Top = 3
        Width = 34
        Height = 13
        Caption = 'Max z'
        Visible = False
      end
      object Label1: TLabel
        Left = 23
        Top = 30
        Width = 31
        Height = 13
        Caption = 'Min z'
        Visible = False
      end
      object Edit6: TEdit
        Left = 68
        Top = 0
        Width = 57
        Height = 21
        TabOrder = 0
        Visible = False
      end
      object Edit5: TEdit
        Left = 68
        Top = 27
        Width = 57
        Height = 21
        TabOrder = 1
        Visible = False
      end
    end
  end
  object ComboBox1: TComboBox
    Left = 364
    Top = 171
    Width = 145
    Height = 21
    TabOrder = 21
    Items.Strings = (
      'Full Grid'
      'Part Grid'
      'No Grid'
      'X Full Grid Only'
      'X Part Grid Only'
      'X Time, Y Full Grid '
      'X Time, Y Part Grid ')
  end
end
