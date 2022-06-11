object PointClassForm: TPointClassForm
  Left = 699
  Top = 401
  BorderIcons = []
  Caption = 'Point classification map'
  ClientHeight = 519
  ClientWidth = 509
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  Position = poDesigned
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label7: TLabel
    Left = 32
    Top = 408
    Width = 53
    Height = 13
    Caption = 'Opacity (%)'
  end
  object RedrawSpeedButton12: TSpeedButton
    Left = 185
    Top = 402
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
  object Memo1: TMemo
    Left = 316
    Top = 0
    Width = 193
    Height = 519
    Align = alRight
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object HelpBtn: TBitBtn
    Left = 139
    Top = 490
    Width = 71
    Height = 25
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    OnClick = HelpBtnClick
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 8
    Top = 488
    Width = 77
    Height = 27
    Caption = 'Close'
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    OnClick = CancelBtnClick
    IsControl = True
  end
  object RadioGroup4: TRadioGroup
    Left = 24
    Top = 433
    Width = 199
    Height = 49
    Caption = 'Mask map'
    Columns = 3
    ItemIndex = 1
    Items.Strings = (
      'Separate'
      'Overlaid'
      'Both')
    TabOrder = 3
    OnClick = RadioGroup4Click
  end
  object Edit7: TEdit
    Left = 104
    Top = 406
    Width = 58
    Height = 21
    TabOrder = 4
    Text = 'Edit7'
    OnChange = Edit7Change
  end
  object PageControl1: TPageControl
    Left = -3
    Top = 0
    Width = 313
    Height = 393
    ActivePage = Tabsheet2
    TabOrder = 5
    object TabSheet1: TTabSheet
      Caption = 'Simple point classifications'
      object Label5: TLabel
        Left = 139
        Top = 325
        Width = 74
        Height = 13
        Caption = 'Mask radius (m)'
      end
      object Label6: TLabel
        Left = 24
        Top = 112
        Width = 52
        Height = 13
        Caption = 'Elev tol (m)'
      end
      object Label3: TLabel
        Left = 41
        Top = 83
        Width = 60
        Height = 13
        Caption = 'Window size'
      end
      object Label4: TLabel
        Left = 8
        Top = 60
        Width = 107
        Height = 13
        Caption = 'Min slope to classify ('#176')'
      end
      object Label2: TLabel
        Left = 8
        Top = 27
        Width = 93
        Height = 13
        Caption = 'Convexity tolerance'
      end
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 77
        Height = 13
        Caption = 'Slope tolerance '
      end
      object RadioGroup2: TRadioGroup
        Left = 8
        Top = 184
        Width = 206
        Height = 32
        Caption = 'Ridge/valley tolerance'
        Columns = 3
        Items.Strings = (
          '7'
          '6'
          '5')
        TabOrder = 0
      end
      object CheckBox1: TCheckBox
        Left = 7
        Top = 348
        Width = 76
        Height = 17
        Caption = 'Invert mask'
        TabOrder = 1
      end
      object CheckBox3: TCheckBox
        Left = 89
        Top = 348
        Width = 105
        Height = 17
        Caption = 'Exaggerate points'
        TabOrder = 2
      end
      object CheckBox4: TCheckBox
        Left = 200
        Top = 348
        Width = 65
        Height = 17
        Caption = 'Clear'
        TabOrder = 3
        OnClick = CheckBox4Click
      end
      object Edit5: TEdit
        Left = 219
        Top = 322
        Width = 46
        Height = 21
        TabOrder = 4
      end
      object RadioGroup3: TRadioGroup
        Left = 8
        Top = 284
        Width = 183
        Height = 37
        Caption = 'Analysis region'
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Entire DEM'
          'Current map')
        TabOrder = 5
        OnClick = RadioGroup3Click
      end
      object BitBtn6: TBitBtn
        Left = 168
        Top = 222
        Width = 73
        Height = 25
        Caption = 'All point map'
        TabOrder = 6
        OnClick = BitBtn6Click
      end
      object BitBtn3: TBitBtn
        Left = 88
        Top = 222
        Width = 74
        Height = 25
        Caption = 'Valley map'
        TabOrder = 7
        OnClick = BitBtn3Click
      end
      object BitBtn5: TBitBtn
        Left = 88
        Top = 253
        Width = 74
        Height = 25
        Caption = 'Valley mask'
        TabOrder = 8
        OnClick = BitBtn2Click
      end
      object BitBtn2: TBitBtn
        Left = 8
        Top = 252
        Width = 74
        Height = 25
        Caption = 'Ridge mask'
        TabOrder = 9
        OnClick = BitBtn2Click
      end
      object BitBtn4: TBitBtn
        Left = 8
        Top = 221
        Width = 74
        Height = 25
        Caption = 'Ridge map'
        TabOrder = 10
        OnClick = BitBtn4Click
      end
      object RadioGroup1: TRadioGroup
        Left = 8
        Top = 144
        Width = 206
        Height = 32
        Caption = 'Algorithm'
        Columns = 3
        ItemIndex = 0
        Items.Strings = (
          'Wood'
          'Simple'
          'Simple 2')
        TabOrder = 11
        OnClick = RadioGroup1Click
      end
      object Edit1: TEdit
        Left = 128
        Top = 8
        Width = 65
        Height = 21
        TabOrder = 12
      end
      object Edit2: TEdit
        Left = 128
        Top = 30
        Width = 65
        Height = 21
        TabOrder = 13
      end
      object Edit4: TEdit
        Left = 128
        Top = 57
        Width = 65
        Height = 21
        TabOrder = 14
      end
      object Edit3: TEdit
        Left = 126
        Top = 83
        Width = 65
        Height = 21
        TabOrder = 15
      end
      object Edit6: TEdit
        Left = 128
        Top = 112
        Width = 63
        Height = 21
        TabOrder = 16
      end
    end
    object Tabsheet2: TTabSheet
      Caption = 'Iwahishi && Pike'
      ImageIndex = 1
      object Label28: TLabel
        Left = 17
        Top = 86
        Width = 54
        Height = 13
        Caption = 'Roughness'
      end
      object Label27: TLabel
        Left = 22
        Top = 67
        Width = 46
        Height = 13
        Caption = 'Convexity'
      end
      object Label29: TLabel
        Left = 32
        Top = 48
        Width = 36
        Height = 13
        Caption = 'Slope 3'
      end
      object Label26: TLabel
        Left = 35
        Top = 27
        Width = 36
        Height = 13
        Caption = 'Slope 2'
      end
      object Label21: TLabel
        Left = 35
        Top = 8
        Width = 36
        Height = 13
        Caption = 'Slope 1'
      end
      object Label8: TLabel
        Left = 152
        Top = 152
        Width = 114
        Height = 13
        Caption = 'Rioughness box size (m)'
      end
      object Edit17: TEdit
        Left = 76
        Top = 86
        Width = 46
        Height = 21
        TabOrder = 0
        Text = 'Edit13'
      end
      object Edit16: TEdit
        Left = 76
        Top = 67
        Width = 46
        Height = 21
        TabOrder = 1
        Text = 'Edit13'
      end
      object Edit15: TEdit
        Left = 76
        Top = 43
        Width = 46
        Height = 21
        TabOrder = 2
        Text = 'Edit13'
      end
      object Edit14: TEdit
        Left = 76
        Top = 22
        Width = 46
        Height = 21
        TabOrder = 3
        Text = 'Edit13'
      end
      object Edit13: TEdit
        Left = 76
        Top = 3
        Width = 46
        Height = 21
        TabOrder = 4
        Text = 'Edit13'
      end
      object RadioGroup5: TRadioGroup
        Left = 17
        Top = 113
        Width = 105
        Height = 105
        Caption = 'Categories'
        Items.Strings = (
          '8'
          '12'
          '16')
        TabOrder = 5
        OnClick = RadioGroup5Click
      end
      object BitBtn1: TBitBtn
        Left = 152
        Top = 224
        Width = 97
        Height = 25
        Caption = 'Draw'
        TabOrder = 6
        OnClick = BitBtn1Click
      end
      object Edit8: TEdit
        Left = 209
        Top = 171
        Width = 57
        Height = 21
        TabOrder = 7
        Text = 'Edit8'
        OnChange = Edit8Change
      end
      object RadioGroup6: TRadioGroup
        Left = 152
        Top = 27
        Width = 137
        Height = 105
        Caption = 'Roughness measure'
        ItemIndex = 0
        Items.Strings = (
          'Slope std dev'
          'Directional cosines'
          '')
        TabOrder = 8
        OnClick = RadioGroup6Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Pennock'
      ImageIndex = 2
      object CheckBox2: TCheckBox
        Left = 24
        Top = 24
        Width = 97
        Height = 17
        Caption = 'Filter first'
        TabOrder = 0
      end
      object BitBtn7: TBitBtn
        Left = 31
        Top = 56
        Width = 217
        Height = 25
        Caption = 'WhiteBox PennockLandClassification'
        TabOrder = 1
        OnClick = BitBtn7Click
      end
    end
  end
end
