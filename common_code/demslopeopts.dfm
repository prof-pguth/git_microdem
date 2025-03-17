object SlopeOptForm: TSlopeOptForm
  Left = 363
  Top = 242
  ActiveControl = OKBtn
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Slope Map Options'
  ClientHeight = 330
  ClientWidth = 597
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  TextHeight = 13
  object Label3: TLabel
    Left = 124
    Top = 10
    Width = 5
    Height = 13
  end
  object Image1: TImage
    Left = 240
    Top = 40
    Width = 100
    Height = 100
  end
  object Label6: TLabel
    Left = 135
    Top = 15
    Width = 39
    Height = 13
    Caption = 'Label6'
  end
  object Label1: TLabel
    Left = 5
    Top = 211
    Width = 79
    Height = 13
    Caption = 'Max slope (%)'
  end
  object RedrawSpeedButton12: TSpeedButton
    Left = 265
    Top = 284
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
  object ClipboardSpeedButton: TSpeedButton
    Left = 296
    Top = 284
    Width = 25
    Height = 25
    Hint = 'Copy map to clipboard'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFF0000000000FFFFF0FFFFFFFFFF0FFFF0FFFFFFFFFF0FFFF0F9FFFFFFF
      F0FFFF0FF9FFFFFFF0FF9999999FFFFFF0FF99999999FFFFF0FF99999999FFFF
      F0FF9999999FFFFFF0FFFF0FF9FFFFFFF0FFFF0F9FFFFFFFF0FFFF0FFFDDDDFF
      F0FFFFF000DDDD000FFFFFFFFDDFFDDFFFFFFFFFFFDDDDFFFFFF}
    OnClick = ClipboardSpeedButtonClick
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 284
    Width = 77
    Height = 27
    Caption = '&OK'
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 100
    Top = 284
    Width = 77
    Height = 27
    Caption = '&Cancel'
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 183
    Top = 284
    Width = 76
    Height = 27
    Caption = 'Help'
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    OnClick = HelpBtnClick
    IsControl = True
  end
  object Button11: TButton
    Left = 2
    Top = 10
    Width = 116
    Height = 25
    Caption = 'Slope Algorithm'
    TabOrder = 3
    OnClick = Button11Click
  end
  object Edit2: TEdit
    Left = 97
    Top = 208
    Width = 80
    Height = 21
    TabOrder = 4
    OnChange = Edit2Change
  end
  object CheckBox1: TCheckBox
    Left = 5
    Top = 258
    Width = 161
    Height = 17
    Caption = 'Immediate redraws'
    TabOrder = 5
    OnClick = CheckBox1Click
  end
  object RadioGroup2: TRadioGroup
    Left = 8
    Top = 41
    Width = 212
    Height = 50
    Caption = 'LSQ order'
    Columns = 4
    Items.Strings = (
      '1'
      '2'
      '3'
      '4')
    TabOrder = 6
    OnClick = RadioGroup2Click
  end
  object RadioGroup3: TRadioGroup
    Left = 8
    Top = 97
    Width = 212
    Height = 50
    Caption = 'LSQ window size'
    Columns = 4
    Items.Strings = (
      '3x3'
      '5x5'
      '7x7'
      '9x9')
    TabOrder = 7
    OnClick = RadioGroup3Click
  end
  object PageControl1: TPageControl
    Left = 384
    Top = 8
    Width = 201
    Height = 289
    ActivePage = TabSheet1
    TabOrder = 8
    object TabSheet1: TTabSheet
      Caption = 'Slope '
      object RadioGroup1: TRadioGroup
        Left = 3
        Top = 16
        Width = 169
        Height = 169
        Caption = 'Slope Colors'
        Items.Strings = (
          'Standard Categories'
          'Trafficability Categories'
          'Gray Scale'
          'Gray scale (reversed)'
          'Rainbow '
          'Pastel '
          'Go/NoGo ')
        TabOrder = 0
        OnClick = RadioGroup1Click
      end
      object BitBtn1: TBitBtn
        Left = 3
        Top = 203
        Width = 126
        Height = 25
        Caption = 'Slope colors'
        TabOrder = 1
        OnClick = BitBtn1Click
      end
      object BitBtn2: TBitBtn
        Left = 3
        Top = 233
        Width = 126
        Height = 25
        Caption = 'Create slope grid'
        TabOrder = 2
        OnClick = BitBtn2Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Aspect'
      ImageIndex = 1
      object RadioGroup4: TRadioGroup
        Left = 8
        Top = 8
        Width = 169
        Height = 201
        Caption = 'Aspect color options'
        Items.Strings = (
          '8 cardinal directions'
          'Terrain color scale'
          'Spectrum colors'
          'Rainbow colors'
          'CET-C1'
          'CET-C2'
          'CET-C6'
          'CET-C7')
        TabOrder = 0
        OnClick = RadioGroup4Click
      end
    end
  end
  object CheckBox2: TCheckBox
    Left = 5
    Top = 235
    Width = 239
    Height = 17
    Caption = 'No missing data in window allowed'
    TabOrder = 9
    OnClick = CheckBox2Click
  end
  object RadioGroup5: TRadioGroup
    Left = 8
    Top = 153
    Width = 212
    Height = 40
    Caption = 'Use windws points'
    Columns = 3
    Items.Strings = (
      'All'
      'Edge'
      'Queen'#39's')
    TabOrder = 10
  end
end
