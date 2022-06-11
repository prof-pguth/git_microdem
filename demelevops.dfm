inherited ElevOptionsForm: TElevOptionsForm
  Left = 663
  Top = 235
  Caption = 'Elevation map options'
  ClientHeight = 484
  ClientWidth = 528
  FormStyle = fsStayOnTop
  Position = poDefaultSizeOnly
  OnClose = FormClose
  OnCreate = FormCreate
  ExplicitWidth = 546
  ExplicitHeight = 531
  PixelsPerInch = 96
  TextHeight = 20
  inherited Bevel1: TBevel
    Left = 0
    Top = -5
    Width = 337
    Height = 441
    ExplicitLeft = 0
    ExplicitTop = -5
    ExplicitWidth = 337
    ExplicitHeight = 441
  end
  object Image1: TImage [1]
    Left = 200
    Top = 143
    Width = 100
    Height = 100
  end
  object Image2: TImage [2]
    Left = 343
    Top = 8
    Width = 35
    Height = 265
  end
  object RedrawSpeedButton12: TSpeedButton [3]
    Left = 256
    Top = 442
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
  inherited OKBtn: TButton
    Left = 4
    Top = 442
    OnClick = OKBtnClick
    ExplicitLeft = 4
    ExplicitTop = 442
  end
  inherited CancelBtn: TButton
    Left = 85
    Top = 442
    ExplicitLeft = 85
    ExplicitTop = 442
  end
  object HelpBtn: TButton
    Left = 166
    Top = 442
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 2
    OnClick = HelpBtnClick
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 3
    Width = 186
    Height = 253
    Caption = 'Display colors'
    Items.Strings = (
      'Terrain color scale'
      'Gray scale (monochrome)'
      'Alternate bands'
      'IHS colors'
      'Land/sea scales'
      'Spectrum colors'
      'Rainbow'
      'Fixed color palette'
      'Stretched color palette'
      'Contrast'
      'Grayscale reversed')
    TabOrder = 3
    OnClick = RadioGroup1Click
  end
  object CheckBox2: TCheckBox
    Left = 200
    Top = 3
    Width = 113
    Height = 17
    Caption = 'Ocean check'
    TabOrder = 4
  end
  object CheckBox3: TCheckBox
    Left = 200
    Top = 26
    Width = 100
    Height = 17
    Caption = 'Lake check'
    TabOrder = 5
  end
  object Button1: TButton
    Left = 200
    Top = 50
    Width = 75
    Height = 25
    Caption = 'IHS'
    TabOrder = 6
    OnClick = Button1Click
  end
  object BitBtn1: TBitBtn
    Left = 200
    Top = 81
    Width = 75
    Height = 25
    Caption = 'Missing'
    TabOrder = 7
    OnClick = BitBtn1Click
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 302
    Width = 281
    Height = 28
    TabOrder = 8
    OnChange = ComboBox1Change
  end
  object Button2: TButton
    Left = 200
    Top = 112
    Width = 75
    Height = 25
    Caption = 'z Range'
    TabOrder = 9
    OnClick = Button2Click
  end
  object BitBtn4: TBitBtn
    Left = 343
    Top = 342
    Width = 75
    Height = 25
    Caption = 'Show'
    TabOrder = 10
    OnClick = BitBtn4Click
  end
  object CheckBox1: TCheckBox
    Left = 343
    Top = 302
    Width = 154
    Height = 15
    Caption = 'Tables > 256 colors'
    TabOrder = 11
  end
  object RadioGroup3: TRadioGroup
    Left = 8
    Top = 336
    Width = 242
    Height = 64
    Caption = 'Grayscale (monochrome)'
    Columns = 7
    Enabled = False
    Items.Strings = (
      'Gr'
      'R '
      'G '
      'B '
      'C '
      'Y '
      'M')
    TabOrder = 12
    OnClick = RadioGroup3Click
  end
  object CheckBox4: TCheckBox
    Left = 256
    Top = 383
    Width = 66
    Height = 17
    Caption = 'Invert'
    Enabled = False
    TabOrder = 13
    OnClick = CheckBox4Click
  end
  object CheckBox5: TCheckBox
    Left = 256
    Top = 406
    Width = 66
    Height = 17
    Caption = 'Log10'
    TabOrder = 14
    OnClick = CheckBox5Click
  end
  object CheckBox6: TCheckBox
    Left = 79
    Top = 406
    Width = 158
    Height = 17
    Caption = 'Immediate redraws'
    TabOrder = 15
    OnClick = CheckBox6Click
  end
  object ComboBox2: TComboBox
    Left = 8
    Top = 262
    Width = 281
    Height = 28
    TabOrder = 16
    OnChange = ComboBox2Change
  end
end
