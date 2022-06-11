object LidarMultipleDisplayForm: TLidarMultipleDisplayForm
  Left = 0
  Top = 0
  Caption = 'LidarMultipleDisplayForm'
  ClientHeight = 333
  ClientWidth = 506
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 360
    Top = 144
    Width = 59
    Height = 13
    Caption = 'Opacity (%)'
  end
  object RedrawSpeedButton12: TSpeedButton
    Left = 22
    Top = 300
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
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 145
    Height = 209
    Caption = 'Base map layer'
    ItemIndex = 0
    Items.Strings = (
      'DSM'
      'DTM'
      'NVS'
      'Lidar intensity'
      'Lidar classification'
      'RGB image'
      'CIR image'
      'CHM'
      'Change')
    TabOrder = 0
    OnClick = RadioGroup1Click
  end
  object RadioGroup2: TRadioGroup
    Left = 159
    Top = 8
    Width = 153
    Height = 105
    Caption = 'Elevation coloring'
    Items.Strings = (
      'Grayscale reflectance'
      'Slope reverse grayscale'
      'Openness'
      'IHS reflectance')
    TabOrder = 1
    OnClick = RadioGroup2Click
  end
  object CheckBox1: TCheckBox
    Left = 22
    Top = 254
    Width = 97
    Height = 17
    Caption = 'Grayscale base'
    TabOrder = 2
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 22
    Top = 277
    Width = 97
    Height = 17
    Caption = 'Subdue base'
    TabOrder = 3
    OnClick = CheckBox2Click
  end
  object BitBtn1: TBitBtn
    Left = 176
    Top = 201
    Width = 75
    Height = 25
    Caption = 'Openness'
    TabOrder = 4
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 257
    Top = 201
    Width = 75
    Height = 25
    Caption = 'Slope'
    TabOrder = 5
    OnClick = BitBtn2Click
  end
  object RadioGroup3: TRadioGroup
    Left = 352
    Top = 8
    Width = 122
    Height = 122
    Caption = 'Color layer'
    ItemIndex = 0
    Items.Strings = (
      'None'
      'Classification'
      'RGB'
      'CHM'
      'Ground only'
      'Change')
    TabOrder = 6
    OnClick = RadioGroup3Click
  end
  object RadioGroup4: TRadioGroup
    Left = 352
    Top = 201
    Width = 122
    Height = 105
    Caption = 'Terrain shadows'
    ItemIndex = 0
    Items.Strings = (
      'None'
      'DSM')
    TabOrder = 7
    OnClick = RadioGroup4Click
  end
  object Edit1: TEdit
    Left = 425
    Top = 136
    Width = 49
    Height = 21
    TabOrder = 8
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object CheckBox3: TCheckBox
    Left = 377
    Top = 163
    Width = 97
    Height = 17
    Caption = 'Colorize in place'
    TabOrder = 9
    OnClick = CheckBox3Click
  end
  object BitBtn3: TBitBtn
    Left = 176
    Top = 263
    Width = 75
    Height = 25
    Caption = '+ Change'
    TabOrder = 10
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 257
    Top = 263
    Width = 75
    Height = 25
    Caption = '- Change'
    TabOrder = 11
    OnClick = BitBtn4Click
  end
  object BitBtn5: TBitBtn
    Left = 8
    Top = 223
    Width = 75
    Height = 25
    Caption = 'Horiz bands'
    TabOrder = 12
    OnClick = BitBtn5Click
  end
  object BitBtn6: TBitBtn
    Left = 89
    Top = 223
    Width = 75
    Height = 25
    Caption = 'Vert bands'
    TabOrder = 13
    OnClick = BitBtn6Click
  end
  object BitBtn7: TBitBtn
    Left = 166
    Top = 119
    Width = 75
    Height = 25
    Caption = 'Horiz bands'
    TabOrder = 14
    OnClick = BitBtn7Click
  end
  object BitBtn8: TBitBtn
    Left = 247
    Top = 119
    Width = 75
    Height = 25
    Caption = 'Vert bands'
    TabOrder = 15
    OnClick = BitBtn8Click
  end
  object CheckBox4: TCheckBox
    Left = 360
    Top = 312
    Width = 97
    Height = 17
    Caption = 'NVS and DTM'
    TabOrder = 16
    OnClick = CheckBox4Click
  end
  object CheckBox5: TCheckBox
    Left = 235
    Top = 150
    Width = 97
    Height = 17
    Caption = 'Color allowed'
    TabOrder = 17
    OnClick = CheckBox5Click
  end
  object CheckBox6: TCheckBox
    Left = 67
    Top = 300
    Width = 97
    Height = 17
    Caption = 'All base options'
    TabOrder = 18
    OnClick = CheckBox6Click
  end
end
