object SlicerForm: TSlicerForm
  Left = 0
  Top = 0
  ClientHeight = 211
  ClientWidth = 808
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object BitBtn1: TBitBtn
    Left = 8
    Top = 153
    Width = 38
    Height = 25
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
      FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
      CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
      FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object RadioGroup1: TRadioGroup
    Left = 581
    Top = 8
    Width = 57
    Height = 105
    Caption = 'Plane'
    ItemIndex = 0
    Items.Strings = (
      'x-y'
      'x-z'
      'y-z')
    TabOrder = 1
    OnClick = RadioGroup1Click
  end
  object BitBtn2: TBitBtn
    Left = 52
    Top = 154
    Width = 50
    Height = 25
    Glyph.Data = {
      8A000000424D8A000000000000003E0000002800000020000000130000000100
      0100000000004C000000C40E0000C40E0000020000000000000000000000FFFF
      FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE003FFFF00707FFC0DFF9FF81DF
      FF7F02555400075855C30715D583074645F3077FFCD300FFFE1383FFFFFFC0FF
      F9FFF00207FFFE003FFFFFFFFFFF}
    TabOrder = 2
    OnClick = BitBtn2Click
  end
  object HelpBtn: TBitBtn
    Left = 222
    Top = 153
    Width = 48
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 3
    OnClick = HelpBtnClick
    IsControl = True
  end
  object RadioGroup2: TRadioGroup
    Left = 644
    Top = 8
    Width = 50
    Height = 103
    Caption = 'Graph'
    Items.Strings = (
      '1'
      '2'
      '3'
      '4')
    TabOrder = 4
    OnClick = RadioGroup2Click
  end
  object BitBtn16: TBitBtn
    Left = 108
    Top = 153
    Width = 61
    Height = 25
    Caption = 'Map pick'
    TabOrder = 5
    OnClick = BitBtn16Click
  end
  object BitBtn11: TBitBtn
    Left = 492
    Top = 193
    Width = 70
    Height = 25
    Caption = 'Trans && Rot'
    TabOrder = 6
    OnClick = BitBtn11Click
  end
  object RadioGroup4: TRadioGroup
    Left = 286
    Top = 145
    Width = 181
    Height = 33
    Caption = 'Slice thinning'
    Columns = 5
    ItemIndex = 0
    Items.Strings = (
      '1'
      '2'
      '5'
      '10'
      '25')
    TabOrder = 7
    OnClick = RadioGroup4Click
  end
  object CloudPickGroupBox1: TGroupBox
    Left = 239
    Top = 8
    Width = 210
    Height = 131
    Caption = 'Cloud'
    TabOrder = 8
    object CheckBoxCloud1: TCheckBox
      Left = 9
      Top = 15
      Width = 169
      Height = 17
      Caption = 'CheckBoxCloud1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = CheckBoxCloud1Click
    end
    object CheckBoxCloud2: TCheckBox
      Left = 11
      Top = 38
      Width = 169
      Height = 17
      Caption = 'CheckBoxCloud2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = CheckBoxCloud2Click
    end
    object CheckBoxCloud3: TCheckBox
      Left = 11
      Top = 61
      Width = 174
      Height = 17
      Caption = 'CheckBoxCloud3'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = CheckBoxCloud3Click
    end
    object CheckBoxCloud4: TCheckBox
      Left = 11
      Top = 84
      Width = 166
      Height = 19
      Caption = 'CheckBoxCloud4'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = CheckBoxCloud4Click
    end
    object CheckBoxCloud5: TCheckBox
      Left = 11
      Top = 109
      Width = 126
      Height = 17
      Caption = 'CheckBoxCloud5'
      TabOrder = 4
      OnClick = CheckBoxCloud5Click
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 146
    Height = 115
    Caption = 'Slice parameters'
    TabOrder = 9
    object Label2: TLabel
      Left = 3
      Top = 18
      Width = 31
      Height = 13
      Caption = 'Label2'
    end
    object Label5: TLabel
      Left = 3
      Top = 41
      Width = 33
      Height = 13
      Caption = 'Center'
    end
    object Label1: TLabel
      Left = 3
      Top = 65
      Width = 43
      Height = 13
      Caption = 'Thick (m)'
    end
    object Edit1: TEdit
      Left = 64
      Top = 62
      Width = 68
      Height = 21
      TabOrder = 0
      Text = '2'
      OnChange = Edit1Change
    end
    object Edit9: TEdit
      Left = 64
      Top = 35
      Width = 68
      Height = 21
      TabOrder = 1
      OnChange = Edit9Change
    end
    object CheckBox7: TCheckBox
      Left = 8
      Top = 84
      Width = 82
      Height = 17
      Caption = 'Label slices'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object GroupBox2: TGroupBox
    Left = 160
    Top = 8
    Width = 73
    Height = 115
    Caption = 'Slide in/out'
    TabOrder = 10
    object SpeedButton1: TSpeedButton
      Left = 17
      Top = 48
      Width = 32
      Height = 30
      Glyph.Data = {
        E6010000424DE601000000000000760000002800000019000000170000000100
        0400000000007001000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFF0000000FFFFFFFFF000000FFFFFFFFFF0000000FFFFFFF00000
        0000FFFFFFFFF0000000FFFFF0000FFFFFF000FFFFFFF0000000FFF0000FFFFF
        FFFFF00FFFFFF0000000FFF0000FFFFFFFFFF000FFFFF0000000FFF00000FFFF
        FFFF00000FFFF0000000FF0FF0000FFFFFF0000000FFF0000000FF0FFF0000FF
        FF0000FF00FFF0000000F0FFFFF0000FF0000FFFF00FF0000000F0FFFFFF0000
        0000FFFFF00FF0000000F0FFFFFFF000000FFFFFF00FF0000000F0FFFFFFF000
        000FFFFFF00FF0000000F0FFFFFF00000000FFFFF00FF0000000F0FFFFF0000F
        F0000FFFF00FF0000000FF0FFF0000FFFF0000FF00FFF0000000FF0FF0000FFF
        FFF0000000FFF0000000FFF00000FFFFFFFF00000FFFF0000000FFF0000FFFFF
        FFFFF0000FFFF0000000FFFF000FFFFFFFFFF0000FFFF0000000FFFFF0000FFF
        FFF000FFFFFFF0000000FFFFFFF0F0000000FFFFFFFFF0000000FFFFFFFF0000
        000FFFFFFFFFF0000000}
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 17
      Top = 18
      Width = 32
      Height = 30
      Glyph.Data = {
        8A010000424D8A01000000000000760000002800000018000000170000000100
        0400000000001401000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFF0000000000FFFFFFFFFFFFF000000000000FFFFFFFFFF
        00000FFFFFF00000FFFFFFF0000FFFFFFFFFF0000FFFFFF000FFFFFFFFFFFF00
        00FFFF000FFFFFFFFFFFFFF000FFF000FFFFFFFFFFFFFFFF000FF000FFFFFF00
        0FFFFFFF000FF00FFFFFF00000FFFFFFF00FF00FFFFF0000000FFFFFF00FF00F
        FFFF0000000FFFFFF00FF00FFFFF0000000FFFFFF00FF00FFFFFF00000FFFFFF
        F00FF000FFFFFF000FFFFFFF000FF0000FFFFFFFFFFFFFFF000FFF000FFFFFFF
        FFFFFFF000FFFFF000FFFFFFFFFFFF0000FFFFF00000FFFFFFFFF0000FFFFFFF
        00000FFFFFF00000FFFFFFFFFF000000000000FFFFFFFFFFFFFFF000000FFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      OnClick = SpeedButton2Click
    end
    object Edit10: TEdit
      Left = 3
      Top = 84
      Width = 54
      Height = 21
      TabOrder = 0
      Text = 'Edit10'
      OnChange = Edit10Change
    end
  end
  object GroupBox3: TGroupBox
    Left = 700
    Top = 8
    Width = 93
    Height = 121
    Caption = 'Graph size'
    TabOrder = 11
    object Wudth: TLabel
      Left = 3
      Top = 16
      Width = 28
      Height = 13
      Caption = 'Width'
    end
    object Label4: TLabel
      Left = 6
      Top = 44
      Width = 31
      Height = 13
      Caption = 'Height'
    end
    object Edit2: TEdit
      Left = 45
      Top = 17
      Width = 37
      Height = 21
      TabOrder = 0
      Text = '1200'
    end
    object Edit4: TEdit
      Left = 43
      Top = 44
      Width = 38
      Height = 21
      TabOrder = 1
      Text = '1200'
    end
    object CheckBoxCorrectScaling1: TCheckBox
      Left = 8
      Top = 72
      Width = 97
      Height = 17
      Caption = '1:1 scaling'
      TabOrder = 2
      OnClick = CheckBoxCorrectScaling1Click
    end
  end
  object RadioGroup3: TRadioGroup
    Left = 455
    Top = 8
    Width = 120
    Height = 105
    Caption = 'Display parameter'
    TabOrder = 12
    OnClick = RadioGroup3Click
  end
  object BitBtn42: TBitBtn
    Left = 175
    Top = 153
    Width = 41
    Height = 25
    Caption = 'Polar '
    TabOrder = 13
    OnClick = BitBtn42Click
  end
  object BitBtn43: TBitBtn
    Left = 249
    Top = 193
    Width = 75
    Height = 25
    Caption = 'Box stats'
    TabOrder = 14
    OnClick = BitBtn43Click
  end
  object BitBtn44: TBitBtn
    Left = 330
    Top = 193
    Width = 75
    Height = 25
    Caption = 'Canopy'
    TabOrder = 15
    OnClick = BitBtn44Click
  end
  object BitBtn45: TBitBtn
    Left = 411
    Top = 193
    Width = 75
    Height = 25
    Caption = 'Global DEMs'
    TabOrder = 16
    OnClick = BitBtn45Click
  end
end
