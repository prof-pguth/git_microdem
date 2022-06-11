object StratOptsForm: TStratOptsForm
  Left = 350
  Top = 199
  Caption = 'Stratcol Options'
  ClientHeight = 388
  ClientWidth = 534
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox2: TGroupBox
    Left = 8
    Top = 104
    Width = 153
    Height = 169
    Caption = 'Drawing'
    TabOrder = 0
    object CheckBox4: TCheckBox
      Left = 8
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Boundary ticks'
      TabOrder = 0
    end
    object CheckBox5: TCheckBox
      Left = 8
      Top = 32
      Width = 129
      Height = 17
      Caption = 'Erosional right margin'
      TabOrder = 1
    end
    object CheckBox6: TCheckBox
      Left = 8
      Top = 48
      Width = 121
      Height = 17
      Caption = 'Variable resistance'
      TabOrder = 2
    end
    object CheckBox7: TCheckBox
      Left = 8
      Top = 64
      Width = 129
      Height = 17
      Caption = 'Overprint lithologies'
      TabOrder = 3
    end
    object CheckBox8: TCheckBox
      Left = 8
      Top = 80
      Width = 120
      Height = 17
      Caption = 'Complex unit bases'
      TabOrder = 4
    end
    object CheckBox12: TCheckBox
      Left = 8
      Top = 96
      Width = 97
      Height = 17
      Caption = 'Show age bar'
      TabOrder = 5
    end
    object CheckBox10: TCheckBox
      Left = 8
      Top = 112
      Width = 121
      Height = 17
      Caption = 'Column descriptions'
      TabOrder = 6
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 128
      Width = 137
      Height = 17
      Caption = 'Absolute value thickness'
      TabOrder = 7
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 144
      Width = 129
      Height = 17
      Caption = 'Right side thickness'
      TabOrder = 8
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 8
    Width = 153
    Height = 89
    Caption = 'Data entry'
    TabOrder = 1
    object CheckBox3: TCheckBox
      Left = 8
      Top = 16
      Width = 121
      Height = 17
      Caption = 'Automatic short labels'
      TabOrder = 0
    end
    object CheckBox9: TCheckBox
      Left = 8
      Top = 32
      Width = 97
      Height = 17
      Caption = 'Require lat/long'
      TabOrder = 1
    end
    object CheckBox11: TCheckBox
      Left = 8
      Top = 48
      Width = 97
      Height = 17
      Caption = 'Rapid entry'
      TabOrder = 2
    end
  end
  object RadioGroup1: TRadioGroup
    Left = 216
    Top = 8
    Width = 113
    Height = 33
    Caption = 'Thickness labels'
    Columns = 2
    Items.Strings = (
      'Meters'
      'Feet')
    TabOrder = 2
  end
  object RadioGroup2: TRadioGroup
    Left = 216
    Top = 48
    Width = 113
    Height = 33
    Caption = 'Thickness starts'
    Columns = 2
    Items.Strings = (
      'Top'
      'Base')
    TabOrder = 3
  end
  object GroupBox4: TGroupBox
    Left = 184
    Top = 224
    Width = 209
    Height = 153
    Caption = ' '
    TabOrder = 4
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 98
      Height = 13
      Caption = 'Column width (pixels)'
    end
    object Label2: TLabel
      Left = 24
      Top = 80
      Width = 102
      Height = 13
      Caption = 'Diagram width (pixels)'
    end
    object Label3: TLabel
      Left = 24
      Top = 104
      Width = 106
      Height = 13
      Caption = 'Diagram height (pixels)'
    end
    object Label4: TLabel
      Left = 8
      Top = 32
      Width = 122
      Height = 13
      Caption = 'Column separation (pixels)'
    end
    object Label5: TLabel
      Left = 24
      Top = 56
      Width = 99
      Height = 13
      Caption = 'Default thickness (m)'
    end
    object Label6: TLabel
      Left = 16
      Top = 128
      Width = 81
      Height = 13
      Caption = 'Scale label offset'
    end
    object Edit1: TEdit
      Left = 144
      Top = 8
      Width = 57
      Height = 21
      TabOrder = 0
    end
    object Edit2: TEdit
      Left = 144
      Top = 32
      Width = 57
      Height = 21
      TabOrder = 1
    end
    object Edit3: TEdit
      Left = 144
      Top = 56
      Width = 57
      Height = 21
      TabOrder = 2
    end
    object Edit4: TEdit
      Left = 144
      Top = 80
      Width = 57
      Height = 21
      TabOrder = 3
    end
    object Edit5: TEdit
      Left = 144
      Top = 104
      Width = 57
      Height = 21
      TabOrder = 4
    end
    object Edit6: TEdit
      Left = 144
      Top = 128
      Width = 57
      Height = 21
      TabOrder = 5
      Text = '0'
    end
  end
  object RadioGroup3: TRadioGroup
    Left = 344
    Top = 16
    Width = 161
    Height = 105
    Caption = 'Colors && patterns'
    Items.Strings = (
      'Colored Patterns'
      'Solid Colors'
      'Black and White Patterns'
      'No Patterns')
    TabOrder = 5
  end
  object OKBtn: TBitBtn
    Left = 422
    Top = 326
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 6
    OnClick = OKBtnClick
    IsControl = True
  end
  object RadioGroup4: TRadioGroup
    Left = 216
    Top = 88
    Width = 113
    Height = 49
    Caption = 'Text'
    Items.Strings = (
      'Horizontal'
      'Vertical')
    TabOrder = 7
  end
  object RadioGroup5: TRadioGroup
    Left = 216
    Top = 136
    Width = 113
    Height = 49
    Caption = 'Text'
    Items.Strings = (
      'Inside column'
      'Beside column')
    TabOrder = 8
  end
  object RadioGroup6: TRadioGroup
    Left = 344
    Top = 120
    Width = 161
    Height = 73
    Caption = 'Unit labels'
    Items.Strings = (
      'Short labels'
      'Long labels'
      'None')
    TabOrder = 9
  end
  object BitBtn1: TBitBtn
    Left = 424
    Top = 296
    Width = 75
    Height = 25
    Caption = 'Reset defaults'
    TabOrder = 10
    OnClick = BitBtn1Click
  end
  object RadioGroup7: TRadioGroup
    Left = 216
    Top = 184
    Width = 113
    Height = 33
    Caption = 'Align columns'
    Columns = 2
    Items.Strings = (
      'Top'
      'Base')
    TabOrder = 11
  end
  object RadioGroup8: TRadioGroup
    Left = 8
    Top = 288
    Width = 153
    Height = 81
    Caption = 'Column locations'
    Items.Strings = (
      'None'
      'Lat/long'
      'Literal')
    TabOrder = 12
  end
end
