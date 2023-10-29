object FanAlgParams: TFanAlgParams
  Left = 233
  Top = 155
  BorderStyle = bsDialog
  Caption = 'LOS/Fan Algorithm Options'
  ClientHeight = 497
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  TextHeight = 13
  object Label6: TLabel
    Left = 191
    Top = 75
    Width = 94
    Height = 13
    Caption = 'Ray Spacing ('#176'):'
  end
  object Label5: TLabel
    Left = 128
    Top = 16
    Width = 39
    Height = 13
    Caption = 'Label5'
  end
  object Label3: TLabel
    Left = 384
    Top = 280
    Width = 116
    Height = 13
    Caption = 'Switchover distance'
  end
  object Label4: TLabel
    Left = 23
    Top = 387
    Width = 171
    Height = 13
    Caption = 'Closest blocking distance (m) '
  end
  object Label8: TLabel
    Left = 232
    Top = 425
    Width = 92
    Height = 13
    Caption = 'Fan zoom factor'
  end
  object Edit6: TEdit
    Left = 291
    Top = 67
    Width = 54
    Height = 21
    TabOrder = 0
    Text = ' '
  end
  object RadioGroup2: TRadioGroup
    Left = 8
    Top = 40
    Width = 161
    Height = 73
    Caption = 'Fan drawing method'
    Items.Strings = (
      'Radial lines, discrete'
      'Point to point'
      'Radials, full coverage')
    TabOrder = 1
    OnClick = RadioGroup2Click
  end
  object Button2: TButton
    Left = 8
    Top = 8
    Width = 113
    Height = 25
    Caption = 'Vertical Curvature'
    TabOrder = 2
    OnClick = Button2Click
  end
  object RadioGroup3: TRadioGroup
    Left = 8
    Top = 146
    Width = 217
    Height = 76
    Caption = 'Point selection method'
    Items.Strings = (
      'Scaled radial spacing '
      'Constant radial spacing')
    TabOrder = 3
    OnClick = RadioGroup3Click
  end
  object CheckBox1: TCheckBox
    Left = 210
    Top = 100
    Width = 161
    Height = 17
    Caption = 'Viewer on grid node'
    TabOrder = 4
  end
  object CheckBox2: TCheckBox
    Left = 210
    Top = 123
    Width = 169
    Height = 17
    Caption = 'Target on grid node'
    TabOrder = 5
  end
  object Panel1: TPanel
    Left = 0
    Top = 456
    Width = 512
    Height = 41
    Align = alBottom
    TabOrder = 6
    ExplicitTop = 455
    ExplicitWidth = 508
    object OKBtn: TBitBtn
      Left = 8
      Top = 6
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
      Left = 95
      Top = 6
      Width = 78
      Height = 27
      Kind = bkCancel
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 1
      IsControl = True
    end
    object HelpBtn: TBitBtn
      Left = 183
      Top = 6
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
  end
  object RadioGroup4: TRadioGroup
    Left = 8
    Top = 240
    Width = 161
    Height = 129
    Caption = 'Point interpolation'
    TabOrder = 7
  end
  object RadioGroup5: TRadioGroup
    Left = 175
    Top = 240
    Width = 185
    Height = 105
    Caption = 'Horizontal earth curvature'
    TabOrder = 8
  end
  object Panel2: TPanel
    Left = 258
    Top = 146
    Width = 217
    Height = 88
    TabOrder = 9
    object Label2: TLabel
      Left = 17
      Top = 36
      Width = 120
      Height = 13
      Caption = 'Map spacing multiple'
    end
    object Label1: TLabel
      Left = 14
      Top = 12
      Width = 123
      Height = 13
      Caption = 'DEM spacing multiple'
    end
    object Label7: TLabel
      Left = 16
      Top = 63
      Width = 105
      Height = 13
      Caption = 'Point Spacing (m):'
    end
    object Edit1: TEdit
      Left = 144
      Top = 12
      Width = 57
      Height = 21
      TabOrder = 0
    end
    object Edit2: TEdit
      Left = 144
      Top = 36
      Width = 57
      Height = 21
      TabOrder = 1
    end
    object Edit7: TEdit
      Left = 147
      Top = 63
      Width = 54
      Height = 21
      TabOrder = 2
    end
  end
  object Edit3: TEdit
    Left = 400
    Top = 304
    Width = 81
    Height = 21
    TabOrder = 10
  end
  object BitBtn1: TBitBtn
    Left = 400
    Top = 344
    Width = 75
    Height = 25
    Caption = 'Defaults'
    TabOrder = 11
    OnClick = BitBtn1Click
  end
  object Edit4: TEdit
    Left = 200
    Top = 384
    Width = 81
    Height = 21
    TabOrder = 12
  end
  object CheckBox3: TCheckBox
    Left = 16
    Top = 424
    Width = 171
    Height = 17
    Caption = 'Missing data blocks LOS'
    TabOrder = 13
  end
  object CheckBox4: TCheckBox
    Left = 184
    Top = 44
    Width = 201
    Height = 17
    Caption = 'Radials from zoom map size'
    Enabled = False
    TabOrder = 14
  end
  object Edit5: TEdit
    Left = 336
    Top = 422
    Width = 81
    Height = 21
    TabOrder = 15
    Text = 'Edit5'
  end
end
