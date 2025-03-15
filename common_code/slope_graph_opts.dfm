object slopegraphopts: Tslopegraphopts
  Left = 792
  Top = 205
  BorderIcons = []
  Caption = 'Elevation/Slope Graph options'
  ClientHeight = 287
  ClientWidth = 356
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 207
    Top = 95
    Width = 55
    Height = 13
    Caption = 'Elev bin (m)'
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 9
    Width = 153
    Height = 17
    Caption = 'Elevation frequencies'
    TabOrder = 0
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 32
    Width = 201
    Height = 17
    Caption = 'Slope frequencies (percent)'
    TabOrder = 1
  end
  object CheckBox3: TCheckBox
    Left = 8
    Top = 78
    Width = 209
    Height = 17
    Caption = 'Elevation slope plot (%)'
    TabOrder = 2
  end
  object CheckBox4: TCheckBox
    Left = 8
    Top = 55
    Width = 169
    Height = 17
    Caption = 'Cumulative slope'
    TabOrder = 3
  end
  object CheckBox5: TCheckBox
    Left = 8
    Top = 165
    Width = 161
    Height = 17
    Caption = 'Aspect rose'
    TabOrder = 4
  end
  object CheckBox6: TCheckBox
    Left = 8
    Top = 123
    Width = 209
    Height = 17
    Caption = 'Elevation slope diagram (degrees)'
    TabOrder = 5
  end
  object CheckBox7: TCheckBox
    Left = 8
    Top = 188
    Width = 169
    Height = 17
    Caption = 'Text results'
    TabOrder = 6
  end
  object CheckBox8: TCheckBox
    Left = 8
    Top = 211
    Width = 217
    Height = 17
    Caption = 'Color legend'
    TabOrder = 7
  end
  object CheckBox9: TCheckBox
    Left = 24
    Top = 95
    Width = 177
    Height = 17
    Caption = 'Slope standard deviation'
    TabOrder = 8
  end
  object HelpBtn: TBitBtn
    Left = 118
    Top = 252
    Width = 83
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 9
    OnClick = HelpBtnClick
    IsControl = True
  end
  object OKBtn: TBitBtn
    Left = 24
    Top = 252
    Width = 73
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 10
    OnClick = OKBtnClick
    IsControl = True
  end
  object Edit1: TEdit
    Left = 268
    Top = 92
    Width = 54
    Height = 21
    TabOrder = 11
    Text = 'Edit1'
  end
  object CheckBox10: TCheckBox
    Left = 8
    Top = 146
    Width = 145
    Height = 17
    Caption = 'Elevation roughness'
    TabOrder = 12
  end
end
