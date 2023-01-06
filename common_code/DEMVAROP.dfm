inherited VariogramOptions: TVariogramOptions
  Left = 234
  Top = 421
  Caption = 'Variogram options'
  ClientHeight = 202
  ClientWidth = 480
  OnCreate = FormCreate
  ExplicitWidth = 498
  ExplicitHeight = 249
  TextHeight = 20
  inherited Bevel1: TBevel
    Left = 0
    Top = 4
    Width = 390
    Height = 205
    ExplicitLeft = 0
    ExplicitTop = 4
    ExplicitWidth = 390
    ExplicitHeight = 205
  end
  object Label1: TLabel [1]
    Left = 24
    Top = 88
    Width = 241
    Height = 20
    Caption = 'Grid spacing between sample points'
  end
  object Label2: TLabel [2]
    Left = 32
    Top = 112
    Width = 212
    Height = 20
    Caption = 'Sampling interval along profiles'
  end
  object Label3: TLabel [3]
    Left = 32
    Top = 136
    Width = 222
    Height = 20
    Caption = 'Maximum  distance to go out (m)'
  end
  object Label4: TLabel [4]
    Left = 32
    Top = 160
    Width = 168
    Height = 20
    Caption = 'Points required to use bin'
  end
  inherited OKBtn: TButton
    Left = 396
    ExplicitLeft = 396
  end
  inherited CancelBtn: TButton
    Left = 396
    ExplicitLeft = 396
  end
  object HelpBtn: TButton
    Left = 396
    Top = 68
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 2
    OnClick = HelpBtnClick
  end
  object CheckBox1: TCheckBox
    Left = 24
    Top = 28
    Width = 121
    Height = 17
    Caption = 'Log/log plot'
    TabOrder = 3
  end
  object CheckBox2: TCheckBox
    Left = 24
    Top = 44
    Width = 145
    Height = 17
    Caption = 'Semi variogram'
    TabOrder = 4
  end
  object Edit1: TEdit
    Left = 272
    Top = 85
    Width = 57
    Height = 28
    TabOrder = 5
  end
  object Edit2: TEdit
    Left = 272
    Top = 109
    Width = 57
    Height = 28
    TabOrder = 6
  end
  object Edit3: TEdit
    Left = 272
    Top = 133
    Width = 57
    Height = 28
    TabOrder = 7
  end
  object Edit4: TEdit
    Left = 272
    Top = 157
    Width = 57
    Height = 28
    TabOrder = 8
  end
  object CheckBox3: TCheckBox
    Left = 24
    Top = 12
    Width = 97
    Height = 17
    Caption = 'Graph'
    TabOrder = 9
  end
  object CheckBox4: TCheckBox
    Left = 184
    Top = 11
    Width = 185
    Height = 17
    Caption = 'Gamma computations'
    TabOrder = 10
  end
  object CheckBox5: TCheckBox
    Left = 184
    Top = 27
    Width = 145
    Height = 17
    Caption = 'Text output'
    TabOrder = 11
  end
  object CheckBox6: TCheckBox
    Left = 184
    Top = 43
    Width = 145
    Height = 17
    Caption = 'Equal spacing'
    TabOrder = 12
    OnClick = CheckBox6Click
  end
  object CheckBox7: TCheckBox
    Left = 184
    Top = 61
    Width = 185
    Height = 17
    Caption = 'Slope/fractal dimension'
    TabOrder = 13
  end
end
