inherited ImportParamsDialog: TImportParamsDialog
  Left = 496
  Top = 193
  Caption = 'ASCII XYZ Import Parameters'
  ClientHeight = 355
  ClientWidth = 541
  Position = poDefaultSizeOnly
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  ExplicitWidth = 553
  ExplicitHeight = 393
  TextHeight = 15
  inherited Bevel1: TBevel
    Left = 7
    Width = 523
    Height = 302
    ExplicitLeft = 7
    ExplicitWidth = 523
    ExplicitHeight = 302
  end
  object Label1: TLabel [1]
    Left = 24
    Top = 192
    Width = 85
    Height = 15
    Caption = 'Z value multiple'
  end
  object Label2: TLabel [2]
    Left = 24
    Top = 241
    Width = 42
    Height = 15
    Caption = 'No data'
  end
  object Label3: TLabel [3]
    Left = 336
    Top = 224
    Width = 53
    Height = 15
    Caption = 'UTM zone'
  end
  object Label4: TLabel [4]
    Left = 376
    Top = 160
    Width = 57
    Height = 15
    Caption = 'Thin factor'
  end
  object Label5: TLabel [5]
    Left = 376
    Top = 248
    Width = 34
    Height = 15
    Caption = 'Label5'
  end
  inherited OKBtn: TButton
    Left = 152
    Top = 316
    ExplicitLeft = 152
    ExplicitTop = 316
  end
  inherited CancelBtn: TButton
    Left = 254
    Top = 316
    ExplicitLeft = 254
    ExplicitTop = 316
  end
  object HelpBtn: TButton
    Left = 357
    Top = 316
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 2
    OnClick = HelpBtnClick
  end
  object Memo1: TMemo
    Left = 224
    Top = 25
    Width = 281
    Height = 121
    ScrollBars = ssBoth
    TabOrder = 3
    WordWrap = False
  end
  object MultLong1: TCheckBox
    Left = 224
    Top = 168
    Width = 145
    Height = 17
    Caption = 'Multiply longitude by -1'
    TabOrder = 4
  end
  object ZMult1: TCheckBox
    Left = 224
    Top = 184
    Width = 97
    Height = 17
    Caption = 'Multiply z by -1'
    TabOrder = 5
    OnClick = ZMult1Click
  end
  object RadioGroup2: TRadioGroup
    Left = 24
    Top = 54
    Width = 177
    Height = 132
    Caption = 'XY Format'
    ItemIndex = 0
    Items.Strings = (
      'East-north (UTM)'
      'x-y rectangular'
      'Latitude-longitude'
      'Longitude-latitude'
      'Lat-long (dd-mm-sss)')
    TabOrder = 6
    OnClick = RadioGroup2Click
  end
  object CheckBox1: TCheckBox
    Left = 224
    Top = 200
    Width = 97
    Height = 17
    Caption = 'Plot raw data'
    TabOrder = 7
  end
  object ComboBox1: TComboBox
    Left = 24
    Top = 211
    Width = 81
    Height = 23
    TabOrder = 8
    Text = '1'
    OnChange = ComboBox1Change
    Items.Strings = (
      '0.0001'
      '0.001'
      '0.01'
      '0.1'
      '1'
      '10'
      '100'
      '1000')
  end
  object Edit1: TEdit
    Left = 72
    Top = 240
    Width = 90
    Height = 23
    TabOrder = 9
  end
  object CheckBox4: TCheckBox
    Left = 224
    Top = 152
    Width = 113
    Height = 17
    Caption = 'Elevations in feet'
    TabOrder = 10
  end
  object RadioGroup1: TRadioGroup
    Left = 191
    Top = 223
    Width = 145
    Height = 45
    Caption = 'Hemisphere'
    Columns = 2
    Items.Strings = (
      'North '
      'South')
    TabOrder = 11
    OnClick = RadioGroup1Click
  end
  object Edit2: TEdit
    Left = 426
    Top = 221
    Width = 48
    Height = 23
    TabOrder = 12
    Text = 'Edit2'
    OnChange = Edit2Change
  end
  object RadioGroup3: TRadioGroup
    Left = 16
    Top = 16
    Width = 202
    Height = 41
    Caption = 'Import'
    Columns = 3
    Items.Strings = (
      'Grid'
      'DB'
      'Both')
    TabOrder = 13
    OnClick = RadioGroup3Click
  end
  object Edit3: TEdit
    Left = 408
    Top = 179
    Width = 38
    Height = 23
    TabOrder = 14
    Text = '1'
    OnChange = Edit3Change
  end
end
