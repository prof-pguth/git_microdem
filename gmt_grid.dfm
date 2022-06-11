object gmt_form: Tgmt_form
  Left = 306
  Top = 174
  Caption = 'GMT gridding options'
  ClientHeight = 340
  ClientWidth = 537
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 176
    Top = 152
    Width = 38
    Height = 13
    Caption = 'Tension'
  end
  object Label2: TLabel
    Left = 16
    Top = 88
    Width = 22
    Height = 13
    Caption = 'xinc '
  end
  object Label3: TLabel
    Left = 16
    Top = 120
    Width = 22
    Height = 13
    Caption = 'yinc '
  end
  object Label4: TLabel
    Left = 200
    Top = 112
    Width = 3
    Height = 13
  end
  object Label5: TLabel
    Left = 280
    Top = 120
    Width = 3
    Height = 13
  end
  object Label6: TLabel
    Left = 312
    Top = 160
    Width = 57
    Height = 13
    Caption = 'Search Rad'
  end
  object Label7: TLabel
    Left = 464
    Top = 224
    Width = 57
    Height = 13
    Caption = 'Mask radius'
  end
  object Label8: TLabel
    Left = 152
    Top = 96
    Width = 20
    Height = 13
    Caption = 'ncol'
  end
  object Label9: TLabel
    Left = 152
    Top = 120
    Width = 23
    Height = 13
    Caption = 'nrow'
  end
  object Edit1: TEdit
    Left = 72
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 64
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'Edit2'
    OnChange = Edit2Change
  end
  object Edit3: TEdit
    Left = 0
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'Edit3'
    OnChange = Edit3Change
  end
  object Edit4: TEdit
    Left = 136
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'Edit4'
    OnChange = Edit4Change
  end
  object BitBtn1: TBitBtn
    Left = 48
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Preprocess'
    TabOrder = 4
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 168
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Grid'
    TabOrder = 5
    OnClick = BitBtn2Click
  end
  object RadioGroup1: TRadioGroup
    Left = 32
    Top = 192
    Width = 121
    Height = 81
    Caption = 'Preprocessor'
    ItemIndex = 1
    Items.Strings = (
      'Mean'
      'Median'
      'Mode'
      'None')
    TabOrder = 6
    OnClick = RadioGroup1Click
  end
  object Edit5: TEdit
    Left = 64
    Top = 88
    Width = 73
    Height = 21
    TabOrder = 7
    Text = '0.00001'
    OnChange = Edit5Change
  end
  object BitBtn3: TBitBtn
    Left = 280
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Display GRD'
    Enabled = False
    TabOrder = 8
    OnClick = BitBtn3Click
  end
  object Edit6: TEdit
    Left = 224
    Top = 152
    Width = 65
    Height = 21
    TabOrder = 9
    Text = '0.35'
  end
  object Edit7: TEdit
    Left = 64
    Top = 120
    Width = 73
    Height = 21
    Enabled = False
    TabOrder = 10
    Text = '0.00001'
    OnChange = Edit7Change
  end
  object CheckBox1: TCheckBox
    Left = 320
    Top = 88
    Width = 137
    Height = 17
    Caption = 'Equal x and y spacing'
    Checked = True
    State = cbChecked
    TabOrder = 11
    OnClick = CheckBox1Click
  end
  object RadioGroup2: TRadioGroup
    Left = 184
    Top = 200
    Width = 177
    Height = 73
    Caption = 'Grid method'
    ItemIndex = 0
    Items.Strings = (
      'Continuous curvature gridding '
      'Optimal Delauney triangulation '
      'Nearest neighbor')
    TabOrder = 12
    OnClick = RadioGroup2Click
  end
  object CheckBox2: TCheckBox
    Left = 288
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Verbose'
    Checked = True
    State = cbChecked
    TabOrder = 13
  end
  object Edit8: TEdit
    Left = 376
    Top = 152
    Width = 73
    Height = 21
    TabOrder = 14
    Text = '0.00002'
  end
  object RadioGroup3: TRadioGroup
    Left = 368
    Top = 200
    Width = 81
    Height = 73
    Caption = 'Search'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemIndex = 1
    Items.Strings = (
      'Quadrant'
      'Octant')
    ParentFont = False
    TabOrder = 15
  end
  object RadioGroup4: TRadioGroup
    Left = 24
    Top = 144
    Width = 129
    Height = 41
    Columns = 3
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ItemIndex = 0
    Items.Strings = (
      #176
      #39
      '"')
    ParentFont = False
    TabOrder = 16
  end
  object BitBtn4: TBitBtn
    Left = 392
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Mask'
    Enabled = False
    TabOrder = 17
    OnClick = BitBtn4Click
  end
  object Edit9: TEdit
    Left = 472
    Top = 240
    Width = 49
    Height = 21
    TabOrder = 18
    Text = '2'
    OnChange = Edit9Change
  end
  object Edit10: TEdit
    Left = 192
    Top = 88
    Width = 65
    Height = 21
    TabOrder = 19
    Text = 'Edit10'
  end
  object Edit11: TEdit
    Left = 192
    Top = 120
    Width = 65
    Height = 21
    TabOrder = 20
    Text = 'Edit11'
  end
end
