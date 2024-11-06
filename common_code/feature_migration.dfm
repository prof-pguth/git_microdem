object FeatureMigrationForm: TFeatureMigrationForm
  Left = 0
  Top = 0
  ClientHeight = 343
  ClientWidth = 629
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 160
    Width = 52
    Height = 13
    Caption = 'Feature ID'
  end
  object Label2: TLabel
    Left = 112
    Top = 208
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 112
    Top = 237
    Width = 31
    Height = 13
    Caption = 'Label3'
  end
  object Edit1: TEdit
    Left = 128
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '15'
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 128
    Top = 88
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '0'
    OnChange = Edit2Change
  end
  object Edit3: TEdit
    Left = 32
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '0'
    OnChange = Edit3Change
  end
  object Edit4: TEdit
    Left = 191
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '30'
    OnChange = Edit4Change
  end
  object Edit5: TEdit
    Left = 90
    Top = 157
    Width = 121
    Height = 21
    TabOrder = 4
    Text = '5'
    OnChange = Edit5Change
  end
  object Memo1: TMemo
    Left = 394
    Top = 0
    Width = 235
    Height = 343
    Align = alRight
    Lines.Strings = (
      'Memo1')
    TabOrder = 5
  end
  object BitBtn1: TBitBtn
    Left = 24
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Original'
    TabOrder = 6
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 24
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Migrated'
    TabOrder = 7
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 152
    Top = 280
    Width = 97
    Height = 25
    Caption = 'Single feature'
    TabOrder = 8
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 152
    Top = 311
    Width = 97
    Height = 25
    Caption = 'All features'
    TabOrder = 9
    OnClick = BitBtn4Click
  end
  object BitBtn5: TBitBtn
    Left = 255
    Top = 308
    Width = 114
    Height = 25
    Caption = 'Both directions (all)'
    TabOrder = 10
    OnClick = BitBtn5Click
  end
end
