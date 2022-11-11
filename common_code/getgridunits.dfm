object GetGridForm: TGetGridForm
  Left = 540
  Top = 301
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Units for DEM Grid'
  ClientHeight = 143
  ClientWidth = 276
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poOwnerFormCenter
  OnClose = FormClose
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 38
    Height = 13
    Caption = 'Latitude'
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 47
    Height = 13
    Caption = 'Longitude'
  end
  object Edit1: TEdit
    Left = 80
    Top = 8
    Width = 57
    Height = 21
    TabOrder = 0
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 152
    Top = 8
    Width = 57
    Height = 21
    TabOrder = 1
    OnChange = Edit2Change
  end
  object Edit3: TEdit
    Left = 225
    Top = 8
    Width = 49
    Height = 21
    TabOrder = 2
    OnChange = Edit3Change
  end
  object Edit4: TEdit
    Left = 80
    Top = 40
    Width = 57
    Height = 21
    TabOrder = 3
    OnChange = Edit4Change
  end
  object Edit5: TEdit
    Left = 152
    Top = 40
    Width = 57
    Height = 21
    TabOrder = 4
    OnChange = Edit5Change
  end
  object Edit6: TEdit
    Left = 224
    Top = 40
    Width = 49
    Height = 21
    TabOrder = 5
    OnChange = Edit6Change
  end
  object RadioGroup1: TRadioGroup
    Left = 80
    Top = 67
    Width = 193
    Height = 33
    Caption = 'Units'
    Columns = 3
    ItemIndex = 2
    Items.Strings = (
      'Degrees'
      'Minutes'
      'Seconds')
    TabOrder = 6
    OnClick = RadioGroup1Click
  end
  object BitBtn1: TBitBtn
    Left = 40
    Top = 119
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 7
    OnClick = BitBtn1Click
  end
end
