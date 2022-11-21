inherited C: TC
  Left = 599
  Top = 327
  BorderIcons = []
  Caption = 'Chirps Options'
  ClientHeight = 223
  ClientWidth = 466
  OnClose = FormClose
  ExplicitWidth = 484
  ExplicitHeight = 270
  TextHeight = 20
  inherited Bevel1: TBevel
    Left = 231
    Top = 95
    Width = 41
    Height = 53
    ExplicitLeft = 231
    ExplicitTop = 95
    ExplicitWidth = 41
    ExplicitHeight = 53
  end
  object Label3: TLabel [1]
    Left = 16
    Top = 103
    Width = 52
    Height = 20
    Caption = 'Top (m)'
  end
  object Label4: TLabel [2]
    Left = 16
    Top = 130
    Width = 58
    Height = 20
    Caption = 'Base (m)'
  end
  object Label1: TLabel [3]
    Left = 16
    Top = 176
    Width = 198
    Height = 20
    Caption = 'Returns to show in each ping  '
  end
  inherited OKBtn: TButton
    Left = 347
    ExplicitLeft = 347
  end
  inherited CancelBtn: TButton
    Left = 347
    Top = 39
    ExplicitLeft = 347
    ExplicitTop = 39
  end
  object HelpBtn: TButton
    Left = 347
    Top = 70
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 2
    OnClick = HelpBtnClick
  end
  object Edit3: TEdit
    Left = 80
    Top = 95
    Width = 121
    Height = 28
    TabOrder = 3
    Text = 'Edit3'
  end
  object Edit4: TEdit
    Left = 80
    Top = 127
    Width = 121
    Height = 28
    TabOrder = 4
    Text = 'Edit4'
  end
  object RadioGroup2: TRadioGroup
    Left = 168
    Top = 8
    Width = 173
    Height = 81
    Caption = 'Trace graphs'
    Items.Strings = (
      'Intensity of return'
      'Grayscale value'
      'Both')
    TabOrder = 5
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 161
    Height = 81
    Caption = 'Chirps Display'
    ItemIndex = 2
    Items.Strings = (
      'Low frequency'
      'High frequency'
      'Color combination')
    TabOrder = 6
  end
  object Edit1: TEdit
    Left = 220
    Top = 173
    Width = 72
    Height = 28
    TabOrder = 7
    Text = 'Edit1'
  end
end
