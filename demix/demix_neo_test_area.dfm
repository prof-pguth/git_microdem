object New_area_evals_form: TNew_area_evals_form
  Left = 0
  Top = 0
  Caption = 'New_area_evals_form'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  TextHeight = 15
  object RadioGroup1: TRadioGroup
    Left = 24
    Top = 40
    Width = 185
    Height = 169
    Caption = 'Land surface parameter'
    ItemIndex = 0
    Items.Strings = (
      'Elevation'
      'Slope'
      'Hillshade'
      'RRI'
      'Openness')
    TabOrder = 0
    OnClick = RadioGroup1Click
  end
  object BitBtn1: TBitBtn
    Left = 280
    Top = 40
    Width = 201
    Height = 25
    Caption = 'Scatter plots compared to reference'
    TabOrder = 1
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 280
    Top = 80
    Width = 201
    Height = 25
    Caption = 'Histo'#8237'grams'
    TabOrder = 2
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 280
    Top = 120
    Width = 193
    Height = 25
    Caption = 'Difference map to reference DTM'
    TabOrder = 3
    OnClick = BitBtn3Click
  end
end
