object regionsizeform: Tregionsizeform
  Left = 0
  Top = 0
  Caption = 'Region size graphs'
  ClientHeight = 203
  ClientWidth = 422
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
    Left = 216
    Top = 16
    Width = 70
    Height = 13
    Caption = 'Max range (m)'
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 185
    Height = 205
    Caption = 'Parameter'
    ItemIndex = 0
    Items.Strings = (
      'Relief'
      'Upward openness'
      'Downward openness'
      'Difference openness'
      'Summit'
      'Base Level'
      'Geophycisal Relief'
      'Dropoff'
      'Elevation relief'
      'Elevation moment'
      'Slope moment'
      'Plan curvature  moment'
      'Profile curvature moment')
    TabOrder = 0
    OnClick = RadioGroup1Click
  end
  object Edit1: TEdit
    Left = 306
    Top = 13
    Width = 65
    Height = 21
    TabOrder = 1
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object BitBtn1: TBitBtn
    Left = 211
    Top = 43
    Width = 75
    Height = 25
    Caption = 'Make graph'
    TabOrder = 2
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 211
    Top = 74
    Width = 75
    Height = 25
    Caption = 'New point'
    TabOrder = 3
    OnClick = BitBtn2Click
  end
  object RadioGroup2: TRadioGroup
    Left = 211
    Top = 105
    Width = 94
    Height = 105
    Caption = 'Moment'
    ItemIndex = 0
    Items.Strings = (
      'Average'
      'Std Dev'
      'Skewness'
      'Curtosis')
    TabOrder = 4
    OnClick = RadioGroup2Click
  end
end
