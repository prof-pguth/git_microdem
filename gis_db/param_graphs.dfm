object ParamGraphForm: TParamGraphForm
  Left = 0
  Top = 0
  Caption = 'ParamGraphForm'
  ClientHeight = 148
  ClientWidth = 467
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  TextHeight = 13
  object Image1: TImage
    Left = 200
    Top = 16
    Width = 257
    Height = 129
  end
  object ComboBox1: TComboBox
    Left = 16
    Top = 16
    Width = 145
    Height = 21
    TabOrder = 0
    Text = 'ComboBox1'
    OnChange = ComboBox1Change
  end
  object BitBtn1: TBitBtn
    Left = 32
    Top = 56
    Width = 49
    Height = 25
    Caption = '<'
    TabOrder = 1
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 87
    Top = 56
    Width = 41
    Height = 25
    Caption = '>'
    TabOrder = 2
    OnClick = BitBtn2Click
  end
end
