object SimpleContourOptions: TSimpleContourOptions
  Left = 725
  Top = 447
  Caption = 'Contour options'
  ClientHeight = 268
  ClientWidth = 222
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDesigned
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 160
    Width = 74
    Height = 13
    Caption = 'Contour interval'
  end
  object Label2: TLabel
    Left = 24
    Top = 200
    Width = 78
    Height = 13
    Caption = 'Max traingle size'
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 16
    Width = 97
    Height = 17
    Caption = 'Label points'
    TabOrder = 0
  end
  object CheckBox2: TCheckBox
    Left = 16
    Top = 32
    Width = 129
    Height = 17
    Caption = 'Color code points'
    TabOrder = 1
  end
  object CheckBox3: TCheckBox
    Left = 16
    Top = 48
    Width = 169
    Height = 17
    Caption = 'Show Delauney triangles'
    TabOrder = 2
  end
  object BitBtn1: TBitBtn
    Left = 110
    Top = 235
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 3
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 25
    Top = 81
    Width = 105
    Height = 25
    Caption = 'Contours'
    TabOrder = 4
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 24
    Top = 112
    Width = 105
    Height = 25
    Caption = 'Delaunay'
    TabOrder = 5
    OnClick = BitBtn3Click
  end
  object Edit1: TEdit
    Left = 110
    Top = 157
    Width = 80
    Height = 21
    TabOrder = 6
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 110
    Top = 197
    Width = 80
    Height = 21
    TabOrder = 7
    Text = 'Edit2'
  end
end
