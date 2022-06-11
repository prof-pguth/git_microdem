object NetQuizForm: TNetQuizForm
  Left = 0
  Top = 0
  Caption = 'Stereo net quiz'
  ClientHeight = 229
  ClientWidth = 439
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 24
    Width = 65
    Height = 13
    Caption = 'Dip and strike'
  end
  object Label2: TLabel
    Left = 19
    Top = 208
    Width = 3
    Height = 13
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Edit1: TEdit
    Left = 84
    Top = 24
    Width = 89
    Height = 21
    TabOrder = 0
    OnChange = Edit1Change
  end
  object BitBtn1: TBitBtn
    Left = 32
    Top = 51
    Width = 107
    Height = 25
    Caption = 'Check great circle'
    TabOrder = 1
    OnClick = BitBtn1Click
  end
  object BitBtn4: TBitBtn
    Left = 32
    Top = 160
    Width = 97
    Height = 25
    Caption = 'Great circle'
    TabOrder = 2
    OnClick = BitBtn4Click
  end
  object BitBtn2: TBitBtn
    Left = 32
    Top = 104
    Width = 81
    Height = 41
    Caption = 'Net'
    Glyph.Data = {
      46020000424D460200000000000076000000280000001D0000001D0000000100
      040000000000D001000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFF000FFFFFFFFFFF0000000FFFFFFFFFFF000FFFFFFFF000F
      F000FF000FFFFFFFF000FFFFFFF0FFFF0F0F0FFFF0FFFFFFF000FFFFFF0F0FFF
      0F0F0FFF0F0FFFFFF000FFFFF0000000000000000000FFFFF000FFFF0FF0FFF0
      FF0FF0FFF0FF0FFFF000FFF0FF0FFF0FFF0FF0FFF0FFF0FFF000FF0FFF0FFF0F
      FF0FFF0FFF0FFF0FF000FF0000000000000000000000000FF000FF0FF0FFF0FF
      FF0FFF0FFFF0FF0FF000F0FFF0FF0FFFFF0FFFF0FFF0FFF0F000F0FF0FFF0FFF
      FF0FFFF0FFFF0FF0F000F0FF0FFF0FFFFF0FFFF0FFFF0FF0F000F00000000000
      0000000000000000F000F0FF0FFF0FFFFF0FFFF0FFFF0FF0F000F0FFF0FFF0FF
      FF0FFF0FFFF0FFF0F000F0FFF0FFF0FFFF0FFF0FFFF0FFF0F000FF0FF0FFF0FF
      FF0FFF0FFFF0FF0FF000FF0000000000000000000000000FF000FF0FFF0FFF0F
      FF0FF0FFFF0FFF0FF000FFF0FFF0FFF0FF0FF0FFF0FFF0FFF000FFFF0FF0FFF0
      FF0FF0FF0FFF0FFFF000FFFFF0000000000000000000FFFFF000FFFFFF0F0FFF
      0F0F0FF0FF0FFFFFF000FFFFFFF0F0FF0F0F0FF0F0FFFFFFF000FFFFFFFF000F
      F000FF000FFFFFFFF000FFFFFFFFFFF0000000FFFFFFFFFFF000FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFF000}
    TabOrder = 3
    OnClick = BitBtn2Click
  end
  object Memo1: TMemo
    Left = 192
    Top = 0
    Width = 247
    Height = 229
    Align = alRight
    TabOrder = 4
  end
  object BitBtn3: TBitBtn
    Left = 84
    Top = 196
    Width = 75
    Height = 25
    Caption = 'Tolerance'
    TabOrder = 5
    OnClick = BitBtn3Click
  end
end
