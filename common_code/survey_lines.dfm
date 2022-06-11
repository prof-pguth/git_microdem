object GetTracjksForm: TGetTracjksForm
  Left = 0
  Top = 0
  Caption = 'Get survey tracks'
  ClientHeight = 299
  ClientWidth = 262
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 64
    Width = 67
    Height = 13
    Caption = 'Track heading'
  end
  object Label2: TLabel
    Left = 24
    Top = 96
    Width = 78
    Height = 13
    Caption = 'Track length (m)'
  end
  object Label3: TLabel
    Left = 24
    Top = 120
    Width = 101
    Height = 13
    Caption = 'Length (m)extended '
  end
  object Label4: TLabel
    Left = 24
    Top = 152
    Width = 79
    Height = 13
    Caption = 'Track spaing (m)'
  end
  object Label5: TLabel
    Left = 24
    Top = 177
    Width = 69
    Height = 13
    Caption = 'Tracks to right'
  end
  object Label6: TLabel
    Left = 24
    Top = 208
    Width = 69
    Height = 13
    Caption = 'Trackes to left'
  end
  object Label7: TLabel
    Left = 53
    Top = 39
    Width = 123
    Height = 13
    Caption = 'Starting point not defined'
  end
  object BitBtn1: TBitBtn
    Left = 16
    Top = 8
    Width = 129
    Height = 25
    Caption = 'Center point'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object Edit1: TEdit
    Left = 131
    Top = 61
    Width = 98
    Height = 21
    TabOrder = 1
    Text = '0'
  end
  object Edit2: TEdit
    Left = 131
    Top = 93
    Width = 98
    Height = 21
    TabOrder = 2
    Text = '250'
  end
  object Edit3: TEdit
    Left = 131
    Top = 120
    Width = 98
    Height = 21
    TabOrder = 3
    Text = '250'
  end
  object Edit4: TEdit
    Left = 131
    Top = 147
    Width = 98
    Height = 21
    TabOrder = 4
    Text = '100'
  end
  object Edit5: TEdit
    Left = 131
    Top = 174
    Width = 98
    Height = 21
    TabOrder = 5
    Text = '5'
  end
  object Edit6: TEdit
    Left = 128
    Top = 200
    Width = 101
    Height = 21
    TabOrder = 6
    Text = '5'
  end
  object BitBtn2: TBitBtn
    Left = 40
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Plot'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
      FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
      CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
      FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
    TabOrder = 7
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 144
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Create DB'
    TabOrder = 8
    OnClick = BitBtn3Click
  end
end
