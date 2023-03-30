object GrayscaleForm: TGrayscaleForm
  Left = 0
  Top = 0
  Caption = 'Set colors'
  ClientHeight = 210
  ClientWidth = 523
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 37
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 256
    Top = 37
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Image2: TImage
    Left = 8
    Top = 0
    Width = 512
    Height = 31
  end
  object Label3: TLabel
    Left = 32
    Top = 107
    Width = 31
    Height = 13
    Caption = 'Label3'
  end
  object Label4: TLabel
    Left = 256
    Top = 107
    Width = 31
    Height = 13
    Caption = 'Label4'
  end
  object TrackBar1: TTrackBar
    Left = 32
    Top = 56
    Width = 150
    Height = 45
    Max = 180
    Frequency = 10
    TabOrder = 0
    OnChange = TrackBar1Change
  end
  object TrackBar2: TTrackBar
    Left = 256
    Top = 56
    Width = 150
    Height = 45
    Max = 180
    Frequency = 10
    TabOrder = 1
    OnChange = TrackBar2Change
  end
  object BitBtn1: TBitBtn
    Left = 432
    Top = 177
    Width = 75
    Height = 25
    Caption = 'Redraw'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
      FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
      CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
      FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
    TabOrder = 2
    OnClick = BitBtn1Click
  end
  object TrackBar3: TTrackBar
    Left = 32
    Top = 126
    Width = 150
    Height = 45
    Max = 255
    Frequency = 10
    TabOrder = 3
    OnChange = TrackBar3Change
  end
  object TrackBar4: TTrackBar
    Left = 256
    Top = 126
    Width = 150
    Height = 45
    Max = 255
    Frequency = 10
    TabOrder = 4
    OnChange = TrackBar4Change
  end
  object BitBtn2: TBitBtn
    Left = 212
    Top = 177
    Width = 75
    Height = 25
    Caption = 'Defaults'
    TabOrder = 5
    OnClick = BitBtn2Click
  end
end
