object SatTractForm: TSatTractForm
  Left = 143
  Top = 242
  Caption = 'Satellite tracking'
  ClientHeight = 333
  ClientWidth = 466
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 466
    Height = 78
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 366
    object Label1: TLabel
      Left = 360
      Top = 60
      Width = 32
      Height = 13
      Caption = 'Label1'
    end
    object RedrawSpeedButton12: TSpeedButton
      Left = 216
      Top = 41
      Width = 25
      Height = 25
      Hint = 'Force redraw'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
        FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
        CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
        FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
      OnClick = RedrawSpeedButton12Click
    end
    object BitBtn1: TBitBtn
      Left = 216
      Top = 10
      Width = 49
      Height = 25
      Caption = 'Pause'
      TabOrder = 0
      OnClick = BitBtn1Click
    end
    object BitBtn2: TBitBtn
      Left = 271
      Top = 10
      Width = 49
      Height = 25
      Caption = 'Resume'
      Enabled = False
      TabOrder = 1
      OnClick = BitBtn2Click
    end
    object CheckBox1: TCheckBox
      Left = 368
      Top = 41
      Width = 97
      Height = 17
      Caption = 'Trails'
      TabOrder = 2
    end
    object BitBtn4: TBitBtn
      Left = 361
      Top = 10
      Width = 60
      Height = 25
      Caption = 'Sats'
      TabOrder = 3
      OnClick = BitBtn4Click
    end
    object BitBtn5: TBitBtn
      Left = 256
      Top = 41
      Width = 75
      Height = 25
      Caption = 'Sky map'
      TabOrder = 4
      OnClick = BitBtn5Click
    end
    object RadioGroup1: TRadioGroup
      Left = 0
      Top = 10
      Width = 193
      Height = 62
      Caption = 'Time interval (minutes)'
      Columns = 4
      ItemIndex = 3
      Items.Strings = (
        '1'
        '2'
        '5'
        '10'
        '15'
        '20'
        '30')
      TabOrder = 5
      OnClick = RadioGroup1Click
    end
  end
  object StringGrid1: TStringGrid
    Left = 0
    Top = 78
    Width = 466
    Height = 255
    Align = alClient
    FixedCols = 0
    TabOrder = 1
    ExplicitWidth = 366
    ColWidths = (
      64
      64
      64
      64
      64)
    RowHeights = (
      24
      24
      24
      24
      24)
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 96
    Top = 504
  end
end
