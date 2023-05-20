object dblimit: Tdblimit
  Left = 339
  Top = 503
  BorderIcons = []
  Caption = 'Table Columns'
  ClientHeight = 406
  ClientWidth = 293
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poMainFormCenter
  TextHeight = 13
  object Label1: TLabel
    Left = 208
    Top = 192
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 208
    Top = 216
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object OKBtn: TBitBtn
    Left = 208
    Top = 340
    Width = 73
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    OnClick = OKBtnClick
    IsControl = True
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 200
    Height = 406
    HorzScrollBar.Visible = False
    Align = alLeft
    TabOrder = 1
    ExplicitHeight = 407
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 180
      Height = 161
      AutoSize = True
      OnDblClick = Image1DblClick
      OnMouseMove = Image1MouseMove
    end
  end
  object BitBtn3: TBitBtn
    Left = 206
    Top = 149
    Width = 77
    Height = 25
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    OnClick = BitBtn3Click
    IsControl = True
  end
  object Button1: TButton
    Left = 208
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Hide all'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 208
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Show all'
    TabOrder = 4
    OnClick = Button2Click
  end
  object BitBtn1: TBitBtn
    Left = 206
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Save fields'
    TabOrder = 5
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 206
    Top = 287
    Width = 75
    Height = 25
    Caption = 'Restore'
    TabOrder = 6
    OnClick = BitBtn2Click
  end
end
