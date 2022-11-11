object SatTractForm: TSatTractForm
  Left = 143
  Top = 242
  Caption = 'Satellite tracking'
  ClientHeight = 342
  ClientWidth = 372
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
    Width = 372
    Height = 78
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 378
    object Label2: TLabel
      Left = 8
      Top = 8
      Width = 97
      Height = 13
      Caption = 'Time increment (min)'
    end
    object Label1: TLabel
      Left = 8
      Top = 59
      Width = 32
      Height = 13
      Caption = 'Label1'
    end
    object Edit1: TEdit
      Left = 112
      Top = 8
      Width = 41
      Height = 21
      TabOrder = 0
      Text = '15'
      OnChange = Edit1Change
    end
    object BitBtn1: TBitBtn
      Left = 168
      Top = 8
      Width = 49
      Height = 25
      Caption = 'Pause'
      TabOrder = 1
      OnClick = BitBtn1Click
    end
    object BitBtn2: TBitBtn
      Left = 216
      Top = 8
      Width = 49
      Height = 25
      Caption = 'Resume'
      Enabled = False
      TabOrder = 2
      OnClick = BitBtn2Click
    end
    object CheckBox1: TCheckBox
      Left = 16
      Top = 40
      Width = 97
      Height = 17
      Caption = 'Trails'
      TabOrder = 3
    end
    object BitBtn4: TBitBtn
      Left = 313
      Top = 8
      Width = 60
      Height = 25
      Caption = 'Sats'
      TabOrder = 4
      OnClick = BitBtn4Click
    end
    object BitBtn5: TBitBtn
      Left = 208
      Top = 39
      Width = 75
      Height = 25
      Caption = 'Sky map'
      TabOrder = 5
      OnClick = BitBtn5Click
    end
  end
  object StringGrid1: TStringGrid
    Left = 0
    Top = 78
    Width = 372
    Height = 264
    Align = alClient
    FixedCols = 0
    TabOrder = 1
    ExplicitWidth = 378
    ExplicitHeight = 273
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
