object SatHeaderForm: TSatHeaderForm
  Left = 338
  Top = 200
  ClientHeight = 326
  ClientWidth = 531
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object StringGrid1: TStringGrid
    Left = 0
    Top = 105
    Width = 531
    Height = 180
    Align = alClient
    ColCount = 3
    TabOrder = 0
    ExplicitWidth = 527
    ExplicitHeight = 179
  end
  object Panel1: TPanel
    Left = 0
    Top = 285
    Width = 531
    Height = 41
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 284
    ExplicitWidth = 527
    object OKBtn: TBitBtn
      Left = 192
      Top = 6
      Width = 77
      Height = 27
      Kind = bkOK
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 0
      OnClick = OKBtnClick
      IsControl = True
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 531
    Height = 105
    Align = alTop
    TabOrder = 2
    ExplicitWidth = 527
    object Label5: TLabel
      Left = 8
      Top = 72
      Width = 60
      Height = 13
      Caption = 'Default gray:'
    end
    object Label6: TLabel
      Left = 136
      Top = 72
      Width = 20
      Height = 13
      Caption = 'Red'
    end
    object Label7: TLabel
      Left = 224
      Top = 72
      Width = 29
      Height = 13
      Caption = 'Green'
    end
    object Label4: TLabel
      Left = 320
      Top = 40
      Width = 27
      Height = 13
      Caption = 'Rows'
    end
    object Label3: TLabel
      Left = 232
      Top = 40
      Width = 23
      Height = 13
      Caption = 'Cols:'
    end
    object Label2: TLabel
      Left = 8
      Top = 32
      Width = 84
      Height = 13
      Caption = 'Number of bands:'
    end
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 60
      Height = 13
      Caption = 'Scene Title: '
    end
    object Label8: TLabel
      Left = 326
      Top = 72
      Width = 21
      Height = 13
      Caption = 'Blue'
    end
    object Edit5: TEdit
      Left = 72
      Top = 72
      Width = 49
      Height = 21
      TabOrder = 0
      Text = 'Edit5'
    end
    object Edit6: TEdit
      Left = 168
      Top = 72
      Width = 49
      Height = 21
      TabOrder = 1
      Text = 'Edit6'
    end
    object Edit7: TEdit
      Left = 264
      Top = 72
      Width = 49
      Height = 21
      TabOrder = 2
      Text = 'Edit7'
    end
    object Edit8: TEdit
      Left = 352
      Top = 72
      Width = 57
      Height = 21
      TabOrder = 3
      Text = 'Edit8'
    end
    object Edit4: TEdit
      Left = 352
      Top = 32
      Width = 57
      Height = 21
      TabOrder = 4
      Text = 'Edit4'
    end
    object Edit3: TEdit
      Left = 264
      Top = 32
      Width = 49
      Height = 21
      TabOrder = 5
      Text = 'Edit3'
    end
    object Edit2: TEdit
      Left = 96
      Top = 32
      Width = 73
      Height = 21
      TabOrder = 6
      Text = 'Edit2'
    end
    object Edit1: TEdit
      Left = 88
      Top = 8
      Width = 305
      Height = 21
      TabOrder = 7
      Text = 'Edit1'
    end
  end
end
