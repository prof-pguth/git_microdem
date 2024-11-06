object pickcounty: Tpickcounty
  Left = 598
  Top = 435
  ClientHeight = 343
  ClientWidth = 521
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefaultSizeOnly
  OnActivate = FormActivate
  OnCreate = FormCreate
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 0
    Top = 41
    Width = 521
    Height = 268
    Align = alClient
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnDblClick = DBGrid1DblClick
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 521
    Height = 41
    ButtonHeight = 21
    Caption = 'ToolBar1'
    TabOrder = 0
    ExplicitWidth = 533
    DesignSize = (
      521
      41)
    object OKBtn: TBitBtn
      Left = 0
      Top = 0
      Width = 81
      Height = 21
      Anchors = [akLeft, akTop, akRight, akBottom]
      Kind = bkOK
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 0
      OnClick = OKBtnClick
      IsControl = True
    end
    object Label1: TLabel
      Left = 81
      Top = 0
      Width = 25
      Height = 21
      Caption = 'State'
    end
    object Edit1: TEdit
      Left = 106
      Top = 0
      Width = 52
      Height = 21
      CharCase = ecUpperCase
      TabOrder = 2
      OnChange = Edit1Change
    end
    object BitBtn1: TBitBtn
      Left = 158
      Top = 0
      Width = 75
      Height = 21
      Caption = 'Re-Index'
      TabOrder = 3
      OnClick = BitBtn1Click
    end
    object HelpBtn: TBitBtn
      Left = 233
      Top = 0
      Width = 77
      Height = 21
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 1
      OnClick = HelpBtnClick
      IsControl = True
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 309
    Width = 521
    Height = 34
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 327
    ExplicitWidth = 533
    object CheckBox1: TCheckBox
      Left = 11
      Top = 2
      Width = 97
      Height = 23
      Caption = 'Show neighbors'
      TabOrder = 0
    end
  end
  object DataSource1: TDataSource
    Left = 472
    Top = 40
  end
end
