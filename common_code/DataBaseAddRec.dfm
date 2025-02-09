object DbaddRecForm: TDbaddRecForm
  Left = 237
  Top = 313
  BorderIcons = []
  Caption = 'Add record to data base'
  ClientHeight = 153
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 529
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 444
    object Button1: TButton
      Left = 9
      Top = 10
      Width = 137
      Height = 25
      Caption = '+ Rec && resume digitizing'
      TabOrder = 0
      OnClick = WriteRecContinueDigitButton
    end
    object HelpBtn: TBitBtn
      Left = 287
      Top = 10
      Width = 77
      Height = 25
      Caption = 'Help'
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 1
      OnClick = HelpBtnClick
      IsControl = True
    end
    object Button2: TButton
      Left = 152
      Top = 10
      Width = 129
      Height = 25
      Caption = '+ Rec && close shapefile'
      TabOrder = 2
      OnClick = Button2Click
    end
    object CheckBox1: TCheckBox
      Left = 376
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Copy last rec'
      TabOrder = 3
    end
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 41
    Width = 529
    Height = 112
    Align = alClient
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnCellClick = DBGrid1CellClick
  end
  object DataSource1: TDataSource
    OnDataChange = DataSource1DataChange
    Left = 232
    Top = 48
  end
end
