object DbaForm: TDbaForm
  Left = 920
  Top = 145
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Set Data Base Fields'
  ClientHeight = 284
  ClientWidth = 307
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 307
    Height = 41
    Align = alTop
    TabOrder = 0
    object Button1: TButton
      Left = 8
      Top = 10
      Width = 65
      Height = 25
      Caption = 'Create DB'
      TabOrder = 0
      OnClick = Button1Click
    end
    object HelpBtn: TBitBtn
      Left = 167
      Top = 10
      Width = 58
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
      Left = 80
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Copy DB'
      TabOrder = 2
      OnClick = Button2Click
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 41
    Width = 307
    Height = 243
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Record View'
      object Label2: TLabel
        Left = 18
        Top = 52
        Width = 53
        Height = 13
        Caption = '&Field Name'
        FocusControl = DBEdit1
      end
      object Label1: TLabel
        Left = 16
        Top = 168
        Width = 58
        Height = 13
        Caption = 'Field Length'
      end
      object Label3: TLabel
        Left = 16
        Top = 192
        Width = 43
        Height = 13
        Caption = 'Decimals'
      end
      object DBRadioGroup1: TDBRadioGroup
        Left = 16
        Top = 80
        Width = 89
        Height = 73
        Caption = 'Field type'
        DataField = 'TYPE'
        DataSource = DataSource1
        Items.Strings = (
          'String'
          'Integer'
          'Float')
        ParentBackground = True
        TabOrder = 0
        Values.Strings = (
          'String'
          'Integer'
          'Float'
          '')
        OnChange = DBRadioGroup1Change
      end
      object DBEdit1: TDBEdit
        Left = 80
        Top = 48
        Width = 121
        Height = 21
        CharCase = ecUpperCase
        DataField = 'NAME'
        DataSource = DataSource1
        TabOrder = 1
      end
      object DBEdit2: TDBEdit
        Left = 88
        Top = 160
        Width = 89
        Height = 21
        DataField = 'LENGTH'
        DataSource = DataSource1
        TabOrder = 2
      end
      object DBEdit3: TDBEdit
        Left = 88
        Top = 192
        Width = 89
        Height = 21
        DataField = 'DECIMALS'
        DataSource = DataSource1
        TabOrder = 3
      end
      object BitBtn1: TBitBtn
        Left = 16
        Top = 8
        Width = 89
        Height = 25
        Caption = 'Insert'
        TabOrder = 4
        OnClick = BitBtn1Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Grid View'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object DBGrid1: TDBGrid
        Left = 0
        Top = 0
        Width = 299
        Height = 215
        Align = alClient
        DataSource = DataSource1
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
    end
  end
  object DataSource1: TDataSource
    Left = 240
    Top = 8
  end
end
