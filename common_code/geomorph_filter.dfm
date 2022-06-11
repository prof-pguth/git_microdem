object GeomorphFilterForm: TGeomorphFilterForm
  Left = 0
  Top = 0
  Caption = 'Geomorphic filtering'
  ClientHeight = 535
  ClientWidth = 811
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefaultSizeOnly
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 41
    Width = 811
    Height = 494
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Conditions'
      object DBGrid1: TDBGrid
        Left = 0
        Top = 0
        Width = 803
        Height = 466
        Align = alClient
        DataSource = DataSource1
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 811
    Height = 41
    Align = alTop
    TabOrder = 1
    object BitBtn2: TBitBtn
      Left = 17
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Filter'
      Enabled = False
      TabOrder = 0
      OnClick = BitBtn2Click
    end
    object BitBtn5: TBitBtn
      Left = 179
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Local Optima'
      Enabled = False
      TabOrder = 1
      OnClick = BitBtn5Click
    end
    object BitBtn1: TBitBtn
      Left = 98
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Unfilter'
      TabOrder = 2
      OnClick = BitBtn1Click
    end
    object BitBtn7: TBitBtn
      Left = 341
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Close all'
      TabOrder = 3
      OnClick = BitBtn7Click
    end
    object BitBtn9: TBitBtn
      Left = 260
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Regions'
      TabOrder = 4
      OnClick = BitBtn9Click
    end
  end
  object DataSource1: TDataSource
    Left = 168
    Top = 104
  end
end
