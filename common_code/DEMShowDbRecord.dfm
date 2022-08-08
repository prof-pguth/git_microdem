object showrecordform: Tshowrecordform
  Left = 322
  Top = 220
  BorderIcons = [biSystemMenu]
  Caption = 'Data Base Record'
  ClientHeight = 552
  ClientWidth = 1191
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 533
    Width = 1191
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 1191
    Height = 29
    ButtonHeight = 21
    Caption = 'ToolBar1'
    TabOrder = 1
    object BitBtn1: TBitBtn
      Left = 0
      Top = 0
      Width = 75
      Height = 21
      Caption = 'Post edits'
      TabOrder = 0
      OnClick = BitBtn1Click
    end
    object BitBtn2: TBitBtn
      Left = 75
      Top = 0
      Width = 44
      Height = 21
      Caption = 'All fields'
      TabOrder = 1
      OnClick = BitBtn2Click
    end
    object BitBtn20: TBitBtn
      Left = 119
      Top = 0
      Width = 46
      Height = 21
      Caption = 'ABC'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000120B0000120B00001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
        DADAADADADA4ADADADADDADADA444ADADADAADADA44444ADADADDADADAD4DADA
        DADAADADADA4ADADADADDADAD00000DADADAADADA0EFE0ADADADDADAD0FEF0DA
        DADAADADA0EFE0ADADADDADAD0FEF0DADADAA00000EFE00000ADD0FFF0FEF0FF
        F0DAA0FFF0EFE0FFF0ADD0FFF0FEF0FFF0DAADADADADADADADAD}
      TabOrder = 13
      OnClick = BitBtn20Click
    end
    object BitBtn14: TBitBtn
      Left = 165
      Top = 0
      Width = 43
      Height = 21
      Caption = '< Prev'
      TabOrder = 10
      OnClick = BitBtn14Click
    end
    object BitBtn12: TBitBtn
      Left = 208
      Top = 0
      Width = 43
      Height = 21
      Caption = 'Next >'
      TabOrder = 9
      OnClick = BitBtn12Click
    end
    object BitBtn3: TBitBtn
      Left = 251
      Top = 0
      Width = 37
      Height = 21
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33333333333333333333EEEEEEEEEEEEEEE333FFFFFFFFFFFFF3E00000000000
        00E337777777777777F3E0F77777777770E337F33333333337F3E0F333333333
        70E337F33333333337F3E0F33333333370E337F333FF3FF337F3E0F330030033
        70E337F3377F77F337F3E0F33003003370E337F3377F77F337F3E0F330030033
        70E337F3377F77F337F3E0F33003003370E337F3377F77F337F3E0F330030033
        70E337F33773773337F3E0F33333333370E337F33333333337F3E0F333333333
        70E337F33333333337F3E0FFFFFFFFFFF0E337FFFFFFFFFFF7F3E00000000000
        00E33777777777777733EEEEEEEEEEEEEEE33333333333333333}
      NumGlyphs = 2
      TabOrder = 15
      Visible = False
      OnClick = BitBtn3Click
    end
    object BitBtn5: TBitBtn
      Left = 288
      Top = 0
      Width = 33
      Height = 21
      Enabled = False
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33333333333333333333EEEEEEEEEEEEEEE333FFFFFFFFFFFFF3E00000000000
        00E337777777777777F3E0F77777777770E337F33333333337F3E0F333333333
        70E337F3333F333337F3E0F33303333370E337F3337FF33337F3E0F333003333
        70E337F33377FF3337F3E0F33300033370E337F333777FF337F3E0F333000033
        70E337F33377773337F3E0F33300033370E337F33377733337F3E0F333003333
        70E337F33377333337F3E0F33303333370E337F33373333337F3E0F333333333
        70E337F33333333337F3E0FFFFFFFFFFF0E337FFFFFFFFFFF7F3E00000000000
        00E33777777777777733EEEEEEEEEEEEEEE33333333333333333}
      NumGlyphs = 2
      TabOrder = 16
      Visible = False
      OnClick = BitBtn5Click
    end
    object WWWBtn1: TBitBtn
      Left = 321
      Top = 0
      Width = 56
      Height = 21
      Caption = 'WWW'
      TabOrder = 2
      OnClick = WWWBtn1Click
    end
    object BitBtn4: TBitBtn
      Left = 377
      Top = 0
      Width = 75
      Height = 21
      Caption = 'DEM'
      TabOrder = 5
      OnClick = BitBtn4Click
    end
    object ColorBitBtn: TBitBtn
      Left = 452
      Top = 0
      Width = 75
      Height = 21
      Caption = 'Color'
      TabOrder = 3
      Visible = False
      OnClick = ColorBitBtnClick
    end
    object BitBtn8: TBitBtn
      Left = 527
      Top = 0
      Width = 75
      Height = 21
      Caption = 'Lat/Long'
      TabOrder = 4
      OnClick = BitBtn8Click
    end
    object BitBtn10: TBitBtn
      Left = 602
      Top = 0
      Width = 75
      Height = 21
      Caption = 'CSV Save'
      TabOrder = 6
      OnClick = BitBtn10Click
    end
    object BitBtn11: TBitBtn
      Left = 677
      Top = 0
      Width = 66
      Height = 21
      Caption = 'Fan properties'
      TabOrder = 8
      OnClick = BitBtn11Click
    end
    object BitBtn7: TBitBtn
      Left = 743
      Top = 0
      Width = 39
      Height = 21
      Caption = 'HL fan'
      TabOrder = 7
      OnClick = BitBtn7Click
    end
    object BitBtn13: TBitBtn
      Left = 782
      Top = 0
      Width = 40
      Height = 21
      Caption = '+Font'
      TabOrder = 11
      OnClick = BitBtn13Click
    end
    object BitBtn15: TBitBtn
      Left = 822
      Top = 0
      Width = 40
      Height = 21
      Caption = 'HTML'
      TabOrder = 12
      OnClick = BitBtn15Click
    end
    object BitBtn16: TBitBtn
      Left = 862
      Top = 0
      Width = 75
      Height = 21
      Caption = 'Options'
      TabOrder = 14
      OnClick = BitBtn16Click
    end
    object BitBtn6: TBitBtn
      Left = 937
      Top = 0
      Width = 75
      Height = 21
      TabOrder = 17
      OnClick = BitBtn6Click
    end
    object SpeedButton1: TSpeedButton
      Left = 1012
      Top = 0
      Width = 32
      Height = 21
      Caption = 'KML'
      OnClick = SpeedButton1Click
    end
    object BitBtn9: TBitBtn
      Left = 1044
      Top = 0
      Width = 45
      Height = 21
      Caption = 'Use="Y"'
      TabOrder = 18
      OnClick = BitBtn9Click
    end
    object BitBtn17: TBitBtn
      Left = 1089
      Top = 0
      Width = 40
      Height = 21
      Caption = 'Use="N"'
      TabOrder = 19
      OnClick = BitBtn17Click
    end
    object BitBtn18: TBitBtn
      Left = 1129
      Top = 0
      Width = 50
      Height = 21
      Caption = 'Delete'
      TabOrder = 20
      OnClick = BitBtn18Click
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 29
    Width = 1191
    Height = 504
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'Database'
      object StringGrid1: TStringGrid
        Left = 0
        Top = 0
        Width = 1183
        Height = 476
        Align = alClient
        ColCount = 2
        DefaultColWidth = 120
        DefaultRowHeight = 16
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSizing, goColSizing, goRowMoving, goColMoving]
        TabOrder = 0
        ColWidths = (
          120
          120)
        RowHeights = (
          16
          16
          16
          16
          16)
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 1183
        Height = 476
        Align = alClient
        TabOrder = 0
      end
    end
    object TabSheet3DProf: TTabSheet
      Caption = '3D profile'
      ImageIndex = 2
      object Image2: TImage
        Left = 0
        Top = 0
        Width = 1183
        Height = 476
        Align = alClient
        OnMouseDown = Image2MouseDown
        ExplicitLeft = 8
        ExplicitTop = 8
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'NLCD'
      ImageIndex = 3
      object Memo3: TMemo
        Left = 0
        Top = 0
        Width = 1183
        Height = 476
        Align = alClient
        TabOrder = 0
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Climograph'
      ImageIndex = 4
      object Image1: TImage
        Left = 0
        Top = 0
        Width = 1183
        Height = 476
        Align = alClient
        OnMouseDown = Image1MouseDown
        ExplicitLeft = 8
        ExplicitTop = 16
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Focal planes'
      ImageIndex = 5
      TabVisible = False
      object Memo4: TMemo
        Left = 0
        Top = 0
        Width = 265
        Height = 193
        TabOrder = 0
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'SSO diagram'
      ImageIndex = 6
      object Image3: TImage
        Left = 0
        Top = 0
        Width = 1183
        Height = 476
        Align = alClient
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'Aspect diagram'
      ImageIndex = 7
      object Image4: TImage
        Left = 0
        Top = 0
        Width = 1183
        Height = 476
        Align = alClient
        ExplicitLeft = 8
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
    object TabSheet8: TTabSheet
      Caption = 'Time series'
      ImageIndex = 8
      object Image5: TImage
        Left = 0
        Top = 0
        Width = 1183
        Height = 476
        Align = alClient
        OnDblClick = Image5DblClick
        OnMouseDown = Image5MouseDown
        ExplicitLeft = 16
        ExplicitTop = 8
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
    object TabSheet9: TTabSheet
      Caption = 'Blow up map'
      ImageIndex = 9
      object Image6: TImage
        Left = 0
        Top = 0
        Width = 1183
        Height = 476
        Align = alClient
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 296
    Top = 112
    object Saveimage1: TMenuItem
      Caption = 'Save image'
      OnClick = Saveimage1Click
    end
    object Copyimagetoclipboard1: TMenuItem
      Caption = 'Copy image to clipboard'
      OnClick = Copyimagetoclipboard1Click
    end
    object Climographoptions1: TMenuItem
      Caption = 'Climograph options'
      OnClick = Climographoptions1Click
    end
    object Focalmechanismoptions1: TMenuItem
      Caption = 'Focal mechanism options'
      OnClick = Focalmechanismoptions1Click
    end
    object Focalmechanismsize1: TMenuItem
      Caption = 'Focal mechanism size'
      OnClick = Focalmechanismsize1Click
    end
    object Focalmechanismfill1: TMenuItem
      Caption = 'Focal mechanism fill'
      OnClick = Focalmechanismfill1Click
    end
    object Popoutgraph1: TMenuItem
      Caption = 'Pop out graph'
      OnClick = Popoutgraph1Click
    end
    object Resia1: TMenuItem
      Caption = 'Resize image'
      OnClick = Resia1Click
    end
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 384
    Top = 240
  end
  object PopupMenu2: TPopupMenu
    Left = 408
    Top = 112
    object Openprofile1: TMenuItem
      Caption = 'Open profile'
      OnClick = Openprofile1Click
    end
    object Copyimagetoclipboard2: TMenuItem
      Caption = 'Copy image to clipboard'
      OnClick = Copyimagetoclipboard2Click
    end
    object Resizeimage1: TMenuItem
      Caption = 'Resize image'
      OnClick = Resizeimage1Click
    end
  end
  object PopupMenu3: TPopupMenu
    Left = 592
    Top = 96
    object Copyimagetoclipboard3: TMenuItem
      Caption = 'Copy image to clipboard'
      OnClick = Copyimagetoclipboard3Click
    end
    object Popoutgraph2: TMenuItem
      Caption = 'Pop out graph'
      OnClick = Popoutgraph2Click
    end
    object Redrawgraph1: TMenuItem
      Caption = 'Redraw graph'
      OnClick = Redrawgraph1Click
    end
  end
end
