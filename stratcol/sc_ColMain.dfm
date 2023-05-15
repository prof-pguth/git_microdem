object ColMainF: TColMainF
  Left = 278
  Top = 273
  Caption = 'STRATCOL'
  ClientHeight = 451
  ClientWidth = 588
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'System'
  Font.Style = []
  FormStyle = fsMDIChild
  KeyPreview = True
  Menu = MainMenu1
  Position = poDefault
  PrintScale = poNone
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 421
    Width = 588
    Height = 30
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 400
    ExplicitWidth = 584
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 29
    Width = 588
    Height = 392
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 584
    ExplicitHeight = 371
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 584
      Height = 388
      Hint = 'Enter column from keyboard'
      Align = alClient
      AutoSize = True
      OnDblClick = Image1DblClick
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
      ExplicitLeft = 3
      ExplicitTop = 4
      ExplicitWidth = 640
      ExplicitHeight = 371
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 588
    Height = 29
    ButtonHeight = 25
    Caption = 'ToolBar1'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    ExplicitWidth = 584
    object SpeedButton2: TSpeedButton
      Left = 0
      Top = 0
      Width = 23
      Height = 25
      Hint = 'Create new column'
      Caption = ' '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'System'
      Font.Style = [fsBold]
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333033333
        333333333373F3333333333330F033333333333337373F33333333330FFF0333
        33333333733F73F333333330F80FF033333333373373373F3333330F80F7FF03
        33333373373F3F73F33330F70F0F0FF03333373F737373F73F33330F77F7F0FF
        03333373F33F373F73F33330F70F0F0FF03333373F737373373F33330F77F7F7
        FF03333373F33F3F3F73333330F70F0F07F03333373F737373373333330F77FF
        7F0333333373F33F337333333330F707F033333333373F733733333333330F7F
        03333333333373F373F33333333330F0303333F3F3F3F73737F3303030303303
        3033373737373F7FF73303030303000003337373737377777333}
      NumGlyphs = 2
      ParentFont = False
      OnClick = SpeedButton2Click
    end
    object SpeedButton1: TSpeedButton
      Left = 23
      Top = 0
      Width = 23
      Height = 25
      Hint = 'Load column from file'
      Glyph.Data = {
        7E010000424D7E01000000000000760000002800000016000000160000000100
        0400000000000801000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFF00F00000000000000000000F00F09999999999999999990F00F099
        99999999999999900F00F099999999999999990FFF00F099999999999999990F
        FF00F000000000000000000FFF00F0CCCCCCCCCCCCCCCCC0FF00F0CCCCCCCCCC
        CCCCCCC0FF00F0CCCCCCCCCCCCCCCCC0FF00F0CCCCCCCCCCCCCCCCCC0F00F0CC
        CCCCCCCCCCCCCCC0FF00F0CCCCCCCCCCCCCCCC0FFF00F0CCCCCCCCCCCCCCC0FF
        FF00F00000000000000000FFFF00F0AAAAAAAAAAAAAAAA0FFF00F0AAAAAAAAAA
        AAAAAAA0FF00F0AAAAAAAAAAAAAAAA0FFF00F0AAAAAAAAAAAAAAA0FFFF00F0AA
        AAAAAAAAAAAAAA00FF00F00000000000000000000F00FFFFFFFFFFFFFFFFFFFF
        FF00}
      OnClick = SpeedButton1Click
    end
    object RedrawSpeedButton12: TSpeedButton
      Left = 46
      Top = 0
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
    object ClipboardSpeedButton: TSpeedButton
      Left = 71
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Copy map to clipboard'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFF0000000000FFFFF0FFFFFFFFFF0FFFF0FFFFFFFFFF0FFFF0F9FFFFFFF
        F0FFFF0FF9FFFFFFF0FF9999999FFFFFF0FF99999999FFFFF0FF99999999FFFF
        F0FF9999999FFFFFF0FFFF0FF9FFFFFFF0FFFF0F9FFFFFFFF0FFFF0FFFDDDDFF
        F0FFFFF000DDDD000FFFFFFFFDDFFDDFFFFFFFFFFFDDDDFFFFFF}
      OnClick = ClipboardSpeedButtonClick
    end
  end
  object MainMenu1: TMainMenu
    Left = 16
    Top = 40
    object File1: TMenuItem
      Caption = '&File'
      object SaveImage1: TMenuItem
        Caption = 'Save &Image'
        OnClick = SaveImage1Click
      end
      object SaveFiles1: TMenuItem
        Caption = 'Save &Files'
        OnClick = SaveFiles1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object OpenDiagram1: TMenuItem
        Caption = '&Open Diagram'
        OnClick = OpenDiagram1Click
      end
      object Savediagram1: TMenuItem
        Caption = '&Save diagram'
        Enabled = False
        OnClick = Savediagram1Click
      end
      object CloseDiagram1: TMenuItem
        Caption = '&Close Diagram'
        Enabled = False
        OnClick = CloseDiagram1Click
      end
      object N5: TMenuItem
        Caption = '-'
        Visible = False
      end
      object Convertlengthunits1: TMenuItem
        Caption = 'Convert length units'
        Visible = False
        object Timefiles1: TMenuItem
          Caption = '&Time files'
          OnClick = Timefiles1Click
        end
        object Fossilranges2: TMenuItem
          Caption = '&Fossil ranges'
          OnClick = Fossilranges2Click
        end
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit Stratcol'
        OnClick = Exit1Click
      end
    end
    object Timecorrelation1: TMenuItem
      Caption = '&Time correlation'
      object TimeScalefromFile1: TMenuItem
        Caption = '&Time Scale from File'
        OnClick = TimeScalefromFile1Click
      end
      object ColumnfromFile1: TMenuItem
        Caption = '&Column from File'
        OnClick = ColumnfromFile1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object CreateNew1: TMenuItem
        Caption = 'Create &New'
        object AddColumn1: TMenuItem
          Caption = '&New Column'
          OnClick = AddColumn1Click
        end
        object TimeScale1: TMenuItem
          Caption = '&New Time Scale'
          OnClick = TimeScale1Click
        end
      end
      object Newtimelimits1: TMenuItem
        Caption = '&New time limits'
        Enabled = False
        OnClick = Newtimelimits1Click
      end
      object Redraw1: TMenuItem
        Caption = '&Redraw'
        OnClick = Redraw1Click
      end
    end
    object Rockcorrelation1: TMenuItem
      Caption = '&Rock correlation'
      object Labelfile1: TMenuItem
        Caption = '&Label file'
        OnClick = Labelfile1Click
      end
      object Columnfromfile2: TMenuItem
        Caption = 'Column &File'
        OnClick = Columnfromfile2Click
      end
      object FossilRanges1: TMenuItem
        Caption = 'Fossil &Ranges'
        OnClick = FossilRanges1Click
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object Createnew2: TMenuItem
        Caption = '&Create new'
        object Column1: TMenuItem
          Caption = '&Column '
          OnClick = Column1Click
        end
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Rescalethickness1: TMenuItem
        Caption = 'Rescale thickness'
        OnClick = Rescalethickness1Click
      end
      object Redraw2: TMenuItem
        Caption = 'Redraw'
        OnClick = Redraw2Click
      end
    end
    object Options1: TMenuItem
      Caption = 'Stratcol Options'
      Hint = ' '
      object Set1: TMenuItem
        Caption = 'Set options'
        OnClick = Set1Click
      end
      object Font1: TMenuItem
        Caption = '&Font'
        object Scalebar1: TMenuItem
          Caption = '&Scale bar'
          OnClick = Scalebar1Click
        end
        object Columns1: TMenuItem
          Caption = '&Columns'
          OnClick = Columns1Click
        end
      end
      object TimeAxis1: TMenuItem
        Caption = 'Time /Thickness &Axis'
        object Present1: TMenuItem
          Caption = '&Present'
          OnClick = Present1Click
        end
        object Ticksize1: TMenuItem
          Caption = '&Tick size'
          OnClick = Ticksize1Click
        end
        object Grid1: TMenuItem
          Caption = '&Grid'
          OnClick = Grid1Click
        end
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object EditLithologyPatterns1: TMenuItem
        Caption = 'Edit Lithology &Patterns'
        OnClick = EditLithologyPatterns1Click
      end
      object PatternNames1: TMenuItem
        Caption = 'Pattern &Names'
        object Create1: TMenuItem
          Caption = '&Create'
          OnClick = Create1Click
        end
        object Select1: TMenuItem
          Caption = '&Select'
          OnClick = Select1Click
        end
        object Display1: TMenuItem
          Caption = 'Display'
          OnClick = Display1Click
        end
      end
      object CreateGISDB1: TMenuItem
        Caption = 'Create GIS DB'
        OnClick = CreateGISDB1Click
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      OnClick = Help1Click
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'System'
    Font.Style = []
    Left = 77
    Top = 40
  end
  object PopupMenu1: TPopupMenu
    Left = 146
    Top = 42
    object Tickincrement1: TMenuItem
      Caption = 'Tick increment'
      OnClick = Tickincrement1Click
    end
    object Columnheight1: TMenuItem
      Caption = 'Column height'
      OnClick = Columnheight1Click
    end
  end
  object PopupMenu2: TPopupMenu
    Left = 240
    Top = 53
    object Setverticalpixelsize1: TMenuItem
      Caption = 'Set vertical pixel size'
      OnClick = Setverticalpixelsize1Click
    end
  end
end
