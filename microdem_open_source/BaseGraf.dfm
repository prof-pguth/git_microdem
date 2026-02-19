object ThisBaseGraph: TThisBaseGraph
  Left = 607
  Top = 262
  Hint = 'Graph legend'
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Graph'
  ClientHeight = 273
  ClientWidth = 809
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  FormStyle = fsMDIChild
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000070000000700000000000FFFF
    F0FFFFFF7FFFFFFF7FFFFFF7FFF0FFFFF0FFFFFF7FFFFFFF7FFFFFF7FFF0FFFF
    F0FFFFFF7FFFFFFF7FFFFFF7FFF0FFFFF0FFFFFF7FFFFFFF7FFFFFF7FFF0FFFF
    F777777777777777777777777770FFFFF0FFFFFF7FFFFFFF7FFFFFF7FFF0FFFF
    F0FFFFFF7FFFFFFF7FFFFFF7FFF0FFFFF0FFFFFF7FFFFFFF7FFFFFF7FFF0FFFF
    F0FFFFFF7FFFFFFF7FFFFFF7FFF0FFFFF0FFFFFF7FFFFFFF79999997FFF0FFFF
    F0FFFFFF7FFFFFFF79FFFF97FFF0FFFFF077777777777777777777777777FFFF
    F9FFFFFF7FFFFFF97FFFFFF7F999FFFFF099FFFF7FFFF99F7FFFFFF7FFF0FFFF
    F0F99999799999FF7FFFFFF7FFF0FFFFF0FFFFFF7FFFFFFF7FFFFFF7FFF0FFFF
    F0FFFFFF7FFFFFFF7FFFFFF7FFF0FFFFF0FFFFFF7FFFFFFF7FFFFFF7FFF0FFFF
    F0FFFFFF7FFFFFFF7FFFFFF7FFF0FFFFF0FFFFFF7FFFFFFF7FFFFFF7FFF0FFFF
    F077777777777777777777777770FFFFF0FFFFFF7FFFFFFF7FFFFFF7FFF0FFFF
    F0FFFFFF7FFFFFFF7FFFFFF7FFF0FFFFF0FFFFFF7FFFFFFF7FFFFFF7FFF0FFFF
    F0FFFFFF7FFFFFFF7FFFFFF7FFF0FFFFF0000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  Menu = MainMenu1
  Position = poDefault
  PrintScale = poPrintToFit
  ShowHint = True
  Visible = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 16
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 28
    Width = 809
    Height = 215
    Align = alClient
    TabOrder = 0
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 805
      Height = 211
      Hint = 'Legend'
      Align = alClient
      OnDblClick = Image1DblClick
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
      ExplicitLeft = 3
      ExplicitTop = -4
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 243
    Width = 809
    Height = 30
    Align = alBottom
    Caption = ' '
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -21
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 809
    Height = 28
    TabOrder = 2
    object SpeedButton2: TSpeedButton
      Left = 0
      Top = 0
      Width = 25
      Height = 22
      Hint = 'Save image'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333330070
        7700333333337777777733333333008088003333333377F73377333333330088
        88003333333377FFFF7733333333000000003FFFFFFF77777777000000000000
        000077777777777777770FFFFFFF0FFFFFF07F3333337F3333370FFFFFFF0FFF
        FFF07F3FF3FF7FFFFFF70F00F0080CCC9CC07F773773777777770FFFFFFFF039
        99337F3FFFF3F7F777F30F0000F0F09999937F7777373777777F0FFFFFFFF999
        99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
        99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
        93337FFFF7737777733300000033333333337777773333333333}
      NumGlyphs = 2
      OnClick = SpeedButton2Click
    end
    object SpeedButton7: TSpeedButton
      Left = 25
      Top = 0
      Width = 25
      Height = 22
      Hint = 'Copy to clipboard'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFF0000000000FFFFF0FFFFFFFFFF0FFFF0FFFFFFFFFF0FFFF0F9FFFFFFF
        F0FFFF0FF9FFFFFFF0FF9999999FFFFFF0FF99999999FFFFF0FF99999999FFFF
        F0FF9999999FFFFFF0FFFF0FF9FFFFFFF0FFFF0F9FFFFFFFF0FFFF0FFFDDDDFF
        F0FFFFF000DDDD000FFFFFFFFDDFFDDFFFFFFFFFFFDDDDFFFFFF}
      OnClick = SpeedButton7Click
    end
    object SpeedButton4: TSpeedButton
      Left = 50
      Top = 0
      Width = 25
      Height = 22
      Hint = 'Rescale graph'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00337333733373
        3373337F3F7F3F7F3F7F33737373737373733F7F7F7F7F7F7F7F770000000000
        000077777777777777773303333333333333337FF333333F33333709333333C3
        333337773F3FF373F333330393993C3C33333F7F7F77F7F7FFFF77079797977C
        77777777777777777777330339339333C333337FF73373F37F33370C333C3933
        933337773F3737F37FF33303C3C33939C9333F7F7F7FF7F777FF7707C7C77797
        7C97777777777777777733033C3333333C33337F37F33333373F37033C333333
        33C3377F37333333337333033333333333333F7FFFFFFFFFFFFF770777777777
        7777777777777777777733333333333333333333333333333333}
      NumGlyphs = 2
      OnClick = SpeedButton4Click
    end
    object SpeedButton5: TSpeedButton
      Left = 75
      Top = 0
      Width = 25
      Height = 22
      Hint = 'Drag resize'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        70777777777777070F07777777777700FFF077777777770FFF0777777777770F
        F077000700070000000707777777770777770777777777077777777777777777
        7777077777777707777707777777770777770777777777077777777777777777
        7777077777777707777707777777770777770007000700077777}
      OnClick = SpeedButton5Click
    end
    object SpeedButton8: TSpeedButton
      Left = 100
      Top = 0
      Width = 17
      Height = 22
      Hint = 'Force redraw'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
        FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
        CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
        FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
      OnClick = SpeedButton8Click
    end
    object LegendSpeedButton: TSpeedButton
      Left = 117
      Top = 0
      Width = 23
      Height = 22
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333300030003
        0003333377737773777333333333333333333FFFFFFFFFFFFFFF770000000000
        0000777777777777777733039993BBB3CCC3337F737F737F737F37039993BBB3
        CCC3377F737F737F737F33039993BBB3CCC33F7F737F737F737F77079997BBB7
        CCC77777737773777377330399930003CCC3337F737F7773737F370399933333
        CCC3377F737F3333737F330399933333CCC33F7F737FFFFF737F770700077777
        CCC77777777777777377330333333333CCC3337F33333333737F370333333333
        0003377F33333333777333033333333333333F7FFFFFFFFFFFFF770777777777
        7777777777777777777733333333333333333333333333333333}
      NumGlyphs = 2
      OnClick = LegendSpeedButtonClick
    end
    object SpeedButton6: TSpeedButton
      Left = 140
      Top = 0
      Width = 23
      Height = 22
      Glyph.Data = {
        42010000424D4201000000000000760000002800000013000000110000000100
        040000000000CC00000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFF00000FFFFFFFF0000FFFFFFF00000FFF99FFFF00FFFFFFFF00000FFF9
        9FFFF00FFFFFFFF00000F0FFFFFFF00FFFFFFFF00000FF0FFFFFF00FFFFFFFF0
        0000FFF0F99FF00780F000F00000FFFF099F000F00FFFFF00000FFFFF0FFFFFF
        FFFFF7F00000FFFFFF0FFFFFFFFFF0F00000FFFFFFF0FFFFFFF880F00000FFFF
        99FF0FF99FFFFFF00000FFFF99FFF0F99FFFFFF00000FFFFFFFFFF0FFFFFFFF0
        0000FFFFFFFFFFF0FFF99FF00000FFFFFFFFFFFFFFFF9FF00000FFFFFFFFFFFF
        FFFFFFF00000}
      OnClick = SpeedButton6Click
    end
    object SpeedButton9: TSpeedButton
      Left = 163
      Top = 0
      Width = 23
      Height = 22
      Hint = 'Increase font size'
      Glyph.Data = {
        66010000424D6601000000000000760000002800000017000000140000000100
        040000000000F000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFF0FFFFFFFFFFFFFFFFFFFFFFF0FFFFFFFFFFFFFFFFFFFFFFF0F000
        FFFFFFF000FFFFFFFFF0F000FFFFFFF000FFFFFFFFF0FF000FFFFF000FFFFFFF
        FFF0FF00000000000FFFFFFFFFF0FF00000000000FFFFFFFFFF0FFF000000000
        FFFFFFFFFFF0FFF000FFF000FFFFFFFFFFF0FFF000FFF000FFFFFFFFFFF0FFFF
        000F000FFFFFFFFFFFF0FFFF000F000F99999999FFF0FFFF000F000FF999999F
        FFF0FFFFF00000FFFF99999FFFF0FFFFF00000FFFF9999FFFFF0FFFFF00000FF
        FFF999FFFFF0FFFFFFFFFFFFFFFF9FFFFFF0FFFFFFFFFFFFFFFFFFFFFFF0FFFF
        FFFFFFFFFFFFFFFFFFF0}
      OnClick = SpeedButton9Click
    end
    object SpeedButton10: TSpeedButton
      Left = 186
      Top = 0
      Width = 23
      Height = 22
      Hint = 'Decrease font size'
      Glyph.Data = {
        4E010000424D4E01000000000000760000002800000018000000120000000100
        040000000000D800000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFFF00FFFFFFFFFFFFFFF
        00FFFFF00FFFFFFFFFFFFFFF00FFFFF00FFFFFFFFFFFFFFFF0000000FFFFFFFF
        FFFFFFFFF0000000FFFFFFFFFFFFFFFFF00FFF00FFFFFFFFFFFFFFFFFF00F00F
        FFFFFFFFFFFFFFFFFF00F00FFFFFFFFFFFFFFFFFFF00F00FFFF9FFFFFFFFFFFF
        FFF000FFFF999FFFFFFFFFFFFFF000FFFF9999FFFFFFFFFFFFFFFFFFF99999FF
        FFFFFFFFFFFFFFFFF999999FFFFFFFFFFFFFFFFF99999999FFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      OnClick = SpeedButton10Click
    end
    object IDSpeedButton: TSpeedButton
      Left = 209
      Top = 0
      Width = 23
      Height = 22
      Hint = 'Identify database record'
      Caption = 'ID'
      Visible = False
      OnClick = IDSpeedButtonClick
    end
    object SpeedButton11: TSpeedButton
      Left = 232
      Top = 0
      Width = 23
      Height = 22
      Hint = 'Measure distance'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        707777777777777770777777777777700000777777777777707777777777777C
        70777777777777C77777777777777C77777777777777C7777777777777777777
        7777777777C77777777777777C77777777777707C77777777777770777777777
        7777000007777777777777077777777777777707777777777777}
      OnClick = SpeedButton11Click
    end
    object SpeedButton15: TSpeedButton
      Left = 255
      Top = 0
      Width = 23
      Height = 22
      Hint = 'Measure slope between points'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        F0FFFFFFFFFFFFFFF0FFFFFFFFFFFFF00000FFFFFFFFFFFFF0FFFFFFFFFFFFFC
        F0FFFFFF99F99F99FFFFFFFF99F99C99FFFFFFFF99F99F99FFFFFFFF99F99F99
        FFFFFFFF99C99F99FFFFFFFF9999F99FFFFFFF0FCFFFFFFFFFFFFF0FFFFFFFFF
        FFFF00000FFFFFFFFFFFFF0FFFFFFFFFFFFFFF0FFFFFFFFFFFFF}
      OnClick = SpeedButton15Click
    end
    object SpeedButton12: TSpeedButton
      Left = 278
      Top = 0
      Width = 23
      Height = 22
      Hint = 'Save image'
      Caption = 'S'
      OnClick = SpeedButton12Click
    end
    object SpeedButton13: TSpeedButton
      Left = 301
      Top = 0
      Width = 23
      Height = 22
      Hint = 'Restore image'
      Caption = 'R'
      Enabled = False
      OnClick = SpeedButton13Click
    end
    object SpeedButton14: TSpeedButton
      Left = 324
      Top = 0
      Width = 23
      Height = 22
      Caption = 'X'
      OnClick = SpeedButton14Click
    end
  end
  object MainMenu1: TMainMenu
    Left = 24
    Top = 48
    object File1: TMenuItem
      Caption = '&File (Graph)'
      GroupIndex = 1
      object Separatehistograms1: TMenuItem
        Caption = 'Separate histograms'
        OnClick = Separatehistograms1Click
      end
      object Saveimage1: TMenuItem
        Caption = '&Save image'
        OnClick = Saveimage1Click
      end
      object Copytoclipboard1: TMenuItem
        Caption = 'Copy to clipboard'
        OnClick = Copytoclipboard1Click
      end
      object Showtoolbar1: TMenuItem
        Caption = 'Show toolbar'
        Checked = True
        OnClick = Showtoolbar1Click
      end
      object Legend1: TMenuItem
        Caption = 'Legend'
        OnClick = Legend1Click
      end
      object Close1: TMenuItem
        Caption = '&Close'
        OnClick = Close1Click
      end
    end
    object Rescale1: TMenuItem
      Caption = 'Res&cale'
      GroupIndex = 1
      Hint = 'Modify graph'
      object Graphsettings2: TMenuItem
        Caption = '&Graph settings'
        OnClick = Graphsettings2Click
      end
      object N11scaling1: TMenuItem
        Caption = '1:1 scaling'
        OnClick = N11scaling1Click
      end
      object Blowupmap1: TMenuItem
        Caption = 'Blow up graph'
        OnClick = Blowupmap1Click
      end
      object Dragresize1: TMenuItem
        Caption = 'Drag resize'
        OnClick = Dragresize1Click
      end
      object Keyboardresize1: TMenuItem
        Caption = 'Keyboard resize'
        OnClick = Keyboardresize1Click
      end
      object Font1: TMenuItem
        Caption = '&Font'
        GroupIndex = 1
        OnClick = Font1Click
      end
      object LineandPointMarkers2: TMenuItem
        Caption = 'Line and Point Markers'
        GroupIndex = 1
        OnClick = LineandPointMarkers2Click
      end
      object Bestfitlinecolor1: TMenuItem
        Caption = 'Best fit line color'
        GroupIndex = 1
        OnClick = Bestfitlinecolor1Click
      end
      object Labelpointsatopprofile1: TMenuItem
        Caption = 'Label points atop profile'
        GroupIndex = 1
        OnClick = Labelpointsatopprofile1Click
      end
      object Blackandwhitegraph1: TMenuItem
        Caption = 'Black and white graph'
        GroupIndex = 1
        OnClick = Blackandwhitegraph1Click
      end
      object Zcolorrange1: TMenuItem
        Caption = 'Z color range'
        GroupIndex = 1
        OnClick = Zcolorrange1Click
      end
      object extinlowerleftcorner1: TMenuItem
        Caption = 'Text in lower left corner'
        GroupIndex = 1
        OnClick = extinlowerleftcorner1Click
      end
      object extinlowerrightcorner1: TMenuItem
        Caption = 'Text in lower right corner'
        GroupIndex = 1
        OnClick = extinlowerrightcorner1Click
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit points'
      GroupIndex = 1
      object Freedrag1: TMenuItem
        Caption = 'Free drag'
        OnClick = Freedrag1Click
      end
      object DragInXDirection1: TMenuItem
        Caption = 'Drag in X direction'
        OnClick = DragInXDirection1Click
      end
      object DraginYdirection1: TMenuItem
        Caption = 'Drag in Y direction'
        OnClick = DraginYdirection1Click
      end
    end
    object Analyze1: TMenuItem
      Caption = '&Analyze Graph'
      GroupIndex = 1
      Hint = 'Data on graph'
      object Filter1: TMenuItem
        Caption = 'Filter'
        object None1: TMenuItem
          Caption = '&None'
          Checked = True
          OnClick = None1Click
        end
        object N3term1: TMenuItem
          Caption = '&3 term'
          OnClick = N3term1Click
        end
        object N5term1: TMenuItem
          Caption = '&5 term'
          OnClick = N5term1Click
        end
        object n7term1: TMenuItem
          Caption = '&7 term'
          OnClick = n7term1Click
        end
        object N9term1: TMenuItem
          Caption = '&9 term'
          OnClick = N9term1Click
        end
        object Custom1: TMenuItem
          Caption = 'Custom'
          OnClick = Custom1Click
        end
        object N1: TMenuItem
          Caption = '-'
        end
        object Medianalongx1: TMenuItem
          Caption = 'Median along x'
          OnClick = Medianalongx1Click
        end
      end
      object FFT1: TMenuItem
        Caption = '&FFT'
        object AlongXaxis1: TMenuItem
          Caption = 'Along &X axis'
          OnClick = AlongXaxis1Click
        end
        object AlongYaxis1: TMenuItem
          Caption = 'Along &Y axis'
          OnClick = AlongYaxis1Click
        end
      end
      object Autocorrelation1: TMenuItem
        Caption = 'Autocorrelation'
        object Alongxaxis2: TMenuItem
          Caption = 'Along x axis'
          OnClick = Alongxaxis2Click
        end
        object Alongyaxis2: TMenuItem
          Caption = 'Along y axis'
          OnClick = Alongyaxis2Click
        end
      end
      object Crosscorrelation1: TMenuItem
        Caption = 'Cross correlation'
        object Alongxaxis3: TMenuItem
          Caption = 'Along x axis'
          OnClick = Alongxaxis3Click
        end
      end
      object Fitfouriercurve1: TMenuItem
        Caption = 'Fit fourier curve'
        object Aongxaxis1: TMenuItem
          Caption = 'Aong x axis'
          OnClick = Aongxaxis1Click
        end
      end
      object Scale1: TMenuItem
        Caption = 'Scale &Y axis'
        OnClick = Scale1Click
      end
      object Viewdata1: TMenuItem
        Caption = '&View data'
        OnClick = Viewdata1Click
      end
      object Linearfit2: TMenuItem
        Caption = 'Linear fit'
        object Linearfit1: TMenuItem
          Caption = 'Entire data set'
          OnClick = Linearfit1Click
        end
        object Ongraphdataonly1: TMenuItem
          Caption = 'On graph data only'
          OnClick = Ongraphdataonly1Click
        end
      end
      object FindpeakYvalueineachseries1: TMenuItem
        Caption = 'Find peak Y value in each series (mode)'
        OnClick = FindpeakYvalueineachseries1Click
      end
      object Monthlyaverage1: TMenuItem
        Caption = 'Monthly average'
        OnClick = Monthlyaverage1Click
      end
      object Zcolorscalelegend1: TMenuItem
        Caption = 'Z color scale legend'
        OnClick = Zcolorscalelegend1Click
      end
      object PointDensity2: TMenuItem
        Caption = 'Point Density'
        object Pointdensity1: TMenuItem
          Caption = 'All points'
          OnClick = Pointdensity1Click
        end
        object Xaxis1: TMenuItem
          Caption = 'X- axis'
          OnClick = Xaxis1Click
        end
        object Yaxis1: TMenuItem
          Caption = 'Y-axis'
          OnClick = Yaxis1Click
        end
      end
    end
    object Option1: TMenuItem
      Caption = 'Graph &Options'
      GroupIndex = 1
      Hint = 'For graph'
      Visible = False
      object Graphparameters1: TMenuItem
        Caption = 'Graph &Size'
        GroupIndex = 1
        OnClick = Graphparameters1Click
      end
      object ContourLineWidth1: TMenuItem
        Caption = 'Contour Line Width'
        GroupIndex = 1
        OnClick = ContourLineWidth1Click
      end
    end
    object Hidden1: TMenuItem
      Caption = 'Hidden'
      GroupIndex = 3
      Visible = False
      object ChangeGraphSettings: TMenuItem
        Caption = 'ChangeGraphSettings'
        OnClick = ChangeGraphSettingsClick
      end
      object RedrawDiagram11: TMenuItem
        Caption = 'RedrawDiagram1'
        OnClick = RedrawDiagram11Click
      end
    end
  end
  object ColorDialog1: TColorDialog
    Left = 32
    Top = 112
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 168
    Top = 88
  end
  object PopupMenu1: TPopupMenu
    Left = 304
    Top = 64
    object Rescalegraph1: TMenuItem
      Caption = 'Rescale graph'
      OnClick = Rescalegraph1Click
    end
    object Blowupgraph1: TMenuItem
      Caption = 'Blow up graph'
      OnClick = Blowupgraph1Click
    end
    object Lidarpanoramalimits1: TMenuItem
      Caption = 'Lidar panorama limits'
      OnClick = Lidarpanoramalimits1Click
    end
    object Lineandpointmarkers1: TMenuItem
      Caption = 'Line and point markers'
      OnClick = Lineandpointmarkers1Click
    end
    object Legend2: TMenuItem
      Caption = 'Legend'
      object Pasteontograph1: TMenuItem
        Caption = 'Paste onto graph'
        OnClick = Pasteontograph1Click
      end
      object Pasteontograph2: TMenuItem
        Caption = 'New window'
        OnClick = Pasteontograph2Click
      end
      object Copytoclipboard3: TMenuItem
        Caption = 'Copy to clipboard'
      end
    end
    object Grayscalegraph1: TMenuItem
      Caption = 'Grayscale graph'
      OnClick = Grayscalegraph1Click
    end
    object Animate1: TMenuItem
      Caption = 'Animate'
      OnClick = Animate1Click
    end
    object Imagewithseparatalayers1: TMenuItem
      Caption = 'Image with separata layers in panels'
      OnClick = Imagewithseparatalayers1Click
    end
    object Saveimage2: TMenuItem
      Caption = 'Save image'
      OnClick = Saveimage2Click
    end
    object Copytoclipboard2: TMenuItem
      Bitmap.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFF0000000000FFFFF0FFFFFFFFFF0FFFF0FFFFFFFFFF0FFFF0F9FFFFFFF
        F0FFFF0FF9FFFFFFF0FF9999999FFFFFF0FF99999999FFFFF0FF99999999FFFF
        F0FF9999999FFFFFF0FFFF0FF9FFFFFFF0FFFF0F9FFFFFFFF0FFFF0FFFDDDDFF
        F0FFFFF000DDDD000FFFFFFFFDDFFDDFFFFFFFFFFFDDDDFFFFFF}
      Caption = 'Copy to clipboard'
      OnClick = Copytoclipboard2Click
    end
    object Copytoclipboardwithaddedlegend1: TMenuItem
      Caption = 'Copy to clipboard with added legend'
      OnClick = Copytoclipboardwithaddedlegend1Click
    end
    object Pastefromclipboard1: TMenuItem
      Caption = 'Paste from clipboard'
      OnClick = Pastefromclipboard1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object DifferentSatellite1: TMenuItem
      Caption = 'Different satellite'
      object Landsat1: TMenuItem
        Caption = 'Landsat 8/9'
        OnClick = Landsat1Click
      end
      object Landsat2: TMenuItem
        Caption = 'Older Landsat TM/ETM'
        OnClick = Landsat2Click
      end
      object Sentinel21: TMenuItem
        Caption = 'Sentinel-2'
        OnClick = Sentinel21Click
      end
      object Sentinel22: TMenuItem
        Caption = 'World View 2'
        OnClick = Sentinel22Click
      end
      object Spot51: TMenuItem
        Caption = 'Spot 5'
        OnClick = Spot51Click
      end
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Abortcurrentoperation1: TMenuItem
      Caption = 'Abort current operation'
      OnClick = Abortcurrentoperation1Click
    end
  end
end
