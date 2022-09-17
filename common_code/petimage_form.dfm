object ImageDisplayForm: TImageDisplayForm
  Left = 180
  Top = 250
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'ImageDisplayForm'
  ClientHeight = 732
  ClientWidth = 933
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  FormStyle = fsMDIChild
  Menu = MainMenu1
  Visible = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 16
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 26
    Width = 933
    Height = 665
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 939
    ExplicitHeight = 699
    object Image1: TImage
      Left = -2
      Top = 3
      Width = 425
      Height = 209
      AutoSize = True
      OnDblClick = Image1DblClick
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 691
    Width = 933
    Height = 0
    Align = alBottom
    Caption = 'Panel1'
    TabOrder = 1
    ExplicitTop = 725
    ExplicitWidth = 939
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 933
    Height = 26
    ButtonHeight = 24
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    object SpeedButton2: TSpeedButton
      Left = 0
      Top = 0
      Width = 25
      Height = 24
      Hint = 'Save image'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
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
    object ClipboardSpeedButton: TSpeedButton
      Left = 25
      Top = 0
      Width = 25
      Height = 24
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
      OnClick = ClipboardSpeedButtonClick
    end
    object SpeedButton4: TSpeedButton
      Left = 50
      Top = 0
      Width = 25
      Height = 24
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333333FFF33FFFFF33333300033000
        00333337773377777333333330333300033333337FF33777F333333330733300
        0333333377FFF777F33333333700000073333333777777773333333333033000
        3333333337FF777F333333333307300033333333377F777F3333333333703007
        33333333377F7773333333333330000333333333337777F33333333333300003
        33333333337777F3333333333337007333333333337777333333333333330033
        3333333333377333333333333333033333333333333733333333333333333333
        3333333333333333333333333333333333333333333333333333}
      NumGlyphs = 2
      OnClick = SpeedButton4Click
    end
    object SpeedButton5: TSpeedButton
      Left = 75
      Top = 0
      Width = 25
      Height = 24
      Hint = 'Help'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333FFFFF3333333333F797F3333333333F737373FF333333BFB999BFB
        33333337737773773F3333BFBF797FBFB33333733337333373F33BFBFBFBFBFB
        FB3337F33333F33337F33FBFBFB9BFBFBF3337333337F333373FFBFBFBF97BFB
        FBF37F333337FF33337FBFBFBFB99FBFBFB37F3333377FF3337FFBFBFBFB99FB
        FBF37F33333377FF337FBFBF77BF799FBFB37F333FF3377F337FFBFB99FB799B
        FBF373F377F3377F33733FBF997F799FBF3337F377FFF77337F33BFBF99999FB
        FB33373F37777733373333BFBF999FBFB3333373FF77733F7333333BFBFBFBFB
        3333333773FFFF77333333333FBFBF3333333333377777333333}
      NumGlyphs = 2
    end
    object RedrawSpeedButton12: TSpeedButton
      Left = 100
      Top = 0
      Width = 25
      Height = 24
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
    end
    object SpeedButton7: TSpeedButton
      Left = 125
      Top = 0
      Width = 23
      Height = 24
      Hint = 'Geolocate on image'
      Caption = '+?'
    end
    object SpeedButton8: TSpeedButton
      Left = 148
      Top = 0
      Width = 28
      Height = 24
      Hint = 'Digitize on image'
      Caption = 'Dig'
    end
    object SpeedButton6: TSpeedButton
      Left = 176
      Top = 0
      Width = 23
      Height = 24
      Caption = '>'
      OnClick = SpeedButton6Click
    end
    object V: TSpeedButton
      Left = 199
      Top = 0
      Width = 23
      Height = 24
      Caption = 'V'
    end
    object SpeedButton9: TSpeedButton
      Left = 222
      Top = 0
      Width = 23
      Height = 24
      Caption = '<'
    end
    object SpeedButton10: TSpeedButton
      Left = 245
      Top = 0
      Width = 23
      Height = 24
      Caption = '^'
      OnClick = SpeedButton10Click
    end
    object ZoomInSpeedButton4: TSpeedButton
      Left = 268
      Top = 0
      Width = 25
      Height = 24
      Hint = 'Zoom in'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33033333333333333F7F3333333333333000333333333333F777333333333333
        000333333333333F777333333333333000333333333333F77733333333333300
        033333333FFF3F777333333700073B703333333F7773F77733333307777700B3
        33333377333777733333307F8F8F7033333337F333F337F3333377F8F9F8F773
        3333373337F3373F3333078F898F870333337F33F7FFF37F333307F99999F703
        33337F377777337F3333078F898F8703333373F337F33373333377F8F9F8F773
        333337F3373337F33333307F8F8F70333333373FF333F7333333330777770333
        333333773FF77333333333370007333333333333777333333333}
      NumGlyphs = 2
    end
    object ZoomOutSpeedButton5: TSpeedButton
      Left = 293
      Top = 0
      Width = 25
      Height = 24
      Hint = 'Zoom out'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33033333333333333F7F3333333333333000333333333333F777333333333333
        000333333333333F777333333333333000333333333333F77733333333333300
        033333333FFF3F777333333700073B703333333F7773F77733333307777700B3
        333333773337777333333078F8F87033333337F3333337F33333778F8F8F8773
        333337333333373F333307F8F8F8F70333337F33FFFFF37F3333078999998703
        33337F377777337F333307F8F8F8F703333373F3333333733333778F8F8F8773
        333337F3333337F333333078F8F870333333373FF333F7333333330777770333
        333333773FF77333333333370007333333333333777333333333}
      NumGlyphs = 2
    end
    object SpeedButton11: TSpeedButton
      Left = 318
      Top = 0
      Width = 23
      Height = 24
      Caption = 'r+'
      OnClick = SpeedButton11Click
    end
    object SpeedButton14: TSpeedButton
      Left = 341
      Top = 0
      Width = 23
      Height = 24
      Caption = '<'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'System'
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
      OnClick = SpeedButton14Click
    end
    object SpeedButton15: TSpeedButton
      Left = 364
      Top = 0
      Width = 23
      Height = 24
      Caption = '>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'System'
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
      OnClick = SpeedButton15Click
    end
    object SpeedButton16: TSpeedButton
      Left = 387
      Top = 0
      Width = 23
      Height = 24
      Caption = 'X'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'System'
      Font.Style = [fsBold, fsItalic]
      ParentFont = False
      Visible = False
      OnClick = SpeedButton16Click
    end
    object SpeedButton17: TSpeedButton
      Left = 410
      Top = 0
      Width = 37
      Height = 24
      Caption = 'Kml'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
      OnClick = SpeedButton17Click
    end
    object SpeedButton3: TSpeedButton
      Left = 447
      Top = 0
      Width = 49
      Height = 24
      Caption = 'On top'
      OnClick = SpeedButton3Click
    end
    object ComboBox1: TComboBox
      Left = 496
      Top = 0
      Width = 81
      Height = 24
      ItemIndex = 2
      TabOrder = 0
      Text = '100%'
      OnChange = ComboBox1Change
      Items.Strings = (
        '400%'
        '200%'
        '100%'
        '50%'
        '33%'
        '25%'
        '20%'
        '12.5%')
    end
    object BitBtn2: TBitBtn
      Left = 577
      Top = 0
      Width = 75
      Height = 24
      Caption = 'Prnt Scr'
      TabOrder = 1
      OnClick = BitBtn2Click
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 691
    Width = 933
    Height = 0
    Panels = <
      item
        Width = 100
      end
      item
        Width = 50
      end>
    ExplicitTop = 725
    ExplicitWidth = 939
  end
  object Panel2: TPanel
    Left = 0
    Top = 691
    Width = 933
    Height = 41
    Align = alBottom
    TabOrder = 4
    ExplicitTop = 725
    ExplicitWidth = 939
    object TrackBar1: TTrackBar
      Left = 240
      Top = 6
      Width = 539
      Height = 39
      Max = 255
      Frequency = 10
      Position = 122
      TabOrder = 0
      OnChange = TrackBar1Change
    end
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 248
      Height = 39
      Align = alLeft
      TabOrder = 1
      object Label1: TLabel
        Left = 121
        Top = 8
        Width = 115
        Height = 16
        Caption = 'Alpha blend:  50%'
      end
      object SpeedButton12: TSpeedButton
        Left = 6
        Top = 8
        Width = 25
        Height = 25
        Hint = 'Save alpah blend  image'
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
        OnClick = SpeedButton12Click
      end
      object SpeedButton13: TSpeedButton
        Left = 56
        Top = 8
        Width = 23
        Height = 25
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFF0000000000FFFFF0FFFFFFFFFF0FFFF0FFFFFFFFFF0FFFF0F9FFFFFFF
          F0FFFF0FF9FFFFFFF0FF9999999FFFFFF0FF99999999FFFFF0FF99999999FFFF
          F0FF9999999FFFFFF0FFFF0FF9FFFFFFF0FFFF0F9FFFFFFFF0FFFF0FFFDDDDFF
          F0FFFFF000DDDD000FFFFFFFFDDFFDDFFFFFFFFFFFDDDDFFFFFF}
        OnClick = SpeedButton13Click
      end
      object CancelBtn: TSpeedButton
        Left = 81
        Top = 8
        Width = 25
        Height = 25
        Glyph.Data = {
          DE010000424DDE01000000000000760000002800000024000000120000000100
          0400000000006801000000000000000000001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          333333333333333333333333000033338833333333333333333F333333333333
          0000333911833333983333333388F333333F3333000033391118333911833333
          38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
          911118111118333338F3338F833338F3000033333911111111833333338F3338
          3333F8330000333333911111183333333338F333333F83330000333333311111
          8333333333338F3333383333000033333339111183333333333338F333833333
          00003333339111118333333333333833338F3333000033333911181118333333
          33338333338F333300003333911183911183333333383338F338F33300003333
          9118333911183333338F33838F338F33000033333913333391113333338FF833
          38F338F300003333333333333919333333388333338FFF830000333333333333
          3333333333333333333888330000333333333333333333333333333333333333
          0000}
        NumGlyphs = 2
        OnClick = CancelBtnClick
      end
      object BitBtn1: TBitBtn
        Left = 29
        Top = 8
        Width = 27
        Height = 25
        Hint = 'Save alpha blend movie'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000000
          033333FFFF77777773F330000077777770333777773FFFFFF733077777000000
          03337F3F3F777777733F0797A770003333007F737337773F3377077777778803
          30807F333333337FF73707888887880007707F3FFFF333777F37070000878807
          07807F777733337F7F3707888887880808807F333333337F7F37077777778800
          08807F333FFF337773F7088800088803308073FF777FFF733737300008000033
          33003777737777333377333080333333333333F7373333333333300803333333
          33333773733333333333088033333333333373F7F33333333333308033333333
          3333373733333333333333033333333333333373333333333333}
        NumGlyphs = 2
        TabOrder = 0
        OnClick = BitBtn1Click
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 153
    Top = 177
    object File1: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Caption = '&New image'
        OnClick = New1Click
      end
      object Replaceimage1: TMenuItem
        Caption = '&Replace image'
        OnClick = Replaceimage1Click
      end
      object Reloadimage1: TMenuItem
        Caption = 'Reload image'
      end
      object Refresh1: TMenuItem
        Caption = 'Refresh'
      end
      object Saveimage1: TMenuItem
        Caption = '&Save image'
        Enabled = False
        OnClick = Saveimage1Click
      end
      object Saveimageas1: TMenuItem
        Caption = 'Save image as'
        OnClick = Saveimageas1Click
      end
      object Pastefromclipboard1: TMenuItem
        Caption = 'Paste from clipboard'
      end
      object Alphablending1: TMenuItem
        Caption = 'Alpha blending'
        OnClick = Alphablending1Click
      end
      object ransparentGIFS1: TMenuItem
        Caption = 'Transparent GIFS'
        OnClick = ransparentGIFS1Click
      end
      object PutEXIFdataintoJPEGS1: TMenuItem
        Caption = 'Clone EXIF data into JPEGS'
        OnClick = PutEXIFdataintoJPEGS1Click
      end
      object Reviewdirectory1: TMenuItem
        Caption = 'Review directory'
        OnClick = Reviewdirectory1Click
      end
      object Subsetpictures1: TMenuItem
        Caption = 'Subset pictures'
        object Forceaspectratio1: TMenuItem
          Caption = 'Aspect ratio'
          OnClick = Forceaspectratio1Click
        end
        object Pickaspectratio1: TMenuItem
          Caption = 'Pick aspect ratio'
          OnClick = Pickaspectratio1Click
        end
        object Anyaspectratio1: TMenuItem
          Caption = 'Any aspect ratio'
          OnClick = Anyaspectratio1Click
        end
      end
      object ools1: TMenuItem
        Caption = 'Tools'
        OnClick = ools1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Caption = '&Close'
        OnClick = Close1Click
      end
    end
    object Calculate1: TMenuItem
      Caption = '&Calculate'
      object Distance1: TMenuItem
        Caption = '&Distance'
        OnClick = Distance1Click
      end
    end
    object Flood1: TMenuItem
      Caption = 'F&lood'
      object Fill1: TMenuItem
        Caption = '&Fill'
        OnClick = Fill1Click
      end
    end
    object Recolor1: TMenuItem
      Caption = '&Recolor'
      object Blackandwhite1: TMenuItem
        Caption = '&Black and white'
        object ColorstoWhite1: TMenuItem
          Caption = 'Colors to &Black'
          OnClick = ColorstoWhite1Click
        end
        object ColorstoWhite2: TMenuItem
          Caption = 'Colors to &White'
          OnClick = ColorstoWhite2Click
        end
      end
      object Negative1: TMenuItem
        Caption = '&Negative'
        OnClick = Negative1Click
      end
      object Specifiedcolors1: TMenuItem
        Caption = '&Specified colors'
        OnClick = Specifiedcolors1Click
      end
      object Leavesinglecolor1: TMenuItem
        Caption = '&Leave single color'
        OnClick = Leavesinglecolor1Click
      end
      object Convertwhitetonearwhite1: TMenuItem
        Caption = 'Convert white to near white'
        OnClick = Convertwhitetonearwhite1Click
      end
      object MakeGrayscale1: TMenuItem
        Caption = 'Make Grayscale'
        OnClick = MakeGrayscale1Click
      end
      object Strecchgrayscale1: TMenuItem
        Caption = 'Stretch grayscale'
        OnClick = Strecchgrayscale1Click
      end
      object Subdueimage1: TMenuItem
        Caption = 'Subdue image'
        OnClick = Subdueimage1Click
      end
      object Removenongraypixels1: TMenuItem
        Caption = 'Remove non-gray pixels'
        OnClick = Removenongraypixels1Click
      end
      object Removegrays1: TMenuItem
        Caption = '&Remove grays above threhhold'
        OnClick = Removegrays1Click
      end
      object Colorize1: TMenuItem
        Caption = '&Colorize'
        object Red1: TMenuItem
          Caption = '&Red'
          OnClick = Red1Click
        end
        object Green1: TMenuItem
          Caption = '&Green'
          OnClick = Green1Click
        end
        object Blue1: TMenuItem
          Caption = '&Blue'
          OnClick = Blue1Click
        end
        object Cyan1: TMenuItem
          Caption = 'Cyan'
          OnClick = Cyan1Click
        end
        object Yellow1: TMenuItem
          Caption = 'Yellow'
        end
        object Magenta1: TMenuItem
          Caption = 'Magenta'
          OnClick = Magenta1Click
        end
        object MakeRGBseparates1: TMenuItem
          Caption = 'Make R/G/B separates'
        end
        object MakeRGBgrayscales1: TMenuItem
          Caption = 'Make R/G/B grayscales'
        end
        object MakeRGBandgrayscaleseparates1: TMenuItem
          Caption = 'Make RGB and grayscale separates'
          OnClick = MakeRGBandgrayscaleseparates1Click
        end
        object MakeIHSseparates1: TMenuItem
          Caption = 'Make HLS separates'
          OnClick = MakeIHSseparates1Click
        end
      end
      object MergeRGBseparates1: TMenuItem
        Caption = 'Merge RGB separates'
        OnClick = MergeRGBseparates1Click
      end
      object Getcolorpalette1: TMenuItem
        Caption = 'Get color palette'
        OnClick = Getcolorpalette1Click
      end
      object Picksolidcolor1: TMenuItem
        Caption = 'Pick solid color'
      end
      object Horizontalpalette1: TMenuItem
        Caption = 'Horizontal palette'
        OnClick = Horizontalpalette1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Colorcrosssections1: TMenuItem
        Caption = 'Color cross sections'
      end
      object Pickhorizon1: TMenuItem
        Caption = 'Pick horizon'
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Dilateimage1: TMenuItem
        Caption = 'Dilate image'
        OnClick = Dilateimage1Click
      end
      object Erodeimage1: TMenuItem
        Caption = 'Erode image'
        OnClick = Erodeimage1Click
      end
      object Skeletonize1: TMenuItem
        Caption = 'Skeletonize binary (Black/white) image'
        OnClick = Skeletonize1Click
      end
      object Smoothingfilter1: TMenuItem
        Caption = 'Smoothing filter'
        OnClick = Smoothingfilter1Click
      end
      object Removewhitemargins1: TMenuItem
        Caption = 'Remove white margins'
        OnClick = Removewhitemargins1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Batchprocess1: TMenuItem
        Caption = 'Batch process'
        OnClick = Batchprocess1Click
      end
    end
    object Text1: TMenuItem
      Caption = '&Text'
      object Add1: TMenuItem
        Caption = '&Add'
        OnClick = Add1Click
      end
    end
    object Rotate1: TMenuItem
      Caption = 'Rotate'
      object Specifiedangle1: TMenuItem
        Caption = 'Specified angle'
        OnClick = Specifiedangle1Click
      end
      object Linetohorizontal1: TMenuItem
        Caption = 'Line to horizontal'
        OnClick = Linetohorizontal1Click
      end
      object Linetovertical1: TMenuItem
        Caption = 'Line to vertical'
        OnClick = Linetovertical1Click
      end
      object N90degrees1: TMenuItem
        Caption = '90 degrees CW'
        OnClick = N90degrees1Click
      end
      object N180degrees1: TMenuItem
        Caption = '180 degrees'
        OnClick = N180degrees1Click
      end
      object N270degrees1: TMenuItem
        Caption = '270 degrees CW'
        OnClick = N270degrees1Click
      end
    end
    object Zoom1: TMenuItem
      Caption = 'Zoom'
    end
    object Options1: TMenuItem
      Caption = 'Image options'
      object ShowPosition1: TMenuItem
        Caption = 'Show Position'
        OnClick = ShowPosition1Click
      end
      object Showcolors1: TMenuItem
        Caption = 'Show colors'
        OnClick = Showcolors1Click
      end
      object Changecolumns1: TMenuItem
        Caption = 'Change columns'
        Visible = False
        OnClick = Changecolumns1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'All images|*.bmp;*.gif;*.jpg;*.tif|BMP Images|*.BMP|JPEG Images|' +
      '*.JPG|GIF Images|*.GIF|TIFF Images|*.TIF'
    Left = 49
    Top = 121
  end
  object ColorDialog1: TColorDialog
    Left = 290
    Top = 122
  end
  object ColorDialog2: TColorDialog
    Left = 370
    Top = 130
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 
      'All |*.jpg;*.jpeg;*.bmp;*.gif;*.tif;*.tiff;*.png;|JPEG|*.jpg;*.j' +
      'peg|GIF|*.gif|Bitmaps |*.bmp|TIFF|*.tif;*.tiff|PNG|*.png'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Title = 'Image'
    Left = 72
    Top = 56
  end
  object PopupMenu1: TPopupMenu
    Left = 136
    Top = 88
    object Saveimage2: TMenuItem
      Caption = 'Save image'
      OnClick = Saveimage2Click
    end
    object Pastefromclipboard2: TMenuItem
      Caption = 'Paste new image from clipboard'
    end
    object Overlaynewimagefromclipboard1: TMenuItem
      Caption = 'Overlay new image from clipboard'
      OnClick = Overlaynewimagefromclipboard1Click
    end
    object Copyimagetoclipboard1: TMenuItem
      Caption = 'Copy image to clipboard'
      OnClick = Copyimagetoclipboard1Click
    end
    object Makethisupperleftcorner1: TMenuItem
      Caption = 'Make this upper left corner'
    end
    object Makethislowerrightcorner1: TMenuItem
      Caption = 'Make this lower right corner'
    end
    object Startregionselection1: TMenuItem
      Caption = 'Start region selection'
    end
    object Pickthispointforsolidcolor1: TMenuItem
      Caption = 'Pick this point for solid color'
    end
    object Solid1: TMenuItem
      Caption = 'Solid color'
      object Belowthispoint1: TMenuItem
        Caption = 'Below this point'
        OnClick = Belowthispoint1Click
      end
      object Rightofthispoint1: TMenuItem
        Caption = 'Right of this point'
        OnClick = Rightofthispoint1Click
      end
      object Leftofthispoint1: TMenuItem
        Caption = 'Left of this point'
        OnClick = Leftofthispoint1Click
      end
      object Abovethispoint1: TMenuItem
        Caption = 'Above this point'
        OnClick = Abovethispoint1Click
      end
    end
    object CopytColortoclipboard1: TMenuItem
      Caption = 'Copy tColor to clipboard'
      OnClick = CopytColortoclipboard1Click
    end
  end
  object PopupMenu2: TPopupMenu
    Left = 248
    Top = 194
    object Savesubset1: TMenuItem
      Caption = 'Save subset'
      OnClick = Savesubset1Click
    end
    object Subsetthispicture1: TMenuItem
      Caption = 'Subset this picture'
      OnClick = Subsetthispicture1Click
    end
    object Usethissubsetonmultiplebitmaps1: TMenuItem
      Caption = 'Use this subset on multiple bitmaps'
    end
  end
end
