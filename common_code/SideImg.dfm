object sideimage: Tsideimage
  Left = 202
  Top = 316
  Caption = 'XTF Side Scan Sonar Imagery'
  ClientHeight = 471
  ClientWidth = 711
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'System'
  Font.Style = []
  FormStyle = fsMDIChild
  Menu = MainMenu1
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 16
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 29
    Width = 711
    Height = 442
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 725
    ExplicitHeight = 518
    object Image1: TImage
      Left = 0
      Top = 2
      Width = 593
      Height = 201
      OnDblClick = Image1DblClick
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 711
    Height = 29
    ButtonHeight = 25
    Caption = 'ToolBar1'
    TabOrder = 1
    ExplicitWidth = 719
    object SpeedButton4: TSpeedButton
      Left = 0
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Save image in file'
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
      OnClick = SpeedButton4Click
    end
    object SpeedButton3: TSpeedButton
      Left = 25
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Copy image to clipboard'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFF0000000000FFFFF0FFFFFFFFFF0FFFF0FFFFFFFFFF0FFFF0F9FFFFFFF
        F0FFFF0FF9FFFFFFF0FF9999999FFFFFF0FF99999999FFFFF0FF99999999FFFF
        F0FF9999999FFFFFF0FFFF0FF9FFFFFFF0FFFF0F9FFFFFFFF0FFFF0FFFDDDDFF
        F0FFFFF000DDDD000FFFFFFFFDDFFDDFFFFFFFFFFFDDDDFFFFFF}
      OnClick = SpeedButton3Click
    end
    object SpeedButton2: TSpeedButton
      Left = 50
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Full size image'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33033333333333333F8F3333333333333000333333333333F888333333333333
        000333333333333F888333333333333000333333333333F88833333333333300
        033333333FFF3F888333333800083B803333333F8883F8883333330FFFFF00B3
        3333338833388883333330FFFFFFF033333338F3333338F333338FFFF0FFFF83
        333338333333383F33330FFFF0FFFF0333338F333333338F33330FFFF0FFFF03
        33338F333333338F33330FFFF0FFFF03333383F33333338333338FFF00FFFF83
        333338F3333338F3333330FFF0FFF0333333383FF333F8333333330FFFFF0333
        333333883FF88333333333380008333333333333888333333333}
      NumGlyphs = 2
      OnClick = SpeedButton2Click
    end
    object SpeedButton6: TSpeedButton
      Left = 75
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Graphic subset'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        70777777777777070F07777777777700FFF077777777770FFF0777777777770F
        F077000700070000000707777777770777770777777777077777777777777777
        7777077777777707777707777777770777770777777777077777777777777777
        7777077777777707777707777777770777770007000700077777}
      OnClick = SpeedButton6Click
    end
    object SpeedButton7: TSpeedButton
      Left = 100
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Entire record'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00700000777000
        0007099997777779999009777777777779900997777777779790097977777779
        7790097700070007779007770777770777707777077777077777777777777777
        7777077707777707777709770777770777700977000700977790097977777779
        7790099777777777979009999977779999907000007770000007}
      OnClick = SpeedButton7Click
    end
    object MeasureDistanceSpeedButton14: TSpeedButton
      Left = 125
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Calculate distance'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        707777777777777770777777777777700000777777777777707777777777777C
        70777777777777C77777777777777C77777777777777C7777777777777777777
        7777777777C77777777777777C77777777777707C77777777777770777777777
        7777000007777777777777077777777777777707777777777777}
      OnClick = MeasureDistanceSpeedButton14Click
    end
    object SpeedButton8: TSpeedButton
      Left = 150
      Top = 0
      Width = 25
      Height = 25
      Hint = 'High frequency'
      Caption = 'Hi'
      OnClick = SpeedButton8Click
    end
    object SpeedButton9: TSpeedButton
      Left = 175
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Low frequency'
      Caption = 'Lo'
      OnClick = SpeedButton9Click
    end
    object BitBtn1: TBitBtn
      Left = 200
      Top = 0
      Width = 40
      Height = 25
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333333333333FF3FF3333333333CC30003333333333773777333333333C33
        3000333FF33337F33777339933333C3333333377F33337F3333F339933333C33
        33003377333337F33377333333333C333300333F333337F33377339333333C33
        3333337FF3333733333F33993333C33333003377FF33733333773339933C3333
        330033377FF73F33337733339933C33333333FF377F373F3333F993399333C33
        330077F377F337F33377993399333C33330077FF773337F33377399993333C33
        33333777733337F333FF333333333C33300033333333373FF7773333333333CC
        3000333333333377377733333333333333333333333333333333}
      NumGlyphs = 2
      TabOrder = 0
      OnClick = BitBtn1Click
    end
    object RedrawSpeedButton12: TSpeedButton
      Left = 240
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
    object ComboBox1: TComboBox
      Left = 265
      Top = 0
      Width = 104
      Height = 24
      TabOrder = 1
      OnChange = ComboBox1Change
      Items.Strings = (
        '100%'
        '50%'
        '33%'
        '25%'
        '20%'
        '17%'
        '14%'
        '12.5%'
        '11%'
        '10%')
    end
    object SpeedButton10: TSpeedButton
      Left = 369
      Top = 0
      Width = 23
      Height = 25
      Caption = 'B-'
      OnClick = SpeedButton10Click
    end
    object SpeedButton11: TSpeedButton
      Left = 392
      Top = 0
      Width = 23
      Height = 25
      Caption = 'B+'
      OnClick = SpeedButton11Click
    end
    object SpeedButton12: TSpeedButton
      Left = 415
      Top = 0
      Width = 23
      Height = 25
      Caption = 'W-'
      OnClick = SpeedButton12Click
    end
    object SpeedButton13: TSpeedButton
      Left = 438
      Top = 0
      Width = 23
      Height = 25
      Caption = 'W+'
      OnClick = SpeedButton13Click
    end
  end
  object MainMenu1: TMainMenu
    Left = 41
    Top = 49
    object File1: TMenuItem
      Caption = '&File'
      object Subset1: TMenuItem
        Caption = '&Subset'
        OnClick = Subset1Click
      end
      object Merge1: TMenuItem
        Caption = 'Merge'
        OnClick = Merge1Click
      end
      object Saveimage1: TMenuItem
        Caption = '&Save image'
        OnClick = Saveimage1Click
      end
      object Close1: TMenuItem
        Caption = '&Close'
        OnClick = Close1Click
      end
    end
    object Calculate1: TMenuItem
      Caption = '&Calculate'
      Enabled = False
      object Distance1: TMenuItem
        Caption = '&Distance'
        OnClick = Distance1Click
      end
      object Twowaytraveltime1: TMenuItem
        Caption = '&Two way travel time'
        OnClick = Twowaytraveltime1Click
      end
    end
    object Sidescanoptions1: TMenuItem
      Caption = '&Sidescan options'
      object Displayoptions2: TMenuItem
        Caption = 'Display options'
        OnClick = Displayoptions2Click
      end
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 154
    Top = 98
  end
  object PopupMenu1: TPopupMenu
    Left = 80
    Top = 104
    object Displayoptions1: TMenuItem
      Caption = 'Display options'
      OnClick = Displayoptions1Click
    end
    object Histogram1: TMenuItem
      Caption = 'Histogram'
      OnClick = Histogram1Click
    end
    object OpenhistogramDB1: TMenuItem
      Caption = 'Open histogram DB'
      OnClick = OpenhistogramDB1Click
    end
    object Showpingtable1: TMenuItem
      Caption = 'Show ping table'
      OnClick = Showpingtable1Click
    end
    object Showcoverageonmap1: TMenuItem
      Caption = 'Show coverage on map'
      OnClick = Showcoverageonmap1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Makethisthelastrecorddisplayed1: TMenuItem
      Caption = 'Clip image above this point'
      OnClick = Makethisthelastrecorddisplayed1Click
    end
    object Makethisthefirstrecord1: TMenuItem
      Caption = 'Clip image below this point'
      OnClick = Makethisthefirstrecord1Click
    end
    object Inserttargetrecord1: TMenuItem
      Caption = 'Insert target record'
      OnClick = Inserttargetrecord1Click
    end
    object Entirerecord1: TMenuItem
      Caption = 'Display entire record'
      OnClick = Entirerecord1Click
    end
    object Fullresolutiondisplay1: TMenuItem
      Caption = 'Full resolution display'
      OnClick = Fullresolutiondisplay1Click
    end
    object Labelcorners1: TMenuItem
      Caption = 'Label corners'
      OnClick = Labelcorners1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Exportasregisteredimage1: TMenuItem
      Caption = 'Export as registered image'
    end
    object ExportKML1: TMenuItem
      Caption = 'Export KML'
      OnClick = ExportKML1Click
    end
    object Grap1: TMenuItem
      Caption = 'Graphical file export'
      OnClick = Grap1Click
    end
    object ExportcurrentsubsetXTF1: TMenuItem
      Caption = 'Export current subset (XTF)'
      OnClick = ExportcurrentsubsetXTF1Click
    end
    object Exportsanitizedcoordinates1: TMenuItem
      Caption = 'Export sanitized coordinates'
      OnClick = Exportsanitizedcoordinates1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object XTFfileheader1: TMenuItem
      Caption = 'XTF file header'
      OnClick = XTFfileheader1Click
    end
    object Mapinfo1: TMenuItem
      Caption = 'Map info'
      OnClick = Mapinfo1Click
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object Copytoclipboard1: TMenuItem
      Caption = 'Copy to clipboard'
      OnClick = Copytoclipboard1Click
    end
    object Saveimage2: TMenuItem
      Caption = 'Save image'
      OnClick = Saveimage2Click
    end
    object Savelabelledimage1: TMenuItem
      Caption = 'Save labelled image'
      OnClick = Savelabelledimage1Click
    end
  end
end
