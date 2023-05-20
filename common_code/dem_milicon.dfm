object MilIconsForm: TMilIconsForm
  Left = 292
  Top = 281
  BorderIcons = [biSystemMenu]
  Caption = 'MICRODEM Icon Composer'
  ClientHeight = 684
  ClientWidth = 968
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Menu = IconMenu1
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 0
    Top = 0
    Width = 748
    Height = 684
    Align = alClient
    TabOrder = 0
    OnChange = TabControl1Change
    object ScrollBox1: TScrollBox
      Left = 4
      Top = 6
      Width = 740
      Height = 674
      Align = alClient
      TabOrder = 0
      ExplicitWidth = 744
      ExplicitHeight = 675
      object Image1: TImage
        Left = 0
        Top = 0
        Width = 746
        Height = 680
        Align = alClient
        AutoSize = True
        OnDblClick = Image1DblClick
        OnMouseMove = Image1MouseMove
        ExplicitWidth = 625
        ExplicitHeight = 625
      end
    end
  end
  object Panel1: TPanel
    Left = 748
    Top = 0
    Width = 220
    Height = 684
    Align = alRight
    TabOrder = 1
    ExplicitLeft = 752
    ExplicitHeight = 685
    object Image2: TImage
      Left = 2
      Top = 0
      Width = 207
      Height = 150
    end
    object Label4: TLabel
      Left = 28
      Top = 264
      Width = 45
      Height = 13
      Caption = 'Text size '
    end
    object Label3: TLabel
      Left = 28
      Top = 304
      Width = 57
      Height = 13
      Caption = 'Symbol Size'
    end
    object Label2: TLabel
      Left = 61
      Top = 191
      Width = 31
      Height = 13
      Caption = 'Parent'
    end
    object Label1: TLabel
      Left = 61
      Top = 159
      Width = 19
      Height = 13
      Caption = 'Unit'
    end
    object Label6: TLabel
      Left = 7
      Top = 240
      Width = 33
      Height = 13
      Caption = 'Bottom'
    end
    object Label5: TLabel
      Left = 7
      Top = 210
      Width = 19
      Height = 13
      Caption = 'Top'
    end
    object ClipboardSpeedButton: TSpeedButton
      Left = 6
      Top = 519
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
    object UpDown2: TUpDown
      Left = 6
      Top = 259
      Width = 16
      Height = 24
      TabOrder = 0
      OnClick = UpDown2Click
    end
    object UpDown1: TUpDown
      Left = 6
      Top = 304
      Width = 16
      Height = 24
      TabOrder = 1
      OnClick = UpDown1Click
    end
    object RadioGroup1: TRadioGroup
      Left = 114
      Top = 156
      Width = 87
      Height = 172
      Caption = 'Unit Size'
      Items.Strings = (
        'None'
        'Squad'
        'Section'
        'Platoon'
        'Company'
        'Battalion'
        'Regiment'
        'Brigade'
        'Division'
        'Corps')
      TabOrder = 2
      OnClick = RadioGroup1Click
    end
    object RadioGroup5: TRadioGroup
      Left = 114
      Top = 156
      Width = 88
      Height = 89
      Caption = 'Infrastructure'
      Items.Strings = (
        'Unknown'
        'Operational'
        'Damaged'
        'Destroyed')
      TabOrder = 3
      OnClick = RadioGroup5Click
    end
    object BitBtn3: TBitBtn
      Left = 2
      Top = 334
      Width = 71
      Height = 25
      Caption = 'Symbol'
      TabOrder = 4
      OnClick = BitBtn3Click
    end
    object BitBtn1: TBitBtn
      Left = 2
      Top = 396
      Width = 71
      Height = 25
      Caption = 'Fill'
      TabOrder = 5
      OnClick = BitBtn1Click
    end
    object BitBtn2: TBitBtn
      Left = 115
      Top = 519
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 6
      OnClick = BitBtn2Click
    end
    object RadioGroup2: TRadioGroup
      Left = 16
      Top = 432
      Width = 153
      Height = 81
      Caption = 'Symbol Color Fill'
      Columns = 2
      ItemIndex = 4
      Items.Strings = (
        'None '
        '1/4'
        '1/2'
        '3/4'
        'Full')
      TabOrder = 7
      OnClick = RadioGroup2Click
    end
    object BitBtn4: TBitBtn
      Left = 90
      Top = 334
      Width = 75
      Height = 25
      Caption = '+ Map'
      TabOrder = 8
      OnClick = BitBtn4Click
    end
    object Edit1: TEdit
      Left = 6
      Top = 156
      Width = 49
      Height = 21
      TabOrder = 9
      Text = ' '
      OnChange = Edit1Change
    end
    object Edit2: TEdit
      Left = 6
      Top = 183
      Width = 49
      Height = 21
      TabOrder = 10
      OnChange = Edit2Change
    end
    object Edit4: TEdit
      Left = 46
      Top = 237
      Width = 45
      Height = 21
      TabOrder = 11
      OnChange = Edit4Change
    end
    object Edit3: TEdit
      Left = 46
      Top = 213
      Width = 45
      Height = 21
      TabOrder = 12
      OnChange = Edit3Change
    end
    object BitBtn5: TBitBtn
      Left = 2
      Top = 365
      Width = 71
      Height = 25
      Caption = 'Text'
      TabOrder = 13
      OnClick = BitBtn5Click
    end
  end
  object IconMenu1: TMainMenu
    Left = 112
    Top = 368
    object Close1: TMenuItem
      Caption = '&Close'
      OnClick = Close1Click
    end
    object Symboltoclipboard1: TMenuItem
      Caption = '&Symbol to clipboard'
      OnClick = Symboltoclipboard1Click
    end
    object Savetofile1: TMenuItem
      Caption = 'Save to file'
      OnClick = Savetofile1Click
    end
    object Font1: TMenuItem
      Caption = '&Font'
      object Newsymbolfont1: TMenuItem
        Caption = 'New symbol font'
        OnClick = Newsymbolfont1Click
      end
      object extfont1: TMenuItem
        Caption = 'Text font'
        OnClick = extfont1Click
      end
    end
    object Options1: TMenuItem
      Caption = 'Options'
      object Military0widthfonts1: TMenuItem
        Caption = 'Military 0-width fonts'
        Checked = True
        OnClick = Military0widthfonts1Click
      end
      object Largefontsamples1: TMenuItem
        Caption = 'Large font samples'
        Checked = True
        OnClick = Largefontsamples1Click
      end
      object Showmilitarygroundsymbols1: TMenuItem
        Caption = 'Show military ground symbols'
        OnClick = Showmilitarygroundsymbols1Click
      end
      object Shownavalsymbols1: TMenuItem
        Caption = 'Show naval symbols'
        OnClick = Shownavalsymbols1Click
      end
      object Showairsymbols1: TMenuItem
        Caption = 'Show military  air symbols'
        OnClick = Showairsymbols1Click
      end
    end
    object Mulitipleicons1: TMenuItem
      Caption = 'Mulitiple icons'
      OnClick = Mulitipleicons1Click
    end
    object Help1: TMenuItem
      Caption = '&Help'
      OnClick = Help1Click
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [fdTrueTypeOnly, fdEffects]
    Left = 152
    Top = 368
  end
end
