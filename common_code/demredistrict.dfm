object RedistrictForm: TRedistrictForm
  Left = 1897
  Top = 229
  BorderIcons = [biSystemMenu]
  Caption = 'Redistricting'
  ClientHeight = 321
  ClientWidth = 753
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  Visible = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 753
    Height = 81
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 749
    object Label1: TLabel
      Left = 8
      Top = 35
      Width = 64
      Height = 13
      Caption = 'Evenness (%)'
    end
    object Label2: TLabel
      Left = 575
      Top = 6
      Width = 43
      Height = 13
      Caption = 'Race (%)'
    end
    object Label3: TLabel
      Left = 589
      Top = 38
      Width = 19
      Height = 13
      Caption = 'Pop'
    end
    object RedrawSpeedButton12: TSpeedButton
      Left = 95
      Top = 3
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
      Left = 8
      Top = 8
      Width = 81
      Height = 21
      TabOrder = 0
      OnChange = ComboBox1Change
    end
    object BitBtn2: TBitBtn
      Left = 126
      Top = 3
      Width = 58
      Height = 25
      Caption = 'Pick one'
      Enabled = False
      TabOrder = 1
      OnClick = BitBtn2Click
    end
    object BitBtn3: TBitBtn
      Left = 126
      Top = 34
      Width = 58
      Height = 25
      Caption = 'Pick box'
      Enabled = False
      TabOrder = 2
      OnClick = BitBtn3Click
    end
    object BitBtn4: TBitBtn
      Left = 268
      Top = 1
      Width = 48
      Height = 25
      Caption = 'Redraw'
      TabOrder = 3
      OnClick = BitBtn4Click
    end
    object HelpBtn: TBitBtn
      Left = 322
      Top = 32
      Width = 50
      Height = 25
      Caption = 'Help'
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 4
      OnClick = HelpBtnClick
      IsControl = True
    end
    object BitBtn5: TBitBtn
      Left = 268
      Top = 32
      Width = 48
      Height = 25
      Caption = 'Report'
      TabOrder = 5
      OnClick = BitBtn5Click
    end
    object BitBtn6: TBitBtn
      Left = 322
      Top = 1
      Width = 49
      Height = 25
      Caption = 'Districts'
      TabOrder = 6
      OnClick = BitBtn6Click
    end
    object Edit1: TEdit
      Left = 624
      Top = 10
      Width = 35
      Height = 21
      Enabled = False
      TabOrder = 7
      Text = '50'
      OnChange = Edit1Change
    end
    object Edit2: TEdit
      Left = 78
      Top = 32
      Width = 23
      Height = 21
      TabOrder = 8
      OnChange = Edit2Change
    end
    object CheckBox2: TCheckBox
      Left = 589
      Top = 57
      Width = 97
      Height = 17
      Caption = '1 race'
      TabOrder = 9
      OnClick = CheckBox2Click
    end
    object RadioGroup1: TRadioGroup
      Left = 378
      Top = 2
      Width = 191
      Height = 57
      Caption = 'Highlight'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'None'
        'Black'
        'Hispanic'
        'Population'
        'Pop && black'
        'Pop && Hispanic')
      TabOrder = 10
      OnClick = RadioGroup1Click
    end
    object BitBtn1: TBitBtn
      Left = 665
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Pick by filter'
      Enabled = False
      TabOrder = 11
      OnClick = BitBtn1Click
    end
    object Edit3: TEdit
      Left = 614
      Top = 36
      Width = 45
      Height = 21
      Enabled = False
      TabOrder = 12
      Text = '250'
      OnChange = Edit3Change
    end
    object CheckBox1: TCheckBox
      Left = 191
      Top = 9
      Width = 77
      Height = 17
      Caption = 'Quick pick'
      TabOrder = 13
    end
  end
  object DBGrid1: TDBGrid
    Left = 57
    Top = 81
    Width = 696
    Height = 221
    Align = alClient
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnDrawColumnCell = DBGrid1DrawColumnCell
  end
  object Panel2: TPanel
    Left = 0
    Top = 81
    Width = 57
    Height = 221
    Align = alLeft
    TabOrder = 2
    ExplicitHeight = 220
    object Image1: TImage
      Left = 1
      Top = 1
      Width = 55
      Height = 219
      Align = alClient
      OnDblClick = Image1DblClick
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      ExplicitLeft = 2
      ExplicitTop = 6
      ExplicitHeight = 166
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 302
    Width = 753
    Height = 19
    Panels = <
      item
        Text = 'rtest'
        Width = 50
      end>
    ExplicitTop = 301
    ExplicitWidth = 749
  end
  object ColorDialog1: TColorDialog
    Left = 88
    Top = 120
  end
  object DataSource1: TDataSource
    Left = 80
    Top = 200
  end
  object PopupMenu1: TPopupMenu
    Left = 160
    Top = 120
    object Showdistricts1: TMenuItem
      Caption = 'Show districts'
      OnClick = Showdistricts1Click
    end
    object Showpercentageminoritypopulation1: TMenuItem
      Caption = 'Show percentage black population'
      OnClick = Showpercentageminoritypopulation1Click
    end
    object ShowpercentageHispanicpopulation1: TMenuItem
      Caption = 'Show percentage Hispanic population'
      OnClick = ShowpercentageHispanicpopulation1Click
    end
    object Showpopulation1: TMenuItem
      Caption = 'Show population'
      OnClick = Showpopulation1Click
    end
    object Showpopulationdensity1: TMenuItem
      Caption = 'Show population density'
      OnClick = Showpopulationdensity1Click
    end
  end
  object PopupMenu2: TPopupMenu
    Left = 232
    Top = 120
    object Copylegendtoclipboard1: TMenuItem
      Caption = 'Copy legend to clipboard'
      OnClick = Copylegendtoclipboard1Click
    end
  end
  object PopupMenu3: TPopupMenu
    Left = 296
    Top = 120
    object CreateNewDistricts1: TMenuItem
      Caption = 'Create new districts'
      OnClick = CreateNewDistricts1Click
    end
    object Clearalldistricts1: TMenuItem
      Caption = 'Clear all districts'
      OnClick = Clearalldistricts1Click
    end
    object Assigntodistricts1: TMenuItem
      Caption = 'Assign to districts'
      object Randomizedistricts1: TMenuItem
        Caption = 'Randomize districts'
        OnClick = Randomizedistricts1Click
      end
      object Verticalbands1: TMenuItem
        Caption = 'Vertical bands'
        OnClick = Verticalbands1Click
      end
      object Horizontalbands1: TMenuItem
        Caption = 'Horizontal bands'
        OnClick = Horizontalbands1Click
      end
    end
    object Createnewdistrict1: TMenuItem
      Caption = 'Add new district'
      OnClick = Createnewdistrict1Click
    end
    object Recolorcurrentdistrict1: TMenuItem
      Caption = 'Recolor current district'
      OnClick = Recolorcurrentdistrict1Click
    end
  end
end
