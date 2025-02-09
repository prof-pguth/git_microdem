object DEMeditForm: TDEMeditForm
  Left = 748
  Top = 211
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'DEM Edit Window'
  ClientHeight = 284
  ClientWidth = 549
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  FormStyle = fsMDIChild
  Menu = MainMenu1
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object StringGrid1: TStringGrid
    Left = 0
    Top = 0
    Width = 549
    Height = 243
    Align = alClient
    ColCount = 26
    RowCount = 26
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
    TabOrder = 0
    OnClick = StringGrid1Click
    OnDblClick = StringGrid1DblClick
    OnGetEditText = StringGrid1GetEditText
    OnMouseMove = StringGrid1MouseMove
    ExplicitWidth = 594
    ExplicitHeight = 250
    ColWidths = (
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64)
    RowHeights = (
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24)
  end
  object Panel1: TPanel
    Left = 0
    Top = 243
    Width = 549
    Height = 41
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 250
    ExplicitWidth = 594
    object Label1: TLabel
      Left = 33
      Top = 16
      Width = 5
      Height = 13
    end
    object HelpBtn: TBitBtn
      Left = 304
      Top = 6
      Width = 57
      Height = 27
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 0
      OnClick = HelpBtnClick
      IsControl = True
    end
    object BitBtn1: TBitBtn
      Left = 248
      Top = 6
      Width = 50
      Height = 25
      Hint = 'Decimal places to show'
      Caption = '0.0001'
      TabOrder = 1
      OnClick = BitBtn1Click
    end
    object CheckBox1: TCheckBox
      Left = 367
      Top = 6
      Width = 97
      Height = 17
      Caption = 'Hide 0 z value'
      TabOrder = 2
      OnClick = CheckBox1Click
    end
  end
  object MainMenu1: TMainMenu
    Left = 400
    Top = 400
    object Editmode1: TMenuItem
      Caption = '&Edit '
      object Delete1: TMenuItem
        Caption = '&Delete Mode'
        OnClick = Delete1Click
      end
      object Replace1: TMenuItem
        Caption = '&Replace Mode'
        Checked = True
        OnClick = Replace1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Deletepoints1: TMenuItem
        Caption = 'Delete points'
        object Entirewindow1: TMenuItem
          Caption = '&Entire window'
          OnClick = Entirewindow1Click
        end
        object Column1: TMenuItem
          Caption = '&Column'
          OnClick = Column1Click
        end
        object Row1: TMenuItem
          Caption = '&Row'
          OnClick = Row1Click
        end
      end
    end
    object Map1: TMenuItem
      Caption = '&Map'
      object Showwindow1: TMenuItem
        Caption = '&Show window'
        OnClick = Showwindow1Click
      end
      object Update1: TMenuItem
        Caption = '&Update'
        OnClick = Update1Click
      end
    end
    object Clickoptions1: TMenuItem
      Caption = 'Click options'
      object Slopealgorithms1: TMenuItem
        Caption = 'Slope algorithms'
        OnClick = Slopealgorithms1Click
      end
    end
  end
end
