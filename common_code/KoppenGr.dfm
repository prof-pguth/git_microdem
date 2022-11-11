object KoppenGraph: TKoppenGraph
  Left = 407
  Top = 333
  Caption = 'KoppenGraph'
  ClientHeight = 329
  ClientWidth = 585
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'System'
  Font.Style = []
  FormStyle = fsMDIChild
  Menu = MainMenu1
  Position = poDefault
  PrintScale = poPrintToFit
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 16
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 585
    Height = 329
    Align = alClient
    OnMouseDown = Image1MouseDown
    ExplicitWidth = 591
    ExplicitHeight = 304
  end
  object MainMenu1: TMainMenu
    Left = 16
    Top = 16
    object File1: TMenuItem
      Caption = '&File'
      object Saveimage1: TMenuItem
        Caption = '&Save image'
        OnClick = Saveimage1Click
      end
      object Copytoclipboard2: TMenuItem
        Caption = 'Copy to clipboard'
        OnClick = Copytoclipboard2Click
      end
      object Close1: TMenuItem
        Caption = '&Close'
        OnClick = Close1Click
      end
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'BMP'
    Left = 104
    Top = 48
  end
  object PopupMenu1: TPopupMenu
    Left = 112
    Top = 120
    object Modifygraphoptions1: TMenuItem
      Caption = 'Modify graph options'
      OnClick = Modifygraphoptions1Click
    end
    object Saveimage2: TMenuItem
      Caption = 'Save image'
      OnClick = Saveimage2Click
    end
    object Copytoclipboard1: TMenuItem
      Caption = 'Copy to clipboard'
      OnClick = Copytoclipboard1Click
    end
  end
end
