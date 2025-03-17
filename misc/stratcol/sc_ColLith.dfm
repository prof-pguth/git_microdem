object patternf: Tpatternf
  Left = 450
  Top = 359
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Lithology'
  ClientHeight = 408
  ClientWidth = 531
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Menu = MainMenu1
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object LithImage1: TImage
    Left = 0
    Top = 0
    Width = 531
    Height = 389
    Align = alClient
    AutoSize = True
    OnDblClick = LithImage1DblClick
    OnMouseDown = LithImage1MouseDown
    OnMouseMove = LithImage1MouseMove
    ExplicitWidth = 535
    ExplicitHeight = 363
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 389
    Width = 531
    Height = 19
    Panels = <
      item
        Width = 200
      end>
    ExplicitTop = 390
    ExplicitWidth = 535
  end
  object MainMenu1: TMainMenu
    Left = 128
    Top = 48
    object File1: TMenuItem
      Caption = '&File'
      object SaveallpatternsasBMPzipatone1: TMenuItem
        Caption = 'Save all patterns as BMP zipatone'
        OnClick = SaveallpatternsasBMPzipatone1Click
      end
      object Exit1: TMenuItem
        Caption = '&Close'
        OnClick = Exit1Click
      end
    end
    object EditPattern1: TMenuItem
      Caption = '&Edit Patterns'
      object Recolor1: TMenuItem
        Caption = '&Recolor'
        object Allpatterns1: TMenuItem
          Caption = '&All patterns'
          OnClick = Allpatterns1Click
        end
        object Recolor2: TMenuItem
          Caption = '&Individual patterns'
          OnClick = Recolor2Click
        end
      end
      object Rename1: TMenuItem
        Caption = 'Rename'
        OnClick = Rename1Click
      end
      object Pattern1: TMenuItem
        Caption = 'Pattern'
        OnClick = Pattern1Click
      end
      object Delete1: TMenuItem
        Caption = '&Delete'
        OnClick = Delete1Click
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      OnClick = Help1Click
    end
  end
  object ColorDialog1: TColorDialog
    Left = 208
    Top = 48
  end
  object PopupMenu1: TPopupMenu
    Left = 136
    Top = 112
    object Recolor3: TMenuItem
      Caption = 'Recolor'
      OnClick = Recolor3Click
    end
    object Eidt1: TMenuItem
      Caption = 'Edit pattern'
      OnClick = Eidt1Click
    end
    object SaveasZipatoneBMP1: TMenuItem
      Caption = 'Save as Zipatone BMP'
      OnClick = SaveasZipatoneBMP1Click
    end
  end
end
