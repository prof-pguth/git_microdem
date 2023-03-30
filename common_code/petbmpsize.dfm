object NewBMPForm: TNewBMPForm
  Left = 200
  Top = 99
  ActiveControl = WidthEdit
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Bitmap Dimensions'
  ClientHeight = 120
  ClientWidth = 215
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'System'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 16
  object Label1: TLabel
    Left = 21
    Top = 13
    Width = 38
    Height = 16
    Caption = '&Width'
    FocusControl = WidthEdit
  end
  object Label2: TLabel
    Left = 21
    Top = 45
    Width = 42
    Height = 16
    Caption = '&Height'
    FocusControl = HeightEdit
  end
  object WidthEdit: TEdit
    Left = 79
    Top = 8
    Width = 97
    Height = 28
    TabOrder = 0
  end
  object HeightEdit: TEdit
    Left = 78
    Top = 42
    Width = 97
    Height = 28
    TabOrder = 1
  end
  object BitBtn1: TBitBtn
    Left = 29
    Top = 88
    Width = 71
    Height = 27
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
  end
  object BitBtn2: TBitBtn
    Left = 115
    Top = 88
    Width = 78
    Height = 27
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
  end
end
