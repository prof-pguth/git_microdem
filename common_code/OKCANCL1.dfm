object OKBottomDlg: TOKBottomDlg
  Left = 198
  Top = 83
  BorderStyle = bsDialog
  Caption = 'Dialog'
  ClientHeight = 142
  ClientWidth = 265
  Color = clBtnFace
  ParentFont = True
  Position = poScreenCenter
  TextHeight = 20
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 297
    Height = 161
    Shape = bsFrame
  end
  object OKBtn: TButton
    Left = 79
    Top = 180
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 159
    Top = 180
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
end
