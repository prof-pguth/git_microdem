object OKRightDlg: TOKRightDlg
  Left = 1590
  Top = 229
  BorderStyle = bsDialog
  Caption = 'Dialog'
  ClientHeight = 150
  ClientWidth = 358
  Color = clBtnFace
  ParentFont = True
  Position = poScreenCenter
  TextHeight = 20
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 281
    Height = 161
    Shape = bsFrame
  end
  object OKBtn: TButton
    Left = 300
    Top = 8
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 300
    Top = 38
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
end
