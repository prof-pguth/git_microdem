object PetmarAboutBox: TPetmarAboutBox
  Left = 451
  Top = 314
  ActiveControl = OKButton
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 219
  ClientWidth = 448
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 16
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 450
    Height = 188
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object image1: TImage
      Left = 8
      Top = 8
      Width = 110
      Height = 160
      Cursor = crNoDrop
      AutoSize = True
      Stretch = True
      IsControl = True
    end
    object ProductName: TLabel
      Left = 130
      Top = 48
      Width = 171
      Height = 32
      Caption = 'Product Name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -28
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      IsControl = True
    end
    object Version: TLabel
      Left = 138
      Top = 114
      Width = 56
      Height = 19
      Caption = 'Version'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -17
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      IsControl = True
    end
    object Copyright: TLabel
      Left = 134
      Top = 88
      Width = 236
      Height = 17
      Caption = 'PETMAR Trilobite Breeding Ranch'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -15
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      IsControl = True
    end
    object Label1: TLabel
      Left = 138
      Top = 163
      Width = 169
      Height = 16
      Caption = 'Code copyright 1984-2023'
    end
    object ProgramIcon: TImage
      Left = 336
      Top = 8
      Width = 32
      Height = 32
    end
    object Label2: TLabel
      Left = 168
      Top = 16
      Width = 4
      Height = 19
      Font.Charset = ANSI_CHARSET
      Font.Color = clRed
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold, fsItalic]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 136
      Top = 136
      Width = 44
      Height = 16
      Caption = 'Label3'
    end
  end
  object OKButton: TBitBtn
    Left = 176
    Top = 202
    Width = 64
    Height = 27
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    ParentFont = False
    Spacing = -1
    TabOrder = 1
    IsControl = True
  end
end
