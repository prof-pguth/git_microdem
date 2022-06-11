object ToggleDBfieldsForm: TToggleDBfieldsForm
  Left = 262
  Top = 116
  Caption = 'Select '
  ClientHeight = 393
  ClientWidth = 404
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poScreenCenter
  OnClose = FormClose
  OnResize = FormResize
  TextHeight = 13
  object ListBox1: TListBox
    Left = 0
    Top = 35
    Width = 202
    Height = 309
    Align = alLeft
    Color = clGray
    ItemHeight = 13
    Sorted = True
    TabOrder = 0
    OnClick = ListBox1Click
  end
  object ListBox2: TListBox
    Left = 203
    Top = 35
    Width = 201
    Height = 309
    Align = alRight
    ItemHeight = 13
    Sorted = True
    TabOrder = 1
    OnClick = ListBox2Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 344
    Width = 404
    Height = 49
    Align = alBottom
    TabOrder = 2
    object OKBtn: TBitBtn
      Left = 32
      Top = 14
      Width = 77
      Height = 27
      Kind = bkOK
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 0
      OnClick = OKBtnClick
      IsControl = True
    end
    object CancelBtn: TBitBtn
      Left = 115
      Top = 14
      Width = 77
      Height = 27
      Kind = bkCancel
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 1
      OnClick = CancelBtnClick
      IsControl = True
    end
    object HelpBtn: TBitBtn
      Left = 198
      Top = 14
      Width = 76
      Height = 27
      Enabled = False
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 2
      IsControl = True
    end
    object Button1: TButton
      Left = 289
      Top = 15
      Width = 53
      Height = 26
      Caption = 'All'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -19
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 348
      Top = 15
      Width = 53
      Height = 26
      Caption = 'None'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -19
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 4
      OnClick = Button1Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 404
    Height = 35
    Align = alTop
    TabOrder = 3
    object Label2: TLabel
      Left = 32
      Top = 14
      Width = 43
      Height = 13
      Caption = 'Disable'
    end
    object Label1: TLabel
      Left = 262
      Top = 14
      Width = 40
      Height = 13
      Caption = 'Enable'
    end
    object ComboBox1: TComboBox
      Left = 111
      Top = 8
      Width = 130
      Height = 21
      TabOrder = 0
      OnChange = ComboBox1Change
    end
  end
end
