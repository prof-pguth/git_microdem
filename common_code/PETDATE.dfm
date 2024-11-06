object GetDateForm: TGetDateForm
  Left = 574
  Top = 386
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Starting date and duration'
  ClientHeight = 228
  ClientWidth = 237
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 30
    Height = 13
    Caption = 'Month'
  end
  object Label2: TLabel
    Left = 24
    Top = 48
    Width = 19
    Height = 13
    Caption = 'Day'
  end
  object Label3: TLabel
    Left = 24
    Top = 88
    Width = 22
    Height = 13
    Caption = 'Year'
  end
  object Label4: TLabel
    Left = 24
    Top = 128
    Width = 71
    Height = 13
    Caption = 'Duration (days)'
  end
  object Edit1: TEdit
    Left = 112
    Top = 16
    Width = 81
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 112
    Top = 48
    Width = 81
    Height = 21
    TabOrder = 1
    Text = 'Edit2'
  end
  object Edit3: TEdit
    Left = 112
    Top = 88
    Width = 81
    Height = 21
    TabOrder = 2
    Text = 'Edit3'
  end
  object Edit4: TEdit
    Left = 112
    Top = 128
    Width = 81
    Height = 21
    TabOrder = 3
    Text = 'Edit4'
  end
  object BitBtn2: TBitBtn
    Left = 80
    Top = 183
    Width = 77
    Height = 27
    Caption = 'OK'
    Default = True
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    Margin = 2
    ModalResult = 1
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 4
    OnClick = BitBtn2Click
    IsControl = True
  end
  object CheckBox1: TCheckBox
    Left = 24
    Top = 160
    Width = 71
    Height = 17
    Caption = 'Rise/set'
    TabOrder = 5
  end
  object CheckBox2: TCheckBox
    Left = 101
    Top = 160
    Width = 97
    Height = 17
    Caption = 'Moon phases'
    TabOrder = 6
  end
end
