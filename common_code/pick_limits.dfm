object picklimitsForm: TpicklimitsForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Pick limits'
  ClientHeight = 266
  ClientWidth = 362
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  OnClose = FormClose
  TextHeight = 13
  object Label1: TLabel
    Left = 144
    Top = 34
    Width = 18
    Height = 13
    Caption = 'Top'
  end
  object Label2: TLabel
    Left = 144
    Top = 91
    Width = 34
    Height = 13
    Caption = 'Bottom'
  end
  object Label3: TLabel
    Left = 8
    Top = 64
    Width = 19
    Height = 13
    Caption = 'Left'
  end
  object Label4: TLabel
    Left = 306
    Top = 61
    Width = 25
    Height = 13
    Caption = 'Right'
  end
  object Label5: TLabel
    Left = 72
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Label5'
  end
  object Label6: TLabel
    Left = 226
    Top = 141
    Width = 31
    Height = 13
    Caption = 'Label6'
  end
  object Label7: TLabel
    Left = 16
    Top = 176
    Width = 31
    Height = 13
    Caption = 'Label7'
  end
  object Edit1: TEdit
    Left = 104
    Top = 53
    Width = 121
    Height = 21
    TabOrder = 0
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 104
    Top = 110
    Width = 121
    Height = 21
    TabOrder = 1
    OnChange = Edit2Change
  end
  object Edit3: TEdit
    Left = 0
    Top = 83
    Width = 121
    Height = 21
    TabOrder = 2
    OnChange = Edit3Change
  end
  object Edit4: TEdit
    Left = 226
    Top = 80
    Width = 121
    Height = 21
    TabOrder = 3
    OnChange = Edit4Change
  end
  object HelpBtn: TBitBtn
    Left = 117
    Top = 230
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 4
    OnClick = HelpBtnClick
    IsControl = True
  end
  object BitBtn1: TBitBtn
    Left = 16
    Top = 231
    Width = 75
    Height = 25
    Caption = 'OK'
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
    NumGlyphs = 2
    TabOrder = 5
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 264
    Top = 173
    Width = 75
    Height = 25
    Caption = 'Entire DEM'
    TabOrder = 6
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 264
    Top = 235
    Width = 75
    Height = 25
    Caption = 'Interior DEM'
    TabOrder = 7
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 264
    Top = 204
    Width = 75
    Height = 25
    Caption = 'Current map'
    TabOrder = 8
    OnClick = BitBtn4Click
  end
  object BitBtn5: TBitBtn
    Left = 8
    Top = 8
    Width = 49
    Height = 25
    Caption = 'NW'
    TabOrder = 9
    OnClick = BitBtn5Click
  end
  object BitBtn6: TBitBtn
    Left = 282
    Top = 136
    Width = 49
    Height = 25
    Caption = 'SE'
    TabOrder = 10
    OnClick = BitBtn6Click
  end
end
