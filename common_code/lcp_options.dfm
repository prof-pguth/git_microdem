object LCP_form: TLCP_form
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Least cost path options'
  ClientHeight = 493
  ClientWidth = 556
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 84
    Top = 24
    Width = 73
    Height = 13
    Caption = 'Impossible cost'
  end
  object Label2: TLabel
    Left = 50
    Top = 55
    Width = 107
    Height = 13
    Caption = 'Max impossible in path'
  end
  object Label3: TLabel
    Left = 104
    Top = 78
    Width = 53
    Height = 13
    Caption = 'Buffer cost'
  end
  object Label4: TLabel
    Left = 91
    Top = 109
    Width = 66
    Height = 13
    Caption = 'Buffer rounds'
  end
  object Label5: TLabel
    Left = 40
    Top = 128
    Width = 130
    Height = 13
    Caption = 'Cost surface resolution (m)'
  end
  object Label6: TLabel
    Left = 77
    Top = 159
    Width = 80
    Height = 13
    Caption = 'Free start points'
  end
  object Label7: TLabel
    Left = 91
    Top = 186
    Width = 61
    Height = 13
    Caption = 'Precinct field'
  end
  object Label9: TLabel
    Left = 8
    Top = 304
    Width = 101
    Height = 13
    Caption = 'Route starting points'
  end
  object Label10: TLabel
    Left = 8
    Top = 352
    Width = 96
    Height = 13
    Caption = 'Route ending points'
  end
  object Label11: TLabel
    Left = 8
    Top = 255
    Width = 67
    Height = 13
    Caption = 'Road network'
  end
  object Edit1: TEdit
    Left = 176
    Top = 21
    Width = 73
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 176
    Top = 48
    Width = 73
    Height = 21
    TabOrder = 1
    Text = 'Edit1'
  end
  object Edit3: TEdit
    Left = 176
    Top = 75
    Width = 73
    Height = 21
    TabOrder = 2
    Text = 'Edit1'
  end
  object Edit4: TEdit
    Left = 176
    Top = 102
    Width = 73
    Height = 21
    TabOrder = 3
    Text = 'Edit1'
  end
  object Edit5: TEdit
    Left = 176
    Top = 129
    Width = 73
    Height = 21
    TabOrder = 4
    Text = 'Edit1'
  end
  object Edit6: TEdit
    Left = 176
    Top = 156
    Width = 73
    Height = 21
    TabOrder = 5
    Text = 'Edit1'
  end
  object Edit7: TEdit
    Left = 176
    Top = 183
    Width = 73
    Height = 21
    TabOrder = 6
    Text = 'Edit1'
  end
  object BitBtn10: TBitBtn
    Left = 21
    Top = 438
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
    TabOrder = 7
    OnClick = BitBtn10Click
    IsControl = True
  end
  object BitBtn3: TBitBtn
    Left = 104
    Top = 439
    Width = 77
    Height = 25
    Caption = 'Help'
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 8
    OnClick = BitBtn3Click
    IsControl = True
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 209
    Width = 129
    Height = 40
    Caption = 'Distance'
    Columns = 2
    Items.Strings = (
      'Meters'
      'Pixels')
    TabOrder = 9
  end
  object CheckBox1: TCheckBox
    Left = 306
    Top = 178
    Width = 97
    Height = 17
    Caption = 'Weight diagonals'
    TabOrder = 10
  end
  object CreateCostSurfaceBitBtn1: TBitBtn
    Left = 21
    Top = 398
    Width = 129
    Height = 25
    Caption = 'Create cost surface'
    TabOrder = 11
    OnClick = CreateCostSurfaceBitBtn1Click
  end
  object Edit9: TEdit
    Left = 8
    Top = 323
    Width = 433
    Height = 21
    TabOrder = 12
    Text = 'Edit8'
  end
  object RouteStartBitBtn4: TBitBtn
    Left = 457
    Top = 321
    Width = 34
    Height = 25
    Caption = '...'
    TabOrder = 13
    OnClick = RouteStartBitBtn4Click
  end
  object CreatePathSurfaceBitBtn5: TBitBtn
    Left = 156
    Top = 398
    Width = 129
    Height = 25
    Caption = 'Create path surfaces'
    TabOrder = 14
    OnClick = CreatePathSurfaceBitBtn5Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 474
    Width = 556
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object GroupBox1: TGroupBox
    Left = 306
    Top = 8
    Width = 111
    Height = 105
    Caption = 'Save path surface'
    TabOrder = 16
    object CheckBox2: TCheckBox
      Left = 8
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Cost'
      TabOrder = 0
    end
    object CheckBox3: TCheckBox
      Left = 8
      Top = 42
      Width = 97
      Height = 29
      Caption = 'Direction'
      TabOrder = 1
    end
    object CheckBox4: TCheckBox
      Left = 8
      Top = 74
      Width = 97
      Height = 17
      Caption = 'Distance'
      TabOrder = 2
    end
  end
  object RouteEndBitBtn6: TBitBtn
    Left = 457
    Top = 369
    Width = 34
    Height = 25
    Caption = '...'
    TabOrder = 17
    OnClick = RouteEndBitBtn6Click
  end
  object Edit10: TEdit
    Left = 8
    Top = 371
    Width = 433
    Height = 21
    TabOrder = 18
    Text = 'Edit8'
  end
  object CreateRouteCostBitBtn7: TBitBtn
    Left = 291
    Top = 398
    Width = 129
    Height = 25
    Caption = 'Create route costs'
    TabOrder = 19
    OnClick = CreateRouteCostBitBtn7Click
  end
  object BitBtn8: TBitBtn
    Left = 457
    Top = 400
    Width = 75
    Height = 25
    Caption = 'One step'
    TabOrder = 20
    OnClick = BitBtn8Click
  end
  object RoadNetworkBitBtn9: TBitBtn
    Left = 457
    Top = 272
    Width = 34
    Height = 25
    Caption = '...'
    TabOrder = 21
    OnClick = RoadNetworkBitBtn9Click
  end
  object Edit11: TEdit
    Left = 8
    Top = 274
    Width = 433
    Height = 21
    TabOrder = 22
    Text = 'Edit8'
  end
  object CheckBox5: TCheckBox
    Left = 312
    Top = 136
    Width = 97
    Height = 17
    Caption = 'Overwrite'
    TabOrder = 23
  end
  object CheckBox6: TCheckBox
    Left = 306
    Top = 224
    Width = 226
    Height = 17
    Caption = 'Shortest distance  (all roads same cost)'
    TabOrder = 24
    OnClick = CheckBox6Click
  end
  object CheckBox7: TCheckBox
    Left = 306
    Top = 201
    Width = 97
    Height = 17
    Caption = 'Least cost path'
    TabOrder = 25
    OnClick = CheckBox7Click
  end
end
