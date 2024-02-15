object SupClassAuxGrids: TSupClassAuxGrids
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Multiple grid masking'
  ClientHeight = 642
  ClientWidth = 798
  Color = cl3DLight
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label7: TLabel
    Left = 8
    Top = 105
    Width = 3
    Height = 13
  end
  object Label8: TLabel
    Left = 113
    Top = 104
    Width = 3
    Height = 13
  end
  object Label9: TLabel
    Left = 204
    Top = 105
    Width = 3
    Height = 13
  end
  object Label19: TLabel
    Left = 303
    Top = 104
    Width = 3
    Height = 13
  end
  object Label25: TLabel
    Left = 400
    Top = 104
    Width = 3
    Height = 13
  end
  object Label3: TLabel
    Left = 567
    Top = 452
    Width = 25
    Height = 13
    Caption = 'Class'
  end
  object Label4: TLabel
    Left = 546
    Top = 493
    Width = 111
    Height = 13
    Caption = 'Neighbors to expand >'
  end
  object Label1: TLabel
    Left = 546
    Top = 512
    Width = 103
    Height = 13
    Caption = 'Neighbors to shrink <'
  end
  object CancelBtn: TBitBtn
    Left = 636
    Top = 415
    Width = 85
    Height = 25
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    OnClick = CancelBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 567
    Top = 415
    Width = 63
    Height = 25
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    OnClick = HelpBtnClick
    IsControl = True
  end
  object Edit1: TEdit
    Left = 329
    Top = 83
    Width = 59
    Height = 21
    TabOrder = 2
  end
  object Use1: TCheckBox
    Left = 8
    Top = 40
    Width = 53
    Height = 17
    Caption = 'Use 1'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = Use1Click
  end
  object Use2: TCheckBox
    Left = 8
    Top = 63
    Width = 53
    Height = 17
    Caption = 'Use 2'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = Use2Click
  end
  object Use3: TCheckBox
    Left = 8
    Top = 86
    Width = 53
    Height = 17
    Caption = 'Use 3'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = Use3Click
  end
  object Use4: TCheckBox
    Left = 8
    Top = 109
    Width = 53
    Height = 17
    Caption = 'Use 4'
    Checked = True
    State = cbChecked
    TabOrder = 6
    OnClick = Use4Click
  end
  object Use5: TCheckBox
    Left = 8
    Top = 132
    Width = 53
    Height = 17
    Caption = 'Use 5'
    Checked = True
    State = cbChecked
    TabOrder = 7
    OnClick = Use5Click
  end
  object RadioGroup2: TRadioGroup
    Left = 688
    Top = 8
    Width = 89
    Height = 133
    Caption = 'Limits'
    ItemIndex = 0
    Items.Strings = (
      'Min/Max'
      '5/95'
      '10/90'
      '25/75'
      'Mean/Std'
      'User')
    TabOrder = 8
    OnClick = RadioGroup2Click
  end
  object BitBtn10: TBitBtn
    Left = 543
    Top = 285
    Width = 113
    Height = 47
    Caption = 'Pick open grids'
    Glyph.Data = {
      36080000424D3608000000000000360400002800000020000000200000000100
      0800000000000004000000000000000000000001000000010000000000008800
      00008A0000008C0000008E000000900000009200000094000000960000009800
      00009A0000009C0000009E000000A0000000A2000000A4000000A6000000A800
      0000AA000000AC000000AE000000B0000000B2000000B4000000B6000000B800
      0000BA000000BC000000BE000000C0000000C2000000C4000000C6000000C800
      0000CA000000CC000000CE000000D0000000D2000000D4000000D6000000D800
      0000DA000000DC000000888800008A8A00008C8C00008E8E0000909000009292
      00009494000096960000989800009A9A00009C9C00009E9E0000A0A00000A2A2
      0000A4A40000A6A60000A8A80000AAAA0000ACAC0000AEAE0000B0B00000B4B4
      0000B6B60000B8B80000BABA0000BCBC0000BEBE0000C0C00000C2C20000C4C4
      0000C6C60000C8C80000CACA0000CCCC0000CECE0000D0D00000D2D20000D4D4
      0000D6D60000D8D80000DADA0000DCDC000000880000008A0000008C0000008E
      00000090000000920000009400000096000000980000009A0000009C0000009E
      000000A0000000A2000000A4000000A6000000A8000000AA000000AC000000AE
      000000B0000000B2000000B4000000B6000000B8000000BA000000BC000000BE
      000000C0000000C2000000C4000000C6000000C8000000CA000000CC000000CE
      000000D0000000D2000000D4000000D6000000D8000000DA000000DC00000088
      8800008A8A00008C8C00008E8E00009090000092920000949400009696000098
      9800009A9A00009C9C00009E9E0000A0A00000A2A20000A4A40000A6A60000A8
      A80000AAAA0000ACAC0000AEAE0000B0B00000B2B20000B4B40000B6B60000B8
      B80000BABA0000BCBC0000BEBE0000C0C00000C2C20000C4C40000C6C60000C8
      C80000CACA0000CCCC0000CECE0000D0D00000D2D20000D4D40000D6D60000D8
      D80000DADA0000DCDC000000880000008A0000008C0000008E00000090000000
      920000009400000096000000980000009A0000009C0000009E000000A0000000
      A2000000A4000000A6000000A8000000AA000000AC000000B0000000B2000000
      B4000000B6000000B8000000BA000000BC000000BE000000C0000000C2000000
      C4000000C6000000C8000000CA000000CC000000CE000000D0000000D2000000
      D4000000D6000000D8000000DA000000DC00880088008A008A008C008C008E00
      8E0090009000920092009400940096009600980098009A009A009C009C009E00
      9E00A000A000A200A200A400A400A600A600A800A800AA00AA00AC00AC00AE00
      AE00B000B000B200B200B400B400B600B600B800B800BA00BA00BC00BC00BE00
      BE00C000C000C200C200C400C400C600C600C800C800CA00CA00CC00CC00CE00
      CE00D000D000D200D200D400D400D600D600D800D800FFFFFF0000C4B9B0A399
      928D8F9293989B9E9E9D9B9A9896938E8A837C746B6868645C5400CEC1B3A89A
      928A8A8B8D9093959698999898969593908A837A73726B615A5700D1C8B8AA9D
      92878385858687898D93969694949596959089837E776D665F5900C3BFB9AD9E
      9287807E7F818283878D93938F8D909393938E88827B70675D5700BBB6B0A79D
      92867E78787A7B8083868B8E8A85868B8B8E8D8B837A70675D5700B1ABA69E97
      908478737374757B7E818487867F80828386868580786D60585500A8A39B9892
      887D7A76736D707377797D80827D797B7D80807E7A736B5F5550009E9996918A
      828183817E726A6C6F7274787C7B757373787A78736E665D554D009E9B98928B
      878B8D8B887D6F65676A6D707577726B6B6E70726E665F59524C00AAA8A39B8E
      9195989792867A6C606163666B7070696164676A68615A544F4B00B5B2A89C93
      9A9D9F9E9C8D8173665E5D5F63686B6A635B5F61605D58504C4A00BAB3A69A9C
      A0A3A6A5A19687786A615D5A5D6063636059595B5B5A574F4C4800BBB1A59EA3
      A7ACAFADA89A8A7E766B605B575B5D5C5B584F555554524E4B4700BBAEA8A7AB
      B0B5B7B2ACA08F8C7F73645B5355585451514B4B4D4C4C48484400BDB8B0B0B4
      B7BCBEB8B3A89E95887A6A5E524D504D4847454445454344423F00C5BDBBB8BC
      BEC0C0B9B5AFA69C8E7E7061544C4A484544454544434242414000C5C6C5C2C6
      C6BEB9B3B0A9A096887B6D60534C48474545464645454847444200C4CACBC9CC
      C4BBB2A9A59E968A7D70635A504B484647474748484D504F4B4600C0C8CECCC6
      BFB8AEA09A8F867E70655B524C494847484A4A4B4D515454514C00BCC3C9C8C2
      BDB5A89B9384776F675B524C4A484A4C4D4E4D4E4F535658585300B9BFC2C3BE
      BBB2A5978E7F6E63584F4C4B494D5053545454505255585B5C5A00B7BDBEBEBB
      B9B1A4958C7C6C62534D4C4C50565858575756545457595B5E5D00B5B9B8B5B3
      B0ABA08F867465574F4C4F575F6464605D5C5B5956585A5D5F6000B3B3B1AEAA
      A69D94887F6D5D514D4F57626B706F6862605F5D585A5B5C5D5D00B0AEABA6A0
      9A8D867672675A4F4D55616D787D7A726B6863605D5C5C5B5B5A00A9A7A4A099
      8E86766C675B544F515F6D798587837B726D6862605D5B58565400A09E9C978F
      85746C625D5551525B6B798690938D8075706A64605E5D5B5A58009896938D84
      776A625D58515259677785929B9A8F8277706A6562605F5E5D5B00908D8B8377
      6B635955545257627282919EA39A8F8277706A65666662605F5D008583827A6F
      665E565353576170808E9BA3A29B908577706B686B6A6562605F00787678736A
      625A575658607081909B9EA09D958D8277726F6D6E6F6A656260000000000000
      0000000000000000000000000000000000000000000000000000}
    TabOrder = 9
    OnClick = BitBtn10Click
  end
  object StringGrid1: TStringGrid
    Left = 80
    Top = 0
    Width = 457
    Height = 250
    TabOrder = 10
    ColWidths = (
      64
      64
      64
      64
      64)
    RowHeights = (
      24
      24
      24
      24
      24)
  end
  object BitBtn12: TBitBtn
    Left = 543
    Top = 338
    Width = 107
    Height = 40
    Caption = 'Create Mask'
    TabOrder = 11
    OnClick = BitBtn12Click
  end
  object CheckBox11: TCheckBox
    Left = 663
    Top = 170
    Width = 97
    Height = 17
    Caption = 'Overwrite masks'
    TabOrder = 12
    OnClick = CheckBox11Click
  end
  object Lock1: TCheckBox
    Left = 543
    Top = 8
    Width = 56
    Height = 17
    Caption = 'Lock1'
    TabOrder = 13
  end
  object Lock2: TCheckBox
    Left = 543
    Top = 32
    Width = 56
    Height = 17
    Caption = 'Lock2'
    TabOrder = 14
  end
  object Lock3: TCheckBox
    Left = 543
    Top = 55
    Width = 56
    Height = 17
    Caption = 'Lock3'
    TabOrder = 15
  end
  object Lock4: TCheckBox
    Left = 543
    Top = 78
    Width = 56
    Height = 17
    Caption = 'Lock4'
    TabOrder = 16
  end
  object Lock5: TCheckBox
    Left = 543
    Top = 101
    Width = 56
    Height = 17
    Caption = 'Lock5'
    TabOrder = 17
  end
  object RadioGroup1: TRadioGroup
    Left = 567
    Top = 223
    Width = 227
    Height = 34
    Caption = 'Matches needed'
    Columns = 8
    DragKind = dkDock
    ItemIndex = 0
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8')
    TabOrder = 18
    OnClick = RadioGroup1Click
  end
  object BitBtn1: TBitBtn
    Left = 376
    Top = 275
    Width = 75
    Height = 25
    Caption = 'Set values'
    TabOrder = 19
    OnClick = BitBtn1Click
  end
  object Use6: TCheckBox
    Left = 8
    Top = 155
    Width = 53
    Height = 17
    Caption = 'Use 6'
    Checked = True
    State = cbChecked
    TabOrder = 20
    OnClick = Use6Click
  end
  object Lock6: TCheckBox
    Left = 543
    Top = 124
    Width = 97
    Height = 17
    Caption = 'Lock6'
    TabOrder = 21
  end
  object BitBtn2: TBitBtn
    Left = 159
    Top = 275
    Width = 107
    Height = 25
    Caption = 'Save table (HTML)'
    TabOrder = 22
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 295
    Top = 275
    Width = 75
    Height = 25
    Caption = 'Set mins'
    TabOrder = 23
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 457
    Top = 275
    Width = 75
    Height = 25
    Caption = 'Set maxes'
    TabOrder = 24
    OnClick = BitBtn4Click
  end
  object BitBtn5: TBitBtn
    Left = 543
    Top = 384
    Width = 87
    Height = 25
    Caption = 'Save DEM list'
    TabOrder = 25
    OnClick = BitBtn5Click
  end
  object BitBtn6: TBitBtn
    Left = 636
    Top = 384
    Width = 87
    Height = 25
    Caption = 'Load DEM list'
    TabOrder = 26
    OnClick = BitBtn6Click
  end
  object BitBtn7: TBitBtn
    Left = 662
    Top = 296
    Width = 89
    Height = 25
    Caption = 'Add single grid'
    TabOrder = 27
    OnClick = BitBtn7Click
  end
  object CheckBox8: TCheckBox
    Left = 663
    Top = 147
    Width = 97
    Height = 17
    Caption = 'Grid maps'
    TabOrder = 28
  end
  object BitBtn8: TBitBtn
    Left = 656
    Top = 353
    Width = 75
    Height = 25
    Caption = 'Save mask'
    TabOrder = 29
    OnClick = BitBtn8Click
  end
  object BitBtn9: TBitBtn
    Left = 8
    Top = 275
    Width = 126
    Height = 25
    Caption = 'Training point statistics'
    TabOrder = 30
    OnClick = BitBtn9Click
  end
  object Lock7: TCheckBox
    Left = 543
    Top = 147
    Width = 58
    Height = 17
    Caption = 'Lock7'
    TabOrder = 31
  end
  object Use7: TCheckBox
    Left = 8
    Top = 178
    Width = 66
    Height = 17
    Caption = 'Use 7'
    Checked = True
    State = cbChecked
    TabOrder = 32
    OnClick = Use7Click
  end
  object BitBtn11: TBitBtn
    Left = 8
    Top = 8
    Width = 25
    Height = 25
    Caption = 'All'
    TabOrder = 33
    OnClick = BitBtn11Click
  end
  object BitBtn13: TBitBtn
    Left = 40
    Top = 8
    Width = 34
    Height = 25
    Caption = 'None'
    TabOrder = 34
    OnClick = BitBtn13Click
  end
  object Req1: TCheckBox
    Left = 607
    Top = 8
    Width = 50
    Height = 17
    Caption = 'Req1'
    TabOrder = 35
    OnClick = Req1Click
  end
  object Req2: TCheckBox
    Left = 607
    Top = 31
    Width = 50
    Height = 17
    Caption = 'Req2'
    TabOrder = 36
    OnClick = Req2Click
  end
  object Req3: TCheckBox
    Left = 607
    Top = 46
    Width = 50
    Height = 17
    Caption = 'Req3'
    TabOrder = 37
    OnClick = Req3Click
  end
  object Req4: TCheckBox
    Left = 607
    Top = 69
    Width = 50
    Height = 17
    Caption = 'Req4'
    TabOrder = 38
    OnClick = Req4Click
  end
  object Req5: TCheckBox
    Left = 607
    Top = 92
    Width = 50
    Height = 17
    Caption = 'Req5'
    TabOrder = 39
    OnClick = Req5Click
  end
  object Req6: TCheckBox
    Left = 607
    Top = 115
    Width = 50
    Height = 17
    Caption = 'Req6'
    TabOrder = 40
    OnClick = Req6Click
  end
  object Req7: TCheckBox
    Left = 607
    Top = 138
    Width = 50
    Height = 17
    Caption = 'Req7'
    TabOrder = 41
    OnClick = Req7Click
  end
  object Use8: TCheckBox
    Left = 8
    Top = 201
    Width = 53
    Height = 17
    Caption = 'Use 8'
    Checked = True
    State = cbChecked
    TabOrder = 42
    OnClick = Use8Click
  end
  object Lock8: TCheckBox
    Left = 543
    Top = 170
    Width = 42
    Height = 17
    Caption = 'Lock8'
    TabOrder = 43
  end
  object Req8: TCheckBox
    Left = 607
    Top = 161
    Width = 49
    Height = 17
    Caption = 'Req8'
    TabOrder = 44
    OnClick = Req8Click
  end
  object Edit4: TEdit
    Left = 608
    Top = 452
    Width = 66
    Height = 21
    TabOrder = 45
    Text = 'Edit4'
    OnChange = Edit4Change
  end
  object BitBtn15: TBitBtn
    Left = 560
    Top = 544
    Width = 46
    Height = 25
    Caption = 'Expand'
    TabOrder = 46
    OnClick = BitBtn15Click
  end
  object Edit5: TEdit
    Left = 663
    Top = 490
    Width = 43
    Height = 21
    TabOrder = 47
  end
  object BitBtn16: TBitBtn
    Left = 714
    Top = 561
    Width = 75
    Height = 25
    Caption = 'Stats 2'
    TabOrder = 48
    OnClick = BitBtn16Click
  end
  object BitBtn18: TBitBtn
    Left = 612
    Top = 544
    Width = 46
    Height = 25
    Caption = 'Shrink'
    TabOrder = 49
    OnClick = BitBtn18Click
  end
  object BitBtn14: TBitBtn
    Left = 538
    Top = 256
    Width = 87
    Height = 25
    Caption = 'Mask options'
    TabOrder = 50
    OnClick = BitBtn14Click
  end
  object RadioGroup3: TRadioGroup
    Left = 536
    Top = 592
    Width = 241
    Height = 43
    Caption = 'Overlay grid'
    Columns = 8
    ItemIndex = 0
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8')
    TabOrder = 51
    OnClick = RadioGroup3Click
  end
  object BitBtn20: TBitBtn
    Left = 664
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Training set'
    TabOrder = 52
    OnClick = BitBtn20Click
  end
  object PageControl1: TPageControl
    Left = 20
    Top = 306
    Width = 421
    Height = 320
    ActivePage = TabSheet1
    TabOrder = 53
    object TabSheet1: TTabSheet
      Caption = 'History'
      object Memo4: TMemo
        Left = 0
        Top = 0
        Width = 413
        Height = 251
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object Panel1: TPanel
        Left = 0
        Top = 251
        Width = 413
        Height = 41
        Align = alBottom
        TabOrder = 1
        object BitBtn19: TBitBtn
          Left = 135
          Top = 16
          Width = 75
          Height = 25
          Caption = 'Save memo'
          TabOrder = 0
          OnClick = BitBtn19Click
        end
        object BitBtn17: TBitBtn
          Left = 35
          Top = 16
          Width = 75
          Height = 25
          Caption = 'Clear memo'
          TabOrder = 1
          OnClick = BitBtn17Click
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Current'
      ImageIndex = 1
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 413
        Height = 292
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Roaming'
      ImageIndex = 2
      object Memo2: TMemo
        Left = 3
        Top = 3
        Width = 286
        Height = 110
        Lines.Strings = (
          '')
        TabOrder = 0
      end
      object Memo3: TMemo
        Left = 3
        Top = 119
        Width = 286
        Height = 116
        TabOrder = 1
      end
    end
  end
  object Req9: TCheckBox
    Left = 608
    Top = 184
    Width = 49
    Height = 17
    Caption = 'Req9'
    TabOrder = 54
    OnClick = Req9Click
  end
  object Req10: TCheckBox
    Left = 608
    Top = 200
    Width = 50
    Height = 17
    Caption = 'Req10'
    TabOrder = 55
    OnClick = Req10Click
  end
  object Lock9: TCheckBox
    Left = 543
    Top = 184
    Width = 59
    Height = 17
    Caption = 'Lock9'
    TabOrder = 56
  end
  object Lock10: TCheckBox
    Left = 543
    Top = 207
    Width = 59
    Height = 17
    Caption = 'Lock10'
    TabOrder = 57
  end
  object Use9: TCheckBox
    Left = 8
    Top = 224
    Width = 66
    Height = 17
    Caption = 'Use9'
    Checked = True
    State = cbChecked
    TabOrder = 58
    OnClick = Use9Click
  end
  object Use10: TCheckBox
    Left = 8
    Top = 248
    Width = 97
    Height = 17
    Caption = 'Use10'
    Checked = True
    State = cbChecked
    TabOrder = 59
    OnClick = Use10Click
  end
  object Edit2: TEdit
    Left = 664
    Top = 517
    Width = 43
    Height = 21
    TabOrder = 60
    Text = 'Edit2'
  end
  object RadioGroup4: TRadioGroup
    Left = 712
    Top = 481
    Width = 76
    Height = 34
    Caption = 'Radius'
    Columns = 3
    Items.Strings = (
      '1'
      '2'
      '3')
    TabOrder = 61
  end
  object RadioGroup5: TRadioGroup
    Left = 713
    Top = 521
    Width = 76
    Height = 34
    Caption = 'Radius'
    Columns = 3
    Items.Strings = (
      '1'
      '2'
      '3')
    TabOrder = 62
  end
end
