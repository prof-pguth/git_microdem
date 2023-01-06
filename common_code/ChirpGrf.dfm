inherited ChirpGraph: TChirpGraph
  Left = 181
  Top = 201
  Caption = 'ChirpGraph'
  ClientHeight = 396
  ClientWidth = 1188
  ExplicitWidth = 1206
  ExplicitHeight = 468
  TextHeight = 16
  inherited ScrollBox1: TScrollBox
    Width = 1188
    Height = 318
    ExplicitWidth = 1194
    ExplicitHeight = 352
    inherited Image1: TImage
      Width = 1196
      Height = 382
      ExplicitTop = -1
      ExplicitWidth = 886
      ExplicitHeight = 362
    end
  end
  inherited Panel1: TPanel
    Top = 346
    Width = 1188
    Height = 50
    Caption = ''
    Font.Color = clBlack
    Font.Height = -16
    ExplicitTop = 380
    ExplicitWidth = 1194
    ExplicitHeight = 50
    object Label1: TLabel
      Left = 105
      Top = 6
      Width = 35
      Height = 19
      Caption = 'Gain'
      Font.Charset = ANSI_CHARSET
      Font.Color = clRed
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 1018
      Top = 6
      Width = 33
      Height = 19
      Caption = 'TVG'
      Enabled = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clRed
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 105
      Top = 30
      Width = 43
      Height = 16
      Caption = 'Label3'
      Font.Charset = ANSI_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label4: TLabel
      Left = 1018
      Top = 30
      Width = 18
      Height = 16
      Caption = '1.0'
      Enabled = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object SpinButton1: TSpinButton
      Left = 81
      Top = 6
      Width = 20
      Height = 25
      DownGlyph.Data = {
        DE000000424DDE00000000000000360000002800000009000000060000000100
        180000000000A800000000000000000000000000000000000000007F7F007F7F
        007F7F007F7F007F7F007F7F007F7F007F7F007F7F00007F7F007F7F007F7F00
        7F7F000000007F7F007F7F007F7F007F7F80007F7F007F7F007F7F0000000000
        00000000007F7F007F7F007F7FFF007F7F007F7F000000000000000000000000
        000000007F7F007F7F00007F7F00000000000000000000000000000000000000
        0000007F7F00007F7F007F7F007F7F007F7F007F7F007F7F007F7F007F7F007F
        7F00}
      TabOrder = 0
      UpGlyph.Data = {
        DE000000424DDE00000000000000360000002800000009000000060000000100
        180000000000A800000000000000000000000000000000000000007F7F007F7F
        007F7F007F7F007F7F007F7F007F7F007F7F007F7F00007F7F00000000000000
        0000000000000000000000000000007F7F80007F7F007F7F0000000000000000
        00000000000000007F7F007F7FFF007F7F007F7F007F7F000000000000000000
        007F7F007F7F007F7FFF007F7F007F7F007F7F007F7F000000007F7F007F7F00
        7F7F007F7FFF007F7F007F7F007F7F007F7F007F7F007F7F007F7F007F7F007F
        7FFF}
      OnDownClick = SpinButton1DownClick
      OnUpClick = SpinButton1UpClick
    end
    object SpinButton2: TSpinButton
      Left = 983
      Top = 6
      Width = 20
      Height = 25
      DownGlyph.Data = {
        DE000000424DDE00000000000000360000002800000009000000060000000100
        180000000000A800000000000000000000000000000000000000007F7F007F7F
        007F7F007F7F007F7F007F7F007F7F007F7F007F7F00007F7F007F7F007F7F00
        7F7F000000007F7F007F7F007F7F007F7F00007F7F007F7F007F7F0000000000
        00000000007F7F007F7F007F7F00007F7F007F7F000000000000000000000000
        000000007F7F007F7F00007F7F00000000000000000000000000000000000000
        0000007F7F00007F7F007F7F007F7F007F7F007F7F007F7F007F7F007F7F007F
        7F00}
      TabOrder = 1
      UpGlyph.Data = {
        DE000000424DDE00000000000000360000002800000009000000060000000100
        180000000000A800000000000000000000000000000000000000007F7F007F7F
        007F7F007F7F007F7F007F7F007F7F007F7F007F7F00007F7F00000000000000
        0000000000000000000000000000007F7F80007F7F007F7F0000000000000000
        00000000000000007F7F007F7FFF007F7F007F7F007F7F000000000000000000
        007F7F007F7F007F7FFF007F7F007F7F007F7F007F7F000000007F7F007F7F00
        7F7F007F7FFF007F7F007F7F007F7F007F7F007F7F007F7F007F7F007F7F007F
        7FFF}
      OnDownClick = SpinButton2DownClick
      OnUpClick = SpinButton2UpClick
    end
    object CheckBox1: TCheckBox
      Left = 527
      Top = 6
      Width = 113
      Height = 17
      Caption = 'Reverse line'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = CheckBox1Click
    end
    object RadioGroup1: TRadioGroup
      Left = 258
      Top = 6
      Width = 162
      Height = 35
      Caption = 'X thinnng'
      Columns = 6
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ItemIndex = 0
      Items.Strings = (
        '1'
        '2'
        '3'
        '4'
        '5'
        '6')
      ParentFont = False
      TabOrder = 3
      OnClick = RadioGroup1Click
    end
    object RadioGroup2: TRadioGroup
      Left = 423
      Top = 6
      Width = 98
      Height = 35
      Caption = 'Y thinnng'
      Columns = 3
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ItemIndex = 0
      Items.Strings = (
        '1'
        '2'
        '3')
      ParentFont = False
      TabOrder = 4
      OnClick = RadioGroup2Click
    end
    object BitBtn3: TBitBtn
      Left = -5
      Top = 6
      Width = 80
      Height = 25
      Caption = 'Redraw'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
        FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
        CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
        FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
      ParentFont = False
      TabOrder = 5
      OnClick = BitBtn3Click
    end
    object CheckBox2: TCheckBox
      Left = 527
      Top = 25
      Width = 113
      Height = 17
      Caption = 'Protractor'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 6
      OnClick = CheckBox2Click
    end
    object ChirpBitBtn1: TBitBtn
      Left = 727
      Top = 6
      Width = 89
      Height = 35
      Caption = 'Options'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333393333
        3333AAAAAAA9AAAAAAAAAAAAAAA9AAAAAAAAEEEEEEE9EEEEEEEEEEEEEEE9EEEE
        EEEE3333333933333333FFFFFFF9FFFFFFFFFFFFFFF9FFFFFFFFFFFFFFF9FFFF
        FFFFFFFFFF888FFFFFFFCCCCC88888CCCCCCFFFFF88888FFFFFFFFFFFFF8FFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      ParentFont = False
      TabOrder = 7
      OnClick = ChirpBitBtn1Click
    end
    object CheckBox3: TCheckBox
      Left = -232
      Top = 152
      Width = 97
      Height = 17
      Caption = 'CheckBox3'
      TabOrder = 8
    end
    object CheckBox4: TCheckBox
      Left = 832
      Top = 6
      Width = 97
      Height = 17
      Caption = 'TWTT'
      Font.Charset = ANSI_CHARSET
      Font.Color = clRed
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 9
      OnClick = CheckBox4Click
    end
    object CheckBox5: TCheckBox
      Left = 832
      Top = 24
      Width = 145
      Height = 17
      Caption = 'Immediate redraw'
      Font.Charset = ANSI_CHARSET
      Font.Color = clRed
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 11
      OnClick = CheckBox5Click
    end
    object RadioGroup3: TRadioGroup
      Left = 154
      Top = 4
      Width = 98
      Height = 35
      Caption = 'X dupe'
      Columns = 3
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ItemIndex = 0
      Items.Strings = (
        '1'
        '2'
        '3')
      ParentFont = False
      TabOrder = 10
      OnClick = RadioGroup3Click
    end
    object BitBtn1: TBitBtn
      Left = 646
      Top = 6
      Width = 75
      Height = 35
      Caption = 'Fence'
      TabOrder = 12
      OnClick = BitBtn1Click
    end
  end
  inherited ToolBar1: TToolBar
    Width = 1188
    ExplicitWidth = 1194
  end
  inherited MainMenu1: TMainMenu
    inherited Option1: TMenuItem
      object Chirpsoptions1: TMenuItem
        Caption = 'Chirps options'
        GroupIndex = 1
        OnClick = Chirpsoptions1Click
      end
    end
    object Chirpoptions1: TMenuItem
      Caption = 'Chirp options'
      GroupIndex = 3
      OnClick = Chirpoptions1Click
    end
  end
  inherited FontDialog1: TFontDialog
    Left = 176
    Top = 96
  end
  object ChirpPopupMenu2: TPopupMenu
    Left = 640
    Top = 200
    object Displayoptions1: TMenuItem
      Caption = 'Display options'
      OnClick = Displayoptions1Click
    end
    object Segment1: TMenuItem
      Caption = 'Segment'
      OnClick = Segment1Click
    end
    object Subset1: TMenuItem
      Caption = 'Subset'
      OnClick = Subset1Click
    end
    object Index1: TMenuItem
      Caption = 'Index'
      OnClick = Index1Click
    end
    object race1: TMenuItem
      Caption = 'Trace'
      OnClick = race1Click
    end
    object Depthfile1: TMenuItem
      Caption = 'Depth file'
      object New1: TMenuItem
        Caption = 'New'
        OnClick = New1Click
      end
      object OPen1: TMenuItem
        Caption = 'Open'
        OnClick = OPen1Click
      end
    end
    object Digitize1: TMenuItem
      Caption = 'Digitize'
      Enabled = False
      object Depths1: TMenuItem
        Caption = 'Depths'
        OnClick = Depths1Click
      end
      object hicknesses1: TMenuItem
        Caption = 'Thicknesses'
        OnClick = hicknesses1Click
      end
      object Done1: TMenuItem
        Caption = 'Done'
        OnClick = Done1Click
      end
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object Returnstoshow1: TMenuItem
      Caption = 'Returns to show'
      OnClick = Returnstoshow1Click
    end
  end
end
