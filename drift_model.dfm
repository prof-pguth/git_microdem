object Drifting_form: TDrifting_form
  Left = 0
  Top = 0
  ClientHeight = 141
  ClientWidth = 556
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = -1
    Width = 556
    Height = 142
    Align = alBottom
    Caption = 'Panel2'
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 103
      Top = 1
      Width = 452
      Height = 140
      ActivePage = TabSheet2
      Align = alClient
      TabOrder = 0
      object TabSheet2: TTabSheet
        Caption = 'Vector options'
        ImageIndex = 1
        object CheckBox6: TCheckBox
          Left = 121
          Top = 3
          Width = 97
          Height = 17
          Caption = 'Motions vectors'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = CheckBox6Click
        end
        object CheckBox7: TCheckBox
          Left = 121
          Top = 27
          Width = 97
          Height = 17
          Caption = 'Wind vectors'
          TabOrder = 1
          OnClick = CheckBox7Click
        end
        object BitBtn3: TBitBtn
          Left = 224
          Top = 9
          Width = 75
          Height = 25
          Caption = 'Ship'
          TabOrder = 2
          OnClick = BitBtn3Click
        end
        object BitBtn4: TBitBtn
          Left = 224
          Top = 40
          Width = 75
          Height = 25
          Caption = 'Tide'
          TabOrder = 3
          OnClick = BitBtn4Click
        end
        object BitBtn5: TBitBtn
          Left = 305
          Top = 9
          Width = 100
          Height = 25
          Caption = 'Wind current'
          TabOrder = 4
          OnClick = BitBtn5Click
        end
        object BitBtn6: TBitBtn
          Left = 305
          Top = 40
          Width = 100
          Height = 25
          Caption = 'Wind'
          TabOrder = 5
          OnClick = BitBtn6Click
        end
        object CheckBox1: TCheckBox
          Left = 121
          Top = 50
          Width = 97
          Height = 17
          Caption = 'Label times'
          TabOrder = 6
          OnClick = CheckBox1Click
        end
        object RadioGroup3: TRadioGroup
          Left = 3
          Top = 75
          Width = 185
          Height = 30
          Caption = 'Vector locations'
          Columns = 2
          ItemIndex = 1
          Items.Strings = (
            'Center start'
            'Tail to point')
          TabOrder = 7
          OnClick = RadioGroup3Click
        end
        object BitBtn15: TBitBtn
          Left = 279
          Top = 71
          Width = 74
          Height = 25
          Caption = 'Resultant'
          TabOrder = 8
          OnClick = BitBtn15Click
        end
        object GroupBox1: TGroupBox
          Left = 3
          Top = 3
          Width = 103
          Height = 66
          Caption = 'Consider'
          TabOrder = 9
          object CheckBox3: TCheckBox
            Left = 16
            Top = 48
            Width = 89
            Height = 17
            Caption = 'Wind current'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = CheckBox3Click
          end
          object CheckBox2: TCheckBox
            Left = 16
            Top = 30
            Width = 89
            Height = 17
            Caption = 'Tide current'
            Checked = True
            State = cbChecked
            TabOrder = 1
            OnClick = CheckBox2Click
          end
          object CheckBox4: TCheckBox
            Left = 16
            Top = 14
            Width = 89
            Height = 17
            Caption = 'Ship underway'
            Checked = True
            State = cbChecked
            TabOrder = 2
            OnClick = CheckBox4Click
          end
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'Uncertainty'
        ImageIndex = 2
        object Label1: TLabel
          Left = 147
          Top = 11
          Width = 113
          Height = 13
          Caption = 'Heading uncertainty ('#176')'
        end
        object Label2: TLabel
          Left = 144
          Top = 37
          Width = 125
          Height = 13
          Caption = 'Speed uncertainty (knots)'
        end
        object Label5: TLabel
          Left = 144
          Top = 64
          Width = 110
          Height = 13
          Caption = 'Speed uncertainty (%)'
        end
        object RadioGroup2: TRadioGroup
          Left = 3
          Top = 2
          Width = 121
          Height = 75
          Caption = 'Uncertainty'
          ItemIndex = 0
          Items.Strings = (
            'None'
            'Heading'
            'Speed'
            'Heading && Speed')
          TabOrder = 0
          OnClick = RadioGroup2Click
        end
        object Edit1: TEdit
          Left = 276
          Top = 3
          Width = 56
          Height = 21
          TabOrder = 1
          Text = '10'
        end
        object Edit2: TEdit
          Left = 275
          Top = 30
          Width = 57
          Height = 21
          TabOrder = 2
          Text = '0.5'
        end
        object Edit6: TEdit
          Left = 275
          Top = 57
          Width = 57
          Height = 21
          TabOrder = 3
          Text = '25'
        end
        object RadioGroup1: TRadioGroup
          Left = 3
          Top = 78
          Width = 230
          Height = 31
          Caption = 'Speed uncertainty'
          Columns = 2
          ItemIndex = 1
          Items.Strings = (
            'Magnitude'
            'Percentage')
          TabOrder = 4
          OnClick = RadioGroup1Click
        end
      end
      object O: TTabSheet
        Caption = 'Oceanography'
        ImageIndex = 3
        object Label3: TLabel
          Left = 16
          Top = 12
          Width = 154
          Height = 13
          Caption = 'Wind current (% of wind speed)'
        end
        object Label4: TLabel
          Left = 16
          Top = 39
          Width = 170
          Height = 13
          Caption = 'Coriolis deflection ('#186'; +CW, - CCW)'
        end
        object Edit4: TEdit
          Left = 192
          Top = 36
          Width = 57
          Height = 21
          TabOrder = 0
          Text = '15'
          OnChange = Edit4Change
        end
        object Edit3: TEdit
          Left = 192
          Top = 9
          Width = 56
          Height = 21
          TabOrder = 1
          Text = '3'
          OnChange = Edit3Change
        end
      end
      object TabSheet1: TTabSheet
        Caption = 'Model DB'
        ImageIndex = 3
        object BitBtn17: TBitBtn
          Left = 175
          Top = 3
          Width = 122
          Height = 25
          Caption = 'Range of models'
          TabOrder = 0
          OnClick = BitBtn17Click
        end
        object BitBtn7: TBitBtn
          Left = 176
          Top = 34
          Width = 121
          Height = 25
          Caption = 'Constant ship speed'
          TabOrder = 1
          OnClick = BitBtn7Click
        end
        object BitBtn8: TBitBtn
          Left = 176
          Top = 65
          Width = 121
          Height = 25
          Caption = 'Constant ship heading'
          TabOrder = 2
          OnClick = BitBtn8Click
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'Tides'
        ImageIndex = 4
        object Label7: TLabel
          Left = 10
          Top = 39
          Width = 104
          Height = 13
          Caption = 'Movie time delay (ms)'
        end
        object ComboBox1: TComboBox
          Left = 3
          Top = 3
          Width = 161
          Height = 21
          TabOrder = 0
          OnChange = ComboBox1Change
        end
        object BitBtn9: TBitBtn
          Left = 223
          Top = 84
          Width = 75
          Height = 25
          Caption = 'Tide plot'
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000010000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
            FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
            CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
            FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
          TabOrder = 1
          OnClick = BitBtn9Click
        end
        object CheckBox8: TCheckBox
          Left = 17
          Top = 94
          Width = 97
          Height = 17
          Caption = 'Label vectors'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object BitBtn10: TBitBtn
          Left = 321
          Top = 52
          Width = 84
          Height = 25
          Caption = 'Tide movie'
          TabOrder = 3
          OnClick = BitBtn10Click
        end
        object Edit8: TEdit
          Left = 120
          Top = 34
          Width = 89
          Height = 21
          TabOrder = 4
          Text = '750'
        end
        object BitBtn11: TBitBtn
          Left = 321
          Top = 83
          Width = 84
          Height = 26
          Caption = 'New tide  DBF'
          TabOrder = 5
          OnClick = BitBtn11Click
        end
        object BitBtn12: TBitBtn
          Left = 170
          Top = 3
          Width = 26
          Height = 25
          Caption = '<'
          TabOrder = 6
          OnClick = BitBtn12Click
        end
        object BitBtn13: TBitBtn
          Left = 202
          Top = 3
          Width = 26
          Height = 25
          Caption = '>'
          TabOrder = 7
          OnClick = BitBtn13Click
        end
        object CheckBox5: TCheckBox
          Left = 120
          Top = 92
          Width = 97
          Height = 17
          Caption = 'Tide DBF'
          Checked = True
          State = cbChecked
          TabOrder = 8
        end
      end
      object TabSheet5: TTabSheet
        Caption = 'Adjust'
        ImageIndex = 5
        object CheckBox10: TCheckBox
          Left = 16
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Ship speeds'
          TabOrder = 0
        end
        object CheckBox11: TCheckBox
          Left = 16
          Top = 48
          Width = 97
          Height = 17
          Caption = 'Ship heading'
          TabOrder = 1
        end
      end
      object TabSheet6: TTabSheet
        Caption = 'Monte Carlo'
        ImageIndex = 6
        object Label9: TLabel
          Left = 8
          Top = 24
          Width = 52
          Height = 13
          Caption = 'Ship speed'
        end
        object Label10: TLabel
          Left = 3
          Top = 59
          Width = 61
          Height = 13
          Caption = 'Ship heading'
        end
        object Label11: TLabel
          Left = 104
          Top = 2
          Width = 38
          Height = 13
          Caption = 'Std Dev'
        end
        object Label12: TLabel
          Left = 240
          Top = 16
          Width = 74
          Height = 13
          Caption = 'Number of runs'
        end
        object Label13: TLabel
          Left = 240
          Top = 43
          Width = 59
          Height = 13
          Caption = 'Grid size (m)'
        end
        object Edit9: TEdit
          Left = 88
          Top = 21
          Width = 73
          Height = 21
          TabOrder = 0
          Text = '0.5'
        end
        object Edit10: TEdit
          Left = 88
          Top = 56
          Width = 73
          Height = 21
          TabOrder = 1
          Text = '10'
        end
        object Edit11: TEdit
          Left = 327
          Top = 13
          Width = 65
          Height = 21
          TabOrder = 2
          Text = '1000'
        end
        object BitBtn14: TBitBtn
          Left = 240
          Top = 84
          Width = 75
          Height = 25
          Caption = 'Run'
          TabOrder = 3
          OnClick = BitBtn14Click
        end
        object Edit12: TEdit
          Left = 327
          Top = 40
          Width = 66
          Height = 21
          TabOrder = 4
          Text = '500'
        end
      end
    end
    object Panel1: TPanel
      Left = 1
      Top = 1
      Width = 102
      Height = 140
      Align = alLeft
      TabOrder = 1
      object BitBtn1: TBitBtn
        Left = 7
        Top = 32
        Width = 89
        Height = 25
        Caption = 'Drift plot'
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
          FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
          CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
          FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
        TabOrder = 0
        OnClick = BitBtn1Click
      end
      object BitBtn2: TBitBtn
        Left = 7
        Top = 63
        Width = 89
        Height = 25
        Caption = 'Drift movie'
        TabOrder = 1
        OnClick = BitBtn2Click
      end
      object HelpBtn: TBitBtn
        Left = 7
        Top = 105
        Width = 89
        Height = 27
        Kind = bkHelp
        Margin = 2
        NumGlyphs = 2
        Spacing = -1
        TabOrder = 2
        OnClick = HelpBtnClick
        IsControl = True
      end
    end
  end
end
