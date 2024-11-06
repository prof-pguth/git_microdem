object HyperspectralForm: THyperspectralForm
  Left = 0
  Top = 0
  Caption = 'HyperspectralForm'
  ClientHeight = 501
  ClientWidth = 856
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Panel1: TPanel
    Left = 265
    Top = 0
    Width = 285
    Height = 501
    Align = alLeft
    TabOrder = 0
    ExplicitHeight = 506
    object Label1: TLabel
      Left = 40
      Top = 45
      Width = 31
      Height = 13
      Caption = 'Label1'
    end
    object Label2: TLabel
      Left = 32
      Top = 109
      Width = 31
      Height = 13
      Caption = 'Label2'
    end
    object Label3: TLabel
      Left = 32
      Top = 181
      Width = 31
      Height = 13
      Caption = 'Label3'
    end
    object TrackBar1: TTrackBar
      Left = 23
      Top = 64
      Width = 257
      Height = 39
      Max = 224
      Min = 1
      Frequency = 25
      Position = 128
      TabOrder = 0
      OnChange = TrackBar1Change
      OnKeyUp = TrackBar1KeyUp
    end
    object TrackBar2: TTrackBar
      Left = 20
      Top = 128
      Width = 257
      Height = 45
      Max = 224
      Min = 1
      Frequency = 25
      Position = 33
      TabOrder = 1
      OnChange = TrackBar2Change
      OnKeyUp = TrackBar2KeyUp
    end
    object TrackBar3: TTrackBar
      Left = 20
      Top = 200
      Width = 257
      Height = 45
      Max = 224
      Min = 1
      Frequency = 25
      Position = 10
      TabOrder = 2
      OnChange = TrackBar3Change
      OnEndDrag = TrackBar3EndDrag
      OnKeyUp = TrackBar3KeyUp
    end
    object RadioGroup1: TRadioGroup
      Left = 64
      Top = 8
      Width = 215
      Height = 33
      Caption = 'Display'
      Columns = 2
      ItemIndex = 1
      Items.Strings = (
        'Single band'
        'Color composite')
      TabOrder = 3
      OnClick = RadioGroup1Click
    end
    object BitBtn2: TBitBtn
      Left = 33
      Top = 287
      Width = 153
      Height = 25
      Caption = 'Hyperspectral cube'
      TabOrder = 4
      OnClick = BitBtn2Click
    end
    object BitBtn4: TBitBtn
      Left = 32
      Top = 320
      Width = 153
      Height = 25
      Caption = 'Subset hyperspectral cube'
      TabOrder = 5
      OnClick = BitBtn4Click
    end
    object BitBtn5: TBitBtn
      Left = 32
      Top = 382
      Width = 153
      Height = 25
      Caption = 'Scatter plots'
      TabOrder = 6
      OnClick = BitBtn5Click
    end
    object BitBtn6: TBitBtn
      Left = 32
      Top = 413
      Width = 153
      Height = 25
      Caption = 'True color image'
      TabOrder = 7
      OnClick = BitBtn6Click
    end
    object HelpBtn: TBitBtn
      Left = 191
      Top = 287
      Width = 77
      Height = 27
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 8
      OnClick = HelpBtnClick
      IsControl = True
    end
    object CheckBox1: TCheckBox
      Left = 192
      Top = 264
      Width = 97
      Height = 17
      Caption = 'Band centers'
      Checked = True
      State = cbChecked
      TabOrder = 9
    end
    object BitBtn7: TBitBtn
      Left = 8
      Top = 9
      Width = 26
      Height = 25
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
        FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
        CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
        FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
      TabOrder = 10
      OnClick = BitBtn7Click
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 265
    Height = 501
    Align = alLeft
    TabOrder = 1
    ExplicitHeight = 506
    object Image1: TImage
      Left = 0
      Top = -2
      Width = 105
      Height = 105
      AutoSize = True
      OnMouseMove = Image1MouseMove
    end
  end
  object Panel2: TPanel
    Left = 550
    Top = 0
    Width = 300
    Height = 539
    TabOrder = 2
    Visible = False
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 312
      Height = 537
      ActivePage = TabSheet1
      Align = alLeft
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'Display'
        object Label4: TLabel
          Left = 152
          Top = 127
          Width = 60
          Height = 13
          Caption = 'High tail (%)'
        end
        object Label5: TLabel
          Left = 154
          Top = 154
          Width = 58
          Height = 13
          Caption = 'Low tail (%)'
        end
        object Red: TLabel
          Left = 200
          Top = 200
          Width = 19
          Height = 13
          Caption = 'Red'
        end
        object Label6: TLabel
          Left = 200
          Top = 222
          Width = 29
          Height = 13
          Caption = 'Green'
        end
        object Label7: TLabel
          Left = 192
          Top = 200
          Width = 3
          Height = 13
        end
        object Label8: TLabel
          Left = 200
          Top = 249
          Width = 20
          Height = 13
          Caption = 'Blue'
        end
        object Label9: TLabel
          Left = 192
          Top = 280
          Width = 47
          Height = 13
          Caption = 'Grayscale'
        end
        object ListBox2: TListBox
          Left = 200
          Top = 300
          Width = 81
          Height = 84
          ItemHeight = 13
          TabOrder = 0
          Visible = False
          OnClick = ListBox2Click
        end
        object BitBtn11: TBitBtn
          Left = 87
          Top = 409
          Width = 75
          Height = 25
          Caption = 'True color'
          TabOrder = 1
          Visible = False
          OnClick = BitBtn11Click
        end
        object BitBtn10: TBitBtn
          Left = 6
          Top = 409
          Width = 75
          Height = 25
          Caption = 'False color'
          TabOrder = 2
          Visible = False
          OnClick = BitBtn10Click
        end
        object CheckBox2: TCheckBox
          Left = 151
          Top = 104
          Width = 98
          Height = 17
          Caption = 'By Wavelength'
          TabOrder = 3
          OnClick = CheckBox2Click
        end
        object BitBtn8: TBitBtn
          Left = 3
          Top = 8
          Width = 129
          Height = 25
          Caption = 'Basic band stats'
          TabOrder = 4
          OnClick = BitBtn8Click
        end
        object BitBtn3: TBitBtn
          Left = 138
          Top = 8
          Width = 150
          Height = 25
          Caption = 'Unsupervised classification'
          TabOrder = 5
          OnClick = BitBtn3Click
        end
        object BitBtn13: TBitBtn
          Left = 3
          Top = 39
          Width = 129
          Height = 25
          Caption = 'Histograms'
          TabOrder = 6
          OnClick = BitBtn13Click
        end
        object BitBtn14: TBitBtn
          Left = 138
          Top = 39
          Width = 151
          Height = 25
          Caption = 'Grid scattergrams'
          TabOrder = 7
          OnClick = BitBtn14Click
        end
        object BitBtn15: TBitBtn
          Left = 3
          Top = 70
          Width = 129
          Height = 25
          Caption = 'Correlation matrix'
          TabOrder = 8
          OnClick = BitBtn15Click
        end
        object Edit1: TEdit
          Left = 218
          Top = 127
          Width = 41
          Height = 21
          TabOrder = 9
          Text = 'Edit1'
          OnChange = Edit1Change
        end
        object Edit2: TEdit
          Left = 218
          Top = 154
          Width = 41
          Height = 21
          TabOrder = 10
          Text = 'Edit1'
          OnChange = Edit2Change
        end
        object BitBtn16: TBitBtn
          Left = 16
          Top = 125
          Width = 75
          Height = 25
          Caption = 'Redraw'
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000010000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
            FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
            CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
            FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
          TabOrder = 11
          OnClick = BitBtn16Click
        end
        object ComboBox1: TComboBox
          Left = 1
          Top = 192
          Width = 184
          Height = 21
          TabOrder = 12
          Text = 'ComboBox1'
          OnChange = ComboBox1Change
        end
        object ComboBox2: TComboBox
          Left = 1
          Top = 219
          Width = 184
          Height = 21
          TabOrder = 13
          Text = 'ComboBox2'
          OnChange = ComboBox2Change
        end
        object ComboBox3: TComboBox
          Left = 1
          Top = 246
          Width = 184
          Height = 21
          TabOrder = 14
          Text = 'ComboBox3'
          OnChange = ComboBox3Change
        end
        object ComboBox4: TComboBox
          Left = 3
          Top = 273
          Width = 182
          Height = 21
          TabOrder = 15
          Text = 'ComboBox4'
          OnChange = ComboBox4Change
        end
        object BitBt12: TBitBtn
          Left = 168
          Top = 409
          Width = 75
          Height = 25
          Caption = 'Pick bands'
          TabOrder = 16
          Visible = False
        end
        object BitBtn9: TBitBtn
          Left = 138
          Top = 72
          Width = 151
          Height = 25
          Caption = 'Class scattergrams'
          Enabled = False
          TabOrder = 17
          OnClick = BitBtn9Click
        end
        object RadioGroup2: TRadioGroup
          Left = 8
          Top = 304
          Width = 153
          Height = 99
          Caption = 'Display mode'
          Items.Strings = (
            'Grayscale single band'
            'RGB true color'
            'RGB false color'
            'RGB pick bands')
          TabOrder = 18
          OnClick = RadioGroup2Click
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Reflectance spectra'
        ImageIndex = 1
        object BitBtn1: TBitBtn
          Left = 1
          Top = 6
          Width = 193
          Height = 25
          Caption = 'Reflectance library spectra'
          TabOrder = 0
          OnClick = BitBtn1Click
        end
        object ListBox1: TListBox
          Left = 3
          Top = 37
          Width = 193
          Height = 388
          ItemHeight = 13
          TabOrder = 1
          OnClick = ListBox1Click
        end
      end
    end
  end
end
