inherited EROSContrastForm: TEROSContrastForm
  Left = 634
  Top = 251
  Caption = 'Image Contrast Enhancement'
  ClientHeight = 340
  ClientWidth = 622
  FormStyle = fsStayOnTop
  Position = poDefaultSizeOnly
  StyleElements = [seFont, seClient, seBorder]
  OnClose = FormClose
  OnCreate = FormCreate
  ExplicitWidth = 638
  ExplicitHeight = 379
  TextHeight = 15
  inherited Bevel1: TBevel
    Width = 409
    Height = 289
    ExplicitWidth = 409
    ExplicitHeight = 289
  end
  object Image1: TImage [1]
    Left = 432
    Top = 18
    Width = 128
    Height = 128
  end
  object RedrawSpeedButton12: TSpeedButton [2]
    Left = 423
    Top = 233
    Width = 25
    Height = 25
    Hint = 'Force redraw'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
      FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
      CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
      FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
    OnClick = RedrawSpeedButton12Click
  end
  object DupeMapSpeedButton18: TSpeedButton [3]
    Left = 468
    Top = 240
    Width = 25
    Height = 25
    Hint = 'New map window'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
      7777777777000000000777777707777777077777770777777707777777079999
      7707777777077799770770000007797977077077770797797707707777097777
      7707707777900000000770777977707777777077977770777777707977777077
      7777707777777077777770000000007777777777777777777777}
    OnClick = DupeMapSpeedButton18Click
  end
  inherited OKBtn: TButton
    Left = 18
    Top = 303
    OnClick = OKBtnClick
    ExplicitLeft = 18
    ExplicitTop = 303
  end
  inherited CancelBtn: TButton
    Left = 99
    Top = 303
    OnClick = CancelBtnClick
    ExplicitLeft = 99
    ExplicitTop = 303
  end
  object HelpBtn: TButton
    Left = 180
    Top = 303
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 2
    OnClick = HelpBtnClick
  end
  object CheckBox3: TCheckBox
    Left = 432
    Top = 168
    Width = 128
    Height = 17
    Caption = 'Quick redraw'
    TabOrder = 3
    OnClick = CheckBox3Click
  end
  object CheckBox1: TCheckBox
    Left = 432
    Top = 191
    Width = 161
    Height = 17
    Caption = 'Histogram ignore 0'
    TabOrder = 4
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 18
    Width = 401
    Height = 267
    ActivePage = TabSheet1
    TabOrder = 5
    object TabSheet1: TTabSheet
      Caption = 'Satellite band'
      object GroupBox2: TGroupBox
        Left = 3
        Top = 3
        Width = 257
        Height = 54
        Caption = 'Single band'
        TabOrder = 0
        object ComboBox4: TComboBox
          Left = 3
          Top = 30
          Width = 223
          Height = 23
          TabOrder = 0
          Text = 'ComboBox1'
          OnChange = ComboBox4Change
        end
        object UpDown1: TUpDown
          Left = 232
          Top = 26
          Width = 17
          Height = 25
          Position = 1
          TabOrder = 1
          OnClick = UpDown1Click
        end
      end
      object GroupBox1: TGroupBox
        Left = 3
        Top = 63
        Width = 259
        Height = 169
        Caption = 'Multiband '
        TabOrder = 1
        object TLabel
          Left = 16
          Top = 20
          Width = 101
          Height = 13
          Caption = 'Band to display in red'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label5: TLabel
          Left = 16
          Top = 44
          Width = 113
          Height = 13
          Caption = 'Band to display in green'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGreen
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label6: TLabel
          Left = 16
          Top = 116
          Width = 106
          Height = 13
          Caption = 'Band to display in blue'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object TLabel
          Left = 16
          Top = 68
          Width = 113
          Height = 13
          Caption = 'Band to display in green'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clLime
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object ComboBox1: TComboBox
          Left = 8
          Top = 36
          Width = 218
          Height = 23
          TabOrder = 0
          Text = ' '
          OnChange = ComboBox1Change
        end
        object ComboBox2: TComboBox
          Left = 8
          Top = 84
          Width = 218
          Height = 23
          TabOrder = 1
          Text = ' '
          OnChange = ComboBox2Change
        end
        object ComboBox3: TComboBox
          Left = 8
          Top = 132
          Width = 218
          Height = 23
          TabOrder = 2
          OnChange = ComboBox3Change
        end
      end
      object RadioGroup2: TRadioGroup
        Left = 268
        Top = 3
        Width = 122
        Height = 133
        Caption = 'Display mode'
        Items.Strings = (
          'Single band'
          'True color'
          'False color'
          'False veg color'
          'Pick 3 band')
        TabOrder = 2
        OnClick = RadioGroup2Click
      end
      object CheckBox2: TCheckBox
        Left = 268
        Top = 154
        Width = 122
        Height = 18
        Caption = 'Pan sharpened'
        TabOrder = 3
        OnClick = CheckBox2Click
      end
    end
    object Contrast: TTabSheet
      Caption = 'Contrast'
      ImageIndex = 1
      object Label1: TLabel
        Left = 263
        Top = 73
        Width = 84
        Height = 15
        Caption = 'Low tail size (%)'
      end
      object Label4: TLabel
        Left = 263
        Top = 13
        Width = 88
        Height = 15
        Caption = 'High tail size (%)'
      end
      object Label2: TLabel
        Left = 272
        Top = 144
        Width = 22
        Height = 15
        Caption = 'Low'
      end
      object Label3: TLabel
        Left = 272
        Top = 172
        Width = 26
        Height = 15
        Caption = 'High'
      end
      object RadioGroup1: TRadioGroup
        Left = 0
        Top = 3
        Width = 257
        Height = 159
        Caption = 'Enhancement'
        Items.Strings = (
          'None'
          'Histogram equalization'
          'Linear stretch'
          'Linear stretch, exclude tails'
          'Linear stretch, exclude cloud tail'
          'Linear stretch, defined')
        TabOrder = 0
        OnClick = RadioGroup1Click
      end
      object Edit1: TEdit
        Left = 326
        Top = 99
        Width = 49
        Height = 23
        TabOrder = 1
        Text = 'Edit1'
        OnChange = Edit1Change
      end
      object Edit4: TEdit
        Left = 326
        Top = 39
        Width = 49
        Height = 23
        TabOrder = 2
        Text = 'Edit4'
        OnChange = Edit4Change
      end
      object BitBtn1: TBitBtn
        Left = 3
        Top = 168
        Width = 119
        Height = 25
        Caption = 'Enhancement'
        TabOrder = 3
        OnClick = BitBtn1Click
      end
      object Edit2: TEdit
        Left = 326
        Top = 144
        Width = 49
        Height = 23
        TabOrder = 4
        Text = '0'
        OnChange = Edit2Change
      end
      object Edit3: TEdit
        Left = 326
        Top = 173
        Width = 49
        Height = 23
        TabOrder = 5
        Text = 'Edit3'
        OnChange = Edit3Change
      end
    end
  end
  object BitBtn2: TBitBtn
    Left = 423
    Top = 303
    Width = 122
    Height = 25
    Caption = 'Make defaults'
    TabOrder = 6
    OnClick = BitBtn2Click
  end
end
