inherited SSOCalcDlg: TSSOCalcDlg
  Left = 325
  Top = 120
  Caption = 'Topographic Fabric Calculation'
  ClientHeight = 458
  ClientWidth = 648
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  ExplicitWidth = 664
  ExplicitHeight = 497
  TextHeight = 15
  inherited Bevel1: TBevel
    Left = 252
    Top = 400
    Width = 224
    Height = 7
    ExplicitLeft = 252
    ExplicitTop = 400
    ExplicitWidth = 224
    ExplicitHeight = 7
  end
  inherited OKBtn: TButton
    Left = 151
    Top = 413
    Width = 36
    OnClick = OKBtnClick
    ExplicitLeft = 151
    ExplicitTop = 413
    ExplicitWidth = 36
  end
  inherited CancelBtn: TButton
    Left = 193
    Top = 413
    Width = 53
    OnClick = CancelBtnClick
    ExplicitLeft = 193
    ExplicitTop = 413
    ExplicitWidth = 53
  end
  object HelpBtn: TButton
    Left = 252
    Top = 413
    Width = 45
    Height = 25
    Caption = '&Help'
    TabOrder = 2
    OnClick = HelpBtnClick
  end
  object BitBtn3: TBitBtn
    Left = 13
    Top = 413
    Width = 132
    Height = 25
    Caption = 'New base map'
    TabOrder = 3
    OnClick = BitBtn3Click
  end
  object Memo1: TMemo
    Left = 391
    Top = 8
    Width = 266
    Height = 385
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object PageControl1: TPageControl
    Left = 1
    Top = -4
    Width = 384
    Height = 349
    ActivePage = TabSheet1
    TabOrder = 5
    object TabSheet1: TTabSheet
      Caption = 'Organization'
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 376
        Height = 319
        Align = alClient
        TabOrder = 0
        object Label1: TLabel
          Left = 7
          Top = 5
          Width = 109
          Height = 15
          Caption = 'Point Separation (m)'
        end
        object Label2: TLabel
          Left = 7
          Top = 30
          Width = 81
          Height = 15
          Caption = 'Region size (m)'
        end
        object Label3: TLabel
          Left = 14
          Top = 193
          Width = 84
          Height = 15
          Caption = 'Length Multiple'
        end
        object Label5: TLabel
          Left = 14
          Top = 164
          Width = 116
          Height = 15
          Caption = 'Flatness cutoff, s1s2 <'
        end
        object Label4: TLabel
          Left = 7
          Top = 79
          Width = 99
          Height = 15
          Caption = 'Min points for SSO'
        end
        object Label6: TLabel
          Left = 228
          Top = 29
          Width = 3
          Height = 15
        end
        object Label7: TLabel
          Left = 228
          Top = 3
          Width = 3
          Height = 15
        end
        object Label12: TLabel
          Left = 14
          Top = 135
          Width = 139
          Height = 15
          Caption = 'Organization cutoff, s2s3>'
        end
        object Label17: TLabel
          Left = 6
          Top = 60
          Width = 107
          Height = 15
          Caption = 'Sampling increment'
        end
        object Label18: TLabel
          Left = 226
          Top = 56
          Width = 3
          Height = 15
        end
        object Label21: TLabel
          Left = 170
          Top = 224
          Width = 21
          Height = 15
          Caption = 'Min'
        end
        object Label23: TLabel
          Left = 257
          Top = 221
          Width = 23
          Height = 15
          Caption = 'Max'
        end
        object Label24: TLabel
          Left = 208
          Top = 195
          Width = 61
          Height = 15
          Caption = 'Thin to plot'
        end
        object RedrawSpeedButton12: TSpeedButton
          Left = 226
          Top = 270
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
        object Edit1: TEdit
          Left = 179
          Top = 0
          Width = 43
          Height = 28
          TabOrder = 0
          Text = ' '
          OnChange = Edit1Change
        end
        object Edit2: TEdit
          Left = 179
          Top = 27
          Width = 41
          Height = 28
          TabOrder = 1
          Text = ' '
          OnChange = Edit2Change
        end
        object Edit3: TEdit
          Left = 138
          Top = 190
          Width = 43
          Height = 28
          TabOrder = 2
          Text = ' '
          OnChange = Edit3Change
        end
        object Edit5: TEdit
          Left = 194
          Top = 161
          Width = 43
          Height = 28
          TabOrder = 3
          Text = ' '
          OnChange = Edit5Change
        end
        object BitBtn1: TBitBtn
          Left = 6
          Top = 270
          Width = 89
          Height = 25
          Caption = 'Fabric'
          TabOrder = 4
          OnClick = BitBtn1Click
        end
        object Edit4: TEdit
          Left = 179
          Top = 76
          Width = 41
          Height = 28
          TabOrder = 5
        end
        object BitBtn2: TBitBtn
          Left = 101
          Top = 270
          Width = 119
          Height = 25
          Caption = 'Compute fabric'
          TabOrder = 6
          OnClick = BitBtn2Click
        end
        object Edit9: TEdit
          Left = 194
          Top = 127
          Width = 42
          Height = 28
          TabOrder = 7
        end
        object Edit14: TEdit
          Left = 179
          Top = 53
          Width = 41
          Height = 28
          TabOrder = 8
          OnChange = Edit14Change
        end
        object CheckBox1: TCheckBox
          Left = 13
          Top = 220
          Width = 119
          Height = 17
          Caption = 'Color by field'
          TabOrder = 9
          OnClick = CheckBox1Click
        end
        object Edit17: TEdit
          Left = 209
          Top = 221
          Width = 42
          Height = 28
          TabOrder = 10
          Text = 'Edit17'
          OnChange = Edit17Change
        end
        object Edit18: TEdit
          Left = 302
          Top = 221
          Width = 51
          Height = 28
          TabOrder = 11
          Text = 'Edit18'
          OnChange = Edit18Change
        end
        object CheckBox6: TCheckBox
          Left = 22
          Top = 103
          Width = 97
          Height = 17
          Caption = 'All in table'
          TabOrder = 12
          OnClick = CheckBox6Click
        end
        object Edit20: TEdit
          Left = 302
          Top = 190
          Width = 50
          Height = 28
          TabOrder = 13
          Text = 'Edit20'
          OnChange = Edit20Change
        end
        object ComboBox1: TComboBox
          Left = 7
          Top = 243
          Width = 111
          Height = 23
          TabOrder = 14
          OnChange = ComboBox1Change
        end
        object BitBtn5: TBitBtn
          Left = 257
          Top = 270
          Width = 92
          Height = 25
          Caption = 'Point fabric'
          TabOrder = 15
          OnClick = BitBtn5Click
        end
      end
    end
    object Wavelength: TTabSheet
      Caption = 'Wavelength'
      ImageIndex = 1
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 376
        Height = 314
        Align = alClient
        TabOrder = 0
        object Label8: TLabel
          Left = 76
          Top = 38
          Width = 157
          Height = 15
          Caption = 'Wavelength computation (m)'
        end
        object Label10: TLabel
          Left = 76
          Top = 68
          Width = 80
          Height = 15
          Caption = 'Min height (m)'
        end
        object Label11: TLabel
          Left = 76
          Top = 95
          Width = 81
          Height = 15
          Caption = 'Region (points)'
        end
        object Label22: TLabel
          Left = 76
          Top = 122
          Width = 77
          Height = 15
          Caption = 'Number peaks'
        end
        object Label9: TLabel
          Left = 15
          Top = 175
          Width = 3
          Height = 15
        end
        object Label16: TLabel
          Left = 76
          Top = 150
          Width = 130
          Height = 15
          Caption = 'Spacing along crests (m)'
        end
        object Label19: TLabel
          Left = 80
          Top = 179
          Width = 144
          Height = 15
          Caption = 'Dune sampling interval (m)'
        end
        object Label20: TLabel
          Left = 80
          Top = 200
          Width = 119
          Height = 15
          Caption = 'Max gap tracing crests'
        end
        object CheckBox3: TCheckBox
          Left = 15
          Top = 0
          Width = 226
          Height = 17
          Caption = 'Wavelength/height database'
          TabOrder = 0
          OnClick = CheckBox3Click
        end
        object Edit6: TEdit
          Left = 8
          Top = 65
          Width = 62
          Height = 28
          TabOrder = 1
        end
        object Edit7: TEdit
          Left = 8
          Top = 38
          Width = 62
          Height = 28
          TabOrder = 2
          OnChange = Edit7Change
        end
        object Edit19: TEdit
          Left = 7
          Top = 92
          Width = 63
          Height = 28
          TabOrder = 3
          OnChange = Edit19Change
        end
        object Edit24: TEdit
          Left = 6
          Top = 119
          Width = 64
          Height = 28
          TabOrder = 4
        end
        object CheckBox2: TCheckBox
          Left = 15
          Top = 15
          Width = 139
          Height = 17
          Caption = 'Show crests/troughs'
          TabOrder = 5
          OnClick = CheckBox2Click
        end
        object BitBtn4: TBitBtn
          Left = 95
          Top = 255
          Width = 75
          Height = 25
          Caption = 'Calculate'
          TabOrder = 6
          OnClick = BitBtn4Click
        end
        object Edit13: TEdit
          Left = 8
          Top = 147
          Width = 62
          Height = 28
          TabOrder = 7
        end
        object BitBtn8: TBitBtn
          Left = 176
          Top = 255
          Width = 105
          Height = 25
          Caption = 'Thread crests'
          TabOrder = 8
          OnClick = BitBtn8Click
        end
        object Edit15: TEdit
          Left = 8
          Top = 171
          Width = 62
          Height = 28
          TabOrder = 9
          Text = 'Edit15'
        end
        object Edit16: TEdit
          Left = 8
          Top = 194
          Width = 62
          Height = 28
          TabOrder = 10
          Text = 'Edit16'
        end
        object BitBtn9: TBitBtn
          Left = 14
          Top = 255
          Width = 75
          Height = 25
          Caption = 'Map crests'
          TabOrder = 11
          OnClick = BitBtn9Click
        end
        object BitBtn6: TBitBtn
          Left = 15
          Top = 286
          Width = 139
          Height = 25
          Caption = 'Point wavelength'
          TabOrder = 12
          OnClick = BitBtn6Click
        end
        object CheckBox4: TCheckBox
          Left = 10
          Top = 228
          Width = 209
          Height = 17
          Caption = 'Amplitude distance (m)'
          TabOrder = 13
        end
        object Edit8: TEdit
          Left = 225
          Top = 221
          Width = 43
          Height = 28
          TabOrder = 14
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Inflection'
      ImageIndex = 2
      object Label13: TLabel
        Left = 8
        Top = 16
        Width = 80
        Height = 15
        Caption = 'Min region (m)'
      end
      object Label14: TLabel
        Left = 8
        Top = 40
        Width = 82
        Height = 15
        Caption = 'Max region (m)'
      end
      object Label15: TLabel
        Left = 8
        Top = 64
        Width = 126
        Height = 15
        Caption = 'Saddle tolerance (posts)'
      end
      object Edit10: TEdit
        Left = 189
        Top = 10
        Width = 52
        Height = 28
        TabOrder = 0
        Text = 'Edit10'
        OnChange = Edit10Change
      end
      object Edit11: TEdit
        Left = 188
        Top = 37
        Width = 53
        Height = 28
        TabOrder = 1
        Text = 'Edit11'
      end
      object Edit12: TEdit
        Left = 188
        Top = 61
        Width = 53
        Height = 28
        TabOrder = 2
        Text = 'Edit12'
      end
      object BitBtn7: TBitBtn
        Left = 137
        Top = 136
        Width = 75
        Height = 25
        Caption = 'Inflection'
        TabOrder = 3
        OnClick = BitBtn7Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Options'
      ImageIndex = 3
      object CheckBox5: TCheckBox
        Left = 24
        Top = 9
        Width = 97
        Height = 17
        Caption = 'Plot aspect rose'
        TabOrder = 0
        OnClick = CheckBox5Click
      end
      object CheckBox7: TCheckBox
        Left = 24
        Top = 32
        Width = 185
        Height = 17
        Caption = 'Plot SSO diagram'
        TabOrder = 1
        OnClick = CheckBox7Click
      end
      object RadioGroup2: TRadioGroup
        Left = 8
        Top = 56
        Width = 201
        Height = 81
        Caption = 'SSO Plot'
        Items.Strings = (
          'Normals (poles)'
          'Downhill direction')
        TabOrder = 2
        OnClick = RadioGroup2Click
      end
      object CheckBox8: TCheckBox
        Left = 24
        Top = 144
        Width = 158
        Height = 17
        Caption = 'Terrain blowup'
        TabOrder = 3
        OnClick = CheckBox8Click
      end
    end
  end
  object RadioGroup1: TRadioGroup
    Left = 1
    Top = 359
    Width = 224
    Height = 48
    Caption = 'Cover'
    Columns = 2
    Items.Strings = (
      'Full DEM'
      'Map area')
    TabOrder = 6
    OnClick = RadioGroup1Click
  end
end
