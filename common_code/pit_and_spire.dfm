object PitSpireForm: TPitSpireForm
  Left = 0
  Top = 0
  BorderIcons = [biMaximize]
  Caption = 'Feature detection'
  ClientHeight = 558
  ClientWidth = 570
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Memo1: TMemo
    Left = 300
    Top = 0
    Width = 270
    Height = 558
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitWidth = 278
    ExplicitHeight = 560
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 300
    Height = 558
    Align = alLeft
    TabOrder = 1
    ExplicitHeight = 560
    object Label27: TLabel
      Left = 48
      Top = 264
      Width = 37
      Height = 13
      Caption = 'Label27'
    end
    object CancelBtn: TBitBtn
      Left = 21
      Top = 512
      Width = 66
      Height = 27
      Kind = bkCancel
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 0
      OnClick = CancelBtnClick
      IsControl = True
    end
    object HelpBtn: TBitBtn
      Left = 116
      Top = 512
      Width = 58
      Height = 27
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 1
      OnClick = HelpBtnClick
      IsControl = True
    end
    object RadioGroup1: TRadioGroup
      Left = 13
      Top = 407
      Width = 193
      Height = 38
      Caption = 'Analyze'
      Columns = 2
      ItemIndex = 1
      Items.Strings = (
        'Entire DEM'
        'Map area only')
      TabOrder = 2
    end
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 298
      Height = 377
      ActivePage = TabSheet1
      Align = alTop
      TabOrder = 3
      object TabSheet1: TTabSheet
        Caption = 'Pit/spire'
        object GroupBox3: TGroupBox
          Left = 3
          Top = 218
          Width = 249
          Height = 123
          Caption = 'Spires'
          TabOrder = 0
          object Label19: TLabel
            Left = 3
            Top = 49
            Width = 77
            Height = 13
            Caption = 'Min points lower'
          end
          object Label2: TLabel
            Left = 3
            Top = 30
            Width = 76
            Height = 13
            Caption = 'Spire height (m)'
          end
          object Label5: TLabel
            Left = 3
            Top = 11
            Width = 110
            Height = 13
            Caption = 'Spire search radius (m)'
          end
          object BitBtn2: TBitBtn
            Left = 131
            Top = 86
            Width = 75
            Height = 25
            Caption = 'Spires'
            TabOrder = 0
            OnClick = BitBtn2Click
          end
          object BitBtn7: TBitBtn
            Left = 3
            Top = 86
            Width = 75
            Height = 25
            Caption = 'Spires DB'
            TabOrder = 1
            OnClick = BitBtn7Click
          end
          object Edit19: TEdit
            Left = 131
            Top = 59
            Width = 59
            Height = 21
            TabOrder = 2
          end
          object Edit2: TEdit
            Left = 134
            Top = 32
            Width = 57
            Height = 21
            TabOrder = 3
          end
          object Edit1: TEdit
            Left = 133
            Top = 8
            Width = 57
            Height = 21
            TabOrder = 4
          end
        end
        object GroupBox2: TGroupBox
          Left = 3
          Top = 118
          Width = 249
          Height = 94
          Caption = 'Pits'
          TabOrder = 1
          object Label4: TLabel
            Left = 18
            Top = 35
            Width = 62
            Height = 13
            Caption = 'Pit depth (m)'
          end
          object Label3: TLabel
            Left = 28
            Top = 16
            Width = 98
            Height = 13
            Caption = 'Pit search radius (m)'
          end
          object BitBtn3: TBitBtn
            Left = 136
            Top = 64
            Width = 75
            Height = 25
            Caption = 'Pits'
            TabOrder = 0
            OnClick = BitBtn3Click
          end
          object Edit3: TEdit
            Left = 140
            Top = 10
            Width = 57
            Height = 21
            TabOrder = 1
          end
          object Edit4: TEdit
            Left = 140
            Top = 37
            Width = 57
            Height = 21
            TabOrder = 2
          end
          object BitBtn9: TBitBtn
            Left = 13
            Top = 64
            Width = 75
            Height = 25
            Caption = 'Pits DB'
            TabOrder = 3
            OnClick = BitBtn9Click
          end
        end
        object GroupBox1: TGroupBox
          Left = 3
          Top = 3
          Width = 249
          Height = 109
          Caption = 'Peaks'
          TabOrder = 2
          object Label6: TLabel
            Left = 11
            Top = 51
            Width = 75
            Height = 13
            Caption = 'Peak height (m)'
          end
          object Label1: TLabel
            Left = 10
            Top = 21
            Width = 109
            Height = 13
            Caption = 'Peak search radius (m)'
          end
          object BitBtn13: TBitBtn
            Left = 21
            Top = 70
            Width = 75
            Height = 25
            Caption = 'Peaks DB'
            TabOrder = 0
            OnClick = BitBtn13Click
          end
          object BitBtn4: TBitBtn
            Left = 144
            Top = 71
            Width = 75
            Height = 25
            Caption = 'Peaks'
            TabOrder = 1
            OnClick = BitBtn4Click
          end
          object Edit6: TEdit
            Left = 144
            Top = 48
            Width = 57
            Height = 21
            TabOrder = 2
          end
          object Edit5: TEdit
            Left = 144
            Top = 21
            Width = 57
            Height = 21
            TabOrder = 3
          end
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Building'
        ImageIndex = 1
        object Label7: TLabel
          Left = 8
          Top = 3
          Width = 107
          Height = 13
          Caption = 'Min building height (m)'
        end
        object Label8: TLabel
          Left = 13
          Top = 57
          Width = 77
          Height = 13
          Caption = 'Min points lower'
        end
        object Label9: TLabel
          Left = 3
          Top = 87
          Width = 81
          Height = 13
          Caption = 'Max points lower'
        end
        object Label17: TLabel
          Left = 13
          Top = 122
          Width = 110
          Height = 13
          Caption = 'Roof search region (m)'
        end
        object Label18: TLabel
          Left = 3
          Top = 38
          Width = 114
          Height = 13
          Caption = 'Max building height (m) '
        end
        object Label20: TLabel
          Left = 16
          Top = 224
          Width = 93
          Height = 13
          Caption = 'Max roof slope (%)'
        end
        object Label25: TLabel
          Left = 13
          Top = 146
          Width = 93
          Height = 13
          Caption = 'Min edge slope (%)'
        end
        object Label26: TLabel
          Left = 14
          Top = 172
          Width = 95
          Height = 13
          Caption = 'Min building size (m)'
        end
        object Label28: TLabel
          Left = 24
          Top = 248
          Width = 89
          Height = 13
          Caption = 'Min roof slope (%)'
        end
        object Label29: TLabel
          Left = 24
          Top = 267
          Width = 74
          Height = 13
          Caption = 'Slope tolerance'
        end
        object Label30: TLabel
          Left = 24
          Top = 288
          Width = 81
          Height = 13
          Caption = 'Aspect tolerance'
        end
        object Label31: TLabel
          Left = 24
          Top = 317
          Width = 48
          Height = 13
          Caption = 'Neighbors'
        end
        object Label32: TLabel
          Left = 16
          Top = 191
          Width = 99
          Height = 13
          Caption = 'Max building size (m)'
        end
        object Edit7: TEdit
          Left = 121
          Top = 3
          Width = 72
          Height = 21
          TabOrder = 0
        end
        object Edit8: TEdit
          Left = 121
          Top = 57
          Width = 72
          Height = 21
          TabOrder = 1
        end
        object Edit9: TEdit
          Left = 121
          Top = 84
          Width = 72
          Height = 21
          TabOrder = 2
        end
        object BitBtn8: TBitBtn
          Left = 204
          Top = 167
          Width = 83
          Height = 25
          Caption = 'Flat roof'
          TabOrder = 3
          OnClick = BitBtn8Click
        end
        object Edit17: TEdit
          Left = 129
          Top = 119
          Width = 65
          Height = 21
          TabOrder = 4
        end
        object BitBtn5: TBitBtn
          Left = 212
          Top = 75
          Width = 75
          Height = 25
          Caption = 'Edges'
          TabOrder = 5
          OnClick = BitBtn5Click
        end
        object Edit18: TEdit
          Left = 118
          Top = 30
          Width = 72
          Height = 21
          TabOrder = 6
        end
        object Edit20: TEdit
          Left = 132
          Top = 218
          Width = 65
          Height = 21
          TabOrder = 7
        end
        object RadioGroup2: TRadioGroup
          Left = 203
          Top = 122
          Width = 84
          Height = 39
          Caption = 'Roof algorithm'
          Columns = 3
          ItemIndex = 2
          Items.Strings = (
            '1'
            '2'
            '3')
          TabOrder = 8
        end
        object Edit27: TEdit
          Left = 129
          Top = 146
          Width = 65
          Height = 21
          TabOrder = 9
        end
        object Edit28: TEdit
          Left = 129
          Top = 173
          Width = 65
          Height = 21
          TabOrder = 10
          OnChange = Edit28Change
        end
        object Edit29: TEdit
          Left = 132
          Top = 245
          Width = 65
          Height = 21
          TabOrder = 11
        end
        object Edit30: TEdit
          Left = 132
          Top = 266
          Width = 65
          Height = 21
          TabOrder = 12
        end
        object Edit31: TEdit
          Left = 132
          Top = 288
          Width = 65
          Height = 21
          TabOrder = 13
        end
        object Edit32: TEdit
          Left = 132
          Top = 312
          Width = 65
          Height = 21
          TabOrder = 14
        end
        object PeakRoofBtn: TBitBtn
          Left = 203
          Top = 293
          Width = 75
          Height = 25
          Caption = 'Peaked roof'
          TabOrder = 15
          OnClick = PeakRoofBtnClick
        end
        object Flatroadbtn: TBitBtn
          Left = 208
          Top = 240
          Width = 75
          Height = 25
          Caption = 'Flat road'
          TabOrder = 16
          OnClick = FlatroadbtnClick
        end
        object Edit33: TEdit
          Left = 129
          Top = 192
          Width = 65
          Height = 21
          TabOrder = 17
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'Power line'
        ImageIndex = 2
        object Label10: TLabel
          Left = 8
          Top = 48
          Width = 103
          Height = 13
          Caption = 'Lowerst vacant voxel'
        end
        object Label11: TLabel
          Left = 8
          Top = 24
          Width = 101
          Height = 13
          Caption = 'Highest vacant voxel'
        end
        object Label12: TLabel
          Left = 8
          Top = 88
          Width = 99
          Height = 13
          Caption = 'Max occupied voxels'
        end
        object TLabel
          Left = 16
          Top = 120
          Width = 3
          Height = 13
        end
        object Label13: TLabel
          Left = 3
          Top = 115
          Width = 93
          Height = 13
          Caption = 'Required neighbors'
        end
        object Edit10: TEdit
          Left = 117
          Top = 21
          Width = 70
          Height = 21
          TabOrder = 0
        end
        object Edit11: TEdit
          Left = 117
          Top = 48
          Width = 70
          Height = 21
          TabOrder = 1
        end
        object Edit12: TEdit
          Left = 117
          Top = 85
          Width = 70
          Height = 21
          TabOrder = 2
        end
        object Edit13: TEdit
          Left = 117
          Top = 112
          Width = 70
          Height = 21
          TabOrder = 3
        end
        object CheckBox5: TCheckBox
          Left = 40
          Top = 160
          Width = 137
          Height = 17
          Caption = 'Plot occupied voxels'
          TabOrder = 4
        end
        object CheckBox6: TCheckBox
          Left = 40
          Top = 184
          Width = 129
          Height = 17
          Caption = 'Plot first neighbors'
          TabOrder = 5
        end
        object CheckBox7: TCheckBox
          Left = 40
          Top = 208
          Width = 137
          Height = 17
          Caption = 'Plot second neighbors'
          TabOrder = 6
        end
        object BitBtn18: TBitBtn
          Left = 83
          Top = 280
          Width = 110
          Height = 25
          Caption = 'Find power lines'
          TabOrder = 7
          OnClick = BitBtn18Click
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'Wall'
        ImageIndex = 3
        object Label14: TLabel
          Left = 0
          Top = 24
          Width = 103
          Height = 13
          Caption = 'Min ht (m) across wall'
        end
        object Label15: TLabel
          Left = 0
          Top = 56
          Width = 116
          Height = 13
          Caption = 'Max delta (m) along wall'
        end
        object Label16: TLabel
          Left = 0
          Top = 88
          Width = 54
          Height = 13
          Caption = 'Region size'
        end
        object Edit14: TEdit
          Left = 136
          Top = 21
          Width = 72
          Height = 21
          TabOrder = 0
          Text = 'Edit14'
        end
        object Edit15: TEdit
          Left = 136
          Top = 53
          Width = 72
          Height = 21
          TabOrder = 1
          Text = 'Edit15'
        end
        object Edit16: TEdit
          Left = 136
          Top = 85
          Width = 72
          Height = 21
          TabOrder = 2
          Text = 'Edit16'
        end
        object BitBtn6: TBitBtn
          Left = 48
          Top = 152
          Width = 89
          Height = 25
          Caption = 'Walls'
          TabOrder = 3
          OnClick = BitBtn6Click
        end
        object BitBtn14: TBitBtn
          Left = 48
          Top = 200
          Width = 83
          Height = 25
          Caption = 'Find walls'
          TabOrder = 4
          OnClick = BitBtn14Click
        end
      end
      object TabSheet5: TTabSheet
        Caption = 'Road'
        ImageIndex = 4
        object Label21: TLabel
          Left = 18
          Top = 11
          Width = 72
          Height = 13
          Caption = 'SSO region (m)'
        end
        object Label22: TLabel
          Left = 18
          Top = 111
          Width = 128
          Height = 13
          Caption = 'Same trend region size (m)'
        end
        object Label23: TLabel
          Left = 18
          Top = 138
          Width = 113
          Height = 13
          Caption = 'Azimuth sensitivity ('#177#176')'
        end
        object Label24: TLabel
          Left = 48
          Top = 208
          Width = 92
          Height = 13
          Caption = 'Road width (pixels)'
        end
        object Edit21: TEdit
          Left = 109
          Top = 8
          Width = 81
          Height = 21
          TabOrder = 0
          Text = 'Edit21'
          OnChange = Edit21Change
        end
        object BitBtn10: TBitBtn
          Left = 60
          Top = 80
          Width = 89
          Height = 25
          Caption = 'Create SSO'
          TabOrder = 1
          OnClick = BitBtn10Click
        end
        object CheckBox1: TCheckBox
          Left = 16
          Top = 35
          Width = 129
          Height = 17
          Caption = 'Limit to SSO strength >'
          TabOrder = 2
          OnClick = CheckBox1Click
        end
        object Edit22: TEdit
          Left = 151
          Top = 35
          Width = 73
          Height = 21
          TabOrder = 3
          Text = 'Edit22'
          OnChange = Edit22Change
        end
        object Edit23: TEdit
          Left = 152
          Top = 108
          Width = 72
          Height = 21
          TabOrder = 4
          Text = 'Edit23'
          OnChange = Edit23Change
        end
        object Edit24: TEdit
          Left = 152
          Top = 135
          Width = 72
          Height = 21
          TabOrder = 5
          Text = 'Edit24'
          OnChange = Edit24Change
        end
        object BitBtn11: TBitBtn
          Left = 60
          Top = 162
          Width = 141
          Height = 25
          Caption = 'Create likely roads (box)'
          TabOrder = 6
          OnClick = BitBtn11Click
        end
        object CheckBox8: TCheckBox
          Left = 16
          Top = 57
          Width = 129
          Height = 17
          Caption = 'Limit to SSO flatness <'
          TabOrder = 7
          OnClick = CheckBox8Click
        end
        object Edit25: TEdit
          Left = 151
          Top = 62
          Width = 73
          Height = 21
          TabOrder = 8
          Text = 'Edit25'
          OnChange = Edit25Change
        end
        object BitBtn12: TBitBtn
          Left = 56
          Top = 240
          Width = 168
          Height = 25
          Caption = 'Create likely roads (shaped)'
          TabOrder = 9
          OnClick = BitBtn12Click
        end
        object Edit26: TEdit
          Left = 152
          Top = 208
          Width = 72
          Height = 21
          TabOrder = 10
          Text = '5'
        end
      end
      object TabSheet6: TTabSheet
        Caption = 'Corner'
        ImageIndex = 5
        object BitBtn15: TBitBtn
          Left = 136
          Top = 72
          Width = 75
          Height = 25
          Caption = 'Corner'
          TabOrder = 0
          OnClick = BitBtn15Click
        end
      end
    end
    object CheckBox4: TCheckBox
      Left = 16
      Top = 384
      Width = 104
      Height = 17
      Caption = 'Replace open DB'
      TabOrder = 4
      OnClick = CheckBox4Click
    end
    object RadioGroup3: TRadioGroup
      Left = 212
      Top = 384
      Width = 66
      Height = 92
      Caption = 'Create'
      ItemIndex = 0
      Items.Strings = (
        'Grid'
        'DB'
        'Both')
      TabOrder = 5
    end
    object BitBtn16: TBitBtn
      Left = 102
      Top = 473
      Width = 87
      Height = 25
      Caption = 'Mask options'
      TabOrder = 6
      OnClick = BitBtn16Click
    end
    object BitBtn17: TBitBtn
      Left = 21
      Top = 473
      Width = 75
      Height = 25
      Caption = 'Clear  memo'
      TabOrder = 7
      OnClick = BitBtn17Click
    end
    object CheckBox2: TCheckBox
      Left = 126
      Top = 384
      Width = 72
      Height = 17
      Caption = 'All DEMs'
      TabOrder = 8
    end
  end
end
