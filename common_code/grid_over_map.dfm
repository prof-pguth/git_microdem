object GridOverlayonMap: TGridOverlayonMap
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Geomorph atlas--classify'
  ClientHeight = 464
  ClientWidth = 518
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
  object Label1: TLabel
    Left = 8
    Top = 51
    Width = 20
    Height = 13
    Caption = 'Max'
  end
  object Label2: TLabel
    Left = 8
    Top = 78
    Width = 16
    Height = 13
    Caption = 'Min'
  end
  object Label3: TLabel
    Left = 113
    Top = 78
    Width = 16
    Height = 13
    Caption = 'Min'
  end
  object Label4: TLabel
    Left = 109
    Top = 51
    Width = 20
    Height = 13
    Caption = 'Max'
  end
  object Label5: TLabel
    Left = 197
    Top = 51
    Width = 20
    Height = 13
    Caption = 'Max'
  end
  object Label6: TLabel
    Left = 201
    Top = 78
    Width = 16
    Height = 13
    Caption = 'Min'
  end
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
  object Label14: TLabel
    Left = 99
    Top = 280
    Width = 37
    Height = 13
    Caption = 'Label14'
  end
  object Label18: TLabel
    Left = 298
    Top = 54
    Width = 20
    Height = 13
    Caption = 'Max'
  end
  object Label19: TLabel
    Left = 303
    Top = 104
    Width = 3
    Height = 13
  end
  object Label20: TLabel
    Left = 298
    Top = 78
    Width = 16
    Height = 13
    Caption = 'Min'
  end
  object Label23: TLabel
    Left = 394
    Top = 54
    Width = 20
    Height = 13
    Caption = 'Max'
  end
  object Label24: TLabel
    Left = 394
    Top = 78
    Width = 16
    Height = 13
    Caption = 'Min'
  end
  object Label25: TLabel
    Left = 400
    Top = 104
    Width = 3
    Height = 13
  end
  object Label10: TLabel
    Left = 8
    Top = 152
    Width = 26
    Height = 13
    Caption = 'Slope'
  end
  object Label30: TLabel
    Left = 8
    Top = 416
    Width = 50
    Height = 13
    Caption = 'Graph size'
  end
  object CancelBtn: TBitBtn
    Left = 372
    Top = 335
    Width = 67
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
    Left = 303
    Top = 335
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
  object ComboBox1: TComboBox
    Left = 8
    Top = 24
    Width = 80
    Height = 21
    TabOrder = 2
    OnChange = ComboBox1Change
  end
  object Edit1: TEdit
    Left = 34
    Top = 51
    Width = 59
    Height = 21
    TabOrder = 3
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 34
    Top = 78
    Width = 59
    Height = 21
    TabOrder = 4
    OnChange = Edit2Change
  end
  object Edit3: TEdit
    Left = 135
    Top = 78
    Width = 54
    Height = 21
    TabOrder = 5
    OnChange = Edit3Change
  end
  object Edit4: TEdit
    Left = 135
    Top = 51
    Width = 54
    Height = 21
    TabOrder = 6
    OnChange = Edit4Change
  end
  object ComboBox2: TComboBox
    Left = 109
    Top = 24
    Width = 80
    Height = 21
    TabOrder = 7
    OnChange = ComboBox2Change
  end
  object ComboBox3: TComboBox
    Left = 197
    Top = 24
    Width = 80
    Height = 21
    TabOrder = 8
    OnChange = ComboBox3Change
  end
  object Edit5: TEdit
    Left = 223
    Top = 51
    Width = 54
    Height = 21
    TabOrder = 9
    OnChange = Edit5Change
  end
  object Edit6: TEdit
    Left = 223
    Top = 78
    Width = 54
    Height = 21
    TabOrder = 10
    OnChange = Edit6Change
  end
  object Memo1: TMemo
    Left = 319
    Top = 123
    Width = 183
    Height = 107
    ScrollBars = ssVertical
    TabOrder = 11
  end
  object BitBtn3: TBitBtn
    Left = 8
    Top = 268
    Width = 85
    Height = 25
    Caption = 'New Base map'
    TabOrder = 12
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 8
    Top = 335
    Width = 53
    Height = 25
    Caption = 'Classify'
    TabOrder = 13
    OnClick = BitBtn4Click
  end
  object ComboBox4: TComboBox
    Left = 298
    Top = 24
    Width = 80
    Height = 21
    TabOrder = 14
    OnChange = ComboBox4Change
  end
  object Edit10: TEdit
    Left = 324
    Top = 51
    Width = 54
    Height = 21
    TabOrder = 15
    OnChange = Edit10Change
  end
  object Edit11: TEdit
    Left = 324
    Top = 77
    Width = 54
    Height = 21
    TabOrder = 16
    OnChange = Edit11Change
  end
  object HistBitBtn: TBitBtn
    Left = 67
    Top = 335
    Width = 59
    Height = 25
    Caption = 'Histogram'
    TabOrder = 17
    OnClick = HistBitBtnClick
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 1
    Width = 97
    Height = 17
    Caption = 'Use 1'
    TabOrder = 18
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 111
    Top = 1
    Width = 97
    Height = 17
    Caption = 'Use 2'
    TabOrder = 19
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 197
    Top = 1
    Width = 97
    Height = 17
    Caption = 'Use 3'
    TabOrder = 20
    OnClick = CheckBox3Click
  end
  object CheckBox4: TCheckBox
    Left = 300
    Top = 1
    Width = 97
    Height = 17
    Caption = 'Use 4'
    TabOrder = 21
    OnClick = CheckBox4Click
  end
  object BitBtn6: TBitBtn
    Left = 132
    Top = 335
    Width = 75
    Height = 25
    Caption = 'Scatter plots'
    TabOrder = 22
    OnClick = BitBtn6Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 442
    Width = 518
    Height = 22
    Panels = <
      item
        Width = 50
      end>
    ExplicitTop = 406
    ExplicitWidth = 480
  end
  object ComboBox5: TComboBox
    Left = 394
    Top = 24
    Width = 80
    Height = 21
    TabOrder = 24
    OnChange = ComboBox5Change
  end
  object Edit7: TEdit
    Left = 420
    Top = 51
    Width = 54
    Height = 21
    TabOrder = 25
    OnChange = Edit7Change
  end
  object Edit12: TEdit
    Left = 420
    Top = 77
    Width = 54
    Height = 21
    TabOrder = 26
    OnChange = Edit12Change
  end
  object CheckBox5: TCheckBox
    Left = 394
    Top = 1
    Width = 97
    Height = 17
    Caption = 'Use 5'
    TabOrder = 27
    OnClick = CheckBox5Click
  end
  object GroupBox1: TGroupBox
    Left = 319
    Top = 236
    Width = 191
    Height = 93
    Caption = 'Region filter'
    TabOrder = 28
    object Label15: TLabel
      Left = 8
      Top = 36
      Width = 45
      Height = 13
      Caption = 'Filter size'
    end
    object Label22: TLabel
      Left = 97
      Top = 38
      Width = 37
      Height = 13
      Caption = 'Label22'
    end
    object Label16: TLabel
      Left = 8
      Top = 65
      Width = 83
      Height = 13
      Caption = 'Matches required'
    end
    object Label13: TLabel
      Left = 138
      Top = 65
      Width = 37
      Height = 13
      Caption = 'Label13'
    end
    object CheckBox7: TCheckBox
      Left = 12
      Top = 15
      Width = 97
      Height = 17
      Caption = 'Region filter'
      TabOrder = 0
      OnClick = CheckBox7Click
    end
    object Edit8: TEdit
      Left = 59
      Top = 38
      Width = 32
      Height = 21
      TabOrder = 1
      Text = '5'
      OnChange = Edit8Change
    end
    object Edit9: TEdit
      Left = 97
      Top = 65
      Width = 35
      Height = 21
      TabOrder = 2
      Text = '50'
      OnChange = Edit9Change
    end
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 124
    Width = 289
    Height = 135
    ActivePage = TabSheet3
    TabOrder = 29
    object TabSheet1: TTabSheet
      Caption = 'Mask overlay'
      object RadioGroup1: TRadioGroup
        Left = 0
        Top = 7
        Width = 305
        Height = 70
        Caption = 'Mask Overlay'
        TabOrder = 0
        OnClick = RadioGroup1Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Color overlay'
      ImageIndex = 1
      object GroupBox2: TGroupBox
        Left = 0
        Top = 14
        Width = 225
        Height = 42
        Caption = 'Color overlay'
        TabOrder = 0
        object ComboBox6: TComboBox
          Left = 3
          Top = 14
          Width = 98
          Height = 21
          TabOrder = 0
          OnChange = ComboBox6Change
        end
        object BitBtn1: TBitBtn
          Left = 107
          Top = 10
          Width = 36
          Height = 25
          Caption = 'Color'
          TabOrder = 1
          OnClick = BitBtn1Click
        end
        object CheckBox8: TCheckBox
          Left = 149
          Top = 15
          Width = 68
          Height = 17
          Caption = 'Use mask'
          TabOrder = 2
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'I && P class'
      ImageIndex = 2
      object Label11: TLabel
        Left = 22
        Top = 3
        Width = 26
        Height = 13
        Caption = 'Slope'
      end
      object Label12: TLabel
        Left = 11
        Top = 35
        Width = 49
        Height = 13
        Caption = 'Convexity'
      end
      object Label17: TLabel
        Left = 4
        Top = 54
        Width = 53
        Height = 13
        Caption = 'Roughness'
      end
      object Label21: TLabel
        Left = 179
        Top = 8
        Width = 35
        Height = 13
        Caption = 'Slope 1'
      end
      object Label26: TLabel
        Left = 179
        Top = 27
        Width = 35
        Height = 13
        Caption = 'Slope 2'
      end
      object Label27: TLabel
        Left = 166
        Top = 67
        Width = 49
        Height = 13
        Caption = 'Convexity'
      end
      object Label28: TLabel
        Left = 161
        Top = 86
        Width = 53
        Height = 13
        Caption = 'Roughness'
      end
      object Label29: TLabel
        Left = 176
        Top = 48
        Width = 35
        Height = 13
        Caption = 'Slope 3'
      end
      object ComboBox7: TComboBox
        Left = 66
        Top = 3
        Width = 97
        Height = 21
        TabOrder = 0
      end
      object ComboBox8: TComboBox
        Left = 66
        Top = 30
        Width = 97
        Height = 21
        TabOrder = 1
      end
      object ComboBox9: TComboBox
        Left = 66
        Top = 54
        Width = 97
        Height = 21
        TabOrder = 2
      end
      object BitBtn2: TBitBtn
        Left = 3
        Top = 79
        Width = 54
        Height = 25
        Caption = 'Draw'
        TabOrder = 3
        OnClick = BitBtn2Click
      end
      object CheckBox9: TCheckBox
        Left = 66
        Top = 86
        Width = 86
        Height = 17
        Caption = 'IHS merge'
        Checked = True
        State = cbChecked
        TabOrder = 4
      end
      object Edit13: TEdit
        Left = 220
        Top = 3
        Width = 46
        Height = 21
        TabOrder = 5
        Text = 'Edit13'
      end
      object Edit14: TEdit
        Left = 220
        Top = 22
        Width = 46
        Height = 21
        TabOrder = 6
        Text = 'Edit13'
      end
      object Edit15: TEdit
        Left = 220
        Top = 43
        Width = 46
        Height = 21
        TabOrder = 7
        Text = 'Edit13'
      end
      object Edit16: TEdit
        Left = 220
        Top = 67
        Width = 46
        Height = 21
        TabOrder = 8
        Text = 'Edit13'
      end
      object Edit17: TEdit
        Left = 220
        Top = 86
        Width = 46
        Height = 21
        TabOrder = 9
        Text = 'Edit13'
      end
    end
  end
  object BitBtn5: TBitBtn
    Left = 8
    Top = 366
    Width = 64
    Height = 25
    Caption = 'DBF export'
    TabOrder = 30
    OnClick = BitBtn5Click
  end
  object BitBtn7: TBitBtn
    Left = 418
    Top = 366
    Width = 75
    Height = 25
    Caption = 'Grid inventory'
    TabOrder = 31
    OnClick = BitBtn7Click
  end
  object BitBtn8: TBitBtn
    Left = 78
    Top = 366
    Width = 75
    Height = 25
    Caption = 'Cluster'
    TabOrder = 32
    OnClick = BitBtn8Click
  end
  object BitBtn9: TBitBtn
    Left = 256
    Top = 366
    Width = 75
    Height = 25
    Caption = 'Make mask'
    TabOrder = 33
    OnClick = BitBtn9Click
  end
  object Edit18: TEdit
    Left = 80
    Top = 408
    Width = 57
    Height = 21
    TabOrder = 34
    Text = 'Edit18'
    OnChange = Edit18Change
  end
  object CheckBox10: TCheckBox
    Left = 192
    Top = 416
    Width = 114
    Height = 17
    Caption = 'Label each graph'
    TabOrder = 35
    OnClick = CheckBox10Click
  end
  object BitBtn11: TBitBtn
    Left = 337
    Top = 366
    Width = 75
    Height = 25
    Caption = 'Norm grids'
    TabOrder = 36
    OnClick = BitBtn11Click
  end
  object RadioGroup2: TRadioGroup
    Left = 16
    Top = 299
    Width = 272
    Height = 30
    Caption = 'Symbol size'
    Columns = 6
    ItemIndex = 0
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6')
    TabOrder = 37
    OnClick = RadioGroup2Click
  end
end
