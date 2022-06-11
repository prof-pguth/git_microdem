inherited PickGrid: TPickGrid
  Left = 671
  Top = 214
  BorderIcons = [biSystemMenu]
  Caption = 'Grid Options'
  ClientHeight = 485
  ClientWidth = 412
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  ExplicitWidth = 430
  ExplicitHeight = 532
  PixelsPerInch = 96
  TextHeight = 20
  inherited Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 412
    Height = 485
    Align = alClient
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitWidth = 443
    ExplicitHeight = 513
  end
  object Label7: TLabel [1]
    Left = 32
    Top = 47
    Width = 44
    Height = 20
    Caption = 'Label7'
  end
  inherited OKBtn: TButton
    Left = 19
    Top = 449
    Width = 44
    ExplicitLeft = 19
    ExplicitTop = 449
    ExplicitWidth = 44
  end
  inherited CancelBtn: TButton
    Left = 69
    Top = 449
    Width = 61
    Visible = False
    ExplicitLeft = 69
    ExplicitTop = 449
    ExplicitWidth = 61
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 0
    Width = 357
    Height = 41
    Caption = 'Show'
    Columns = 4
    Items.Strings = (
      'UTM'
      'Lat/Long'
      'Both'
      'Neither')
    TabOrder = 2
    OnClick = RadioGroup1Click
  end
  object CheckBox4: TCheckBox
    Left = 22
    Top = 96
    Width = 170
    Height = 17
    Caption = 'Secondary datum'
    TabOrder = 3
    OnClick = CheckBox4Click
  end
  object CheckBox3: TCheckBox
    Left = 227
    Top = 73
    Width = 127
    Height = 17
    Caption = 'Native grid'
    TabOrder = 4
    OnClick = CheckBox3Click
  end
  object CheckBox1: TCheckBox
    Left = 23
    Top = 73
    Width = 126
    Height = 17
    Caption = 'Primary datum'
    TabOrder = 5
    OnClick = CheckBox4Click
  end
  object CheckBox7: TCheckBox
    Left = 18
    Top = 233
    Width = 159
    Height = 17
    Caption = 'Labels inside map'
    TabOrder = 6
    OnClick = CheckBox7Click
  end
  object BitBtn7: TBitBtn
    Left = 172
    Top = 233
    Width = 88
    Height = 17
    Caption = 'Font'
    TabOrder = 7
    OnClick = BitBtn7Click
  end
  object BitBtn8: TBitBtn
    Left = 266
    Top = 232
    Width = 120
    Height = 22
    Caption = 'Background'
    TabOrder = 8
    OnClick = BitBtn8Click
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 267
    Width = 313
    Height = 134
    ActivePage = TabSheet2
    TabOrder = 9
    object TabSheet1: TTabSheet
      Caption = 'Primary datum'
      object Label3: TLabel
        Left = 14
        Top = 64
        Width = 44
        Height = 20
        Caption = 'Label3'
      end
      object BitBtn4: TBitBtn
        Left = 111
        Top = 12
        Width = 112
        Height = 25
        Caption = 'Lat/Long '
        TabOrder = 0
        OnClick = BitBtn4Click
      end
      object BitBtn1: TBitBtn
        Left = 0
        Top = 12
        Width = 105
        Height = 25
        Caption = 'UTM Grid'
        TabOrder = 1
        OnClick = BitBtn1Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Secondary datum'
      ImageIndex = 1
      object Label4: TLabel
        Left = 20
        Top = 34
        Width = 44
        Height = 20
        Caption = 'Label4'
      end
      object Button1: TButton
        Left = 3
        Top = 3
        Width = 174
        Height = 25
        Caption = 'New secondary datum'
        TabOrder = 0
        OnClick = Button1Click
      end
      object BitBtn2: TBitBtn
        Left = 3
        Top = 60
        Width = 112
        Height = 25
        Caption = 'Secondary UTM'
        TabOrder = 1
        OnClick = BitBtn2Click
      end
      object BitBtn6: TBitBtn
        Left = 121
        Top = 60
        Width = 111
        Height = 25
        Caption = 'Secondary Geo'
        TabOrder = 2
        OnClick = BitBtn6Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Native grid'
      ImageIndex = 2
      object Label5: TLabel
        Left = 21
        Top = 41
        Width = 154
        Height = 20
        Caption = 'Native grid interval (m)'
      end
      object Native: TBitBtn
        Left = 13
        Top = 3
        Width = 104
        Height = 25
        Caption = 'Native'
        TabOrder = 0
        OnClick = NativeClick
      end
      object Edit3: TEdit
        Left = 193
        Top = 40
        Width = 57
        Height = 28
        TabOrder = 1
        Text = 'Edit3'
        OnChange = Edit3Change
      end
      object CheckBox6: TCheckBox
        Left = 13
        Top = 67
        Width = 139
        Height = 17
        Caption = 'Label native grid'
        TabOrder = 2
        OnClick = CheckBox6Click
      end
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 118
    Width = 184
    Height = 109
    Caption = 'UTM'
    TabOrder = 10
    object Label2: TLabel
      Left = 10
      Top = 39
      Width = 120
      Height = 20
      Caption = 'Max pixel size (m)'
    end
    object Label1: TLabel
      Left = 15
      Top = 12
      Width = 108
      Height = 20
      Caption = 'Grid interval (m)'
    end
    object Label6: TLabel
      Left = 24
      Top = 72
      Width = 67
      Height = 20
      Caption = 'UTM zone'
    end
    object Edit2: TEdit
      Left = 132
      Top = 38
      Width = 49
      Height = 28
      TabOrder = 0
      Text = 'Edit2'
      OnChange = Edit2Change
    end
    object Edit1: TEdit
      Left = 129
      Top = 12
      Width = 52
      Height = 28
      TabOrder = 1
      Text = 'Edit1'
      OnChange = Edit1Change
    end
    object Edit4: TEdit
      Left = 129
      Top = 72
      Width = 52
      Height = 28
      TabOrder = 2
      Text = 'Edit4'
      OnChange = Edit4Change
    end
  end
  object GroupBox2: TGroupBox
    Left = 198
    Top = 118
    Width = 167
    Height = 108
    Caption = 'Lat/long graticule'
    TabOrder = 11
    object BitBtn5: TBitBtn
      Left = 16
      Top = 73
      Width = 118
      Height = 25
      Caption = 'Graticule'
      TabOrder = 0
      OnClick = BitBtn5Click
    end
    object RadioGroup2: TRadioGroup
      Left = 3
      Top = 18
      Width = 153
      Height = 49
      Caption = 'Draw'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Lines'
        'Ticks')
      TabOrder = 1
      OnClick = RadioGroup2Click
    end
  end
  object BitBtn3: TBitBtn
    Left = 218
    Top = 449
    Width = 95
    Height = 25
    Caption = 'KML export'
    TabOrder = 12
    OnClick = BitBtn3Click
  end
  object HelpBtn: TButton
    Left = 136
    Top = 449
    Width = 61
    Height = 25
    Caption = '&Help'
    TabOrder = 13
    OnClick = HelpBtnClick
  end
  object CheckBox2: TCheckBox
    Left = 170
    Top = 415
    Width = 92
    Height = 17
    Caption = 'Label KML'
    TabOrder = 14
    OnClick = CheckBox2Click
  end
  object CheckBox5: TCheckBox
    Left = 19
    Top = 415
    Width = 106
    Height = 17
    Caption = 'Short UTM'
    TabOrder = 15
    OnClick = CheckBox5Click
  end
end
