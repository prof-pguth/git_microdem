inherited PickThreeBandForm: TPickThreeBandForm
  Left = 368
  Top = 232
  BorderIcons = []
  Caption = 'Pick Bands for Display'
  ClientHeight = 435
  ClientWidth = 506
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  ExplicitWidth = 512
  ExplicitHeight = 464
  PixelsPerInch = 96
  TextHeight = 13
  inherited Bevel1: TBevel
    Top = 16
    Height = 281
    ExplicitTop = 16
    ExplicitHeight = 281
  end
  object Image1: TImage [1]
    Left = 320
    Top = 8
    Width = 128
    Height = 128
  end
  inherited OKBtn: TButton
    Left = 420
    Top = 152
    ExplicitLeft = 420
    ExplicitTop = 152
  end
  inherited CancelBtn: TButton
    Left = 420
    Top = 182
    Visible = False
    ExplicitLeft = 420
    ExplicitTop = 182
  end
  object HelpBtn: TButton
    Left = 420
    Top = 212
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 2
    OnClick = HelpBtnClick
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 80
    Width = 257
    Height = 169
    Caption = 'Multiband '
    TabOrder = 3
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
    object Label1: TLabel
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
    object Label2: TLabel
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
      Width = 201
      Height = 21
      TabOrder = 0
      Text = ' '
      OnChange = ComboBox1Change
    end
    object ComboBox2: TComboBox
      Left = 8
      Top = 84
      Width = 201
      Height = 21
      TabOrder = 1
      Text = ' '
      OnChange = ComboBox2Change
    end
    object ComboBox3: TComboBox
      Left = 8
      Top = 132
      Width = 201
      Height = 21
      TabOrder = 2
      OnChange = ComboBox3Change
    end
  end
  object GroupBox2: TGroupBox
    Left = 16
    Top = 24
    Width = 257
    Height = 49
    Caption = 'Single band'
    TabOrder = 4
    object ComboBox4: TComboBox
      Left = 3
      Top = 25
      Width = 201
      Height = 21
      TabOrder = 0
      Text = 'ComboBox1'
      OnChange = ComboBox4Change
    end
    object UpDown1: TUpDown
      Left = 216
      Top = 24
      Width = 17
      Height = 25
      Position = 1
      TabOrder = 1
      OnClick = UpDown1Click
    end
  end
  object RadioGroup1: TRadioGroup
    Left = 304
    Top = 144
    Width = 105
    Height = 105
    Caption = 'Display mode'
    Items.Strings = (
      'Single band'
      'Multiband')
    TabOrder = 5
    OnClick = RadioGroup1Click
  end
  object Quick: TCheckBox
    Left = 304
    Top = 256
    Width = 97
    Height = 17
    Caption = 'Quick redraw'
    TabOrder = 6
    OnClick = QuickClick
  end
  object BitBtn2: TBitBtn
    Left = 123
    Top = 255
    Width = 75
    Height = 25
    Caption = 'False color'
    TabOrder = 7
    OnClick = BitBtn2Click
  end
  object BitBtn1: TBitBtn
    Left = 19
    Top = 255
    Width = 75
    Height = 25
    Caption = 'True color'
    TabOrder = 8
    OnClick = BitBtn1Click
  end
end
