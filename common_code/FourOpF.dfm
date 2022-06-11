inherited FourierOptionsForm: TFourierOptionsForm
  Left = 519
  Top = 324
  Caption = 'Fourier Transform Options'
  ClientHeight = 295
  ClientWidth = 331
  ExplicitWidth = 337
  ExplicitHeight = 324
  PixelsPerInch = 96
  TextHeight = 13
  inherited Bevel1: TBevel
    Left = 16
    Top = 16
    Height = 217
    ExplicitLeft = 16
    ExplicitTop = 16
    ExplicitHeight = 217
  end
  object Label1: TLabel [1]
    Left = 40
    Top = 200
    Width = 19
    Height = 13
    Caption = 'Bins'
  end
  object Label2: TLabel [2]
    Left = 168
    Top = 200
    Width = 24
    Height = 13
    Caption = 'Units'
  end
  inherited OKBtn: TButton
    Left = 54
    Top = 252
    ExplicitLeft = 54
    ExplicitTop = 252
  end
  inherited CancelBtn: TButton
    Left = 142
    Top = 252
    ExplicitLeft = 142
    ExplicitTop = 252
  end
  object HelpBtn: TButton
    Left = 230
    Top = 252
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 2
    OnClick = HelpBtnClick
  end
  object GroupBox1: TGroupBox
    Left = 32
    Top = 24
    Width = 89
    Height = 73
    Caption = 'Window'
    TabOrder = 3
    object RadioButton1: TRadioButton
      Left = 8
      Top = 16
      Width = 70
      Height = 17
      Caption = 'Parzen'
      TabOrder = 0
    end
    object RadioButton2: TRadioButton
      Left = 8
      Top = 32
      Width = 70
      Height = 17
      Caption = 'Square'
      TabOrder = 1
    end
    object RadioButton3: TRadioButton
      Left = 8
      Top = 48
      Width = 70
      Height = 17
      Caption = 'Welch'
      TabOrder = 2
    end
  end
  object CheckBox4: TCheckBox
    Left = 32
    Top = 120
    Width = 137
    Height = 17
    Caption = 'Overlap segments'
    TabOrder = 4
  end
  object CheckBox5: TCheckBox
    Left = 32
    Top = 144
    Width = 145
    Height = 17
    Caption = 'Zero pad end series'
    TabOrder = 5
  end
  object GroupBox2: TGroupBox
    Left = 152
    Top = 24
    Width = 129
    Height = 105
    Caption = 'Segment Size'
    TabOrder = 6
    object RadioButton6: TRadioButton
      Left = 8
      Top = 16
      Width = 45
      Height = 17
      Caption = '16'
      TabOrder = 0
    end
    object RadioButton7: TRadioButton
      Left = 8
      Top = 32
      Width = 45
      Height = 17
      Caption = '32'
      TabOrder = 1
    end
    object RadioButton8: TRadioButton
      Left = 8
      Top = 48
      Width = 45
      Height = 17
      Caption = '64'
      TabOrder = 2
    end
    object RadioButton9: TRadioButton
      Left = 8
      Top = 64
      Width = 45
      Height = 17
      Caption = '128'
      TabOrder = 3
    end
    object RadioButton10: TRadioButton
      Left = 64
      Top = 16
      Width = 45
      Height = 17
      Caption = '256'
      TabOrder = 4
    end
    object RadioButton11: TRadioButton
      Left = 64
      Top = 32
      Width = 45
      Height = 17
      Caption = '512'
      TabOrder = 5
    end
    object RadioButton12: TRadioButton
      Left = 64
      Top = 48
      Width = 45
      Height = 17
      Caption = '1024'
      TabOrder = 6
    end
    object RadioButton13: TRadioButton
      Left = 64
      Top = 64
      Width = 45
      Height = 17
      Caption = '2048'
      TabOrder = 7
    end
    object RadioButton4: TRadioButton
      Left = 64
      Top = 80
      Width = 113
      Height = 17
      Caption = '4096'
      TabOrder = 8
    end
  end
  object GroupBox3: TGroupBox
    Left = 168
    Top = 136
    Width = 105
    Height = 57
    Caption = 'X Graph Axis'
    TabOrder = 7
    object RadioButton14: TRadioButton
      Left = 8
      Top = 16
      Width = 81
      Height = 17
      Caption = 'Period'
      TabOrder = 0
    end
    object RadioButton15: TRadioButton
      Left = 8
      Top = 32
      Width = 89
      Height = 17
      Caption = 'Frequency'
      TabOrder = 1
    end
  end
  object Edit1: TEdit
    Left = 80
    Top = 200
    Width = 65
    Height = 21
    TabOrder = 8
  end
  object Edit2: TEdit
    Left = 198
    Top = 199
    Width = 57
    Height = 21
    TabOrder = 9
  end
end
