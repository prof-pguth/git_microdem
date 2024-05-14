object fuv_ssim_control: Tfuv_ssim_control
  Left = 0
  Top = 0
  Caption = 'fuv_ssim_control_form'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnCreate = FormCreate
  TextHeight = 15
  object GroupBox9: TGroupBox
    Left = 304
    Top = 8
    Width = 241
    Height = 386
    Caption = 'SSIM/FUV parameters'
    TabOrder = 0
    object CheckBox12: TCheckBox
      Left = 16
      Top = 23
      Width = 97
      Height = 17
      Caption = 'Elevation'
      TabOrder = 0
    end
    object CheckBox13: TCheckBox
      Left = 16
      Top = 46
      Width = 97
      Height = 17
      Caption = 'Slope'
      TabOrder = 1
    end
    object CheckBox14: TCheckBox
      Left = 16
      Top = 69
      Width = 97
      Height = 17
      Caption = 'Roughness'
      TabOrder = 2
    end
    object CheckBox15: TCheckBox
      Left = 16
      Top = 92
      Width = 97
      Height = 17
      Caption = 'RRI'
      TabOrder = 3
    end
    object Hillshade: TCheckBox
      Left = 16
      Top = 115
      Width = 97
      Height = 17
      Caption = 'Hillshade'
      TabOrder = 4
    end
    object CheckBox17: TCheckBox
      Left = 16
      Top = 138
      Width = 166
      Height = 17
      Caption = 'TPI (detrended elevation)'
      TabOrder = 5
    end
    object CheckBox19: TCheckBox
      Left = 16
      Top = 161
      Width = 129
      Height = 17
      Caption = 'Flow accumulation'
      TabOrder = 6
    end
    object CheckBox20: TCheckBox
      Left = 16
      Top = 184
      Width = 97
      Height = 17
      Caption = 'Wetness index'
      TabOrder = 7
    end
    object CheckBox21: TCheckBox
      Left = 16
      Top = 207
      Width = 97
      Height = 17
      Caption = 'LS factor'
      TabOrder = 8
    end
    object HAND: TCheckBox
      Left = 16
      Top = 230
      Width = 97
      Height = 17
      Caption = 'HAND'
      TabOrder = 9
    end
    object CheckBox26: TCheckBox
      Left = 16
      Top = 253
      Width = 129
      Height = 17
      Caption = 'Profile curvature'
      TabOrder = 10
    end
    object CheckBox27: TCheckBox
      Left = 16
      Top = 272
      Width = 97
      Height = 22
      Caption = 'Plan curvature'
      TabOrder = 11
    end
    object CheckBox28: TCheckBox
      Left = 16
      Top = 293
      Width = 137
      Height = 17
      Caption = 'Tangential curvature'
      TabOrder = 12
    end
    object CheckBox7: TCheckBox
      Left = 16
      Top = 312
      Width = 97
      Height = 17
      Caption = 'Rotor'
      TabOrder = 13
    end
    object CheckBox8: TCheckBox
      Left = 16
      Top = 335
      Width = 209
      Height = 17
      Caption = 'Openness (upward and downward)'
      TabOrder = 14
    end
    object CheckBox10: TCheckBox
      Left = 16
      Top = 358
      Width = 169
      Height = 17
      Caption = 'Convergence index'
      TabOrder = 15
    end
  end
  object GroupBox1: TGroupBox
    Left = 24
    Top = 8
    Width = 185
    Height = 124
    Caption = 'Elevation ranges'
    TabOrder = 1
    object CheckBox1: TCheckBox
      Left = 8
      Top = 24
      Width = 97
      Height = 17
      Caption = 'All'
      TabOrder = 0
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 47
      Width = 174
      Height = 17
      Caption = 'U120--add Coaatal'
      TabOrder = 1
    end
    object CheckBox3: TCheckBox
      Left = 8
      Top = 73
      Width = 134
      Height = 17
      Caption = 'U80--add diluvium'
      TabOrder = 2
    end
    object CheckBox4: TCheckBox
      Left = 8
      Top = 96
      Width = 129
      Height = 17
      Caption = 'U10--add delta'
      TabOrder = 3
    end
  end
  object CheckBox5: TCheckBox
    Left = 40
    Top = 192
    Width = 97
    Height = 17
    Caption = 'Overwrite'
    TabOrder = 2
  end
  object CheckBox6: TCheckBox
    Left = 40
    Top = 224
    Width = 97
    Height = 17
    Caption = 'All areas'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object CheckBox25: TCheckBox
    Left = 40
    Top = 293
    Width = 185
    Height = 17
    Caption = 'Open maps during SSIM/FUV'
    TabOrder = 4
  end
  object CheckBox24: TCheckBox
    Left = 40
    Top = 270
    Width = 137
    Height = 17
    Caption = 'Do FUV criteria'
    TabOrder = 5
  end
  object CheckBox22: TCheckBox
    Left = 40
    Top = 247
    Width = 137
    Height = 17
    Caption = 'Do SSIM criteria'
    TabOrder = 6
  end
  object BitBtn1: TBitBtn
    Left = 24
    Top = 400
    Width = 105
    Height = 25
    Caption = 'Process'
    TabOrder = 7
    OnClick = BitBtn1Click
  end
  object BitBtn38: TBitBtn
    Left = 469
    Top = 400
    Width = 113
    Height = 25
    Caption = 'Save defaults'
    TabOrder = 8
    OnClick = BitBtn38Click
  end
  object CheckBox9: TCheckBox
    Left = 40
    Top = 328
    Width = 185
    Height = 17
    Caption = 'Process loops foward '
    TabOrder = 9
  end
  object CheckBox157: TCheckBox
    Left = 40
    Top = 351
    Width = 199
    Height = 17
    Caption = 'Show WinExec window'
    TabOrder = 10
  end
  object BitBtn2: TBitBtn
    Left = 150
    Top = 400
    Width = 107
    Height = 25
    Caption = 'Merge Areas'
    TabOrder = 11
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 280
    Top = 400
    Width = 105
    Height = 25
    Caption = 'Make MD grids'
    Enabled = False
    TabOrder = 12
    OnClick = BitBtn3Click
  end
end
