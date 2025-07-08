object fuv_ssim_control: Tfuv_ssim_control
  Left = 0
  Top = 0
  Caption = 'fuv_ssim_control_form'
  ClientHeight = 266
  ClientWidth = 618
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 243
    Top = 8
    Width = 103
    Height = 15
    Caption = 'Required tile fill (%)'
  end
  object GroupBox1: TGroupBox
    Left = 243
    Top = 40
    Width = 185
    Height = 124
    Caption = 'Elevation ranges'
    TabOrder = 0
    object CheckBox1: TCheckBox
      Left = 8
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Full'
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
    Left = 8
    Top = 40
    Width = 97
    Height = 17
    Caption = 'Overwrite'
    TabOrder = 1
  end
  object CheckBox6: TCheckBox
    Left = 8
    Top = 72
    Width = 97
    Height = 17
    Caption = 'All areas'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object CheckBox25: TCheckBox
    Left = 8
    Top = 141
    Width = 185
    Height = 17
    Caption = 'Open maps during SSIM/FUV'
    TabOrder = 3
  end
  object CheckBox24: TCheckBox
    Left = 8
    Top = 118
    Width = 137
    Height = 17
    Caption = 'Do FUV criteria'
    TabOrder = 4
  end
  object CheckBox22: TCheckBox
    Left = 8
    Top = 95
    Width = 137
    Height = 17
    Caption = 'Do SSIM criteria'
    TabOrder = 5
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 233
    Width = 105
    Height = 25
    Caption = 'Process'
    TabOrder = 6
    OnClick = BitBtn1Click
  end
  object BitBtn38: TBitBtn
    Left = 315
    Top = 229
    Width = 113
    Height = 25
    Caption = 'Save defaults'
    TabOrder = 7
    OnClick = BitBtn38Click
  end
  object CheckBox9: TCheckBox
    Left = 8
    Top = 176
    Width = 185
    Height = 17
    Caption = 'Process loops foward '
    TabOrder = 8
  end
  object CheckBox157: TCheckBox
    Left = 8
    Top = 199
    Width = 199
    Height = 17
    Caption = 'Show WinExec window'
    TabOrder = 9
  end
  object BitBtn2: TBitBtn
    Left = 119
    Top = 233
    Width = 107
    Height = 25
    Caption = 'Merge Areas'
    TabOrder = 10
    OnClick = BitBtn2Click
  end
  object BitBtn5: TBitBtn
    Left = 231
    Top = 229
    Width = 75
    Height = 25
    Caption = 'Spawn'
    TabOrder = 11
    OnClick = BitBtn5Click
  end
  object CheckBox11: TCheckBox
    Left = 8
    Top = 8
    Width = 185
    Height = 17
    Caption = 'Area instead of DEMIX tiles'
    TabOrder = 12
  end
  object BitBtn3: TBitBtn
    Left = 251
    Top = 176
    Width = 113
    Height = 25
    Caption = 'Pick criteria'
    TabOrder = 13
    OnClick = BitBtn3Click
  end
  object Edit1: TEdit
    Left = 368
    Top = 5
    Width = 57
    Height = 23
    TabOrder = 14
    Text = 'Edit1'
  end
  object CheckBox7: TCheckBox
    Left = 151
    Top = 95
    Width = 97
    Height = 17
    Caption = 'Do partials'
    TabOrder = 15
  end
  object RadioGroup1: TRadioGroup
    Left = 448
    Top = 40
    Width = 129
    Height = 124
    Caption = 'Criteria set to use'
    Items.Strings = (
      'FUV'
      'SSIM'
      'Partials'
      'Curvatures')
    TabOrder = 16
    OnClick = RadioGroup1Click
  end
end
