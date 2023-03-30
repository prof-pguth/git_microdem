object DemixFilterForm: TDemixFilterForm
  Left = 0
  Top = 0
  Caption = 'Demix Filter Pick Graphs'
  ClientHeight = 384
  ClientWidth = 525
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
    Left = 352
    Top = 128
    Width = 63
    Height = 15
    Caption = 'Graph x size'
  end
  object Label2: TLabel
    Left = 352
    Top = 157
    Width = 63
    Height = 15
    Caption = 'Graph y size'
  end
  object Label3: TLabel
    Left = 368
    Top = 48
    Width = 139
    Height = 15
    Caption = 'DEM type, Tiles, Land Type'
  end
  object BitBtn1: TBitBtn
    Left = 347
    Top = 16
    Width = 150
    Height = 25
    Caption = 'Multiple Criteria per Tile'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 97
    Height = 153
    Caption = 'DEM type'
    TabOrder = 1
    object Memo1: TMemo
      Left = 3
      Top = 24
      Width = 86
      Height = 129
      Lines.Strings = (
        'DSM'
        'DTM')
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 167
    Top = 175
    Width = 121
    Height = 191
    Caption = 'DEMIX tile'
    TabOrder = 2
    object Memo2: TMemo
      Left = 3
      Top = 21
      Width = 110
      Height = 129
      Lines.Strings = (
        'N28XW018B'
        'N59TE009G'
        'N36XW003D')
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object Load: TBitBtn
      Left = 3
      Top = 156
      Width = 75
      Height = 25
      Caption = 'Load'
      TabOrder = 1
      OnClick = LoadClick
    end
  end
  object GroupBox3: TGroupBox
    Left = 222
    Top = 8
    Width = 105
    Height = 161
    Caption = 'Land type'
    TabOrder = 3
    object Memo3: TMemo
      Left = 3
      Top = 24
      Width = 89
      Height = 129
      Lines.Strings = (
        'ALL'
        'BARREN'
        'CLIFF'
        'FLAT'
        'FOREST'
        'GENTLE'
        'STEEP'
        'URBAN')
      TabOrder = 0
    end
  end
  object GroupBox4: TGroupBox
    Left = 294
    Top = 175
    Width = 129
    Height = 191
    Caption = 'Criteria'
    TabOrder = 4
    object Memo4: TMemo
      Left = 3
      Top = 16
      Width = 116
      Height = 137
      Lines.Strings = (
        'ELVD_MEAN'
        'ELVD_MED'
        'ELVD_AVD'
        'ELVD_STD'
        'ELVD_MAE'
        'ELVD_RMSE'
        'ELVD_LE90'
        'SLPD_MEAN'
        'SLPD_MED'
        'SLPD_AVD'
        'SLPD_STD'
        'SLPD_MAE'
        'SLPD_RMSE'
        'SLPD_LE90'
        'RUFD_MEAN'
        'RUFD_MED'
        'RUFD_AVD'
        'RUFD_STD'
        'RUFD_MAE'
        'RUFD_RMSE'
        'RUFD_LE90')
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object BitBtn2: TBitBtn
      Left = 0
      Top = 158
      Width = 75
      Height = 25
      Caption = 'Load'
      TabOrder = 1
      OnClick = BitBtn2Click
    end
  end
  object GroupBox5: TGroupBox
    Left = 111
    Top = 8
    Width = 105
    Height = 153
    Caption = 'Candidate DEM'
    TabOrder = 5
    object Memo5: TMemo
      Left = 3
      Top = 24
      Width = 89
      Height = 129
      Lines.Strings = (
        'ALOS'
        'ASTER'
        'COP'
        'FABDEM'
        'NASA'
        'SRTM')
      TabOrder = 0
    end
  end
  object GroupBox6: TGroupBox
    Left = 8
    Top = 175
    Width = 153
    Height = 191
    Caption = 'Areas'
    TabOrder = 6
    object Memo6: TMemo
      Left = 3
      Top = 24
      Width = 142
      Height = 129
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object BitBtn3: TBitBtn
      Left = 11
      Top = 156
      Width = 75
      Height = 25
      Caption = 'Load'
      TabOrder = 1
      OnClick = BitBtn3Click
    end
  end
  object Edit1: TEdit
    Left = 432
    Top = 125
    Width = 65
    Height = 23
    TabOrder = 7
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 432
    Top = 154
    Width = 65
    Height = 23
    TabOrder = 8
    Text = 'Edit2'
    OnChange = Edit2Change
  end
end
