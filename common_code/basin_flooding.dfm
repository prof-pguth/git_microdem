object FloodingForm: TFloodingForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Basin Flooding'
  ClientHeight = 445
  ClientWidth = 479
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StringGrid1: TStringGrid
    Left = 225
    Top = 0
    Width = 254
    Height = 426
    Align = alClient
    FixedCols = 0
    ScrollBars = ssVertical
    TabOrder = 0
    ColWidths = (
      64
      64
      64
      64
      64)
    RowHeights = (
      24
      24
      24
      24
      24)
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 426
    Width = 479
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 225
    Height = 426
    Align = alLeft
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 84
      Height = 13
      Caption = 'Reservoir top (m)'
    end
    object Label4: TLabel
      Left = 11
      Top = 54
      Width = 47
      Height = 13
      Caption = 'MLLW (ft)'
    end
    object Label5: TLabel
      Left = 2
      Top = 100
      Width = 96
      Height = 13
      Caption = 'NAVD88 - MLLW (m)'
    end
    object BitBtn1: TBitBtn
      Left = 122
      Top = 159
      Width = 75
      Height = 25
      Caption = 'Movie'
      TabOrder = 0
      OnClick = BitBtn1Click
    end
    object BitBtn2: TBitBtn
      Left = 108
      Top = 225
      Width = 75
      Height = 25
      Caption = 'HTML'
      TabOrder = 1
      OnClick = BitBtn2Click
    end
    object BitBtn3: TBitBtn
      Left = 12
      Top = 225
      Width = 75
      Height = 25
      Caption = 'Start points'
      TabOrder = 2
      OnClick = BitBtn3Click
    end
    object CancelBtn: TBitBtn
      Left = 105
      Top = 376
      Width = 77
      Height = 27
      Caption = 'Close'
      Kind = bkCancel
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 3
      OnClick = CancelBtnClick
      IsControl = True
    end
    object CheckBox1: TCheckBox
      Left = 18
      Top = 202
      Width = 81
      Height = 17
      Caption = 'Label maps'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object Edit1: TEdit
      Left = 8
      Top = 27
      Width = 81
      Height = 21
      TabOrder = 5
      Text = 'Edit1'
    end
    object FloodButton: TBitBtn
      Left = 11
      Top = 159
      Width = 75
      Height = 25
      Caption = 'Flood 1 level'
      TabOrder = 6
      OnClick = FloodButtonClick
    end
    object HelpBtn: TBitBtn
      Left = 10
      Top = 376
      Width = 77
      Height = 27
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 7
      OnClick = HelpBtnClick
      IsControl = True
    end
    object RadioGroup1: TRadioGroup
      Left = 4
      Top = 295
      Width = 215
      Height = 75
      Caption = 'Algorithm'
      Columns = 2
      ItemIndex = 1
      Items.Strings = (
        'Elevations'
        'Flooding'
        'Complex'
        'Color/White'
        'Color overlay')
      TabOrder = 8
      OnClick = RadioGroup1Click
    end
    object CheckBox2: TCheckBox
      Left = 105
      Top = 202
      Width = 97
      Height = 17
      Caption = 'KML export'
      TabOrder = 9
    end
    object Edit5: TEdit
      Left = 14
      Top = 73
      Width = 73
      Height = 21
      TabOrder = 10
      OnChange = Edit5Change
    end
    object Edit6: TEdit
      Left = 14
      Top = 119
      Width = 75
      Height = 21
      TabOrder = 11
      Text = '0.235'
      OnChange = Edit6Change
    end
    object GroupBox1: TGroupBox
      Left = 104
      Top = 8
      Width = 115
      Height = 145
      Caption = 'Flooding movie'
      TabOrder = 12
      object Label3: TLabel
        Left = 3
        Top = 92
        Width = 44
        Height = 13
        Caption = 'Interval  '
      end
      object Label2: TLabel
        Left = 3
        Top = 54
        Width = 50
        Height = 13
        Caption = 'Low level  '
      end
      object Label6: TLabel
        Left = 14
        Top = 19
        Width = 46
        Height = 13
        Caption = 'Top  level'
      end
      object Edit3: TEdit
        Left = 19
        Top = 111
        Width = 81
        Height = 21
        TabOrder = 0
        Text = '1'
      end
      object Edit2: TEdit
        Left = 19
        Top = 73
        Width = 81
        Height = 21
        TabOrder = 1
        Text = 'Edit2'
      end
      object Edit4: TEdit
        Left = 19
        Top = 34
        Width = 81
        Height = 21
        TabOrder = 2
        Text = 'Edit2'
      end
    end
    object RadioGroup2: TRadioGroup
      Left = 8
      Top = 256
      Width = 189
      Height = 33
      Caption = 'Movie'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'NAVD88 (m)'
        'MLLW (ft)')
      TabOrder = 13
    end
    object BitBtn4: TBitBtn
      Left = 189
      Top = 194
      Width = 30
      Height = 25
      TabOrder = 14
      OnClick = BitBtn4Click
    end
  end
end
