inherited RangeCircleForm: TRangeCircleForm
  Left = 661
  Top = 131
  BorderIcons = []
  Caption = 'Select Range Circles (Meters)'
  ClientHeight = 236
  ClientWidth = 394
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  ExplicitWidth = 412
  ExplicitHeight = 283
  TextHeight = 20
  inherited Bevel1: TBevel
    Width = 185
    Height = 177
    ExplicitWidth = 185
    ExplicitHeight = 177
  end
  object Label1: TLabel [1]
    Left = 32
    Top = 16
    Width = 42
    Height = 20
    Caption = 'Range'
  end
  object Label2: TLabel [2]
    Left = 104
    Top = 16
    Width = 40
    Height = 20
    Caption = 'Name'
  end
  inherited OKBtn: TButton
    Left = 218
    Top = 16
    ExplicitLeft = 218
    ExplicitTop = 16
  end
  inherited CancelBtn: TButton
    Left = 218
    Top = 54
    ExplicitLeft = 218
    ExplicitTop = 54
  end
  object HelpBtn: TButton
    Left = 218
    Top = 92
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 2
    OnClick = HelpBtnClick
  end
  object StringGrid1: TStringGrid
    Left = 32
    Top = 42
    Width = 137
    Height = 129
    ColCount = 2
    FixedCols = 0
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    ScrollBars = ssNone
    TabOrder = 3
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 191
    Width = 377
    Height = 55
    Caption = 'Range Units'
    Columns = 5
    Items.Strings = (
      'ft'
      'yds'
      'meter'
      'mile'
      'nm')
    TabOrder = 4
    OnClick = RadioGroup1Click
  end
end
