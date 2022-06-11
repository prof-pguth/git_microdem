object ThreadTimerForm: TThreadTimerForm
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'Thread progress'
  ClientHeight = 280
  ClientWidth = 295
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Gauge1: TGauge
    Left = 84
    Top = 54
    Width = 203
    Height = 20
    BackColor = clRed
    Color = clLime
    Enabled = False
    ForeColor = clLime
    ParentColor = False
    Progress = 0
  end
  object Gauge2: TGauge
    Left = 84
    Top = 80
    Width = 203
    Height = 20
    BackColor = clRed
    Color = clLime
    Enabled = False
    ForeColor = clLime
    ParentColor = False
    Progress = 0
  end
  object Gauge3: TGauge
    Left = 84
    Top = 106
    Width = 203
    Height = 20
    BackColor = clRed
    Color = clLime
    Enabled = False
    ForeColor = clLime
    ParentColor = False
    Progress = 0
  end
  object Gauge4: TGauge
    Left = 84
    Top = 132
    Width = 203
    Height = 20
    BackColor = clRed
    Color = clLime
    Enabled = False
    ForeColor = clLime
    ParentColor = False
    Progress = 0
  end
  object Gauge5: TGauge
    Left = 84
    Top = 158
    Width = 203
    Height = 20
    BackColor = clRed
    Color = clLime
    Enabled = False
    ForeColor = clLime
    ParentColor = False
    Progress = 0
  end
  object Gauge6: TGauge
    Left = 84
    Top = 184
    Width = 203
    Height = 20
    BackColor = clRed
    Color = clLime
    Enabled = False
    ForeColor = clLime
    ParentColor = False
    Progress = 0
  end
  object Gauge7: TGauge
    Left = 84
    Top = 210
    Width = 203
    Height = 20
    BackColor = clRed
    Color = clLime
    Enabled = False
    ForeColor = clLime
    ParentColor = False
    Progress = 0
  end
  object Gauge8: TGauge
    Left = 84
    Top = 236
    Width = 203
    Height = 20
    BackColor = clRed
    Color = clLime
    Enabled = False
    ForeColor = clLime
    ParentColor = False
    Progress = 0
  end
  object OverallGauge9: TGauge
    Left = 8
    Top = 8
    Width = 279
    Height = 32
    BackColor = clMaroon
    Color = clLime
    ForeColor = clBlue
    ParentColor = False
    Progress = 0
  end
  object Label1: TLabel
    Left = 8
    Top = 61
    Width = 3
    Height = 13
  end
  object Label2: TLabel
    Left = 8
    Top = 88
    Width = 3
    Height = 13
  end
  object Label3: TLabel
    Left = 16
    Top = 112
    Width = 3
    Height = 13
  end
  object Label4: TLabel
    Left = 16
    Top = 134
    Width = 3
    Height = 13
  end
  object Label5: TLabel
    Left = 16
    Top = 168
    Width = 3
    Height = 13
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 261
    Width = 295
    Height = 19
    Panels = <
      item
        Width = 75
      end
      item
        Width = 250
      end>
  end
end
