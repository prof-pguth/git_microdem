inherited FFTGraph: TFFTGraph
  ClientHeight = 469
  ClientWidth = 827
  StyleElements = [seFont, seClient, seBorder]
  ExplicitWidth = 843
  ExplicitHeight = 508
  TextHeight = 16
  inherited ScrollBox1: TScrollBox
    Width = 827
    Height = 411
    ExplicitWidth = 823
    ExplicitHeight = 390
    inherited Image1: TImage
      Width = 823
      Height = 407
      ExplicitWidth = 823
      ExplicitHeight = 407
    end
  end
  inherited Panel1: TPanel
    Top = 439
    Width = 827
    StyleElements = [seFont, seClient, seBorder]
    ExplicitTop = 418
    ExplicitWidth = 823
  end
  inherited ToolBar1: TToolBar
    Width = 827
    ExplicitWidth = 823
    object SeekPeakButton: TSpeedButton
      Left = 353
      Top = 0
      Width = 25
      Height = 22
      Hint = 'Seek peaks'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFF9FFFF
        FFFFFFFFFFF9FFFFFFFFFFFFF9F9F9FFFFFFFFFFFF999FFFFFFFFFFFFF999FFF
        FFFFFFFFFFF9FFFFFFFF000000F9F00000000000000FF0000000FFFF000F000F
        FFFFFFFFF00F000FFFFFFFFFF00000FFFFFFFFFFF00000FFFFFFFFFFF0000FFF
        FFFFFFFFFF000FFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFF}
      OnClick = SeekPeakButtonClick
    end
    object fftbutton: TSpeedButton
      Left = 378
      Top = 0
      Width = 23
      Height = 22
      Hint = 'FFT options'
      Glyph.Data = {
        66010000424D6601000000000000760000002800000014000000140000000100
        040000000000F000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FFF16FFFF16FFFF7006F0000FFF1
        6FFFF16FFF81007F0000FFF16FFFF16FFF816FFF0000FFF16FFFF16FFF816FFF
        0000FFF16FFFF16FFF816FFF0000FFF16FFFF16FFF816FFF0000FFF16FFFF16F
        FF816FFF0000FFF16FFFF16FFF816FFF0000FFF16FFFF16FFF816FFF0000F700
        004700004700007F0000F700004700004700007F0000FFF16FFFF16FFF816FFF
        0000FFF14FFFF14FFF816FFF0000FFF30006F30006816FFF0000FFFF1006FF10
        06FF8FFF0000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FFFF
        FFFFFFFFFFFFFFFF0000}
      OnClick = fftbuttonClick
    end
    object SlopeSpeedButton: TSpeedButton
      Left = 401
      Top = 0
      Width = 23
      Height = 22
      Caption = 'dx/dy'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial Narrow'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SlopeSpeedButtonClick
    end
  end
  inherited MainMenu1: TMainMenu
    inherited File1: TMenuItem
      inherited Close1: TMenuItem
        OnClick = nil
      end
    end
    object Reprocess1: TMenuItem [4]
      Caption = 'Reprocess  FFT'
      GroupIndex = 3
      object Changeparameters1: TMenuItem
        Caption = '&Change parameters'
        OnClick = Changeparameters1Click
      end
    end
    inherited Option1: TMenuItem
      GroupIndex = 3
    end
  end
end
