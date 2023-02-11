object SolorPosForm1: TSolorPosForm1
  Left = 49
  Top = 62
  Caption = 'Solar Position V2.0'
  ClientHeight = 573
  ClientWidth = 810
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnActivate = FormActivate
  TextHeight = 13
  object PBox: TPaintBox
    Left = 512
    Top = 256
    Width = 265
    Height = 265
    OnPaint = PBoxPaint
  end
  object ImageLbl: TLabel
    Left = 512
    Top = 208
    Width = 265
    Height = 45
    AutoSize = False
    WordWrap = True
  end
  object Label4: TLabel
    Left = 520
    Top = 56
    Width = 233
    Height = 49
    AutoSize = False
    Caption = 
      'Analemma assumes points are measured each day for year at the ti' +
      'me specified in the panel at left.  (Should be daylight, typical' +
      'ly 12 PM.)'
    WordWrap = True
  end
  object Memo1: TMemo
    Left = 24
    Top = 312
    Width = 473
    Height = 217
    Lines.Strings = (
      
        'This is version 2.0 corrects several minor errors and adds an en' +
        'hancement or two.'
      ''
      
        '1. There are a few time zones which are not fractional hours inc' +
        'rements from GMT.  Most of '
      
        'these are an odd  number of half hour increments and are include' +
        'd here. x.5 included here.'
      ''
      
        '2. Near the poles, the Civil twilight calculations caused "Inval' +
        'id operation errors'#39' when the sun '
      
        'never got within -6 degrees below the horizon.  Error message is' +
        ' now produced.'
      ''
      
        '3. The previous solar plot was confusing as well incorrect under' +
        ' some conditions.  It has been '
      
        'replaced by an hourly geocentric projection of the sun on the ea' +
        'rth'#39's surface.  The image is '
      
        'oriented with the input latittude at right angles to the screen ' +
        '(i.e. pointing directly at the viewer).'
      ''
      
        '4. A new button sets Date, Time, Time Zone, and Daylight Savings' +
        ' offset to current system '
      'values.')
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object ShowBtn: TButton
    Left = 24
    Top = 280
    Width = 289
    Height = 25
    Caption = 'Compute Sunrise/Sunset'
    TabOrder = 0
    OnClick = ShowBtnClick
  end
  object AnalemmaBtn: TButton
    Left = 520
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Analemma'
    TabOrder = 2
    OnClick = AnalemmaBtnClick
  end
  object Panel1: TPanel
    Left = 24
    Top = 16
    Width = 465
    Height = 257
    TabOrder = 3
    object Label2: TLabel
      Left = 32
      Top = 84
      Width = 86
      Height = 13
      Caption = 'Longitude (D M S)'
    end
    object Label1: TLabel
      Left = 32
      Top = 36
      Width = 77
      Height = 13
      Caption = 'Latitude (D M S)'
    end
    object Label3: TLabel
      Left = 32
      Top = 126
      Width = 23
      Height = 13
      Caption = 'Date'
    end
    object Label8: TLabel
      Left = 152
      Top = 126
      Width = 51
      Height = 13
      Caption = 'Time Zone'
    end
    object Label9: TLabel
      Left = 32
      Top = 176
      Width = 51
      Height = 13
      Caption = 'Local  time'
    end
    object Label5: TLabel
      Left = 152
      Top = 176
      Width = 64
      Height = 13
      Caption = 'Time step (hr)'
    end
    object LongEdt: TEdit
      Left = 136
      Top = 80
      Width = 97
      Height = 21
      TabOrder = 0
      Text = '70 11 '
      OnChange = LongEdtExit
    end
    object EWRGrp: TRadioGroup
      Left = 248
      Top = 66
      Width = 81
      Height = 45
      ItemIndex = 1
      Items.Strings = (
        'East'
        'West')
      TabOrder = 1
      OnClick = PlotLastClick
    end
    object NSRGrp: TRadioGroup
      Left = 248
      Top = 18
      Width = 81
      Height = 45
      ItemIndex = 0
      Items.Strings = (
        'North'
        'South')
      TabOrder = 2
      OnClick = PlotLastClick
    end
    object LatEdt: TEdit
      Left = 136
      Top = 32
      Width = 97
      Height = 21
      TabOrder = 3
      Text = '32 45 22'
      OnChange = PlotLastClick
    end
    object DatePicker: TDateTimePicker
      Left = 32
      Top = 140
      Width = 97
      Height = 21
      Date = 37027.000000000000000000
      Time = 37027.000000000000000000
      ParseInput = True
      TabOrder = 4
      OnChange = PlotLastClick
      OnUserInput = DatePickerUserInput
    end
    object TimePicker: TDateTimePicker
      Left = 32
      Top = 188
      Width = 89
      Height = 21
      Date = 44965.000000000000000000
      Time = 0.500000000000000000
      Kind = dtkTime
      TabOrder = 5
      OnChange = PlotLastClick
    end
    object DLSRGrp: TRadioGroup
      Left = 264
      Top = 176
      Width = 185
      Height = 65
      Caption = 'Daylight Saving'
      ItemIndex = 0
      Items.Strings = (
        'Added 0 hours to local'
        'Added 1 hour to local'
        'Added 2 hours to local')
      TabOrder = 6
      OnClick = ShowBtnClick
    end
    object TzBox: TComboBox
      Left = 152
      Top = 140
      Width = 297
      Height = 21
      TabOrder = 7
      Text = 
        'GMT+01:00 Berlin, Stockholm, Rome, Bern, Brussels, Vienna, Paris' +
        ', Madrid, Amsterdam, Prague, Warsaw, Budapest'
      OnChange = ShowBtnClick
      Items.Strings = (
        'GMT-12:00 Eniwetok, Kwajalein'
        'GMT-11:00 Midway Island, Samoa'
        'GMT-10:00 Hawaii'
        'GMT-09:00 Alaska'
        'GMT-08:00 Pacific Time (US & Canada); Tijuana'
        'GMT-07:00 Mountain Time (US & Canada), Arizona'
        
          'GMT-06:00 Central Time (US & Canada), Mexico City, Tegucigalpa, ' +
          'Saskatchewan'
        
          'GMT-05:00 Eastern Time (US & Canada), Indiana(East), Bogota, Lim' +
          'a '
        'GMT-04:00 Atlantic Time (Canada), Caracas, La Paz'
        'GMT-03:30 Newfoundland'
        'GMT-03:00 Brasilia, Buenos Aires, GeorgeTown'
        'GMT-02:00 Mid-Atlantic'
        'GMT-01:00 Azores, Cape Verdes Is.'
        
          'GMT-00:00 GMT, Dublin, Edinburgh, London, Lisbon, Monrovia, Casa' +
          'blanca'
        
          'GMT+01:00 Berlin, Stockholm, Rome, Bern, Brussels, Vienna, Paris' +
          ', Madrid, Amsterdam, Prague, Warsaw, Budapest'
        
          'GMT+02:00 Athens, Helsinki, Istanbul, Cairo, Eastern Europe, Har' +
          'are, Pretoria, Israel'
        
          'GMT+03:00 Baghdad, Kuwait, Nairobi, Riyadh, Moscow, St. Petersbu' +
          'rg, Kazan, Volgograd'
        'GMT+03:30 Tehran'
        'GMT+04:00 Abu Dhabi, Muscat, Tbilisi'
        'GMT+04:30 Kabul'
        'GMT+05:00 Islamabad, Karachi, Ekaterinburg, Tashkent'
        'GMT+05:30 Bombay, Calcutta, Madras, New Delhi, Colombo'
        'GMT+06:00 Almaty, Dahka'
        'GMT+07:00 Bangkok, Jakarta, Hanoi'
        
          'GMT+08:00 Beijing, Chongquing, Urumqi, Hong Kong, Perth, Singapo' +
          're, Taipei'
        'GMT+09:00 Tokyo, Osaka, Sapporo, Seoul, Yakutsk'
        'GMT+09:30 Adelaide, Darwin'
        
          'GMT+10:00 Brisbane, Melbourne, Sydney, Guam, Port Moresby, Vladi' +
          'vostok, Hobart'
        'GMT+11:00 Magadan, Solomon Is., New Caledonia'
        'GMT+12:00 Fiji, Kamchatka, Marshall Is., Wellington, Auckland')
    end
    object GetSystimeBtn: TButton
      Left = 32
      Top = 216
      Width = 217
      Height = 25
      Caption = 'Set date/time to now'
      TabOrder = 8
      OnClick = GetSystimeBtnClick
    end
    object Edit1: TEdit
      Left = 152
      Top = 189
      Width = 66
      Height = 21
      TabOrder = 9
      Text = '0.25'
    end
  end
  object AnTypegrp: TRadioGroup
    Left = 520
    Top = 104
    Width = 233
    Height = 81
    Caption = 'Analemma type'
    ItemIndex = 0
    Items.Strings = (
      'Shadow'
      'Camera pointing south'
      'Camera point at sun on input date/time')
    TabOrder = 4
  end
  object StaticText1: TStaticText
    Left = 0
    Top = 556
    Width = 810
    Height = 17
    Cursor = crHandPoint
    Align = alBottom
    Alignment = taCenter
    Caption = 'Copyright  '#169' 2003-2005, Gary Darby,  www.DelphiForFun.org'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    TabOrder = 5
    OnClick = StaticText1Click
    ExplicitWidth = 293
  end
end
