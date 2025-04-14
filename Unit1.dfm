object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = #1055#1086#1089#1084#1086#1090' '#1088#1077#1087#1086#1079#1080#1090#1072#1088#1080#1103
  ClientHeight = 441
  ClientWidth = 620
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Label1: TLabel
    Left = 7
    Top = 3
    Width = 141
    Height = 21
    Caption = #1048#1084#1103' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI Semibold'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelRepo: TLabel
    Left = 56
    Top = 32
    Width = 47
    Height = 21
    Caption = #1056#1045#1055#1054' '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelFiles: TLabel
    Left = 224
    Top = 35
    Width = 55
    Height = 21
    Caption = #1060#1072#1081#1083#1099
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelContet: TLabel
    Left = 467
    Top = 31
    Width = 63
    Height = 21
    Caption = #1050#1086#1085#1090#1077#1085#1090
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object EditUsername: TEdit
    Left = 175
    Top = 0
    Width = 169
    Height = 29
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI Semibold'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    Text = 'maxth7'
  end
  object ListBoxRepo: TListBox
    Left = 8
    Top = 56
    Width = 162
    Height = 377
    Align = alCustom
    ItemHeight = 15
    TabOrder = 1
    OnClick = ListBoxRepoClick
  end
  object ListBoxFiles: TListBox
    Left = 175
    Top = 56
    Width = 179
    Height = 377
    ItemHeight = 15
    TabOrder = 2
    OnClick = ListBoxFilesClick
  end
  object ButtonGetRepo: TButton
    Left = 360
    Top = 0
    Width = 241
    Height = 25
    Caption = #1055#1086#1083#1091#1095#1080#1090#1100' '#1080#1084#1077#1085#1072' '#1088#1077#1087#1086#1079#1080#1090#1072#1088#1080#1077#1074
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = ButtonGetRepoClick
  end
  object RichEditFileContent: TRichEdit
    Left = 360
    Top = 56
    Width = 252
    Height = 377
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Lines.Strings = (
      '')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 4
  end
end
