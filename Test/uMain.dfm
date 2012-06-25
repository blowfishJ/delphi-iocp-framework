object fmTestIocp: TfmTestIocp
  Left = 0
  Top = 0
  Caption = 'Test IOCP'
  ClientHeight = 386
  ClientWidth = 747
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 48
    Top = 32
    Width = 449
    Height = 193
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object edtServerPort: TEdit
    Left = 151
    Top = 278
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '8877'
  end
  object Button1: TButton
    Left = 24
    Top = 305
    Width = 75
    Height = 25
    Caption = 'connect'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 48
    Top = 1
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 136
    Top = 1
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 278
    Top = 308
    Width = 75
    Height = 25
    Caption = 'send'
    TabOrder = 5
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 105
    Top = 305
    Width = 75
    Height = 25
    Caption = 'disconnect'
    TabOrder = 6
    OnClick = Button5Click
  end
  object edtServerIP: TEdit
    Left = 24
    Top = 278
    Width = 121
    Height = 21
    TabOrder = 7
    Text = '127.0.0.1'
  end
  object edtSendPackSize: TSpinEdit
    Left = 278
    Top = 280
    Width = 121
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 8
    Value = 1
  end
  object Button6: TButton
    Left = 384
    Top = 351
    Width = 75
    Height = 25
    Caption = 'put files'
    TabOrder = 9
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 384
    Top = 320
    Width = 75
    Height = 25
    Caption = 'get files'
    TabOrder = 10
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 432
    Top = 231
    Width = 75
    Height = 25
    Caption = 'get url'
    TabOrder = 11
  end
  object edtURL: TEdit
    Left = 16
    Top = 240
    Width = 410
    Height = 21
    TabOrder = 12
    Text = 'http://127.0.0.1/'
  end
  object Button9: TButton
    Left = 432
    Top = 262
    Width = 75
    Height = 25
    Caption = 'post url'
    TabOrder = 13
  end
  object Button10: TButton
    Left = 384
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button10'
    TabOrder = 14
    OnClick = Button10Click
  end
  object Button11: TButton
    Left = 528
    Top = 144
    Width = 75
    Height = 25
    Caption = 'set web host'
    TabOrder = 15
    OnClick = Button11Click
  end
  object edtWeb: TEdit
    Left = 528
    Top = 104
    Width = 121
    Height = 21
    TabOrder = 16
    Text = 'www.lazybird.cn'
  end
  object Button12: TButton
    Left = 528
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Button12'
    TabOrder = 17
    OnClick = Button12Click
  end
  object Button13: TButton
    Left = 592
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button13'
    TabOrder = 18
    OnClick = Button13Click
  end
end
