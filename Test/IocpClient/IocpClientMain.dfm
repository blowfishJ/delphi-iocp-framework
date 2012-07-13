object fmIocpClient: TfmIocpClient
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'IocpClient'
  ClientHeight = 348
  ClientWidth = 384
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
  object Panel1: TPanel
    Left = 0
    Top = 126
    Width = 384
    Height = 222
    Align = alClient
    TabOrder = 0
    object lbConnections: TLabel
      Left = 136
      Top = 23
      Width = 67
      Height = 13
      Caption = 'lbConnections'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lbSentBytes: TLabel
      Left = 136
      Top = 43
      Width = 57
      Height = 13
      Caption = 'lbSentBytes'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lbRecvBytes: TLabel
      Left = 136
      Top = 63
      Width = 59
      Height = 13
      Caption = 'lbRecvBytes'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lbHandleUsedMemory: TLabel
      Left = 136
      Top = 83
      Width = 103
      Height = 13
      Caption = 'lbHandleUsedMemory'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lbHandleFreeMemory: TLabel
      Left = 136
      Top = 103
      Width = 101
      Height = 13
      Caption = 'lbHandleFreeMemory'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lbIoUsedMemory: TLabel
      Left = 136
      Top = 122
      Width = 80
      Height = 13
      Caption = 'lbIoUsedMemory'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lbIoFreeMemory: TLabel
      Left = 136
      Top = 142
      Width = 78
      Height = 13
      Caption = 'lbIoFreeMemory'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label1: TLabel
      Left = 16
      Top = 23
      Width = 48
      Height = 13
      Caption = #36830#25509#25968#65306
    end
    object Label2: TLabel
      Left = 16
      Top = 43
      Width = 72
      Height = 13
      Caption = #24050#21457#36865#23383#33410#65306
    end
    object Label3: TLabel
      Left = 16
      Top = 63
      Width = 72
      Height = 13
      Caption = #24050#25509#25910#23383#33410#65306
    end
    object Label4: TLabel
      Left = 16
      Top = 83
      Width = 84
      Height = 13
      Caption = #36830#25509#20351#29992#20869#23384#65306
    end
    object Label5: TLabel
      Left = 16
      Top = 103
      Width = 84
      Height = 13
      Caption = #36830#25509#31354#38386#20869#23384#65306
    end
    object Label8: TLabel
      Left = 16
      Top = 122
      Width = 72
      Height = 13
      Caption = 'IO'#20351#29992#20869#23384#65306
    end
    object Label9: TLabel
      Left = 16
      Top = 142
      Width = 72
      Height = 13
      Caption = 'IO'#31354#38386#20869#23384#65306
    end
    object lbRunTime: TLabel
      Left = 136
      Top = 4
      Width = 49
      Height = 13
      Caption = 'lbRunTime'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label13: TLabel
      Left = 16
      Top = 4
      Width = 48
      Height = 13
      Caption = #24050#36816#34892#65306
    end
    object lbSndQueueUsedMemory: TLabel
      Left = 136
      Top = 162
      Width = 120
      Height = 13
      Caption = 'lbSndQueueUsedMemory'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lbSndQueueFreeMemory: TLabel
      Left = 136
      Top = 182
      Width = 118
      Height = 13
      Caption = 'lbSndQueueFreeMemory'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label15: TLabel
      Left = 16
      Top = 162
      Width = 108
      Height = 13
      Caption = #21457#36865#38431#21015#20351#29992#20869#23384#65306
    end
    object Label16: TLabel
      Left = 16
      Top = 182
      Width = 108
      Height = 13
      Caption = #21457#36865#38431#21015#31354#38386#20869#23384#65306
    end
    object Label12: TLabel
      Left = 16
      Top = 202
      Width = 72
      Height = 13
      Caption = #24453#22788#29702#35831#27714#65306
    end
    object lbPendingRequest: TLabel
      Left = 136
      Top = 202
      Width = 86
      Height = 13
      Caption = 'lbPendingRequest'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 384
    Height = 126
    Align = alTop
    TabOrder = 1
    object btnConnect: TButton
      Left = 16
      Top = 20
      Width = 75
      Height = 25
      Caption = #24320#22987
      TabOrder = 0
      OnClick = btnConnectClick
    end
    object btnDisconnect: TButton
      Left = 16
      Top = 51
      Width = 75
      Height = 25
      Caption = #20572#27490
      TabOrder = 1
      OnClick = btnDisconnectClick
    end
    object edtPort: TLabeledEdit
      Left = 239
      Top = 20
      Width = 121
      Height = 21
      EditLabel.Width = 36
      EditLabel.Height = 13
      EditLabel.Caption = #31471#21475#65306
      NumbersOnly = True
      TabOrder = 2
      Text = '8877'
    end
    object edtHost: TLabeledEdit
      Left = 112
      Top = 20
      Width = 121
      Height = 21
      EditLabel.Width = 72
      EditLabel.Height = 13
      EditLabel.Caption = #26381#21153#22120#22320#22336#65306
      TabOrder = 3
      Text = '127.0.0.1'
    end
    object edtThreads: TLabeledEdit
      Left = 112
      Top = 60
      Width = 121
      Height = 21
      EditLabel.Width = 48
      EditLabel.Height = 13
      EditLabel.Caption = #36830#25509#25968#65306
      NumbersOnly = True
      TabOrder = 4
      Text = '100'
    end
    object edtSendDelay: TLabeledEdit
      Left = 239
      Top = 60
      Width = 121
      Height = 21
      EditLabel.Width = 108
      EditLabel.Height = 13
      EditLabel.Caption = #21457#36865#38388#38548#65288#27627#31186#65289#65306
      NumbersOnly = True
      TabOrder = 5
      Text = '0'
      OnChange = edtSendDelayChange
    end
    object cbConsole: TCheckBox
      Left = 23
      Top = 90
      Width = 59
      Height = 17
      Caption = #25511#21046#21488
      TabOrder = 6
      OnClick = cbConsoleClick
    end
    object edtTimeout: TLabeledEdit
      Left = 112
      Top = 100
      Width = 121
      Height = 21
      Hint = '0'#34920#31034#19981#38480#21046
      EditLabel.Width = 72
      EditLabel.Height = 13
      EditLabel.Caption = #36229#26102#65288#31186#65289#65306
      NumbersOnly = True
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      Text = '0'
    end
    object edtLife: TLabeledEdit
      Left = 239
      Top = 100
      Width = 121
      Height = 21
      Hint = '0'#34920#31034#19981#38480#21046
      EditLabel.Width = 84
      EditLabel.Height = 13
      EditLabel.Caption = #29983#21629#26399#65288#31186#65289#65306
      NumbersOnly = True
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      Text = '0'
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 272
    Top = 136
  end
end
