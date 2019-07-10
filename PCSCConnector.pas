unit PCSCConnector;

interface

uses
  Windows, Messages, Classes, Forms, SysUtils,
  SCardErr, WinSCard, WinSmCrd;

type
  TProcSource = (esInit, esConnect, esGetStatus, esTransmit);
  TPCSCStateChange = (scReaderListChange, scReaderPadding,
      scCardInsert, scCardConnect, scCardDisconnect, scCardRemove, scCardInvalid);

  TPCSCErrorEvent = procedure(Sender: TObject; ProcSource: TProcSource; ErrCode: Integer) of object;
  TPCSCStateChangeEvent = procedure(Sender: TObject;
      StateChange: TPCSCStateChange; StateChangeStr: string) of object;

  TPCSCProtocol = (pcpDefault, pcpTx, pcpT0, pcpT1, pcpRAW);
  TPCSCShareMode = (psmExclusive, psmShared, psmDirect);
  TPCSCDispositionMode = (pdmLeave, pdmReset, pdmUnpower, pdmEject);

const
  MaxApduLength = 260; // CLA + INS + P1..3 + 255Bytes
  SCARD_PCI_T0: SCARD_IO_REQUEST = (dwProtocol: 1; dbPciLength: 8);
  SCARD_PCI_T1: SCARD_IO_REQUEST = (dwProtocol: 2; dbPciLength: 8);
  SCARD_PROTOCOL_T0 = $00000001;
  SCARD_PROTOCOL_T1 = $00000002;
  SCARD_PROTOCOL_RAW = $00010000;

type
  TPCSCConnector = class(TComponent)
  private
    FAutoConnect: Boolean;
    FAutoDetect: Boolean;
    FProtocol: TPCSCProtocol;
    FShareMode: TPCSCShareMode;
    FDispositionMode: TPCSCDispositionMode;

    FContext: Integer;
    FCardHandle: Integer;

    FReaderIndex: Integer;
    FReaderList: TStringlist;

    FReaderState: Integer;

    FWatchThread: TThread;

    FAttrProtocol: Cardinal;
    FAttrICCType: string;
    FAttrCardATR: string;
    FAttrVendorName: string;
    FAttrVendorSerial: string;

    FOnStateChange: TPCSCStateChangeEvent;
    FOnError: TPCSCErrorEvent;

    procedure SetAutoDetect(const Value: Boolean);
    function GetConnected: Boolean;
    function GetAPIReady: Boolean;
    function GetCardMute: Boolean;
    function GetCardPresent: Boolean;
    function GetReaderPadding: Boolean;
  protected
    procedure Loaded; override;

    procedure SetReaderState(const NewState: Integer);
    procedure GetReaderAttributes;
    procedure ClearReaderAttributes;
    procedure DoStateChange(State: TPCSCStateChange);

    procedure SetReaderIndex(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ResetAPI: Boolean;
    procedure Refresh;

    procedure Cancel;

    function Connect: Boolean;
    procedure Disconnect;

    function GetResponseFromCard(const APDU: TBytes): TBytes; overload;
    function GetResponseFromCard(const APDUHex: string): string; overload;
    function GetResponseFromCard(const APDUHexCmd: string; var Data: string): string; overload;

    property ReaderIndex: Integer read FReaderIndex write SetReaderIndex default -1;
    property ReaderList: TStringList read FReaderList;

    property Connected: Boolean read GetConnected;

    property ReaderState: Integer read FReaderState;

    property AttrProtocol: Cardinal read FAttrProtocol;
    property AttrICCType: string read FAttrICCType;
    property AttrCardATR: string read FAttrCardATR;
    property AttrVendorName: string read FAttrVendorName;
    property AttrVendorSerial: string read FAttrVendorSerial;

    property APIReady: Boolean read GetAPIReady;
    property CardMute: Boolean read GetCardMute; 
    property CardPresent: Boolean read GetCardPresent; 
    property ReaderPadding: Boolean read GetReaderPadding; 
  published
    property AutoConnect: Boolean read FAutoConnect write FAutoConnect default True;
    property AutoDetect: Boolean read FAutoDetect write SetAutoDetect default False;
    property Protocol: TPCSCProtocol
        read FProtocol write FProtocol default pcpDefault;
    property ShareMode: TPCSCShareMode
        read FShareMode write FShareMode default psmExclusive;
    property DispositionMode: TPCSCDispositionMode
        read FDispositionMode write FDispositionMode default pdmReset;

    property OnStateChange: TPCSCStateChangeEvent read FOnStateChange write FOnStateChange;
    property OnError: TPCSCErrorEvent read FOnError write FOnError;
  end;

function FillData(OldData: string; Len: Integer): string;
function Hex2Bytes(const Value: string): TBytes;
function Bytes2Hex(const Value: TBytes): string;
function Bytes2AnsiString(Value: TBytes): Ansistring;

procedure Register;

implementation

uses
  AnsiStrings;

procedure Register;
begin
  RegisterComponents('PCSC Ctrl', [TPCSCConnector]);
end;

function FillData(OldData: string; Len: Integer): string;
begin
  Result := Copy(OldData + StringOfChar('0', Len), 1, Len);
end;

function Hex2Bytes(const Value: string): TBytes;
const
  HexChars = '0123456789abcdefABCDEF';
var
  Hex: string;
  i, j: integer;
begin
  SetLength(Result, Length(Value) div 2);
  j := 0;
  for i := 1 to Length(Value) do
  begin
    if Pos(Value[i], HexChars) > 0 then
      Hex := Hex + AnsiUpperCase(Value[i]);
    if Length(Hex) = 2 then
    begin
      Result[j] := Byte(StrToInt('$' + Hex));
      Inc(j);
      Hex := '';
    end;
  end;
  SetLength(Result, j);
end;

function Bytes2Hex(const Value: TBytes): string;
var
  i: integer;
begin
  Result := '';
  for i := Low(Value) to High(Value) do
    Result := Result + IntToHex(Value[i], 2) + ' ';
  Result := Trim(Result);
end;

function Bytes2AnsiString(Value: TBytes): Ansistring;
var
  len: integer;
begin
  Result := '';
  len := Length(Value);
  if len = 0 then
    Exit;
  SetLength(Result, len);
  Move(Value[0], Result[1], len);
end;

type
  EPCSCException = class(Exception)
  private
    FSender: TPCSCConnector;
    FProcSource: TProcSource;
    FErrCode: Integer;
  public
    constructor Create(ASender: TPCSCConnector; AProcSource: TProcSource; AErrCode: Integer);
    class function Check(ASender: TPCSCConnector; AProcSource: TProcSource; AErrCode: Integer): Boolean;

    property Sender: TPCSCConnector read FSender;
    property ProcSource: TProcSource read FProcSource;
    property ErrCode: Integer read FErrCode;
  end;

{ EPCSCException }

constructor EPCSCException.Create(ASender: TPCSCConnector;
    AProcSource: TProcSource; AErrCode: Integer);
const
  ProcSourceToStr: array [TProcSource] of string =
      ('Init', 'Connect', 'GetStatus', 'Transmit');
begin
  FSender := ASender;
  FProcSource := AProcSource;
  FErrCode := AErrCode;

  inherited Create('PCSCConnector.' + ProcSourceToStr[ProcSource] + '() fail.' + #13 +
          'Error Code=$' + IntToHex(AErrCode, 8));
end;

class function EPCSCException.Check(ASender: TPCSCConnector;
  AProcSource: TProcSource; AErrCode: Integer): Boolean;
begin
  Result := AErrCode = SCARD_S_SUCCESS;
  if not Result then
    raise EPCSCException.Create(ASender, AProcSource, AErrCode);
end;

type
  TDelimiters = set of AnsiChar;

function SortOutSubstrings(const From: string; var t: array of string; const Delim: TDelimiters = [' ', ';'];
    const ConcatDelim: Boolean = True): Integer;
var a, b, s, i: Integer;
  sep: Boolean;
begin
  a := 1;
  b := Low(t);
  s := 1;
  i := 0;
  sep := ConcatDelim;
  t[b] := '';

  while a <= Length(From) do
  begin
    if not CharInSet(From[a], Delim) then
    begin
      Inc(i);
      sep := False;
    end
    else
    begin
      if not sep then
      begin
        t[b] := Copy(From, s, i);
        Inc(b);
        if b > High(t) then
          Break;
        t[b] := '';
      end;
      if ConcatDelim then
        sep := True;
      s := a + 1;
      i := 0;
    end;
    Inc(a);
  end;

  if (b <= High(t)) and (i > 0) then
  begin
    t[b] := Copy(From, s, i);
    Inc(b);
  end;

  for a := b + 1 to High(t) do
    t[a] := '';
  Result := b;
end;

type
  TPCSCConnectorWatchThread = class(TThread)
  private
    FOwner: TPCSCConnector;
    FContext: Integer;
    FReaderListStr: string;
    FSelectReader: string;
    FStates: SCARD_READERSTATEW;
    FRet: DWORD;
    procedure Sync_ReaderList;
    procedure Sync_ReaderState;
  protected
    procedure Execute; override;
    procedure RefreshStatus;
  public
    constructor Create(AOwner: TPCSCConnector; CreateSuspended: Boolean = False);
    procedure Sync_SetReaderName;
    
    class procedure StaticRefresh(AOwner: TPCSCConnector);
  end;

{ TPCSCConnectorWatchThread }

constructor TPCSCConnectorWatchThread.Create(AOwner: TPCSCConnector; CreateSuspended: Boolean = False);
begin
  FOwner := AOwner;
  FContext := FOwner.FContext;
  FReaderListStr := '';

  FillChar(FStates, SizeOf(FStates), #0);
  FStates.cbAtr := 36;
  FStates.pvUserData := nil;
  FStates.dwEventState := FOwner.ReaderState;

  Sync_SetReaderName;

  FRet := 0;

  FreeOnTerminate := False;

  inherited Create(CreateSuspended);
end;

procedure TPCSCConnectorWatchThread.Sync_SetReaderName;
begin
  if FOwner.ReaderIndex >= 0 then
    FSelectReader := FOwner.ReaderList[FOwner.ReaderIndex]
  else
    FSelectReader := '';

  FillChar(FStates, SizeOf(FStates), #0);
  FStates.cbAtr := 36;

  if FSelectReader <> '' then
    FStates.szReader := PWideChar(FSelectReader)
  else
    FStates.szReader := nil;
end;

procedure TPCSCConnectorWatchThread.Execute;
begin
  while not Terminated do
  begin
    try
      RefreshStatus;
    except
    end;

    Sleep(1000);
  end;
end;

procedure TPCSCConnectorWatchThread.RefreshStatus;
var
  NewReaderListStr: WideString;
  NewReaderListStrSize: Integer;
begin
  if (SCardListReaders(FContext, nil, nil, NewReaderListStrSize) = SCARD_S_SUCCESS)
      and (NewReaderListStrSize <> Length(FReaderListStr)) then
  begin
    SetLength(NewReaderListStr, NewReaderListStrSize);

    if (SCardListReaders(FContext, nil, PChar(NewReaderListStr), NewReaderListStrSize) = SCARD_S_SUCCESS)
        and (FReaderListStr <> NewReaderListStr) then
    begin
      FReaderListStr := NewReaderListStr;
      Sync_ReaderList;
    end;
  end;

  if FStates.szReader <> nil then
  begin
    FStates.dwCurrentState := FStates.dwEventState;
    FRet := SCardGetStatusChange(FContext, 0, @FStates, 1);
    if ((FRet <> SCARD_S_SUCCESS) and
        (FRet <> SCARD_E_TIMEOUT) and
        (FRet <> SCARD_E_CANCELLED)) or
        (FStates.dwCurrentState <> FStates.dwEventState) then
      Sync_ReaderState;
  end;
end;

procedure TPCSCConnectorWatchThread.Sync_ReaderList;
var
  TmpReaderNames: array[0..MAXIMUM_SMARTCARD_READERS] of string;
  i: Integer;
begin
  FOwner.FReaderList.Clear;

  SortOutSubstrings(FReaderListStr, TmpReaderNames, [#0]);
  for i := 0 to MAXIMUM_SMARTCARD_READERS do
  begin
    if TmpReaderNames[i] = '' then
      Break;
    FOwner.FReaderList.Add(TmpReaderNames[i]);
  end;

  if FOwner.FReaderList.IndexOf(FSelectReader) < 0 then
  begin
    FOwner.FReaderIndex := -1;
    FSelectReader := '';
  end
  else
    FOwner.FReaderIndex := FOwner.FReaderList.IndexOf(FSelectReader);

  FOwner.DoStateChange(scReaderListChange);
  if (FOwner.FReaderIndex < 0) and (FOwner.FReaderList.Count > 0) then
  begin
    FOwner.FReaderIndex := 0;
    Sync_SetReaderName;
  end;
end;

procedure TPCSCConnectorWatchThread.Sync_ReaderState;
begin
  try
    EPCSCException.Check(FOwner, esGetStatus, FRet);
    FOwner.SetReaderState(FStates.dwEventState);
  except
    on E: EPCSCException do
    begin
      if Assigned(FOwner.FOnError) then
        FOwner.FOnError(E.Sender, E.ProcSource, E.ErrCode);
    end;
    on E: Exception do
      ;
  end;
end;

class procedure TPCSCConnectorWatchThread.StaticRefresh(AOwner: TPCSCConnector);
var
  SelfThread: TPCSCConnectorWatchThread;
begin
  try
    SelfThread := TPCSCConnectorWatchThread.Create(AOwner, True);
    try
      SelfThread.RefreshStatus;
    finally
      SelfThread.Free;
    end;
  except
  end;
end;

{ TPCSCConnector }

constructor TPCSCConnector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAutoConnect := True;
  FAutoDetect := False;
  FProtocol := pcpDefault;
  FShareMode := psmExclusive;
  FDispositionMode := pdmReset;

  FContext := 0;
  FCardHandle := 0;
  
  FReaderIndex := -1;
  FReaderList := TStringlist.Create;

  FReaderState := SCARD_STATE_UNAWARE;

  FWatchThread := nil;
end;

destructor TPCSCConnector.Destroy;
begin
  Cancel;

  if FWatchThread <> nil then
  begin
    FWatchThread.Terminate;
    FreeAndNil(FWatchThread);
  end;
  
  SCardReleaseContext(FContext);
  FReaderList.Free;

  inherited Destroy;
end;

function TPCSCConnector.ResetAPI: Boolean;
begin
  Result := False;

  try
    if APIReady then
    begin
      if FWatchThread <> nil then
      begin
        FWatchThread.Terminate;
        FreeAndNil(FWatchThread);
      end;
      
      Cancel;
      SCardReleaseContext(FContext);
    end;
  except
  end;

  if not APIReady then
  begin
    Result := PCSCLoadDLL;
    if not Result then
      Exit;

    try
      EPCSCException.Check(Self, esInit,
        SCardEstablishContext(SCARD_SCOPE_USER, nil, nil, @FContext));
      if FAutoDetect then
        FWatchThread := TPCSCConnectorWatchThread.Create(Self);
      Result := True;
    except
      on E: EPCSCException do
      begin
        if Assigned(FOnError) then
          FOnError(E.Sender, E.ProcSource, E.ErrCode);
      end;
      on E: Exception do
        ;
    end;
  end;
end;

procedure TPCSCConnector.Refresh;
begin
  if FWatchThread = nil then
    TPCSCConnectorWatchThread.StaticRefresh(Self);
end;

procedure TPCSCConnector.Cancel;
begin
  if not APIReady then
    Exit;

  Disconnect;
  SCardCancel(FContext);
end;

function TPCSCConnector.Connect: Boolean;
const
  ProtocolMap: array [TPCSCProtocol] of Cardinal =
      (SCARD_PROTOCOL_DEFAULT, SCARD_PROTOCOL_T0 or SCARD_PROTOCOL_T1,
      SCARD_PROTOCOL_T0, SCARD_PROTOCOL_T1, SCARD_PROTOCOL_RAW);
  ShareModeMap: array [TPCSCShareMode] of Integer =
      (SCARD_SHARE_EXCLUSIVE, SCARD_SHARE_SHARED, SCARD_SHARE_DIRECT);
begin
  Result := False;

  if not APIReady then
    Exit;

  Disconnect;

  try
    if (FReaderIndex < 0) and (FReaderList.Count > 0) then
      FReaderIndex := 0;

    if FReaderIndex >= 0 then
    begin
      try
        EPCSCException.Check(Self, esConnect,
          SCardConnect(FContext, PChar(FReaderList[FReaderIndex]), ShareModeMap[FShareMode],
          ProtocolMap[FProtocol], FCardHandle, FAttrProtocol));
        GetReaderAttributes;
        DoStateChange(scCardConnect);
      except
        on E: EPCSCException do
        begin
          if Assigned(FOnError) then
            FOnError(E.Sender, E.ProcSource, E.ErrCode);
        end;
        on E: Exception do
          ;
      end;
    end;
  finally
    Result := Connected;
  end;
end;

procedure TPCSCConnector.Disconnect;
const
  DispositionModeMap: array [TPCSCDispositionMode] of Integer =
      (SCARD_LEAVE_CARD, SCARD_RESET_CARD, SCARD_UNPOWER_CARD, SCARD_EJECT_CARD);
begin
  if not APIReady then
    Exit;

  if Connected then
  begin
    SCardDisconnect(FCardHandle, DispositionModeMap[FDispositionMode]);
    FCardHandle := 0;
    if not (csDestroying in ComponentState) then
      DoStateChange(scCardDisconnect);
  end;
end;

function TPCSCConnector.GetResponseFromCard(const APDU: TBytes): TBytes;
var
  RetVar: Cardinal;
  ALen: Cardinal;
  RLen: Cardinal;
  PSendPci: Pointer;
  PReadPci: Pointer;
begin
  SetLength(Result, 0);

  if not APIReady then
    Exit;


  ALen := Length(APDU);

  if ALen > MaxApduLength then
    Exit;

  SetLength(Result, 59);
  FillChar(Result[0], 59, 0);
  RLen := Length(Result);

  case FAttrProtocol of
    SCARD_PROTOCOL_T0:
      begin
        PSendPci := @SCARD_PCI_T0;
        PReadPci := @SCARD_PCI_T0;
      end;
    SCARD_PROTOCOL_T1:
      begin
        PSendPci := @SCARD_PCI_T1;
        PReadPci := @SCARD_PCI_T1;
      end;
  else
    PSendPci := nil;
    PReadPci := nil;
  end;

  RetVar := SCardTransmit(FCardHandle, PSendPci, PByte(APDU), ALen, PReadPci, PByte(Result), RLen);

  if RetVar = SCARD_S_SUCCESS then
    SetLength(Result, RLen)
  else
  begin
    SetLength(Result, 0);
    if Assigned(FOnError) then
      FOnError(Self, esTransmit, RetVar);
  end;
end;

function TPCSCConnector.GetResponseFromCard(const APDUHex: string): string;
begin
  Result := Bytes2Hex(GetResponseFromCard(Hex2Bytes(APDUHex)));
end;

function TPCSCConnector.GetResponseFromCard(const APDUHexCmd: string; var Data: string): string;
begin
  Result := GetResponseFromCard(APDUHexCmd + Data);

  if Result <> '' then
  begin
    Data := Copy(Result, 1, Length(Result) - 4);
    Result := Copy(Result, Length(Result) - 3, 4);
  end
  else
  begin
    Data := '';
    Result := '';
  end;
end;

procedure TPCSCConnector.Loaded;
begin
  inherited;

  if not (csDesigning in ComponentState) then
    try
      ResetAPI;
      TPCSCConnectorWatchThread.StaticRefresh(Self);
    except
    end;
end;

procedure TPCSCConnector.SetReaderState(const NewState: Integer);
var
  CardInOld, CardInNew: Boolean;
  ReaderEmOld, ReaderEmNew: Boolean;
  CardMuteOld, CardMuteNew: Boolean;
  CardIgnore: Boolean;
begin
  if NewState = FReaderState then
    Exit;

  CardInOld := (FReaderState and SCARD_STATE_PRESENT) > 0;
  CardInNew := (NewState and SCARD_STATE_PRESENT) > 0;

  ReaderEmOld := (FReaderState and SCARD_STATE_EMPTY) > 0;
  ReaderEmNew := (NewState and SCARD_STATE_EMPTY) > 0;

  CardMuteOld := (FReaderState and SCARD_STATE_MUTE) > 0;
  CardMuteNew := (NewState and SCARD_STATE_MUTE) > 0;

  CardIgnore := (NewState and SCARD_STATE_IGNORE) > 0;

  FReaderState := NewState;
  
  if CardMuteNew and not CardMuteOld then
    DoStateChange(scCardInvalid);

  if CardInNew and not CardInOld and
    not CardMuteNew and not CardIgnore then
  begin
    DoStateChange(scCardInsert);
    if Connected then
      GetReaderAttributes
    else if FAutoConnect then
      Connect;
  end;

  if CardInOld and not CardInNew then
  begin
    ClearReaderAttributes;
    Disconnect;
    DoStateChange(scCardRemove);
  end;

  if ReaderEmNew and not ReaderEmOld then
    DoStateChange(scReaderPadding);
end;

procedure TPCSCConnector.GetReaderAttributes;
var
  Ret: DWORD;
  Buf: string;
  IntBuf: Integer;
  BufLen: Integer;
begin
  ClearReaderAttributes;

  Buf := StringOfChar(#0, 127);
  BufLen := Length(Buf);
  Ret := SCardGetAttrib(FCardHandle, SCARD_ATTR_ATR_STRING, Pointer(Buf), BufLen);
  if Ret = SCARD_S_SUCCESS then
    FAttrCardATR := Copy(Buf, 1, BufLen);

  BufLen := Length(Buf);
  Ret := SCardGetAttrib(FCardHandle, SCARD_ATTR_VENDOR_NAME, Pointer(Buf), BufLen);
  if Ret = SCARD_S_SUCCESS then
    FAttrVendorName := Copy(Buf, 1, BufLen);

  BufLen := Length(Buf);
  Ret := SCardGetAttrib(FCardHandle, SCARD_ATTR_VENDOR_IFD_SERIAL_NO, Pointer(Buf), BufLen);
  if Ret = SCARD_S_SUCCESS then
    FAttrVendorSerial := Copy(Buf, 1, BufLen);

  BufLen := SizeOf(IntBuf);
  Ret := SCardGetAttrib(FCardHandle, SCARD_ATTR_CURRENT_PROTOCOL_TYPE, @IntBuf, BufLen);
  if Ret = SCARD_S_SUCCESS then
    FAttrProtocol := IntBuf;

  BufLen := SizeOf(IntBuf);
  Ret := SCardGetAttrib(FCardHandle, SCARD_ATTR_ICC_TYPE_PER_ATR, @IntBuf, BufLen);
  if Ret = SCARD_S_SUCCESS then
  begin
    case IntBuf of
      1: FAttrICCType := 'ISO7816A';
      2: FAttrICCType := 'ISO7816S';
    else
      FAttrICCType := 'UNKNOWN';
    end;
  end;
end;

procedure TPCSCConnector.ClearReaderAttributes;
begin
  FAttrCardATR := '';
  FAttrVendorName := '';
  FAttrVendorSerial := '';
  FAttrProtocol := 0;
  FAttrICCType := '';
end;

procedure TPCSCConnector.DoStateChange(State: TPCSCStateChange);
const
  PCSCStateChangeToStr: array [TPCSCStateChange] of string =
      ('Reader List Changed', 'Reader Padding',
      'Card Insert', 'Card Connect', 'Card Disconnect', 'Card Remove', 'Card Invalid');
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Self, State, PCSCStateChangeToStr[State]);
end;

procedure TPCSCConnector.SetReaderIndex(const Value: Integer);
begin
  if not APIReady then
    Exit;

  if Value <> FReaderIndex then
  begin
    Disconnect;

    if (Value >= 0) and (Value < FReaderList.Count) then
    begin
      FReaderIndex := Value;
      if FWatchThread = nil then
        TPCSCConnectorWatchThread.StaticRefresh(Self)
      else
        TThread.Synchronize(FWatchThread, TPCSCConnectorWatchThread(FWatchThread).Sync_SetReaderName);
      if FAutoConnect then
        Connect;
    end
    else
      FReaderIndex := -1;
  end;
end;

procedure TPCSCConnector.SetAutoDetect(const Value: Boolean);
begin
  if FAutoDetect <> Value then
  begin
    if FWatchThread <> nil then
    begin
      FWatchThread.Terminate;
      FreeAndNil(FWatchThread);
    end;

    if Value then
      FWatchThread := TPCSCConnectorWatchThread.Create(Self);

    FAutoDetect := Value;
  end;
end;

function TPCSCConnector.GetConnected: Boolean;
begin
  Result := FCardHandle <> 0;
end;

function TPCSCConnector.GetAPIReady: Boolean;
begin
  Result := (hWinscardDLL <> 0) and
      (SCardIsValidContext(FContext) = SCARD_S_SUCCESS);
end;

function TPCSCConnector.GetCardMute: Boolean;
begin
  Result := (FReaderState and SCARD_STATE_MUTE) > 0;
end;

function TPCSCConnector.GetCardPresent: Boolean;
begin
  Result := (FReaderState and SCARD_STATE_PRESENT) > 0;
end;

function TPCSCConnector.GetReaderPadding: Boolean;
begin
  Result := (FReaderState and SCARD_STATE_EMPTY) > 0;
end;

end.
