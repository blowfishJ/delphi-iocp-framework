unit Iocp.HttpUtils;

interface

uses
  Windows, Classes, SysUtils, StrUtils, DateUtils;

type
  THttpDirEntry = class
    Visible: Boolean; { TRUE if the entry is to be shown in list  }
    Name: string;
    Size: Int64;
    Year: Word;
    Month: Word;
    Day: Word;
    Hour: Word;
    Min: Word;
    Sec: Word;
    Directory: Boolean;
    ReadOnly: Boolean;
    SysFile: Boolean;
    Hidden: Boolean; { File is hidden, not the same as Visible !  }
  end;

  TCharsetDetectResult = (cdrAscii, cdrUtf8, cdrUnknown);

function URLEncode(const s: RawByteString): string; overload;
function URLEncode(const s: string): string; overload;
function URLDecode(const s: string; DetectUtf8: Boolean = True): string;
procedure SetHeader(Header: TStrings; const Key, Value: string); overload;
procedure SetHeader(var Header: string; const Key, Value: string); overload;
function FixHeader(const Header: string): string;
function ExtractURLEncodedValue(Msg: PChar; Name: string; var Value: string;
  DetectUtf8: Boolean = True): Boolean; overload;
function ExtractURLEncodedValue(const Msg: string; Name: string; var Value: string;
  DetectUtf8: Boolean = True): Boolean; overload;
function RFC1123_Date(aDate: TDateTime): string;
function MakeCookie(const Name, Value: string; Expires: TDateTime;
  const Path: string; const Domain: string = ''): string;
function GetCookieValue(const CookieString: string; const Name: string; var Value: string): Boolean;
function IsDirectory(const Path: string): Boolean;
function DosPathToUnixPath(const Path: string): string;
function UnixPathToDosPath(const Path: string): string;
function BuildDirList(const RealPath, RequestPath: string): RawByteString;

function Posn(const s, t: string; Count: Integer): Integer;
procedure ParseURL(const url: string; var Proto, User, Pass, Host, Port, Path: string);
function CharsetDetect(const Buf: Pointer; Len: Integer): TCharsetDetectResult; overload;
function CharsetDetect(const Str: RawByteString): TCharsetDetectResult; overload;
function IsUtf8Valid(const Buf: Pointer; Len: Integer): Boolean; overload;
function IsUtf8Valid(const Str: RawByteString): Boolean; overload;
function htoin(Value: PWideChar; Len: Integer): Integer; overload;
function htoin(Value: PAnsiChar; Len: Integer): Integer; overload;
function htoi2(Value: PWideChar): Integer; overload;
function htoi2(Value: PAnsiChar): Integer; overload;
function IsXDigit(Ch: WideChar): Boolean; overload;
function IsXDigit(Ch: AnsiChar): Boolean; overload;
function XDigit(Ch: WideChar): Integer; overload;
function XDigit(Ch: AnsiChar): Integer; overload;

implementation

type
  TCharSet = set of AnsiChar;

const
  UriProtocolSchemeAllowedChars: TCharSet = ['a'..'z', '0'..'9', '+', '-', '.'];

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Find the count'th occurence of the s string in the t string.              }
{ If count < 0 then look from the back                                      }

function Posn(const s, t: string; Count: Integer): Integer;
var
  i, h, Last: Integer;
  u: string;
begin
  u := t;
  if Count > 0 then
  begin
    Result := Length(t);
    for i := 1 to Count do
    begin
      h := Pos(s, u);
      if h > 0 then
        u := Copy(u, h + 1, Length(u))
      else
      begin
        u := '';
        Inc(Result);
      end;
    end;
    Result := Result - Length(u);
  end
  else if Count < 0 then
  begin
    Last := 0;
    for i := Length(t) downto 1 do
    begin
      u := Copy(t, i, Length(t));
      h := Pos(s, u);
      if (h <> 0) and ((h + i) <> Last) then
      begin
        Last := h + i - 1;
        Inc(Count);
        if Count = 0 then
          Break;
      end;
    end;
    if Count = 0 then
      Result := Last
    else
      Result := 0;
  end
  else
    Result := 0;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Syntax of an URL: protocol://[user[:password]@]server[:port]/path         }

procedure ParseURL(const url: string; var Proto, User, Pass, Host, Port, Path:
  string);
var
  p, q, i: Integer;
  s: string;
  CurPath: string;
begin
  CurPath := Path;
  Proto := '';
  User := '';
  Pass := '';
  Host := '';
  Port := '';
  Path := '';

  if Length(url) < 1 then Exit;
  try
    { Handle path beginning with "./" or "../".          }
    { This code handle only simple cases !               }
    { Handle path relative to current document directory }
    if (Copy(url, 1, 2) = './') then
    begin
      p := Posn('/', CurPath, -1);
      if p > Length(CurPath) then
        p := 0;
      if p = 0 then
        CurPath := '/'
      else
        CurPath := Copy(CurPath, 1, p);
      Path := CurPath + Copy(url, 3, Length(url));
      Exit;
    end
      { Handle path relative to current document parent directory }
    else if (Copy(url, 1, 3) = '../') then
    begin
      p := Posn('/', CurPath, -1);
      if p > Length(CurPath) then
        p := 0;
      if p = 0 then
        CurPath := '/'
      else
        CurPath := Copy(CurPath, 1, p);

      s := Copy(url, 4, Length(url));
      { We could have several levels }
      while True do
      begin
        CurPath := Copy(CurPath, 1, p - 1);
        p := Posn('/', CurPath, -1);
        if p > Length(CurPath) then
          p := 0;
        if p = 0 then
          CurPath := '/'
        else
          CurPath := Copy(CurPath, 1, p);
        if (Copy(s, 1, 3) <> '../') then
          Break;
        s := Copy(s, 4, Length(s));
      end;

      Path := CurPath + Copy(s, 1, Length(s));
      Exit;
    end;

    p := Pos('://', url);
    q := p;
    if p <> 0 then
    begin
      s := LowerCase(Copy(url, 1, p - 1));
      for i := 1 to Length(s) do
      begin
        if not (AnsiChar(s[i]) in UriProtocolSchemeAllowedChars) then
        begin
          q := i;
          Break;
        end;
      end;
      if q < p then
      begin
        p := 0;
        Proto := 'http';
      end;
    end;
    if p = 0 then
    begin
      if (url[1] = '/') then
      begin
        { Relative path without protocol specified }
        Proto := 'http';
        p := 1;
        if (Length(url) > 1) and (url[2] <> '/') then
        begin
          { Relative path }
          Path := Copy(url, 1, Length(url));
          Exit;
        end;
      end
      else if LowerCase(Copy(url, 1, 5)) = 'http:' then
      begin
        Proto := 'http';
        p := 6;
        if (Length(url) > 6) and (url[7] <> '/') then
        begin
          { Relative path }
          Path := Copy(url, 6, Length(url));
          Exit;
        end;
      end
      else if LowerCase(Copy(url, 1, 7)) = 'mailto:' then
      begin
        Proto := 'mailto';
        p := Pos(':', url);
      end;
    end
    else
    begin
      Proto := LowerCase(Copy(url, 1, p - 1));
      Inc(p, 2);
    end;
    s := Copy(url, p + 1, Length(url));

    p := Pos('/', s);
    q := Pos('?', s);
    if (q > 0) and ((q < p) or (p = 0)) then
      p := q;
    if p = 0 then
      p := Length(s) + 1;
    Path := Copy(s, p, Length(s));
    s := Copy(s, 1, p - 1);

    p := Posn(':', s, -1);
    if p > Length(s) then
      p := 0;
    q := Posn('@', s, -1);
    if q > Length(s) then
      q := 0;
    if (p = 0) and (q = 0) then
    begin { no user, password or port }
      Host := s;
      Exit;
    end
    else if q < p then
    begin { a port given }
      Port := Copy(s, p + 1, Length(s));
      Host := Copy(s, q + 1, p - q - 1);
      if q = 0 then
        Exit; { no user, password }
      s := Copy(s, 1, q - 1);
    end
    else
    begin
      Host := Copy(s, q + 1, Length(s));
      s := Copy(s, 1, q - 1);
    end;
    p := Pos(':', s);
    if p = 0 then
      User := s
    else
    begin
      User := Copy(s, 1, p - 1);
      Pass := Copy(s, p + 1, Length(s));
    end;
  finally
    if (Port = '') then
    begin
      if SameText(Proto, 'http') then
        Port := '80'
      else if SameText(Proto, 'https') then
        Port := '443';
    end;

    if (Path = '') then
      Path := '/';
  end;
end;

function CharsetDetect(const Buf: Pointer; Len: Integer): TCharsetDetectResult;
var
  PEndBuf: PByte;
  PBuf: PByte;
  Byte2Mask: Byte;
  Ch: Byte;
  Trailing: Integer; // trailing (continuation) bytes to follow
begin
  PBuf := Buf;
  PEndBuf := Pointer(INT_PTR(Buf) + Len);
  Byte2Mask := $00;
  Trailing := 0;
  Result := cdrAscii;
  while (PBuf <> PEndBuf) do
  begin
    Ch := PBuf^;
    Inc(PBuf);
    if Trailing <> 0 then
    begin
      if Ch and $C0 = $80 then // Does trailing byte follow UTF-8 format?
      begin
        if (Byte2Mask <> 0) then // Need to check 2nd byte for proper range?
          if Ch and Byte2Mask <> 0 then // Are appropriate bits set?
            Byte2Mask := 0
          else
          begin
            Result := cdrUnknown;
            Exit;
          end;
        Dec(Trailing);
        Result := cdrUtf8;
      end
      else
      begin
        Result := cdrUnknown;
        Exit;
      end;
    end
    else
    begin
      if Ch and $80 = 0 then
        Continue // valid 1 byte UTF-8
      else if Ch and $E0 = $C0 then // valid 2 byte UTF-8
      begin
        if Ch and $1E <> 0 then // Is UTF-8 byte in proper range?
          Trailing := 1
        else
        begin
          Result := cdrUnknown;
          Exit;
        end;
      end
      else if Ch and $F0 = $E0 then // valid 3 byte UTF-8
      begin
        if Ch and $0F = 0 then // Is UTF-8 byte in proper range?
          Byte2Mask := $20; // If not set mask to check next byte
        Trailing := 2;
      end
      else if Ch and $F8 = $F0 then // valid 4 byte UTF-8
      begin
        if Ch and $07 = 0 then // Is UTF-8 byte in proper range?
          Byte2Mask := $30; // If not set mask to check next byte
        Trailing := 3;
      end
          { 4 byte is the maximum today, see ISO 10646, so let's break here }
          { else if Ch and $FC = $F8 then     // valid 5 byte UTF-8
            begin
                if Ch and $03 = 0 then        // Is UTF-8 byte in  proper range?
                    Byte2Mask := $38;         // If not set mask to check next byte
                Trailing := 4;
            end
            else if Ch and $FE = $FC then     // valid 6 byte UTF-8
            begin
                if ch and $01 = 0 then        // Is UTF-8 byte in proper range?
                    Byte2Mask := $3C;         // If not set mask to check next byte
                Trailing := 5;
            end}
      else
      begin
        Result := cdrUnknown;
        Exit;
      end;
    end;
  end; // while

  case Result of
    cdrUtf8, cdrAscii: if Trailing <> 0 then Result := cdrUnknown;
  end;
end;

function CharsetDetect(const Str: RawByteString): TCharsetDetectResult;
begin
  Result := CharsetDetect(Pointer(Str), Length(Str));
end;

function IsUtf8Valid(const Buf: Pointer; Len: Integer): Boolean;
begin
  Result := CharsetDetect(Buf, Len) <> cdrUnknown;
end;

function IsUtf8Valid(const Str: RawByteString): Boolean;
begin
  Result := IsUtf8Valid(Pointer(Str), Length(Str));
end;

function URLEncode(const s: RawByteString): string; overload;
var
  i, J: Integer;
  AStr: UTF8String;
  RStr: AnsiString;
  HexStr: string;
begin
  AStr := s;
  SetLength(RStr, Length(AStr) * 3);
  J := 0;
  for i := 1 to Length(AStr) do
  begin
    case AStr[i] of
      '0'..'9', 'A'..'Z', 'a'..'z':
        begin
          Inc(J);
          RStr[J] := AStr[i];
        end
    else
      Inc(J);
      RStr[J] := '%';
      HexStr := IntToHex(Ord(AStr[i]), 2);
      Inc(J);
      RStr[J] := AnsiChar(HexStr[1]);
      Inc(J);
      RStr[J] := AnsiChar(HexStr[2]);
    end;
  end;
  SetLength(RStr, J);

  Result := string(RStr);
end;

function URLEncode(const s: string): string;
begin
  Result := URLEncode(UTF8Encode(s));
end;

function URLDecode(const s: string; DetectUtf8: Boolean): string;
var
  i, J, L: Integer;
  U8Str: AnsiString;
  Ch: AnsiChar;
begin
  L := Length(s);
  SetLength(U8Str, L);
  i := 1;
  J := 0;
  while (i <= L){ and (s[i] <> '&')} do
  begin
    Ch := AnsiChar(s[i]);
    if Ch = '%' then
    begin
      Ch := AnsiChar(htoi2(PChar(@s[i + 1])));
      Inc(i, 2);
    end
    else if Ch = '+' then
      Ch := ' ';
    Inc(J);
    U8Str[J] := Ch;
    Inc(i);
  end;
  SetLength(U8Str, J);

  if DetectUtf8 and IsUtf8Valid(U8Str) then
    Result := UTF8ToString(U8Str)
  else
    Result := string(U8Str);
end;

procedure SetHeader(Header: TStrings; const Key, Value: string); overload;
var
  i: Integer;
begin
  for i := 0 to Header.Count - 1 do
  begin
    if (Pos(LowerCase(Key) + ':', LowerCase(string(Header[i]))) = 1) then
    begin
      Header[i] := string(Key + ': ' + Value);
      Exit;
    end;
  end;

  Header.Add(string(Key + ': ' + Value));
end;

procedure SetHeader(var Header: string; const Key, Value: string); overload;
var
  HeaderList: TStringList;
begin
  HeaderList := TStringList.Create;
  try
    HeaderList.Text := string(Header);
    SetHeader(HeaderList, Key, Value);
    Header := string(HeaderList.Text);
  finally
    HeaderList.Free;
  end;
end;

function FixHeader(const Header: string): string;
begin
  Result := Header;
  if (RightStr(Header, 4) <> #13#10#13#10) then
  begin
    if (RightStr(Header, 2) = #13#10) then
      Result := Result + #13#10
    else
      Result := Result + #13#10#13#10;
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

function XDigit(Ch: WideChar): Integer;
begin
  case Ch of
    '0'..'9': Result := Ord(Ch) - Ord('0');
  else
    Result := (Ord(Ch) and 15) + 9;
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

function XDigit(Ch: AnsiChar): Integer;
begin
  case Ch of
    '0'..'9': Result := Ord(Ch) - Ord('0');
  else
    Result := (Ord(Ch) and 15) + 9;
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

function IsXDigit(Ch: WideChar): Boolean;
begin
  Result := ((Ch >= '0') and (Ch <= '9')) or
    ((Ch >= 'a') and (Ch <= 'f')) or
    ((Ch >= 'A') and (Ch <= 'F'));
end;

function IsXDigit(Ch: AnsiChar): Boolean;
begin
  Result := ((Ch >= '0') and (Ch <= '9')) or
    ((Ch >= 'a') and (Ch <= 'f')) or
    ((Ch >= 'A') and (Ch <= 'F'));
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

function htoin(Value: PWideChar; Len: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  i := 0;
  while (i < Len) and (Value[i] = ' ') do
    i := i + 1;
  while (i < Len) and (IsXDigit(Value[i])) do
  begin
    Result := Result * 16 + XDigit(Value[i]);
    i := i + 1;
  end;
end;

function htoin(Value: PAnsiChar; Len: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  i := 0;
  while (i < Len) and (Value[i] = ' ') do
    i := i + 1;
  while (i < Len) and (IsXDigit(Value[i])) do
  begin
    Result := Result * 16 + XDigit(Value[i]);
    i := i + 1;
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

function htoi2(Value: PWideChar): Integer;
begin
  Result := htoin(Value, 2);
end;

function htoi2(Value: PAnsiChar): Integer;
begin
  Result := htoin(Value, 2);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Retrieve a single value by name out of an URL encoded data stream         }
{ In the stream, every space is replaced by a '+'. The '%' character is     }
{ an escape character. The next two are 2 digits hexadecimal codes ascii    }
{ code value. The stream is constitued by name=value couples separated      }
{ by a single '&' character. The special characters are coded by the '%'    }
{ followed by hex-ascii character code.                                     }
function ExtractURLEncodedValue(
  Msg: PChar; { URL Encoded stream                     }
  Name: string; { Variable name to look for              }
  var Value: string; { Where to put variable value            }
  DetectUtf8: Boolean): Boolean; { Found or not found that's the question }
var
  NameLen: Integer;
  FoundLen: Integer; {tps}
  Ch: AnsiChar;
  p, q: PChar;
  U8Str: AnsiString;
begin
  Result := False;
  Value := '';
  if Msg = nil then { Empty source }
    Exit;

  NameLen := Length(Name);
  U8Str := '';
  p := Msg;
  while p^ <> #0 do
  begin
    q := p;
    while (p^ <> #0) and (p^ <> '=') do
      Inc(p);
    FoundLen := p - q; {tps}
    if p^ = '=' then
      Inc(p);
    if (StrLIComp(q, @Name[1], NameLen) = 0) and
      (NameLen = FoundLen) then
    begin {tps}
      while (p^ <> #0) and (p^ <> '&') do
      begin
        Ch := AnsiChar(Ord(p^)); // should contain nothing but < ord 128
        if Ch = '%' then
        begin
          if p[1] <> #0 then // V1.35 Added test
            Ch := AnsiChar(htoi2(p + 1));
          Inc(p, 2);
        end
        else if Ch = '+' then
          Ch := ' ';
        U8Str := U8Str + Ch;
        Inc(p);
      end;
      Result := True;
      Break;
    end;
    while (p^ <> #0) and (p^ <> '&') do
      Inc(p);
    if p^ = '&' then
      Inc(p);
  end;

  if DetectUtf8 and IsUtf8Valid(U8Str) then
    Value := UTF8ToString(U8Str)
  else
    Value := string(U8Str);
end;

function ExtractURLEncodedValue(
  const Msg: string; { URL Encoded stream                     }
  Name: string; { Variable name to look for              }
  var Value: string; { Where to put variable value            }
  DetectUtf8: Boolean): Boolean; overload;
begin
  Result := ExtractURLEncodedValue(PChar(Msg), Name, Value, DetectUtf8);
end;

function RFC1123_Date(aDate: TDateTime): string;
const
  StrWeekDay: string = 'MonTueWedThuFriSatSun';
  StrMonth: string = 'JanFebMarAprMayJunJulAugSepOctNovDec';
var
  Year, Month, Day: Word;
  Hour, Min, Sec, MSec: Word;
  DayOfWeek: Word;
begin
  DecodeDate(aDate, Year, Month, Day);
  DecodeTime(aDate, Hour, Min, Sec, MSec);
  DayOfWeek := ((Trunc(aDate) - 2) mod 7);
  Result := Copy(StrWeekDay, 1 + DayOfWeek * 3, 3) + ', ' +
    Format('%2.2d %s %4.4d %2.2d:%2.2d:%2.2d',
    [Day, Copy(StrMonth, 1 + 3 * (Month - 1), 3),
    Year, Hour, Min, Sec]);
end;

function MakeCookie(const Name, Value: string; Expires: TDateTime;
  const Path: string; const Domain: string): string;
begin
  Result := 'Set-Cookie: ' + Name + '=' + URLEncode(Value);
  if Length(Value) = 0 then
    Result := Result + '_NONE_; EXPIRES=' + RFC1123_Date(Date - 7)
  else if Expires <> 0 then
    Result := Result + '; EXPIRES=' + RFC1123_Date(Expires);
  if Domain <> '' then
    Result := Result + '; DOMAIN=' + Domain;
  Result := Result + '; PATH=' + Path + #13#10;
end;

function GetCookieValue(
  const CookieString: string; { Cookie string from header line         }
  const Name: string; { Cookie name to look for                }
  var Value: string) { Where to put variable value            }
  : Boolean; { Found or not found that's the question }
var
  NameLen: Integer;
  Ch: Char;
  p, q: PChar;
begin
  Value := '';
  Result := False;

  if (CookieString = '') or (Name = '') then
    Exit;

  NameLen := Length(Name);
  p := @CookieString[1];
  while p^ <> #0 do
  begin
    while (p^ <> #0) and (p^ = ' ') do
      Inc(p);
    q := p;
    while (p^ <> #0) and (p^ <> '=') do
      Inc(p);
    if p^ = '=' then
      Inc(p);
    if StrLIComp(q, @Name[1], NameLen) = 0 then
    begin
      while (p^ <> #0) and (p^ <> ';') do
      begin
        Ch := p^;
        if Ch = '%' then
        begin
          Ch := Char(htoi2(p + 1));
          Inc(p, 2);
        end
        else if Ch = '+' then
          Ch := ' ';
        Value := Value + Ch;
        Inc(p);
      end;
      Result := True;
      Break;
    end;
    while (p^ <> #0) and (p^ <> ';') do
      Inc(p);
    if p^ = ';' then
      Inc(p);
  end;
end;

function IsDirectory(const Path: string): Boolean;
var
  Attr: DWORD;
begin
  Attr := GetFileAttributes(PChar(ExcludeTrailingPathdelimiter(Path)));
  Result := (Attr <> MaxDWord) and ((Attr and FILE_ATTRIBUTE_DIRECTORY) <> 0);
end;

function DosPathToUnixPath(const Path: string): string;
begin
  Result := StringReplace(Path, '\', '/', [rfReplaceAll]);
end;

function UnixPathToDosPath(const Path: string): string;
begin
  Result := StringReplace(Path, '/', '\', [rfReplaceAll]);
end;

const
  KBYTES = Int64(1024);
  MBYTES = KBYTES * 1024;
  GBYTES = MBYTES * 1024;
  TBYTES = GBYTES * 1024;
  PBYTES = TBYTES * 1024;

function SmartSizeToStr(Bytes: Int64): string;
begin
  if (Bytes < KBYTES) then
    Result := Format('%dB', [Bytes])
  else if (Bytes < MBYTES) then
    Result := Format('%.2fK ', [Bytes / KBYTES])
  else if (Bytes < GBYTES) then
    Result := Format('%.2fM ', [Bytes / MBYTES])
  else if (Bytes < TBYTES) then
    Result := Format('%.2fG ', [Bytes / GBYTES])
  else if (Bytes < PBYTES) then
    Result := Format('%.2fT ', [Bytes / TBYTES])
  else
    Result := Format('%.2fP ', [Bytes / PBYTES]);
end;

function FormatDirEntry(const Path: string; F: THttpDirEntry): string;
var
  Attr, Link, NameString, SizeString: string;
begin
  if (F.Name = '.') or (F.Name = '..') then
  begin
    Result := '';
    Exit;
  end;

  // drwsh
  Attr := '-rw--';
  if F.Directory then
  begin
    Attr[1] := 'd';
    SizeString := '';
    NameString := '<font color="#000080">' + F.Name + '</font>';
  end
  else
  begin
    SizeString := SmartSizeToStr(F.Size);
    NameString := F.Name;
  end;

  if F.ReadOnly then
    Attr[3] := '-';

  if F.SysFile then
    Attr[4] := 's';

  if F.Hidden then
    Attr[5] := 'h';

  if (Path[Length(Path)] = '/') then
    Link := URLEncode(F.Name)
  else
    Link := Path + '/' + URLEncode(F.Name);

  Result :=
    '<TD WIDTH="55%" NOWRAP><A HREF="' + Link + '">' + NameString + '</A></TD>' +
    '<TD WIDTH="5%" ALIGN="LEFT" NOWRAP>' + Attr + '</TD>' +
    '<TD WIDTH="%15" ALIGN="right" NOWRAP>' + SizeString + '</TD>' +
    '<TD WIDTH="5%" NOWRAP></TD>' +
    '<TD WIDTH="20%" NOWRAP>' + Format('%.4d-%.2d-%.2d %.2d:%.2d:%.2d', [F.Year, F.Month, F.Day, F.Hour, F.Min, F.Sec]) + '</TD>';
end;

function PathToURL(const Path: string): string;
var
  i, j: Integer;
  s, SubPath: string;
begin
  Result := '<A HREF="/"><b><font color="#FF0000">/</font></b></A> ';
  SubPath := '/';

  j := 1;
  while True do
  begin
    i := PosEx('/', Path, j);
    if (i <= 0) then Break;

    if (i > 1) then
    begin
      s := Copy(Path, j, i - j);
      SubPath := SubPath + URLEncode(s) + '/';
      Result := Result + '<A HREF="' + SubPath + '"><b><font color="#000080">' + s + '</font></b></A> / ';
    end;

    j := i + 1;
  end;

  if (j < Length(Path)) then
  begin
    s := Copy(Path, j, Length(Path) - j + 1);
    SubPath := SubPath + URLEncode(s) + '/';
    Result := Result + '<A HREF="' + SubPath + '"><b><font color="#000080">' + s + '</font></b></A> / ';
  end;
end;

function BuildDirList(const RealPath, RequestPath: string): RawByteString;
var
  Status: Integer;
  F: TSearchRec;
  DirList: TStringList;
  FileList: TStringList;
  Data: THttpDirEntry;
  i: Integer;
  ms: Word;
  Total: Cardinal;
  TotalBytes: Int64;
  Document, HTML: string;
begin
  Document := RealPath;
  DirList := TStringList.Create;
  FileList := TStringList.Create;
  Status := FindFirst(Document + '\*.*', faAnyFile, F);
  while Status = 0 do
  begin
    if (F.Name <> '.') and (F.Name <> '..') then
    begin
      Data := THttpDirEntry.Create;
      Data.Visible := True;
      Data.Name := F.Name;
      Data.Size := F.Size;
      DecodeDateTime(F.TimeStamp, Data.Year, Data.Month, Data.Day, Data.Hour, Data.Min, Data.Sec, ms);
      Data.Directory := ((F.Attr and faDirectory) <> 0);
      Data.ReadOnly := ((F.Attr and faReadOnly) <> 0);
      Data.SysFile := ((F.Attr and faSysFile) <> 0);
      Data.Hidden := ((F.Attr and faHidden) <> 0);

      if ((F.Attr and faDirectory) <> 0) then
        DirList.AddObject(Data.Name, Data)
      else
        FileList.AddObject(Data.Name, Data);
    end;

    Status := FindNext(F);
  end;
  FindClose(F);
  DirList.Sort;
  FileList.Sort;

  HTML :=
    '<HTML>' +
    '<HEAD>' +
    '' +
    '<STYLE TYPE="text/css">' +
    '.dirline { font-family: "Microsoft Yahei",simsun,arial; color: black; font-style: normal; }' +
    '.hline {height:0;overflow:hiddne;border-top:1px solid #C3C3C3}' +
    '.vline {width:0;overflow:hiddne;border-left:1px solid #C3C3C3}' +
    'a:link {text-decoration: none; color: #000000;}' +
    'a:visited {text-decoration: none; color: #000000;} ' +
    'a:hover {text-decoration: underline; color: #0000FF;}' +
    'a:active {text-decoration: none; color: #000000;}' +
    '</STYLE>' +
    '<TITLE>文件列表</TITLE>' +
    '<meta http-equiv="Content-Type" content="text/html; charset=utf-8">' +
    '</HEAD>' +
    '<BODY>' +
    '<TABLE CLASS="dirline" WIDTH="90%" ALIGN="CENTER">' +
    '<TR><TD>' + PathToURL(RequestPath) + ':<BR><BR></TD></TR></TABLE>';

  TotalBytes := 0;
  Total := DirList.Count + FileList.Count;
  if Total <= 0 then
    HTML := HTML + '<TABLE CLASS="dirline" WIDTH="90%" ALIGN="CENTER"><TR><TD><BR>空目录</TD></TR></TABLE>'
  else
  begin
    HTML := HTML +
      // 标题
      '<TABLE CLASS="dirline" WIDTH="90%" ALIGN="CENTER">' +
      '<TR>' +
      '<TD WIDTH="55%" NOWRAP>文件名</TD>' +
      '<TD WIDTH="5%" ALIGN="LEFT" NOWRAP>属性</TD>' +
      '<TD WIDTH="%15" ALIGN="right" NOWRAP>大小</TD>' +
      '<TD WIDTH="5%" NOWRAP></TD>' +
      '<TD WIDTH="20%" NOWRAP>修改时间</TD>' +
      '</TR>' +
      '</TABLE>' +

      // 一条灰色横线
      '<TABLE CLASS="dirline" WIDTH="90%" ALIGN="CENTER">' +
      '<TR><TD HEIGHT="3"><div class="hline"></div></TD></TR>' +
      '</TABLE>' +

      // 文件列表表格
      '<TABLE CLASS="dirline" WIDTH="90%" ALIGN="CENTER">';

    for i := 0 to DirList.Count - 1 do
    begin
      Data := THttpDirEntry(DirList.Objects[i]);
      HTML := HTML + '<TR>' + FormatDirEntry(RequestPath, Data) + '</TR>';
      Data.Free;
    end;

    for i := 0 to FileList.Count - 1 do
    begin
      Data := THttpDirEntry(FileList.Objects[i]);
      HTML := HTML + '<TR>' + FormatDirEntry(RequestPath, Data) + '</TR>';
      TotalBytes := TotalBytes + Data.Size;
      Data.Free;
    end;

    HTML := HTML + '</TABLE>' +
      // 一条灰色横线
      '<TABLE CLASS="dirline" WIDTH="90%" ALIGN="CENTER">' +
      '<TR><TD HEIGHT="3"><div class="hline"></div></TD></TR>' +
      '</TABLE>' +

      // 页脚统计信息
      '<TABLE CLASS="dirline" WIDTH="90%" ALIGN="CENTER">' +
      '<TR>' +
      '<TD WIDTH="55%" NOWRAP>' + Format('目录: %d, 文件: %d', [DirList.Count, FileList.Count]) + '</TD>' +
      '<TD WIDTH="5%" NOWRAP></TD>' +
      '<TD WIDTH="%15" ALIGN="right" NOWRAP>' + SmartSizeToStr(TotalBytes) + '</TD>' +
      '<TD WIDTH="5%" NOWRAP></TD>' +
      '<TD WIDTH="20%" NOWRAP></TD>' +
      '</TR>' +
      '</TABLE>';

    DirList.Free;
    FileList.Free;
  end;

  HTML := HTML + '</BODY></HTML>';
  Result := UTF8Encode(HTML);
end;

end.

