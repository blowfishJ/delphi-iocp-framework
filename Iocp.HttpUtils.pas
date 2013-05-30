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

{$region '基础函数'}
procedure ByteToHex(B: Byte; P: PChar);
function HexToByte(P: PChar): Byte;
function RFC1123_Date(const ADate: TDateTime): string;
{$endregion}

function URLEncode(const S: string; Encodeing: TEncoding = nil): string;
function URLDecode(const S: string; Encodeing: TEncoding = nil): string;
function ExtractURLEncodedValue(const Msg, Name: string; var Value: string;
  Encodeing: TEncoding = nil): Boolean;
function GetCookieValue(const CookieString, Name: string; var Value: string): Boolean;
function MakeCookie(const Name, Value: string; Expires: TDateTime;
  const Path: string; const Domain: string = ''): string;

procedure SetHeader(Header: TStrings; const Key, Value: string); overload;
procedure SetHeader(var Header: string; const Key, Value: string); overload;
function FixHeader(const Header: string): string;

function Posn(const s, t: string; Count: Integer): Integer;
procedure ParseURL(const url: string; var Proto, User, Pass, Host, Port, Path: string);

function IsDirectory(const Path: string): Boolean;
function DosPathToUnixPath(const Path: string): string;
function UnixPathToDosPath(const Path: string): string;
function BuildDirList(const RealPath, RequestPath: string): RawByteString;

implementation

const
  UriProtocolSchemeAllowedChars = ['a'..'z', '0'..'9', '+', '-', '.'];

procedure ByteToHex(B: Byte; P: PChar);
const
  HexChar: array [0..15] of Char =
    ('0', '1', '2', '3', '4', '5', '6', '7',
     '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
begin
  P[0] := HexChar[(B shr 4)];
  P[1] := HexChar[(B and $0F)];
end;

function HexToByte(P: PChar): Byte;
var
  I, B, N: Byte;
begin
  Result := 0;
  for I := 0 to 1 do
  begin
    B := Byte(P[I]);
    case B of
      Byte('0')..Byte('9'): N := B - Byte('0');
    else
      N := (B and $0F) + 9;
    end;
    Result := Result shl 4 + N;
  end;
end;

function URLEncode(const S: string; Encodeing: TEncoding = nil): string;
var
  I, J: Integer;
  B: Byte;
  RStr: string;
  LEncodeing: TEncoding;
  LBytes: TBytes;
begin
  if (S = '') then Exit('');

  if Assigned(Encodeing) then
    LEncodeing := Encodeing
  else
    LEncodeing := TEncoding.UTF8;

  LBytes := LEncodeing.GetBytes(S);
  SetLength(RStr, Length(LBytes) * 3);
  J := 0;
  for I := Low(LBytes) to High(LBytes) do
  begin
    B := LBytes[I];
    case B of
      Byte('0')..Byte('9'), Byte('A')..Byte('Z'), Byte('a')..Byte('z'):
        begin
          Inc(J);
          RStr[J] := Char(B);
        end;
      Byte(' '): RStr[J] := '+';
    else
      Inc(J);
      RStr[J] := '%';
      ByteToHex(B, @RStr[J + 1]);
      Inc(J, 2);
    end;
  end;
  SetLength(RStr, J);

  Result := RStr;
end;

function URLDecode(const S: string; Encodeing: TEncoding = nil): string;
var
  I, J, L: Integer;
  LEncodeing: TEncoding;
  LBytes: TBytes;
  B: Byte;
begin
  if (S = '') then Exit('');


  if Assigned(Encodeing) then
    LEncodeing := Encodeing
  else
    LEncodeing := TEncoding.UTF8;

  L := Length(S);
  SetLength(LBytes, L);
  I := 1;
  J := 0;
  while (I <= L) do
  begin
    B := Byte(S[I]);
    if (B = Byte('%')) then
    begin
      B := HexToByte(@S[I + 1]);
      Inc(I, 2);
    end
    else if (B = Byte('+')) then
      B := Byte(' ');
    Inc(I);
    LBytes[J] := B;
    Inc(J);
  end;

  Result := LEncodeing.GetString(LBytes, 0, J);
end;

function ExtractURLEncodedValue(const Msg, Name: string; var Value: string;
  Encodeing: TEncoding = nil): Boolean;
var
  J: Integer;
  LEncodeing: TEncoding;
  LBytes: TBytes;
  B: Byte;
  NameLen: Integer;
  FoundLen: Integer;
  p, q: PChar;
begin
  Result := False;
  Value := '';
  if (Msg = '') then Exit;

  if Assigned(Encodeing) then
    LEncodeing := Encodeing
  else
    LEncodeing := TEncoding.UTF8;

  SetLength(LBytes, Length(Msg));
  J := 0;

  NameLen := Length(Name);
  p := PChar(Msg);
  while p^ <> #0 do
  begin
    q := p;
    while (p^ <> #0) and (p^ <> '=') do
      Inc(p);
    FoundLen := p - q;
    if (p^ = '=') then
      Inc(p);
    if (StrLIComp(q, @Name[1], NameLen) = 0) and
      (NameLen = FoundLen) then
    begin
      while (p^ <> #0) and (p^ <> '&') do
      begin
        B := Byte(p^);
        if (B = Byte('%')) then
        begin
          if (p[1] <> #0) then
            B := HexToByte(p + 1);
          Inc(p, 2);
        end
        else if (B = Byte('+')) then
          B := Byte(' ');
        LBytes[J] := B;
        Inc(J);
        Inc(p);
      end;
      Result := True;
      Break;
    end;
    while (p^ <> #0) and (p^ <> '&') do
      Inc(p);
    if (p^ = '&') then
      Inc(p);
  end;

  Value := LEncodeing.GetString(LBytes, 0, J);
end;

function GetCookieValue(const CookieString, Name: string; var Value: string): Boolean;
var
  NameLen: Integer;
  Ch: Char;
  p, q: PChar;
begin
  Value := '';
  Result := False;
  if (CookieString = '') or (Name = '') then Exit;

  NameLen := Length(Name);
  p := PChar(CookieString);
  while (p^ <> #0) do
  begin
    while (p^ <> #0) and (p^ = ' ') do
      Inc(p);
    q := p;
    while (p^ <> #0) and (p^ <> '=') do
      Inc(p);
    if (p^ = '=') then
      Inc(p);
    if (StrLIComp(q, @Name[1], NameLen) = 0) then
    begin
      while (p^ <> #0) and (p^ <> ';') do
      begin
        Ch := p^;
        if (Ch = '%') then
        begin
          Ch := Char(HexToByte(p + 1));
          Inc(p, 2);
        end
        else if (Ch = '+') then
          Ch := ' ';
        Value := Value + Ch;
        Inc(p);
      end;
      Result := True;
      Break;
    end;
    while (p^ <> #0) and (p^ <> ';') do
      Inc(p);
    if (p^ = ';') then
      Inc(p);
  end;
end;

function RFC1123_Date(const ADate: TDateTime): string;
const
  StrWeekDay: array [1..7] of string =
    ('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun');
  StrMonth: array [1..12] of string =
    ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
var
  Year, Month, Day: Word;
  Hour, Min, Sec, MSec: Word;
  DayOfWeek: Word;
begin
  DecodeDate(ADate, Year, Month, Day);
  DecodeTime(ADate, Hour, Min, Sec, MSec);
  DayOfWeek := ((Trunc(ADate) - 2) mod 7) + 1;
  Result := Format('%s, %.2d %s %.4d %.2d:%.2d:%.2d',
    [StrWeekDay[DayOfWeek], Day, StrMonth[Month], Year, Hour, Min, Sec]);
end;

function MakeCookie(const Name, Value: string; Expires: TDateTime;
  const Path: string; const Domain: string): string;
begin
  Result := 'Set-Cookie: ' + Name + '=' + URLEncode(Value);
  if (Value = '') then
    Result := Result + '_NONE_';
  if (Expires <> 0) then
    Result := Result + '; EXPIRES=' + RFC1123_Date(Expires);
  if (Domain <> '') then
    Result := Result + '; DOMAIN=' + Domain;
  Result := Result + '; PATH=' + Path + #13#10;
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

