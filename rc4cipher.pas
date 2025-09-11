unit RC4Cipher;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, StrUtils;

type
  // --- Array types ---
  TUnicodeStringArray = array of UnicodeString;
  TWords = array of Word;
  TWideChars = array of WideChar;

  // --- WideChar helper ---
  TWideCharHelper = type helper for WideChar
  public
    function getUTF8Length : integer;
    function getUTF16Length : integer;
    function getCPLength : integer;
    function toCharCode : integer;
    function fromCharCode(chr : integer) : WideChar;
  end;

  // --- UnicodeString helper ---
  TUnicodeStringHelper = type helper for UnicodeString
  public
    function length : integer; overload;
    function substring(index : Integer): unicodestring; overload;
    function substring(index : Integer; len : Integer): unicodestring; overload;
    function charCodeAt(index : integer) : integer;
    function charAt(index : integer) : WideChar;
    function split(const Separators: array of WideChar): TUnicodeStringArray; overload;

    function toWideCharArray : TWideChars;
    function toWordArray : TWords;

    function toUTF8Bytes : TBytes;
    function toUTF16Bytes : TBytes;
    function toCPBytes : TBytes;

    function getUTF8BytesLength : integer;
    function getUTF16BytesLength : integer;
    function getCPBytesLength : integer;

    function fromUTF8Bytes(bytes : TBytes) : UnicodeString;
    function fromUTF16Bytes(bytes : TBytes) : UnicodeString;
    function fromCPBytes(bytes : TBytes) : UnicodeString;

    function hasUTF8BrokenBytes(bytes : TBytes) : boolean;
    function hasUTF16BrokenBytes(bytes : TBytes) : boolean;
    function hasCPBrokenBytes(bytes : TBytes) : boolean;

    function getUTF8BrokenBytes(bytes : TBytes) : TBytes;
    function getUTF16BrokenBytes(bytes : TBytes) : TBytes;
    function getCPBrokenBytes(bytes : TBytes) : TBytes;

    procedure mapCP(map : TWideChars);
  end;

  // --- RC4 class ---
  RC4 = class
  public
    class function Encrypt(Key, Text: String): String;
    class function Decrypt(Key, Text: String): String;
  end;

implementation

{ === TWideCharHelper === }

function TWideCharHelper.getUTF8Length : integer;
begin
  if integer(self) < $80 then result := 1
  else if integer(self) < $800 then result := 2
  else if integer(self) < $10000 then result := 3
  else result := 4;
end;

function TWideCharHelper.getUTF16Length : integer; inline;
begin
  result := 2;
end;

function TWideCharHelper.getCPLength : integer; inline;
begin
  result := 1;
end;

function TWideCharHelper.toCharCode : integer; inline;
begin
  result := integer(self);
end;

function TWideCharHelper.fromCharCode(chr : integer) : WideChar; inline;
begin
  result := WideChar(chr);
end;

{ === TUnicodeStringHelper === }

function TUnicodeStringHelper.length : integer; inline;
begin
  result := system.length(self);
end;

function TUnicodeStringHelper.substring(index : Integer): unicodestring;
var
  strlen, len : integer;
begin
  strlen := self.length;
  if (index < 0) or (index >= strlen) then
    result := ''
  else
  begin
    len := strlen - index;
    setlength(result, len);
    move(self[1 + index], result[1], len * sizeof(WideChar));
  end;
end;

function TUnicodeStringHelper.substring(index : Integer; len : Integer): unicodestring;
var
  strlen : integer;
begin
  strlen := self.length;
  if (index < 0) or (index >= strlen) or (len <= 0) then
    result := ''
  else
  begin
    if index + len > strlen then
      len := strlen - index;
    setlength(result, len);
    move(self[1 + index], result[1], len * sizeof(WideChar));
  end;
end;

function TUnicodeStringHelper.charCodeAt(index : integer) : integer; inline;
begin
  if (index < 0) or (index >= self.length) then
    result := 0
  else
    result := self[index + 1].toCharCode;
end;

function TUnicodeStringHelper.charAt(index : integer) : WideChar; inline;
begin
  if (index < 0) or (index >= self.length) then
    result := WideChar(0)
  else
    result := self[index + 1];
end;

function TUnicodeStringHelper.split(const Separators: array of WideChar): TUnicodeStringArray;
var
  i, j, lastpos : integer;
  ch : widechar;
begin
  setlength(result, 0);
  lastpos := 0;
  for i := 0 to self.length - 1 do
  begin
    ch := self.charAt(i);
    for j := 0 to system.length(Separators) - 1 do
    begin
      if ch = Separators[j] then
      begin
        setlength(result, system.length(result) + 1);
        result[high(result)] := self.substring(lastpos, i - lastpos);
        lastpos := i + 1;
        break;
      end;
    end;
  end;
  setlength(result, system.length(result) + 1);
  result[high(result)] := self.substring(lastpos);
end;

function TUnicodeStringHelper.toWideCharArray : TWideChars;
begin
  setlength(result, self.length);
  move(self[1], result[0], sizeof(WideChar));
end;

function TUnicodeStringHelper.toWordArray : TWords;
var len : longint;
begin
  len := self.length;
  setlength(result, len);
  move(self[1], result[0], sizeof(WideChar) * len);
end;

function TUnicodeStringHelper.toUTF8Bytes : TBytes;
var
  cv, i, len, cl : integer;
  p : pbyte;
  cw : WideChar;
begin
  len := self.getUTF8BytesLength;
  setlength(Result, len);
  p := @Result[0];
  for i := 1 to self.length do
  begin
    cw := self[i];
    cv := cw.toCharCode;
    cl := cw.getUTF8Length;
    case cl of
      1: p^ := cv;
      2: begin
           p^ := %11000000 or ((cv >> 6) and %00011111); inc(p);
           p^ := %10000000 or (cv and %00111111);
         end;
      3: begin
           p^ := %11100000 or ((cv >> 12) and %00001111); inc(p);
           p^ := %10000000 or ((cv >> 6) and %00111111); inc(p);
           p^ := %10000000 or (cv and %00111111);
         end;
      4: raise exception.create('Characters $10000+ unsupported');
    end;
    inc(p);
  end;
end;

function TUnicodeStringHelper.toUTF16Bytes : TBytes;
var len : integer;
begin
  len := self.getUTF16BytesLength;
  setlength(Result, len);
  move(self[1], Result[0], len);
end;

function TUnicodeStringHelper.toCPBytes : TBytes;
var len, i, cv : integer;
begin
  len := self.getUTF8BytesLength;
  setlength(Result, len);
  for i := 1 to len do
  begin
    cv := self[i].toCharCode;
    if cv > 255 then cv := 0;
    Result[i - 1] := cv;
  end;
end;

function TUnicodeStringHelper.getUTF8BytesLength : integer;
var i : integer;
begin
  result := 0;
  for i := 1 to system.length(self) do
    result += self[i].getUTF8Length;
end;

function TUnicodeStringHelper.getUTF16BytesLength : integer; inline;
begin
  result := self.length shl 1;
end;

function TUnicodeStringHelper.getCPBytesLength : integer; inline;
begin
  result := self.length;
end;

function TUnicodeStringHelper.fromUTF8Bytes(bytes : TBytes) : UnicodeString;
var len, pos : integer; val : UInt32; b : byte;
begin
  len := system.length(bytes); result := ''; pos := 0;
  while pos < len do
  begin
    b := bytes[pos];
    if (b and %11111000) = %11110000 then
      raise exception.create('Characters $10000+ unsupported')
    else if (b and %11110000) = %11100000 then
    begin
      if pos + 3 <= len then
      begin
        val := (bytes[pos + 2] and $3F)
             or ((bytes[pos + 1] and $3F) shl 6)
             or ((b and $0F) shl 12);
        result += WideChar(val);
      end;
      inc(pos,3);
    end
    else if (b and %11100000) = %11000000 then
    begin
      if pos + 2 <= len then
      begin
        val := (bytes[pos + 1] and $3F)
             or ((b and $1F) shl 6);
        result += WideChar(val);
      end;
      inc(pos,2);
    end
    else begin
      result += WideChar(b);
      inc(pos);
    end;
  end;
end;

function TUnicodeStringHelper.fromUTF16Bytes(bytes : TBytes) : UnicodeString;
var len,pos:integer;
begin
  len:=system.length(bytes); result:=''; pos:=0;
  while pos < len do
  begin
    if pos+1<len then
      result += widechar(bytes[pos] + (bytes[pos+1] shl 8));
    inc(pos,2);
  end;
end;

function TUnicodeStringHelper.fromCPBytes(bytes : TBytes) : UnicodeString;
var len,i:integer;
begin
  len:=system.length(bytes); result:='';
  for i:=0 to len-1 do result+=WideChar(bytes[i]);
end;

function TUnicodeStringHelper.getUTF8BrokenBytes(bytes : TBytes) : TBytes;
var len,pos:integer; b:byte;
begin
  len:=system.length(bytes); pos:=0;
  while pos < len do
  begin
    b:=bytes[pos];
    if (b and %11110000) = %11100000 then
    begin
      if pos+3>len then begin
        setlength(result,len-pos);
        move(bytes[pos],result[0],len-pos); exit;
      end;
      inc(pos,3);
    end
    else if (b and %11100000) = %11000000 then
    begin
      if pos+2>len then begin
        setlength(result,len-pos);
        move(bytes[pos],result[0],len-pos); exit;
      end;
      inc(pos,2);
    end
    else inc(pos);
  end;
  setlength(result,0);
end;

function TUnicodeStringHelper.getUTF16BrokenBytes(bytes : TBytes) : TBytes;
begin
  if self.HasUTF16BrokenBytes(bytes) then
  begin
    setlength(Result,1);
    Result[0]:=bytes[high(bytes)];
  end else setlength(Result,0);
end;

function TUnicodeStringHelper.getCPBrokenBytes(bytes : TBytes) : TBytes;
begin
  setlength(Result,0);
end;

function TUnicodeStringHelper.hasUTF8BrokenBytes(bytes : TBytes) : boolean;
var len,pos:integer; b:byte;
begin
  len:=system.length(bytes); pos:=0;
  while pos < len do
  begin
    b:=bytes[pos];
    if (b and %11110000) = %11100000 then
    begin
      if pos+3>=len then exit(true);
      inc(pos,3);
    end
    else if (b and %11100000) = %11000000 then
    begin
      if pos+2>=len then exit(true);
      inc(pos,2);
    end
    else inc(pos);
  end;
  result:=false;
end;

function TUnicodeStringHelper.hasUTF16BrokenBytes(bytes : TBytes) : boolean; inline;
begin
  result := (system.length(bytes) and 1) <> 0;
end;

function TUnicodeStringHelper.hasCPBrokenBytes(bytes : TBytes) : boolean; inline;
begin
  result := false;
end;

procedure TUnicodeStringHelper.mapCP(map : TWideChars);
var len,i:integer; pwc:PWideChar; cpchr:integer;
begin
  if system.length(map)<>256 then
    raise exception.create('Invalid mapping table length. Needs 256 characters.');
  len:=self.length;
  pwc:=getmemory(len*sizeof(WideChar));
  move(self[1],pwc,len*sizeof(WideChar));
  self:='';
  for i:=0 to len-1 do
  begin
    cpchr:=pwc[i].toCharCode;
    if cpchr>255 then cpchr:=0;
    self += map[cpchr];
  end;
  freememory(pwc);
end;

{ === RC4 === }

class function RC4.Encrypt(Key, Text: String): String;
var
  i,j,x,y: Integer;
  s: array[0..255] of Integer;
  unicodeKey: UnicodeString;
  unicodeText: UnicodeString;
  charCodeAt: Integer;
  ct: String;
  ctInt: Integer;
begin
  for i := 0 to 255 do s[i] := i;
  unicodeKey := UnicodeString(Key);
  j := 0;
  for i := 0 to 255 do
  begin
    charCodeAt := unicodeKey.charCodeAt(i mod Length(Key));
    j := (j + s[i] + charCodeAt) mod 256;
    x := s[i]; s[i] := s[j]; s[j] := x;
  end;
  i := 0; j := 0;
  unicodeText := UnicodeString(Text);
  ct := '';
  for y:=0 to (Length(Text)-1) do
  begin
    i := (i + 1) mod 256;
    j := (j + s[i]) mod 256;
    x := s[i]; s[i] := s[j]; s[j] := x;
    ctInt := unicodeText.charCodeAt(y) xor (s[(s[i] + s[j]) mod 256]);
    ct := concat(ct, IntToHex(ctInt, 2));
  end;
  Result := UpperCase(ct);
end;

class function RC4.Decrypt(Key, Text: String): String;
var
  c,i,j,x,y: Integer;
  s: array[0..255] of Integer;
  unicodeKey: UnicodeString;
  copyText: String;
  charCodeAt: Integer;
  fromCharCode: WideChar;
  ct: String;
begin
  for i := 0 to 255 do s[i] := i;
  j := 0; unicodeKey := UnicodeString(Key);
  for i := 0 to 255 do
  begin
    charCodeAt := unicodeKey.charCodeAt(i mod Length(Key));
    j := (j + s[i] + charCodeAt) mod 256;
    x := s[i]; s[i] := s[j]; s[j] := x;
  end;
  i := 0; j := 0; ct := '';
  if (0 = (Length(Text) and 1)) and (Length(Text) > 0) then
  begin
    c := 0; y := 0;
    while y < Length(Text) do
    begin
      i := (i + 1) mod 256;
      j := (j + s[i]) mod 256;
      x := s[i]; s[i] := s[j]; s[j] := x;
      copyText := Copy(Text,c,2);
      charCodeAt := Hex2Dec(copyText) xor (s[(s[i] + s[j]) mod 256]);
      fromCharCode := WideChar(charCodeAt);
      ct := concat(ct, String(fromCharCode));
      y := y + 2;
      c := y + 1;
    end;
  end;
  Result := ct;
end;

end.

