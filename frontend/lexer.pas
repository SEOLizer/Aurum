{$mode objfpc}{$H+}
unit lexer;

interface

uses
  SysUtils,
  diag;

type
  TTokenKind = (
    // Literale
    tkIntLit, tkFloatLit, tkStrLit, tkCharLit, tkIdent,
    // Keywords
    tkFn, tkVar, tkLet, tkCo, tkCon,
    tkIf, tkElse, tkWhile, tkReturn,
    tkTrue, tkFalse, tkExtern, tkCase, tkSwitch, tkBreak, tkDefault,
    tkUnit, tkImport, tkPub, tkAs, tkType, tkStruct, tkArray,
    tkFor, tkTo, tkDownto, tkDo, tkRepeat, tkUntil,
    // Operatoren
    tkPlus, tkMinus, tkStar, tkSlash, tkPercent,
    tkAssign,
    tkEq, tkNeq,
    tkLt, tkLe, tkGt, tkGe,
    tkAnd, tkOr, tkNot,
    // Trennzeichen
    tkLParen, tkRParen, tkLBrace, tkRBrace,
    tkLBracket, tkRBracket,
    tkColon, tkComma, tkSemicolon,     tkEllipsis, tkDot, tkAt,
    // Sonstiges
    tkEOF, tkError
  );

  TToken = record
    Kind: TTokenKind;
    Value: string;
    Span: TSourceSpan;
  end;

  TLexer = class
  private
    FSource: string;
    FFileName: string;
    FPos: Integer;
    FLine: Integer;
    FCol: Integer;
    FDiag: TDiagnostics;
    FPeeked: Boolean;
    FPeekTok: TToken;

    function CurrentChar: Char;
    function PeekChar: Char;
    procedure Advance;
    procedure SkipWhitespace;
    procedure SkipLineComment;
    procedure SkipBlockComment;
    function IsAtEnd: Boolean;

    function MakeToken(kind: TTokenKind; const value: string;
      startLine, startCol, len: Integer): TToken;
    function ReadNumber: TToken;
    function ReadString: TToken;
    function ReadCharLit: TToken;
    function ReadIdentOrKeyword: TToken;
    function LookupKeyword(const s: string): TTokenKind;
  public
    constructor Create(const source, fileName: string; d: TDiagnostics);
    destructor Destroy; override;

    function NextToken: TToken;
    function PeekToken: TToken;
  end;

function TokenKindToStr(kind: TTokenKind): string;

implementation

function TokenKindToStr(kind: TTokenKind): string;
begin
  case kind of
    tkIntLit:    Result := 'IntLit';
    tkFloatLit:  Result := 'FloatLit';
    tkStrLit:    Result := 'StrLit';
    tkCharLit:   Result := 'CharLit';
    tkIdent:     Result := 'Ident';
    tkFn:        Result := 'fn';
    tkVar:       Result := 'var';
    tkLet:       Result := 'let';
    tkCo:        Result := 'co';
    tkCon:       Result := 'con';
    tkIf:        Result := 'if';
    tkElse:      Result := 'else';
    tkWhile:     Result := 'while';
    tkReturn:    Result := 'return';
    tkTrue:      Result := 'true';
    tkFalse:     Result := 'false';
    tkExtern:    Result := 'extern';
    tkCase:      Result := 'case';
    tkSwitch:    Result := 'switch';
    tkBreak:     Result := 'break';
    tkDefault:   Result := 'default';
    tkUnit:      Result := 'unit';
    tkImport:    Result := 'import';
    tkPub:       Result := 'pub';
    tkAs:        Result := 'as';
    tkType:      Result := 'type';
    tkStruct:    Result := 'struct';
    tkArray:     Result := 'array';
    tkFor:       Result := 'for';
    tkTo:        Result := 'to';
    tkDownto:    Result := 'downto';
    tkDo:        Result := 'do';
    tkRepeat:     Result := 'repeat';
    tkUntil:      Result := 'until';
    tkPlus:       Result := '+';
    tkMinus:     Result := '-';
    tkStar:      Result := '*';
    tkSlash:     Result := '/';
    tkPercent:   Result := '%';
    tkAssign:    Result := ':=';
    tkEq:        Result := '==';
    tkNeq:       Result := '!=';
    tkLt:        Result := '<';
    tkLe:        Result := '<=';
    tkGt:        Result := '>';
    tkGe:        Result := '>=';
    tkAnd:       Result := '&&';
    tkOr:        Result := '||';
    tkNot:       Result := '!';
    tkLParen:    Result := '(';
    tkRParen:    Result := ')';
    tkLBrace:    Result := '{';
    tkRBrace:    Result := '}';
    tkLBracket:  Result := '[';
    tkRBracket:  Result := ']';
    tkColon:     Result := ':';
    tkComma:     Result := ',';
    tkSemicolon: Result := ';';
    tkEllipsis:  Result := '...';
    tkDot:       Result := '.';
    tkAt:        Result := '@';
    tkEOF:       Result := 'EOF';
    tkError:     Result := 'ERROR';
  end;
end;

{ TLexer }

constructor TLexer.Create(const source, fileName: string; d: TDiagnostics);
begin
  inherited Create;
  FSource := source;
  FFileName := fileName;
  FPos := 1;
  FLine := 1;
  FCol := 1;
  FDiag := d;
  FPeeked := False;
end;

destructor TLexer.Destroy;
begin
  inherited Destroy;
end;

function TLexer.CurrentChar: Char;
begin
  if FPos <= Length(FSource) then
    Result := FSource[FPos]
  else
    Result := #0;
end;

function TLexer.PeekChar: Char;
begin
  if FPos + 1 <= Length(FSource) then
    Result := FSource[FPos + 1]
  else
    Result := #0;
end;

procedure TLexer.Advance;
begin
  if FPos <= Length(FSource) then
  begin
    if FSource[FPos] = #10 then
    begin
      Inc(FLine);
      FCol := 1;
    end
    else
      Inc(FCol);
    Inc(FPos);
  end;
end;

function TLexer.IsAtEnd: Boolean;
begin
  Result := FPos > Length(FSource);
end;

procedure TLexer.SkipWhitespace;
begin
  while (not IsAtEnd) and (CurrentChar in [' ', #9, #13, #10]) do
    Advance;
end;

procedure TLexer.SkipLineComment;
begin
  // Wir stehen auf dem ersten '/'
  Advance; // zweites '/'
  Advance;
  while (not IsAtEnd) and (CurrentChar <> #10) do
    Advance;
end;

procedure TLexer.SkipBlockComment;
var
  startLine, startCol: Integer;
begin
  startLine := FLine;
  startCol := FCol;
  Advance; // '*'
  Advance;
  while not IsAtEnd do
  begin
    if (CurrentChar = '*') and (PeekChar = '/') then
    begin
      Advance; // '*'
      Advance; // '/'
      Exit;
    end;
    Advance;
  end;
  FDiag.Error('unterminated block comment',
    MakeSpan(startLine, startCol, 2, FFileName));
end;

function TLexer.MakeToken(kind: TTokenKind; const value: string;
  startLine, startCol, len: Integer): TToken;
begin
  Result.Kind := kind;
  Result.Value := value;
  Result.Span := MakeSpan(startLine, startCol, len, FFileName);
end;

function TLexer.ReadNumber: TToken;
var
  startPos, startCol: Integer;
  s: string;
  isFloat: Boolean;
  currentLen: Integer;
begin
  startPos := FPos;
  startCol := FCol;
  isFloat := False;

  while (not IsAtEnd) and (CurrentChar in ['0'..'9']) do
    Advance;

  // Prüfen auf Dezimalpunkt für Float-Literale
  if (not IsAtEnd) and (CurrentChar = '.') then
  begin
    // Prüfen, ob nach dem Punkt eine Ziffer kommt, sonst ist es ein Dot-Token
    if (FPos + 1 <= Length(FSource)) and (FSource[FPos + 1] in ['0'..'9']) then
    begin
      isFloat := True;
      Advance; // Punkt überspringen
      while (not IsAtEnd) and (CurrentChar in ['0'..'9']) do
        Advance;
    end;
  end;

  currentLen := FPos - startPos;
  s := Copy(FSource, startPos, currentLen);

  if isFloat then
    Result := MakeToken(tkFloatLit, s, FLine, startCol, currentLen)
  else
    Result := MakeToken(tkIntLit, s, FLine, startCol, currentLen);
end;

function TLexer.ReadString: TToken;
var
  startLine, startCol: Integer;
  s: string;
  c: Char;
begin
  startLine := FLine;
  startCol := FCol;
  Advance; // öffnendes "
  s := '';
  while (not IsAtEnd) and (CurrentChar <> '"') do
  begin
    if CurrentChar = #10 then
    begin
      FDiag.Error('unterminated string literal',
        MakeSpan(startLine, startCol, 1, FFileName));
      Result := MakeToken(tkError, '', startLine, startCol, 1);
      Exit;
    end;
    if CurrentChar = '\' then
    begin
      Advance;
      if IsAtEnd then
      begin
        FDiag.Error('unexpected end of file in string escape',
          MakeSpan(FLine, FCol, 1, FFileName));
        Result := MakeToken(tkError, '', startLine, startCol, 1);
        Exit;
      end;
      c := CurrentChar;
      case c of
        'n':  s := s + #10;
        'r':  s := s + #13;
        't':  s := s + #9;
        '\':  s := s + '\';
        '"':  s := s + '"';
        '0':  s := s + #0;
      else
        FDiag.Error('unknown escape sequence: \' + c,
          MakeSpan(FLine, FCol - 1, 2, FFileName));
      end;
      Advance;
    end
    else
    begin
      s := s + CurrentChar;
      Advance;
    end;
  end;
  if IsAtEnd then
  begin
    FDiag.Error('unterminated string literal',
      MakeSpan(startLine, startCol, 1, FFileName));
    Result := MakeToken(tkError, '', startLine, startCol, 1);
    Exit;
  end;
  Advance; // schließendes "
  Result := MakeToken(tkStrLit, s, startLine, startCol,
    FCol - startCol);
end;

function TLexer.ReadCharLit: TToken;
var
  startLine, startCol: Integer;
  ch: string;
  c: Char;
begin
  startLine := FLine;
  startCol := FCol;
  Advance; // öffnendes '
  ch := '';
  if IsAtEnd then
  begin
    FDiag.Error('unterminated char literal',
      MakeSpan(startLine, startCol, 1, FFileName));
    Result := MakeToken(tkError, '', startLine, startCol, 1);
    Exit;
  end;
  if CurrentChar = '\' then
  begin
    Advance;
    if IsAtEnd then
    begin
      FDiag.Error('unterminated char literal escape',
        MakeSpan(startLine, startCol, 1, FFileName));
      Result := MakeToken(tkError, '', startLine, startCol, 1);
      Exit;
    end;
    c := CurrentChar;
    case c of
      'n':  ch := #10;
      'r':  ch := #13;
      't':  ch := #9;
      '\':  ch := '\';
      '''': ch := '''';
      '0':  ch := #0;
    else
      FDiag.Error('unknown escape in char literal: \' + c,
        MakeSpan(FLine, FCol - 1, 2, FFileName));
      ch := c;
    end;
    Advance;
  end
  else
  begin
    ch := CurrentChar;
    Advance;
  end;
  if IsAtEnd or (CurrentChar <> '''') then
  begin
    FDiag.Error('unterminated char literal, expected closing quote',
      MakeSpan(startLine, startCol, 1, FFileName));
    Result := MakeToken(tkError, ch, startLine, startCol, 1);
    Exit;
  end;
  Advance; // schließendes '
  Result := MakeToken(tkCharLit, ch, startLine, startCol,
    FCol - startCol);
end;

function TLexer.LookupKeyword(const s: string): TTokenKind;
begin
  case s of
    'fn':      Result := tkFn;
    'var':     Result := tkVar;
    'let':     Result := tkLet;
    'co':      Result := tkCo;
    'con':     Result := tkCon;
    'if':      Result := tkIf;
    'else':    Result := tkElse;
    'while':   Result := tkWhile;
    'return':  Result := tkReturn;
    'true':    Result := tkTrue;
    'false':   Result := tkFalse;
    'extern':  Result := tkExtern;
    'case':    Result := tkCase;
    'switch':  Result := tkSwitch;
    'break':   Result := tkBreak;
    'default': Result := tkDefault;
    'unit':    Result := tkUnit;
    'import':  Result := tkImport;
    'pub':     Result := tkPub;
    'as':      Result := tkAs;
    'type':    Result := tkType;
    'struct':  Result := tkStruct;
    'array':   Result := tkArray;
    'for':     Result := tkFor;
    'to':      Result := tkTo;
    'downto':  Result := tkDownto;
    'do':      Result := tkDo;
    'repeat':     Result := tkRepeat;
    'until':      Result := tkUntil;
  else
    Result := tkIdent;
  end;
end;

function TLexer.ReadIdentOrKeyword: TToken;
var
  startPos, startCol: Integer;
  s: string;
  kind: TTokenKind;
begin
  startPos := FPos;
  startCol := FCol;
  while (not IsAtEnd) and
    (CurrentChar in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
    Advance;
  s := Copy(FSource, startPos, FPos - startPos);
  kind := LookupKeyword(s);
  Result := MakeToken(kind, s, FLine, startCol, Length(s));
end;

function TLexer.NextToken: TToken;
var
  startLine, startCol: Integer;
  c: Char;
begin
  if FPeeked then
  begin
    FPeeked := False;
    Result := FPeekTok;
    Exit;
  end;

  // Whitespace und Kommentare überspringen
  while True do
  begin
    SkipWhitespace;
    if IsAtEnd then
    begin
      Result := MakeToken(tkEOF, '', FLine, FCol, 0);
      Exit;
    end;
    // Kommentare
    if (CurrentChar = '/') and (PeekChar = '/') then
    begin
      SkipLineComment;
      Continue;
    end;
    if (CurrentChar = '/') and (PeekChar = '*') then
    begin
      SkipBlockComment;
      Continue;
    end;
    Break;
  end;

  startLine := FLine;
  startCol := FCol;
  c := CurrentChar;

  // Zahlen
  if c in ['0'..'9'] then
  begin
    Result := ReadNumber;
    Exit;
  end;

  // Strings
  if c = '"' then
  begin
    Result := ReadString;
    Exit;
  end;

  // Char literals
  if c = '''' then
  begin
    Result := ReadCharLit;
    Exit;
  end;

  // Identifier / Keywords
  if c in ['A'..'Z', 'a'..'z', '_'] then
  begin
    Result := ReadIdentOrKeyword;
    Exit;
  end;

  // Operatoren und Trennzeichen
  case c of
    '+': begin Advance; Result := MakeToken(tkPlus, '+', startLine, startCol, 1); end;
    '-': begin Advance; Result := MakeToken(tkMinus, '-', startLine, startCol, 1); end;
    '*': begin Advance; Result := MakeToken(tkStar, '*', startLine, startCol, 1); end;
    '%': begin Advance; Result := MakeToken(tkPercent, '%', startLine, startCol, 1); end;
    '(': begin Advance; Result := MakeToken(tkLParen, '(', startLine, startCol, 1); end;
    ')': begin Advance; Result := MakeToken(tkRParen, ')', startLine, startCol, 1); end;
    '{': begin Advance; Result := MakeToken(tkLBrace, '{', startLine, startCol, 1); end;
    '}': begin Advance; Result := MakeToken(tkRBrace, '}', startLine, startCol, 1); end;
    ',': begin Advance; Result := MakeToken(tkComma, ',', startLine, startCol, 1); end;
    ';': begin Advance; Result := MakeToken(tkSemicolon, ';', startLine, startCol, 1); end;
    '.': begin
      // handle '...' ellipsis
      if (FPos + 2 <= Length(FSource)) and (FSource[FPos + 1] = '.') and (FSource[FPos + 2] = '.') then
      begin
        // consume three dots
        Advance; Advance; Advance;
        Result := MakeToken(tkEllipsis, '...', startLine, startCol, 3);
      end
      else
      begin
        Advance; Result := MakeToken(tkDot, '.', startLine, startCol, 1);
      end;
    end;
    '@': begin Advance; Result := MakeToken(tkAt, '@', startLine, startCol, 1); end;
    '[': begin Advance; Result := MakeToken(tkLBracket, '[', startLine, startCol, 1); end;
    ']': begin Advance; Result := MakeToken(tkRBracket, ']', startLine, startCol, 1); end;

    '/': begin
      Advance;
      Result := MakeToken(tkSlash, '/', startLine, startCol, 1);
    end;

    ':': begin
      Advance;
      if (not IsAtEnd) and (CurrentChar = '=') then
      begin
        Advance;
        Result := MakeToken(tkAssign, ':=', startLine, startCol, 2);
      end
      else
        Result := MakeToken(tkColon, ':', startLine, startCol, 1);
    end;

    '=': begin
      Advance;
      if (not IsAtEnd) and (CurrentChar = '=') then
      begin
        Advance;
        Result := MakeToken(tkEq, '==', startLine, startCol, 2);
      end
      else
      begin
        FDiag.Error('unexpected ''='', did you mean '':='' or ''==''?',
          MakeSpan(startLine, startCol, 1, FFileName));
        Result := MakeToken(tkError, '=', startLine, startCol, 1);
      end;
    end;

    '!': begin
      Advance;
      if (not IsAtEnd) and (CurrentChar = '=') then
      begin
        Advance;
        Result := MakeToken(tkNeq, '!=', startLine, startCol, 2);
      end
      else
        Result := MakeToken(tkNot, '!', startLine, startCol, 1);
    end;

    '<': begin
      Advance;
      if (not IsAtEnd) and (CurrentChar = '=') then
      begin
        Advance;
        Result := MakeToken(tkLe, '<=', startLine, startCol, 2);
      end
      else
        Result := MakeToken(tkLt, '<', startLine, startCol, 1);
    end;

    '>': begin
      Advance;
      if (not IsAtEnd) and (CurrentChar = '=') then
      begin
        Advance;
        Result := MakeToken(tkGe, '>=', startLine, startCol, 2);
      end
      else
        Result := MakeToken(tkGt, '>', startLine, startCol, 1);
    end;

    '&': begin
      Advance;
      if (not IsAtEnd) and (CurrentChar = '&') then
      begin
        Advance;
        Result := MakeToken(tkAnd, '&&', startLine, startCol, 2);
      end
      else
      begin
        FDiag.Error('unexpected ''&'', did you mean ''&&''?',
          MakeSpan(startLine, startCol, 1, FFileName));
        Result := MakeToken(tkError, '&', startLine, startCol, 1);
      end;
    end;

    '|': begin
      Advance;
      if (not IsAtEnd) and (CurrentChar = '|') then
      begin
        Advance;
        Result := MakeToken(tkOr, '||', startLine, startCol, 2);
      end
      else
      begin
        FDiag.Error('unexpected ''|'', did you mean ''||''?',
          MakeSpan(startLine, startCol, 1, FFileName));
        Result := MakeToken(tkError, '|', startLine, startCol, 1);
      end;
    end;

  else
    Advance;
    FDiag.Error('unexpected character: ' + c,
      MakeSpan(startLine, startCol, 1, FFileName));
    Result := MakeToken(tkError, c, startLine, startCol, 1);
  end;
end;

function TLexer.PeekToken: TToken;
begin
  if not FPeeked then
  begin
    FPeekTok := NextToken;
    FPeeked := True;
  end;
  Result := FPeekTok;
end;

end.
