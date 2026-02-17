{$mode objfpc}{$H+}
program test_lexer;

uses
  SysUtils,
  fpcunit, testregistry, consoletestrunner,
  diag, lexer;

type
  TLexerTest = class(TTestCase)
  private
    FDiag: TDiagnostics;
    function Lex(const src: string): TLexer;
    procedure AssertToken(tok: TToken; expectedKind: TTokenKind;
      const expectedValue: string);
    procedure AssertTokenKind(tok: TToken; expectedKind: TTokenKind);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Einzelne Tokens
    procedure TestIntLiteral;
    procedure TestIntLiteralZero;
    procedure TestStringLiteral;
    procedure TestStringEscapes;
    procedure TestStringEscapeNull;
    procedure TestIdentifier;
    procedure TestIdentifierUnderscore;
    // Keywords
    procedure TestKeywordFn;
    procedure TestKeywordVar;
    procedure TestKeywordLet;
    procedure TestKeywordCo;
    procedure TestKeywordCon;
    procedure TestKeywordIf;
    procedure TestKeywordElse;
    procedure TestKeywordWhile;
    procedure TestKeywordReturn;
    procedure TestKeywordTrue;
    procedure TestKeywordFalse;
    procedure TestKeywordExtern;
    // Neue Keywords (Phase 2)
    procedure TestKeywordUnit;
    procedure TestKeywordImport;
    procedure TestKeywordPub;
    procedure TestKeywordAs;
    procedure TestKeywordType;
    procedure TestKeywordStruct;
    procedure TestKeywordFor;
    procedure TestKeywordTo;
    procedure TestKeywordDownto;
    procedure TestKeywordDo;
    procedure TestKeywordRepeat;
    procedure TestKeywordUntil;
    // Char-Literal
    procedure TestCharLiteral;
    procedure TestCharLiteralEscape;
    // Neue Delimiter
    procedure TestNewDelimiters;
    // Operatoren
    procedure TestOperatorsArith;
    procedure TestOperatorAssign;
    procedure TestOperatorsCompare;
    procedure TestOperatorsLogic;
    // Trennzeichen
    procedure TestDelimiters;
    // Kommentare
    procedure TestLineComment;
    procedure TestBlockComment;
    // Zusammengesetzte Sequenzen
    procedure TestFunctionDecl;
    procedure TestVarDeclAssign;
    // SourceSpan
    procedure TestSpanLineCol;
    procedure TestSpanMultiLine;
    // PeekToken
    procedure TestPeekToken;
    // Fehlerfälle
    procedure TestUnterminatedString;
    procedure TestUnknownChar;
    procedure TestSingleEquals;
    procedure TestSingleAmpersand;
    procedure TestSinglePipe;
    // EOF
    procedure TestEmptySource;
  end;

procedure TLexerTest.SetUp;
begin
  FDiag := TDiagnostics.Create;
end;

procedure TLexerTest.TearDown;
begin
  FDiag.Free;
end;

function TLexerTest.Lex(const src: string): TLexer;
begin
  Result := TLexer.Create(src, 'test.lyx', FDiag);
end;

procedure TLexerTest.AssertToken(tok: TToken; expectedKind: TTokenKind;
  const expectedValue: string);
begin
  AssertTrue('Erwartet ' + TokenKindToStr(expectedKind) + ', bekommen ' +
    TokenKindToStr(tok.Kind), tok.Kind = expectedKind);
  AssertEquals(expectedValue, tok.Value);
end;

procedure TLexerTest.AssertTokenKind(tok: TToken; expectedKind: TTokenKind);
begin
  AssertTrue('Erwartet ' + TokenKindToStr(expectedKind) + ', bekommen ' +
    TokenKindToStr(tok.Kind), tok.Kind = expectedKind);
end;

// --- Einzelne Tokens ---

procedure TLexerTest.TestIntLiteral;
var
  l: TLexer;
begin
  l := Lex('42');
  try
    AssertToken(l.NextToken, tkIntLit, '42');
    AssertTokenKind(l.NextToken, tkEOF);
  finally
    l.Free;
  end;
end;

procedure TLexerTest.TestIntLiteralZero;
var
  l: TLexer;
begin
  l := Lex('0');
  try
    AssertToken(l.NextToken, tkIntLit, '0');
  finally
    l.Free;
  end;
end;

procedure TLexerTest.TestStringLiteral;
var
  l: TLexer;
  t: TToken;
begin
  l := Lex('"hello"');
  try
    t := l.NextToken;
    AssertTokenKind(t, tkStrLit);
    AssertEquals('hello', t.Value);
  finally
    l.Free;
  end;
end;

procedure TLexerTest.TestStringEscapes;
var
  l: TLexer;
  t: TToken;
begin
  l := Lex('"a\nb\tc\\d\"e"');
  try
    t := l.NextToken;
    AssertTokenKind(t, tkStrLit);
    // Erwartung: a + LF + b + TAB + c + \ + d + " + e
    AssertEquals('a' + #10 + 'b' + #9 + 'c\d"e', t.Value);
  finally
    l.Free;
  end;
end;

procedure TLexerTest.TestStringEscapeNull;
var
  l: TLexer;
  t: TToken;
begin
  l := Lex('"x\0y"');
  try
    t := l.NextToken;
    AssertTokenKind(t, tkStrLit);
    AssertEquals(3, Length(t.Value));
    AssertEquals('x', t.Value[1]);
    AssertEquals(#0, t.Value[2]);
    AssertEquals('y', t.Value[3]);
  finally
    l.Free;
  end;
end;

procedure TLexerTest.TestIdentifier;
var
  l: TLexer;
begin
  l := Lex('myVar');
  try
    AssertToken(l.NextToken, tkIdent, 'myVar');
  finally
    l.Free;
  end;
end;

procedure TLexerTest.TestIdentifierUnderscore;
var
  l: TLexer;
begin
  l := Lex('_foo_bar2');
  try
    AssertToken(l.NextToken, tkIdent, '_foo_bar2');
  finally
    l.Free;
  end;
end;

// --- Keywords ---

procedure TLexerTest.TestKeywordFn;
var l: TLexer;
begin
  l := Lex('fn'); try AssertToken(l.NextToken, tkFn, 'fn'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordVar;
var l: TLexer;
begin
  l := Lex('var'); try AssertToken(l.NextToken, tkVar, 'var'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordLet;
var l: TLexer;
begin
  l := Lex('let'); try AssertToken(l.NextToken, tkLet, 'let'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordCo;
var l: TLexer;
begin
  l := Lex('co'); try AssertToken(l.NextToken, tkCo, 'co'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordCon;
var l: TLexer;
begin
  l := Lex('con'); try AssertToken(l.NextToken, tkCon, 'con'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordIf;
var l: TLexer;
begin
  l := Lex('if'); try AssertToken(l.NextToken, tkIf, 'if'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordElse;
var l: TLexer;
begin
  l := Lex('else'); try AssertToken(l.NextToken, tkElse, 'else'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordWhile;
var l: TLexer;
begin
  l := Lex('while'); try AssertToken(l.NextToken, tkWhile, 'while'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordReturn;
var l: TLexer;
begin
  l := Lex('return'); try AssertToken(l.NextToken, tkReturn, 'return'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordTrue;
var l: TLexer;
begin
  l := Lex('true'); try AssertToken(l.NextToken, tkTrue, 'true'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordFalse;
var l: TLexer;
begin
  l := Lex('false'); try AssertToken(l.NextToken, tkFalse, 'false'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordExtern;
var l: TLexer;
begin
  l := Lex('extern'); try AssertToken(l.NextToken, tkExtern, 'extern'); finally l.Free; end;
end;

// --- Neue Keywords (Phase 2) ---

procedure TLexerTest.TestKeywordUnit;
var l: TLexer;
begin
  l := Lex('unit'); try AssertToken(l.NextToken, tkUnit, 'unit'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordImport;
var l: TLexer;
begin
  l := Lex('import'); try AssertToken(l.NextToken, tkImport, 'import'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordPub;
var l: TLexer;
begin
  l := Lex('pub'); try AssertToken(l.NextToken, tkPub, 'pub'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordAs;
var l: TLexer;
begin
  l := Lex('as'); try AssertToken(l.NextToken, tkAs, 'as'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordType;
var l: TLexer;
begin
  l := Lex('type'); try AssertToken(l.NextToken, tkType, 'type'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordStruct;
var l: TLexer;
begin
  l := Lex('struct'); try AssertToken(l.NextToken, tkStruct, 'struct'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordFor;
var l: TLexer;
begin
  l := Lex('for'); try AssertToken(l.NextToken, tkFor, 'for'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordTo;
var l: TLexer;
begin
  l := Lex('to'); try AssertToken(l.NextToken, tkTo, 'to'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordDownto;
var l: TLexer;
begin
  l := Lex('downto'); try AssertToken(l.NextToken, tkDownto, 'downto'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordDo;
var l: TLexer;
begin
  l := Lex('do'); try AssertToken(l.NextToken, tkDo, 'do'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordRepeat;
var l: TLexer;
begin
  l := Lex('repeat'); try AssertToken(l.NextToken, tkRepeat, 'repeat'); finally l.Free; end;
end;

procedure TLexerTest.TestKeywordUntil;
var l: TLexer;
begin
  l := Lex('until'); try AssertToken(l.NextToken, tkUntil, 'until'); finally l.Free; end;
end;

// --- Char-Literal ---

procedure TLexerTest.TestCharLiteral;
var
  l: TLexer;
  t: TToken;
begin
  l := Lex('''a''');
  try
    t := l.NextToken;
    AssertTokenKind(t, tkCharLit);
    AssertEquals('a', t.Value);
  finally
    l.Free;
  end;
end;

procedure TLexerTest.TestCharLiteralEscape;
var
  l: TLexer;
  t: TToken;
begin
  l := Lex('''\n''');
  try
    t := l.NextToken;
    AssertTokenKind(t, tkCharLit);
    AssertEquals(#10, t.Value);
  finally
    l.Free;
  end;
end;

// --- Neue Delimiter ---

procedure TLexerTest.TestNewDelimiters;
var
  l: TLexer;
begin
  l := Lex('[ ] . @');
  try
    AssertTokenKind(l.NextToken, tkLBracket);
    AssertTokenKind(l.NextToken, tkRBracket);
    AssertTokenKind(l.NextToken, tkDot);
    AssertTokenKind(l.NextToken, tkAt);
    AssertTokenKind(l.NextToken, tkEOF);
  finally
    l.Free;
  end;
end;

// --- Operatoren ---

procedure TLexerTest.TestOperatorsArith;
var
  l: TLexer;
begin
  l := Lex('+ - * / %');
  try
    AssertTokenKind(l.NextToken, tkPlus);
    AssertTokenKind(l.NextToken, tkMinus);
    AssertTokenKind(l.NextToken, tkStar);
    AssertTokenKind(l.NextToken, tkSlash);
    AssertTokenKind(l.NextToken, tkPercent);
    AssertTokenKind(l.NextToken, tkEOF);
  finally
    l.Free;
  end;
end;

procedure TLexerTest.TestOperatorAssign;
var
  l: TLexer;
begin
  l := Lex(':=');
  try
    AssertToken(l.NextToken, tkAssign, ':=');
  finally
    l.Free;
  end;
end;

procedure TLexerTest.TestOperatorsCompare;
var
  l: TLexer;
begin
  l := Lex('== != < <= > >=');
  try
    AssertTokenKind(l.NextToken, tkEq);
    AssertTokenKind(l.NextToken, tkNeq);
    AssertTokenKind(l.NextToken, tkLt);
    AssertTokenKind(l.NextToken, tkLe);
    AssertTokenKind(l.NextToken, tkGt);
    AssertTokenKind(l.NextToken, tkGe);
    AssertTokenKind(l.NextToken, tkEOF);
  finally
    l.Free;
  end;
end;

procedure TLexerTest.TestOperatorsLogic;
var
  l: TLexer;
begin
  l := Lex('&& || !');
  try
    AssertTokenKind(l.NextToken, tkAnd);
    AssertTokenKind(l.NextToken, tkOr);
    AssertTokenKind(l.NextToken, tkNot);
    AssertTokenKind(l.NextToken, tkEOF);
  finally
    l.Free;
  end;
end;

// --- Trennzeichen ---

procedure TLexerTest.TestDelimiters;
var
  l: TLexer;
begin
  l := Lex('( ) { } : , ;');
  try
    AssertTokenKind(l.NextToken, tkLParen);
    AssertTokenKind(l.NextToken, tkRParen);
    AssertTokenKind(l.NextToken, tkLBrace);
    AssertTokenKind(l.NextToken, tkRBrace);
    AssertTokenKind(l.NextToken, tkColon);
    AssertTokenKind(l.NextToken, tkComma);
    AssertTokenKind(l.NextToken, tkSemicolon);
    AssertTokenKind(l.NextToken, tkEOF);
  finally
    l.Free;
  end;
end;

// --- Kommentare ---

procedure TLexerTest.TestLineComment;
var
  l: TLexer;
begin
  l := Lex('42 // das ist ein Kommentar' + #10 + '7');
  try
    AssertToken(l.NextToken, tkIntLit, '42');
    AssertToken(l.NextToken, tkIntLit, '7');
    AssertTokenKind(l.NextToken, tkEOF);
  finally
    l.Free;
  end;
end;

procedure TLexerTest.TestBlockComment;
var
  l: TLexer;
begin
  l := Lex('42 /* kommentar */ 7');
  try
    AssertToken(l.NextToken, tkIntLit, '42');
    AssertToken(l.NextToken, tkIntLit, '7');
    AssertTokenKind(l.NextToken, tkEOF);
  finally
    l.Free;
  end;
end;

// --- Zusammengesetzte Sequenzen ---

procedure TLexerTest.TestFunctionDecl;
var
  l: TLexer;
begin
  l := Lex('fn main(): int64 {');
  try
    AssertTokenKind(l.NextToken, tkFn);
    AssertToken(l.NextToken, tkIdent, 'main');
    AssertTokenKind(l.NextToken, tkLParen);
    AssertTokenKind(l.NextToken, tkRParen);
    AssertTokenKind(l.NextToken, tkColon);
    AssertToken(l.NextToken, tkIdent, 'int64');
    AssertTokenKind(l.NextToken, tkLBrace);
  finally
    l.Free;
  end;
end;

procedure TLexerTest.TestVarDeclAssign;
var
  l: TLexer;
begin
  l := Lex('var x: int64 := 42;');
  try
    AssertTokenKind(l.NextToken, tkVar);
    AssertToken(l.NextToken, tkIdent, 'x');
    AssertTokenKind(l.NextToken, tkColon);
    AssertToken(l.NextToken, tkIdent, 'int64');
    AssertTokenKind(l.NextToken, tkAssign);
    AssertToken(l.NextToken, tkIntLit, '42');
    AssertTokenKind(l.NextToken, tkSemicolon);
  finally
    l.Free;
  end;
end;

// --- SourceSpan ---

procedure TLexerTest.TestSpanLineCol;
var
  l: TLexer;
  t: TToken;
begin
  l := Lex('  fn');
  try
    t := l.NextToken;
    AssertEquals(1, t.Span.Line);
    AssertEquals(3, t.Span.Col);
    AssertEquals(2, t.Span.Len);
  finally
    l.Free;
  end;
end;

procedure TLexerTest.TestSpanMultiLine;
var
  l: TLexer;
  t: TToken;
begin
  l := Lex('x' + #10 + '  y');
  try
    t := l.NextToken; // x auf Zeile 1
    AssertEquals(1, t.Span.Line);
    AssertEquals(1, t.Span.Col);
    t := l.NextToken; // y auf Zeile 2, Spalte 3
    AssertEquals(2, t.Span.Line);
    AssertEquals(3, t.Span.Col);
  finally
    l.Free;
  end;
end;

// --- PeekToken ---

procedure TLexerTest.TestPeekToken;
var
  l: TLexer;
  t1, t2: TToken;
begin
  l := Lex('fn main');
  try
    t1 := l.PeekToken;
    AssertTokenKind(t1, tkFn);
    t2 := l.NextToken; // Muss dasselbe Token zurückgeben
    AssertTokenKind(t2, tkFn);
    AssertToken(l.NextToken, tkIdent, 'main');
  finally
    l.Free;
  end;
end;

// --- Fehlerfälle ---

procedure TLexerTest.TestUnterminatedString;
var
  l: TLexer;
begin
  l := Lex('"unterminated');
  try
    l.NextToken;
    AssertTrue('Fehler erwartet', FDiag.HasErrors);
  finally
    l.Free;
  end;
end;

procedure TLexerTest.TestUnknownChar;
var
  l: TLexer;
begin
  l := Lex('§');
  try
    l.NextToken;
    AssertTrue('Fehler erwartet', FDiag.HasErrors);
  finally
    l.Free;
  end;
end;

procedure TLexerTest.TestSingleEquals;
var
  l: TLexer;
begin
  l := Lex('=');
  try
    l.NextToken;
    AssertTrue('Fehler für einzelnes = erwartet', FDiag.HasErrors);
  finally
    l.Free;
  end;
end;

procedure TLexerTest.TestSingleAmpersand;
var
  l: TLexer;
begin
  l := Lex('&');
  try
    l.NextToken;
    AssertTrue('Fehler für einzelnes & erwartet', FDiag.HasErrors);
  finally
    l.Free;
  end;
end;

procedure TLexerTest.TestSinglePipe;
var
  l: TLexer;
begin
  l := Lex('|');
  try
    l.NextToken;
    AssertTrue('Fehler für einzelnes | erwartet', FDiag.HasErrors);
  finally
    l.Free;
  end;
end;

// --- EOF ---

procedure TLexerTest.TestEmptySource;
var
  l: TLexer;
begin
  l := Lex('');
  try
    AssertTokenKind(l.NextToken, tkEOF);
    AssertTokenKind(l.NextToken, tkEOF); // Wiederholtes EOF
  finally
    l.Free;
  end;
end;

var
  app: TTestRunner;
begin
  RegisterTest(TLexerTest);
  app := TTestRunner.Create(nil);
  try
    app.Run;
  finally
    app.Free;
  end;
end.
