{$mode objfpc}{$H+}
program debug_lex;
uses SysUtils, diag, lexer;
var d: TDiagnostics; l: TLexer; t: TToken;
begin
  d := TDiagnostics.Create;
  l := TLexer.Create('fn main(): int64 { var i: int64 := 0; if (i < 10) { i := i + 1; } while (i < 5) { i := i + 1; } return 0; }', 'test.au', d);
  try
    repeat
      t := l.NextToken;
      WriteLn(TokenKindToStr(t.Kind), ' "', t.Value, '" at ', t.Span.Line, ':', t.Span.Col);
    until t.Kind = tkEOF;
  finally
    l.Free;
    d.Free;
  end;
end.