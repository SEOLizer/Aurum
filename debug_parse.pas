{$mode objfpc}{$H+}
program debug_parse;
uses SysUtils, diag, lexer, parser, ast;
var d: TDiagnostics; l: TLexer; p: TParser; prog: TAstProgram; i: Integer; fDecl: TAstFuncDecl; blk: TAstBlock; ifn: TAstIf;
begin
  d := TDiagnostics.Create;
  try
    l := TLexer.Create('fn main(): int64 { var i: int64 := 0; if (i < 10) { i := i + 1; } while (i < 5) { i := i + 1; } return 0; }', 'test.au', d);
    p := TParser.Create(l, d);
    prog := p.ParseProgram;
    WriteLn('Decls: ', Length(prog.Decls));
    if Length(prog.Decls) > 0 then
    begin
      if prog.Decls[0] is TAstFuncDecl then
      begin
        fDecl := TAstFuncDecl(prog.Decls[0]);
        WriteLn('Func: ', fDecl.Name, ' params=', Length(fDecl.Params), ' ret=', Ord(fDecl.ReturnType));
        blk := fDecl.Body;
        WriteLn('Block stmts: ', Length(blk.Stmts));
        for i := 0 to High(blk.Stmts) do
        begin
          Write('stmt[', i, ']: ', NodeKindToStr(blk.Stmts[i].Kind));
          if blk.Stmts[i] is TAstIf then
          begin
            ifn := TAstIf(blk.Stmts[i]);
            Write(' -> If.Then=', NodeKindToStr(ifn.ThenBranch.Kind));
          end;
          WriteLn('');
        end;
      end;
    end;
    // intentionally not freeing prog/p/l for quick debug
    // prog.Free;
    // p.Free;
    // l.Free;
  finally
    d.Free;
  end;
end.
