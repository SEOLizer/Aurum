{$mode objfpc}{$H+}
program test_switch;

uses
  SysUtils, Classes,
  fpcunit, testregistry, consoletestrunner,
  diag, lexer, parser, ast, sema;

type
  TSwitchTest = class(TTestCase)
  private
    FDiag: TDiagnostics;
    function ParseProgramFromFile(const path: string): TAstProgram;
  published
    procedure TestParseSwitchStructure;
  end;

function TSwitchTest.ParseProgramFromFile(const path: string): TAstProgram;
var
  sl: TStringList;
  src: string;
  lex: TLexer;
  p: TParser;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(path);
    src := sl.Text;
  finally
    sl.Free;
  end;

  FDiag := TDiagnostics.Create;
  lex := TLexer.Create(src, path, FDiag);
  try
    p := TParser.Create(lex, FDiag);
    try
      Result := p.ParseProgram;
    finally
      p.Free;
    end;
  finally
    lex.Free;
    FDiag.Free;
  end;
end;

procedure TSwitchTest.TestParseSwitchStructure;
var
  prog: TAstProgram;
  decl: TAstNode;
  f: TAstFuncDecl;
  blk: TAstBlock;
  stmt: TAstStmt;
  sw: TAstSwitch;
begin
  prog := ParseProgramFromFile('examples/syntax_highlight_examples/case_switch.au');
  try
    // Expect function classify + main -> at least 2 decls
    AssertTrue(Length(prog.Decls) >= 2);

    // Find classify function
    f := nil;
    for decl in prog.Decls do
      if (decl is TAstFuncDecl) and (TAstFuncDecl(decl).Name = 'classify') then
      begin
        f := TAstFuncDecl(decl);
        Break;
      end;
    AssertTrue(Assigned(f));

    blk := f.Body;
    AssertTrue(Assigned(blk));
    // first stmt should be switch
    AssertTrue(Length(blk.Stmts) >= 1);
    stmt := blk.Stmts[0];
    AssertTrue(stmt is TAstSwitch);
    sw := TAstSwitch(stmt);
    // Expect at least two cases and a default
    AssertTrue(Length(sw.Cases) = 2);
    AssertTrue(Assigned(sw.Default));
  finally
    prog.Free;
  end;
end;

var
  app: TTestRunner;
begin
  RegisterTest(TSwitchTest);
  app := TTestRunner.Create(nil);
  try
    app.Run;
  finally
    app.Free;
  end;
end.
