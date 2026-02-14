{$mode objfpc}{$H+}
program test_parser;

uses
  SysUtils,
  fpcunit, testregistry, consoletestrunner,
  diag, lexer, parser, ast;

type
  TParserTest = class(TTestCase)
  private
    FDiag: TDiagnostics;
    function ParseProgramFromSource(const src: string): TAstProgram;
  published
    procedure TestParseFunctionSimple;
    procedure TestParseVarLetCoAndAssignPrecedence;
    procedure TestParseConTopLevel;
  end;

function TParserTest.ParseProgramFromSource(const src: string): TAstProgram;
var
  lex: TLexer;
  p: TParser;
begin
  FDiag := TDiagnostics.Create;
  lex := TLexer.Create(src, 'test.au', FDiag);
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

procedure TParserTest.TestParseFunctionSimple;
var
  prog: TAstProgram;
  f: TAstFuncDecl;
  blk: TAstBlock;
  stmt: TAstStmt;
  exprStmt: TAstExprStmt;
  call: TAstCall;
begin
  prog := ParseProgramFromSource('fn main(): int64 { print_str("Hello"); return 0; }');
  try
    AssertEquals(1, Length(prog.Decls));
    AssertTrue(prog.Decls[0] is TAstFuncDecl);
    f := TAstFuncDecl(prog.Decls[0]);
    AssertEquals('main', f.Name);
    AssertTrue(f.ReturnType = atInt64);
    blk := f.Body;
    AssertTrue(Assigned(blk));
    AssertTrue(Length(blk.Stmts) >= 2);
    stmt := blk.Stmts[0];
    AssertTrue(stmt is TAstExprStmt);
    exprStmt := TAstExprStmt(stmt);
    AssertTrue(exprStmt.Expr is TAstCall);
    call := TAstCall(exprStmt.Expr);
    AssertEquals('print_str', call.Name);
    AssertEquals(1, Length(call.Args));
    AssertTrue(call.Args[0] is TAstStrLit);
    AssertEquals('Hello', TAstStrLit(call.Args[0]).Value);
  finally
    prog.Free;
  end;
end;

procedure TParserTest.TestParseVarLetCoAndAssignPrecedence;
var
  prog: TAstProgram;
  f: TAstFuncDecl;
  blk: TAstBlock;
  decl: TAstVarDecl;
  assignStmt: TAstAssign;
  bin: TAstBinOp;
begin
  prog := ParseProgramFromSource('fn main(): int64 { var i: int64 := 0; i := 1 + 2 * 3; return i; }');
  try
    AssertEquals(1, Length(prog.Decls));
    f := TAstFuncDecl(prog.Decls[0]);
    blk := f.Body;
    // first stmt is var decl
    AssertTrue(blk.Stmts[0] is TAstVarDecl);
    decl := TAstVarDecl(blk.Stmts[0]);
    AssertEquals('i', decl.Name);
    // second stmt is assign
    AssertTrue(blk.Stmts[1] is TAstAssign);
    assignStmt := TAstAssign(blk.Stmts[1]);
    // RHS should be BinOp + with nested Mul on right
    AssertTrue(assignStmt.Value is TAstBinOp);
    bin := TAstBinOp(assignStmt.Value);
    AssertTrue(bin.Op = tkPlus);
    AssertTrue(bin.Right is TAstBinOp);
    AssertTrue(TAstBinOp(bin.Right).Op = tkStar);
  finally
    prog.Free;
  end;
end;


procedure TParserTest.TestParseConTopLevel;
var
  prog: TAstProgram;
  decl: TAstNode;
  con: TAstConDecl;
begin
  prog := ParseProgramFromSource('con LIMIT: int64 := 5; fn main(): int64 { return 0; }');
  try
    AssertEquals(2, Length(prog.Decls));
    decl := prog.Decls[0];
    AssertTrue(decl is TAstConDecl);
    con := TAstConDecl(decl);
    AssertEquals('LIMIT', con.Name);
    AssertTrue(con.InitExpr is TAstIntLit);
    AssertEquals(5, TAstIntLit(con.InitExpr).Value);
  finally
    prog.Free;
  end;
end;

var
  app: TTestRunner;
begin
  RegisterTest(TParserTest);
  app := TTestRunner.Create(nil);
  try
    app.Run;
  finally
    app.Free;
  end;
end.
