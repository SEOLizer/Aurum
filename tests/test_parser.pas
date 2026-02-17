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
    // Neue Tests (Phase 2)
    procedure TestParseUnitDecl;
    procedure TestParseImportDecl;
    procedure TestParsePubFn;
    procedure TestParseForLoopTo;
    procedure TestParseForLoopDownto;
    procedure TestParseRepeatUntil;
    procedure TestParseCharLiteral;
    procedure TestParseFieldAccess;
    procedure TestParseIndexAccess;
    procedure TestParseExternDecl; // new
  end;

function TParserTest.ParseProgramFromSource(const src: string): TAstProgram;
var
  lex: TLexer;
  p: TParser;
begin
  FDiag := TDiagnostics.Create;
  lex := TLexer.Create(src, 'test.lyx', FDiag);
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

procedure TParserTest.TestParseExternDecl;
var
  prog: TAstProgram;
  d: TAstNode;
  f: TAstFuncDecl;
begin
  prog := ParseProgramFromSource('extern fn puts(s: pchar): void; fn main(): int64 { puts("hi"); return 0; }');
  try
    // Expect two declarations: extern puts, and main
    AssertTrue(Length(prog.Decls) >= 2);
    // first decl should be extern func or second depending on ordering
    d := prog.Decls[0];
    if d is TAstFuncDecl then
    begin
      f := TAstFuncDecl(d);
      // ensure extern flag possibly set
      // find any func named 'puts'
      if f.Name <> 'puts' then
      begin
        // scan for puts
        for d in prog.Decls do
          if (d is TAstFuncDecl) and (TAstFuncDecl(d).Name = 'puts') then
          begin
            f := TAstFuncDecl(d); Break;
          end;
      end;
      AssertEquals('puts', f.Name);
      AssertTrue(f.IsExtern);
      AssertTrue(f.Body = nil);
    end;
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

// --- Neue Tests (Phase 2) ---

procedure TParserTest.TestParseUnitDecl;
var
  prog: TAstProgram;
  u: TAstUnitDecl;
begin
  prog := ParseProgramFromSource('unit foo; fn main(): int64 { return 0; }');
  try
    AssertEquals(2, Length(prog.Decls));
    AssertTrue(prog.Decls[0] is TAstUnitDecl);
    u := TAstUnitDecl(prog.Decls[0]);
    AssertEquals('foo', u.UnitPath);
  finally
    prog.Free;
  end;
end;

procedure TParserTest.TestParseImportDecl;
var
  prog: TAstProgram;
  imp: TAstImportDecl;
begin
  // Import uses identifier paths (not string literals)
  prog := ParseProgramFromSource('import std.io; fn main(): int64 { return 0; }');
  try
    AssertEquals(2, Length(prog.Decls));
    AssertTrue(prog.Decls[0] is TAstImportDecl);
    imp := TAstImportDecl(prog.Decls[0]);
    AssertEquals('std.io', imp.UnitPath);
    AssertEquals('', imp.Alias);
  finally
    prog.Free;
  end;
end;

procedure TParserTest.TestParsePubFn;
var
  prog: TAstProgram;
  f: TAstFuncDecl;
begin
  // Test that pub fn parses (IsPublic not stored in AST yet)
  prog := ParseProgramFromSource('pub fn main(): int64 { return 0; }');
  try
    AssertEquals(1, Length(prog.Decls));
    AssertTrue(prog.Decls[0] is TAstFuncDecl);
    f := TAstFuncDecl(prog.Decls[0]);
    AssertEquals('main', f.Name);
    // Note: IsPublic is parsed but not stored in AST yet
  finally
    prog.Free;
  end;
end;

procedure TParserTest.TestParseForLoopTo;
var
  prog: TAstProgram;
  f: TAstFuncDecl;
  forStmt: TAstFor;
  blk: TAstBlock;
begin
  prog := ParseProgramFromSource('fn main(): int64 { for i := 0 to 5 do { print_int(i); } return 0; }');
  try
    f := TAstFuncDecl(prog.Decls[0]);
    blk := f.Body;
    AssertTrue(blk.Stmts[0] is TAstFor);
    forStmt := TAstFor(blk.Stmts[0]);
    AssertEquals('i', forStmt.VarName);
    AssertFalse(forStmt.IsDownto);
    AssertTrue(forStmt.StartExpr is TAstIntLit);
    AssertTrue(forStmt.EndExpr is TAstIntLit);
    AssertTrue(forStmt.Body is TAstBlock);
  finally
    prog.Free;
  end;
end;

procedure TParserTest.TestParseForLoopDownto;
var
  prog: TAstProgram;
  f: TAstFuncDecl;
  forStmt: TAstFor;
  blk: TAstBlock;
begin
  prog := ParseProgramFromSource('fn main(): int64 { for i := 10 downto 1 do print_int(i); return 0; }');
  try
    f := TAstFuncDecl(prog.Decls[0]);
    blk := f.Body;
    AssertTrue(blk.Stmts[0] is TAstFor);
    forStmt := TAstFor(blk.Stmts[0]);
    AssertEquals('i', forStmt.VarName);
    AssertTrue(forStmt.IsDownto);
  finally
    prog.Free;
  end;
end;

procedure TParserTest.TestParseRepeatUntil;
var
  prog: TAstProgram;
  f: TAstFuncDecl;
  repeatStmt: TAstRepeatUntil;
  blk: TAstBlock;
begin
  prog := ParseProgramFromSource('fn main(): int64 { var x: int64 := 0; repeat { x := x + 1; } until x > 5; return x; }');
  try
    f := TAstFuncDecl(prog.Decls[0]);
    blk := f.Body;
    AssertTrue(blk.Stmts[1] is TAstRepeatUntil);
    repeatStmt := TAstRepeatUntil(blk.Stmts[1]);
    AssertTrue(repeatStmt.Body is TAstBlock);
    AssertTrue(repeatStmt.Cond is TAstBinOp);
  finally
    prog.Free;
  end;
end;

procedure TParserTest.TestParseCharLiteral;
var
  prog: TAstProgram;
  f: TAstFuncDecl;
  ret: TAstReturn;
  charLit: TAstCharLit;
  blk: TAstBlock;
begin
  prog := ParseProgramFromSource('fn main(): int64 { return ''A''; }');
  try
    f := TAstFuncDecl(prog.Decls[0]);
    blk := f.Body;
    AssertTrue(blk.Stmts[0] is TAstReturn);
    ret := TAstReturn(blk.Stmts[0]);
    AssertTrue(ret.Value is TAstCharLit);
    charLit := TAstCharLit(ret.Value);
    AssertEquals('A', charLit.Value);
  finally
    prog.Free;
  end;
end;

procedure TParserTest.TestParseFieldAccess;
var
  prog: TAstProgram;
  f: TAstFuncDecl;
  exprStmt: TAstExprStmt;
  fieldAcc: TAstFieldAccess;
  blk: TAstBlock;
  ident: TAstIdent;
begin
  prog := ParseProgramFromSource('fn main(): int64 { print_int(obj.field); return 0; }');
  try
    f := TAstFuncDecl(prog.Decls[0]);
    blk := f.Body;
    AssertTrue(blk.Stmts[0] is TAstExprStmt);
    exprStmt := TAstExprStmt(blk.Stmts[0]);
    AssertTrue(exprStmt.Expr is TAstCall);
    // first arg is field access
    AssertTrue(TAstCall(exprStmt.Expr).Args[0] is TAstFieldAccess);
    fieldAcc := TAstFieldAccess(TAstCall(exprStmt.Expr).Args[0]);
    AssertTrue(fieldAcc.Obj is TAstIdent);
    ident := TAstIdent(fieldAcc.Obj);
    AssertEquals('obj', ident.Name);
    AssertEquals('field', fieldAcc.Field);
  finally
    prog.Free;
  end;
end;

procedure TParserTest.TestParseIndexAccess;
var
  prog: TAstProgram;
  f: TAstFuncDecl;
  exprStmt: TAstExprStmt;
  idxAcc: TAstIndexAccess;
  blk: TAstBlock;
  ident: TAstIdent;
begin
  prog := ParseProgramFromSource('fn main(): int64 { print_int(arr[0]); return 0; }');
  try
    f := TAstFuncDecl(prog.Decls[0]);
    blk := f.Body;
    AssertTrue(blk.Stmts[0] is TAstExprStmt);
    exprStmt := TAstExprStmt(blk.Stmts[0]);
    AssertTrue(exprStmt.Expr is TAstCall);
    // first arg is index access
    AssertTrue(TAstCall(exprStmt.Expr).Args[0] is TAstIndexAccess);
    idxAcc := TAstIndexAccess(TAstCall(exprStmt.Expr).Args[0]);
    AssertTrue(idxAcc.Obj is TAstIdent);
    ident := TAstIdent(idxAcc.Obj);
    AssertEquals('arr', ident.Name);
    AssertTrue(idxAcc.Index is TAstIntLit);
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
