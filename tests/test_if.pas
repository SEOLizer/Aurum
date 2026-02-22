{$mode objfpc}{$H+}
program test_if;

uses
  SysUtils,
  Classes,
  fpcunit, testregistry, consoletestrunner,
  diag, lexer, parser, ast;

type
  TIfTest = class(TTestCase)
  private
    FDiag: TDiagnostics;
    function ParseProgramFromFile(const path: string): TAstProgram;
  published
    procedure TestIfParsingAndStructure;
  end;

function TIfTest.ParseProgramFromFile(const path: string): TAstProgram;
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

procedure TIfTest.TestIfParsingAndStructure;
var
  prog: TAstProgram;
  decl: TAstNode;
  f: TAstFuncDecl;
  blk: TAstBlock;
  stmt: TAstStmt;
  if1, if2, if3: TAstIf;
begin
  prog := ParseProgramFromFile('examples/if_test.lyx');
  try
    // Erwartet: eine fn main plus con NL -> zwei Decls
    AssertTrue(Length(prog.Decls) >= 2);
    WriteLn('CHK: decls >=2');

    // Finde main-Funktion
    f := nil;
    for decl in prog.Decls do
      if (decl is TAstFuncDecl) and (TAstFuncDecl(decl).Name = 'main') then
      begin
        f := TAstFuncDecl(decl);
        Break;
      end;
    AssertTrue(Assigned(f));
    WriteLn('CHK: found main');

    blk := f.Body;
    AssertTrue(Assigned(blk));

    // Stmts: [let x..., if1, if2, if3, return]
    AssertTrue(Length(blk.Stmts) >= 4);
    WriteLn('CHK: stmts >=4');

    // Erste If ist an Position 1
    WriteLn('CHK: checking if1');
    stmt := blk.Stmts[1];
    AssertTrue(stmt is TAstIf);
    if1 := TAstIf(stmt);
    // Bedingung: x > 5 -> BinOp mit Op = tkGt
    AssertTrue(if1.Cond is TAstBinOp);
    AssertTrue(TAstBinOp(if1.Cond).Op = tkGt);
    // ThenBranch sollte ein Block mit einem Call PrintStr
    AssertTrue(if1.ThenBranch is TAstBlock);

    // Zweite If (Position 2) prüft &&
    WriteLn('CHK: checking if2');
    stmt := blk.Stmts[2];
    AssertTrue(stmt is TAstIf);
    if2 := TAstIf(stmt);
    AssertTrue(if2.Cond is TAstBinOp);
    AssertTrue(TAstBinOp(if2.Cond).Op = tkAnd);

    // Dritte If (Position 3) ist verschachtelt
    WriteLn('CHK: checking if3');
    stmt := blk.Stmts[3];
    AssertTrue(stmt is TAstIf);
    if3 := TAstIf(stmt);
    // Outer cond: x == 0 -> tkEq
    AssertTrue(if3.Cond is TAstBinOp);
    AssertTrue(TAstBinOp(if3.Cond).Op = tkEq);
    // ElseBranch sollte entweder direkt ein TAstIf sein oder ein Block mit einem If als erstem Statement
    AssertTrue(Assigned(if3.ElseBranch));
    if if3.ElseBranch is TAstIf then
    begin
      AssertTrue(TAstIf(if3.ElseBranch).Cond is TAstBinOp);
      AssertTrue(TAstBinOp(TAstIf(if3.ElseBranch).Cond).Op = tkEq);
    end
    else
    begin
      AssertTrue(if3.ElseBranch is TAstBlock);
      // Block muss mindestens ein Statement haben und das erste muss ein If sein
      AssertTrue(Length(TAstBlock(if3.ElseBranch).Stmts) >= 1);
      AssertTrue(TAstBlock(if3.ElseBranch).Stmts[0] is TAstIf);
      AssertTrue(TAstIf(TAstBlock(if3.ElseBranch).Stmts[0]).Cond is TAstBinOp);
      AssertTrue(TAstBinOp(TAstIf(TAstBlock(if3.ElseBranch).Stmts[0]).Cond).Op = tkEq);
    end

  finally
    prog.Free;
  end;
end;

var
  app: TTestRunner;
begin
  RegisterTest(TIfTest);
  app := TTestRunner.Create(nil);
  try
    app.Run;
  finally
    app.Free;
  end;
end.
