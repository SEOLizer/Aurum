{$mode objfpc}{$H+}
program test_index_assign;

uses SysUtils, Classes, fpcunit, testregistry, consoletestrunner,
     diag, lexer, parser, ast, sema, ir, lower_ast_to_ir;

type
  TIndexAssignTest = class(TTestCase)
  private
    FDiag: TDiagnostics;
    function ParseAndLower(const src, fname: string): TIRModule;
  published
    procedure TestStaticIndexAssign;
    procedure TestDynamicIndexAssign;
    procedure TestParserCreatesIndexAssignNode;
  end;

function TIndexAssignTest.ParseAndLower(const src, fname: string): TIRModule;
var
  lex: TLexer;
  p: TParser;
  prog: TAstProgram;
  s: TSema;
  lower: TIRLowering;
  modl: TIRModule;
begin
  FDiag := TDiagnostics.Create;
  lex := TLexer.Create(src, fname, FDiag);
  try
    p := TParser.Create(lex, FDiag);
    try
      prog := p.ParseProgram;
      s := TSema.Create(FDiag, nil);
      try
        s.Analyze(prog);
      finally
        s.Free;
      end;
      modl := TIRModule.Create;
      lower := TIRLowering.Create(modl, FDiag);
      try
        lower.Lower(prog);
        Result := modl;
      finally
        lower.Free;
      end;
    finally
      p.Free;
    end;
  finally
    lex.Free;
    FDiag.Free;
  end;
end;

procedure TIndexAssignTest.TestStaticIndexAssign;
var
  modl: TIRModule;
  f: TIRFunction;
  i, j: Integer;
  foundStoreElem: Boolean;
begin
  // Test: arr[0] := 42; with static index
  modl := ParseAndLower(
    'fn main(): int64 {' + LineEnding +
    '  var a: int64[3] := [1,2,3];' + LineEnding +
    '  a[0] := 42;' + LineEnding +
    '  return a[0];' + LineEnding +
    '}',
    'test_index_assign_static.au'
  );
  try
    foundStoreElem := False;
    for i := 0 to High(modl.Functions) do
    begin
      f := modl.Functions[i];
      if f.Name <> 'main' then Continue;
      for j := 0 to High(f.Instructions) do
      begin
        if f.Instructions[j].Op = irStoreElem then
        begin
          foundStoreElem := True;
          // static index should be stored in ImmInt
          AssertEquals('static index in ImmInt', 0, f.Instructions[j].ImmInt);
        end;
      end;
    end;
    AssertTrue('irStoreElem instruction expected for static index assignment', foundStoreElem);
  finally
    modl.Free;
  end;
end;

procedure TIndexAssignTest.TestDynamicIndexAssign;
var
  modl: TIRModule;
  f: TIRFunction;
  i, j: Integer;
  foundStoreElemDyn: Boolean;
begin
  // Test: arr[i] := 42; with dynamic index
  modl := ParseAndLower(
    'fn main(): int64 {' + LineEnding +
    '  var a: int64[3] := [1,2,3];' + LineEnding +
    '  var i: int64 := 1;' + LineEnding +
    '  a[i] := 42;' + LineEnding +
    '  return a[1];' + LineEnding +
    '}',
    'test_index_assign_dyn.au'
  );
  try
    foundStoreElemDyn := False;
    for i := 0 to High(modl.Functions) do
    begin
      f := modl.Functions[i];
      if f.Name <> 'main' then Continue;
      for j := 0 to High(f.Instructions) do
      begin
        if f.Instructions[j].Op = irStoreElemDyn then
        begin
          foundStoreElemDyn := True;
          // dynamic index uses Src2 for index temp
          AssertTrue('Src2 (index temp) should be >= 0', f.Instructions[j].Src2 >= 0);
          // Src3 should hold the value temp
          AssertTrue('Src3 (value temp) should be >= 0', f.Instructions[j].Src3 >= 0);
        end;
      end;
    end;
    AssertTrue('irStoreElemDyn instruction expected for dynamic index assignment', foundStoreElemDyn);
  finally
    modl.Free;
  end;
end;

procedure TIndexAssignTest.TestParserCreatesIndexAssignNode;
var
  lex: TLexer;
  p: TParser;
  prog: TAstProgram;
  d: TDiagnostics;
  fn: TAstFuncDecl;
  blk: TAstBlock;
  foundIndexAssign: Boolean;
  i: Integer;
begin
  d := TDiagnostics.Create;
  try
    lex := TLexer.Create(
      'fn test() {' + LineEnding +
      '  var x: int64[5] := [0,0,0,0,0];' + LineEnding +
      '  x[2] := 99;' + LineEnding +
      '}',
      'test_parser_index_assign.au', d);
    try
      p := TParser.Create(lex, d);
      try
        prog := p.ParseProgram;
        try
          // Find the function
          AssertTrue('Should have at least one declaration', Length(prog.Decls) > 0);
          AssertTrue('First decl should be FuncDecl', prog.Decls[0] is TAstFuncDecl);
          fn := TAstFuncDecl(prog.Decls[0]);
          blk := fn.Body;
          AssertNotNull('Function body should exist', blk);

          // Search for TAstIndexAssign in the block
          foundIndexAssign := False;
          for i := 0 to High(blk.Stmts) do
          begin
            if blk.Stmts[i] is TAstIndexAssign then
            begin
              foundIndexAssign := True;
              // Verify the TAstIndexAssign structure
              with TAstIndexAssign(blk.Stmts[i]) do
              begin
                AssertNotNull('Target should exist', Target);
                AssertNotNull('Value should exist', Value);
                AssertTrue('Target.Obj should be TAstIdent', Target.Obj is TAstIdent);
                AssertEquals('Target.Obj name should be x', 'x', TAstIdent(Target.Obj).Name);
                AssertTrue('Target.Index should be TAstIntLit', Target.Index is TAstIntLit);
                AssertEquals('Target.Index value should be 2', 2, TAstIntLit(Target.Index).Value);
                AssertTrue('Value should be TAstIntLit', Value is TAstIntLit);
                AssertEquals('Value should be 99', 99, TAstIntLit(Value).Value);
              end;
            end;
          end;
          AssertTrue('Should find TAstIndexAssign node', foundIndexAssign);
        finally
          prog.Free;
        end;
      finally
        p.Free;
      end;
    finally
      lex.Free;
    end;
  finally
    d.Free;
  end;
end;

var
  app: TTestRunner;
begin
  RegisterTest(TIndexAssignTest);
  app := TTestRunner.Create(nil);
  try
    app.Run;
  finally
    app.Free;
  end;
end.
