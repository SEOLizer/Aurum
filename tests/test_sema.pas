{$mode objfpc}{$H+}
program test_sema;

uses
  SysUtils,
  fpcunit, testregistry, consoletestrunner,
  diag, lexer, parser, ast, sema;

type
  TSemaTest = class(TTestCase)
  private
    function AnalyzeSource(const src: string): TDiagnostics;
  published
    procedure TestValidVarAndAssign;
    procedure TestAssignToLetErrors;
    procedure TestTypeMismatchInDecl;
    procedure TestUndeclaredAssignmentError;
    procedure TestCallArgTypeCheck;
  end;

function TSemaTest.AnalyzeSource(const src: string): TDiagnostics;
var
  d: TDiagnostics;
  lx: TLexer;
  p: TParser;
  prog: TAstProgram;
  s: TSema;
begin
  d := TDiagnostics.Create;
  lx := TLexer.Create(src, 'test.au', d);
  try
    p := TParser.Create(lx, d);
    try
      prog := p.ParseProgram;
    finally
      p.Free;
    end;
  finally
    lx.Free;
  end;

  s := TSema.Create(d);
  try
    s.Analyze(prog);
  finally
    s.Free;
  end;

  prog.Free;
  Result := d;
end;

procedure TSemaTest.TestValidVarAndAssign;
var
  d: TDiagnostics;
begin
  d := AnalyzeSource('fn main(): int64 { var x: int64 := 1; x := x + 2; return 0; }');
  try
    AssertEquals(0, d.ErrorCount);
  finally
    d.Free;
  end;
end;

procedure TSemaTest.TestAssignToLetErrors;
var
  d: TDiagnostics;
begin
  d := AnalyzeSource('fn main(): int64 { let y: int64 := 1; y := 2; return 0; }');
  try
    AssertTrue(d.ErrorCount >= 1);
  finally
    d.Free;
  end;
end;

procedure TSemaTest.TestTypeMismatchInDecl;
var
  d: TDiagnostics;
begin
  d := AnalyzeSource('fn main(): int64 { var b: bool := 1; return 0; }');
  try
    AssertTrue(d.ErrorCount >= 1);
  finally
    d.Free;
  end;
end;

procedure TSemaTest.TestUndeclaredAssignmentError;
var
  d: TDiagnostics;
begin
  d := AnalyzeSource('fn main(): int64 { x := 1; return 0; }');
  try
    AssertTrue(d.ErrorCount >= 1);
  finally
    d.Free;
  end;
end;

procedure TSemaTest.TestCallArgTypeCheck;
var
  d: TDiagnostics;
begin
  d := AnalyzeSource('fn main(): int64 { print_int(42); print_str("hi"); print_int(true); return 0; }');
  try
    AssertTrue(d.ErrorCount >= 1);
  finally
    d.Free;
  end;
end;

var
  app: TTestRunner;
begin
  RegisterTest(TSemaTest);
  app := TTestRunner.Create(nil);
  try
    app.Run;
  finally
    app.Free;
  end;
end.
