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
    // Neue Tests (Phase 2)
    procedure TestForLoopValid;
    procedure TestForLoopNonIntStartError;
    procedure TestRepeatUntilConditionBool;
    procedure TestRepeatUntilConditionNonBoolError;
    procedure TestCharTypeValid;
    procedure TestCharTypeMismatchError;
    procedure TestExternDeclaration; // new
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
  lx := TLexer.Create(src, 'test.lyx', d);
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

procedure TSemaTest.TestExternDeclaration;
var d: TDiagnostics;
begin
  d := AnalyzeSource('extern fn puts(s: pchar): void; fn main(): int64 { puts("hi"); return 0; }');
  try
    // extern declaration should satisfy call site
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

// --- Neue Tests (Phase 2) ---

procedure TSemaTest.TestForLoopValid;
var
  d: TDiagnostics;
begin
  d := AnalyzeSource('fn main(): int64 { for i := 0 to 5 do print_int(i); return 0; }');
  try
    AssertEquals(0, d.ErrorCount);
  finally
    d.Free;
  end;
end;

procedure TSemaTest.TestForLoopNonIntStartError;
var
  d: TDiagnostics;
begin
  // For loop requires int64 for start/end expressions
  d := AnalyzeSource('fn main(): int64 { for i := true to 5 do print_int(i); return 0; }');
  try
    AssertTrue(d.ErrorCount >= 1);
  finally
    d.Free;
  end;
end;

procedure TSemaTest.TestRepeatUntilConditionBool;
var
  d: TDiagnostics;
begin
  // Simple repeat-until with block body
  d := AnalyzeSource('fn main(): int64 { var x: int64 := 0; repeat { x := x + 1; } until x > 5; return x; }');
  try
    AssertEquals(0, d.ErrorCount);
  finally
    d.Free;
  end;
end;

procedure TSemaTest.TestRepeatUntilConditionNonBoolError;
var
  d: TDiagnostics;
begin
  // Repeat-until condition must be bool
  d := AnalyzeSource('fn main(): int64 { var x: int64 := 0; repeat x := x + 1; until x + 5; return x; }');
  try
    AssertTrue(d.ErrorCount >= 1);
  finally
    d.Free;
  end;
end;

procedure TSemaTest.TestCharTypeValid;
var
  d: TDiagnostics;
begin
  d := AnalyzeSource('fn main(): int64 { var c: char := ''A''; return 0; }');
  try
    AssertEquals(0, d.ErrorCount);
  finally
    d.Free;
  end;
end;

procedure TSemaTest.TestCharTypeMismatchError;
var
  d: TDiagnostics;
begin
  // Char variable assigned int value
  d := AnalyzeSource('fn main(): int64 { var c: char := 65; return 0; }');
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
