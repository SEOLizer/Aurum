{$mode objfpc}{$H+}
program test_codegen_widths;

uses
  SysUtils,
  Classes,
  fpcunit, testregistry, consoletestrunner,
  diag, lexer, parser, ast, sema, ir, lower_ast_to_ir;

type
  TCodegenWidthsTest = class(TTestCase)
  private
    FDiag: TDiagnostics;
    function ParseAndLower(const src, fname: string): TIRModule;
  published
    procedure TestConstFoldInt8SignedValue;
    procedure TestConstFoldUInt8UnsignedValue;
    procedure TestNonLiteralInitEmitsTruncAndExt;
  end;

function TCodegenWidthsTest.ParseAndLower(const src, fname: string): TIRModule;
var
  lex: TLexer;
  p: TParser;
  prog: TAstProgram;
  s: TSema;
begin
  FDiag := TDiagnostics.Create;
  lex := TLexer.Create(src, fname, FDiag);
  try
    p := TParser.Create(lex, FDiag);
    try
      prog := p.ParseProgram;
      s := TSema.Create(FDiag);
      try
        s.Analyze(prog);
      finally
        s.Free;
      end;
      Result := TIRLowering.Create(TIRModule.Create, FDiag).Lower(prog);
    finally
      p.Free;
    end;
  finally
    lex.Free;
    FDiag.Free;
  end;
end;

procedure TCodegenWidthsTest.TestConstFoldInt8SignedValue;
{ var a: int8 := 130 should const-fold to irConstInt(-126) }
var
  modl: TIRModule;
  f: TIRFunction;
  i, j: Integer;
  foundMinus126: Boolean;
begin
  modl := ParseAndLower(
    'fn main(): int64 {' + LineEnding +
    '  var a: int8 := 130;' + LineEnding +
    '  return a;' + LineEnding +
    '}',
    'test_int8.au'
  );
  try
    foundMinus126 := False;
    // look in main function for irConstInt with value -126
    for i := 0 to High(modl.Functions) do
    begin
      f := modl.Functions[i];
      if f.Name <> 'main' then Continue;
      for j := 0 to High(f.Instructions) do
      begin
        if (f.Instructions[j].Op = irConstInt) and
           (f.Instructions[j].ImmInt = -126) then
          foundMinus126 := True;
      end;
    end;
    AssertTrue('Expected irConstInt(-126) for int8 := 130', foundMinus126);
  finally
    modl.Free;
  end;
end;

procedure TCodegenWidthsTest.TestConstFoldUInt8UnsignedValue;
{ var b: uint8 := 250 should const-fold to irConstInt(250) }
var
  modl: TIRModule;
  f: TIRFunction;
  i, j: Integer;
  found250: Boolean;
begin
  modl := ParseAndLower(
    'fn main(): int64 {' + LineEnding +
    '  var b: uint8 := 250;' + LineEnding +
    '  return b;' + LineEnding +
    '}',
    'test_uint8.au'
  );
  try
    found250 := False;
    for i := 0 to High(modl.Functions) do
    begin
      f := modl.Functions[i];
      if f.Name <> 'main' then Continue;
      for j := 0 to High(f.Instructions) do
      begin
        if (f.Instructions[j].Op = irConstInt) and
           (f.Instructions[j].ImmInt = 250) then
          found250 := True;
      end;
    end;
    AssertTrue('Expected irConstInt(250) for uint8 := 250', found250);
  finally
    modl.Free;
  end;
end;

procedure TCodegenWidthsTest.TestNonLiteralInitEmitsTruncAndExt;
{ When init value is non-literal (e.g. a + b), Trunc/SExt/ZExt IR ops are emitted }
var
  modl: TIRModule;
  f: TIRFunction;
  i, j: Integer;
  foundTrunc, foundExt: Boolean;
begin
  modl := ParseAndLower(
    'fn main(): int64 {' + LineEnding +
    '  var x: int64 := 10;' + LineEnding +
    '  var y: int8 := x;' + LineEnding +  // non-literal init -> trunc
    '  var z: int64 := y;' + LineEnding +  // load from y -> SExt
    '  return z;' + LineEnding +
    '}',
    'test_nonlit.au'
  );
  try
    foundTrunc := False;
    foundExt := False;
    for i := 0 to High(modl.Functions) do
    begin
      f := modl.Functions[i];
      if f.Name <> 'main' then Continue;
      for j := 0 to High(f.Instructions) do
      begin
        if f.Instructions[j].Op = irTrunc then foundTrunc := True;
        if f.Instructions[j].Op = irSExt then foundExt := True;
      end;
    end;
    AssertTrue('Expected irTrunc for non-literal int8 init', foundTrunc);
    AssertTrue('Expected irSExt when loading int8 local', foundExt);
  finally
    modl.Free;
  end;
end;

var
  app: TTestRunner;
begin
  RegisterTest(TCodegenWidthsTest);
  app := TTestRunner.Create(nil);
  try
    app.Run;
  finally
    app.Free;
  end;
end.
