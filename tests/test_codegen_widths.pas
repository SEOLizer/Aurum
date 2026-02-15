{$mode objfpc}{$H+}
program test_codegen_widths;

uses
  SysUtils, Classes,
  fpcunit, testregistry, consoletestrunner,
  diag, lexer, parser, ast, sema, ir, lower_ast_to_ir;

type
  TCodegenWidthsTest = class(TTestCase)
  private
    FDiag: TDiagnostics;
    function ParseAndLower(const path: string): TIRModule;
  published
    procedure TestLowerEmitsExtendOrTruncOps;
  end;

function TCodegenWidthsTest.ParseAndLower(const path: string): TIRModule;
var
  sl: TStringList;
  src: string;
  lex: TLexer;
  p: TParser;
  prog: TAstProgram;
  s: TSema;
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
      prog := p.ParseProgram;
      s := TSema.Create(FDiag);
      try
        s.Analyze(prog);
      finally
        s.Free;
      end;
      // Lower
      Result := TIRLowering.Create(TIRModule.Create, FDiag).Lower(prog);
    finally
      p.Free;
    end;
  finally
    lex.Free;
    FDiag.Free;
  end;
end;

procedure TCodegenWidthsTest.TestLowerEmitsExtendOrTruncOps;
var
  modl: TIRModule;
  f: TIRFunction;
  i, j: Integer;
  found: Boolean;
begin
  modl := ParseAndLower('examples/int_widths.au');
  try
    found := False;
    for i := 0 to High(modl.Functions) do
    begin
      f := modl.Functions[i];
      for j := 0 to High(f.Instructions) do
      begin
        case f.Instructions[j].Op of
          irSExt, irZExt, irTrunc: found := True;
        end;
      end;
    end;
    AssertTrue(found, 'Expected at least one SExt/ZExt/Trunc in lowered IR for width handling');
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
  end.
