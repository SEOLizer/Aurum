{$mode objfpc}{$H+}
program test_ir;

uses
  SysUtils,
  fpcunit, testregistry, consoletestrunner,
  diag, lexer, parser, ast, sema, ir, lower_ast_to_ir;

type
  TIRTest = class(TTestCase)
  published
    procedure TestLowerSimpleFunction;
  end;

procedure TIRTest.TestLowerSimpleFunction;
var
  d: TDiagnostics;
  l: TLexer;
  p: TParser;
  prog: TAstProgram;
  s: TSema;
  modl: TIRModule;
  lower: TIRLowering;
  fn: TIRFunction;
  found: Boolean;
  i: Integer;
begin
  d := TDiagnostics.Create;
  try
    l := TLexer.Create('fn main(): int64 { var i: int64 := 0; i := 1 + 2 * 3; print_str("hi\n"); return 0; }', 'test.lyx', d);
    p := TParser.Create(l, d);
    prog := p.ParseProgram;
    s := TSema.Create(d);
    s.Analyze(prog);
    modl := TIRModule.Create;
    lower := TIRLowering.Create(modl, d);
    try
      WriteLn('DEBUG: about to call lower.Lower');
      try
        lower.Lower(prog);
        WriteLn('DEBUG: returned from lower.Lower');
      except
        on E: Exception do
        begin
          WriteLn('DEBUG: lower.Lower raised exception: ' + E.ClassName + ' - ' + E.Message);
          raise;
        end;
      end;
      AssertTrue(Length(modl.Functions) >= 1);
      fn := modl.Functions[0];
      AssertTrue(Length(fn.Instructions) > 0);
      // basic check: there should be a store to local for initial var
      // find any irStoreLocal
      found := False;
      for i := 0 to High(fn.Instructions) do
        if fn.Instructions[i].Op = irStoreLocal then found := True;
      AssertTrue(found);
    finally
      lower.Free;
      modl.Free;
    end;
    prog.Free;
    p.Free;
    l.Free;
    s.Free;
  finally
    d.Free;
  end;
end;

var
  app: TTestRunner;
begin
  RegisterTest(TIRTest);
  app := TTestRunner.Create(nil);
  try
    app.Run;
  finally
    app.Free;
  end;
end.
