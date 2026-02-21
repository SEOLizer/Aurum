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
    procedure TestLowerVarargsCall;
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

procedure TIRTest.TestLowerVarargsCall;
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
  hasCallWithMultipleArgs: Boolean;
begin
  d := TDiagnostics.Create;
  try
    // Test that built-in varargs function (printf) generates correct IR
    l := TLexer.Create('fn main(): int64 { printf("hello"); printf("num: %d", 42); return 0; }', 'test.lyx', d);
    try
      p := TParser.Create(l, d);
      try
        prog := p.ParseProgram;
        s := TSema.Create(d);
        try
          s.Analyze(prog);
          AssertEquals('Semantic analysis should pass for varargs calls', 0, d.ErrorCount);
          
          modl := TIRModule.Create;
          try
            lower := TIRLowering.Create(modl, d);
            try
              lower.Lower(prog);
              
              // Find main function in IR
              found := False;
              for i := 0 to High(modl.Functions) do
              begin
                if modl.Functions[i].Name = 'main' then
                begin
                  fn := modl.Functions[i];
                  found := True;
                  Break;
                end;
              end;
              AssertTrue('Should find main function in IR', found);
               
              // Check that there are call instructions with different argument counts
              hasCallWithMultipleArgs := False;
              for i := 0 to High(fn.Instructions) do
              begin
                // Check both irCall and irCallBuiltin
                if (fn.Instructions[i].Op = irCall) or (fn.Instructions[i].Op = irCallBuiltin) then
                begin
                  if fn.Instructions[i].ImmInt > 1 then // more than 1 argument
                    hasCallWithMultipleArgs := True;
                end;
              end;
              AssertTrue('Should have varargs calls with multiple arguments', hasCallWithMultipleArgs);
              
            finally
              lower.Free;
            end;
          finally
            modl.Free;
          end;
        finally
          s.Free;
        end;
      finally
        p.Free;
      end;
    finally
      l.Free;
    end;
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
