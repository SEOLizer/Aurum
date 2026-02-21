{$mode objfpc}{$H+}
program test_array_static;

uses SysUtils, Classes, fpcunit, testregistry, consoletestrunner, diag, lexer, parser, ast, sema, ir, lower_ast_to_ir;

type
  TArrayStaticTest = class(TTestCase)
  private
    FDiag: TDiagnostics;
    function ParseAndLower(const src, fname: string): TIRModule;
  published
    procedure TestStaticArrayInitAndIndex;
  end;

function TArrayStaticTest.ParseAndLower(const src, fname: string): TIRModule;
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

procedure TArrayStaticTest.TestStaticArrayInitAndIndex;
var
  modl: TIRModule;
  f: TIRFunction;
  i, j: Integer;
  foundStore0, foundStore1, foundStore2, foundLoad1: Boolean;
begin
  modl := ParseAndLower(
    'fn main(): int64 {' + LineEnding +
    '  var a: int64[3] := [2,3,5];' + LineEnding +
    '  return a[1];' + LineEnding +
    '}',
    'test_array.au'
  );
  try
    foundStore0 := False; foundStore1 := False; foundStore2 := False; foundLoad1 := False;
    for i := 0 to High(modl.Functions) do
    begin
      f := modl.Functions[i];
      if f.Name <> 'main' then Continue;
      for j := 0 to High(f.Instructions) do
      begin
        case f.Instructions[j].Op of
          irStoreLocal:
            begin
              if f.Instructions[j].Dest = 0 then foundStore0 := True;
              if f.Instructions[j].Dest = 1 then foundStore1 := True;
              if f.Instructions[j].Dest = 2 then foundStore2 := True;
            end;
          irLoadLocal:
            begin
              if f.Instructions[j].Src1 = 1 then foundLoad1 := True;
            end;
        end;
      end;
    end;
    AssertTrue('store to a[0] expected', foundStore0);
    AssertTrue('store to a[1] expected', foundStore1);
    AssertTrue('store to a[2] expected', foundStore2);
    AssertTrue('load from a[1] expected', foundLoad1);
  finally
    modl.Free;
  end;
end;

var
  app: TTestRunner;
begin
  RegisterTest(TArrayStaticTest);
  app := TTestRunner.Create(nil);
  try
    app.Run;
  finally
    app.Free;
  end;
end.
