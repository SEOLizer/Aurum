{$mode objfpc}{$H+}
program test_lower;
uses SysUtils, Classes, bytes, diag, lexer, parser, ast, sema, ir, lower_ast_to_ir;
var
  src: TStringList;
  d: TDiagnostics;
  lx: TLexer;
  p: TParser;
  prog: TAstProgram;
  s: TSema;
  module: TIRModule;
  lower: TIRLowering;
  i,j: Integer;
begin
  src := TStringList.Create;
  try
    src.LoadFromFile('examples/call.lyx');
    d := TDiagnostics.Create;
    lx := TLexer.Create(src.Text, 'examples/call.lyx', d);
    p := TParser.Create(lx, d);
    prog := p.ParseProgram;
    p.Free; lx.Free;
    s := TSema.Create(d);
    s.Analyze(prog);
    s.Free;
    module := TIRModule.Create;
    lower := TIRLowering.Create(module, d);
    lower.Lower(prog);
    WriteLn('Lowering done, functions=', Length(module.Functions));
    for i := 0 to High(module.Functions) do
    begin
      WriteLn('Function ', module.Functions[i].Name, ' locals=', module.Functions[i].LocalCount);
      for j := 0 to High(module.Functions[i].Instructions) do
      begin
        WriteLn(' instr ', j, ' op=', Ord(module.Functions[i].Instructions[j].Op), ' dest=', module.Functions[i].Instructions[j].Dest,
          ' s1=', module.Functions[i].Instructions[j].Src1, ' s2=', module.Functions[i].Instructions[j].Src2,
          ' imm=', module.Functions[i].Instructions[j].ImmInt, ' str=', module.Functions[i].Instructions[j].ImmStr,
          ' label=', module.Functions[i].Instructions[j].LabelName);
      end;
    end;
    lower.Free; module.Free; prog.Free; d.Free;
  finally
    src.Free;
  end;
end.
