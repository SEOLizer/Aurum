{$mode objfpc}{$H+}
program test_emit_from_lower;

uses SysUtils, Classes, bytes, diag, lexer, parser, ast, sema, ir, lower_ast_to_ir, x86_64_emit;

var
  src: TStringList;
  d: TDiagnostics;
  lx: TLexer;
  p: TParser;
  prog: TAstProgram;
  s: TSema;
  module: TIRModule;
  lower: TIRLowering;
  emit: TX86_64Emitter;
begin
  src := TStringList.Create;
  try
    src.LoadFromFile('examples/call.au');
    d := TDiagnostics.Create;
    lx := TLexer.Create(src.Text, 'examples/call.au', d);
    p := TParser.Create(lx, d);
    prog := p.ParseProgram;
    p.Free; lx.Free;
    s := TSema.Create(d);
    s.Analyze(prog);
    if d.HasErrors then begin d.PrintAll; Halt(1); end;
    s.Free;
    module := TIRModule.Create;
    lower := TIRLowering.Create(module, d);
    lower.Lower(prog);
    writeln('Lowering done, functions=', Length(module.Functions));
    emit := TX86_64Emitter.Create;
    try
      writeln('About to EmitFromIR');
      emit.EmitFromIR(module);
      writeln('EmitFromIR finished');
      emit.GetCodeBuffer.SaveToFile('/tmp/emit_from_lower_code.bin');
      emit.GetDataBuffer.SaveToFile('/tmp/emit_from_lower_data.bin');
      writeln('Wrote code/data');
    finally
      emit.Free;
    end;
    lower.Free; module.Free; prog.Free; d.Free;
  finally
    src.Free;
  end;
end.
