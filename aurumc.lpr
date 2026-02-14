{$mode objfpc}{$H+}
program aurumc;

uses
  SysUtils, Classes, BaseUnix,
  bytes,
  diag, lexer, parser, ast, sema,
  ir, lower_ast_to_ir,
  x86_64_emit, elf64_writer;

var
  inputFile: string;
  outputFile: string;
  src: TStringList;
  d: TDiagnostics;
  lx: TLexer;
  p: TParser;
  prog: TAstProgram;
  s: TSema;
  module: TIRModule;
  lower: TIRLowering;
  emit: TX86_64Emitter;
  codeBuf, dataBuf: TByteBuffer;
  entryVA: UInt64;

begin
  if ParamCount < 1 then
  begin
    WriteLn(StdErr, 'Verwendung: aurumc <datei.au> [-o <output>]');
    Halt(1);
  end;

  inputFile := ParamStr(1);
  outputFile := 'a.out';

  if (ParamCount >= 3) and (ParamStr(2) = '-o') then
    outputFile := ParamStr(3);

  WriteLn('aurumc v0.0.1');
  WriteLn('Eingabe:  ', inputFile);
  WriteLn('Ausgabe:  ', outputFile);

  src := TStringList.Create;
  try
    src.LoadFromFile(inputFile);
    d := TDiagnostics.Create;
    try
      lx := TLexer.Create(src.Text, inputFile, d);
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
        if d.HasErrors then
        begin
          d.PrintAll;
          Halt(1);
        end;
      finally
        s.Free;
      end;

      module := TIRModule.Create;
      lower := TIRLowering.Create(module, d);
      try
        lower.Lower(prog);
        emit := TX86_64Emitter.Create;
        try
          emit.EmitFromIR(module);
          codeBuf := emit.GetCodeBuffer;
          dataBuf := emit.GetDataBuffer;
          entryVA := $400000 + 4096;
          WriteElf64(outputFile, codeBuf, dataBuf, entryVA);
          FpChmod(PChar(outputFile), 493);
          WriteLn('Wrote ', outputFile);
        finally
          emit.Free;
        end;
      finally
        lower.Free;
        module.Free;
      end;

    finally
      d.Free;
    end;
  finally
    src.Free;
  end;
end.
