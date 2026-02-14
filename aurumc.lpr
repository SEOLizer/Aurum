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
  irDump: TStringList;
  fi, ii: Integer;
  ins: TIRInstr;
  fs: TFileStream;

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

  WriteLn('aurumc v0.0.1 EMITTEST');
  WriteLn('Eingabe:  ', inputFile);
  WriteLn('Ausgabe:  ', outputFile);

  src := TStringList.Create;
  try
    src.LoadFromFile(inputFile);
    d := TDiagnostics.Create;
    try
      lx := TLexer.Create(src.Text, inputFile, d);
        try
          emit := TX86_64Emitter.Create;
          try
            WriteLn('EMIT: start');
            if Assigned(module) then
              WriteLn('EMIT: module assigned, strings=', module.Strings.Count)
            else
              WriteLn('EMIT: module is nil');
            WriteLn('EMIT: calling Create/GetCodeBuffer');
            WriteLn('emit obj: ', PtrInt(emit));
            try
              codeBuf := emit.GetCodeBuffer;
              WriteLn('EMIT: GetCodeBuffer ok, size=', codeBuf.Size);
            except
              on E: Exception do
                WriteLn('EMIT: GetCodeBuffer raised: ', E.ClassName, ' ', E.Message);
            end;
            emit.EmitFromIR(module);
            WriteLn('EMIT: done');
            codeBuf := emit.GetCodeBuffer;
            dataBuf := emit.GetDataBuffer;


          // dump code/data buffers for debugging
          fs := TFileStream.Create('/tmp/code_from_aurumc.bin', fmCreate);
          try
            if codeBuf.Size > 0 then fs.WriteBuffer(codeBuf.GetBuffer^, codeBuf.Size);
          finally
            fs.Free;
          end;
          fs := TFileStream.Create('/tmp/data_from_aurumc.bin', fmCreate);
          try
            if dataBuf.Size > 0 then fs.WriteBuffer(dataBuf.GetBuffer^, dataBuf.Size);
          finally
            fs.Free;
          end;

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
