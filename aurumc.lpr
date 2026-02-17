{$mode objfpc}{$H+}
program aurumc;

uses
  SysUtils, Classes, BaseUnix,
  bytes,
  diag, lexer, parser, ast, sema, unit_manager,
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
  um: TUnitManager;
  module: TIRModule;
  lower: TIRLowering;
  emit: TX86_64Emitter;
  codeBuf, dataBuf: TByteBuffer;
  entryVA: UInt64;
  basePath: string;
begin
  if ParamCount < 1 then
  begin
    WriteLn(StdErr, 'Lyx Compiler v0.1.3');
    WriteLn(StdErr, 'Copyright (c) 2026 Andreas Röne. Alle Rechte vorbehalten.');
    WriteLn(StdErr);
    WriteLn(StdErr, 'Verwendung: aurumc <datei.lyx> [-o <output>]');
    Halt(1);
  end;

  inputFile := ParamStr(1);
  outputFile := 'a.out';

  if (ParamCount >= 3) and (ParamStr(2) = '-o') then
    outputFile := ParamStr(3);

  WriteLn('Lyx Compiler v0.1.3');
  WriteLn('Copyright (c) 2026 Andreas Röne. Alle Rechte vorbehalten.');
  WriteLn;
  WriteLn('Eingabe:  ', inputFile);
  WriteLn('Ausgabe:  ', outputFile);

  basePath := ExtractFilePath(inputFile);
  if basePath = '' then
    basePath := '.';

  src := TStringList.Create;
  try
    src.LoadFromFile(inputFile);
    d := TDiagnostics.Create;
    try
      // Phase 1: Parse Hauptdatei
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

      if d.HasErrors then
      begin
        d.PrintAll;
        Halt(1);
      end;

      // Phase 2: Lade alle Imports (UnitManager)
      um := TUnitManager.Create(d);
      try
        um.AddSearchPath(basePath);
        um.LoadAllImports(prog, basePath);

        if d.HasErrors then
        begin
          d.PrintAll;
          Halt(1);
        end;

        // Phase 3: Semantische Analyse
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
            // Entry point is the generated _start placed at code base + 0x1000
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
        um.Free;
      end;

    finally
      d.Free;
    end;
  finally
    src.Free;
  end;
end.
