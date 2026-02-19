{$mode objfpc}{$H+}
program lyxc;

uses
  SysUtils, Classes, BaseUnix,
  bytes, backend_types,
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
  externSymbols: TExternalSymbolArray;
  neededLibs: array of string;

type
  TStringArray = array of string;

function CollectLibraries(const symbols: TExternalSymbolArray): TStringArray;
var
  libList: TStringList;
  i: Integer;
begin
  libList := TStringList.Create;
  try
    libList.Duplicates := dupIgnore;
    libList.Sorted := True;
    
    // Collect unique library names from symbols
    for i := 0 to High(symbols) do
      libList.Add(symbols[i].LibraryName);
    
    // Convert to array
    SetLength(Result, libList.Count);
    for i := 0 to libList.Count - 1 do
      Result[i] := libList[i];
  finally
    libList.Free;
  end;
end;

begin
  if ParamCount < 1 then
  begin
    WriteLn(StdErr, 'Lyx Compiler v0.1.5');
    WriteLn(StdErr, 'Copyright (c) 2026 Andreas Röne. Alle Rechte vorbehalten.');
    WriteLn(StdErr);
    WriteLn(StdErr, 'Verwendung: aurumc <datei.lyx> [-o <output>]');
    Halt(1);
  end;

  inputFile := ParamStr(1);
  outputFile := 'a.out';

  if (ParamCount >= 3) and (ParamStr(2) = '-o') then
    outputFile := ParamStr(3);

  WriteLn('Lyx Compiler v0.1.5');
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

        // Phase 3: Semantische Analyse (mit Unit-Integration)
        s := TSema.Create(d);
        try
          s.AnalyzeWithUnits(prog, um);
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
            
            // Check if we have external symbols - if so, generate dynamic ELF
            externSymbols := emit.GetExternalSymbols;
            if Length(externSymbols) > 0 then
            begin
              // Build unique library list
              neededLibs := CollectLibraries(externSymbols);
              WriteLn('Generating dynamic ELF with ', Length(externSymbols), ' external symbols');
              WriteDynamicElf64(outputFile, codeBuf, dataBuf, entryVA, externSymbols, neededLibs);
            end
            else
            begin
              WriteLn('Generating static ELF (no external symbols)');
              WriteElf64(outputFile, codeBuf, dataBuf, entryVA);
            end;
            
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
