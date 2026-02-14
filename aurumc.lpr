{$mode objfpc}{$H+}
program aurumc;

uses
  SysUtils;

var
  inputFile: string;
  outputFile: string;

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

  // TODO: Pipeline implementieren
  // 1. Quelldatei einlesen
  // 2. Lexer -> Tokens
  // 3. Parser -> AST
  // 4. Sema -> typisierter AST
  // 5. Lowering -> IR
  // 6. x86_64 Emitter -> Code/Data
  // 7. ELF64 Writer -> Executable

  WriteLn('Pipeline noch nicht implementiert.');
  Halt(0);
end.
