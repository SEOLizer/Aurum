{$mode objfpc}{$H+}
program test_integration_examples;

uses SysUtils, BaseUnix;

var
  ret: LongInt;
begin
  // Build compiler
  ret := fpSystem('fpc -O2 -Mobjfpc -Sh aurumc.lpr -oaurumc');
  if ret <> 0 then
  begin
    Writeln('ERROR: building aurumc failed with code ', ret);
    Halt(1);
  end;

  // Compile and run use_math example
  ret := fpSystem('./aurumc examples/use_math.au -o /tmp/use_math');
  if ret <> 0 then
  begin
    Writeln('ERROR: compiling use_math failed with code ', ret);
    Halt(1);
  end;
  ret := fpSystem('/tmp/use_math');
  if ret <> 0 then
  begin
    Writeln('ERROR: running use_math failed with code ', ret);
    Halt(1);
  end;

  // Compile and run use_env example with arguments
  ret := fpSystem('./aurumc examples/use_env.au -o /tmp/use_env');
  if ret <> 0 then
  begin
    Writeln('ERROR: compiling use_env failed with code ', ret);
    Halt(1);
  end;
  ret := fpSystem('/tmp/use_env foo bar');
  if ret <> 0 then
  begin
    Writeln('ERROR: running use_env failed with code ', ret);
    Halt(1);
  end;

  Writeln('Integration examples passed');
end.
