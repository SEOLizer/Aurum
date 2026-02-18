{$mode objfpc}{$H+}
program test_printf;

uses SysUtils, Classes, Process;

function RunCapture(cmd: string; out outText: string): Integer;
var
  p: TProcess;
  sl: TStringList;
begin
  p := TProcess.Create(nil);
  sl := TStringList.Create;
  try
    p.CommandLine := cmd;
    p.Options := p.Options + [poUsePipes];
    p.Execute;
    sl.LoadFromStream(p.Output);
    p.WaitOnExit;
    outText := sl.Text;
    Result := p.ExitStatus;
  finally
    p.Free;
    sl.Free;
  end;
end;

var
  r: Integer; txt: string;
begin
  // build compiler
  r := RunCapture('fpc -O2 -Mobjfpc -Sh lyxc.lpr -olyxc', txt);
  if r <> 0 then Halt(1);

  // compile example
  r := RunCapture('./lyxc examples/test_println.lyx -o /tmp/test_println', txt);
  if r <> 0 then Halt(2);

  // run example
  r := RunCapture('/tmp/test_println', txt);
  if r <> 0 then Halt(3);

  // Basic checks
  if Pos('Hello World', txt) = 0 then Halt(4);
  if Pos('42', txt) = 0 then Halt(5);
  if Pos('true', txt) = 0 then Halt(6);
  if Pos('false', txt) = 0 then Halt(7);
  // Check formatted line
  if Pos('Formatted: answer = 42, pi=3.141500', txt) = 0 then Halt(8);

  // run negative printf example
  r := RunCapture('./lyxc examples/test_printf_negative.lyx -o /tmp/test_printf_negative', txt);
  if r <> 0 then Halt(9);
  r := RunCapture('/tmp/test_printf_negative', txt);
  if r <> 0 then Halt(10);

  // check outputs from negative example
  if Pos('A:one B:2 C:3.140000 D:four', txt) = 0 then Halt(11);
  if Pos('X:onlyone Y:', txt) = 0 then Halt(12); // Y empty
  if Pos('%q', txt) = 0 then Halt(13);
  if Pos('literal percent: % done', txt) = 0 then Halt(14);

  Writeln('printf example ok');
end.
