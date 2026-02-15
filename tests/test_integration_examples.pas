{$mode objfpc}{$H+}
program test_integration_examples;

uses SysUtils, Classes, Process;

function RunCmd(const cmd: string): LongInt;
var
  p: TProcess;
begin
  p := TProcess.Create(nil);
  try
    p.Executable := '/bin/sh';
    p.Parameters.Clear;
    p.Parameters.Add('-c');
    p.Parameters.Add(cmd);
    p.Options := [poWaitOnExit];
    p.Execute;
    Result := p.ExitStatus;
  finally
    p.Free;
  end;
end;

function RunCmdCapture(const cmd: string; out output: string): LongInt;
var
  p: TProcess;
  buf: array[0..4095] of Byte;
  n: LongInt;
begin
  output := '';
  p := TProcess.Create(nil);
  try
    p.Executable := '/bin/sh';
    p.Parameters.Clear;
    p.Parameters.Add('-c');
    p.Parameters.Add(cmd);
    p.Options := [poWaitOnExit, poUsePipes];
    p.Execute;
    repeat
      n := p.Output.Read(buf, SizeOf(buf));
      if n > 0 then
        output := output + Copy(PAnsiChar(@buf[0]), 1, n);
    until n <= 0;
    Result := p.ExitStatus;
  finally
    p.Free;
  end;
end;

var
  ret: LongInt;
  txt: string;
  lines: TStringList;
begin
  // Build compiler
  ret := RunCmd('fpc -O2 -Mobjfpc -Sh -FUlib/ -Fuutil/ -Fufrontend/ -Fuir/ -Fubackend/ -Fubackend/x86_64/ -Fubackend/elf/ aurumc.lpr -oaurumc');
  if ret <> 0 then
  begin
    Writeln('ERROR: building aurumc failed with code ', ret);
    Halt(1);
  end;

  // Compile and run use_math example
  ret := RunCmd('./aurumc examples/use_math.au -o /tmp/use_math');
  if ret <> 0 then
  begin
    Writeln('ERROR: compiling use_math failed with code ', ret);
    Halt(1);
  end;
  ret := RunCmdCapture('/tmp/use_math', txt);
  if ret <> 0 then
  begin
    Writeln('ERROR: running use_math failed with code ', ret);
    Halt(1);
  end;
  if txt <> '14'#10 then
  begin
    Writeln('ERROR: use_math output mismatch. Got:');
    Writeln(txt);
    Halt(1);
  end;

  // Compile and run use_env example with arguments
  ret := RunCmd('./aurumc examples/use_env.au -o /tmp/use_env');
  if ret <> 0 then
  begin
    Writeln('ERROR: compiling use_env failed with code ', ret);
    Halt(1);
  end;
  ret := RunCmdCapture('/tmp/use_env foo bar', txt);
  if ret <> 0 then
  begin
    Writeln('ERROR: running use_env failed with code ', ret);
    Halt(1);
  end;
  lines := TStringList.Create;
  try
    lines.Text := txt;
    if lines.Count < 2 then
    begin
      Writeln('ERROR: use_env produced less than 2 lines:');
      Writeln(txt);
      Halt(1);
    end;
    // first line should be a number >= 1
    try
      ret := StrToInt(Trim(lines[0]));
    except
      Writeln('ERROR: first line of use_env is not an integer: ' + lines[0]);
      Halt(1);
    end;
    if ret < 1 then
    begin
      Writeln('ERROR: arg_count < 1: ', ret);
      Halt(1);
    end;
    // second line should equal program name (arg(0))
    if Pos('use_env', lines[1]) = 0 then
    begin
      Writeln('ERROR: second line does not contain program name. Got: ', lines[1]);
      Halt(1);
    end;
  finally
    lines.Free;
  end;

  Writeln('Integration examples passed');
end.
