{$mode objfpc}{$H+}
program test_integration_examples;

uses SysUtils, Classes, BaseUnix;

function RunCommandCapture(cmd: string; out stdoutPath: string; out stderrPath: string): LongInt;
var
  outf, errf: string;
  fullCmd: string;
begin
  outf := '/tmp/integration_out.txt';
  errf := '/tmp/integration_err.txt';
  stdoutPath := outf;
  stderrPath := errf;
  fullCmd := cmd + ' > ' + outf + ' 2> ' + errf;
  Result := fpSystem(fullCmd);
end;

function FileAsString(const path: string): string;
var
  s: TStringList;
begin
  s := TStringList.Create;
  try
    if FileExists(path) then
      s.LoadFromFile(path)
    else
      s.Text := '';
    Result := s.Text;
  finally
    s.Free;
  end;
end;

function TrimNewline(const s: string): string;
begin
  Result := s;
  if (Length(Result) > 0) and (Result[Length(Result)] = #10) then
    Result := Copy(Result, 1, Length(Result)-1);
  if (Length(Result) > 0) and (Result[Length(Result)] = #13) then
    Result := Copy(Result, 1, Length(Result)-1);
end;

var
  ret: LongInt;
  outp, errp: string;
  txt: string;
  lines: TStringList;
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
  ret := RunCommandCapture('/tmp/use_math', outp, errp);
  if ret <> 0 then
  begin
    Writeln('ERROR: running use_math failed with code ', ret);
    Writeln('STDERR:'); Writeln(FileAsString(errp));
    Halt(1);
  end;
  txt := FileAsString(outp);
  if txt <> '14'#10 then
  begin
    Writeln('ERROR: use_math output mismatch. Got:');
    Writeln(txt);
    Halt(1);
  end;

  // Compile and run use_env example with arguments
  ret := fpSystem('./aurumc examples/use_env.au -o /tmp/use_env');
  if ret <> 0 then
  begin
    Writeln('ERROR: compiling use_env failed with code ', ret);
    Halt(1);
  end;
  ret := RunCommandCapture('/tmp/use_env foo bar', outp, errp);
  if ret <> 0 then
  begin
    Writeln('ERROR: running use_env failed with code ', ret);
    Writeln('STDERR:'); Writeln(FileAsString(errp));
    Halt(1);
  end;
  txt := FileAsString(outp);
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
    // program name can be /tmp/use_env or just use_env depending on platform; accept substring
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
