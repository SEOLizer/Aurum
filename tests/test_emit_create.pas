{$mode objfpc}{$H+}
program test_emit_create;
uses SysUtils, bytes, x86_64_emit;

var e: TX86_64Emitter;
begin
  try
    e := TX86_64Emitter.Create;
    WriteLn('Created emitter');
    e.Free;
    WriteLn('Freed emitter');
  except
    on E: Exception do
      WriteLn('Exception: ', E.ClassName, ' ', E.Message);
  end;
end.
