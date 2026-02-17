{$mode objfpc}{$H+}
program test_diag;

uses
  SysUtils,
  fpcunit, testregistry, consoletestrunner,
  diag;

type
  TDiagnosticsTest = class(TTestCase)
  published
    procedure TestCreateEmpty;
    procedure TestReportError;
    procedure TestReportWarning;
    procedure TestReportNote;
    procedure TestHasErrors;
    procedure TestErrorCount;
    procedure TestWarningCount;
    procedure TestMixedEntries;
    procedure TestFormatWithFileName;
    procedure TestFormatWithoutFileName;
    procedure TestMakeSpan;
    procedure TestNullSpan;
  end;

procedure TDiagnosticsTest.TestCreateEmpty;
var
  d: TDiagnostics;
begin
  d := TDiagnostics.Create;
  try
    AssertEquals(0, d.Count);
    AssertFalse(d.HasErrors);
    AssertEquals(0, d.ErrorCount);
    AssertEquals(0, d.WarningCount);
  finally
    d.Free;
  end;
end;

procedure TDiagnosticsTest.TestReportError;
var
  d: TDiagnostics;
  e: TDiagEntry;
begin
  d := TDiagnostics.Create;
  try
    d.Error('undeclared identifier', MakeSpan(3, 5, 3, 'test.lyx'));
    AssertEquals(1, d.Count);
    AssertTrue(d.HasErrors);
    e := d.GetEntry(0);
    AssertTrue(e.Kind = dkError);
    AssertEquals('undeclared identifier', e.Msg);
    AssertEquals(3, e.Span.Line);
    AssertEquals(5, e.Span.Col);
  finally
    d.Free;
  end;
end;

procedure TDiagnosticsTest.TestReportWarning;
var
  d: TDiagnostics;
begin
  d := TDiagnostics.Create;
  try
    d.Warning('unused variable', MakeSpan(1, 1, 1, 'test.lyx'));
    AssertEquals(1, d.Count);
    AssertFalse(d.HasErrors);
    AssertEquals(0, d.ErrorCount);
    AssertEquals(1, d.WarningCount);
  finally
    d.Free;
  end;
end;

procedure TDiagnosticsTest.TestReportNote;
var
  d: TDiagnostics;
  e: TDiagEntry;
begin
  d := TDiagnostics.Create;
  try
    d.Note('declared here', MakeSpan(2, 3, 5, 'test.lyx'));
    AssertEquals(1, d.Count);
    e := d.GetEntry(0);
    AssertTrue(e.Kind = dkNote);
  finally
    d.Free;
  end;
end;

procedure TDiagnosticsTest.TestHasErrors;
var
  d: TDiagnostics;
begin
  d := TDiagnostics.Create;
  try
    d.Warning('w1', NullSpan);
    d.Note('n1', NullSpan);
    AssertFalse('Nur Warnings/Notes: keine Errors', d.HasErrors);
    d.Error('e1', NullSpan);
    AssertTrue('Nach Error: HasErrors = True', d.HasErrors);
  finally
    d.Free;
  end;
end;

procedure TDiagnosticsTest.TestErrorCount;
var
  d: TDiagnostics;
begin
  d := TDiagnostics.Create;
  try
    d.Error('e1', NullSpan);
    d.Error('e2', NullSpan);
    d.Warning('w1', NullSpan);
    d.Error('e3', NullSpan);
    AssertEquals(3, d.ErrorCount);
  finally
    d.Free;
  end;
end;

procedure TDiagnosticsTest.TestWarningCount;
var
  d: TDiagnostics;
begin
  d := TDiagnostics.Create;
  try
    d.Warning('w1', NullSpan);
    d.Error('e1', NullSpan);
    d.Warning('w2', NullSpan);
    AssertEquals(2, d.WarningCount);
  finally
    d.Free;
  end;
end;

procedure TDiagnosticsTest.TestMixedEntries;
var
  d: TDiagnostics;
begin
  d := TDiagnostics.Create;
  try
    d.Error('e1', NullSpan);
    d.Warning('w1', NullSpan);
    d.Note('n1', NullSpan);
    AssertEquals(3, d.Count);
    AssertEquals(1, d.ErrorCount);
    AssertEquals(1, d.WarningCount);
  finally
    d.Free;
  end;
end;

procedure TDiagnosticsTest.TestFormatWithFileName;
var
  d: TDiagnostics;
  s: string;
begin
  d := TDiagnostics.Create;
  try
    d.Error('type mismatch', MakeSpan(10, 7, 4, 'main.lyx'));
    s := d.FormatEntry(0);
    AssertEquals('main.au:10:7: error: type mismatch', s);
  finally
    d.Free;
  end;
end;

procedure TDiagnosticsTest.TestFormatWithoutFileName;
var
  d: TDiagnostics;
  s: string;
begin
  d := TDiagnostics.Create;
  try
    d.Warning('shadowed variable', MakeSpan(5, 3, 1, ''));
    s := d.FormatEntry(0);
    AssertEquals('5:3: warning: shadowed variable', s);
  finally
    d.Free;
  end;
end;

procedure TDiagnosticsTest.TestMakeSpan;
var
  sp: TSourceSpan;
begin
  sp := MakeSpan(42, 13, 7, 'hello.lyx');
  AssertEquals(42, sp.Line);
  AssertEquals(13, sp.Col);
  AssertEquals(7, sp.Len);
  AssertEquals('hello.au', sp.FileName);
end;

procedure TDiagnosticsTest.TestNullSpan;
var
  sp: TSourceSpan;
begin
  sp := NullSpan;
  AssertEquals(0, sp.Line);
  AssertEquals(0, sp.Col);
  AssertEquals(0, sp.Len);
  AssertEquals('', sp.FileName);
end;

var
  app: TTestRunner;
begin
  RegisterTest(TDiagnosticsTest);
  app := TTestRunner.Create(nil);
  try
    app.Run;
  finally
    app.Free;
  end;
end.
