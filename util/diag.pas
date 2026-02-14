{$mode objfpc}{$H+}
unit diag;

interface

uses
  SysUtils, Classes;

type
  TSourceSpan = record
    Line: Integer;
    Col: Integer;
    Len: Integer;
    FileName: string;
  end;

  TDiagKind = (dkError, dkWarning, dkNote);

  TDiagEntry = record
    Kind: TDiagKind;
    Msg: string;
    Span: TSourceSpan;
  end;

  TDiagnostics = class
  private
    FEntries: array of TDiagEntry;
    FCount: Integer;
    procedure Grow;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Report(kind: TDiagKind; const msg: string; span: TSourceSpan);
    procedure Error(const msg: string; span: TSourceSpan);
    procedure Warning(const msg: string; span: TSourceSpan);
    procedure Note(const msg: string; span: TSourceSpan);

    function HasErrors: Boolean;
    function ErrorCount: Integer;
    function WarningCount: Integer;
    function Count: Integer;
    function GetEntry(index: Integer): TDiagEntry;

    function FormatEntry(index: Integer): string;
    procedure PrintAll;
  end;

function MakeSpan(line, col, len: Integer; const fileName: string): TSourceSpan;
function NullSpan: TSourceSpan;

implementation

function MakeSpan(line, col, len: Integer; const fileName: string): TSourceSpan;
begin
  Result.Line := line;
  Result.Col := col;
  Result.Len := len;
  Result.FileName := fileName;
end;

function NullSpan: TSourceSpan;
begin
  Result.Line := 0;
  Result.Col := 0;
  Result.Len := 0;
  Result.FileName := '';
end;

{ TDiagnostics }

constructor TDiagnostics.Create;
begin
  inherited Create;
  SetLength(FEntries, 16);
  FCount := 0;
end;

destructor TDiagnostics.Destroy;
begin
  FEntries := nil;
  inherited Destroy;
end;

procedure TDiagnostics.Grow;
begin
  if FCount >= Length(FEntries) then
    SetLength(FEntries, Length(FEntries) * 2);
end;

procedure TDiagnostics.Report(kind: TDiagKind; const msg: string;
  span: TSourceSpan);
begin
  Grow;
  FEntries[FCount].Kind := kind;
  FEntries[FCount].Msg := msg;
  FEntries[FCount].Span := span;
  Inc(FCount);
end;

procedure TDiagnostics.Error(const msg: string; span: TSourceSpan);
begin
  Report(dkError, msg, span);
end;

procedure TDiagnostics.Warning(const msg: string; span: TSourceSpan);
begin
  Report(dkWarning, msg, span);
end;

procedure TDiagnostics.Note(const msg: string; span: TSourceSpan);
begin
  Report(dkNote, msg, span);
end;

function TDiagnostics.HasErrors: Boolean;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    if FEntries[i].Kind = dkError then
      Exit(True);
  Result := False;
end;

function TDiagnostics.ErrorCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FCount - 1 do
    if FEntries[i].Kind = dkError then
      Inc(Result);
end;

function TDiagnostics.WarningCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FCount - 1 do
    if FEntries[i].Kind = dkWarning then
      Inc(Result);
end;

function TDiagnostics.Count: Integer;
begin
  Result := FCount;
end;

function TDiagnostics.GetEntry(index: Integer): TDiagEntry;
begin
  Assert((index >= 0) and (index < FCount), 'GetEntry: index out of range');
  Result := FEntries[index];
end;

function TDiagnostics.FormatEntry(index: Integer): string;
var
  e: TDiagEntry;
  kindStr: string;
begin
  e := GetEntry(index);
  case e.Kind of
    dkError:   kindStr := 'error';
    dkWarning: kindStr := 'warning';
    dkNote:    kindStr := 'note';
  end;
  if e.Span.FileName <> '' then
    Result := Format('%s:%d:%d: %s: %s',
      [e.Span.FileName, e.Span.Line, e.Span.Col, kindStr, e.Msg])
  else
    Result := Format('%d:%d: %s: %s',
      [e.Span.Line, e.Span.Col, kindStr, e.Msg]);
end;

procedure TDiagnostics.PrintAll;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    WriteLn(StdErr, FormatEntry(i));
end;

end.
