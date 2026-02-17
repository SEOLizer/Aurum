{$mode objfpc}{$H+}
unit unit_manager;

interface

uses
  SysUtils, Classes,
  ast, diag, lexer, parser;

type
  { Geladene Unit mit ihrem AST }
  TLoadedUnit = class
  public
    UnitPath: string;
    FileName: string;
    AST: TAstProgram;
    IsParsed: Boolean;
    constructor Create(const aUnitPath, aFileName: string; aAST: TAstProgram);
    destructor Destroy; override;
  end;

  { Verwaltet alle geladenen Units }
  TUnitManager = class
  private
    FUnits: TStringList; // UnitPath -> TLoadedUnit
    FSearchPaths: TStringList;
    FDiag: TDiagnostics;
    function ResolveUnitPath(const unitPath: string): string;
    function LoadUnitFile(const unitPath: string; out fileName: string): TAstProgram;
  public
    constructor Create(d: TDiagnostics);
    destructor Destroy; override;
    procedure AddSearchPath(const path: string);
    function LoadUnit(const unitPath: string): TLoadedUnit;
    function FindUnit(const unitPath: string): TLoadedUnit;
    procedure LoadAllImports(prog: TAstProgram; const basePath: string);
    property Units: TStringList read FUnits;
  end;

implementation

{ TLoadedUnit }

constructor TLoadedUnit.Create(const aUnitPath, aFileName: string; aAST: TAstProgram);
begin
  inherited Create;
  UnitPath := aUnitPath;
  FileName := aFileName;
  AST := aAST;
  IsParsed := False;
end;

destructor TLoadedUnit.Destroy;
begin
  if Assigned(AST) then
    AST.Free;
  inherited Destroy;
end;

{ TUnitManager }

constructor TUnitManager.Create(d: TDiagnostics);
begin
  inherited Create;
  FDiag := d;
  FUnits := TStringList.Create;
  FUnits.Sorted := False;
  FSearchPaths := TStringList.Create;
  // Standard-Suchpfad: aktuelles Verzeichnis
  FSearchPaths.Add('.');
  FSearchPaths.Add('./lib');
end;

destructor TUnitManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to FUnits.Count - 1 do
    TLoadedUnit(FUnits.Objects[i]).Free;
  FUnits.Free;
  FSearchPaths.Free;
  inherited Destroy;
end;

procedure TUnitManager.AddSearchPath(const path: string);
begin
  FSearchPaths.Add(path);
end;

function TUnitManager.ResolveUnitPath(const unitPath: string): string;
{ Wandelt Unit-Pfad (z.B. "std.io") in Dateipfad (z.B. "std/io.au") um }
var
  i: Integer;
  filePath, searchPath, fullPath: string;
begin
  // Ersetze '.' durch '/' im Unit-Pfad
  filePath := StringReplace(unitPath, '.', '/', [rfReplaceAll]) + '.lyx';
  
  // Suche in allen Suchpfaden
  for i := 0 to FSearchPaths.Count - 1 do
  begin
    searchPath := FSearchPaths[i];
    fullPath := searchPath + '/' + filePath;
    if FileExists(fullPath) then
    begin
      Result := fullPath;
      Exit;
    end;
  end;
  
  Result := '';
end;

function TUnitManager.LoadUnitFile(const unitPath: string; out fileName: string): TAstProgram;
{ Lädt und parst eine Unit-Datei }
var
  lx: TLexer;
  p: TParser;
  src: TStringList;
begin
  Result := nil;
  fileName := ResolveUnitPath(unitPath);
  
  if fileName = '' then
  begin
    FDiag.Error('cannot find unit: ' + unitPath, Default(TSourceSpan));
    Exit;
  end;
  
  if not FileExists(fileName) then
  begin
    FDiag.Error('unit file not found: ' + fileName, Default(TSourceSpan));
    Exit;
  end;
  
  src := TStringList.Create;
  try
    try
      src.LoadFromFile(fileName);
    except
      on E: Exception do
      begin
        FDiag.Error('cannot read unit file: ' + fileName + ' - ' + E.Message, Default(TSourceSpan));
        Exit;
      end;
    end;
    
    lx := TLexer.Create(src.Text, fileName, FDiag);
    try
      p := TParser.Create(lx, FDiag);
      try
        Result := p.ParseProgram;
      finally
        p.Free;
      end;
    finally
      lx.Free;
    end;
  finally
    src.Free;
  end;
end;

function TUnitManager.LoadUnit(const unitPath: string): TLoadedUnit;
{ Lädt eine Unit (falls noch nicht geladen) }
var
  idx: Integer;
  ast: TAstProgram;
  fileName: string;
begin
  // Prüfe ob Unit bereits geladen
  idx := FUnits.IndexOf(unitPath);
  if idx >= 0 then
  begin
    Result := TLoadedUnit(FUnits.Objects[idx]);
    Exit;
  end;
  
  // Lade neue Unit
  ast := LoadUnitFile(unitPath, fileName);
  if not Assigned(ast) then
  begin
    Result := nil;
    Exit;
  end;
  
  Result := TLoadedUnit.Create(unitPath, fileName, ast);
  FUnits.AddObject(unitPath, Result);
  
  // Rekursiv alle Imports dieser Unit laden
  LoadAllImports(ast, ExtractFilePath(fileName));
end;

function TUnitManager.FindUnit(const unitPath: string): TLoadedUnit;
var
  idx: Integer;
begin
  idx := FUnits.IndexOf(unitPath);
  if idx >= 0 then
    Result := TLoadedUnit(FUnits.Objects[idx])
  else
    Result := nil;
end;

procedure TUnitManager.LoadAllImports(prog: TAstProgram; const basePath: string);
{ Lädt alle Import-Deklarationen eines Programms }
var
  i: Integer;
  decl: TAstNode;
  impDecl: TAstImportDecl;
  unitPath: string;
  loadedUnit: TLoadedUnit;
begin
  if not Assigned(prog) then Exit;
  
  for i := 0 to High(prog.Decls) do
  begin
    decl := prog.Decls[i];
    if decl is TAstImportDecl then
    begin
      impDecl := TAstImportDecl(decl);
      unitPath := impDecl.UnitPath;
      
      // Prüfe ob Unit bereits geladen
      loadedUnit := FindUnit(unitPath);
      if not Assigned(loadedUnit) then
      begin
        // Füge basePath temporär zu Suchpfaden hinzu
        if basePath <> '' then
          FSearchPaths.Insert(0, basePath);
        try
          loadedUnit := LoadUnit(unitPath);
        finally
          if basePath <> '' then
            FSearchPaths.Delete(0);
        end;
        
        if not Assigned(loadedUnit) then
        begin
          FDiag.Error('failed to load import: ' + unitPath, decl.Span);
        end;
      end;
    end;
  end;
end;

end.
