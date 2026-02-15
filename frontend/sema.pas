{$mode objfpc}{$H+}
unit sema;

interface

uses
  SysUtils, Classes, ast, diag, lexer, unit_manager;

type
  TSymbolKind = (symVar, symLet, symCon, symFunc);

  TSymbol = class
  public
    Name: string;
    Kind: TSymbolKind;
    DeclType: TAurumType;
    // for functions
    ParamTypes: array of TAurumType;
    ParamCount: Integer;
    constructor Create(const AName: string);
    destructor Destroy; override;
  end;

  TSema = class
  private
    FDiag: TDiagnostics;
    FScopes: array of TStringList; // each contains name -> TSymbol as object
    FCurrentReturn: TAurumType;
    FUnitManager: TUnitManager;
    FImportedUnits: TStringList; // Alias -> UnitPath for resolving qualified names
    procedure PushScope;
    procedure PopScope;
    procedure AddSymbolToCurrent(sym: TSymbol; span: TSourceSpan);
    function ResolveSymbol(const name: string): TSymbol;
    function ResolveQualifiedName(const qualifier, name: string; span: TSourceSpan): TSymbol;
    procedure DeclareBuiltinFunctions;
    procedure ProcessImports(prog: TAstProgram);
    procedure ImportUnit(imp: TAstImportDecl);
    function TypeEqual(a, b: TAurumType): Boolean;
    function CheckExpr(expr: TAstExpr): TAurumType;
    procedure CheckStmt(stmt: TAstStmt);
  public
    constructor Create(d: TDiagnostics; um: TUnitManager);
    destructor Destroy; override;
    procedure Analyze(prog: TAstProgram);
  end;

implementation

{ TSymbol }

constructor TSymbol.Create(const AName: string);
begin
  inherited Create;
  Name := AName;
  Kind := symVar;
  DeclType := atUnresolved;
  ParamCount := 0;
  SetLength(ParamTypes, 0);
end;

destructor TSymbol.Destroy;
begin
  SetLength(ParamTypes, 0);
  inherited Destroy;
end;

{ TSema }

procedure TSema.PushScope;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Sorted := False;
  SetLength(FScopes, Length(FScopes) + 1);
  FScopes[High(FScopes)] := sl;
end;

procedure TSema.PopScope;
var
  sl: TStringList;
  i: Integer;
begin
  if Length(FScopes) = 0 then Exit;
  sl := FScopes[High(FScopes)];
  // free symbols
  for i := 0 to sl.Count - 1 do
    TObject(sl.Objects[i]).Free;
  sl.Free;
  SetLength(FScopes, Length(FScopes) - 1);
end;

procedure TSema.AddSymbolToCurrent(sym: TSymbol; span: TSourceSpan);
var
  cur: TStringList;
begin
  if Length(FScopes) = 0 then
  begin
    FDiag.Error('internal sema error: no scope', span);
    Exit;
  end;
  cur := FScopes[High(FScopes)];
  if cur.IndexOf(sym.Name) >= 0 then
  begin
    FDiag.Error('redeclaration of symbol: ' + sym.Name, span);
    sym.Free;
    Exit;
  end;
  cur.AddObject(sym.Name, TObject(sym));
end;

function TSema.ResolveSymbol(const name: string): TSymbol;
var
  i, idx: Integer;
  sl: TStringList;
begin
  Result := nil;
  for i := High(FScopes) downto 0 do
  begin
    sl := FScopes[i];
    idx := sl.IndexOf(name);
    if idx >= 0 then
    begin
      Result := TSymbol(sl.Objects[idx]);
      Exit;
    end;
  end;
end;

procedure TSema.DeclareBuiltinFunctions;
var
  s: TSymbol;
begin
  // print_str(pchar) -> void
  s := TSymbol.Create('print_str');
  s.Kind := symFunc;
  s.DeclType := atVoid;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atPChar;
  AddSymbolToCurrent(s, NullSpan);

  // print_int(int64) -> void
  s := TSymbol.Create('print_int');
  s.Kind := symFunc;
  s.DeclType := atVoid;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atInt64;
  AddSymbolToCurrent(s, NullSpan);

  // exit(int64) -> void
  s := TSymbol.Create('exit');
  s.Kind := symFunc;
  s.DeclType := atVoid;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atInt64;
  AddSymbolToCurrent(s, NullSpan);
end;

function IsIntegerType(t: TAurumType): Boolean;
begin
  case t of
    atInt8, atInt16, atInt32, atInt64,
    atUInt8, atUInt16, atUInt32, atUInt64,
    atISize, atUSize: Result := True;
  else
    Result := False;
  end;
end;

function IsNumericType(t: TAurumType): Boolean;
begin
  Result := IsIntegerType(t) or (t in [atF32, atF64]);
end;

function TSema.TypeEqual(a, b: TAurumType): Boolean;
begin
  // exact match
  if a = b then Exit(True);
  // treat any integer widths as compatible for now
  if IsIntegerType(a) and IsIntegerType(b) then Exit(True);
  Result := False;
end;

function TSema.CheckExpr(expr: TAstExpr): TAurumType;
var
  ident: TAstIdent;
  bin: TAstBinOp;
  un: TAstUnaryOp;
  call: TAstCall;
  s: TSymbol;
  i: Integer;
  lt, rt, ot, atype: TAurumType;
begin
  if expr = nil then
  begin
    Result := atUnresolved;
    Exit;
  end;
  case expr.Kind of
    nkIntLit: Result := atInt64;
    nkStrLit: Result := atPChar;
    nkBoolLit: Result := atBool;
    nkCharLit: Result := atChar;
    nkFieldAccess:
      begin
        CheckExpr(TAstFieldAccess(expr).Obj);
        // field access type resolution is deferred until structs are fully implemented
        Result := atUnresolved;
      end;
    nkIndexAccess:
      begin
        CheckExpr(TAstIndexAccess(expr).Obj);
        CheckExpr(TAstIndexAccess(expr).Index);
        // index access type resolution deferred until arrays are fully implemented
        Result := atUnresolved;
      end;
    nkIdent:
      begin
        ident := TAstIdent(expr);
        s := ResolveSymbol(ident.Name);
        if s = nil then
        begin
          FDiag.Error('use of undeclared identifier: ' + ident.Name, ident.Span);
          Result := atUnresolved;
        end
        else
        begin
          Result := s.DeclType;
        end;
      end;
    nkBinOp:
      begin
        bin := TAstBinOp(expr);
        // compute child types
        lt := CheckExpr(bin.Left);
        rt := CheckExpr(bin.Right);
        case bin.Op of
           tkPlus, tkMinus, tkStar, tkSlash, tkPercent:
             begin
               if not IsIntegerType(lt) or not IsIntegerType(rt) then
                 FDiag.Error('type error: arithmetic requires integer operands', bin.Span);
               // promote to 64-bit for now
               Result := atInt64;
             end;
           tkEq, tkNeq, tkLt, tkLe, tkGt, tkGe:
             begin
               if (IsIntegerType(lt) and IsIntegerType(rt)) then
               begin
                 Result := atBool;
               end
               else if (TypeEqual(lt, atPChar) and TypeEqual(rt, atPChar)) then
               begin
                 // pointer/string comparison
                 Result := atBool;
               end
               else
               begin
                 FDiag.Error('type error: comparison requires integer or pchar operands', bin.Span);
                 Result := atUnresolved;
               end;
             end;

          tkAnd, tkOr:
            begin
              if not TypeEqual(lt, atBool) or not TypeEqual(rt, atBool) then
                FDiag.Error('type error: logical operators require bool operands', bin.Span);
              Result := atBool;
            end;
        else
          begin
            FDiag.Error('unsupported binary operator in sema', bin.Span);
            Result := atUnresolved;
          end;
        end;
      end;
    nkUnaryOp:
      begin
        un := TAstUnaryOp(expr);
        ot := CheckExpr(un.Operand);
        if un.Op = tkMinus then
        begin
          if not IsIntegerType(ot) then
            FDiag.Error('type error: unary - requires integer', un.Span);
          Result := atInt64;
        end
        else if un.Op = tkNot then
        begin
          if not TypeEqual(ot, atBool) then
            FDiag.Error('type error: unary ! requires bool', un.Span);
          Result := atBool;
        end
        else
        begin
          FDiag.Error('unsupported unary operator in sema', un.Span);
          Result := atUnresolved;
        end;
      end;
    nkCall:
      begin
        call := TAstCall(expr);
        s := ResolveSymbol(call.Name);
        if s = nil then
        begin
          FDiag.Error('call to undeclared function: ' + call.Name, call.Span);
          Result := atUnresolved;
        end
        else if s.Kind <> symFunc then
        begin
          FDiag.Error('attempt to call non-function: ' + call.Name, call.Span);
          Result := atUnresolved;
        end
        else
        begin
          if Length(call.Args) <> s.ParamCount then
            FDiag.Error(Format('wrong argument count for %s: expected %d, got %d', [call.Name, s.ParamCount, Length(call.Args)]), call.Span);
          for i := 0 to High(call.Args) do
          begin
            atype := CheckExpr(call.Args[i]);
            if (i < s.ParamCount) and (not TypeEqual(atype, s.ParamTypes[i])) then
              FDiag.Error(Format('argument %d of %s: expected %s but got %s', [i, call.Name, AurumTypeToStr(s.ParamTypes[i]), AurumTypeToStr(atype)]), call.Args[i].Span);
          end;
          Result := s.DeclType;
        end;
      end;
  else
    begin
      FDiag.Error('sema: unsupported expr kind', expr.Span);
      Result := atUnresolved;
    end;
  end;
  expr.ResolvedType := Result;
end;

procedure TSema.CheckStmt(stmt: TAstStmt);
var
  vd: TAstVarDecl;
  asg: TAstAssign;
  ifn: TAstIf;
  wh: TAstWhile;
  ret: TAstReturn;
  bs: TAstBlock;
  i: Integer;
  s: TSymbol;
  sym: TSymbol;
  vtype, ctype, rtype: TAurumType;
  sw: TAstSwitch;
  caseVal: TAstExpr;
  cvtype: TAurumType;
begin
  if stmt = nil then Exit;

  case stmt.Kind of
    nkVarDecl:
      begin
        vd := TAstVarDecl(stmt);
        // check init expr type
        vtype := CheckExpr(vd.InitExpr);
        if (vd.DeclType <> atUnresolved) and (not TypeEqual(vtype, vd.DeclType)) then
          FDiag.Error(Format('type mismatch in declaration of %s: expected %s but got %s', [vd.Name, AurumTypeToStr(vd.DeclType), AurumTypeToStr(vtype)]), vd.Span);
        sym := TSymbol.Create(vd.Name);
        case vd.Storage of
          skVar: sym.Kind := symVar;
          skLet: sym.Kind := symLet;
          skCo:  sym.Kind := symCon;
          skCon: sym.Kind := symCon;
        else
          sym.Kind := symVar;
        end;
        if vd.DeclType = atUnresolved then
          sym.DeclType := vtype
        else
          sym.DeclType := vd.DeclType;
        AddSymbolToCurrent(sym, vd.Span);
      end;
    nkAssign:
      begin
        asg := TAstAssign(stmt);
        s := ResolveSymbol(asg.Name);
        if s = nil then
        begin
          FDiag.Error('assignment to undeclared variable: ' + asg.Name, stmt.Span);
          Exit;
        end;
        if s.Kind = symLet then
        begin
          FDiag.Error('assignment to immutable variable: ' + asg.Name, stmt.Span);
        end;
        vtype := CheckExpr(asg.Value);
        if not TypeEqual(vtype, s.DeclType) then
          FDiag.Error(Format('assignment type mismatch: %s := %s', [AurumTypeToStr(s.DeclType), AurumTypeToStr(vtype)]), stmt.Span);
      end;
    nkExprStmt:
      begin
        CheckExpr(TAstExprStmt(stmt).Expr);
      end;
    nkIf:
      begin
        ifn := TAstIf(stmt);
        ctype := CheckExpr(ifn.Cond);
        if not TypeEqual(ctype, atBool) then
          FDiag.Error('if condition must be bool', ifn.Cond.Span);
        // then
        PushScope;
        CheckStmt(ifn.ThenBranch);
        PopScope;
        // else
        if Assigned(ifn.ElseBranch) then
        begin
          PushScope;
          CheckStmt(ifn.ElseBranch);
          PopScope;
        end;
      end;
    nkWhile:
      begin
        wh := TAstWhile(stmt);
        ctype := CheckExpr(wh.Cond);
        if not TypeEqual(ctype, atBool) then
          FDiag.Error('while condition must be bool', wh.Cond.Span);
        PushScope;
        CheckStmt(wh.Body);
        PopScope;
      end;
    nkFor:
      begin
        // for varName := startExpr to/downto endExpr do body
        with TAstFor(stmt) do
        begin
          vtype := CheckExpr(StartExpr);
          if not IsIntegerType(vtype) then
            FDiag.Error('for loop start must be integer', StartExpr.Span);
          ctype := CheckExpr(EndExpr);
          if not IsIntegerType(ctype) then
            FDiag.Error('for loop end must be integer', EndExpr.Span);
          // declare loop variable
          PushScope;
          sym := TSymbol.Create(VarName);
          sym.Kind := symVar;
          sym.DeclType := atInt64;
          AddSymbolToCurrent(sym, Span);
          CheckStmt(Body);
          PopScope;
        end;
      end;
    nkRepeatUntil:
      begin
        PushScope;
        CheckStmt(TAstRepeatUntil(stmt).Body);
        PopScope;
        ctype := CheckExpr(TAstRepeatUntil(stmt).Cond);
        if not TypeEqual(ctype, atBool) then
          FDiag.Error('repeat-until condition must be bool', TAstRepeatUntil(stmt).Cond.Span);
      end;
    nkReturn:
      begin
        ret := TAstReturn(stmt);
        if Assigned(ret.Value) then
        begin
          rtype := CheckExpr(ret.Value);
          if not TypeEqual(rtype, FCurrentReturn) then
            FDiag.Error(Format('return type mismatch: expected %s but got %s', [AurumTypeToStr(FCurrentReturn), AurumTypeToStr(rtype)]), ret.Span);
        end
        else
        begin
          if not TypeEqual(FCurrentReturn, atVoid) then
            FDiag.Error('missing return value for non-void function', ret.Span);
        end;
      end;
    nkBreak:
      begin
        // break allowed in switch/while; semantic check for presence of enclosing loop/switch omitted for simplicity
        Exit;
      end;
    nkSwitch:
      begin
        // switch statement
        sw := TAstSwitch(stmt);
        ctype := CheckExpr(sw.Expr);
        if not IsIntegerType(ctype) then
          FDiag.Error('switch expression must be integer', sw.Expr.Span);
        // check cases
        for i := 0 to High(sw.Cases) do
        begin
          // case value must be constant int
          caseVal := sw.Cases[i].Value;
          cvtype := CheckExpr(caseVal);
          if not IsIntegerType(cvtype) then
            FDiag.Error('case label must be integer', caseVal.Span);
          PushScope;
          CheckStmt(sw.Cases[i].Body);
          PopScope;
        end;
        if Assigned(sw.Default) then
        begin
          PushScope;
          CheckStmt(sw.Default);
          PopScope;
        end;
      end;
    nkBlock:
      begin
        bs := TAstBlock(stmt);
        // block: introduce new scope
        PushScope;
        for i := 0 to High(bs.Stmts) do
          CheckStmt(bs.Stmts[i]);
        PopScope;
      end;
    nkFuncDecl:
      begin
        // nested function? not supported yet
        FDiag.Error('nested function declarations are not supported', stmt.Span);
      end;
    else
      FDiag.Error('sema: unsupported statement kind', stmt.Span);
  end;
end;

constructor TSema.Create(d: TDiagnostics; um: TUnitManager);
begin
  inherited Create;
  FDiag := d;
  FUnitManager := um;
  FImportedUnits := TStringList.Create;
  FImportedUnits.Sorted := False;
  SetLength(FScopes, 0);
  // create global scope
  PushScope;
  DeclareBuiltinFunctions;
  FCurrentReturn := atVoid;
end;

destructor TSema.Destroy;
var
  i: Integer;
begin
  // Nur das StringList freigeben, nicht die referenzierten Units
  // (die gehören dem UnitManager)
  if Assigned(FImportedUnits) then
    FImportedUnits.Free;
  inherited Destroy;
end;

procedure TSema.ProcessImports(prog: TAstProgram);
{ Verarbeitet alle Import-Deklarationen im Programm }
var
  i: Integer;
  decl: TAstNode;
begin
  if not Assigned(prog) then Exit;
  
  for i := 0 to High(prog.Decls) do
  begin
    decl := prog.Decls[i];
    if decl is TAstImportDecl then
      ImportUnit(TAstImportDecl(decl));
  end;
end;

procedure TSema.ImportUnit(imp: TAstImportDecl);
{ Importiert eine Unit und registriert ihre Symbole }
var
  upath: string;
  loadedUnit: TLoadedUnit;
  alias: string;
  i, j: Integer;
  decl: TAstNode;
  fn: TAstFuncDecl;
  sym: TSymbol;
begin
  upath := imp.UnitPath;
  alias := imp.Alias;

  // Unit muss bereits vom UnitManager geladen sein
  if not Assigned(FUnitManager) then
  begin
    FDiag.Error('internal error: no unit manager', imp.Span);
    Exit;
  end;

  loadedUnit := FUnitManager.FindUnit(upath);
  if not Assigned(loadedUnit) then
  begin
    FDiag.Error('unit not loaded: ' + upath, imp.Span);
    Exit;
  end;

  // Registriere Alias für qualifizierte Zugriffe
  if alias = '' then
    alias := ExtractFileName(StringReplace(upath, '.', '/', [rfReplaceAll]));
  if not Assigned(FImportedUnits) then
    FImportedUnits := TStringList.Create;
  FImportedUnits.AddObject(alias, TObject(loadedUnit));
  
  // Importiere öffentliche Symbole (pub) in den globalen Scope
  if Assigned(loadedUnit.AST) then
  begin
    for i := 0 to High(loadedUnit.AST.Decls) do
    begin
      decl := loadedUnit.AST.Decls[i];
      
      // Nur Funktionen für jetzt (später auch Variablen/Types)
      if decl is TAstFuncDecl then
      begin
        fn := TAstFuncDecl(decl);
        // Prüfe ob Funktion public ist (IsPublic Property existiert im AST)
        // Für jetzt importieren wir alle Funktionen
        
        // Prüfe auf Konflikte
        if ResolveSymbol(fn.Name) <> nil then
        begin
          FDiag.Error('import conflicts with existing symbol: ' + fn.Name, imp.Span);
          Continue;
        end;
        
        sym := TSymbol.Create(fn.Name);
        sym.Kind := symFunc;
        sym.DeclType := fn.ReturnType;
        sym.ParamCount := Length(fn.Params);
        SetLength(sym.ParamTypes, sym.ParamCount);
        for j := 0 to sym.ParamCount - 1 do
          sym.ParamTypes[j] := fn.Params[j].ParamType;
        AddSymbolToCurrent(sym, fn.Span);
      end;
    end;
  end;
end;

function TSema.ResolveQualifiedName(const qualifier, name: string; span: TSourceSpan): TSymbol;
{ Löst einen qualifizierten Namen (z.B. "io.print") auf }
var
  idx: Integer;
  loadedUnit: TLoadedUnit;
  i, j: Integer;
  decl: TAstNode;
  fn: TAstFuncDecl;
begin
  Result := nil;
  
  // Finde Unit mit diesem Alias
  idx := FImportedUnits.IndexOf(qualifier);
  if idx < 0 then
  begin
    FDiag.Error('unknown module alias: ' + qualifier, span);
    Exit;
  end;
  
  loadedUnit := TLoadedUnit(FImportedUnits.Objects[idx]);
  if not Assigned(loadedUnit.AST) then
  begin
    FDiag.Error('unit has no AST: ' + qualifier, span);
    Exit;
  end;
  
  // Suche Symbol in der Unit
  for i := 0 to High(loadedUnit.AST.Decls) do
  begin
    decl := loadedUnit.AST.Decls[i];
    if decl is TAstFuncDecl then
    begin
      fn := TAstFuncDecl(decl);
      if fn.Name = name then
      begin
        Result := TSymbol.Create(name);
        Result.Kind := symFunc;
        Result.DeclType := fn.ReturnType;
        Result.ParamCount := Length(fn.Params);
        SetLength(Result.ParamTypes, Result.ParamCount);
        for j := 0 to Result.ParamCount - 1 do
          Result.ParamTypes[j] := fn.Params[j].ParamType;
        Exit;
      end;
    end;
  end;
  
  if Result = nil then
    FDiag.Error('symbol not found in module ' + qualifier + ': ' + name, span);
end;

procedure TSema.Analyze(prog: TAstProgram);
var
  i, j: Integer;
  node: TAstNode;
  fn: TAstFuncDecl;
  con: TAstConDecl;
  s: TSymbol;
  sym: TSymbol;
  itype: TAurumType;
begin
  // Phase 0: Verarbeite Imports
  ProcessImports(prog);
  
  // First pass: register top-level functions and constants
  for i := 0 to High(prog.Decls) do
  begin
    node := prog.Decls[i];
    if node is TAstFuncDecl then
    begin
      fn := TAstFuncDecl(node);
      // check duplicates
      if ResolveSymbol(fn.Name) <> nil then
      begin
        FDiag.Error('redeclaration of function: ' + fn.Name, fn.Span);
        Continue;
      end;
      sym := TSymbol.Create(fn.Name);
      sym.Kind := symFunc;
      sym.DeclType := fn.ReturnType;
      sym.ParamCount := Length(fn.Params);
      SetLength(sym.ParamTypes, sym.ParamCount);
      for j := 0 to sym.ParamCount - 1 do
        sym.ParamTypes[j] := fn.Params[j].ParamType;
      AddSymbolToCurrent(sym, fn.Span);
    end
    else if node is TAstTypeDecl then
    begin
      // type declarations: register as named types (future work)
      // for now, skip
    end
    else if node is TAstConDecl then
    begin
      con := TAstConDecl(node);
      if ResolveSymbol(con.Name) <> nil then
      begin
        FDiag.Error('redeclaration of constant: ' + con.Name, con.Span);
        Continue;
      end;
      // typecheck init expr
      itype := CheckExpr(con.InitExpr);
      if not TypeEqual(itype, con.DeclType) then
        FDiag.Error(Format('constant %s: expected type %s but got %s', [con.Name, AurumTypeToStr(con.DeclType), AurumTypeToStr(itype)]), con.Span);
      sym := TSymbol.Create(con.Name);
      sym.Kind := symCon;
      sym.DeclType := con.DeclType;
      AddSymbolToCurrent(sym, con.Span);
    end;
  end;

  // Second pass: check function bodies
  for i := 0 to High(prog.Decls) do
  begin
    node := prog.Decls[i];
    if node is TAstFuncDecl then
    begin
      fn := TAstFuncDecl(node);
      // enter function scope
      PushScope;
      // declare parameters as vars in local scope
      for j := 0 to High(fn.Params) do
      begin
        sym := TSymbol.Create(fn.Params[j].Name);
        sym.Kind := symVar;
        sym.DeclType := fn.Params[j].ParamType;
        AddSymbolToCurrent(sym, fn.Params[j].Span);
      end;
      // set current return type
      FCurrentReturn := fn.ReturnType;
      // check body
      CheckStmt(fn.Body);
      // leave function scope
      PopScope;
    end;
  end;
end;

end.
