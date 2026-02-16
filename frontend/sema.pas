{$mode objfpc}{$H+}
unit sema;

interface

uses
  SysUtils, Classes, ast, diag, lexer;

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
    procedure PushScope;
    procedure PopScope;
    procedure AddSymbolToCurrent(sym: TSymbol; span: TSourceSpan);
    function ResolveSymbol(const name: string): TSymbol;
    procedure DeclareBuiltinFunctions;
    function TypeEqual(a, b: TAurumType): Boolean;
    function CheckExpr(expr: TAstExpr): TAurumType;
    procedure CheckStmt(stmt: TAstStmt);
  public
    constructor Create(d: TDiagnostics);
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
    atInt8, atInt16, atInt32, atInt64, atUInt8, atUInt16, atUInt32, atUInt64: Result := True;
  else
    Result := False;
  end;
end;

function IsFloatType(t: TAurumType): Boolean;
begin
  case t of
    atF32, atF64: Result := True;
  else
    Result := False;
  end;
end;

function TSema.TypeEqual(a, b: TAurumType): Boolean;
begin
  // exact match
  if a = b then Exit(True);
  // treat any integer widths as compatible for now
  if IsIntegerType(a) and IsIntegerType(b) then Exit(True);
  // allow char to integer conversion
  if (a = atChar) and IsIntegerType(b) then Exit(True);
  if IsIntegerType(a) and (b = atChar) then Exit(True);
  // allow float type compatibility (f64 can be assigned to f32 variables)
  if IsFloatType(a) and IsFloatType(b) then Exit(True);
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
  elementType: TAurumType;
begin
  if expr = nil then
  begin
    Result := atUnresolved;
    Exit;
  end;
  case expr.Kind of
    nkIntLit: Result := atInt64;
    nkFloatLit: Result := atF64;
    nkStrLit: Result := atPChar;
    nkCharLit: Result := atChar;
    nkBoolLit: Result := atBool;
    nkArrayLit:
       begin
         // Array-Literal-Typprüfung: alle Elemente müssen gleichen Typ haben
         if Length(TAstArrayLit(expr).Items) > 0 then
         begin
           elementType := CheckExpr(TAstArrayLit(expr).Items[0]);
           // Prüfe, dass alle anderen Elemente kompatibel sind
           for i := 1 to High(TAstArrayLit(expr).Items) do
           begin
             atype := CheckExpr(TAstArrayLit(expr).Items[i]);
             if not TypeEqual(elementType, atype) then
               FDiag.Error(Format('array element type mismatch: expected %s but got %s', [AurumTypeToStr(elementType), AurumTypeToStr(atype)]), TAstArrayLit(expr).Items[i].Span);
           end;
           // Array-Literal hat always den Typ 'array'
           Result := atArray;
         end
         else
           Result := atArray; // leeres Array ist auch atArray
       end;
      nkArrayIndex:
        begin
          // Array-Index: arr[i] gibt Element-Typ zurück
          // Für jetzt nehmen wir an, dass alle Arrays int64-Elemente haben
          // TODO: Echte Element-Typ-Tracking implementieren
          
          // Prüfe dass Array-Expression tatsächlich ein Array ist
          atype := CheckExpr(TAstArrayIndex(expr).ArrayExpr);
          if not TypeEqual(atype, atArray) then
            FDiag.Error(Format('indexing non-array type: got %s', [AurumTypeToStr(atype)]), TAstArrayIndex(expr).ArrayExpr.Span);
          
          // Prüfe dass Index ein Integer ist
          atype := CheckExpr(TAstArrayIndex(expr).Index);
          if not IsIntegerType(atype) then
            FDiag.Error(Format('array index must be integer, got %s', [AurumTypeToStr(atype)]), TAstArrayIndex(expr).Index.Span);
          
          // Array-Indexing gibt Element-Typ zurück (für jetzt: int64)
          Result := atInt64; // TODO: Echten Element-Typ verwenden
        end;
      nkIndexAccess:
        begin
          // Generischer Index-Zugriff: obj[index]
          // Prüfe dass das Objekt ein Array ist
          atype := CheckExpr(TAstIndexAccess(expr).Obj);
          if not TypeEqual(atype, atArray) then
            FDiag.Error(Format('indexing non-array type: got %s', [AurumTypeToStr(atype)]), TAstIndexAccess(expr).Obj.Span);
          
          // Prüfe dass Index ein Integer ist
          atype := CheckExpr(TAstIndexAccess(expr).Index);
          if not IsIntegerType(atype) then
            FDiag.Error(Format('array index must be integer, got %s', [AurumTypeToStr(atype)]), TAstIndexAccess(expr).Index.Span);
          
          // Index-Zugriff gibt Element-Typ zurück (für jetzt: int64)
          Result := atInt64;
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
  arrayAssign: TAstArrayAssign;
  ifn: TAstIf;
  wh: TAstWhile;
  ret: TAstReturn;
  bs: TAstBlock;
  i: Integer;
  s: TSymbol;
  sym: TSymbol;
  vtype, ctype, rtype, atype: TAurumType;
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
     nkArrayAssign:
       begin
         arrayAssign := TAstArrayAssign(stmt);
         
         // Für Array-Assignment: ArrayExpr sollte ein Identifier (Array-Variable) sein
         // Nicht ein Array-Index-Ausdruck!
         if arrayAssign.ArrayExpr is TAstIdent then
         begin
           // Prüfe dass die Variable tatsächlich ein Array ist
           s := ResolveSymbol(TAstIdent(arrayAssign.ArrayExpr).Name);
           if s = nil then
           begin
             FDiag.Error('assignment to undeclared variable: ' + TAstIdent(arrayAssign.ArrayExpr).Name, arrayAssign.ArrayExpr.Span);
             Exit;
           end;
           if not TypeEqual(s.DeclType, atArray) then
             FDiag.Error(Format('assignment to non-array variable: got %s', [AurumTypeToStr(s.DeclType)]), arrayAssign.ArrayExpr.Span);
         end
         else
         begin
           FDiag.Error('array assignment requires simple array variable', arrayAssign.ArrayExpr.Span);
         end;
         
         // Prüfe dass Index ein Integer ist
         atype := CheckExpr(arrayAssign.Index);
         if not IsIntegerType(atype) then
           FDiag.Error(Format('array index must be integer, got %s', [AurumTypeToStr(atype)]), arrayAssign.Index.Span);
         
         // Prüfe Value-Typ (für jetzt: muss int64 sein, da Arrays int64-Elemente haben)
         vtype := CheckExpr(arrayAssign.Value);
         if not TypeEqual(vtype, atInt64) then
           FDiag.Error(Format('array assignment type mismatch: expected int64 but got %s', [AurumTypeToStr(vtype)]), arrayAssign.Value.Span);
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

constructor TSema.Create(d: TDiagnostics);
begin
  inherited Create;
  FDiag := d;
  SetLength(FScopes, 0);
  // create global scope
  PushScope;
  DeclareBuiltinFunctions;
  FCurrentReturn := atVoid;
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
