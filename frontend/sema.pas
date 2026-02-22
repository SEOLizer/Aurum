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
    TypeName: string; // for named types (structs)
    StructDecl: TAstStructDecl; // if this symbol refers to an instance of a struct type
    ReturnTypeName: string; // for functions: name of return type if struct
    ReturnStructDecl: TAstStructDecl; // for functions: struct decl if returns struct
    ArrayLen: Integer; // 0 = not array, >0 = static length, -1 = dynamic
    // for functions
    ParamTypes: array of TAurumType;
    ParamCount: Integer;
    IsVarArgs: Boolean; // true for variadic functions like printf
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
    FStructTypes: TStringList; // name -> TAstStructDecl as object
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
    function CheckStructLit(sl: TAstStructLit): TAurumType;
    procedure CheckStmt(stmt: TAstStmt);
  public
    constructor Create(d: TDiagnostics; um: TUnitManager = nil);
    destructor Destroy; override;
    procedure Analyze(prog: TAstProgram);
    procedure AnalyzeWithUnits(prog: TAstProgram; um: TUnitManager);
    // Struct layout
    procedure ComputeStructLayouts;
    // AST rewrite helpers
    function RewriteExpr(expr: TAstExpr): TAstExpr;
    function RewriteStmt(stmt: TAstStmt): TAstStmt;
    procedure RewriteAST(prog: TAstProgram);
  end;

implementation

{ TSymbol }

constructor TSymbol.Create(const AName: string);
begin
  inherited Create;
  Name := AName;
  Kind := symVar;
  DeclType := atUnresolved;
  TypeName := '';
  StructDecl := nil;
  ReturnTypeName := '';
  ReturnStructDecl := nil;
  ArrayLen := 0;
  ParamCount := 0;
  IsVarArgs := False;
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

  // printf(pchar, ...) -> void (varargs)
  s := TSymbol.Create('printf');
  s.Kind := symFunc;
  s.DeclType := atVoid;
  s.ParamCount := 1;  // at least 1 required (format string)
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atPChar;
  s.IsVarArgs := True;
  AddSymbolToCurrent(s, NullSpan);

  // exit(int64) -> void
  s := TSymbol.Create('exit');
  s.Kind := symFunc;
  s.DeclType := atVoid;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atInt64;
  AddSymbolToCurrent(s, NullSpan);

  // Buffer/runtime primitives for time formatter
  // buf_put_byte(buf: pchar, idx: int64, b: int64) -> int64
  s := TSymbol.Create('buf_put_byte');
  s.Kind := symFunc;
  s.DeclType := atInt64;
  s.ParamCount := 3;
  SetLength(s.ParamTypes, 3);
  s.ParamTypes[0] := atPChar;
  s.ParamTypes[1] := atInt64;
  s.ParamTypes[2] := atInt64;
  AddSymbolToCurrent(s, NullSpan);

  // itoa_to_buf(val: int64, buf: pchar, idx: int64, buflen: int64, minWidth: int64, padZero: int64) -> int64
  s := TSymbol.Create('itoa_to_buf');
  s.Kind := symFunc;
  s.DeclType := atInt64;
  s.ParamCount := 6;
  SetLength(s.ParamTypes, 6);
  s.ParamTypes[0] := atInt64; // val
  s.ParamTypes[1] := atPChar; // buf
  s.ParamTypes[2] := atInt64; // idx
  s.ParamTypes[3] := atInt64; // buflen
  s.ParamTypes[4] := atInt64; // minWidth
  s.ParamTypes[5] := atInt64; // padZero
  AddSymbolToCurrent(s, NullSpan);

  // Dynamic array builtins
  // append(arrVar: pchar, val: int64) -> void  (alias: push)
  s := TSymbol.Create('append');
  s.Kind := symFunc;
  s.DeclType := atVoid;
  s.ParamCount := 2;
  SetLength(s.ParamTypes, 2);
  s.ParamTypes[0] := atPChar; // pointer to array header stored in variable (we'll pass variable slot)
  s.ParamTypes[1] := atInt64; // value
  AddSymbolToCurrent(s, NullSpan);

  // push(arrVar: pchar, val: int64) -> void  (legacy name)
  s := TSymbol.Create('push');
  s.Kind := symFunc;
  s.DeclType := atVoid;
  s.ParamCount := 2;
  SetLength(s.ParamTypes, 2);
  s.ParamTypes[0] := atPChar;
  s.ParamTypes[1] := atInt64;
  AddSymbolToCurrent(s, NullSpan);

  // pop(arrVar: pchar) -> int64
  s := TSymbol.Create('pop');
  s.Kind := symFunc;
  s.DeclType := atInt64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atPChar;
  AddSymbolToCurrent(s, NullSpan);

  // len(arrVar: pchar) -> int64
  s := TSymbol.Create('len');
  s.Kind := symFunc;
  s.DeclType := atInt64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atPChar;
  AddSymbolToCurrent(s, NullSpan);

  // free(arrVar: pchar) -> void
  s := TSymbol.Create('free');
  s.Kind := symFunc;
  s.DeclType := atVoid;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atPChar;
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
  sSym: TSymbol;
  i, fi: Integer;
  lt, rt, ot, atype: TAurumType;
  qualifier: string;
  identName: string;
  fName: string;
  found: Boolean;
  fldType: TAurumType;
  recv: TAstExpr;
  mName: string;
  mangledName: string;
  args: TAstExprList;
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
        // resolve object expression first
        recv := TAstFieldAccess(expr).Obj;
        recv := RewriteExpr(recv);
        CheckExpr(recv);
        // Attempt to resolve field type statically for simple cases
        if recv is TAstIdent then
        begin
          ident := TAstIdent(recv);
          sSym := ResolveSymbol(ident.Name);
          if Assigned(sSym) and Assigned(sSym.StructDecl) then
          begin
            // lookup field in struct decl
            fName := TAstFieldAccess(expr).Field;
            found := False;
            fldType := atUnresolved;
            for fi := 0 to High(sSym.StructDecl.Fields) do
            begin
              if sSym.StructDecl.Fields[fi].Name = fName then
              begin
                found := True;
                fldType := sSym.StructDecl.Fields[fi].FieldType;
                // if field has named type, we could set more info later
                Break;
              end;
            end;
            if not found then
              FDiag.Error('unknown field ' + fName + ' in type ' + sSym.StructDecl.Name, expr.Span)
            else
            begin
              Result := fldType;
              // annotate AST node with offset + owner
              if expr is TAstFieldAccess then
              begin
                TAstFieldAccess(expr).SetFieldOffset(sSym.StructDecl.FieldOffsets[fi]);
                TAstFieldAccess(expr).SetOwnerName(sSym.StructDecl.Name);
              end;
            end;
            expr.ResolvedType := Result;
            Exit;
          end;
        end;
        // fallback: unresolved
        Result := atUnresolved;
      end;
    nkIndexAccess:
      begin
        // resolve object and index
        CheckExpr(TAstIndexAccess(expr).Obj);
        CheckExpr(TAstIndexAccess(expr).Index);
        if not IsIntegerType(CheckExpr(TAstIndexAccess(expr).Index)) then
          FDiag.Error('array index must be integer', TAstIndexAccess(expr).Index.Span);
        // if indexing an identifier with array metadata, return element type
        if TAstIndexAccess(expr).Obj is TAstIdent then
        begin
          s := ResolveSymbol(TAstIdent(TAstIndexAccess(expr).Obj).Name);
          if Assigned(s) and (s.ArrayLen <> 0) then
          begin
            Result := s.DeclType;
            expr.ResolvedType := Result;
            Exit;
          end;
        end;
        // fallback: unresolved
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
    nkArrayLit:
      begin
        if Length(TAstArrayLit(expr).Items) = 0 then
        begin
          // empty array literal: type unresolved until context gives it
          Result := atUnresolved;
        end
        else
        begin
          // infer from first item
          atype := CheckExpr(TAstArrayLit(expr).Items[0]);
          for i := 1 to High(TAstArrayLit(expr).Items) do
          begin
            ot := CheckExpr(TAstArrayLit(expr).Items[i]);
            if not TypeEqual(ot, atype) then
              FDiag.Error('array literal items must have same type', TAstArrayLit(expr).Items[i].Span);
          end;
          Result := atype;
        end;
      end;
    nkStructLit:
      begin
        // Struct literal: TypeName { field: value, ... }
        Result := CheckStructLit(TAstStructLit(expr));
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
        s := nil; // Initialize to nil
        // rewrite nested args first (non-method calls too)
        for i := 0 to High(call.Args) do
          call.Args[i] := RewriteExpr(call.Args[i]);
        // Special-case: method call desugared by parser to name '_METHOD_<method>'
        if (Length(call.Name) > 8) and (Copy(call.Name,1,8) = '_METHOD_') then
        begin
          mName := Copy(call.Name, 9, MaxInt);
          // receiver must be first arg
          if Length(call.Args) = 0 then
          begin
            FDiag.Error('method call without receiver', call.Span);
            Result := atUnresolved;
            Exit;
          end;
          recv := call.Args[0];
          sSym := nil;
          
          // Check if receiver is a type name (static method call)
          if recv is TAstIdent then
          begin
            sSym := ResolveSymbol(TAstIdent(recv).Name);
            // If not found as symbol, check if it's a struct type name
            if (sSym = nil) and Assigned(FStructTypes) then
            begin
              fi := FStructTypes.IndexOf(TAstIdent(recv).Name);
              if fi >= 0 then
              begin
                // Static method call: Type.method(args)
                mangledName := '_L_' + TAstIdent(recv).Name + '_' + mName;
                s := ResolveSymbol(mangledName);
                if s = nil then
                begin
                  FDiag.Error('call to undeclared static method: ' + mangledName, call.Span);
                  Result := atUnresolved;
                  Exit;
                end;
                // Rewrite call: remove the type name from args, just use the mangled name
                call.SetName(mangledName);
                // Remove the first argument (the type name identifier)
                if Length(call.Args) > 0 then
                begin
                  // Free the type name identifier (first arg)
                  call.Args[0].Free;
                  // Shift remaining args down
                  SetLength(args, Length(call.Args) - 1);
                  for i := 1 to High(call.Args) do
                    args[i-1] := call.Args[i];
                  // Replace args without freeing (we already freed the type name)
                  call.ReplaceArgs(args);
                end;
                // Continue with normal function call checking
                // s is already set to the static method symbol
              end;
            end;
          end;
          
          // Instance method call (receiver is a variable with struct type)
          if (s = nil) and Assigned(sSym) and Assigned(sSym.StructDecl) then
          begin
            mangledName := '_L_' + sSym.StructDecl.Name + '_' + mName;
            s := ResolveSymbol(mangledName);
            if s = nil then
            begin
              FDiag.Error('call to undeclared method: ' + mangledName, call.Span);
              Result := atUnresolved;
              Exit;
            end;
            // perform in-place rewrite of call node to point to mangled function
            call.SetName(mangledName);
            // no change to args (receiver stays as first param)
          end
          else if (s = nil) and Assigned(sSym) and (not Assigned(sSym.StructDecl)) then
          begin
            // sSym found but StructDecl is nil - this is a bug in registration
            FDiag.Error('internal error: variable ' + TAstIdent(recv).Name + ' has no StructDecl', call.Span);
          end;
          
          if s = nil then
          begin
            FDiag.Error('cannot resolve method receiver type for ' + mName, call.Span);
            Result := atUnresolved;
            Exit;
          end;
        end
        else
        begin
          s := ResolveSymbol(call.Name);
          if (s = nil) and (Pos('.', call.Name) > 0) then
          begin
            // Handle qualified name (e.g., 'module.function')
            qualifier := Copy(call.Name, 1, Pos('.', call.Name) - 1);
            identName := Copy(call.Name, Pos('.', call.Name) + 1, MaxInt);
            s := ResolveQualifiedName(qualifier, identName, call.Span);
          end;
        end;

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
          // Check argument count
          if s.IsVarArgs then
          begin
            // Varargs: require at least ParamCount arguments
            if Length(call.Args) < s.ParamCount then
              FDiag.Error(Format('wrong argument count for %s: expected at least %d, got %d', [call.Name, s.ParamCount, Length(call.Args)]), call.Span)
            else
            begin
              // For varargs, only check the fixed parameters
              for i := 0 to s.ParamCount - 1 do
              begin
                atype := CheckExpr(call.Args[i]);
                if (s.ParamTypes[i] <> atUnresolved) and (not TypeEqual(atype, s.ParamTypes[i])) then
                  FDiag.Error(Format('argument %d of %s: expected %s but got %s', [i, call.Name, AurumTypeToStr(s.ParamTypes[i]), AurumTypeToStr(atype)]), call.Args[i].Span);
              end;
              // Check remaining args (varargs)
              for i := s.ParamCount to High(call.Args) do
                CheckExpr(call.Args[i]);
            end;
          end
          else
          begin
            // Non-varargs: exact match required
            if Length(call.Args) <> s.ParamCount then
              FDiag.Error(Format('wrong argument count for %s: expected %d, got %d', [call.Name, s.ParamCount, Length(call.Args)]), call.Span);
            for i := 0 to High(call.Args) do
            begin
              atype := CheckExpr(call.Args[i]);
              if (i < s.ParamCount) and (s.ParamTypes[i] <> atUnresolved) and (not TypeEqual(atype, s.ParamTypes[i])) then
                FDiag.Error(Format('argument %d of %s: expected %s but got %s', [i, call.Name, AurumTypeToStr(s.ParamTypes[i]), AurumTypeToStr(atype)]), call.Args[i].Span);
            end;
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

function TSema.CheckStructLit(sl: TAstStructLit): TAurumType;
var
  idx, i, fi: Integer;
  sd: TAstStructDecl;
  fieldName: string;
  fieldFound: Boolean;
  fieldType, valType: TAurumType;
  usedFields: array of Boolean;
begin
  Result := atUnresolved;
  
  // Lookup struct type by name
  if not Assigned(FStructTypes) then
  begin
    FDiag.Error('no struct types defined', sl.Span);
    Exit;
  end;
  
  idx := FStructTypes.IndexOf(sl.TypeName);
  if idx < 0 then
  begin
    FDiag.Error('unknown struct type: ' + sl.TypeName, sl.Span);
    Exit;
  end;
  
  sd := TAstStructDecl(FStructTypes.Objects[idx]);
  sl.SetStructDecl(sd);
  
  // Track which fields have been initialized
  SetLength(usedFields, Length(sd.Fields));
  for i := 0 to High(usedFields) do
    usedFields[i] := False;
  
  // Check each field initializer
  for i := 0 to High(sl.Fields) do
  begin
    fieldName := sl.Fields[i].Name;
    fieldFound := False;
    
    // Find field in struct
    for fi := 0 to High(sd.Fields) do
    begin
      if sd.Fields[fi].Name = fieldName then
      begin
        fieldFound := True;
        
        // Check for duplicate initialization
        if usedFields[fi] then
        begin
          FDiag.Error('duplicate field initializer: ' + fieldName, sl.Span);
          Continue;
        end;
        usedFields[fi] := True;
        
        // Check value type
        fieldType := sd.Fields[fi].FieldType;
        valType := CheckExpr(sl.Fields[i].Value);
        
        if (fieldType <> atUnresolved) and (not TypeEqual(valType, fieldType)) then
          FDiag.Error(Format('field %s: expected %s but got %s',
            [fieldName, AurumTypeToStr(fieldType), AurumTypeToStr(valType)]), sl.Fields[i].Value.Span);
        
        Break;
      end;
    end;
    
    if not fieldFound then
      FDiag.Error('unknown field in struct literal: ' + fieldName, sl.Span);
  end;
  
  // Note: We don't require all fields to be initialized - missing fields are zero-initialized
  
  Result := atUnresolved; // struct types use atUnresolved + TypeName
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
        // record named type if present
        sym.TypeName := vd.DeclTypeName;
        if (sym.TypeName <> '') and Assigned(FStructTypes) then
        begin
          i := FStructTypes.IndexOf(sym.TypeName);
          if i >= 0 then
            sym.StructDecl := TAstStructDecl(FStructTypes.Objects[i]);
        end;
        // array length metadata
        sym.ArrayLen := vd.ArrayLen;
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
    nkFieldAssign:
      begin
        // field assignment: obj.field := value
        // CheckExpr on the target annotates FieldOffset etc.
        CheckExpr(TAstFieldAssign(stmt).Target);
        vtype := CheckExpr(TAstFieldAssign(stmt).Value);
        // type check: target field type vs value type (when available)
        // for now, just accept - full type inference will come later
      end;
    nkIndexAssign:
      begin
        // index assignment: arr[idx] := value
        // validate target (array/index access)
        CheckExpr(TAstIndexAssign(stmt).Target);
        // validate index is integer
        if not IsIntegerType(CheckExpr(TAstIndexAssign(stmt).Target.Index)) then
          FDiag.Error('array index must be integer', TAstIndexAssign(stmt).Target.Index.Span);
        // validate value
        vtype := CheckExpr(TAstIndexAssign(stmt).Value);
        // type check: element type vs value type
        // for now, just validate types are compatible
        if TAstIndexAssign(stmt).Target.Obj is TAstIdent then
        begin
          s := ResolveSymbol(TAstIdent(TAstIndexAssign(stmt).Target.Obj).Name);
          if Assigned(s) and (s.ArrayLen <> 0) then
          begin
            if not TypeEqual(vtype, s.DeclType) then
              FDiag.Error(Format('index assignment type mismatch: expected %s but got %s',
                [AurumTypeToStr(s.DeclType), AurumTypeToStr(vtype)]), stmt.Span);
          end;
        end;
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

constructor TSema.Create(d: TDiagnostics; um: TUnitManager = nil);
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

  // Freigabe aller verbleibenden Scopes (insbesondere globaler Scope)
  while Length(FScopes) > 0 do
    PopScope;

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

procedure TSema.ComputeStructLayouts;
var
  i, pass, changed, fldIdx: Integer;
  sd: TAstStructDecl;
  totalSize, maxAlign, off, fsize, falign: Integer;
  f: TStructField;
  ok: Boolean;
  idx: Integer;
  other: TAstStructDecl;
  // helper
  function TypeSizeAndAlign(t: TAurumType; out asz, aalign: Integer): Boolean;
  begin
    case t of
      atInt8, atUInt8, atChar, atBool: asz := 1;
      atInt16, atUInt16: asz := 2;
      atInt32, atUInt32, atF32: asz := 4;
      atInt64, atUInt64, atISize, atUSize, atF64, atPChar: asz := 8;
      else
        begin
          asz := 0;
          aalign := 0;
          Exit(False);
        end;
    end;
    aalign := asz;
    Result := True;
  end;
begin
  if not Assigned(FStructTypes) then Exit;
  // iterative fixed-point: try to compute until no change
  pass := 0;
  repeat
    changed := 0;
    Inc(pass);
    for i := 0 to FStructTypes.Count - 1 do
    begin
      sd := TAstStructDecl(FStructTypes.Objects[i]);
      // skip if already computed
      if sd.Size <> 0 then Continue;
      // attempt compute
      totalSize := 0; maxAlign := 1;
      off := 0;
        ok := True;

      for fldIdx := 0 to High(sd.Fields) do
      begin
        f := sd.Fields[fldIdx];
        // determine field size/alignment
        if f.FieldType <> atUnresolved then
        begin
          if not TypeSizeAndAlign(f.FieldType, fsize, falign) then begin ok := False; Break; end;
        end
        else if f.FieldTypeName <> '' then
        begin
            idx := FStructTypes.IndexOf(f.FieldTypeName);
            if idx < 0 then begin ok := False; Break; end;
            other := TAstStructDecl(FStructTypes.Objects[idx]);
            if other.Size = 0 then begin ok := False; Break; end;
            fsize := other.Size;
            falign := other.Align;

        end
        else
        begin
          ok := False; Break;
        end;
        // align current offset
        if falign > maxAlign then maxAlign := falign;
        if (off mod falign) <> 0 then
          off := ((off + falign - 1) div falign) * falign;
        sd.FieldOffsets[fldIdx] := off;
        off := off + fsize;
      end;
        if ok then
        begin
          // final struct align = maxAlign, size rounded up
          sd.SetLayout(off, maxAlign);
          if (off mod sd.Align) <> 0 then
            off := ((off + sd.Align - 1) div sd.Align) * sd.Align;
          sd.SetLayout(off, sd.Align);
          Inc(changed);
        end;

    end;
  until (changed = 0) or (pass > 100);
  // if after iterations some structs remain with Size=0, report error
  for i := 0 to FStructTypes.Count - 1 do
  begin
    sd := TAstStructDecl(FStructTypes.Objects[i]);
    if sd.Size = 0 then
      FDiag.Error('cannot compute layout for struct: ' + sd.Name, sd.Span);
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
        // Nur öffentliche Funktionen importieren
        if not fn.IsPublic then
          Continue;

        // Prüfe auf Konflikte
        if ResolveSymbol(fn.Name) <> nil then
        begin
          FDiag.Error('import conflicts with existing symbol: ' + fn.Name, imp.Span);
          Continue;
        end;

        sym := TSymbol.Create(fn.Name);
        sym.Kind := symFunc;
        sym.DeclType := fn.ReturnType;
        sym.ReturnTypeName := fn.ReturnTypeName;
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
        AddSymbolToCurrent(Result, span);
        Exit;
      end;
    end;
  end;
  
  if Result = nil then
    FDiag.Error('symbol not found in module ' + qualifier + ': ' + name, span);
end;

procedure TSema.Analyze(prog: TAstProgram);
var
  i, j, k, fi: Integer;
  node: TAstNode;
  fn: TAstFuncDecl;
  con: TAstConDecl;
  m: TAstFuncDecl;
  p: TAstParam;
  s: TSymbol;
  sym: TSymbol;
  itype: TAurumType;
begin
  // Phase 0: Verarbeite Imports
  ProcessImports(prog);
  
  // First pass: register top-level functions, constants and struct types
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
       sym.ReturnTypeName := fn.ReturnTypeName;
       // If return type is a struct, look up and store the struct decl
       if (fn.ReturnTypeName <> '') and Assigned(FStructTypes) then
       begin
         fi := FStructTypes.IndexOf(fn.ReturnTypeName);
         if fi >= 0 then
           sym.ReturnStructDecl := TAstStructDecl(FStructTypes.Objects[fi]);
       end;
       sym.ParamCount := Length(fn.Params);
       SetLength(sym.ParamTypes, sym.ParamCount);
       for j := 0 to sym.ParamCount - 1 do
         sym.ParamTypes[j] := fn.Params[j].ParamType;
       AddSymbolToCurrent(sym, fn.Span);
     end
     else if node is TAstStructDecl then
     begin
       // register struct type and its methods as top-level functions (mangled)
       if not Assigned(FStructTypes) then
       begin
         FStructTypes := TStringList.Create;
         FStructTypes.Sorted := False;
       end;
       if FStructTypes.IndexOf(TAstStructDecl(node).Name) >= 0 then
       begin
         FDiag.Error('redeclaration of type: ' + TAstStructDecl(node).Name, node.Span);
         Continue;
       end;
       FStructTypes.AddObject(TAstStructDecl(node).Name, TObject(node));
       // register methods as functions with mangled names
       for j := 0 to High(TAstStructDecl(node).Methods) do
       begin
         m := TAstStructDecl(node).Methods[j];
          
          sym := TSymbol.Create('_L_' + TAstStructDecl(node).Name + '_' + m.Name);
          sym.Kind := symFunc;
          sym.DeclType := m.ReturnType;
          sym.ReturnTypeName := m.ReturnTypeName;
          // Handle 'Self' return type
          if (m.ReturnTypeName = 'Self') or (m.ReturnTypeName = 'self') then
          begin
            sym.ReturnTypeName := TAstStructDecl(node).Name;
            sym.ReturnStructDecl := TAstStructDecl(node);
          end
          else if (m.ReturnTypeName <> '') and Assigned(FStructTypes) then
          begin
            fi := FStructTypes.IndexOf(m.ReturnTypeName);
            if fi >= 0 then
              sym.ReturnStructDecl := TAstStructDecl(FStructTypes.Objects[fi]);
          end;
         
         if m.IsStatic then
         begin
           // Static method: no implicit self parameter
           sym.ParamCount := Length(m.Params);
           SetLength(sym.ParamTypes, sym.ParamCount);
           for k := 0 to High(m.Params) do
             sym.ParamTypes[k] := m.Params[k].ParamType;
         end
         else
         begin
           // Instance method: first param is implicit self
           sym.ParamCount := Length(m.Params) + 1;
           SetLength(sym.ParamTypes, sym.ParamCount);
           sym.ParamTypes[0] := atUnresolved;
           for k := 0 to High(m.Params) do
             sym.ParamTypes[k+1] := m.Params[k].ParamType;
         end;

         AddSymbolToCurrent(sym, m.Span);
       end;
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

  // After registration pass, compute struct layouts before checking bodies
  ComputeStructLayouts;

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

  // Also process methods defined inside structs
  for i := 0 to High(prog.Decls) do
  begin
    node := prog.Decls[i];
    if node is TAstStructDecl then
    begin
      for j := 0 to High(TAstStructDecl(node).Methods) do
      begin
        m := TAstStructDecl(node).Methods[j];
        
        // Resolve 'Self' return type to the owning struct type
        if (m.ReturnTypeName = 'Self') or (m.ReturnTypeName = 'self') then
        begin
          m.ReturnTypeName := TAstStructDecl(node).Name;
          // ReturnType stays atUnresolved since it's a named struct type
        end;
        
        // enter method scope
        PushScope;
        
        // For non-static methods, add implicit self parameter
        if not m.IsStatic then
        begin
          sym := TSymbol.Create('self');
          sym.Kind := symVar;
          sym.DeclType := atUnresolved; // primitive type is unresolved
          sym.TypeName := TAstStructDecl(node).Name; // but we know the struct name
          sym.StructDecl := TAstStructDecl(node); // and the struct declaration
          AddSymbolToCurrent(sym, m.Span);
        end;
        
        // declare method params
        for k := 0 to High(m.Params) do
        begin
          p := m.Params[k];
          sym := TSymbol.Create(p.Name);
          sym.Kind := symVar;
          sym.DeclType := p.ParamType;
          AddSymbolToCurrent(sym, p.Span);
        end;
        // set return type
        FCurrentReturn := m.ReturnType;
        // check body
        CheckStmt(m.Body);
        PopScope;
      end;
    end;
  end;
end;

procedure TSema.AnalyzeWithUnits(prog: TAstProgram; um: TUnitManager);
begin
  FUnitManager := um;
  Analyze(prog);
end;

// ---------------------------------------------------------------
// AST rewrite helpers
// ---------------------------------------------------------------

function TSema.RewriteExpr(expr: TAstExpr): TAstExpr;
var call: TAstCall; i: Integer;
begin
  if expr = nil then Exit(nil);
  // Only handle call-args rewriting for now
  if expr is TAstCall then
  begin
    call := TAstCall(expr);
    for i := 0 to High(call.Args) do
      call.Args[i] := RewriteExpr(call.Args[i]);
  end;
  Result := expr;
end;

function TSema.RewriteStmt(stmt: TAstStmt): TAstStmt;
var newExpr: TAstExpr; newStmt: TAstExprStmt;
begin
  // For now, only rewrite expression statements
  if stmt = nil then Exit(nil);
  if stmt is TAstExprStmt then
  begin
    newExpr := RewriteExpr(TAstExprStmt(stmt).Expr);
    if newExpr <> TAstExprStmt(stmt).Expr then
    begin
      newStmt := TAstExprStmt.Create(newExpr, stmt.Span);
      stmt.Free;
      Exit(newStmt);
    end;
  end;
  Result := stmt;
end;

procedure TSema.RewriteAST(prog: TAstProgram);
var i, j: Integer; fn: TAstFuncDecl;
begin
  if not Assigned(prog) then Exit;
  for i := 0 to High(prog.Decls) do
  begin
    if prog.Decls[i] is TAstFuncDecl then
    begin
      // rewrite statements in function body
      // naive approach: iterate statements and call RewriteStmt
      fn := TAstFuncDecl(prog.Decls[i]);
      if Assigned(fn.Body) then
      begin
        for j := 0 to High(fn.Body.Stmts) do
          fn.Body.Stmts[j] := RewriteStmt(fn.Body.Stmts[j]);
      end;
    end;
  end;
end;

end.
