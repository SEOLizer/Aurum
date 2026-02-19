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
    DeclType: TLyxType;
    DeclTypeName: string; // for named types (e.g., Point)
    // for functions
    ParamTypes: array of TLyxType;
    ParamCount: Integer;
    IsExtern: Boolean;
    HasVarArgs: Boolean;
    CallingConv: string;
    constructor Create(const AName: string);
    destructor Destroy; override;
  end;

  TSema = class
  private
    FDiag: TDiagnostics;
    FScopes: array of TStringList; // each contains name -> TSymbol as object
    FTypeMap: TStringList; // name -> TAstTypeDecl object
    FCurrentReturn: TLyxType;
    procedure PushScope;
    procedure PopScope;
    procedure AddSymbolToCurrent(sym: TSymbol; span: TSourceSpan);
    function ResolveSymbol(const name: string): TSymbol;
    function ResolveTypeDecl(const name: string): TAstTypeDecl;
    procedure DeclareBuiltinFunctions;
    procedure CollectImportedSymbols(um: TUnitManager);
     function TypeEqual(a, b: TLyxType): Boolean;
     function IsCastCompatible(fromType, toType: TLyxType): Boolean;
     function CheckExpr(expr: TAstExpr): TLyxType;
     procedure CheckStmt(stmt: TAstStmt);
     // Resolve function overload by name and argument types
     function ResolveFunction(const name: string; const argTypes: array of TLyxType): TSymbol;
   public

    constructor Create(d: TDiagnostics);
    destructor Destroy; override;
    procedure Analyze(prog: TAstProgram);
    procedure AnalyzeWithUnits(prog: TAstProgram; um: TUnitManager);
  end;

implementation

{ TSymbol }

constructor TSymbol.Create(const AName: string);
begin
  inherited Create;
  Name := AName;
  Kind := symVar;
  DeclType := atUnresolved;
  DeclTypeName := '';
  ParamCount := 0;
  SetLength(ParamTypes, 0);
  IsExtern := False;
  HasVarArgs := False;
  CallingConv := '';
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
  i, j: Integer;
  obj: TObject;
  lst: TList;
begin
  if Length(FScopes) = 0 then Exit;
  sl := FScopes[High(FScopes)];
  // free symbols (handle TSymbol or TList of TSymbol)
  for i := 0 to sl.Count - 1 do
  begin
    obj := sl.Objects[i];
    if obj is TSymbol then
      TSymbol(obj).Free
    else if obj is TList then
    begin
      lst := TList(obj);
      for j := 0 to lst.Count - 1 do
      begin
        TSymbol(lst.Items[j]).Free;
      end;
      lst.Free;
    end
    else
    begin
      // unknown object, free defensively
      obj.Free;
    end;
  end;
  sl.Free;
  SetLength(FScopes, Length(FScopes) - 1);
end;

procedure TSema.AddSymbolToCurrent(sym: TSymbol; span: TSourceSpan);
var
  cur: TStringList;
  idx: Integer;
  existingObj: TObject;
  lst: TList;
  i: Integer;
  existingSym: TSymbol;
begin
  if Length(FScopes) = 0 then
  begin
    FDiag.Error('internal sema error: no scope', span);
    Exit;
  end;
  cur := FScopes[High(FScopes)];
  idx := cur.IndexOf(sym.Name);
  if idx < 0 then
  begin
    // no existing symbol with that name
    cur.AddObject(sym.Name, TObject(sym));
    Exit;
  end;

  // existing entry present: could be a single TSymbol or a TList of TSymbol (overloads)
  existingObj := cur.Objects[idx];
  if existingObj is TSymbol then
  begin
    existingSym := TSymbol(existingObj);
    // if both are functions, allow overloads if signature differs
    if (existingSym.Kind = symFunc) and (sym.Kind = symFunc) then
    begin
      // Check signature collision
      if (existingSym.ParamCount = sym.ParamCount) then
      begin
        // compare parameter types
        for i := 0 to existingSym.ParamCount - 1 do
        begin
          if not TypeEqual(existingSym.ParamTypes[i], sym.ParamTypes[i]) then Break;
        end;
        if i = existingSym.ParamCount then
        begin
          // signatures identical
          FDiag.Error('redeclaration of function with same signature: ' + sym.Name, span);
          sym.Free;
          Exit;
        end;
      end;
      // create a list to hold overloads
      lst := TList.Create;
      lst.Add(existingSym);
      lst.Add(sym);
      cur.Objects[idx] := TObject(lst);
      Exit;
    end
    else
    begin
      // not both functions - redeclaration error
      FDiag.Error('redeclaration of symbol: ' + sym.Name, span);
      sym.Free;
      Exit;
    end;
  end
  else if existingObj is TList then
  begin
    lst := TList(existingObj);
    // check for identical signature among existing overloads
    for i := 0 to lst.Count - 1 do
    begin
      existingSym := TSymbol(lst.Items[i]);
      if (existingSym.ParamCount = sym.ParamCount) and (existingSym.Kind = sym.Kind) then
      begin
        // compare parameter types
        for idx := 0 to existingSym.ParamCount - 1 do
        begin
          if not TypeEqual(existingSym.ParamTypes[idx], sym.ParamTypes[idx]) then Break;
        end;
        if idx = existingSym.ParamCount then
        begin
          FDiag.Error('redeclaration of function with same signature: ' + sym.Name, span);
          sym.Free;
          Exit;
        end;
      end;
    end;
    // append new overload
    lst.Add(sym);
    Exit;
  end
  else
  begin
    // unexpected object type
    FDiag.Error('internal sema error: unexpected symbol object type', span);
    sym.Free;
    Exit;
  end;
end;

 function TSema.ResolveSymbol(const name: string): TSymbol;
 var
   i, idx: Integer;
   sl: TStringList;
   obj: TObject;
   lst: TList;
 begin
   Result := nil;
   for i := High(FScopes) downto 0 do
   begin
     sl := FScopes[i];
     idx := sl.IndexOf(name);
     if idx >= 0 then
     begin
       obj := sl.Objects[idx];
       if obj is TSymbol then
       begin
         Result := TSymbol(obj);
         Exit;
       end
       else if obj is TList then
       begin
         lst := TList(obj);
         if lst.Count > 0 then
         begin
           Result := TSymbol(lst.Items[0]);
           Exit;
         end;
       end;
     end;
   end;
 end;

 function TSema.ResolveTypeDecl(const name: string): TAstTypeDecl;
 var
   idx: Integer;
 begin
   Result := nil;
   if FTypeMap = nil then Exit;
   idx := FTypeMap.IndexOf(name);
   if idx >= 0 then
     Result := TAstTypeDecl(FTypeMap.Objects[idx]);
 end;

procedure TSema.DeclareBuiltinFunctions;
var
  s: TSymbol;
  alias: TSymbol;
begin
  // print_str(pchar) -> void
  s := TSymbol.Create('print_str');
  s.Kind := symFunc;
  s.DeclType := atVoid;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atPChar;
  AddSymbolToCurrent(s, NullSpan);
  // alias: puts -> print_str
  alias := TSymbol.Create('puts');
  alias.Kind := symFunc;
  alias.DeclType := s.DeclType;
  alias.ParamCount := s.ParamCount;
  SetLength(alias.ParamTypes, alias.ParamCount);
  alias.ParamTypes[0] := s.ParamTypes[0];
  AddSymbolToCurrent(alias, NullSpan);

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

  // len(array) -> int64
  s := TSymbol.Create('len');
  s.Kind := symFunc;
  s.DeclType := atInt64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atArray;
  AddSymbolToCurrent(s, NullSpan);

  // strlen(pchar) -> int64
  s := TSymbol.Create('strlen');
  s.Kind := symFunc;
  s.DeclType := atInt64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atPChar;
  AddSymbolToCurrent(s, NullSpan);

  // printf(pchar, ...) -> int64  (varargs)
  s := TSymbol.Create('printf');
  s.Kind := symFunc;
  s.DeclType := atInt64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atPChar;
  s.HasVarArgs := True;
  AddSymbolToCurrent(s, NullSpan);

  // print_float(f64) -> void
  s := TSymbol.Create('print_float');
  s.Kind := symFunc;
  s.DeclType := atVoid;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atF64;
  AddSymbolToCurrent(s, NullSpan);

  // ============================================================================
  // STRING MANIPULATION BUILTINS
  // ============================================================================

  // str_char_at(pchar, int64) -> int64
  s := TSymbol.Create('str_char_at');
  s.Kind := symFunc;
  s.DeclType := atInt64;
  s.ParamCount := 2;
  SetLength(s.ParamTypes, 2);
  s.ParamTypes[0] := atPChar;
  s.ParamTypes[1] := atInt64;
  AddSymbolToCurrent(s, NullSpan);

  // str_set_char(pchar, int64, int64) -> void
  s := TSymbol.Create('str_set_char');
  s.Kind := symFunc;
  s.DeclType := atVoid;
  s.ParamCount := 3;
  SetLength(s.ParamTypes, 3);
  s.ParamTypes[0] := atPChar;  // string
  s.ParamTypes[1] := atInt64;  // index
  s.ParamTypes[2] := atInt64;  // new char value
  AddSymbolToCurrent(s, NullSpan);

  // str_length(pchar) -> int64
  s := TSymbol.Create('str_length');
  s.Kind := symFunc;
  s.DeclType := atInt64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atPChar;
  AddSymbolToCurrent(s, NullSpan);

  // str_compare(pchar, pchar) -> int64
  s := TSymbol.Create('str_compare');
  s.Kind := symFunc;
  s.DeclType := atInt64;
  s.ParamCount := 2;
  SetLength(s.ParamTypes, 2);
  s.ParamTypes[0] := atPChar;
  s.ParamTypes[1] := atPChar;
  AddSymbolToCurrent(s, NullSpan);

  // str_copy_builtin(pchar, pchar) -> void
  s := TSymbol.Create('str_copy_builtin');
  s.Kind := symFunc;
  s.DeclType := atVoid;
  s.ParamCount := 2;
  SetLength(s.ParamTypes, 2);
  s.ParamTypes[0] := atPChar;  // dest
  s.ParamTypes[1] := atPChar;  // src
  AddSymbolToCurrent(s, NullSpan);

  // ============================================================================
  // STRING CONVERSION BUILTINS
  // ============================================================================

  // int_to_str(int64) -> pchar - Convert integer to string
  s := TSymbol.Create('int_to_str');
  s.Kind := symFunc;
  s.DeclType := atPChar;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atInt64;
  AddSymbolToCurrent(s, NullSpan);

  // str_to_int(pchar) -> int64 - Convert string to integer
  s := TSymbol.Create('str_to_int');
  s.Kind := symFunc;
  s.DeclType := atInt64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atPChar;
  AddSymbolToCurrent(s, NullSpan);

  // ============================================================================
  // COMPREHENSIVE MATH BUILTINS (22 functions)
  // ============================================================================

  // abs(int64) -> int64 - Calculate absolute value
  s := TSymbol.Create('abs');
  s.Kind := symFunc;
  s.DeclType := atInt64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atInt64;
  AddSymbolToCurrent(s, NullSpan);

  // fabs(f64) -> f64 - Calculate absolute value (float)
  s := TSymbol.Create('fabs');
  s.Kind := symFunc;
  s.DeclType := atF64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atF64;
  AddSymbolToCurrent(s, NullSpan);

  // arctan(f64) -> f64 - Calculate inverse tangent
  s := TSymbol.Create('arctan');
  s.Kind := symFunc;
  s.DeclType := atF64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atF64;
  AddSymbolToCurrent(s, NullSpan);

  // cos(f64) -> f64 - Calculate cosine of angle
  s := TSymbol.Create('cos');
  s.Kind := symFunc;
  s.DeclType := atF64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atF64;
  AddSymbolToCurrent(s, NullSpan);

  // sin(f64) -> f64 - Calculate sine of angle
  s := TSymbol.Create('sin');
  s.Kind := symFunc;
  s.DeclType := atF64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atF64;
  AddSymbolToCurrent(s, NullSpan);

  // exp(f64) -> f64 - Exponentiate (e^x)
  s := TSymbol.Create('exp');
  s.Kind := symFunc;
  s.DeclType := atF64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atF64;
  AddSymbolToCurrent(s, NullSpan);

  // ln(f64) -> f64 - Calculate natural logarithm
  s := TSymbol.Create('ln');
  s.Kind := symFunc;
  s.DeclType := atF64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atF64;
  AddSymbolToCurrent(s, NullSpan);

  // sqrt(f64) -> f64 - Calculate square root
  s := TSymbol.Create('sqrt');
  s.Kind := symFunc;
  s.DeclType := atF64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atF64;
  AddSymbolToCurrent(s, NullSpan);

  // sqr(f64) -> f64 - Calculate square (x^2)
  s := TSymbol.Create('sqr');
  s.Kind := symFunc;
  s.DeclType := atF64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atF64;
  AddSymbolToCurrent(s, NullSpan);

  // round(f64) -> int64 - Round to nearest integer
  s := TSymbol.Create('round');
  s.Kind := symFunc;
  s.DeclType := atInt64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atF64;
  AddSymbolToCurrent(s, NullSpan);

  // trunc(f64) -> int64 - Truncate to integer
  s := TSymbol.Create('trunc');
  s.Kind := symFunc;
  s.DeclType := atInt64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atF64;
  AddSymbolToCurrent(s, NullSpan);

  // int_part(f64) -> int64 - Calculate integer part
  s := TSymbol.Create('int_part');
  s.Kind := symFunc;
  s.DeclType := atInt64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atF64;
  AddSymbolToCurrent(s, NullSpan);

  // frac(f64) -> f64 - Return fractional part
  s := TSymbol.Create('frac');
  s.Kind := symFunc;
  s.DeclType := atF64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atF64;
  AddSymbolToCurrent(s, NullSpan);

  // pi() -> f64 - Return the value of pi
  s := TSymbol.Create('pi');
  s.Kind := symFunc;
  s.DeclType := atF64;
  s.ParamCount := 0;
  SetLength(s.ParamTypes, 0);
  AddSymbolToCurrent(s, NullSpan);

  // random() -> f64 - Generate random number [0.0, 1.0)
  s := TSymbol.Create('random');
  s.Kind := symFunc;
  s.DeclType := atF64;
  s.ParamCount := 0;
  SetLength(s.ParamTypes, 0);
  AddSymbolToCurrent(s, NullSpan);

  // randomize() -> void - Initialize random number generator
  s := TSymbol.Create('randomize');
  s.Kind := symFunc;
  s.DeclType := atVoid;
  s.ParamCount := 0;
  SetLength(s.ParamTypes, 0);
  AddSymbolToCurrent(s, NullSpan);

  // odd(int64) -> bool - Check if value is odd
  s := TSymbol.Create('odd');
  s.Kind := symFunc;
  s.DeclType := atBool;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atInt64;
  AddSymbolToCurrent(s, NullSpan);

  // hi(int64) -> int64 - Return high 32 bits
  s := TSymbol.Create('hi');
  s.Kind := symFunc;
  s.DeclType := atInt64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atInt64;
  AddSymbolToCurrent(s, NullSpan);

  // lo(int64) -> int64 - Return low 32 bits
  s := TSymbol.Create('lo');
  s.Kind := symFunc;
  s.DeclType := atInt64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atInt64;
  AddSymbolToCurrent(s, NullSpan);

  // swap(int64) -> int64 - Swap high and low 32-bit words
  s := TSymbol.Create('swap');
  s.Kind := symFunc;
  s.DeclType := atInt64;
  s.ParamCount := 1;
  SetLength(s.ParamTypes, 1);
  s.ParamTypes[0] := atInt64;
  AddSymbolToCurrent(s, NullSpan);

  // Note: inc(var x) and dec(var x) require special handling as they modify variables
  // These will be implemented as special parser constructs, not regular function calls
end;

function IsIntegerType(t: TLyxType): Boolean;
begin
  case t of
    atInt8, atInt16, atInt32, atInt64, atUInt8, atUInt16, atUInt32, atUInt64: Result := True;
  else
    Result := False;
  end;
end;

// Resolve function overload by name and argument types
function TSema.ResolveFunction(const name: string; const argTypes: array of TLyxType): TSymbol;
var
  i, idx, j: Integer;
  sl: TStringList;
  obj: TObject;
  sym: TSymbol;
  lst: TList;
  cand: TSymbol;
  argCount: Integer;
  match: Boolean;
begin
  Result := nil;
  argCount := Length(argTypes);
  for i := High(FScopes) downto 0 do
  begin
    sl := FScopes[i];
    idx := sl.IndexOf(name);
    if idx < 0 then Continue;
    obj := sl.Objects[idx];
    if obj is TSymbol then
    begin
      sym := TSymbol(obj);
      if sym.Kind <> symFunc then Continue;
      // check parameter compatibility
      if (not sym.HasVarArgs) and (sym.ParamCount <> argCount) then Continue;
      if sym.HasVarArgs and (argCount < sym.ParamCount) then Continue;
      // check fixed params
      match := True;
      for j := 0 to sym.ParamCount - 1 do
      begin
        if not TypeEqual(sym.ParamTypes[j], argTypes[j]) then
        begin
          match := False;
          Break;
        end;
      end;
      if match then
      begin
        Result := sym;
        Exit;
      end;
    end
    else if obj is TList then
    begin
      lst := TList(obj);
      for j := 0 to lst.Count - 1 do
      begin
        cand := TSymbol(lst.Items[j]);
        if cand.Kind <> symFunc then Continue;
        if (not cand.HasVarArgs) and (cand.ParamCount <> argCount) then Continue;
        if cand.HasVarArgs and (argCount < cand.ParamCount) then Continue;
        match := True;
        // check fixed params
        for idx := 0 to cand.ParamCount - 1 do
        begin
          if not TypeEqual(cand.ParamTypes[idx], argTypes[idx]) then
          begin
            match := False;
            Break;
          end;
        end;
        if match then
        begin
          Result := cand;
          Exit;
        end;
      end;
    end;
  end;
end;

function IsFloatType(t: TLyxType): Boolean;
begin
  case t of
    atF32, atF64: Result := True;
  else
    Result := False;
  end;
end;

function TSema.TypeEqual(a, b: TLyxType): Boolean;
begin
  // exact match
  if a = b then Exit(True);
  // treat any integer widths as compatible for now
  if IsIntegerType(a) and IsIntegerType(b) then Exit(True);
  // allow float type compatibility (f64 can be assigned to f32 variables)
  if IsFloatType(a) and IsFloatType(b) then Exit(True);
  Result := False;
end;

function IsTimeType(t: TLyxType): Boolean;
begin
  Result := (t = atDate) or (t = atTime) or (t = atDateTime) or (t = atTimestamp);
end;

function TSema.IsCastCompatible(fromType, toType: TLyxType): Boolean;
begin
  // Identity cast is always allowed
  if fromType = toType then Exit(True);
  
  // Integer to Integer casts (widening/narrowing)
  if IsIntegerType(fromType) and IsIntegerType(toType) then Exit(True);
  
  // Float to Float casts  
  if IsFloatType(fromType) and IsFloatType(toType) then Exit(True);
  
  // Integer to Float casts
  if IsIntegerType(fromType) and IsFloatType(toType) then Exit(True);
  
  // Float to Integer casts (truncation)
  if IsFloatType(fromType) and IsIntegerType(toType) then Exit(True);
  
  // Integer to/from Time types (time is stored as integer internally)
  if IsIntegerType(fromType) and IsTimeType(toType) then Exit(True);
  if IsTimeType(fromType) and IsIntegerType(toType) then Exit(True);
  
  // Time type to/from other time types
  if IsTimeType(fromType) and IsTimeType(toType) then Exit(True);
  
  // No other casts supported for now (e.g., no pointer casts)
  Result := False;
end;

function TSema.CheckExpr(expr: TAstExpr): TLyxType;
var
  ident: TAstIdent;
  bin: TAstBinOp;
  un: TAstUnaryOp;
  call: TAstCall;
  s: TSymbol;
  i, j: Integer;
  lt, rt, ot, atype: TLyxType;
  elementType: TLyxType;
  castNode: TAstCast;
  sourceType, targetType: TLyxType;
  st: TAstStructLit;
  td: TAstTypeDecl;
  fname: string;
  ftype: TLyxType;
  found: Boolean;
  argTypes: array of TLyxType;
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
               FDiag.Error(Format('array element type mismatch: expected %s but got %s', [LyxTypeToStr(elementType), LyxTypeToStr(atype)]), TAstArrayLit(expr).Items[i].Span);
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
          atype := CheckExpr(TAstArrayIndex(expr).ArrayExpr);
          if not TypeEqual(atype, atArray) then
            FDiag.Error(Format('indexing non-array type: got %s', [LyxTypeToStr(atype)]), TAstArrayIndex(expr).ArrayExpr.Span);
          atype := CheckExpr(TAstArrayIndex(expr).Index);
          if not IsIntegerType(atype) then
            FDiag.Error(Format('array index must be integer, got %s', [LyxTypeToStr(atype)]), TAstArrayIndex(expr).Index.Span);
          Result := atInt64;
        end;
      nkIndexAccess:
        begin
          // Generischer Index-Zugriff: obj[index]
          // Prüfe dass das Objekt ein Array ist
          atype := CheckExpr(TAstIndexAccess(expr).Obj);
          if not TypeEqual(atype, atArray) then
            FDiag.Error(Format('indexing non-array type: got %s', [LyxTypeToStr(atype)]), TAstIndexAccess(expr).Obj.Span);
          
          // Prüfe dass Index ein Integer ist
          atype := CheckExpr(TAstIndexAccess(expr).Index);
          if not IsIntegerType(atype) then
            FDiag.Error(Format('array index must be integer, got %s', [LyxTypeToStr(atype)]), TAstIndexAccess(expr).Index.Span);
          
          // Index-Zugriff gibt Element-Typ zurück (für jetzt: int64)
          Result := atInt64;
        end;
      nkStructLit:
        begin
          st := TAstStructLit(expr);
          td := ResolveTypeDecl(st.TypeName);
          if td = nil then
          begin
            FDiag.Error('use of undeclared type: ' + st.TypeName, st.Span);
            Result := atUnresolved;
            Exit;
          end;
          // For each declared field, ensure provided and type matches
          for i := 0 to High(td.Fields) do
          begin
            fname := td.Fields[i].Name;
            ftype := td.Fields[i].FieldType;
            found := False;
            for j := 0 to st.FieldCount - 1 do
            begin
              if st.GetFieldName(j) = fname then
              begin
                atype := CheckExpr(st.GetFieldValue(j));
                if not TypeEqual(atype, ftype) then
                  FDiag.Error(Format('struct field %s: expected %s but got %s', [fname, LyxTypeToStr(ftype), LyxTypeToStr(atype)]), st.GetFieldValue(j).Span);
                found := True;
                Break;
              end;
            end;
            if not found then
              FDiag.Error('missing field in struct literal: ' + fname, st.Span);
          end;
          Result := atStruct;
        end;
      nkFieldAccess:
        begin
          // Field access: obj.field -> lookup field type in named struct
          ot := CheckExpr(TAstFieldAccess(expr).Obj);
          // Only handle when object is identifier for now
          if TAstFieldAccess(expr).Obj is TAstIdent then
          begin
            s := ResolveSymbol(TAstIdent(TAstFieldAccess(expr).Obj).Name);
            if s = nil then
            begin
              FDiag.Error('field access on undeclared identifier', expr.Span);
              Result := atUnresolved;
              Exit;
            end;
            if s.DeclType = atStruct then
            begin
              if s.DeclTypeName = '' then
              begin
                FDiag.Error('unknown struct type for identifier', expr.Span);
                Result := atUnresolved;
                Exit;
              end;
              td := ResolveTypeDecl(s.DeclTypeName);
              if td = nil then
              begin
                FDiag.Error('use of undeclared type: ' + s.DeclTypeName, expr.Span);
                Result := atUnresolved;
                Exit;
              end;
              // find field
              fname := TAstFieldAccess(expr).Field;
              for i := 0 to High(td.Fields) do
              begin
                if td.Fields[i].Name = fname then
                begin
                  Result := td.Fields[i].FieldType;
                  Exit;
                end;
              end;
              FDiag.Error('unknown field: ' + fname, expr.Span);
              Result := atUnresolved;
              Exit;
            end
            else
            begin
              FDiag.Error(Format('field access on non-struct type: got %s', [LyxTypeToStr(s.DeclType)]), expr.Span);
              Result := atUnresolved;
              Exit;
            end;
          end
          else
          begin
            FDiag.Error('field access on non-identifier object not supported yet', expr.Span);
            Result := atUnresolved;
            Exit;
          end;
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
                // Allow both integer and float arithmetic
                if IsIntegerType(lt) and IsIntegerType(rt) then
                begin
                  // Integer arithmetic - promote to 64-bit for now
                  Result := atInt64;
                end
                else if IsFloatType(lt) and IsFloatType(rt) then
                begin
                  // Float arithmetic - result is f64
                  Result := atF64;
                end
                else
                begin
                  FDiag.Error('type error: arithmetic requires integer or float operands', bin.Span);
                  Result := atInt64;
                end;
              end;
           tkEq, tkNeq, tkLt, tkLe, tkGt, tkGe:
              begin
                if (IsIntegerType(lt) and IsIntegerType(rt)) then
                begin
                  Result := atBool;
                end
                else if (IsFloatType(lt) and IsFloatType(rt)) then
                begin
                  // Float comparison
                  Result := atBool;
                end
                else if (TypeEqual(lt, atPChar) and TypeEqual(rt, atPChar)) then
                begin
                  // pointer/string comparison
                  Result := atBool;
                end
                else
                begin
                  FDiag.Error('type error: comparison requires integer, float, or pchar operands', bin.Span);
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
          if IsIntegerType(ot) then
            Result := atInt64
          else if IsFloatType(ot) then
            Result := atF64
          else
          begin
            FDiag.Error('type error: unary - requires integer or float', un.Span);
            Result := atInt64;
          end;
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
        // Evaluate argument types first
        SetLength(argTypes, Length(call.Args));
        for i := 0 to High(call.Args) do
          argTypes[i] := CheckExpr(call.Args[i]);

        s := ResolveFunction(call.Name, argTypes);
        if s = nil then
        begin
          FDiag.Error('call to undeclared or incompatible function overload: ' + call.Name, call.Span);
          Result := atUnresolved;
        end
        else
        begin
          // Check argument count: varargs functions can have more args than fixed params
          if not s.HasVarArgs and (Length(call.Args) <> s.ParamCount) then
            FDiag.Error(Format('wrong argument count for %s: expected %d, got %d', [call.Name, s.ParamCount, Length(call.Args)]), call.Span)
          else if s.HasVarArgs and (Length(call.Args) < s.ParamCount) then
            FDiag.Error(Format('too few arguments for varargs function %s: expected at least %d, got %d', [call.Name, s.ParamCount, Length(call.Args)]), call.Span);

          // argument type checking: support varargs
          for i := 0 to High(call.Args) do
          begin
            atype := argTypes[i];
            if i < s.ParamCount then
            begin
              if not TypeEqual(atype, s.ParamTypes[i]) then
                FDiag.Error(Format('argument %d of %s: expected %s but got %s', [i, call.Name, LyxTypeToStr(s.ParamTypes[i]), LyxTypeToStr(atype)]), call.Args[i].Span);
            end
            else
            begin
              // extra args: only allowed for varargs functions
              if not s.HasVarArgs then
                FDiag.Error(Format('too many arguments for %s: expected %d, got %d', [call.Name, s.ParamCount, Length(call.Args)]), call.Args[i].Span);
            end;
          end;
          Result := s.DeclType;
        end;

       end;
     nkCast:
       begin
         castNode := TAstCast(expr);
         // Check that the source expression is valid
         sourceType := CheckExpr(castNode.Expr);
         targetType := castNode.TargetType;
         
         // Check cast compatibility
         if IsCastCompatible(sourceType, targetType) then
           Result := targetType
         else
         begin
           FDiag.Error(Format('invalid cast from %s to %s', 
             [LyxTypeToStr(sourceType), LyxTypeToStr(targetType)]), expr.Span);
           Result := atUnresolved;
         end;
       end;
     else
     begin
      // Debug: report unsupported kind
      WriteLn('SEMA DEBUG: unsupported expr kind: ', NodeKindToStr(expr.Kind), ' at ', expr.Span.fileName, ':', expr.Span.line, ',', expr.Span.col);
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
  forNode: TAstFor;
  repNode: TAstRepeatUntil;
  i: Integer;
  s: TSymbol;
  sym: TSymbol;
  vtype, ctype, rtype, atype: TLyxType;
  sw: TAstSwitch;
  caseVal: TAstExpr;
  cvtype: TLyxType;
begin
  if stmt = nil then Exit;

  case stmt.Kind of
    nkVarDecl:
      begin
        vd := TAstVarDecl(stmt);
        // check init expr type
        vtype := CheckExpr(vd.InitExpr);
         if (vd.DeclType <> atUnresolved) and (not TypeEqual(vtype, vd.DeclType)) and (not IsCastCompatible(vtype, vd.DeclType)) then
           FDiag.Error(Format('type mismatch in declaration of %s: expected %s but got %s', [vd.Name, LyxTypeToStr(vd.DeclType), LyxTypeToStr(vtype)]), vd.Span);
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
        // preserve declared type name if present (for named struct types)
        sym.DeclTypeName := vd.DeclTypeName;
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
        if (not TypeEqual(vtype, s.DeclType)) and (not IsCastCompatible(vtype, s.DeclType)) then
          FDiag.Error(Format('assignment type mismatch: %s := %s', [LyxTypeToStr(s.DeclType), LyxTypeToStr(vtype)]), stmt.Span);
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
             FDiag.Error(Format('assignment to non-array variable: got %s', [LyxTypeToStr(s.DeclType)]), arrayAssign.ArrayExpr.Span);
         end
         else
         begin
           FDiag.Error('array assignment requires simple array variable', arrayAssign.ArrayExpr.Span);
         end;
         
         // Prüfe dass Index ein Integer ist
         atype := CheckExpr(arrayAssign.Index);
         if not IsIntegerType(atype) then
           FDiag.Error(Format('array index must be integer, got %s', [LyxTypeToStr(atype)]), arrayAssign.Index.Span);
         
          // Prüfe Value-Typ (für jetzt: muss int64 sein, da Arrays int64-Elemente haben)
          vtype := CheckExpr(arrayAssign.Value);
          if not TypeEqual(vtype, atInt64) then
            FDiag.Error(Format('array assignment type mismatch: expected int64 but got %s', [LyxTypeToStr(vtype)]), arrayAssign.Value.Span);
        end;
    nkFieldAssign:
      begin
        // Field assignment: obj.field := value
        // Just check that the value expression type is valid
        vtype := CheckExpr(TAstFieldAssign(stmt).Value);
        // The actual field type checking happens in the IR lowering
        // For now, just allow any type (will be checked there)
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
        forNode := TAstFor(stmt);
        // Check start and end expressions are integers
        ctype := CheckExpr(forNode.StartExpr);
        if not IsIntegerType(ctype) then
          FDiag.Error('for loop start must be integer', forNode.StartExpr.Span);
        ctype := CheckExpr(forNode.EndExpr);
        if not IsIntegerType(ctype) then
          FDiag.Error('for loop end must be integer', forNode.EndExpr.Span);
        // Declare loop variable in inner scope
        PushScope;
        sym := TSymbol.Create(forNode.VarName);
        sym.Kind := symVar;
        sym.DeclType := atInt64;
        AddSymbolToCurrent(sym, forNode.Span);
        CheckStmt(forNode.Body);
        PopScope;
      end;
    nkRepeatUntil:
      begin
        repNode := TAstRepeatUntil(stmt);
        // Body in eigenem Scope
        PushScope;
        CheckStmt(repNode.Body);
        PopScope;
        // Condition muss bool sein
        ctype := CheckExpr(repNode.Cond);
        if not TypeEqual(ctype, atBool) then
          FDiag.Error('repeat-until condition must be bool', repNode.Cond.Span);
      end;
    nkReturn:
      begin
        ret := TAstReturn(stmt);
        if Assigned(ret.Value) then
        begin
          rtype := CheckExpr(ret.Value);
          if not TypeEqual(rtype, FCurrentReturn) then
            FDiag.Error(Format('return type mismatch: expected %s but got %s', [LyxTypeToStr(FCurrentReturn), LyxTypeToStr(rtype)]), ret.Span);
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
  FTypeMap := TStringList.Create;
  FTypeMap.Sorted := False;
  DeclareBuiltinFunctions;
  FCurrentReturn := atVoid;
end;

destructor TSema.Destroy;
begin
  // Pop and free all scopes and their symbols
  while Length(FScopes) > 0 do
    PopScope;
  if Assigned(FTypeMap) then
    FTypeMap.Free;
  inherited Destroy;
end;

procedure TSema.Analyze(prog: TAstProgram);
var
  i, j: Integer;
  node: TAstNode;
  fn: TAstFuncDecl;
  con: TAstConDecl;
  s: TSymbol;
  sym: TSymbol;
  itype: TLyxType;
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
        // extern / varargs / calling conv
        sym.IsExtern := fn.IsExtern;
        sym.HasVarArgs := fn.IsVarArgs;
        sym.CallingConv := fn.CallingConv;
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
        FDiag.Error(Format('constant %s: expected type %s but got %s', [con.Name, LyxTypeToStr(con.DeclType), LyxTypeToStr(itype)]), con.Span);
      sym := TSymbol.Create(con.Name);
      sym.Kind := symCon;
      sym.DeclType := con.DeclType;
      AddSymbolToCurrent(sym, con.Span);
    end
    else if node is TAstTypeDecl then
    begin
      // register named type
      if FTypeMap.IndexOf(TAstTypeDecl(node).Name) >= 0 then
      begin
        FDiag.Error('redeclaration of type: ' + TAstTypeDecl(node).Name, node.Span);
        Continue;
      end;
      FTypeMap.AddObject(TAstTypeDecl(node).Name, TObject(node));
    end;
  end;

  // Second pass: check function bodies
   for i := 0 to High(prog.Decls) do
   begin
     node := prog.Decls[i];
     if node is TAstFuncDecl then
     begin
       fn := TAstFuncDecl(node);
       // If function is extern or has no body, skip body checking
       if (fn.IsExtern) or (fn.Body = nil) then
         Continue;
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

{ Analyze program with imported units }
procedure TSema.AnalyzeWithUnits(prog: TAstProgram; um: TUnitManager);
begin
  // First collect all public symbols from imported units
  CollectImportedSymbols(um);
  // Then analyze normally
  Analyze(prog);
end;

{ Collect public functions from all loaded units }
procedure TSema.CollectImportedSymbols(um: TUnitManager);
var
  loadedUnit: TLoadedUnit;
  i, j, k: Integer;
  decl: TAstNode;
  fn: TAstFuncDecl;
  sym: TSymbol;
begin
  // Iterate through all loaded units
  for i := 0 to um.Units.Count - 1 do
  begin
    loadedUnit := TLoadedUnit(um.Units.Objects[i]);
    if loadedUnit.AST = nil then Continue;
    
    // Check each declaration in the unit
    for j := 0 to High(loadedUnit.AST.Decls) do
    begin
      decl := loadedUnit.AST.Decls[j];
      
      // Only process public function declarations
      if (decl is TAstFuncDecl) then
      begin
        fn := TAstFuncDecl(decl);
        if fn.IsPublic then
        begin
          // Check for duplicates (could be already imported)
          if ResolveSymbol(fn.Name) <> nil then
            Continue; // Skip duplicates
          
          // Create symbol for imported function
          sym := TSymbol.Create(fn.Name);
          sym.Kind := symFunc;
          sym.DeclType := fn.ReturnType;
          sym.ParamCount := Length(fn.Params);
          SetLength(sym.ParamTypes, sym.ParamCount);
          for k := 0 to sym.ParamCount - 1 do
            sym.ParamTypes[k] := fn.Params[k].ParamType;
          sym.IsExtern := fn.IsExtern;
          sym.HasVarArgs := fn.IsVarArgs;
          sym.CallingConv := fn.CallingConv;
          
          AddSymbolToCurrent(sym, fn.Span);
        end;
      end;
    end;
  end;
end;

end.
