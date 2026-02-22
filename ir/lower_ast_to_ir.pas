{$mode objfpc}{$H+}
unit lower_ast_to_ir;

interface

uses
  SysUtils, Classes,
  ast, ir, diag, lexer, unit_manager;

type
  TConstValue = class
  public
    IsStr: Boolean;
    IntVal: Int64;
    StrVal: string;
  end;

  TIRLowering = class
  private
    FModule: TIRModule;
    FCurrentFunc: TIRFunction;
    FCurrentFuncDecl: TAstFuncDecl;  // current AST function being lowered (for return type info)
    FDiag: TDiagnostics;
    FTempCounter: Integer;
    FLabelCounter: Integer;
    FLocalMap: TStringList; // name -> local index (as object integer)
    FLocalTypes: array of TAurumType; // index -> declared local type
    FLocalElemSize: array of Integer; // index -> element size in bytes for dynamic array locals (0 if not array)
    FLocalIsStruct: array of Boolean; // index -> true if this local is a struct (need address, not value)
    FLocalSlotCount: array of Integer; // index -> number of slots this variable occupies (for structs)
    FConstMap: TStringList; // name -> TConstValue (compile-time constants)
    FLocalConst: array of TConstValue; // per-function local constant values (or nil)
    FBreakStack: TStringList; // stack of break labels
    FStructTypes: TStringList; // struct name -> TAstStructDecl (as object)

    function NewTemp: Integer;
    function NewLabel(const prefix: string): string;
    function AllocLocal(const name: string; aType: TAurumType): Integer;
    function AllocLocalMany(const name: string; aType: TAurumType; count: Integer; isStruct: Boolean = False): Integer;
    function GetLocalType(idx: Integer): TAurumType;
    function ResolveLocal(const name: string): Integer;
    procedure Emit(instr: TIRInstr);

    function LowerStmt(stmt: TAstStmt): Boolean;
    function LowerExpr(expr: TAstExpr): Integer; // returns temp index
    function LowerStructLit(sl: TAstStructLit): Integer; // returns temp with struct address
    procedure LowerStructLitIntoLocal(sl: TAstStructLit; baseLoc: Integer; sd: TAstStructDecl);
    function GetReturnStructDecl: TAstStructDecl; // get struct decl for current func's return type
  public
    constructor Create(modul: TIRModule; diag: TDiagnostics);
    destructor Destroy; override;

    function Lower(prog: TAstProgram): TIRModule;
    procedure LowerImportedUnits(um: TUnitManager);
  end;

implementation

{ Helpers }

function IntToObj(i: Integer): TObject;
begin
  Result := TObject(Pointer(i));
end;

function ObjToInt(o: TObject): Integer;
begin
  Result := Integer(Pointer(o));
end;

{ TIRLowering }

constructor TIRLowering.Create(modul: TIRModule; diag: TDiagnostics);
  begin
    inherited Create;
    FModule := modul;
    FDiag := diag;
    FCurrentFuncDecl := nil;
    FTempCounter := 0;
    FLabelCounter := 0;
    FLocalMap := TStringList.Create;
    FLocalMap.Sorted := False;
    FConstMap := TStringList.Create;
    FConstMap.Sorted := False;
    FBreakStack := TStringList.Create;
    FBreakStack.Sorted := False;
    FStructTypes := TStringList.Create;
    FStructTypes.Sorted := False;
    SetLength(FLocalTypes, 0);
    SetLength(FLocalElemSize, 0);
    SetLength(FLocalIsStruct, 0);
    SetLength(FLocalSlotCount, 0);
    SetLength(FLocalConst, 0);
  end;


destructor TIRLowering.Destroy;
  var
  i: Integer;
begin
  FLocalMap.Free;
  for i := 0 to FConstMap.Count - 1 do
    TObject(FConstMap.Objects[i]).Free;
  FConstMap.Free;
  for i := 0 to Length(FLocalConst)-1 do
    if Assigned(FLocalConst[i]) then FLocalConst[i].Free;
  SetLength(FLocalConst, 0);
  SetLength(FLocalIsStruct, 0);
  FBreakStack.Free;
  // Don't free objects in FStructTypes - they belong to the AST
  FStructTypes.Free;
  inherited Destroy;
end;


function TIRLowering.NewTemp: Integer;
begin
  Result := FTempCounter;
  Inc(FTempCounter);
end;

function TIRLowering.NewLabel(const prefix: string): string;
begin
  Result := Format('%s_%d', [prefix, FLabelCounter]);
  Inc(FLabelCounter);
end;

function TIRLowering.AllocLocal(const name: string; aType: TAurumType): Integer;
var
  idx: Integer;
begin
  idx := FLocalMap.IndexOf(name);
  if idx >= 0 then
  begin
    Result := ObjToInt(FLocalMap.Objects[idx]);
    Exit;
  end;
  Result := FCurrentFunc.LocalCount;
  FCurrentFunc.LocalCount := FCurrentFunc.LocalCount + 1;
  FLocalMap.AddObject(name, IntToObj(Result));
  // ensure FLocalTypes has same length
  SetLength(FLocalTypes, FCurrentFunc.LocalCount);
  FLocalTypes[Result] := aType;
  // ensure FLocalElemSize has same length and initialize to 0
  SetLength(FLocalElemSize, FCurrentFunc.LocalCount);
  FLocalElemSize[Result] := 0;
  // ensure FLocalIsStruct has same length and initialize to false
  SetLength(FLocalIsStruct, FCurrentFunc.LocalCount);
  FLocalIsStruct[Result] := False;
end;

function TIRLowering.AllocLocalMany(const name: string; aType: TAurumType; count: Integer; isStruct: Boolean = False): Integer;
var
  idx, i, base: Integer;
begin
  idx := FLocalMap.IndexOf(name);
  if idx >= 0 then
  begin
    Result := ObjToInt(FLocalMap.Objects[idx]);
    Exit;
  end;
  base := FCurrentFunc.LocalCount;
  FCurrentFunc.LocalCount := FCurrentFunc.LocalCount + count;
  FLocalMap.AddObject(name, IntToObj(base));
  // ensure FLocalTypes has same length
  SetLength(FLocalTypes, FCurrentFunc.LocalCount);
  for i := 0 to count - 1 do
    FLocalTypes[base + i] := aType;
  // ensure FLocalElemSize has same length and initialize entries to 0
  SetLength(FLocalElemSize, FCurrentFunc.LocalCount);
  for i := 0 to count - 1 do
    FLocalElemSize[base + i] := 0;
  // ensure FLocalIsStruct has same length
  SetLength(FLocalIsStruct, FCurrentFunc.LocalCount);
  for i := 0 to count - 1 do
    FLocalIsStruct[base + i] := isStruct and (i = 0); // only mark first slot as struct
  // ensure FLocalSlotCount has same length
  SetLength(FLocalSlotCount, FCurrentFunc.LocalCount);
  FLocalSlotCount[base] := count; // store slot count on first slot
  for i := 1 to count - 1 do
    FLocalSlotCount[base + i] := 0; // other slots don't need count
  Result := base;
end;

function TIRLowering.GetLocalType(idx: Integer): TAurumType;
begin
  if (idx >= 0) and (idx < Length(FLocalTypes)) then
    Result := FLocalTypes[idx]
  else
    Result := atUnresolved;
end;

procedure TIRLowering.Emit(instr: TIRInstr);
begin
  if not Assigned(FCurrentFunc) then
    Exit;
  FCurrentFunc.Emit(instr);
end;

{ Lowering main entry }

function TIRLowering.Lower(prog: TAstProgram): TIRModule;
var
  i: Integer;
  fn: TIRFunction;
  node: TAstNode;
  j: Integer;
  k: Integer;
  m: TAstFuncDecl;
  mangled: string;
  cv: TConstValue;
  instr: TIRInstr;
begin
  instr := Default(TIRInstr);
  // First pass: collect all struct declarations for size lookups
  FStructTypes.Clear;
  for i := 0 to High(prog.Decls) do
  begin
    node := prog.Decls[i];
    if node is TAstStructDecl then
      FStructTypes.AddObject(TAstStructDecl(node).Name, TObject(node));
  end;

  // iterate top-level decls, create functions
  for i := 0 to High(prog.Decls) do
  begin
    node := prog.Decls[i];
    if node is TAstFuncDecl then
    begin
       fn := FModule.AddFunction(TAstFuncDecl(node).Name);
       // Lower function body
       FCurrentFunc := fn;
       FCurrentFuncDecl := TAstFuncDecl(node);
       FLocalMap.Clear;
       FTempCounter := 0;
       fn.ParamCount := Length(TAstFuncDecl(node).Params);
       fn.LocalCount := fn.ParamCount;
       SetLength(FLocalTypes, fn.LocalCount);
       SetLength(FLocalConst, fn.LocalCount);
       for j := 0 to fn.ParamCount - 1 do
       begin
         FLocalMap.AddObject(TAstFuncDecl(node).Params[j].Name, IntToObj(j));
         FLocalTypes[j] := TAstFuncDecl(node).Params[j].ParamType;
         FLocalConst[j] := nil;
       end;

       // lower statements sequentially
       for j := 0 to High(TAstFuncDecl(node).Body.Stmts) do
       begin
         LowerStmt(TAstFuncDecl(node).Body.Stmts[j]);
       end;
       
       // Emit implicit return for void functions if last statement wasn't a return
       if (Length(FCurrentFunc.Instructions) = 0) or 
          (FCurrentFunc.Instructions[High(FCurrentFunc.Instructions)].Op <> irReturn) then
       begin
         instr.Op := irReturn;
         instr.Src1 := -1;
         Emit(instr);
       end;
       
       FCurrentFunc := nil;
       FCurrentFuncDecl := nil;
    end
    else if node is TAstStructDecl then
    begin
      // Lower each method as a top-level mangled function: _L_<Struct>_<Method>
      for j := 0 to High(TAstStructDecl(node).Methods) do
      begin
        m := TAstStructDecl(node).Methods[j];
        mangled := '_L_' + TAstStructDecl(node).Name + '_' + m.Name;
        // create function
        fn := FModule.AddFunction(mangled);
        FCurrentFunc := fn;
        FCurrentFuncDecl := m;  // set current func decl for return type info
        FLocalMap.Clear;
        FTempCounter := 0;
        
        if m.IsStatic then
        begin
          // Static method: no implicit self parameter
          fn.ParamCount := Length(m.Params);
          fn.LocalCount := fn.ParamCount;
          SetLength(FLocalTypes, fn.LocalCount);
          SetLength(FLocalConst, fn.LocalCount);
          SetLength(FLocalIsStruct, fn.LocalCount);
          SetLength(FLocalElemSize, fn.LocalCount);
          // method parameters (no self)
          for k := 0 to High(m.Params) do
          begin
            FLocalMap.AddObject(m.Params[k].Name, IntToObj(k));
            FLocalTypes[k] := m.Params[k].ParamType;
            FLocalConst[k] := nil;
            FLocalIsStruct[k] := False;
            FLocalElemSize[k] := 0;
          end;
        end
        else
        begin
          // Instance method: first param = self
          fn.ParamCount := Length(m.Params) + 1;
          fn.LocalCount := fn.ParamCount;
          SetLength(FLocalTypes, fn.LocalCount);
          SetLength(FLocalConst, fn.LocalCount);
          SetLength(FLocalIsStruct, fn.LocalCount);
          SetLength(FLocalElemSize, fn.LocalCount);
          // implicit self param at index 0
          // Note: self is a pointer to struct passed by caller, NOT a struct on stack
          // So we should NOT mark it as FLocalIsStruct - it's already an address
          FLocalMap.AddObject('self', IntToObj(0));
          FLocalTypes[0] := atUnresolved;
          FLocalConst[0] := nil;
          FLocalIsStruct[0] := False; // self holds address, don't use LEA
          FLocalElemSize[0] := 0;
          // method parameters follow
          for k := 0 to High(m.Params) do
          begin
            FLocalMap.AddObject(m.Params[k].Name, IntToObj(k+1));
            FLocalTypes[k+1] := m.Params[k].ParamType;
            FLocalConst[k+1] := nil;
            FLocalIsStruct[k+1] := False;
            FLocalElemSize[k+1] := 0;
          end;
        end;
        
        // lower body
        for k := 0 to High(m.Body.Stmts) do
          LowerStmt(m.Body.Stmts[k]);
        
        // Emit implicit return for void methods if last statement wasn't a return
        if (Length(FCurrentFunc.Instructions) = 0) or 
           (FCurrentFunc.Instructions[High(FCurrentFunc.Instructions)].Op <> irReturn) then
        begin
          instr.Op := irReturn;
          instr.Src1 := -1;
          Emit(instr);
        end;
        
        FCurrentFunc := nil;
        FCurrentFuncDecl := nil;
      end;
    end
    else if node is TAstConDecl then
    begin
      // register compile-time constant for inline substitution
      cv := TConstValue.Create;
      if TAstConDecl(node).InitExpr is TAstIntLit then
      begin
        cv.IsStr := False;
        cv.IntVal := TAstIntLit(TAstConDecl(node).InitExpr).Value;
      end
      else if TAstConDecl(node).InitExpr is TAstStrLit then
      begin
        cv.IsStr := True;
        cv.StrVal := TAstStrLit(TAstConDecl(node).InitExpr).Value;
      end
      else if TAstConDecl(node).InitExpr is TAstBoolLit then
      begin
        cv.IsStr := False;
        if TAstBoolLit(TAstConDecl(node).InitExpr).Value then
          cv.IntVal := 1
        else
          cv.IntVal := 0;
      end
      else
      begin
        FDiag.Error('con initializer must be a literal', TAstConDecl(node).Span);
        cv.Free;
        Continue;
      end;
      FConstMap.AddObject(TAstConDecl(node).Name, TObject(cv));
    end;
  end;
  Result := FModule;
end;

procedure TIRLowering.LowerImportedUnits(um: TUnitManager);
{ Lower all functions from imported units }
var
  i, j, k: Integer;
  loadedUnit: TLoadedUnit;
  node: TAstNode;
  fn: TIRFunction;
  unitAST: TAstProgram;
begin
  if not Assigned(um) then Exit;

  for i := 0 to um.Units.Count - 1 do
  begin
    loadedUnit := TLoadedUnit(um.Units.Objects[i]);
    if not Assigned(loadedUnit) or not Assigned(loadedUnit.AST) then
      Continue;

    unitAST := loadedUnit.AST;

    // Lower all function declarations from this unit
    for j := 0 to High(unitAST.Decls) do
    begin
      node := unitAST.Decls[j];
      if node is TAstFuncDecl then
      begin
        // Only lower public functions from imported units
        if not TAstFuncDecl(node).IsPublic then
          Continue;

        // Check if function already exists (avoid duplicates)
        fn := FModule.FindFunction(TAstFuncDecl(node).Name);
        if not Assigned(fn) then
        begin
          fn := FModule.AddFunction(TAstFuncDecl(node).Name);
          FCurrentFunc := fn;
          FCurrentFuncDecl := TAstFuncDecl(node);
          FLocalMap.Clear;
          FTempCounter := 0;
          fn.ParamCount := Length(TAstFuncDecl(node).Params);
          fn.LocalCount := fn.ParamCount;
          SetLength(FLocalTypes, fn.LocalCount);
          SetLength(FLocalConst, fn.LocalCount);

          for k := 0 to fn.ParamCount - 1 do
          begin
            FLocalMap.AddObject(TAstFuncDecl(node).Params[k].Name, IntToObj(k));
            FLocalTypes[k] := TAstFuncDecl(node).Params[k].ParamType;
            FLocalConst[k] := nil;
          end;

          // Lower statements
          if Assigned(TAstFuncDecl(node).Body) then
            for k := 0 to High(TAstFuncDecl(node).Body.Stmts) do
              LowerStmt(TAstFuncDecl(node).Body.Stmts[k]);

          FCurrentFunc := nil;
          FCurrentFuncDecl := nil;
        end;
      end;
    end;
  end;
end;

{ Lowering helpers }

// Returns struct decl for current function's return type, or nil if not a struct
function TIRLowering.GetReturnStructDecl: TAstStructDecl;
var
  idx: Integer;
  typeName: string;
begin
  Result := nil;
  if not Assigned(FCurrentFuncDecl) then
    Exit;
  
  typeName := FCurrentFuncDecl.ReturnTypeName;
  if typeName = '' then
    Exit;
  
  // Handle 'Self' (should already be resolved in sema, but just in case)
  if (typeName = 'Self') or (typeName = 'self') then
    Exit; // Can't resolve without struct context here
  
  idx := FStructTypes.IndexOf(typeName);
  if idx >= 0 then
    Result := TAstStructDecl(FStructTypes.Objects[idx]);
end;

function TIRLowering.ResolveLocal(const name: string): Integer;
var
  idx: Integer;
begin
  idx := FLocalMap.IndexOf(name);
  if idx >= 0 then
    Result := ObjToInt(FLocalMap.Objects[idx])
  else
    Result := -1;
end;


function TIRLowering.LowerExpr(expr: TAstExpr): Integer;
  var
    instr: TIRInstr;
    loc: Integer;
    tmp: Integer;
    condTmp: Integer;
    t0, t1, t2: Integer;
    i: Integer;
    strIdx: Integer;
    cv: TConstValue;
    argCount: Integer;
    argTemps: array of Integer;
    fn: TIRFunction;
    ltype: TAurumType;
    width: Integer;
    w: Integer;
    lit: Int64;
    mask64: UInt64;
    truncated: UInt64;
    half: UInt64;
    signedVal: Int64;
    cvLocal: TConstValue;
    vd: TAstVarDecl;
    items: TAstExprList;
    elemSize: Integer;
    fa: TAstFieldAssign;
    fldOffset: Integer;
    ownerName: string;
    slotCount: Integer;
  begin
  Result := -1;
  if not Assigned(expr) then
    Exit;

  // Initialize instruction
  instr := Default(TIRInstr);

  case expr.Kind of
    nkIntLit:
      begin
        // Emit integer constant
        t0 := NewTemp;
        instr.Op := irConstInt;
        instr.Dest := t0;
        instr.ImmInt := TAstIntLit(expr).Value;
        Emit(instr);
        Result := t0;
      end;

    nkStrLit:
      begin
        // Intern string and emit reference
        strIdx := FModule.InternString(TAstStrLit(expr).Value);
        t0 := NewTemp;
        instr.Op := irConstStr;
        instr.Dest := t0;
        instr.ImmStr := IntToStr(strIdx);
        Emit(instr);
        Result := t0;
      end;

    nkBoolLit:
      begin
        // Emit boolean as 0 or 1
        t0 := NewTemp;
        instr.Op := irConstInt;
        instr.Dest := t0;
        if TAstBoolLit(expr).Value then
          instr.ImmInt := 1
        else
          instr.ImmInt := 0;
        Emit(instr);
        Result := t0;
      end;

    nkCharLit:
      begin
        // Emit char as integer constant
        t0 := NewTemp;
        instr.Op := irConstInt;
        instr.Dest := t0;
        instr.ImmInt := Ord(TAstCharLit(expr).Value);
        Emit(instr);
        Result := t0;
      end;

    nkIdent:
      begin
        // Look up local variable
        loc := ResolveLocal(TAstIdent(expr).Name);
        if loc < 0 then
        begin
          // Check if it's a compile-time constant
          i := FConstMap.IndexOf(TAstIdent(expr).Name);
          if i >= 0 then
          begin
            cv := TConstValue(FConstMap.Objects[i]);
            t0 := NewTemp;
            if cv.IsStr then
            begin
              strIdx := FModule.InternString(cv.StrVal);
              instr.Op := irConstStr;
              instr.Dest := t0;
              instr.ImmStr := IntToStr(strIdx);
            end
            else
            begin
              instr.Op := irConstInt;
              instr.Dest := t0;
              instr.ImmInt := cv.IntVal;
            end;
            Emit(instr);
            Result := t0;
          end
          else
          begin
            FDiag.Error('undefined identifier: ' + TAstIdent(expr).Name, expr.Span);
            Exit;
          end;
        end
        else
        begin
          // Check for const-folded local
          if (loc < Length(FLocalConst)) and Assigned(FLocalConst[loc]) then
          begin
            cv := FLocalConst[loc];
            t0 := NewTemp;
            instr.Op := irConstInt;
            instr.Dest := t0;
            instr.ImmInt := cv.IntVal;
            Emit(instr);
            Result := t0;
          end
          else
          begin
            // Check if this local is a struct - if so, load address instead of value
            if (loc < Length(FLocalIsStruct)) and FLocalIsStruct[loc] then
            begin
              // Struct local: load base address for field access
              // Need to know struct size to calculate correct base address
              slotCount := 1;
              if loc < Length(FLocalSlotCount) then
                slotCount := FLocalSlotCount[loc];
              if slotCount < 1 then slotCount := 1;
              
              t0 := NewTemp;
              instr.Op := irLoadStructAddr;
              instr.Dest := t0;
              instr.Src1 := loc;
              instr.StructSize := slotCount * 8; // size in bytes
              Emit(instr);
              Result := t0;
            end
            else
            begin
              // Load local into temp
              t0 := NewTemp;
              instr.Op := irLoadLocal;
              instr.Dest := t0;
              instr.Src1 := loc;
              Emit(instr);
              // If local type is narrower than 64 bits, emit sign- or zero-extend
              ltype := GetLocalType(loc);
              if (ltype <> atUnresolved) and (ltype <> atInt64) and (ltype <> atUInt64) then
              begin
                width := 64;
                case ltype of
                  atInt8, atUInt8: width := 8;
                  atInt16, atUInt16: width := 16;
                  atInt32, atUInt32: width := 32;
                end;
                if ltype in [atInt8, atInt16, atInt32] then
                begin
                  instr.Op := irSExt; instr.Dest := NewTemp; instr.Src1 := t0; instr.ImmInt := width; Emit(instr);
                  Result := instr.Dest;
                end
                else if ltype in [atUInt8, atUInt16, atUInt32] then
                begin
                  instr.Op := irZExt; instr.Dest := NewTemp; instr.Src1 := t0; instr.ImmInt := width; Emit(instr);
                  Result := instr.Dest;
                end
                else
                  Result := t0;
              end
              else
                Result := t0;
            end; // end else (non-struct local)
          end; // end else (non-const-folded)
        end; // end else (local found)
      end;

    nkBinOp:
      begin
        // Lower left and right operands
        t1 := LowerExpr(TAstBinOp(expr).Left);
        t2 := LowerExpr(TAstBinOp(expr).Right);
        if (t1 < 0) or (t2 < 0) then
          Exit;

        t0 := NewTemp;
        case TAstBinOp(expr).Op of
          tkPlus:  instr.Op := irAdd;
          tkMinus: instr.Op := irSub;
          tkStar:  instr.Op := irMul;
          tkSlash: instr.Op := irDiv;
          tkPercent: instr.Op := irMod;
          tkEq:    instr.Op := irCmpEq;
          tkNeq:   instr.Op := irCmpNeq;
          tkLt:    instr.Op := irCmpLt;
          tkLe:    instr.Op := irCmpLe;
          tkGt:    instr.Op := irCmpGt;
          tkGe:    instr.Op := irCmpGe;
          tkAnd:   instr.Op := irAnd;
          tkOr:    instr.Op := irOr;
          tkNor:   instr.Op := irNor;
          tkXor:   instr.Op := irXor;
        else
          FDiag.Error('unsupported binary operator', expr.Span);
          Exit;
        end;
        instr.Dest := t0;
        instr.Src1 := t1;
        instr.Src2 := t2;
        Emit(instr);
        Result := t0;
      end;

    nkUnaryOp:
      begin
        // Lower operand
        t1 := LowerExpr(TAstUnaryOp(expr).Operand);
        if t1 < 0 then
          Exit;

        t0 := NewTemp;
        case TAstUnaryOp(expr).Op of
          tkMinus:
            begin
              instr.Op := irNeg;
              instr.Dest := t0;
              instr.Src1 := t1;
              Emit(instr);
            end;
          tkNot:
            begin
              instr.Op := irNot;
              instr.Dest := t0;
              instr.Src1 := t1;
              Emit(instr);
            end;
        else
          FDiag.Error('unsupported unary operator', expr.Span);
          Exit;
        end;
        Result := t0;
      end;

    nkCall:
      begin
        // Lower arguments
        argCount := Length(TAstCall(expr).Args);
        SetLength(argTemps, argCount);
        for i := 0 to argCount - 1 do
          argTemps[i] := LowerExpr(TAstCall(expr).Args[i]);

        // Check for builtins
        if TAstCall(expr).Name = 'PrintStr' then
        begin
          instr.Op := irCallBuiltin;
          instr.Dest := -1;
          instr.ImmStr := 'PrintStr';
          if argCount >= 1 then
            instr.Src1 := argTemps[0]
          else
            instr.Src1 := -1;
          instr.ImmInt := argCount;
          SetLength(instr.ArgTemps, argCount);
          for i := 0 to argCount - 1 do
            instr.ArgTemps[i] := argTemps[i];
          Emit(instr);
          Result := -1;
        end
        else if TAstCall(expr).Name = 'PrintInt' then
        begin
          instr.Op := irCallBuiltin;
          instr.Dest := -1;
          instr.ImmStr := 'PrintInt';
          if argCount >= 1 then
            instr.Src1 := argTemps[0]
          else
            instr.Src1 := -1;
          instr.ImmInt := argCount;
          SetLength(instr.ArgTemps, argCount);
          for i := 0 to argCount - 1 do
            instr.ArgTemps[i] := argTemps[i];
          Emit(instr);
          Result := -1;
        end
        else if TAstCall(expr).Name = 'printf' then
        begin
          // printf is varargs - emit as builtin with all args
          instr.Op := irCallBuiltin;
          instr.Dest := -1;
          instr.ImmStr := 'printf';
          instr.ImmInt := argCount;
          SetLength(instr.ArgTemps, argCount);
          for i := 0 to argCount - 1 do
            instr.ArgTemps[i] := argTemps[i];
          if argCount >= 1 then
            instr.Src1 := argTemps[0]
          else
            instr.Src1 := -1;
          Emit(instr);
          Result := -1;
        end
        else if TAstCall(expr).Name = 'exit' then
        begin
          instr.Op := irCallBuiltin;
          instr.Dest := -1;
          instr.ImmStr := 'exit';
          instr.ImmInt := argCount;
          SetLength(instr.ArgTemps, argCount);
          for i := 0 to argCount - 1 do
            instr.ArgTemps[i] := argTemps[i];
          if argCount >= 1 then
            instr.Src1 := argTemps[0]
          else
            instr.Src1 := -1;
          Emit(instr);
          Result := -1;
        end
        else
        begin
          // Regular function call
          t0 := NewTemp;
          instr.Op := irCall;
          instr.Dest := t0;
          instr.ImmStr := TAstCall(expr).Name;
          instr.ImmInt := argCount; // Backend needs argCount in ImmInt
          instr.CallMode := cmInternal; // Default to internal
          SetLength(instr.ArgTemps, argCount);
          for i := 0 to argCount - 1 do
            instr.ArgTemps[i] := argTemps[i];
          Emit(instr);
          Result := t0;
        end;
      end;

    nkIndexAccess:
      begin
        // Lower array and index
        t1 := LowerExpr(TAstIndexAccess(expr).Obj);
        t2 := LowerExpr(TAstIndexAccess(expr).Index);
        if (t1 < 0) or (t2 < 0) then
          Exit;

        t0 := NewTemp;
        instr.Op := irLoadElem;
        instr.Dest := t0;
        instr.Src1 := t1;  // array base
        instr.Src2 := t2;  // index
        Emit(instr);
        Result := t0;
      end;

    nkFieldAccess:
      begin
        // Lower object
        t1 := LowerExpr(TAstFieldAccess(expr).Obj);
        if t1 < 0 then
          Exit;

        // If sema annotated the field offset on the AST node, use it
        fldOffset := TAstFieldAccess(expr).FieldOffset;
        ownerName := TAstFieldAccess(expr).OwnerName;

        t0 := NewTemp;
        if fldOffset >= 0 then
        begin
          instr.Op := irLoadField;
          instr.Dest := t0;
          instr.Src1 := t1;
          instr.ImmInt := fldOffset; // use ImmInt for immediate offset
          Emit(instr);
        end
        else
        begin
          // Fallback to name-based access (slower / requires runtime lookup)
          instr.Op := irLoadField;
          instr.Dest := t0;
          instr.Src1 := t1;
          instr.LabelName := TAstFieldAccess(expr).Field;
          Emit(instr);
        end;
        Result := t0;
      end;

    nkArrayLit:
      begin
        // Array literals are typically handled in statement context
        // Return first element temp for now (or error)
        if Length(TAstArrayLit(expr).Items) > 0 then
          Result := LowerExpr(TAstArrayLit(expr).Items[0])
        else
          Result := -1;
      end;

    nkStructLit:
      begin
        // Struct literal: TypeName { field1: val1, field2: val2, ... }
        // Allocate stack space for the struct, initialize fields, return address
        Result := LowerStructLit(TAstStructLit(expr));
      end;

  else
    FDiag.Error('lowering: unsupported expression kind', expr.Span);
    Result := -1;
  end;
end;

function TIRLowering.LowerStructLit(sl: TAstStructLit): Integer;
var
  instr: TIRInstr;
  sd: TAstStructDecl;
  baseLoc, slotsNeeded: Integer;
  i, fi, fldOffset, valTemp, addrTemp: Integer;
  fieldName: string;
  fieldFound: Boolean;
begin
  Result := -1;
  instr := Default(TIRInstr);
  
  // Get struct declaration (should have been set by sema)
  sd := sl.StructDecl;
  if not Assigned(sd) then
  begin
    // Try to look it up
    fi := FStructTypes.IndexOf(sl.TypeName);
    if fi < 0 then
    begin
      FDiag.Error('unknown struct type in literal: ' + sl.TypeName, sl.Span);
      Exit;
    end;
    sd := TAstStructDecl(FStructTypes.Objects[fi]);
  end;
  
  // Allocate stack slots for the struct
  slotsNeeded := (sd.Size + 7) div 8;
  if slotsNeeded < 1 then slotsNeeded := 1;
  
  // Use an anonymous name for the temporary struct
  baseLoc := AllocLocalMany('_structlit_' + IntToStr(FTempCounter), atUnresolved, slotsNeeded, True);
  
  // Zero-initialize first slot (other slots will be written to)
  addrTemp := NewTemp;
  instr.Op := irConstInt;
  instr.Dest := addrTemp;
  instr.ImmInt := 0;
  Emit(instr);
  instr.Op := irStoreLocal;
  instr.Dest := baseLoc;
  instr.Src1 := addrTemp;
  Emit(instr);
  
  // Get address of the struct
  addrTemp := NewTemp;
  instr.Op := irLoadLocalAddr;
  instr.Dest := addrTemp;
  instr.Src1 := baseLoc;
  Emit(instr);
  
  // Initialize each field from the literal
  for i := 0 to High(sl.Fields) do
  begin
    fieldName := sl.Fields[i].Name;
    fieldFound := False;
    
    // Find field in struct declaration
    for fi := 0 to High(sd.Fields) do
    begin
      if sd.Fields[fi].Name = fieldName then
      begin
        fieldFound := True;
        fldOffset := sd.FieldOffsets[fi];
        
        // Lower the value expression
        valTemp := LowerExpr(sl.Fields[i].Value);
        if valTemp < 0 then Continue;
        
        // Store value at field offset
        instr.Op := irStoreField;
        instr.Src1 := addrTemp;  // base address
        instr.Src2 := valTemp;   // value
        instr.ImmInt := fldOffset;
        Emit(instr);
        
        Break;
      end;
    end;
    
    if not fieldFound then
      FDiag.Error('unknown field in struct literal: ' + fieldName, sl.Span);
  end;
  
  // Return the address temp
  Result := addrTemp;
end;

procedure TIRLowering.LowerStructLitIntoLocal(sl: TAstStructLit; baseLoc: Integer; sd: TAstStructDecl);
var
  instr: TIRInstr;
  i, fi, fldOffset, valTemp, addrTemp: Integer;
  fieldName: string;
  fieldFound: Boolean;
begin
  instr := Default(TIRInstr);
  
  // Get address of the local struct
  addrTemp := NewTemp;
  instr.Op := irLoadLocalAddr;
  instr.Dest := addrTemp;
  instr.Src1 := baseLoc;
  Emit(instr);
  
  // Initialize each field from the literal
  for i := 0 to High(sl.Fields) do
  begin
    fieldName := sl.Fields[i].Name;
    fieldFound := False;
    
    // Find field in struct declaration
    for fi := 0 to High(sd.Fields) do
    begin
      if sd.Fields[fi].Name = fieldName then
      begin
        fieldFound := True;
        fldOffset := sd.FieldOffsets[fi];
        
        // Lower the value expression
        valTemp := LowerExpr(sl.Fields[i].Value);
        if valTemp < 0 then Continue;
        
        // Store value at field offset
        instr.Op := irStoreField;
        instr.Src1 := addrTemp;  // base address
        instr.Src2 := valTemp;   // value
        instr.ImmInt := fldOffset;
        Emit(instr);
        
        Break;
      end;
    end;
    
    if not fieldFound then
      FDiag.Error('unknown field in struct literal: ' + fieldName, sl.Span);
  end;
end;

function TIRLowering.LowerStmt(stmt: TAstStmt): Boolean;
  var
    instr: TIRInstr;
    loc: Integer;
    tmp: Integer;
    condTmp: Integer;
    t0, t1, t2: Integer;
    thenLabel, elseLabel, endLabel: string;
    whileNode: TAstWhile;
    startLabel, bodyLabel, exitLabel: string;
    i, k: Integer;
    sw: TAstSwitch;
    switchTmp: Integer;
    endLbl, defaultLbl: string;
    caseLabels: TStringList;
    lbl: string;
    caseTmp: Integer;
    ltype: TAurumType;
    width: Integer;
    w: Integer;
    lit: Int64;
    mask64: UInt64;
    truncated: UInt64;
    half: UInt64;
    signedVal: Int64;
    cvLocal: TConstValue;
    vd: TAstVarDecl;
    items: TAstExprList;
    elemSize: Integer;
    fa: TAstFieldAssign;
    retStructDecl: TAstStructDecl;
    call: TAstCall;
    argCount: Integer;
    argTemps: array of Integer;
  begin
  instr := Default(TIRInstr);
  Result := True;
   if stmt is TAstVarDecl then
    begin
      vd := TAstVarDecl(stmt);
      if vd.ArrayLen > 0 then
      begin
        // static array: allocate consecutive locals and initialize per-item
        loc := AllocLocalMany(vd.Name, vd.DeclType, vd.ArrayLen);
        if vd.InitExpr is TAstArrayLit then
        begin
          items := TAstArrayLit(vd.InitExpr).Items;
          if Length(items) <> vd.ArrayLen then
            FDiag.Error('array literal length mismatch', vd.Span)
          else
          begin
            for i := 0 to High(items) do
            begin
              tmp := LowerExpr(items[i]);
              // store into base + i
              instr.Op := irStoreLocal; instr.Dest := loc + i; instr.Src1 := tmp; Emit(instr);
            end;
          end;
        end
        else
        begin
          // initializer not an array literal: try to lower single expression into first element
          tmp := LowerExpr(vd.InitExpr);
          instr.Op := irStoreLocal; instr.Dest := loc; instr.Src1 := tmp; Emit(instr);
        end;
        Exit(True);
      end
      else if vd.ArrayLen = -1 then
      begin

        // dynamic array: represent as single local pointer (pchar/int64)
        loc := AllocLocal(vd.Name, atPChar);
        // record element size for this local slot (needed by backend for element addressing)
          begin
            elemSize := 8; // default
            case vd.DeclType of
              atInt8, atUInt8: elemSize := 1;
              atInt16, atUInt16: elemSize := 2;
              atInt32, atUInt32: elemSize := 4;
              atInt64, atUInt64: elemSize := 8;
              atChar: elemSize := 1;
              atPChar: elemSize := 8;
            else
              elemSize := 8; // conservative default
            end;
            if loc >= Length(FLocalElemSize) then SetLength(FLocalElemSize, loc+1);
            FLocalElemSize[loc] := elemSize;
          end;

        // initializer: if empty array literal -> set nil (0)
        if vd.InitExpr is TAstArrayLit then
        begin
          // only allow empty literal for now
          if Length(TAstArrayLit(vd.InitExpr).Items) <> 0 then
            FDiag.Error('cannot initialize dynamic array with non-empty literal', vd.Span);
           // emit const 0 -> store
           t0 := NewTemp;
           instr.Op := irConstInt; instr.Dest := t0; instr.ImmInt := 0; Emit(instr);
           instr.Op := irStoreLocal; instr.Dest := loc; instr.Src1 := t0; Emit(instr);

        end

       else
       begin
         tmp := LowerExpr(vd.InitExpr);
         instr.Op := irStoreLocal; instr.Dest := loc; instr.Src1 := tmp; Emit(instr);
       end;
       Exit(True);
     end
     else if (vd.DeclTypeName <> '') and (FStructTypes.IndexOf(vd.DeclTypeName) >= 0) then
     begin
       // Named struct type: allocate slots for the whole struct
       // Size is in bytes, each slot is 8 bytes
       i := FStructTypes.IndexOf(vd.DeclTypeName);
       if TAstStructDecl(FStructTypes.Objects[i]).Size > 0 then
       begin
         loc := AllocLocalMany(vd.Name, atUnresolved, 
           (TAstStructDecl(FStructTypes.Objects[i]).Size + 7) div 8, True);
       end
       else
       begin
         // fallback: 1 slot
         loc := AllocLocal(vd.Name, atUnresolved);
         FLocalIsStruct[loc] := True;
       end;
       // Initialize based on InitExpr type
       if (vd.InitExpr is TAstIntLit) and (TAstIntLit(vd.InitExpr).Value = 0) then
       begin
         // Zero initialization
         t0 := NewTemp;
         instr.Op := irConstInt; instr.Dest := t0; instr.ImmInt := 0; Emit(instr);
         instr.Op := irStoreLocal; instr.Dest := loc; instr.Src1 := t0; Emit(instr);
       end
      else if vd.InitExpr is TAstStructLit then
        begin
          // Struct literal: initialize fields directly into the variable's stack slots
          LowerStructLitIntoLocal(TAstStructLit(vd.InitExpr), loc, TAstStructDecl(FStructTypes.Objects[i]));
        end
        else if vd.InitExpr is TAstCall then
        begin
          // Call returning struct: use irCallStruct to handle RAX+RDX properly
          call := TAstCall(vd.InitExpr);
          argCount := Length(call.Args);
          SetLength(argTemps, argCount);
          for k := 0 to argCount - 1 do
            argTemps[k] := LowerExpr(call.Args[k]);
          
          // Emit irCallStruct with struct size info
          instr.Op := irCallStruct;
          instr.Dest := loc;  // destination is the struct variable slot
          instr.ImmStr := call.Name;
          instr.ImmInt := argCount;
          instr.StructSize := TAstStructDecl(FStructTypes.Objects[i]).Size;
          instr.CallMode := cmInternal;
          SetLength(instr.ArgTemps, argCount);
          for k := 0 to argCount - 1 do
            instr.ArgTemps[k] := argTemps[k];
          Emit(instr);
        end;
        Exit(True);
      end
     else
     begin
       // scalar local
       loc := AllocLocal(vd.Name, vd.DeclType);
       // If initializer is constant integer and the local has narrower signed width, constant fold
       if (vd.InitExpr is TAstIntLit) then
       begin
         lit := TAstIntLit(vd.InitExpr).Value;
         ltype := GetLocalType(loc);
         if (ltype <> atUnresolved) and (ltype <> atInt64) then
         begin
           // determine width in bits
           width := 64;
           case ltype of
             atInt8, atUInt8: width := 8;
             atInt16, atUInt16: width := 16;
             atInt32, atUInt32: width := 32;
             atInt64, atUInt64: width := 64;
           end;
           mask64 := (UInt64(1) shl width) - 1;
           truncated := UInt64(lit) and mask64;
           if (ltype in [atInt8, atInt16, atInt32, atInt64]) then
           begin
             // signed interpretation
             half := UInt64(1) shl (width - 1);
             if truncated >= half then
               signedVal := Int64(truncated) - Int64(UInt64(1) shl width)
             else
               signedVal := Int64(truncated);
             // record local constant for future loads instead of emitting store
             cvLocal := TConstValue.Create;
             cvLocal.IsStr := False;
             cvLocal.IntVal := signedVal;
             if loc >= Length(FLocalConst) then SetLength(FLocalConst, loc+1);
             FLocalConst[loc] := cvLocal;
           end
           else
           begin
             // unsigned: record local constant zero-extended value
             cvLocal := TConstValue.Create;
             cvLocal.IsStr := False;
             cvLocal.IntVal := Int64(truncated);
             if loc >= Length(FLocalConst) then SetLength(FLocalConst, loc+1);
             FLocalConst[loc] := cvLocal;
           end;
           Exit(True);
         end;
       end;
       tmp := LowerExpr(vd.InitExpr);
       // If local has narrower integer width, truncate before store
       ltype := GetLocalType(loc);
       if (ltype <> atUnresolved) and (ltype <> atInt64) then
       begin
         // determine width in bits
         width := 64;
         case ltype of
           atInt8, atUInt8: width := 8;
           atInt16, atUInt16: width := 16;
           atInt32, atUInt32: width := 32;
           atInt64, atUInt64: width := 64;
         end;
         instr.Op := irTrunc; instr.Dest := NewTemp; instr.Src1 := tmp; instr.ImmInt := width; Emit(instr);
         tmp := instr.Dest;
       end;
       instr.Op := irStoreLocal;
       instr.Dest := loc;
       instr.Src1 := tmp;
       Emit(instr);
       Exit(True);
     end;
   end;


    if stmt is TAstAssign then
    begin
      loc := ResolveLocal(TAstAssign(stmt).Name);
      if loc < 0 then
      begin
        FDiag.Error('assignment to undeclared variable: ' + TAstAssign(stmt).Name, stmt.Span);
        Exit(False);
      end;
      // invalidate any const-folded value for this local
      if (loc < Length(FLocalConst)) and Assigned(FLocalConst[loc]) then
      begin
        FLocalConst[loc].Free;
        FLocalConst[loc] := nil;
      end;
      tmp := LowerExpr(TAstAssign(stmt).Value);
      // truncate if local has narrower integer width
      ltype := GetLocalType(loc);
      if (ltype <> atUnresolved) and (ltype <> atInt64) and (ltype <> atUInt64) then
      begin
        width := 64;
        case ltype of
          atInt8, atUInt8: width := 8;
          atInt16, atUInt16: width := 16;
          atInt32, atUInt32: width := 32;
        end;
        instr.Op := irTrunc; instr.Dest := NewTemp; instr.Src1 := tmp;
        instr.ImmInt := width; Emit(instr);
        tmp := instr.Dest;
      end;
      instr.Op := irStoreLocal;
      instr.Dest := loc;
      instr.Src1 := tmp;
      Emit(instr);
      Exit(True);
    end;

    // field assignment: obj.field := value
      if stmt is TAstFieldAssign then
    begin
      fa := TAstFieldAssign(stmt);
      // target is a TAstFieldAccess node
      t1 := LowerExpr(fa.Target.Obj);
      if t1 < 0 then Exit(False);
      t2 := LowerExpr(fa.Value);
      if t2 < 0 then Exit(False);
      instr.Op := irStoreField;
      instr.Src1 := t1; // base pointer
      instr.Src2 := t2; // value temp
      if fa.Target.FieldOffset >= 0 then
        instr.ImmInt := fa.Target.FieldOffset
      else
        instr.LabelName := fa.Target.Field;
      Emit(instr);
      Exit(True);
    end;

    // index assignment: arr[idx] := value
    if stmt is TAstIndexAssign then
    begin
      // t1 = base array/pointer
      t1 := LowerExpr(TAstIndexAssign(stmt).Target.Obj);
      if t1 < 0 then Exit(False);
      // t2 = index expression
      t2 := LowerExpr(TAstIndexAssign(stmt).Target.Index);
      if t2 < 0 then Exit(False);
      // t0 = value to store
      t0 := LowerExpr(TAstIndexAssign(stmt).Value);
      if t0 < 0 then Exit(False);

      // check if index is a constant for static vs dynamic store
      if TAstIndexAssign(stmt).Target.Index is TAstIntLit then
      begin
        // static index: use irStoreElem with ImmInt
        instr.Op := irStoreElem;
        instr.Src1 := t1; // array base
        instr.Src2 := t0; // value
        instr.ImmInt := TAstIntLit(TAstIndexAssign(stmt).Target.Index).Value; // static index
        Emit(instr);
      end
      else
      begin
        // dynamic index: use irStoreElemDyn with 3 sources
        instr.Op := irStoreElemDyn;
        instr.Src1 := t1; // array base
        instr.Src2 := t2; // index temp
        instr.Src3 := t0; // value temp
        Emit(instr);
      end;
      Exit(True);
    end;

  if stmt is TAstExprStmt then
  begin
    LowerExpr(TAstExprStmt(stmt).Expr);
    Exit(True);
  end;

  if stmt is TAstReturn then
  begin
    if Assigned(TAstReturn(stmt).Value) then
    begin
      retStructDecl := GetReturnStructDecl;
      if Assigned(retStructDecl) then
      begin
        // Struct return - use irReturnStruct with size info
        tmp := LowerExpr(TAstReturn(stmt).Value);
        instr.Op := irReturnStruct;
        instr.Src1 := tmp;
        instr.StructSize := retStructDecl.Size;
        instr.StructAlign := retStructDecl.Align;
        Emit(instr);
      end
      else
      begin
        // Scalar return
        tmp := LowerExpr(TAstReturn(stmt).Value);
        instr.Op := irReturn;
        instr.Src1 := tmp;
        Emit(instr);
      end;
    end
    else
    begin
      instr.Op := irReturn;
      instr.Src1 := -1;
      Emit(instr);
    end;
    Exit(True);
  end;

  if stmt is TAstIf then
  begin
    condTmp := LowerExpr(TAstIf(stmt).Cond);
    thenLabel := NewLabel('Lthen');
    elseLabel := NewLabel('Lelse');
    endLabel := NewLabel('Lend');

    // br false -> else
    instr.Op := irBrFalse;
    instr.Src1 := condTmp;
    instr.LabelName := elseLabel;
    Emit(instr);

    // then branch
    LowerStmt(TAstIf(stmt).ThenBranch);
    // jmp end
    instr.Op := irJmp;
    instr.LabelName := endLabel;
    Emit(instr);

    // else label
    instr.Op := irLabel;
    instr.LabelName := elseLabel;
    Emit(instr);
    if Assigned(TAstIf(stmt).ElseBranch) then
      LowerStmt(TAstIf(stmt).ElseBranch);

    // end label
    instr.Op := irLabel;
    instr.LabelName := endLabel;
    Emit(instr);
    Exit(True);
  end;

    if stmt is TAstWhile then
    begin
      whileNode := TAstWhile(stmt);
      startLabel := NewLabel('Lwhile');
      bodyLabel := NewLabel('Lwhile_body');
      exitLabel := NewLabel('Lwhile_end');

      // start label
      instr.Op := irLabel; instr.LabelName := startLabel; Emit(instr);
      condTmp := LowerExpr(whileNode.Cond);
      instr.Op := irBrFalse; instr.Src1 := condTmp; instr.LabelName := exitLabel; Emit(instr);
      // body (support break -> exitLabel)
      FBreakStack.AddObject(exitLabel, nil);
      LowerStmt(whileNode.Body);
      FBreakStack.Delete(FBreakStack.Count - 1);
      // jump to start
      instr.Op := irJmp; instr.LabelName := startLabel; Emit(instr);
      // exit label
      instr.Op := irLabel; instr.LabelName := exitLabel; Emit(instr);
      Exit(True);
    end;


   if stmt is TAstFor then
   begin
     // for varName := start to/downto end do body
     // lower as: var = start; while (var <= end) { body; var := var +/- 1 }
     with TAstFor(stmt) do
     begin
       loc := AllocLocal(VarName, atInt64);
       tmp := LowerExpr(StartExpr);
       instr.Op := irStoreLocal; instr.Dest := loc; instr.Src1 := tmp; Emit(instr);
       startLabel := NewLabel('Lfor');
       exitLabel := NewLabel('Lfor_end');
       // start label
       instr.Op := irLabel; instr.LabelName := startLabel; Emit(instr);
       // load var and end, compare
       t1 := NewTemp;
       instr.Op := irLoadLocal; instr.Dest := t1; instr.Src1 := loc; Emit(instr);
       t2 := LowerExpr(EndExpr);
       condTmp := NewTemp;
       if IsDownto then
         begin instr.Op := irCmpGe; end
       else
         begin instr.Op := irCmpLe; end;
       instr.Dest := condTmp; instr.Src1 := t1; instr.Src2 := t2; Emit(instr);
       instr.Op := irBrFalse; instr.Src1 := condTmp; instr.LabelName := exitLabel; Emit(instr);
       // body
       FBreakStack.AddObject(exitLabel, nil);
       LowerStmt(Body);
       FBreakStack.Delete(FBreakStack.Count - 1);
       // increment/decrement
       t1 := NewTemp;
       instr.Op := irLoadLocal; instr.Dest := t1; instr.Src1 := loc; Emit(instr);
       t2 := NewTemp;
       instr.Op := irConstInt; instr.Dest := t2; instr.ImmInt := 1; Emit(instr);
       condTmp := NewTemp;
       if IsDownto then
         instr.Op := irSub
       else
         instr.Op := irAdd;
       instr.Dest := condTmp; instr.Src1 := t1; instr.Src2 := t2; Emit(instr);
       instr.Op := irStoreLocal; instr.Dest := loc; instr.Src1 := condTmp; Emit(instr);
       // jump to start
       instr.Op := irJmp; instr.LabelName := startLabel; Emit(instr);
       // exit label
       instr.Op := irLabel; instr.LabelName := exitLabel; Emit(instr);
     end;
     Exit(True);
   end;

   if stmt is TAstRepeatUntil then
   begin
     startLabel := NewLabel('Lrepeat');
     exitLabel := NewLabel('Lrepeat_end');
     // start label
     instr.Op := irLabel; instr.LabelName := startLabel; Emit(instr);
     // body
     FBreakStack.AddObject(exitLabel, nil);
     LowerStmt(TAstRepeatUntil(stmt).Body);
     FBreakStack.Delete(FBreakStack.Count - 1);
     // condition
     condTmp := LowerExpr(TAstRepeatUntil(stmt).Cond);
     // if condition false, jump back to start (repeat until cond is true)
     instr.Op := irBrFalse; instr.Src1 := condTmp; instr.LabelName := startLabel; Emit(instr);
     // exit label
     instr.Op := irLabel; instr.LabelName := exitLabel; Emit(instr);
     Exit(True);
   end;

   if stmt is TAstBlock then
   begin
     for i := 0 to High(TAstBlock(stmt).Stmts) do
       LowerStmt(TAstBlock(stmt).Stmts[i]);
     Exit(True);
   end;

   if stmt is TAstBreak then
   begin
     if FBreakStack.Count = 0 then
       FDiag.Error('break outside of loop/switch', stmt.Span)
     else
     begin
       instr.Op := irJmp;
       instr.LabelName := FBreakStack.Strings[FBreakStack.Count - 1];
       Emit(instr);
     end;
     Exit(True);
   end;

   if stmt is TAstSwitch then
   begin
     // Lower switch by generating compares and branches
      sw := TAstSwitch(stmt);
      switchTmp := LowerExpr(sw.Expr);
      endLbl := NewLabel('Lswitch_end');
      defaultLbl := endLbl;
      if Assigned(sw.Default) then
        defaultLbl := NewLabel('Lswitch_default');

      // For each case, create label and compare
      caseLabels := TStringList.Create; try
        for i := 0 to High(sw.Cases) do
        begin
          lbl := NewLabel('Lcase');
          caseLabels.Add(lbl);
          // lower case value
          caseTmp := LowerExpr(sw.Cases[i].Value);
          // cmp eq
          instr.Op := irCmpEq; instr.Dest := NewTemp; instr.Src1 := switchTmp; instr.Src2 := caseTmp; Emit(instr);
          // br true -> caseLbl
          instr.Op := irBrTrue; instr.Src1 := instr.Dest; instr.LabelName := lbl; Emit(instr);
        end;


       // no match -> jump default or end
       instr.Op := irJmp; instr.LabelName := defaultLbl; Emit(instr);

       // emit case bodies
       for i := 0 to High(sw.Cases) do
       begin
         instr.Op := irLabel; instr.LabelName := caseLabels[i]; Emit(instr);
         // push break label for cases
         FBreakStack.AddObject(endLbl, nil);
         LowerStmt(sw.Cases[i].Body);
         FBreakStack.Delete(FBreakStack.Count - 1);
         // after case body, jump to end
         instr.Op := irJmp; instr.LabelName := endLbl; Emit(instr);
       end;

       // default body
       if Assigned(sw.Default) then
       begin
         instr.Op := irLabel; instr.LabelName := defaultLbl; Emit(instr);
         FBreakStack.AddObject(endLbl, nil);
         LowerStmt(sw.Default);
         FBreakStack.Delete(FBreakStack.Count - 1);
         instr.Op := irJmp; instr.LabelName := endLbl; Emit(instr);
       end;

       // end label
       instr.Op := irLabel; instr.LabelName := endLbl; Emit(instr);
     finally
       caseLabels.Free;
     end;
     Exit(True);
   end;

   FDiag.Error('lowering: unsupported statement', stmt.Span);
   Result := False;
 end;


end.
