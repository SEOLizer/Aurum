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
    FDiag: TDiagnostics;
    FTempCounter: Integer;
    FLabelCounter: Integer;
    FLocalMap: TStringList; // name -> local index (as object integer)
    FLocalTypes: array of TAurumType; // index -> declared local type
    FLocalElemSize: array of Integer; // index -> element size in bytes for dynamic array locals (0 if not array)
    FConstMap: TStringList; // name -> TConstValue (compile-time constants)
    FLocalConst: array of TConstValue; // per-function local constant values (or nil)
    FBreakStack: TStringList; // stack of break labels

    function NewTemp: Integer;
    function NewLabel(const prefix: string): string;
    function AllocLocal(const name: string; aType: TAurumType): Integer;
    function AllocLocalMany(const name: string; aType: TAurumType; count: Integer): Integer;
    function GetLocalType(idx: Integer): TAurumType;
    function ResolveLocal(const name: string): Integer;
    procedure Emit(instr: TIRInstr);

    function LowerStmt(stmt: TAstStmt): Boolean;
    function LowerExpr(expr: TAstExpr): Integer; // returns temp index
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
    FTempCounter := 0;
    FLabelCounter := 0;
    FLocalMap := TStringList.Create;
    FLocalMap.Sorted := False;
    FConstMap := TStringList.Create;
    FConstMap.Sorted := False;
    FBreakStack := TStringList.Create;
    FBreakStack.Sorted := False;
    SetLength(FLocalTypes, 0);
    SetLength(FLocalElemSize, 0);
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
  FBreakStack.Free;
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
end;

function TIRLowering.AllocLocalMany(const name: string; aType: TAurumType; count: Integer): Integer;
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
  cv: TConstValue;
begin
  // iterate top-level decls, create functions
  for i := 0 to High(prog.Decls) do
  begin
    node := prog.Decls[i];
    if node is TAstFuncDecl then
    begin
       fn := FModule.AddFunction(TAstFuncDecl(node).Name);
       // Lower function body
       FCurrentFunc := fn;
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
      FCurrentFunc := nil;
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
        end;
      end;
    end;
  end;
end;

{ Lowering helpers }

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
begin
  // Minimal placeholder implementation to restore compileability after merge conflicts.
  // This will be expanded later with full lowering logic.
  Result := -1;
end;
  if expr is TAstStrLit then
  begin
    si := FModule.InternString(TAstStrLit(expr).Value);
    t1 := NewTemp;
    instr.Op := irConstStr;
    instr.Dest := t1;
    instr.ImmStr := IntToStr(si);
    Emit(instr);
    Exit(t1);
  end;
  if expr is TAstCharLit then
  begin
    t1 := NewTemp;
    instr.Op := irConstInt;
    instr.Dest := t1;
    instr.ImmInt := Ord(TAstCharLit(expr).Value);
    Emit(instr);
    Exit(t1);
  end;
  if expr is TAstBoolLit then
  begin
    t1 := NewTemp;
    instr.Op := irConstInt;
    instr.Dest := t1;
    if TAstBoolLit(expr).Value then
      instr.ImmInt := 1
    else
      instr.ImmInt := 0;
    Emit(instr);
    Exit(t1);
  end;
   if expr is TAstIndexAccess then
   begin
     // handle static array index: baseIdent[CONST]
     if (TAstIndexAccess(expr).Obj is TAstIdent) and (TAstIndexAccess(expr).Index is TAstIntLit) then
     begin
       loc := ResolveLocal(TAstIdent(TAstIndexAccess(expr).Obj).Name);
       if loc < 0 then
         FDiag.Error('use of undeclared local ' + TAstIdent(TAstIndexAccess(expr).Obj).Name, expr.Span)
       else
       begin
          w := Integer(TAstIntLit(TAstIndexAccess(expr).Index).Value);
          t1 := NewTemp;
          instr.Op := irLoadLocal; instr.Dest := t1; instr.Src1 := loc + w; Emit(instr);

         Exit(t1);
       end;
     end
     else
     begin
       FDiag.Error('dynamic indexing not yet supported in lowering', expr.Span);
       Exit(-1);
     end;
   end;

   if expr is TAstIdent then
   begin
     // check if this is a compile-time constant (con)

    ci := FConstMap.IndexOf(TAstIdent(expr).Name);
    if ci >= 0 then
    begin
      cv2 := TConstValue(FConstMap.Objects[ci]);
      t1 := NewTemp;
      if cv2.IsStr then
      begin
        si := FModule.InternString(cv2.StrVal);
        instr.Op := irConstStr;
        instr.Dest := t1;
        instr.ImmStr := IntToStr(si);
      end
      else
      begin
        instr.Op := irConstInt;
        instr.Dest := t1;
        instr.ImmInt := cv2.IntVal;
      end;
      Emit(instr);
      Exit(t1);
    end;
    // check if this local was const-folded at declaration (literal init with narrow type)
    loc := ResolveLocal(TAstIdent(expr).Name);
    if loc < 0 then
      FDiag.Error('use of undeclared local ' + TAstIdent(expr).Name, expr.Span);
    if (loc >= 0) and (loc < Length(FLocalConst)) and Assigned(FLocalConst[loc]) then
    begin
      // emit the pre-computed constant value directly (already sign/zero-extended)
      t1 := NewTemp;
      instr.Op := irConstInt;
      instr.Dest := t1;
      instr.ImmInt := FLocalConst[loc].IntVal;
      Emit(instr);
      Exit(t1);
    end;
    // otherwise load from local variable
    t1 := NewTemp;
    instr.Op := irLoadLocal;
    instr.Dest := t1;
    instr.Src1 := loc;
    Emit(instr);
    // If local has narrower width, extend (sign or zero) to 64-bit for operations
    ltype := GetLocalType(loc);
    if (ltype <> atUnresolved) and (ltype <> atInt64) then
    begin
      w := 64;
      case ltype of
        atInt8, atUInt8: w := 8;
        atInt16, atUInt16: w := 16;
        atInt32, atUInt32: w := 32;
        atInt64, atUInt64: w := 64;
      end;
      if (ltype = atUInt8) or (ltype = atUInt16) or (ltype = atUInt32) or (ltype = atUInt64) then
      begin
        instr.Op := irZExt; instr.Dest := NewTemp; instr.Src1 := t1; instr.ImmInt := w; Emit(instr);
        Exit(instr.Dest);
      end
      else
      begin
        instr.Op := irSExt; instr.Dest := NewTemp; instr.Src1 := t1; instr.ImmInt := w; Emit(instr);
        Exit(instr.Dest);
      end;
    end;
    Exit(t1);
  end;
  if expr is TAstBinOp then
  begin
    t1 := LowerExpr(TAstBinOp(expr).Left);
    t2 := LowerExpr(TAstBinOp(expr).Right);
    case TAstBinOp(expr).Op of
      tkPlus: instr.Op := irAdd;
      tkMinus: instr.Op := irSub;
      tkStar: instr.Op := irMul;
      tkSlash: instr.Op := irDiv;
      tkPercent: instr.Op := irMod;
      tkEq: instr.Op := irCmpEq;
      tkNeq: instr.Op := irCmpNeq;
      tkLt: instr.Op := irCmpLt;
      tkLe: instr.Op := irCmpLe;
      tkGt: instr.Op := irCmpGt;
      tkGe: instr.Op := irCmpGe;
      tkAnd:
        begin
          // short-circuit: if left is false, result is 0 (skip right)
          instr.Op := irAnd; // will be used as fallback
          // actually implement short-circuit via branches
          begin
            loc := NewTemp; // result temp
            thenLabel := NewLabel('Land_true');
            elseLabel := NewLabel('Land_false');
            endLabel := NewLabel('Land_end');
            // test left
            instr.Op := irBrFalse; instr.Src1 := t1; instr.LabelName := elseLabel; Emit(instr);
            // left is true, test right
            instr.Op := irBrFalse; instr.Src1 := t2; instr.LabelName := elseLabel; Emit(instr);
            // both true -> result = 1
            instr.Op := irConstInt; instr.Dest := loc; instr.ImmInt := 1; Emit(instr);
            instr.Op := irJmp; instr.LabelName := endLabel; Emit(instr);
            // false label -> result = 0
            instr.Op := irLabel; instr.LabelName := elseLabel; Emit(instr);
            instr.Op := irConstInt; instr.Dest := loc; instr.ImmInt := 0; Emit(instr);
            // end label
            instr.Op := irLabel; instr.LabelName := endLabel; Emit(instr);
            Exit(loc);
          end;
        end;
      tkOr:
        begin
          // short-circuit: if left is true, result is 1 (skip right)
          begin
            loc := NewTemp;
            thenLabel := NewLabel('Lor_true');
            elseLabel := NewLabel('Lor_false');
            endLabel := NewLabel('Lor_end');
            // test left
            instr.Op := irBrTrue; instr.Src1 := t1; instr.LabelName := thenLabel; Emit(instr);
            // left is false, test right
            instr.Op := irBrTrue; instr.Src1 := t2; instr.LabelName := thenLabel; Emit(instr);
            // both false -> result = 0
            instr.Op := irConstInt; instr.Dest := loc; instr.ImmInt := 0; Emit(instr);
            instr.Op := irJmp; instr.LabelName := endLabel; Emit(instr);
            // true label -> result = 1
            instr.Op := irLabel; instr.LabelName := thenLabel; Emit(instr);
            instr.Op := irConstInt; instr.Dest := loc; instr.ImmInt := 1; Emit(instr);
            // end label
            instr.Op := irLabel; instr.LabelName := endLabel; Emit(instr);
            Exit(loc);
          end;
        end;
    else
      instr.Op := irInvalid;
    end;
    instr.Dest := NewTemp;
    instr.Src1 := t1;
    instr.Src2 := t2;
    Emit(instr);
    Exit(instr.Dest);
  end;
  if expr is TAstUnaryOp then
  begin
    t1 := LowerExpr(TAstUnaryOp(expr).Operand);
    if TAstUnaryOp(expr).Op = tkMinus then
    begin
      instr.Op := irNeg;
      instr.Dest := NewTemp;
      instr.Src1 := t1;
      Emit(instr);
      Exit(instr.Dest);
    end
    else if TAstUnaryOp(expr).Op = tkNot then
    begin
      instr.Op := irNot;
      instr.Dest := NewTemp;
      instr.Src1 := t1;
      Emit(instr);
      Exit(instr.Dest);
    end;
  end;
  if expr is TAstCall then
  begin
    // handle builtins: print_str, print_int, exit
    if TAstCall(expr).Name = 'print_str' then
    begin
      t1 := LowerExpr(TAstCall(expr).Args[0]);
      instr.Op := irCallBuiltin;
      instr.ImmStr := 'print_str';
      instr.Src1 := t1;
      Emit(instr);
      Exit(-1); // void
    end
    else if TAstCall(expr).Name = 'print_int' then
    begin
      // constant-fold print_int(x) -> print_str("...") when x is literal
      if (Length(TAstCall(expr).Args) >= 1) and (TAstCall(expr).Args[0] is TAstIntLit) then
      begin
        si := FModule.InternString(IntToStr(TAstIntLit(TAstCall(expr).Args[0]).Value));
        t1 := NewTemp;
        instr.Op := irConstStr;
        instr.Dest := t1;
        instr.ImmStr := IntToStr(si);
        Emit(instr);
        instr.Op := irCallBuiltin;
        instr.ImmStr := 'print_str';
        instr.Src1 := t1;
        Emit(instr);
        Exit(-1);
      end;

      t1 := LowerExpr(TAstCall(expr).Args[0]);
      instr.Op := irCallBuiltin;
      instr.ImmStr := 'print_int';
      instr.Src1 := t1;
      Emit(instr);
      Exit(-1);
    end
    else if TAstCall(expr).Name = 'exit' then
    begin
      t1 := LowerExpr(TAstCall(expr).Args[0]);
      instr.Op := irCallBuiltin;
      instr.ImmStr := 'exit';
      instr.Src1 := t1;
      Emit(instr);
      Exit(-1);
    end
    else if TAstCall(expr).Name = 'buf_put_byte' then
    begin
      // buf_put_byte(buf: pchar, idx: int64, b: int64) -> int64
      t1 := LowerExpr(TAstCall(expr).Args[0]);
      t2 := LowerExpr(TAstCall(expr).Args[1]);
      t3 := LowerExpr(TAstCall(expr).Args[2]);
      instr.Op := irCallBuiltin;
      instr.ImmStr := 'buf_put_byte';
      instr.Src1 := t1;
      instr.Src2 := t2;
      instr.LabelName := IntToStr(t3);
      instr.Dest := NewTemp;
      Emit(instr);
      Exit(instr.Dest);
    end
     else if TAstCall(expr).Name = 'itoa_to_buf' then
     begin
       // itoa_to_buf(val: int64, buf: pchar, idx: int64, buflen: int64) -> int64
       t1 := LowerExpr(TAstCall(expr).Args[0]);
       t2 := LowerExpr(TAstCall(expr).Args[1]);
        t3 := LowerExpr(TAstCall(expr).Args[2]);
        t4 := LowerExpr(TAstCall(expr).Args[3]);
        // optional extras: minWidth, padZero
        if Length(TAstCall(expr).Args) > 4 then
          t5 := LowerExpr(TAstCall(expr).Args[4])
        else
          t5 := -1;
        if Length(TAstCall(expr).Args) > 5 then
          t6 := LowerExpr(TAstCall(expr).Args[5])
        else
          t6 := -1;
        instr.Op := irCallBuiltin;
        instr.ImmStr := 'itoa_to_buf';
        instr.Src1 := t1;
        instr.Src2 := t2;
        instr.LabelName := IntToStr(t3) + ',' + IntToStr(t4) + ',' + IntToStr(t5) + ',' + IntToStr(t6);
        instr.Dest := NewTemp;
        Emit(instr);
        Exit(instr.Dest);
      end
      else if (TAstCall(expr).Name = 'push') or (TAstCall(expr).Name = 'append') then
      begin
        // push(arrVar, val) / append(arrVar, val)
        if Length(TAstCall(expr).Args) <> 2 then
        begin
          FDiag.Error('push/append requires 2 arguments', TAstCall(expr).Span);
          Exit(-1);
        end;
        // first arg must be identifier (variable)
        if not (TAstCall(expr).Args[0] is TAstIdent) then
        begin
          FDiag.Error('push/append: first argument must be array variable identifier', TAstCall(expr).Args[0].Span);
          Exit(-1);
        end;
        arrName := TAstIdent(TAstCall(expr).Args[0]).Name;
        arrLoc := ResolveLocal(arrName);
        if arrLoc < 0 then
        begin
          FDiag.Error('push/append: unknown variable ' + arrName, TAstCall(expr).Args[0].Span);
          Exit(-1);
        end;
        t1 := LowerExpr(TAstCall(expr).Args[1]); // value temp
        instr.Op := irCallBuiltin;
        instr.ImmStr := 'push';
        instr.Src1 := arrLoc;
        instr.Src2 := t1;
        // attach element size metadata if available
        esz := 8;
        if (arrLoc >= 0) and (arrLoc < Length(FLocalElemSize)) then esz := FLocalElemSize[arrLoc];
        instr.LabelName := IntToStr(esz);
        Emit(instr);
        Exit(-1);
      end
      else if TAstCall(expr).Name = 'pop' then
      begin
        // pop(arrVar) -> int64
        if Length(TAstCall(expr).Args) <> 1 then
        begin
          FDiag.Error('pop requires 1 argument', TAstCall(expr).Span);
          Exit(-1);
        end;
        if not (TAstCall(expr).Args[0] is TAstIdent) then
        begin
          FDiag.Error('pop: first argument must be array variable identifier', TAstCall(expr).Args[0].Span);
          Exit(-1);
        end;
        arrName2 := TAstIdent(TAstCall(expr).Args[0]).Name;
        arrLoc2 := ResolveLocal(arrName2);
        if arrLoc2 < 0 then
        begin
          FDiag.Error('pop: unknown variable ' + arrName2, TAstCall(expr).Args[0].Span);
          Exit(-1);
        end;
        instr.Op := irCallBuiltin;
        instr.ImmStr := 'pop';
        instr.Src1 := arrLoc2;
        instr.Dest := NewTemp;
        // attach element size metadata if available
        esz2 := 8;
        if (arrLoc2 >= 0) and (arrLoc2 < Length(FLocalElemSize)) then esz2 := FLocalElemSize[arrLoc2];
        instr.LabelName := IntToStr(esz2);
        Emit(instr);
        Exit(instr.Dest);
      end

     else if TAstCall(expr).Name = 'len' then
     begin
       // len(arrVar) -> int64
       if Length(TAstCall(expr).Args) <> 1 then
       begin
         FDiag.Error('len requires 1 argument', TAstCall(expr).Span);
         Exit(-1);
       end;
       if TAstCall(expr).Args[0] is TAstIdent then
       begin
          name0 := TAstIdent(TAstCall(expr).Args[0]).Name;
          // Resolve local slot for variable
          loc0 := ResolveLocal(name0);
          if loc0 < 0 then
          begin
            FDiag.Error('len: unknown variable ' + name0, TAstCall(expr).Args[0].Span);
            Exit(-1);
          end;
          instr.Op := irCallBuiltin;
          instr.ImmStr := 'len';
          instr.Src1 := loc0;
          instr.Dest := NewTemp;
          // attach element size metadata if available
          esz_len := 8;
          if (loc0 >= 0) and (loc0 < Length(FLocalElemSize)) then esz_len := FLocalElemSize[loc0];
          instr.LabelName := IntToStr(esz_len);
          Emit(instr);
          Exit(instr.Dest);

         end;
       end
       else
       begin
         FDiag.Error('len: argument must be identifier', TAstCall(expr).Args[0].Span);
         Exit(-1);
       end;
     end
     else if TAstCall(expr).Name = 'free' then
     begin
       // free(arrVar)
       if Length(TAstCall(expr).Args) <> 1 then
       begin
         FDiag.Error('free requires 1 argument', TAstCall(expr).Span);
         Exit(-1);
       end;
       if not (TAstCall(expr).Args[0] is TAstIdent) then
       begin
         FDiag.Error('free: first argument must be array variable identifier', TAstCall(expr).Args[0].Span);
         Exit(-1);
       end;
        name1 := TAstIdent(TAstCall(expr).Args[0]).Name;
        loc1 := ResolveLocal(name1);
        if loc1 < 0 then

       begin
         FDiag.Error('free: unknown variable ' + name1, TAstCall(expr).Args[0].Span);
         Exit(-1);
       end;
        instr.Op := irCallBuiltin;
        instr.ImmStr := 'free';
        instr.Src1 := loc1;
        // attach element size metadata if available
         esz_free := 8;
         if (loc1 >= 0) and (loc1 < Length(FLocalElemSize)) then esz_free := FLocalElemSize[loc1];
         instr.LabelName := IntToStr(esz_free);

        Emit(instr);
        Exit(-1);

     end
     else
     begin

      // generic call
      SetLength(argTemps, Length(TAstCall(expr).Args));
      for ai := 0 to High(argTemps) do
        argTemps[ai] := LowerExpr(TAstCall(expr).Args[ai]);
      instr.Op := irCall;
      instr.ImmStr := TAstCall(expr).Name;
      instr.ImmInt := Length(argTemps);
      if instr.ImmInt > 0 then instr.Src1 := argTemps[0] else instr.Src1 := -1;
      if instr.ImmInt > 1 then instr.Src2 := argTemps[1] else instr.Src2 := -1;
      instr.LabelName := '';
      for ai := 2 to High(argTemps) do
      begin
        if instr.LabelName <> '' then instr.LabelName := instr.LabelName + ',';
        instr.LabelName := instr.LabelName + IntToStr(argTemps[ai]);
      end;
      instr.Dest := NewTemp;
      Emit(instr);
      Exit(instr.Dest);
    end;
  end;

  // fallback
  FDiag.Error('lowering: unsupported expr', expr.Span);
  Result := -1;
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
    i: Integer;
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

  if stmt is TAstExprStmt then
  begin
    LowerExpr(TAstExprStmt(stmt).Expr);
    Exit(True);
  end;

  if stmt is TAstReturn then
  begin
    if Assigned(TAstReturn(stmt).Value) then
    begin
      tmp := LowerExpr(TAstReturn(stmt).Value);
      instr.Op := irReturn;
      instr.Src1 := tmp;
      Emit(instr);
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
