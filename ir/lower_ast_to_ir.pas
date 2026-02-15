{$mode objfpc}{$H+}
unit lower_ast_to_ir;

interface

uses
  SysUtils, Classes,
  ast, ir, diag, lexer;

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
    FConstMap: TStringList; // name -> TConstValue (compile-time constants)
    FLocalConst: array of TConstValue; // per-function local constant values (or nil)
    FBreakStack: TStringList; // stack of break labels

    function NewTemp: Integer;
    function NewLabel(const prefix: string): string;
    function AllocLocal(const name: string; aType: TAurumType): Integer;
    function GetLocalType(idx: Integer): TAurumType;
    function ResolveLocal(const name: string): Integer;
    procedure Emit(instr: TIRInstr);

    function LowerStmt(stmt: TAstStmt): Boolean;
    function LowerExpr(expr: TAstExpr): Integer; // returns temp index
  public
    constructor Create(modul: TIRModule; diag: TDiagnostics);
    destructor Destroy; override;

    function Lower(prog: TAstProgram): TIRModule;
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
var
  instr: TIRInstr;
  t1, t2: Integer;
  si: Integer;
  argTemps: array of Integer;
  ai: Integer;
  ci: Integer;
  cv2: TConstValue;
  ltype: TAurumType;
  w: Integer;
  loc: Integer;
begin
  instr := Default(TIRInstr);
  if expr is TAstIntLit then
  begin
    t1 := NewTemp;
    instr.Op := irConstInt;
    instr.Dest := t1;
    instr.ImmInt := TAstIntLit(expr).Value;
    Emit(instr);
    Exit(t1);
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
      tkAnd: instr.Op := irAnd;
      tkOr: instr.Op := irOr;
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
  begin
  instr := Default(TIRInstr);
  Result := True;
  if stmt is TAstVarDecl then
  begin
    loc := AllocLocal(TAstVarDecl(stmt).Name, TAstVarDecl(stmt).DeclType);
    // If initializer is constant integer and the local has narrower signed width, constant fold
    if (TAstVarDecl(stmt).InitExpr is TAstIntLit) then
    begin
      lit := TAstIntLit(TAstVarDecl(stmt).InitExpr).Value;
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
    tmp := LowerExpr(TAstVarDecl(stmt).InitExpr);
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
