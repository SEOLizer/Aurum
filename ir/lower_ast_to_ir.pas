{$mode objfpc}{$H+}
unit lower_ast_to_ir;

interface

uses
  SysUtils, Classes,
  ast, ir, diag, lexer;

type
  TIRLowering = class
  private
    FModule: TIRModule;
    FCurrentFunc: TIRFunction;
    FDiag: TDiagnostics;
    FTempCounter: Integer;
    FLabelCounter: Integer;
    FLocalMap: TStringList; // name -> local index (as object integer)

    function NewTemp: Integer;
    function NewLabel(const prefix: string): string;
    function AllocLocal(const name: string): Integer;
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
end;

destructor TIRLowering.Destroy;
begin
  FLocalMap.Free;
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

function TIRLowering.AllocLocal(const name: string): Integer;
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
      // lower statements sequentially
      for j := 0 to High(TAstFuncDecl(node).Body.Stmts) do
      begin
        LowerStmt(TAstFuncDecl(node).Body.Stmts[j]);
      end;
      FCurrentFunc := nil;
    end
    else if node is TAstConDecl then
    begin
      // constants can be lowered to module strings/consts if needed
      // skip for now
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
begin
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
  if expr is TAstIdent then
  begin
    t1 := NewTemp;
    instr.Op := irLoadLocal;
    instr.Dest := t1;
    instr.Src1 := ResolveLocal(TAstIdent(expr).Name);
    if instr.Src1 < 0 then
      FDiag.Error('use of undeclared local ' + TAstIdent(expr).Name, expr.Span);
    Emit(instr);
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
        // lower args and collect their temp indices
        var argTemps: array of Integer;
        SetLength(argTemps, Length(TAstCall(expr).Args));
        for t1 := 0 to High(TAstCall(expr).Args) do
        begin
          argTemps[t1] := LowerExpr(TAstCall(expr).Args[t1]);
        end;
        instr.Op := irCall;
        instr.ImmStr := TAstCall(expr).Name;
        instr.ImmInt := Length(argTemps);
        if instr.ImmInt > 0 then instr.Src1 := argTemps[0] else instr.Src1 := -1;
        if instr.ImmInt > 1 then instr.Src2 := argTemps[1] else instr.Src2 := -1;
        // serialize remaining temps (from index 2) into LabelName as CSV
        instr.LabelName := '';
        for t1 := 2 to High(argTemps) do
        begin
          if instr.LabelName <> '' then instr.LabelName := instr.LabelName + ',';
          instr.LabelName := instr.LabelName + IntToStr(argTemps[t1]);
        end;
        Emit(instr);
        Result := -1;
        Exit(Result);
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
begin
  Result := True;
  if stmt is TAstVarDecl then
  begin
    loc := AllocLocal(TAstVarDecl(stmt).Name);
    tmp := LowerExpr(TAstVarDecl(stmt).InitExpr);
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
    tmp := LowerExpr(TAstAssign(stmt).Value);
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
    // body
    LowerStmt(whileNode.Body);
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

  FDiag.Error('lowering: unsupported statement', stmt.Span);
  Result := False;
end;

end.

end.
