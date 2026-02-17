{$mode objfpc}{$H+}
unit lower_ast_to_ir;

interface

uses
  SysUtils, Classes,
  ast, ir, diag, lexer;

type
  TConstValueKind = (cvInt, cvFloat, cvString);
  
  TConstValue = class
  public
    Kind: TConstValueKind;
    IntVal: Int64;
    FloatVal: Double;
    StrVal: string;
    
    constructor Create(val: Int64);
    constructor Create(val: Double);
    constructor Create(const val: string);
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
    FTypeMap: TStringList; // name -> TAstTypeDecl (type declarations)
    FLocalTypeNames: array of string; // index -> declared local type name (for struct types)

    function NewTemp: Integer;
    function NewLabel(const prefix: string): string;
    function AllocLocal(const name: string; aType: TAurumType): Integer;
    function GetLocalType(idx: Integer): TAurumType;
    function GetLocalTypeName(idx: Integer): string;
    function ResolveLocal(const name: string): Integer;
    function ResolveTypeDecl(const name: string): TAstTypeDecl;
    function GetStructSize(const typeName: string): Integer;
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
    FTypeMap := TStringList.Create;
    FTypeMap.Sorted := False;
    SetLength(FLocalTypes, 0);
    SetLength(FLocalConst, 0);
    SetLength(FLocalTypeNames, 0);
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
  FTypeMap.Free;
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
  fieldCount: Integer;
  td: TAstTypeDecl;
begin
  idx := FLocalMap.IndexOf(name);
  if idx >= 0 then
  begin
    Result := ObjToInt(FLocalMap.Objects[idx]);
    Exit;
  end;
  Result := FCurrentFunc.LocalCount;

  // For struct types, allocate one slot per field (8 bytes each)
  if aType = atStruct then
  begin
    // Get type name from the variable name lookup
    // We'll store this in FLocalTypeNames after the call
    // For now, just allocate 1 slot - we'll handle multi-slot later
    FCurrentFunc.LocalCount := FCurrentFunc.LocalCount + 1;
  end
  else
  begin
    FCurrentFunc.LocalCount := FCurrentFunc.LocalCount + 1;
  end;

  FLocalMap.AddObject(name, IntToObj(Result));
  // ensure FLocalTypes has same length
  SetLength(FLocalTypes, FCurrentFunc.LocalCount);
  FLocalTypes[Result] := aType;
  // Also ensure FLocalTypeNames has same length
  SetLength(FLocalTypeNames, FCurrentFunc.LocalCount);
  FLocalTypeNames[Result] := '';
end;

function TIRLowering.GetLocalType(idx: Integer): TAurumType;
begin
  if (idx >= 0) and (idx < Length(FLocalTypes)) then
    Result := FLocalTypes[idx]
  else
    Result := atUnresolved;
end;

function TIRLowering.GetLocalTypeName(idx: Integer): string;
begin
  if (idx >= 0) and (idx < Length(FLocalTypeNames)) then
    Result := FLocalTypeNames[idx]
  else
    Result := '';
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
  // First pass: collect type declarations
  for i := 0 to High(prog.Decls) do
  begin
    node := prog.Decls[i];
    if node is TAstTypeDecl then
    begin
      if FTypeMap.IndexOf(TAstTypeDecl(node).Name) < 0 then
        FTypeMap.AddObject(TAstTypeDecl(node).Name, TObject(node));
    end;
  end;

  // Second pass: process functions and constants
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
      if TAstConDecl(node).InitExpr is TAstIntLit then
      begin
        cv := TConstValue.Create(TAstIntLit(TAstConDecl(node).InitExpr).Value);
      end
      else if TAstConDecl(node).InitExpr is TAstStrLit then
      begin
        cv := TConstValue.Create(TAstStrLit(TAstConDecl(node).InitExpr).Value);
      end
      else if TAstConDecl(node).InitExpr is TAstCharLit then
      begin
        cv := TConstValue.Create(Int64(Ord(TAstCharLit(TAstConDecl(node).InitExpr).Value)));
      end
      else if TAstConDecl(node).InitExpr is TAstFloatLit then
      begin
        cv := TConstValue.Create(StrToFloat(TAstFloatLit(TAstConDecl(node).InitExpr).Value));
      end
      else if TAstConDecl(node).InitExpr is TAstBoolLit then
      begin
        if TAstBoolLit(TAstConDecl(node).InitExpr).Value then
          cv := TConstValue.Create(Int64(1))
        else
          cv := TConstValue.Create(Int64(0));
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

function TIRLowering.ResolveTypeDecl(const name: string): TAstTypeDecl;
var
  idx: Integer;
begin
  Result := nil;
  if FTypeMap = nil then Exit;
  idx := FTypeMap.IndexOf(name);
  if idx >= 0 then
    Result := TAstTypeDecl(FTypeMap.Objects[idx]);
end;

function TIRLowering.GetStructSize(const typeName: string): Integer;
var
  td: TAstTypeDecl;
begin
  Result := 0;
  td := ResolveTypeDecl(typeName);
  if td <> nil then
    Result := Length(td.Fields) * 8; // 8 bytes per field
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
  // struct field access variables
  fname: string;
  declTypeName: string;
  td: TAstTypeDecl;
  fieldOffset: Integer;
  fieldIndex: Integer;
  foundField: Boolean;
  // struct literal variables
  st: TAstStructLit;
  structSize: Integer;
  fieldValue: TAstExpr;
  valTemp: Integer;
  j: Integer;
  // array literal variables
  arrayLit: TAstArrayLit;
  arraySize: Integer;
  i: Integer;
  elemTemp: Integer;
  // array indexing variables
  arrayIndex: TAstArrayIndex;
  resultTemp: Integer;
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
  if expr is TAstFloatLit then
  begin
    // Float-Literal als irConstFloat
    t1 := NewTemp;
    instr.Op := irConstFloat;
    instr.Dest := t1;
    instr.ImmFloat := StrToFloat(TAstFloatLit(expr).Value);
    Emit(instr);
    Exit(t1);
  end;
  if expr is TAstCharLit then
  begin
    // Char-Literal als Integer-Wert (ASCII-Code)
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
  if expr is TAstArrayLit then
  begin
    // Array-Literal: Stack-Allokation + Element-Initialisierung
    arrayLit := TAstArrayLit(expr);
    arraySize := Length(arrayLit.Items);
    
    // 1) Allokiere Platz auf Stack (8 bytes pro Element)
    t1 := NewTemp; // wird die Array-Adresse enthalten
    instr.Op := irStackAlloc;
    instr.Dest := t1;
    instr.ImmInt := arraySize * 8; // 8 bytes pro Element
    Emit(instr);
    
    // 2) Initialisiere jedes Element: array[i] = items[i]
    for i := 0 to arraySize - 1 do
    begin
      elemTemp := LowerExpr(arrayLit.Items[i]);
      instr.Op := irStoreElem;
      instr.Dest := 0; // unused
      instr.Src1 := t1; // array base address
      instr.Src2 := elemTemp; // element value
      instr.ImmInt := i; // index
      Emit(instr);
    end;
    
    Exit(t1); // return array base address
  end;
  if expr is TAstStructLit then
  begin
    // Struct-Literal: Stack-Allokation + Feld-Initialisierung
    st := TAstStructLit(expr);
    // Hole Typ-Deklaration
    td := ResolveTypeDecl(st.TypeName);
    if td = nil then
    begin
      FDiag.Error('use of undeclared type: ' + st.TypeName, expr.Span);
      Result := -1;
      Exit;
    end;
    // Berechne Gesamtgröße: 8 Bytes pro Feld
    structSize := Length(td.Fields) * 8;
    // 1) Allokiere Platz auf Stack
    t1 := NewTemp;
    instr.Op := irStackAlloc;
    instr.Dest := t1;
    instr.ImmInt := structSize;
    Emit(instr);
    // 2) Initialisiere jedes Feld
    for i := 0 to High(td.Fields) do
    begin
      // Finde Wert für dieses Feld im Literal
      fieldValue := nil;
      for j := 0 to st.FieldCount - 1 do
      begin
        if st.GetFieldName(j) = td.Fields[i].Name then
        begin
          fieldValue := st.GetFieldValue(j);
          Break;
        end;
      end;
      if fieldValue = nil then
      begin
        FDiag.Error('missing field in struct literal: ' + td.Fields[i].Name, expr.Span);
        Continue;
      end;
      // Lower den Wert
      valTemp := LowerExpr(fieldValue);
      // Store Feld: [base + offset] = value
      instr.Op := irStoreField;
      instr.Dest := 0;
      instr.Src1 := t1; // base address
      instr.Src2 := valTemp; // field value
      instr.ImmInt := i * 8; // offset (8 bytes per field)
      Emit(instr);
    end;
    Exit(t1); // return struct base address
  end;
  if expr is TAstArrayIndex then
  begin
    // Array-Index: arr[i] -> load element
    arrayIndex := TAstArrayIndex(expr);
    
    // Lower array expression (should return array base address)
    t1 := LowerExpr(arrayIndex.ArrayExpr);
    
    // Lower index expression (should return integer)
    t2 := LowerExpr(arrayIndex.Index);
    
    // Load element: dest = array_base[index]
    resultTemp := NewTemp;
    instr.Op := irLoadElem;
    instr.Dest := resultTemp;
    instr.Src1 := t1;  // array base address
    instr.Src2 := t2;  // index
    instr.ImmInt := 0; // element size offset (8 bytes per element)
    Emit(instr);
    
    Exit(resultTemp);
  end;
  if expr is TAstIndexAccess then
  begin
    // Index-Zugriff: obj[index] -> load element
    // Lower object expression (should return array base address)
    t1 := LowerExpr(TAstIndexAccess(expr).Obj);
    
    // Lower index expression (should return integer)
    t2 := LowerExpr(TAstIndexAccess(expr).Index);
    
    // Load element: dest = array_base[index]
    resultTemp := NewTemp;
    instr.Op := irLoadElem;
    instr.Dest := resultTemp;
    instr.Src1 := t1;  // array base address
    instr.Src2 := t2;  // index
    instr.ImmInt := 0; // element size offset (8 bytes per element)
    Emit(instr);
    
    Exit(resultTemp);
  end;
  if expr is TAstIdent then
  begin
    // check if this is a compile-time constant (con)
    ci := FConstMap.IndexOf(TAstIdent(expr).Name);
    if ci >= 0 then
    begin
      cv2 := TConstValue(FConstMap.Objects[ci]);
      t1 := NewTemp;
      case cv2.Kind of
        cvString:
        begin
          si := FModule.InternString(cv2.StrVal);
          instr.Op := irConstStr;
          instr.Dest := t1;
          instr.ImmStr := IntToStr(si);
        end;
        cvInt:
        begin
          instr.Op := irConstInt;
          instr.Dest := t1;
          instr.ImmInt := cv2.IntVal;
        end;
        cvFloat:
        begin
          instr.Op := irConstFloat;
          instr.Dest := t1;
          instr.ImmFloat := cv2.FloatVal;
        end;
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
  if expr is TAstFieldAccess then
  begin
    // Field access: obj.field
    // For simple identifier as object, use the slot directly (not load the value)
    if TAstFieldAccess(expr).Obj is TAstIdent then
    begin
      // Get local slot directly (not through LowerExpr which loads the value)
      loc := ResolveLocal(TAstIdent(TAstFieldAccess(expr).Obj).Name);
      if loc < 0 then
      begin
        FDiag.Error('use of undeclared local ' + TAstIdent(TAstFieldAccess(expr).Obj).Name, expr.Span);
        Result := -1;
        Exit;
      end;
      // Get struct type name from the local slot
      declTypeName := GetLocalTypeName(loc);
      if declTypeName = '' then
      begin
        FDiag.Error('not a struct variable: ' + TAstIdent(TAstFieldAccess(expr).Obj).Name, expr.Span);
        Result := -1;
        Exit;
      end;
      // Get field name
      fname := TAstFieldAccess(expr).Field;
      // Look up the type declaration to find field offset
      td := ResolveTypeDecl(declTypeName);
      if td = nil then
      begin
        FDiag.Error('unknown struct type: ' + declTypeName, expr.Span);
        Result := -1;
        Exit;
      end;
      // Find field and compute index
      fieldIndex := 0;
      foundField := False;
      for i := 0 to High(td.Fields) do
      begin
        if td.Fields[i].Name = fname then
        begin
          foundField := True;
          fieldIndex := i;
          Break;
        end;
      end;
      if not foundField then
      begin
        FDiag.Error('unknown field: ' + fname, expr.Span);
        Result := -1;
        Exit;
      end;
      // Load field: dest = load from [localSlot + fieldIndex * 8]
      resultTemp := NewTemp;
      instr.Op := irLoadField;
      instr.Dest := resultTemp;
      instr.Src1 := loc;  // base address (local slot)
      instr.ImmInt := fieldIndex;  // field index (backend multiplies by 8)
      Emit(instr);
      Exit(resultTemp);
    end;
    // For complex expressions, use the lowered value (not fully supported)
    FDiag.Error('field access on complex expression not supported', expr.Span);
    Result := -1;
    Exit;
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
    else if TAstCall(expr).Name = 'print_float' then
    begin
      t1 := LowerExpr(TAstCall(expr).Args[0]);
      instr.Op := irCallBuiltin;
      instr.ImmStr := 'print_float';
      instr.Src1 := t1;
      Emit(instr);
      Exit(-1);
    end
    else if TAstCall(expr).Name = 'strlen' then
    begin
      t1 := LowerExpr(TAstCall(expr).Args[0]);
      resultTemp := NewTemp;
      instr.Op := irCallBuiltin;
      instr.ImmStr := 'strlen';
      instr.Src1 := t1;
      instr.Dest := resultTemp;  // Return value destination
      Emit(instr);
      Exit(resultTemp);
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
    forNode: TAstFor;
    repNode: TAstRepeatUntil;
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
    // struct variable handling
    st: TAstStructLit;
    fieldValue: TAstExpr;
    valTemp: Integer;
    j: Integer;
    // field assign variables
    fa: TAstFieldAssign;
    declTypeName: string;
    td: TAstTypeDecl;
    fieldOffset: Integer;
    fieldIndex: Integer;
    foundField: Boolean;
    // array assignment variables
    arrayAssignStmt: TAstArrayAssign;
    arrayTemp, indexTemp, valueTemp: Integer;
    // for loop variables
    startTmp, endTmp, iTmp, incTmp, cmpTmp: Integer;
    forLoc: Integer;
    endSlot: Integer;
  begin
  instr := Default(TIRInstr);
  Result := True;
  if stmt is TAstVarDecl then
  begin
    loc := AllocLocal(TAstVarDecl(stmt).Name, TAstVarDecl(stmt).DeclType);
    // If this is a struct type, store the type name for field access
    if TAstVarDecl(stmt).DeclTypeName <> '' then
    begin
      SetLength(FLocalTypeNames, FCurrentFunc.LocalCount);
      FLocalTypeNames[loc] := TAstVarDecl(stmt).DeclTypeName;
    end;

    // Special handling for struct types with struct literal init
    // Check DeclTypeName to identify named struct types
    if (TAstVarDecl(stmt).DeclTypeName <> '') and
       (TAstVarDecl(stmt).InitExpr is TAstStructLit) then
    begin
      // Get struct type declaration
      declTypeName := TAstVarDecl(stmt).DeclTypeName;
      td := ResolveTypeDecl(declTypeName);
      if td = nil then
      begin
        FDiag.Error('unknown struct type: ' + declTypeName, stmt.Span);
        Exit(False);
      end;
      // Get the struct literal
      st := TAstStructLit(TAstVarDecl(stmt).InitExpr);
      // For each field, store directly to the local's memory area
      for i := 0 to High(td.Fields) do
      begin
        // Find value for this field in the literal
        fieldValue := nil;
        for j := 0 to st.FieldCount - 1 do
        begin
          if st.GetFieldName(j) = td.Fields[i].Name then
          begin
            fieldValue := st.GetFieldValue(j);
            Break;
          end;
        end;
        if fieldValue = nil then
        begin
          FDiag.Error('missing field in struct literal: ' + td.Fields[i].Name, stmt.Span);
          Continue;
        end;
        // Lower the field value
        valTemp := LowerExpr(fieldValue);
        // Store field at field index (backend multiplies by 8)
        instr.Op := irStoreField;
        instr.Dest := 0;
        instr.Src1 := loc;  // local slot as base
        instr.Src2 := valTemp;
        instr.ImmInt := i;  // field index (backend will multiply by 8)
        Emit(instr);
      end;
      Exit(True);
    end;

    // If initializer is constant integer and the local has narrower signed width, constant fold
    // BUT NOT for struct types!
    if (TAstVarDecl(stmt).DeclType <> atStruct) and (TAstVarDecl(stmt).InitExpr is TAstIntLit) then
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
          cvLocal := TConstValue.Create(signedVal);
          if loc >= Length(FLocalConst) then SetLength(FLocalConst, loc+1);
          FLocalConst[loc] := cvLocal;
        end
        else
        begin
          // unsigned: record local constant zero-extended value
          cvLocal := TConstValue.Create(Int64(truncated));
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

  if stmt is TAstFieldAssign then
  begin
    // Field assignment: obj.field := value
    fa := TAstFieldAssign(stmt);
    // Only support simple identifier as object
    if not (fa.Obj is TAstIdent) then
    begin
      FDiag.Error('field assignment on complex expression not supported', stmt.Span);
      Exit(False);
    end;
    // Get local slot
    loc := ResolveLocal(TAstIdent(fa.Obj).Name);
    if loc < 0 then
    begin
      FDiag.Error('use of undeclared local ' + TAstIdent(fa.Obj).Name, stmt.Span);
      Exit(False);
    end;
    // Get type name
    declTypeName := GetLocalTypeName(loc);
    if declTypeName = '' then
    begin
      FDiag.Error('not a struct variable: ' + TAstIdent(fa.Obj).Name, stmt.Span);
      Exit(False);
    end;
    // Get type declaration
    td := ResolveTypeDecl(declTypeName);
    if td = nil then
    begin
      FDiag.Error('unknown struct type: ' + declTypeName, stmt.Span);
      Exit(False);
    end;
    // Find field and compute index
    fieldIndex := 0;
    foundField := False;
    for i := 0 to High(td.Fields) do
    begin
      if td.Fields[i].Name = fa.Field then
      begin
        foundField := True;
        fieldIndex := i;
        Break;
      end;
    end;
    if not foundField then
    begin
      FDiag.Error('unknown field: ' + fa.Field, stmt.Span);
      Exit(False);
    end;
    // Lower value expression
    valueTemp := LowerExpr(fa.Value);
    // Store field: *(localSlot + fieldIndex * 8) = value
    instr.Op := irStoreField;
    instr.Dest := 0;
    instr.Src1 := loc;
    instr.Src2 := valueTemp;
    instr.ImmInt := fieldIndex;  // field index (backend multiplies by 8)
    Emit(instr);
    Exit(True);
  end;

  if stmt is TAstArrayAssign then
  begin
    // Array assignment: arr[index] := value
    arrayAssignStmt := TAstArrayAssign(stmt);
    
    // Lower array expression (should return array base address)
    arrayTemp := LowerExpr(arrayAssignStmt.ArrayExpr);
    
    // Lower index expression
    indexTemp := LowerExpr(arrayAssignStmt.Index);
    
    // Lower value expression
    valueTemp := LowerExpr(arrayAssignStmt.Value);
    
    // Store element: array[index] = value (dynamic)
    instr.Op := irStoreElemDyn;
    instr.Dest := 0; // unused
    instr.Src1 := arrayTemp;  // array base address
    instr.Src2 := indexTemp;  // index temp
    instr.Src3 := valueTemp;  // value to store
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
      forNode := TAstFor(stmt);
      startLabel := NewLabel('Lfor');
      exitLabel := NewLabel('Lfor_end');

      // Allocate loop variable
      forLoc := AllocLocal(forNode.VarName, atInt64);

      // Lower start expression and store to loop variable
      startTmp := LowerExpr(forNode.StartExpr);
      instr.Op := irStoreLocal;
      instr.Dest := forLoc;
      instr.Src1 := startTmp;
      Emit(instr);

      // Lower end expression and store to a hidden local (evaluated once)
      endSlot := AllocLocal('__for_end_' + forNode.VarName, atInt64);
      endTmp := LowerExpr(forNode.EndExpr);
      instr.Op := irStoreLocal;
      instr.Dest := endSlot;
      instr.Src1 := endTmp;
      Emit(instr);

      // Loop start label
      instr.Op := irLabel; instr.LabelName := startLabel; Emit(instr);

      // Load loop variable and end value, compare
      iTmp := NewTemp;
      instr.Op := irLoadLocal;
      instr.Dest := iTmp;
      instr.Src1 := forLoc;
      Emit(instr);

      endTmp := NewTemp;
      instr.Op := irLoadLocal;
      instr.Dest := endTmp;
      instr.Src1 := endSlot;
      Emit(instr);

      cmpTmp := NewTemp;
      if forNode.IsDownto then
        instr.Op := irCmpGe  // i >= end for downto
      else
        instr.Op := irCmpLe; // i <= end for to
      instr.Dest := cmpTmp;
      instr.Src1 := iTmp;
      instr.Src2 := endTmp;
      Emit(instr);

      // Branch to exit if condition is false
      instr.Op := irBrFalse;
      instr.Src1 := cmpTmp;
      instr.LabelName := exitLabel;
      Emit(instr);

      // Loop body (support break -> exitLabel)
      FBreakStack.AddObject(exitLabel, nil);
      LowerStmt(forNode.Body);
      FBreakStack.Delete(FBreakStack.Count - 1);

      // Increment/decrement loop variable
      iTmp := NewTemp;
      instr.Op := irLoadLocal;
      instr.Dest := iTmp;
      instr.Src1 := forLoc;
      Emit(instr);

      // Create constant 1
      incTmp := NewTemp;
      instr.Op := irConstInt;
      instr.Dest := incTmp;
      instr.ImmInt := 1;
      Emit(instr);

      // Add or subtract
      cmpTmp := NewTemp;
      if forNode.IsDownto then
        instr.Op := irSub
      else
        instr.Op := irAdd;
      instr.Dest := cmpTmp;
      instr.Src1 := iTmp;
      instr.Src2 := incTmp;
      Emit(instr);

      // Store back
      instr.Op := irStoreLocal;
      instr.Dest := forLoc;
      instr.Src1 := cmpTmp;
      Emit(instr);

      // Jump back to start
      instr.Op := irJmp; instr.LabelName := startLabel; Emit(instr);

      // Exit label
      instr.Op := irLabel; instr.LabelName := exitLabel; Emit(instr);
      Exit(True);
    end;

    if stmt is TAstRepeatUntil then
    begin
      repNode := TAstRepeatUntil(stmt);
      startLabel := NewLabel('Lrepeat');
      exitLabel := NewLabel('Lrepeat_end');

      // Start label
      instr.Op := irLabel; instr.LabelName := startLabel; Emit(instr);

      // Body (support break -> exitLabel)
      FBreakStack.AddObject(exitLabel, nil);
      LowerStmt(repNode.Body);
      FBreakStack.Delete(FBreakStack.Count - 1);

      // Evaluate condition
      condTmp := LowerExpr(repNode.Cond);

      // If condition is FALSE, jump back to start (repeat UNTIL true)
      instr.Op := irBrFalse;
      instr.Src1 := condTmp;
      instr.LabelName := startLabel;
      Emit(instr);

      // Exit label (for break)
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

{ TConstValue constructors }

constructor TConstValue.Create(val: Int64);
begin
  inherited Create;
  Kind := cvInt;
  IntVal := val;
end;

constructor TConstValue.Create(val: Double);
begin
  inherited Create;
  Kind := cvFloat;
  FloatVal := val;
end;

constructor TConstValue.Create(const val: string);
begin
  inherited Create;
  Kind := cvString;
  StrVal := val;
end;

end.
