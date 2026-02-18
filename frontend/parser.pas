{$mode objfpc}{$H+}
unit parser;

interface

uses
  SysUtils, Classes,
  diag, lexer, ast;

type
  TParser = class
  private
    FLexer: TLexer;
    FDiag: TDiagnostics;
    FCurTok: TToken;
    FHasCur: Boolean;

    procedure Advance; // setzt FCurTok
    function Check(kind: TTokenKind): Boolean;
    function Accept(kind: TTokenKind): Boolean;
    procedure Expect(kind: TTokenKind);

    // Parsing-Methoden
    function ParseTopDecl: TAstNode;
    function ParseFuncDecl(isPub: Boolean): TAstFuncDecl;
    function ParseExternDecl: TAstFuncDecl;
    function ParseConDecl(isPub: Boolean): TAstConDecl;
    function ParseTypeDecl(isPub: Boolean): TAstTypeDecl;
    function ParseUnitDecl: TAstUnitDecl;
    function ParseImportDecl: TAstImportDecl;

    function ParseBlock: TAstBlock;
    function ParseStmt: TAstStmt;
    function ParseVarLetCoDecl: TAstVarDecl;
    function ParseForStmt: TAstFor;
    function ParseRepeatUntilStmt: TAstRepeatUntil;
    function ParseAssignStmtOrExprStmt: TAstStmt;

    // Expressions (Präzedenz): Or -> And -> Cmp -> Add -> Mul -> Unary -> Primary -> Postfix
    function ParseExpr: TAstExpr;
    function ParseOrExpr: TAstExpr;
    function ParseAndExpr: TAstExpr;
    function ParseCmpExpr: TAstExpr;
    function ParseAddExpr: TAstExpr;
    function ParseMulExpr: TAstExpr;
    function ParseUnaryExpr: TAstExpr;
    function ParsePrimary: TAstExpr;
    function ParseCallOrIdent: TAstExpr;
    function ParsePostfix(base: TAstExpr): TAstExpr;

    function ParseType: TLyxType;
    function ParseParamList: TAstParamList;
    function ParseParamListWithVarargs(out isVarArgs: Boolean): TAstParamList;
    function ParseArrayLiteral: TAstExpr;
    function ParseStructLiteral(const typeName: string): TAstExpr;
  public
    constructor Create(lexer: TLexer; diag: TDiagnostics);
    destructor Destroy; override;

    function ParseProgram: TAstProgram;
  end;

implementation

{ Helpers }

procedure TParser.Advance;
begin
  FCurTok := FLexer.NextToken;
  FHasCur := True;
end;

function TParser.Check(kind: TTokenKind): Boolean;
begin
  // Ensure token is present (constructor preloads)
  Result := FCurTok.Kind = kind;
end;

function TParser.Accept(kind: TTokenKind): Boolean;
begin
  if FCurTok.Kind = kind then
  begin
    Advance;
    Exit(True);
  end;
  Result := False;
end;

procedure TParser.Expect(kind: TTokenKind);
begin
  if not Accept(kind) then
  begin
    FDiag.Error(Format('expected token %s but got %s', [TokenKindToStr(kind), TokenKindToStr(FCurTok.Kind)]), FCurTok.Span);
    // try to continue: advance once
    Advance;
  end;
end;

{ Parsing }

constructor TParser.Create(lexer: TLexer; diag: TDiagnostics);
begin
  inherited Create;
  FLexer := lexer;
  FDiag := diag;
  FHasCur := True;
  Advance; // load first token
end;

destructor TParser.Destroy;
begin
  inherited Destroy;
end;

function TParser.ParseProgram: TAstProgram;
var
  decls: TAstNodeList;
  d: TAstNode;
begin
  decls := nil;
  // optional unit declaration
  if Check(tkUnit) then
  begin
    d := ParseUnitDecl;
    if d <> nil then
    begin
      SetLength(decls, Length(decls) + 1);
      decls[High(decls)] := d;
    end;
  end;
  // import declarations
  while Check(tkImport) do
  begin
    d := ParseImportDecl;
    if d <> nil then
    begin
      SetLength(decls, Length(decls) + 1);
      decls[High(decls)] := d;
    end;
  end;
  // top-level declarations
  while True do
  begin
    if Check(tkEOF) then Break;
    d := ParseTopDecl;
    if d <> nil then
    begin
      SetLength(decls, Length(decls) + 1);
      decls[High(decls)] := d;
    end
    else
      // Skip token to avoid infinite loop
      Advance;
  end;
  Result := TAstProgram.Create(decls, MakeSpan(1,1,0,''));
end;

function TParser.ParseTopDecl: TAstNode;
begin
  if Check(tkPub) then
  begin
    Advance; // consume 'pub'
    if Check(tkFn) then
      Exit(ParseFuncDecl(True))
    else if Check(tkCon) then
      Exit(ParseConDecl(True))
    else if Check(tkType) then
      Exit(ParseTypeDecl(True))
    else
    begin
      FDiag.Error('expected fn, con or type after pub', FCurTok.Span);
      Exit(nil);
    end;
  end;
  if Check(tkExtern) then
  begin
    // extern function declaration
    Advance; // consume 'extern'
    if Check(tkFn) then
      Exit(ParseExternDecl)
    else
    begin
      FDiag.Error('expected fn after extern', FCurTok.Span);
      Exit(nil);
    end;
  end;
  if Check(tkFn) then
    Exit(ParseFuncDecl(False));
  if Check(tkCon) then
    Exit(ParseConDecl(False));
  if Check(tkType) then
    Exit(ParseTypeDecl(False));
  // unexpected top-level
  FDiag.Error('unexpected top-level declaration', FCurTok.Span);
  Result := nil;
end;

function TParser.ParseFuncDecl(isPub: Boolean): TAstFuncDecl;
var
  name: string;
  params: TAstParamList;
  retType: TLyxType;
  body: TAstBlock;
begin
  // fn
  Expect(tkFn);
  if Check(tkIdent) then
  begin
    name := FCurTok.Value;
    Advance;
  end
  else
  begin
    name := '<anon>';
    FDiag.Error('expected function name', FCurTok.Span);
  end;

  // (
  Expect(tkLParen);
  if not Check(tkRParen) then
    params := ParseParamList
  else
    params := nil;
  Expect(tkRParen);

  // optional : RetType
  if Accept(tkColon) then
    retType := ParseType
  else
    retType := atVoid; // default

  body := ParseBlock;
  Result := TAstFuncDecl.Create(name, params, retType, body, FCurTok.Span, isPub, False);
end;

function TParser.ParseExternDecl: TAstFuncDecl;
var
  name: string;
  params: TAstParamList;
  retType: TLyxType;
  isVarArgs: Boolean;
  callingConv: string;
begin
  // Allow optional calling convention identifier before 'fn', e.g. 'extern c fn'
  callingConv := '';
  if Check(tkIdent) and (FCurTok.Value = 'c') then
  begin
    callingConv := 'c';
    Advance; // consume 'c'
  end;
  Expect(tkFn);
  if Check(tkIdent) then
  begin
    name := FCurTok.Value;
    Advance;
  end
  else
  begin
    name := '<anon>';
    FDiag.Error('expected function name', FCurTok.Span);
  end;

  Expect(tkLParen);
  if not Check(tkRParen) then
    params := ParseParamListWithVarargs(isVarArgs)
  else
  begin
    params := nil;
    isVarArgs := False;
  end;
  Expect(tkRParen);

  if Accept(tkColon) then
    retType := ParseType
  else
    retType := atVoid;

  Expect(tkSemicolon);
  // create function decl with no body and IsExtern flag
  // For now, calling convention not parsed separately (empty)
  Result := TAstFuncDecl.Create(name, params, retType, nil, FCurTok.Span, False, True, isVarArgs, 'c');
end;

function TParser.ParseConDecl(isPub: Boolean): TAstConDecl;
var
  name: string;
  declType: TLyxType;
  initExpr: TAstExpr;
begin
  Expect(tkCon);
  if Check(tkIdent) then
  begin
    name := FCurTok.Value;
    Advance;
  end
  else
  begin
    name := '<anon>';
    FDiag.Error('expected constant name', FCurTok.Span);
  end;
  Expect(tkColon);
  declType := ParseType;
  Expect(tkAssign); // ':='
  initExpr := ParseExpr; // ConstExpr restriction checked in sema
  Expect(tkSemicolon);
  Result := TAstConDecl.Create(name, declType, initExpr, FCurTok.Span);
end;

function TParser.ParseTypeDecl(isPub: Boolean): TAstTypeDecl;
var
  name: string;
  declType: TLyxType;
  fields: TAstTypeFieldList;
  field: TAstTypeField;
begin
  Expect(tkType);
  if Check(tkIdent) then
  begin
    name := FCurTok.Value;
    Advance;
  end
  else
  begin
    name := '<anon>';
    FDiag.Error('expected type name', FCurTok.Span);
  end;
  // ':='
  Expect(tkAssign);
  // If next token is 'struct', parse field list
  fields := nil;
  if Accept(tkStruct) then
  begin
    Expect(tkLBrace);
    while not Check(tkRBrace) and not Check(tkEOF) do
    begin
      if Check(tkIdent) then
      begin
        field.Name := FCurTok.Value; Advance;
        Expect(tkColon);
        field.FieldType := ParseType;
        SetLength(fields, Length(fields) + 1);
        fields[High(fields)] := field;
        Expect(tkSemicolon);
        Continue;
      end
      else
      begin
        FDiag.Error('expected field declaration in struct', FCurTok.Span);
        Break;
      end;
    end;
    Expect(tkRBrace);
    declType := atStruct;
  end
  else
    declType := ParseType;

  Expect(tkSemicolon);
  Result := TAstTypeDecl.Create(name, declType, isPub, FCurTok.Span);
  if declType = atStruct then
    Result.SetStructFields(fields);
end;

function TParser.ParseUnitDecl: TAstUnitDecl;
var
  path: string;
begin
  Expect(tkUnit);
  if Check(tkIdent) then
  begin
    path := FCurTok.Value;
    Advance;
    while Accept(tkDot) do
    begin
      if Check(tkIdent) then
      begin
        path := path + '.' + FCurTok.Value;
        Advance;
      end
      else
        FDiag.Error('expected identifier after dot in unit path', FCurTok.Span);
    end;
  end
  else
  begin
    path := '<anon>';
    FDiag.Error('expected unit name', FCurTok.Span);
  end;
  Expect(tkSemicolon);
  Result := TAstUnitDecl.Create(path, FCurTok.Span);
end;

function TParser.ParseImportDecl: TAstImportDecl;
var
  path, alias: string;
  items: TAstImportItemList;
  item: TAstImportItem;
begin
  Expect(tkImport);
  items := nil;
  alias := '';
  if Check(tkIdent) then
  begin
    path := FCurTok.Value;
    Advance;
    while Accept(tkDot) do
    begin
      if Check(tkIdent) then
      begin
        path := path + '.' + FCurTok.Value;
        Advance;
      end
      else
        FDiag.Error('expected identifier after dot in import path', FCurTok.Span);
    end;
  end
  else
  begin
    path := '<anon>';
    FDiag.Error('expected module path in import', FCurTok.Span);
  end;
  // optional 'as Alias'
  if Accept(tkAs) then
  begin
    if Check(tkIdent) then
    begin
      alias := FCurTok.Value;
      Advance;
    end
    else
      FDiag.Error('expected alias after as', FCurTok.Span);
  end;
  // optional selective import { item, item }
  if Accept(tkLBrace) then
  begin
    while not Check(tkRBrace) and not Check(tkEOF) do
    begin
      item.Alias := '';
      if Check(tkIdent) then
      begin
        item.Name := FCurTok.Value;
        Advance;
      end
      else
      begin
        item.Name := '<anon>';
        FDiag.Error('expected identifier in import list', FCurTok.Span);
      end;
      if Accept(tkAs) then
      begin
        if Check(tkIdent) then
        begin
          item.Alias := FCurTok.Value;
          Advance;
        end
        else
          FDiag.Error('expected alias after as', FCurTok.Span);
      end;
      SetLength(items, Length(items) + 1);
      items[High(items)] := item;
      if not Accept(tkComma) then Break;
    end;
    Expect(tkRBrace);
  end;
  Expect(tkSemicolon);
  Result := TAstImportDecl.Create(path, alias, items, FCurTok.Span);
end;

function TParser.ParseBlock: TAstBlock;
var
  stmts: TAstStmtList;
  s: TAstStmt;
begin
  Expect(tkLBrace);
  stmts := nil;
  while not Check(tkRBrace) and not Check(tkEOF) do
  begin
    s := ParseStmt;
    if s <> nil then
    begin
      SetLength(stmts, Length(stmts) + 1);
      stmts[High(stmts)] := s;
    end
    else
      Advance;
  end;
  Expect(tkRBrace);
  Result := TAstBlock.Create(stmts, FCurTok.Span);
end;

function TParser.ParseStmt: TAstStmt;
var
  cond: TAstExpr;
  thenStmt: TAstStmt;
  elseStmt: TAstStmt;
  bodyStmt: TAstStmt;
  vExpr: TAstExpr;
  cases: TAstCaseList;
  defaultBody: TAstStmt;
  caseObj: TAstCase;
  valExpr: TAstExpr;
  i: Integer;
begin
  if Check(tkVar) or Check(tkLet) or Check(tkCo) then
    Exit(ParseVarLetCoDecl);

  if Check(tkIf) then
  begin
    // if (Expr) Stmt [else Stmt] - REQUIRES parentheses for now
    Advance; // if
    Expect(tkLParen);
    cond := ParseExpr;
    Expect(tkRParen);
    thenStmt := Self.ParseStmt;
    elseStmt := nil;
    if Accept(tkElse) then
      elseStmt := Self.ParseStmt;
    Exit(TAstIf.Create(cond, thenStmt, elseStmt, cond.Span));
  end;

  if Check(tkWhile) then
  begin
    Advance;
    // Flexible while syntax: "while (condition)" or "while condition"
    if Accept(tkLParen) then
    begin
      cond := ParseExpr;
      Expect(tkRParen);
    end
    else
    begin
      cond := ParseExpr;
    end;
    bodyStmt := Self.ParseStmt;
    Exit(TAstWhile.Create(cond, bodyStmt, cond.Span));
  end;

  // switch (expr) { case CONST: stmt ... default: stmt }
  if Check(tkSwitch) then
  begin
    Advance; // switch
    Expect(tkLParen);
    cond := ParseExpr;
    Expect(tkRParen);
    Expect(tkLBrace);
    // collect cases
    SetLength(cases, 0);
    defaultBody := nil;
    while not Check(tkRBrace) and not Check(tkEOF) do
    begin
      if Accept(tkCase) then
      begin
        // parse single const expr
        valExpr := ParseExpr;
        Expect(tkColon);
        // case body: allow either a block or a single statement
        if Check(tkLBrace) then
          bodyStmt := ParseBlock
        else
          bodyStmt := ParseStmt;
        caseObj := TAstCase.Create(valExpr, bodyStmt);
        SetLength(cases, Length(cases) + 1);
        cases[High(cases)] := caseObj;
        Continue;
      end
      else if Accept(tkDefault) then
      begin
        Expect(tkColon);
        // default body must be a block
        defaultBody := ParseBlock;
        Continue;
      end

      else
      begin
        // unexpected token inside switch
        FDiag.Error('unexpected token in switch', FCurTok.Span);
        Advance;
      end;
    end;
    Expect(tkRBrace);
    Exit(TAstSwitch.Create(cond, cases, defaultBody, cond.Span));
  end;

  if Check(tkFor) then
    Exit(ParseForStmt);

  if Check(tkRepeat) then
    Exit(ParseRepeatUntilStmt);

  if Check(tkReturn) then
  begin
    Advance;
    if Check(tkSemicolon) then
    begin
      Advance;
      Exit(TAstReturn.Create(nil, FCurTok.Span));
    end
    else
    begin
      vExpr := ParseExpr;
      Expect(tkSemicolon);
      Exit(TAstReturn.Create(vExpr, vExpr.Span));
    end;
  end;

  if Check(tkLBrace) then
    Exit(ParseBlock);

  // Assignment or expression statement
  Exit(ParseAssignStmtOrExprStmt);
end;

function TParser.ParseVarLetCoDecl: TAstVarDecl;
var
  storage: TStorageKlass;
  name: string;
  declType: TLyxType;
  declTypeName: string;
  initExpr: TAstExpr;
begin
  if Accept(tkVar) then storage := skVar
  else if Accept(tkLet) then storage := skLet
  else if Accept(tkCo) then storage := skCo
  else storage := skVar; // unreachable

  if Check(tkIdent) then
  begin
    name := FCurTok.Value; Advance;
  end
  else
  begin
    name := '<anon>'; FDiag.Error('expected identifier in declaration', FCurTok.Span);
  end;

  Expect(tkColon);
  // capture type name if it's an identifier
  if Check(tkIdent) then
    declTypeName := FCurTok.Value
  else
    declTypeName := '';
  declType := ParseType;

  Expect(tkAssign);
  initExpr := ParseExpr;
  Expect(tkSemicolon);

  Result := TAstVarDecl.Create(storage, name, declType, declTypeName, initExpr, initExpr.Span);
end;

function TParser.ParseForStmt: TAstFor;
var
  varName: string;
  startExpr, endExpr: TAstExpr;
  isDownto: Boolean;
  bodyStmt: TAstStmt;
  span: TSourceSpan;
begin
  span := FCurTok.Span;
  Expect(tkFor);
  if Check(tkIdent) then
  begin
    varName := FCurTok.Value;
    Advance;
  end
  else
  begin
    varName := '<anon>';
    FDiag.Error('expected loop variable name', FCurTok.Span);
  end;
  Expect(tkAssign);
  startExpr := ParseExpr;
  isDownto := False;
  if Accept(tkDownto) then
    isDownto := True
  else
    Expect(tkTo);
  endExpr := ParseExpr;
  Expect(tkDo);
  bodyStmt := Self.ParseStmt;
  Result := TAstFor.Create(varName, startExpr, endExpr, isDownto, bodyStmt, span);
end;

function TParser.ParseRepeatUntilStmt: TAstRepeatUntil;
var
  bodyBlock: TAstBlock;
  cond: TAstExpr;
  span: TSourceSpan;
begin
  span := FCurTok.Span;
  Expect(tkRepeat);
  bodyBlock := ParseBlock;
  Expect(tkUntil);
  cond := ParseExpr;
  Expect(tkSemicolon);
  Result := TAstRepeatUntil.Create(bodyBlock, cond, span);
end;

function TParser.ParseAssignStmtOrExprStmt: TAstStmt;
var
  expr: TAstExpr;
  name: string;
  valExpr: TAstExpr;
  arrExpr: TAstExpr;
  indexExpr: TAstExpr;
  objExpr: TAstExpr;
  fieldName: string;
begin
  expr := ParseExpr;
  // Assignment pattern: ident := expr ;
  // (for now, only simple ident assignment; field/index LValues parsed but not assignable yet)
  if (expr is TAstIdent) and Check(tkAssign) then
  begin
    name := TAstIdent(expr).Name;
    Advance; // consume ':='
    valExpr := ParseExpr;
    Expect(tkSemicolon);
    // free ident node
    expr.Free;
    Exit(TAstAssign.Create(name, valExpr, valExpr.Span));
  end
  else if (expr is TAstIndexAccess) and Check(tkAssign) then
  begin
    // transfer ownership from index access to array assign
    TAstArrayIndex(expr).TransferOwnership(arrExpr, indexExpr);
    expr.Free; // free the old TAstArrayIndex node

    Advance; // consume ':='
    valExpr := ParseExpr;
    Expect(tkSemicolon);
    Exit(TAstArrayAssign.Create(arrExpr, indexExpr, valExpr, valExpr.Span));
  end
  else if (expr is TAstFieldAccess) and Check(tkAssign) then
  begin
    // Field assignment: obj.field := value
    // Transfer ownership from field access before freeing it
    TAstFieldAccess(expr).TransferOwnership(objExpr, fieldName);
    Advance; // consume ':='
    valExpr := ParseExpr;
    Expect(tkSemicolon);
    Result := TAstFieldAssign.Create(objExpr, fieldName, valExpr, valExpr.Span);
    expr.Free;
  end
  else
  begin
    Expect(tkSemicolon);
    Exit(TAstExprStmt.Create(expr, expr.Span));
  end;
end;

{ Expressions }

function TParser.ParseExpr: TAstExpr;
begin
  Result := ParseOrExpr;
end;

function TParser.ParseOrExpr: TAstExpr;
var
  rhs: TAstExpr;
begin
  Result := ParseAndExpr;
  while Accept(tkOr) do
  begin
    rhs := ParseAndExpr;
    Result := TAstBinOp.Create(tkOr, Result, rhs, Result.Span);
  end;
end;

function TParser.ParseAndExpr: TAstExpr;
var
  rhs: TAstExpr;
begin
  Result := ParseCmpExpr;
  while Accept(tkAnd) do
  begin
    rhs := ParseCmpExpr;
    Result := TAstBinOp.Create(tkAnd, Result, rhs, Result.Span);
  end;
end;

function TParser.ParseCmpExpr: TAstExpr;
var
  op: TTokenKind;
  rhs: TAstExpr;
begin
  Result := ParseAddExpr;
  if Check(tkEq) or Check(tkNeq) or Check(tkLt) or Check(tkLe) or Check(tkGt) or Check(tkGe) then
  begin
    op := FCurTok.Kind; Advance;
    rhs := ParseAddExpr;
    Result := TAstBinOp.Create(op, Result, rhs, Result.Span);
  end;
end;

function TParser.ParseAddExpr: TAstExpr;
var
  op: TTokenKind;
  rhs: TAstExpr;
begin
  Result := ParseMulExpr;
  while Check(tkPlus) or Check(tkMinus) do
  begin
    op := FCurTok.Kind; Advance;
    rhs := ParseMulExpr;
    Result := TAstBinOp.Create(op, Result, rhs, Result.Span);
  end;
end;

function TParser.ParseMulExpr: TAstExpr;
var
  op: TTokenKind;
  rhs: TAstExpr;
begin
  Result := ParseUnaryExpr;
  while Check(tkStar) or Check(tkSlash) or Check(tkPercent) do
  begin
    op := FCurTok.Kind; Advance;
    rhs := ParseUnaryExpr;
    Result := TAstBinOp.Create(op, Result, rhs, Result.Span);
  end;
end;

function TParser.ParseUnaryExpr: TAstExpr;
var
  op: TTokenKind;
  operand: TAstExpr;
  span: TSourceSpan;
  value: Int64;
begin
  // Simple, non-recursive approach for debugging
  if Check(tkMinus) then
  begin
    span := FCurTok.Span;
    Advance;
    if Check(tkIntLit) then
    begin
      // fold unary minus applied directly to integer literal
      value := -StrToInt64Def(FCurTok.Value, 0);
      span := FCurTok.Span;
      Advance;
      Exit(TAstIntLit.Create(value, span));
    end;
    if Check(tkFloatLit) then
    begin
      // fold unary minus applied directly to float literal
      Result := TAstFloatLit.Create('-' + FCurTok.Value, FCurTok.Span);
      Advance;
      Exit;
    end;
    // For variables and other expressions, use ParsePrimary directly
    operand := ParsePrimary;
    Result := TAstUnaryOp.Create(tkMinus, operand, span);
    Exit;
  end;

  if Check(tkNot) then
  begin
    op := FCurTok.Kind;
    Advance;
    // For variables and other expressions, use ParsePrimary directly
    operand := ParsePrimary;
    Result := TAstUnaryOp.Create(op, operand, operand.Span);
    Exit;
  end;

  Result := ParsePrimary;
end;

function TParser.ParsePrimary: TAstExpr;
var
  v: Int64;
  span: TSourceSpan;
  s: string;
  b: Boolean;
  e: TAstExpr;
  dummy: TAstIntLit;
begin
  if Check(tkIntLit) then
  begin
    v := StrToInt64Def(FCurTok.Value, 0);
    span := FCurTok.Span;
    Advance;
    Exit(TAstIntLit.Create(v, span));
  end;

  if Check(tkFloatLit) then
  begin
    s := FCurTok.Value;
    span := FCurTok.Span;
    Advance;
    Exit(TAstFloatLit.Create(s, span));
  end;

  if Check(tkStrLit) then
  begin
    s := FCurTok.Value;
    span := FCurTok.Span;
    Advance;
    Exit(TAstStrLit.Create(s, span));
  end;

  if Check(tkCharLit) then
  begin
    span := FCurTok.Span;
    if Length(FCurTok.Value) > 0 then
      Result := TAstCharLit.Create(FCurTok.Value[1], span)
    else
      Result := TAstCharLit.Create(#0, span);
    Advance;
    Exit;
  end;

  if Check(tkTrue) or Check(tkFalse) then
  begin
    b := Check(tkTrue);
    span := FCurTok.Span;
    Advance;
    Exit(TAstBoolLit.Create(b, span));
  end;

  if Check(tkIdent) then
    Exit(ParseCallOrIdent);

  if Accept(tkLParen) then
  begin
    e := ParseExpr;
    Expect(tkRParen);
    Exit(e);
  end;

  if Check(tkLBracket) then
    Exit(ParseArrayLiteral);

  // unexpected primary
  FDiag.Error('unexpected token in expression: ' + TokenKindToStr(FCurTok.Kind), FCurTok.Span);
  // create dummy literal
  dummy := TAstIntLit.Create(0, FCurTok.Span);
  Advance;
  Result := dummy;
end;

function TParser.ParseCallOrIdent: TAstExpr;
var
  name: string;
  args: TAstExprList;
  span: TSourceSpan;
  a: TAstExpr;
begin
  name := FCurTok.Value;
  span := FCurTok.Span;
  Advance; // consume ident
  if Accept(tkLParen) then
  begin
    args := nil;
    if not Check(tkRParen) then
    begin
      // ArgList
      while True do
      begin
        a := ParseExpr;
        SetLength(args, Length(args) + 1);
        args[High(args)] := a;
        if Accept(tkComma) then Continue;
        Break;
      end;
    end;
    Expect(tkRParen);
    Result := ParsePostfix(TAstCall.Create(name, args, span));
  end
  else if Check(tkLBrace) then
  begin
    // Struct-Literal: TypeName { field: value, ... }
    Result := ParsePostfix(ParseStructLiteral(name));
  end
  else
    Result := ParsePostfix(TAstIdent.Create(name, span));
end;

function TParser.ParsePostfix(base: TAstExpr): TAstExpr;
var
  fieldName: string;
  indexExpr: TAstExpr;
begin
  Result := base;
  while True do
  begin
    if Accept(tkDot) then
    begin
      if Check(tkIdent) then
      begin
        fieldName := FCurTok.Value;
        Advance;
        Result := TAstFieldAccess.Create(Result, fieldName, Result.Span);
      end
      else
      begin
        FDiag.Error('expected field name after .', FCurTok.Span);
        Exit;
      end;
    end
    else if Accept(tkLBracket) then
    begin
      indexExpr := ParseExpr;
      Expect(tkRBracket);
      Result := TAstIndexAccess.Create(Result, indexExpr, Result.Span);
    end
    else
      Break;
  end;
end;

function TParser.ParseType: TLyxType;
var s: string;
    dummyType: TLyxType;
    braceCount: Integer;
begin
  if Accept(tkArray) then // Check for array keyword first
  begin
    Result := atArray;
    Exit;
  end;

  if Accept(tkStruct) then // Check for struct keyword
  begin
    // Parse struct body { field: type; ... }
    // For now, just consume the syntax and return atStruct
    Expect(tkLBrace);
    // Skip struct body: consume tokens until matching }
    braceCount := 1;
    while (braceCount > 0) and not Check(tkEOF) do
    begin
      Advance; // move to next token
      if Check(tkLBrace) then Inc(braceCount)
      else if Check(tkRBrace) then Dec(braceCount);
    end;
    Result := atStruct;
    Exit;
  end;

  if Check(tkIdent) then
  begin
    s := FCurTok.Value;
    Advance;
    Result := StrToLyxType(s);
  end
  else
  begin
    FDiag.Error('expected type name', FCurTok.Span);
    Result := atUnresolved;
  end;
end;

function TParser.ParseArrayLiteral: TAstExpr;
var
  items: TAstExprList;
  item: TAstExpr;
  span: TSourceSpan;
begin
  span := FCurTok.Span;
  Expect(tkLBracket);
  items := nil;
  while not Check(tkRBracket) and not Check(tkEOF) do
  begin
    item := ParseExpr;
    SetLength(items, Length(items) + 1);
    items[High(items)] := item;
    if not Accept(tkComma) then Break;
  end;
  Expect(tkRBracket);
  Result := TAstArrayLit.Create(items, span);
end;

function TParser.ParseStructLiteral(const typeName: string): TAstExpr;
var
  span: TSourceSpan;
  fieldName: string;
  fieldValue: TAstExpr;
begin
  span := FCurTok.Span;
  Result := TAstStructLit.Create(typeName, span);
  Expect(tkLBrace);
  while not Check(tkRBrace) and not Check(tkEOF) do
  begin
    if Check(tkIdent) then
    begin
      fieldName := FCurTok.Value;
      Advance;
      Expect(tkColon);
      fieldValue := ParseExpr;
      TAstStructLit(Result).AddField(fieldName, fieldValue);
      if not Accept(tkComma) then Break;
    end
    else
    begin
      FDiag.Error('expected field name in struct literal', FCurTok.Span);
      Break;
    end;
  end;
  Expect(tkRBrace);
end;

function TParser.ParseParamList: TAstParamList;
var
  params: TAstParamList;
  name: string;
  typ: TLyxType;
  p: TAstParam;
begin
  params := nil;
  while not Check(tkRParen) and not Check(tkEOF) do
  begin
    // varargs (ellipsis)
    if Check(tkEllipsis) then
    begin
      // do not consume here; caller will handle it after the parameter list
      Break;
    end;

    if Check(tkIdent) then
    begin
      name := FCurTok.Value; Advance;
    end
    else
    begin
      name := '<anon>'; FDiag.Error('expected parameter name', FCurTok.Span);
    end;
    Expect(tkColon);
    typ := ParseType;
    p.Name := name; p.ParamType := typ; p.Span := FCurTok.Span;
    SetLength(params, Length(params) + 1);
    params[High(params)] := p;
    if Accept(tkComma) then Continue else Break;
  end;
  Result := params;
end;

function TParser.ParseParamListWithVarargs(out isVarArgs: Boolean): TAstParamList;
var
  params: TAstParamList;
  name: string;
  typ: TLyxType;
  p: TAstParam;
begin
  params := nil;
  isVarArgs := False;
  
  while not Check(tkRParen) and not Check(tkEOF) do
  begin
    // varargs (ellipsis)
    if Check(tkEllipsis) then
    begin
      isVarArgs := True;
      Advance; // consume ellipsis
      Break; // no more parameters after ...
    end;

    if Check(tkIdent) then
    begin
      name := FCurTok.Value; Advance;
    end
    else
    begin
      name := '<anon>'; FDiag.Error('expected parameter name', FCurTok.Span);
    end;
    Expect(tkColon);
    typ := ParseType;
    p.Name := name; p.ParamType := typ; p.Span := FCurTok.Span;
    SetLength(params, Length(params) + 1);
    params[High(params)] := p;
    if Accept(tkComma) then Continue else Break;
  end;
  Result := params;
end;

end.
