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
    FLastParamListVarArgs: Boolean;

    procedure Advance; // setzt FCurTok
    function Check(kind: TTokenKind): Boolean;
    function Accept(kind: TTokenKind): Boolean;
    procedure Expect(kind: TTokenKind);

    // Parsing-Methoden
    function ParseTopDecl: TAstNode;
    function ParseFuncDecl(isPub: Boolean): TAstFuncDecl;
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

    // fields
    FLastParamListVarArgs: Boolean;

    function ParseTypeEx(out arrayLen: Integer): TAurumType;
    function ParseType: TAurumType;
    function ParseParamList: TAstParamList;
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
var isExtern: Boolean;
begin
  isExtern := False;
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
  // support 'extern fn ... ;' top-level declarations
  if Check(tkExtern) then
  begin
    isExtern := True;
    Advance;
  end;
  if isExtern then
  begin
    if Check(tkFn) then
    begin
      // parse function signature without body
      Expect(tkFn);
      // reuse ParseFuncDecl for name/params/retType parsing by peeking and backtracking is complex
      // Instead, parse inline here
      var name: string; params: TAstParamList; retType: TAurumType;
      if Check(tkIdent) then
      begin
        name := FCurTok.Value; Advance;
      end
      else
      begin
        name := '<anon>'; FDiag.Error('expected function name', FCurTok.Span);
      end;
      Expect(tkLParen);
      if not Check(tkRParen) then
        params := ParseParamList
      else
        params := nil;
      Expect(tkRParen);
      if Accept(tkColon) then
        retType := ParseType
      else
        retType := atVoid;
      Expect(tkSemicolon);
      Result := TAstFuncDecl.Create(name, params, retType, nil, FCurTok.Span, False);
      // mark extern and varargs if parser recorded them
      TAstFuncDecl(Result).IsExtern := True;
      TAstFuncDecl(Result).IsVarArgs := FLastParamListVarArgs;
      Exit(Result);
    end
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
  retType: TAurumType;
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
  Result := TAstFuncDecl.Create(name, params, retType, body, FCurTok.Span, isPub);
end;

function TParser.ParseConDecl(isPub: Boolean): TAstConDecl;
var
  name: string;
  declType: TAurumType;
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
  declType: TAurumType;
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
  // '='
  if Check(tkEq) then
    Advance
  else
  begin
    FDiag.Error('expected ''='' in type declaration', FCurTok.Span);
  end;
  declType := ParseType;
  Expect(tkSemicolon);
  Result := TAstTypeDecl.Create(name, declType, isPub, FCurTok.Span);
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
    // if (Expr) Stmt [else Stmt]
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
    Expect(tkLParen);
    cond := ParseExpr;
    Expect(tkRParen);
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
  declType: TAurumType;
  initExpr: TAstExpr;
  arrayLen: Integer;
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
  declType := ParseTypeEx(arrayLen);

  Expect(tkAssign);
  initExpr := ParseExpr;
  Expect(tkSemicolon);

  Result := TAstVarDecl.Create(storage, name, declType, arrayLen, initExpr, initExpr.Span);
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
  else if ((expr is TAstFieldAccess) or (expr is TAstIndexAccess)) and Check(tkAssign) then
  begin
    // TODO: full LValue assignment for field/index access
    FDiag.Error('field/index assignment not yet supported', expr.Span);
    Advance; // consume ':='
    valExpr := ParseExpr;
    Expect(tkSemicolon);
    expr.Free;
    valExpr.Free;
    Exit(TAstExprStmt.Create(TAstIntLit.Create(0, FCurTok.Span), FCurTok.Span));
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
  if Check(tkMinus) then
  begin
    span := FCurTok.Span;
    Advance;
    if Check(tkIntLit) then
    begin
      // fold unary minus applied directly to literal
      value := -StrToInt64Def(FCurTok.Value, 0);
      span := FCurTok.Span;
      Advance;
      Exit(TAstIntLit.Create(value, span));
    end;
    operand := ParseUnaryExpr;
    Result := TAstUnaryOp.Create(tkMinus, operand, operand.Span);
    Exit;
  end;

  if Check(tkNot) then
  begin
    op := FCurTok.Kind;
    Advance;
    operand := ParseUnaryExpr;
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
  items: TAstExprList;
  a: TAstExpr;
begin
  if Check(tkIntLit) then
  begin
    v := StrToInt64Def(FCurTok.Value, 0);
    span := FCurTok.Span;
    Advance;
    Exit(TAstIntLit.Create(v, span));
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

  if Accept(tkLBracket) then
  begin
    // array literal: [expr, expr, ...]
    items := nil;
    if not Check(tkRBracket) then
    begin
      while True do
      begin
        a := ParseExpr;
        SetLength(items, Length(items) + 1);
        items[High(items)] := a;
        if Accept(tkComma) then Continue;
        Break;
      end;
    end;
    Expect(tkRBracket);
    Exit(TAstArrayLit.Create(items, FCurTok.Span));
  end;

  if Accept(tkLParen) then
  begin
    e := ParseExpr;
    Expect(tkRParen);
    Exit(e);
  end;

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

function TParser.ParseTypeEx(out arrayLen: Integer): TAurumType;
var s: string;
begin
  arrayLen := 0;
  if Check(tkIdent) then
  begin
    s := FCurTok.Value;
    Advance;
    Result := StrToAurumType(s);
    // optional array suffix: [N] or []
    if Accept(tkLBracket) then
    begin
      if Check(tkRBracket) then
      begin
        // [] dynamic array
        arrayLen := -1;
        Advance; // consume ]
      end
      else if Check(tkIntLit) then
      begin
        arrayLen := StrToIntDef(FCurTok.Value, 0);
        Advance;
        Expect(tkRBracket);
      end
      else
      begin
        FDiag.Error('expected integer literal or ] in array type', FCurTok.Span);
        // try to recover
        if Check(tkRBracket) then Advance;
      end;
    end;
  end
  else
  begin
    FDiag.Error('expected type name', FCurTok.Span);
    Result := atUnresolved;
  end;
end;

function TParser.ParseType: TAurumType;
var dummy: Integer;
begin
  Result := ParseTypeEx(dummy);
end;

function TParser.ParseParamList: TAstParamList;
var
  params: TAstParamList;
  name: string;
  typ: TAurumType;
  p: TAstParam;
  arrLen: Integer;
begin
  params := nil;
  FLastParamListVarArgs := False;
  while not Check(tkRParen) and not Check(tkEOF) do
  begin
    if Check(tkEllipsis) then
    begin
      // varargs marker
      Accept(tkEllipsis);
      FLastParamListVarArgs := True;
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
    typ := ParseTypeEx(arrLen);
    if arrLen <> 0 then
      FDiag.Error('array parameter types not yet supported', FCurTok.Span);
    p.Name := name; p.ParamType := typ; p.Span := FCurTok.Span;
    SetLength(params, Length(params) + 1);
    params[High(params)] := p;
    if Accept(tkComma) then Continue else Break;
  end;
  Result := params;
end;

end.
