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
    function ParseFuncDecl: TAstFuncDecl;
    function ParseConDecl: TAstConDecl;

    function ParseBlock: TAstBlock;
    function ParseStmt: TAstStmt;
    function ParseVarLetCoDecl: TAstVarDecl;
    function ParseAssignStmtOrExprStmt: TAstStmt;

    // Expressions (Präzedenz): Or -> And -> Cmp -> Add -> Mul -> Unary -> Primary
    function ParseExpr: TAstExpr;
    function ParseOrExpr: TAstExpr;
    function ParseAndExpr: TAstExpr;
    function ParseCmpExpr: TAstExpr;
    function ParseAddExpr: TAstExpr;
    function ParseMulExpr: TAstExpr;
    function ParseUnaryExpr: TAstExpr;
    function ParsePrimary: TAstExpr;
    function ParseCallOrIdent: TAstExpr;

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
  if Check(tkFn) then
  begin
    Result := ParseFuncDecl;
    Exit;
  end;
  if Check(tkCon) then
  begin
    Result := ParseConDecl;
    Exit;
  end;
  // unexpected top-level
  FDiag.Error('unexpected top-level declaration', FCurTok.Span);
  Result := nil;
end;

function TParser.ParseFuncDecl: TAstFuncDecl;
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
  Result := TAstFuncDecl.Create(name, params, retType, body, FCurTok.Span);
end;

function TParser.ParseConDecl: TAstConDecl;
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
    thenStmt := ParseStmt;
    elseStmt := nil;
    if Accept(tkElse) then
      elseStmt := ParseStmt;
    Exit(TAstIf.Create(cond, thenStmt, elseStmt, cond.Span));
  end;

  if Check(tkWhile) then
  begin
    Advance;
    Expect(tkLParen);
    cond := ParseExpr;
    Expect(tkRParen);
    bodyStmt := ParseStmt;
    Exit(TAstWhile.Create(cond, bodyStmt, cond.Span));
  end;

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
  declType := ParseType;

  Expect(tkAssign);
  initExpr := ParseExpr;
  Expect(tkSemicolon);

  Result := TAstVarDecl.Create(storage, name, declType, initExpr, initExpr.Span);
end;

function TParser.ParseAssignStmtOrExprStmt: TAstStmt;
var
  expr: TAstExpr;
  name: string;
  valExpr: TAstExpr;
begin
  expr := ParseExpr;
  // Assignment pattern: ident := expr ;
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
begin
  if Check(tkNot) or Check(tkMinus) then
  begin
    op := FCurTok.Kind; Advance;
    operand := ParseUnaryExpr;
    Result := TAstUnaryOp.Create(op, operand, operand.Span);
  end
  else
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

  if Check(tkStrLit) then
  begin
    s := FCurTok.Value;
    span := FCurTok.Span;
    Advance;
    Exit(TAstStrLit.Create(s, span));
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
    Result := TAstCall.Create(name, args, span);
  end
  else
    Result := TAstIdent.Create(name, span);
end;

function TParser.ParseType: TAurumType;
var s: string;
begin
  if Check(tkIdent) then
  begin
    s := FCurTok.Value;
    Advance;
    Result := StrToAurumType(s);
  end
  else
  begin
    FDiag.Error('expected type name', FCurTok.Span);
    Result := atUnresolved;
  end;
end;

function TParser.ParseParamList: TAstParamList;
var
  params: TAstParamList;
  name: string;
  typ: TAurumType;
  p: TAstParam;
begin
  params := nil;
  while not Check(tkRParen) and not Check(tkEOF) do
  begin
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
