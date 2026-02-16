{$mode objfpc}{$H+}
unit ast;

interface

uses
  SysUtils, Classes,
  diag, lexer;

type
  { --- Aurum-Typsystem --- }

  TAurumType = (
    atUnresolved,
    // signed integer widths
    atInt8, atInt16, atInt32, atInt64,
    // unsigned integer widths
    atUInt8, atUInt16, atUInt32, atUInt64,
    // char type
    atChar,
    // floating-point types
    atF32, atF64,
    atBool,
    atVoid,
    atPChar,
    // array types (placeholder)
    atArray
  );

  { --- Speicherklassen --- }

  TStorageKlass = (skVar, skLet, skCo, skCon);

  { --- Knotenarten (für schnellen Typcheck ohne 'is') --- }

  TNodeKind = (
     // Ausdrücke
     nkIntLit, nkFloatLit, nkStrLit, nkCharLit, nkBoolLit, nkIdent,
     nkBinOp, nkUnaryOp, nkCall, nkArrayLit, nkArrayIndex,
     // Statements
     nkVarDecl, nkAssign, nkArrayAssign, nkIf, nkWhile, nkReturn, nkBreak, nkSwitch,
     nkBlock, nkExprStmt,
    // Top-Level
    nkFuncDecl, nkConDecl, nkProgram
  );

  { --- Vorwärtsdeklarationen --- }

  TAstNode = class;
  TAstExpr = class;
  TAstStmt = class;

  { --- Knotenlisten --- }

  TAstNodeList = array of TAstNode;
  TAstExprList = array of TAstExpr;
  TAstStmtList = array of TAstStmt;

  { --- Basisklasse --- }

  TAstNode = class
  private
    FKind: TNodeKind;
    FSpan: TSourceSpan;
  public
    constructor Create(aKind: TNodeKind; aSpan: TSourceSpan);
    destructor Destroy; override;
    property Kind: TNodeKind read FKind;
    property Span: TSourceSpan read FSpan write FSpan;
  end;

  // ================================================================
  // Ausdrücke
  // ================================================================

  TAstExpr = class(TAstNode)
  private
    FResolvedType: TAurumType;
  public
    constructor Create(aKind: TNodeKind; aSpan: TSourceSpan);
    property ResolvedType: TAurumType read FResolvedType write FResolvedType;
  end;

  { Ganzzahl-Literal: 42 }
  TAstIntLit = class(TAstExpr)
  private
    FValue: Int64;
  public
    constructor Create(aValue: Int64; aSpan: TSourceSpan);
    property Value: Int64 read FValue;
  end;

  { Float-Literal: 3.14 }
  TAstFloatLit = class(TAstExpr)
  private
    FValue: string;
  public
    constructor Create(const aValue: string; aSpan: TSourceSpan);
    property Value: string read FValue;
  end;

  { String-Literal: "hello\n" }
  TAstStrLit = class(TAstExpr)
  private
    FValue: string;
  public
    constructor Create(const aValue: string; aSpan: TSourceSpan);
    property Value: string read FValue;
  end;

  { Char-Literal: 'x' }
  TAstCharLit = class(TAstExpr)
  private
    FValue: string;
  public
    constructor Create(const aValue: string; aSpan: TSourceSpan);
    property Value: string read FValue;
  end;

  { Bool-Literal: true / false }
  TAstBoolLit = class(TAstExpr)
  private
    FValue: Boolean;
  public
    constructor Create(aValue: Boolean; aSpan: TSourceSpan);
    property Value: Boolean read FValue;
  end;

  { Identifier-Referenz: x, counter }
  TAstIdent = class(TAstExpr)
  private
    FName: string;
  public
    constructor Create(const aName: string; aSpan: TSourceSpan);
    property Name: string read FName;
  end;

  { Binärer Operator: a + b, x == y }
  TAstBinOp = class(TAstExpr)
  private
    FOp: TTokenKind;
    FLeft: TAstExpr;
    FRight: TAstExpr;
  public
    constructor Create(aOp: TTokenKind; aLeft, aRight: TAstExpr;
      aSpan: TSourceSpan);
    destructor Destroy; override;
    property Op: TTokenKind read FOp;
    property Left: TAstExpr read FLeft;
    property Right: TAstExpr read FRight;
  end;

  { Unärer Operator: -x, !flag }
  TAstUnaryOp = class(TAstExpr)
  private
    FOp: TTokenKind;
    FOperand: TAstExpr;
  public
    constructor Create(aOp: TTokenKind; aOperand: TAstExpr;
      aSpan: TSourceSpan);
    destructor Destroy; override;
    property Op: TTokenKind read FOp;
    property Operand: TAstExpr read FOperand;
  end;

  { Funktionsaufruf: print_str("hi"), foo(1, 2) }
  TAstCall = class(TAstExpr)
  private
    FName: string;
    FArgs: TAstExprList;
  public
    constructor Create(const aName: string; const aArgs: TAstExprList;
      aSpan: TSourceSpan);
    destructor Destroy; override;
    property Name: string read FName;
    property Args: TAstExprList read FArgs;
  end;

   { Array-Literal: [1, 2, 3] }
   TAstArrayLit = class(TAstExpr)
   private
     FItems: TAstExprList;
   public
     constructor Create(const aItems: TAstExprList; aSpan: TSourceSpan);
     destructor Destroy; override;
     property Items: TAstExprList read FItems;
   end;

   { Array-Index: arr[i] }
   TAstArrayIndex = class(TAstExpr)
   protected
     FArray: TAstExpr;
     FIndex: TAstExpr;
   public
     constructor Create(aArray: TAstExpr; aIndex: TAstExpr; aSpan: TSourceSpan);
     destructor Destroy; override;
     property ArrayExpr: TAstExpr read FArray;
     property Index: TAstExpr read FIndex;
     // Transfer ownership for parser (used in array assignment)
     procedure TransferOwnership(out AArray: TAstExpr; out AIndex: TAstExpr);
   end;

  // ================================================================
  // Statements
  // ================================================================

  TAstStmt = class(TAstNode)
  public
    constructor Create(aKind: TNodeKind; aSpan: TSourceSpan);
  end;

  { Variable/Let/Co-Deklaration: var x: int64 := 42; }
  TAstVarDecl = class(TAstStmt)
  private
    FStorage: TStorageKlass;
    FName: string;
    FDeclType: TAurumType;
    FInitExpr: TAstExpr;
  public
    constructor Create(aStorage: TStorageKlass; const aName: string;
      aDeclType: TAurumType; aInitExpr: TAstExpr; aSpan: TSourceSpan);
    destructor Destroy; override;
    property Storage: TStorageKlass read FStorage;
    property Name: string read FName;
    property DeclType: TAurumType read FDeclType;
    property InitExpr: TAstExpr read FInitExpr;
  end;

   { Zuweisung: x := expr; }
   TAstAssign = class(TAstStmt)
   private
     FName: string;
     FValue: TAstExpr;
   public
     constructor Create(const aName: string; aValue: TAstExpr;
       aSpan: TSourceSpan);
     destructor Destroy; override;
     property Name: string read FName;
     property Value: TAstExpr read FValue;
   end;

   { Array-Zuweisung: arr[i] := expr; }
   TAstArrayAssign = class(TAstStmt)
   private
     FArray: TAstExpr;
     FIndex: TAstExpr;
     FValue: TAstExpr;
   public
     constructor Create(aArray: TAstExpr; aIndex: TAstExpr; aValue: TAstExpr;
       aSpan: TSourceSpan);
     destructor Destroy; override;
     property ArrayExpr: TAstExpr read FArray;
     property Index: TAstExpr read FIndex;
     property Value: TAstExpr read FValue;
   end;

  { If-Statement: if (cond) thenStmt [else elseStmt] }
  TAstIf = class(TAstStmt)
  private
    FCond: TAstExpr;
    FThenBranch: TAstStmt;
    FElseBranch: TAstStmt; // kann nil sein
  public
    constructor Create(aCond: TAstExpr; aThen: TAstStmt;
      aElse: TAstStmt; aSpan: TSourceSpan);
    destructor Destroy; override;
    property Cond: TAstExpr read FCond;
    property ThenBranch: TAstStmt read FThenBranch;
    property ElseBranch: TAstStmt read FElseBranch;
  end;

  { Break-Statement: break; }
  TAstBreak = class(TAstStmt)
  public
    constructor Create(aSpan: TSourceSpan);
  end;

  { Switch-Statement: switch (expr) { case val: stmt ... default: stmt }
    Cases are modelled as array of (ValueExpr, BodyStmt) }
  TAstCase = class
  public
    Value: TAstExpr;
    Body: TAstStmt;
    constructor Create(aValue: TAstExpr; aBody: TAstStmt);
    destructor Destroy; override;
  end;
  TAstCaseList = array of TAstCase;

  TAstSwitch = class(TAstStmt)
  private
    FExpr: TAstExpr;
    FCases: TAstCaseList;
    FDefault: TAstStmt;
  public
    constructor Create(aExpr: TAstExpr; const aCases: TAstCaseList; aDefault: TAstStmt; aSpan: TSourceSpan);
    destructor Destroy; override;
    property Expr: TAstExpr read FExpr;
    property Cases: TAstCaseList read FCases;
    property Default: TAstStmt read FDefault;
  end;

  { While-Statement: while (cond) body }
  TAstWhile = class(TAstStmt)
  private
    FCond: TAstExpr;
    FBody: TAstStmt;
  public
    constructor Create(aCond: TAstExpr; aBody: TAstStmt;
      aSpan: TSourceSpan);
    destructor Destroy; override;
    property Cond: TAstExpr read FCond;
    property Body: TAstStmt read FBody;
  end;

  { Return-Statement: return [expr]; }
  TAstReturn = class(TAstStmt)
  private
    FValue: TAstExpr; // kann nil sein (void-Funktionen)
  public
    constructor Create(aValue: TAstExpr; aSpan: TSourceSpan);
    destructor Destroy; override;
    property Value: TAstExpr read FValue;
  end;

  // Block: { stmt1; stmt2; ... }
  TAstBlock = class(TAstStmt)
  private
    FStmts: TAstStmtList;
  public
    constructor Create(const aStmts: TAstStmtList; aSpan: TSourceSpan);
    destructor Destroy; override;
    property Stmts: TAstStmtList read FStmts;
  end;

  { Expression-Statement: expr; }
  TAstExprStmt = class(TAstStmt)
  private
    FExpr: TAstExpr;
  public
    constructor Create(aExpr: TAstExpr; aSpan: TSourceSpan);
    destructor Destroy; override;
    property Expr: TAstExpr read FExpr;
  end;

  // ================================================================
  // Top-Level Deklarationen
  // ================================================================

  { Funktionsparameter }
  TAstParam = record
    Name: string;
    ParamType: TAurumType;
    Span: TSourceSpan;
  end;
  TAstParamList = array of TAstParam;

  // Funktionsdeklaration: fn name(params): retType { body }
  TAstFuncDecl = class(TAstNode)
  private
    FName: string;
    FParams: TAstParamList;
    FReturnType: TAurumType;
    FBody: TAstBlock;
  public
    constructor Create(const aName: string; const aParams: TAstParamList;
      aReturnType: TAurumType; aBody: TAstBlock; aSpan: TSourceSpan);
    destructor Destroy; override;
    property Name: string read FName;
    property Params: TAstParamList read FParams;
    property ReturnType: TAurumType read FReturnType;
    property Body: TAstBlock read FBody;
  end;

  { Con-Deklaration (Top-Level): con NAME: type := constExpr; }
  TAstConDecl = class(TAstNode)
  private
    FName: string;
    FDeclType: TAurumType;
    FInitExpr: TAstExpr;
  public
    constructor Create(const aName: string; aDeclType: TAurumType;
      aInitExpr: TAstExpr; aSpan: TSourceSpan);
    destructor Destroy; override;
    property Name: string read FName;
    property DeclType: TAurumType read FDeclType;
    property InitExpr: TAstExpr read FInitExpr;
  end;

  { Programm (Wurzelknoten): Liste von Top-Level Deklarationen }
  TAstProgram = class(TAstNode)
  private
    FDecls: TAstNodeList;
  public
    constructor Create(const aDecls: TAstNodeList; aSpan: TSourceSpan);
    destructor Destroy; override;
    property Decls: TAstNodeList read FDecls;
  end;

{ --- Hilfsfunktionen --- }

function AurumTypeToStr(t: TAurumType): string;
function StrToAurumType(const s: string): TAurumType;
function StorageKlassToStr(sk: TStorageKlass): string;
function NodeKindToStr(nk: TNodeKind): string;

implementation

{ --- Hilfsfunktionen --- }

function AurumTypeToStr(t: TAurumType): string;
begin
  case t of
    atUnresolved: Result := '<unresolved>';
    atInt8:       Result := 'int8';
    atInt16:      Result := 'int16';
    atInt32:      Result := 'int32';
    atInt64:      Result := 'int64';
    atUInt8:      Result := 'uint8';
    atUInt16:     Result := 'uint16';
    atUInt32:     Result := 'uint32';
    atUInt64:     Result := 'uint64';
    atChar:       Result := 'char';
    atF32:        Result := 'f32';
    atF64:        Result := 'f64';
    atBool:       Result := 'bool';
    atVoid:       Result := 'void';
    atPChar:      Result := 'pchar';
    atArray:      Result := 'array';
  else
    Result := '<unknown>';
  end;
end;

function StrToAurumType(const s: string): TAurumType;
begin
  case s of
    'int8':  Result := atInt8;
    'int16': Result := atInt16;
    'int32': Result := atInt32;
    'int64': Result := atInt64;
    'int':   Result := atInt64; // alias
    'uint8': Result := atUInt8;
    'uint16': Result := atUInt16;
    'uint32': Result := atUInt32;
    'uint64': Result := atUInt64;
    'char':  Result := atChar;
    'f32':   Result := atF32;
    'f64':   Result := atF64;
    'bool':  Result := atBool;
    'void':  Result := atVoid;
    'pchar': Result := atPChar;
    'string': Result := atPChar; // map string to pchar for now
    'array': Result := atArray;
  else
    Result := atUnresolved;
  end;
end;

function StorageKlassToStr(sk: TStorageKlass): string;
begin
  case sk of
    skVar: Result := 'var';
    skLet: Result := 'let';
    skCo:  Result := 'co';
    skCon: Result := 'con';
  end;
end;

function NodeKindToStr(nk: TNodeKind): string;
begin
  case nk of
    nkIntLit:    Result := 'IntLit';
    nkFloatLit:  Result := 'FloatLit';
    nkStrLit:    Result := 'StrLit';
    nkCharLit:   Result := 'CharLit';
    nkBoolLit:   Result := 'BoolLit';
    nkIdent:     Result := 'Ident';
    nkBinOp:     Result := 'BinOp';
    nkUnaryOp:   Result := 'UnaryOp';
    nkCall:      Result := 'Call';
    nkArrayLit:  Result := 'ArrayLit';
    nkArrayIndex: Result := 'ArrayIndex';
    nkVarDecl:   Result := 'VarDecl';
    nkAssign:    Result := 'Assign';
    nkArrayAssign: Result := 'ArrayAssign';
    nkIf:        Result := 'If';
    nkWhile:     Result := 'While';
    nkReturn:    Result := 'Return';
    nkBreak:     Result := 'Break';
    nkSwitch:    Result := 'Switch';
    nkBlock:     Result := 'Block';
    nkExprStmt:  Result := 'ExprStmt';
    nkFuncDecl:  Result := 'FuncDecl';
    nkConDecl:   Result := 'ConDecl';
    nkProgram:   Result := 'Program';
  end;
end;

// ================================================================
// TAstNode
// ================================================================

constructor TAstNode.Create(aKind: TNodeKind; aSpan: TSourceSpan);
begin
  inherited Create;
  FKind := aKind;
  FSpan := aSpan;
end;

destructor TAstNode.Destroy;
begin
  inherited Destroy;
end;

// ================================================================
// TAstExpr
// ================================================================

constructor TAstExpr.Create(aKind: TNodeKind; aSpan: TSourceSpan);
begin
  inherited Create(aKind, aSpan);
  FResolvedType := atUnresolved;
end;

// ================================================================
// TAstIntLit
// ================================================================

constructor TAstIntLit.Create(aValue: Int64; aSpan: TSourceSpan);
begin
  inherited Create(nkIntLit, aSpan);
  FValue := aValue;
  FResolvedType := atInt64;
end;

// ================================================================
// TAstStrLit
// ================================================================

constructor TAstStrLit.Create(const aValue: string; aSpan: TSourceSpan);
begin
  inherited Create(nkStrLit, aSpan);
  FValue := aValue;
  FResolvedType := atPChar;
end;

// ================================================================
// TAstFloatLit
// ================================================================

constructor TAstFloatLit.Create(const aValue: string; aSpan: TSourceSpan);
begin
  inherited Create(nkFloatLit, aSpan);
  FValue := aValue;
  FResolvedType := atF64; // default zu double precision
end;

// ================================================================
// TAstCharLit
// ================================================================

constructor TAstCharLit.Create(const aValue: string; aSpan: TSourceSpan);
begin
  inherited Create(nkCharLit, aSpan);
  FValue := aValue;
  FResolvedType := atChar;
end;

// ================================================================
// TAstBoolLit
// ================================================================

constructor TAstBoolLit.Create(aValue: Boolean; aSpan: TSourceSpan);
begin
  inherited Create(nkBoolLit, aSpan);
  FValue := aValue;
  FResolvedType := atBool;
end;

// ================================================================
// TAstIdent
// ================================================================

constructor TAstIdent.Create(const aName: string; aSpan: TSourceSpan);
begin
  inherited Create(nkIdent, aSpan);
  FName := aName;
end;

// ================================================================
// TAstBinOp
// ================================================================

constructor TAstBinOp.Create(aOp: TTokenKind; aLeft, aRight: TAstExpr;
  aSpan: TSourceSpan);
begin
  inherited Create(nkBinOp, aSpan);
  FOp := aOp;
  FLeft := aLeft;
  FRight := aRight;
end;

destructor TAstBinOp.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  inherited Destroy;
end;

// ================================================================
// TAstUnaryOp
// ================================================================

constructor TAstUnaryOp.Create(aOp: TTokenKind; aOperand: TAstExpr;
  aSpan: TSourceSpan);
begin
  inherited Create(nkUnaryOp, aSpan);
  FOp := aOp;
  FOperand := aOperand;
end;

destructor TAstUnaryOp.Destroy;
begin
  FOperand.Free;
  inherited Destroy;
end;

// ================================================================
// TAstCall
// ================================================================

constructor TAstCall.Create(const aName: string;
  const aArgs: TAstExprList; aSpan: TSourceSpan);
begin
  inherited Create(nkCall, aSpan);
  FName := aName;
  FArgs := aArgs;
end;

destructor TAstCall.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FArgs) do
    FArgs[i].Free;
  FArgs := nil;
  inherited Destroy;
end;

// ================================================================
// TAstStmt
// ================================================================

constructor TAstStmt.Create(aKind: TNodeKind; aSpan: TSourceSpan);
begin
  inherited Create(aKind, aSpan);
end;

// ================================================================
// TAstArrayLit
// ================================================================

constructor TAstArrayLit.Create(const aItems: TAstExprList; aSpan: TSourceSpan);
begin
  inherited Create(nkArrayLit, aSpan);
  FItems := aItems;
end;

destructor TAstArrayLit.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FItems) do
    FItems[i].Free;
  FItems := nil;
  inherited Destroy;
end;

// ================================================================
// TAstVarDecl
// ================================================================

constructor TAstVarDecl.Create(aStorage: TStorageKlass;
  const aName: string; aDeclType: TAurumType; aInitExpr: TAstExpr;
  aSpan: TSourceSpan);
begin
  inherited Create(nkVarDecl, aSpan);
  FStorage := aStorage;
  FName := aName;
  FDeclType := aDeclType;
  FInitExpr := aInitExpr;
end;

destructor TAstVarDecl.Destroy;
begin
  FInitExpr.Free;
  inherited Destroy;
end;

// ================================================================
// TAstAssign
// ================================================================

constructor TAstAssign.Create(const aName: string; aValue: TAstExpr;
  aSpan: TSourceSpan);
begin
  inherited Create(nkAssign, aSpan);
  FName := aName;
  FValue := aValue;
end;

destructor TAstAssign.Destroy;
begin
  FValue.Free;
  inherited Destroy;
end;

// ================================================================
// TAstIf
// ================================================================

constructor TAstIf.Create(aCond: TAstExpr; aThen: TAstStmt;
  aElse: TAstStmt; aSpan: TSourceSpan);
begin
  inherited Create(nkIf, aSpan);
  FCond := aCond;
  FThenBranch := aThen;
  FElseBranch := aElse;
end;

destructor TAstIf.Destroy;
begin
  FCond.Free;
  FThenBranch.Free;
  FElseBranch.Free; // nil.Free ist sicher in FPC
  inherited Destroy;
end;

{ TAstBreak }
constructor TAstBreak.Create(aSpan: TSourceSpan);
begin
  inherited Create(nkBreak, aSpan);
end;

{ TAstSwitch }
constructor TAstSwitch.Create(aExpr: TAstExpr; const aCases: TAstCaseList; aDefault: TAstStmt; aSpan: TSourceSpan);
begin
  inherited Create(nkSwitch, aSpan);
  FExpr := aExpr;
  FCases := aCases;
  FDefault := aDefault;
end;

destructor TAstSwitch.Destroy;
var i: Integer;
begin
  FExpr.Free;
  for i := 0 to High(FCases) do
  begin
    FCases[i].Free;
  end;
  SetLength(FCases, 0);
  FDefault.Free;
  inherited Destroy;
end;

{ TAstCase }
constructor TAstCase.Create(aValue: TAstExpr; aBody: TAstStmt);
begin
  inherited Create;
  Value := aValue;
  Body := aBody;
end;

destructor TAstCase.Destroy;
begin
  Value.Free;
  Body.Free;
  inherited Destroy;
end;

// ================================================================
// TAstWhile
// ================================================================

constructor TAstWhile.Create(aCond: TAstExpr; aBody: TAstStmt;
  aSpan: TSourceSpan);
begin
  inherited Create(nkWhile, aSpan);
  FCond := aCond;
  FBody := aBody;
end;

destructor TAstWhile.Destroy;
begin
  FCond.Free;
  FBody.Free;
  inherited Destroy;
end;

// ================================================================
// TAstReturn
// ================================================================

constructor TAstReturn.Create(aValue: TAstExpr; aSpan: TSourceSpan);
begin
  inherited Create(nkReturn, aSpan);
  FValue := aValue;
end;

destructor TAstReturn.Destroy;
begin
  FValue.Free;
  inherited Destroy;
end;

// ================================================================
// TAstBlock
// ================================================================

constructor TAstBlock.Create(const aStmts: TAstStmtList;
  aSpan: TSourceSpan);
begin
  inherited Create(nkBlock, aSpan);
  FStmts := aStmts;
end;

destructor TAstBlock.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FStmts) do
    FStmts[i].Free;
  FStmts := nil;
  inherited Destroy;
end;

// ================================================================
// TAstExprStmt
// ================================================================

constructor TAstExprStmt.Create(aExpr: TAstExpr; aSpan: TSourceSpan);
begin
  inherited Create(nkExprStmt, aSpan);
  FExpr := aExpr;
end;

destructor TAstExprStmt.Destroy;
begin
  FExpr.Free;
  inherited Destroy;
end;

// ================================================================
// TAstFuncDecl
// ================================================================

constructor TAstFuncDecl.Create(const aName: string;
  const aParams: TAstParamList; aReturnType: TAurumType;
  aBody: TAstBlock; aSpan: TSourceSpan);
begin
  inherited Create(nkFuncDecl, aSpan);
  FName := aName;
  FParams := aParams;
  FReturnType := aReturnType;
  FBody := aBody;
end;

destructor TAstFuncDecl.Destroy;
begin
  FBody.Free;
  FParams := nil;
  inherited Destroy;
end;

// ================================================================
// TAstConDecl
// ================================================================

constructor TAstConDecl.Create(const aName: string;
  aDeclType: TAurumType; aInitExpr: TAstExpr; aSpan: TSourceSpan);
begin
  inherited Create(nkConDecl, aSpan);
  FName := aName;
  FDeclType := aDeclType;
  FInitExpr := aInitExpr;
end;

destructor TAstConDecl.Destroy;
begin
  FInitExpr.Free;
  inherited Destroy;
end;

// ================================================================
// TAstProgram
// ================================================================

constructor TAstProgram.Create(const aDecls: TAstNodeList;
  aSpan: TSourceSpan);
begin
  inherited Create(nkProgram, aSpan);
  FDecls := aDecls;
end;

destructor TAstProgram.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FDecls) do
    FDecls[i].Free;
  FDecls := nil;
  inherited Destroy;
end;

constructor TAstArrayAssign.Create(aArray: TAstExpr; aIndex: TAstExpr; aValue: TAstExpr;
  aSpan: TSourceSpan);
begin
  inherited Create(nkArrayAssign, aSpan);
  FArray := aArray;
  FIndex := aIndex;
  FValue := aValue;
end;

destructor TAstArrayAssign.Destroy;
begin
  FArray.Free;
  FIndex.Free;
  FValue.Free;
  inherited Destroy;
end;

constructor TAstArrayIndex.Create(aArray: TAstExpr; aIndex: TAstExpr; aSpan: TSourceSpan);
begin
  inherited Create(nkArrayIndex, aSpan);
  FArray := aArray;
  FIndex := aIndex;
end;

destructor TAstArrayIndex.Destroy;
begin
  FArray.Free;
  FIndex.Free;
  inherited Destroy;
end;

procedure TAstArrayIndex.TransferOwnership(out AArray: TAstExpr; out AIndex: TAstExpr);
begin
  AArray := FArray;
  AIndex := FIndex;
  FArray := nil; // prevent double-free
  FIndex := nil; // prevent double-free
end;

end.
