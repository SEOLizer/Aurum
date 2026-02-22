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
    // platform-size integers
    atISize, atUSize,
    // floating-point
    atF32, atF64,
    // char
    atChar,
    atBool,
    atVoid,
    atPChar
  );

  { --- Speicherklassen --- }

  TStorageKlass = (skVar, skLet, skCo, skCon);

  { --- Knotenarten (für schnellen Typcheck ohne 'is') --- }

  TNodeKind = (
    // Ausdrücke
    nkIntLit, nkStrLit, nkBoolLit, nkCharLit, nkIdent,
    nkBinOp, nkUnaryOp, nkCall, nkArrayLit, nkStructLit,
    nkFieldAccess, nkIndexAccess,
    // Statements
    nkVarDecl, nkAssign, nkFieldAssign, nkIndexAssign,
    nkIf, nkWhile, nkFor, nkRepeatUntil,
    nkReturn, nkBreak, nkSwitch,
    nkBlock, nkExprStmt,
    // Top-Level
    nkFuncDecl, nkConDecl, nkTypeDecl, nkStructDecl,
    nkUnitDecl, nkImportDecl,
    nkProgram
  );

  { --- Vorwärtsdeklarationen --- }

  TAstNode = class;
  TAstExpr = class;
  TAstStmt = class;
  TAstStructDecl = class;

  { --- Knotenlisten --- }

  TAstNodeList = array of TAstNode;
  TAstExprList = array of TAstExpr;
  TAstStmtList = array of TAstStmt;
  TIntArray = array of Integer;

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

  { String-Literal: "hello\n" }
  TAstStrLit = class(TAstExpr)
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
    procedure SetName(const aName: string);
    procedure SetArgs(const aArgs: TAstExprList);
    procedure ReplaceArgs(const aArgs: TAstExprList); // replaces without freeing old args
    property Name: string read FName;
    property Args: TAstExprList read FArgs;
  end;

  { Array-Literal: [expr, expr, ...] }
  TAstArrayLit = class(TAstExpr)
  private
    FItems: TAstExprList;
  public
    constructor Create(const aItems: TAstExprList; aSpan: TSourceSpan);
    destructor Destroy; override;
    property Items: TAstExprList read FItems;
  end;

  { Char-Literal: 'A' }
  TAstCharLit = class(TAstExpr)
  private
    FValue: Char;
  public
    constructor Create(aValue: Char; aSpan: TSourceSpan);
    property Value: Char read FValue;
  end;

  { Feldzugriff: expr.field }
  TAstFieldAccess = class(TAstExpr)
  private
    FObj: TAstExpr;
    FField: string;
    FFieldOffset: Integer; // -1 if unknown
    FOwnerName: string; // owner struct name if known
  public
    constructor Create(aObj: TAstExpr; const aField: string; aSpan: TSourceSpan);
    destructor Destroy; override;
    procedure SetFieldOffset(aOffset: Integer);
    procedure SetOwnerName(const aName: string);
    function DetachObj: TAstExpr; // transfer ownership of the Obj out of this node
    property Obj: TAstExpr read FObj;
    property Field: string read FField;
    property FieldOffset: Integer read FFieldOffset;
    property OwnerName: string read FOwnerName;
  end;


  { Indexzugriff: expr[index] }
  TAstIndexAccess = class(TAstExpr)
  private
    FObj: TAstExpr;
    FIndex: TAstExpr;
  public
    constructor Create(aObj: TAstExpr; aIndex: TAstExpr; aSpan: TSourceSpan);
    destructor Destroy; override;
    property Obj: TAstExpr read FObj;
    property Index: TAstExpr read FIndex;
  end;

  { Struct-Literal Feld-Initialisierer: name: expr }
  TStructFieldInit = record
    Name: string;
    Value: TAstExpr;
  end;
  TStructFieldInitList = array of TStructFieldInit;

  { Struct-Literal: TypeName { field1: val1, field2: val2 } }
  TAstStructLit = class(TAstExpr)
  private
    FTypeName: string;
    FFields: TStructFieldInitList;
    FStructDecl: TAstStructDecl; // set by sema, nil initially
  public
    constructor Create(const aTypeName: string; const aFields: TStructFieldInitList; aSpan: TSourceSpan);
    destructor Destroy; override;
    procedure SetStructDecl(aDecl: TAstStructDecl);
    property TypeName: string read FTypeName;
    property Fields: TStructFieldInitList read FFields;
    property StructDecl: TAstStructDecl read FStructDecl;
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
    FDeclType: TAurumType; // element type for arrays (primitive) or atUnresolved for named types
    FDeclTypeName: string; // if named type (struct), store the name here
    FArrayLen: Integer;    // 0 = not array, >0 = static length, -1 = dynamic array ([]) 
    FInitExpr: TAstExpr;
  public
    constructor Create(aStorage: TStorageKlass; const aName: string;
      aDeclType: TAurumType; const aDeclTypeName: string; aArrayLen: Integer; aInitExpr: TAstExpr; aSpan: TSourceSpan);
    destructor Destroy; override;
    property Storage: TStorageKlass read FStorage;
    property Name: string read FName;
    property DeclType: TAurumType read FDeclType;
    property DeclTypeName: string read FDeclTypeName;
    property ArrayLen: Integer read FArrayLen;
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

  { Feld-Zuweisung: obj.field := value }
  TAstFieldAssign = class(TAstStmt)
  private
    FTarget: TAstFieldAccess; // the LHS field-access node
    FValue: TAstExpr;
  public
    constructor Create(aTarget: TAstFieldAccess; aValue: TAstExpr; aSpan: TSourceSpan);
    destructor Destroy; override;
    property Target: TAstFieldAccess read FTarget;
    property Value: TAstExpr read FValue;
  end;

  { Index-Zuweisung: arr[idx] := value }
  TAstIndexAssign = class(TAstStmt)
  private
    FTarget: TAstIndexAccess; // the LHS index-access node
    FValue: TAstExpr;
  public
    constructor Create(aTarget: TAstIndexAccess; aValue: TAstExpr; aSpan: TSourceSpan);
    destructor Destroy; override;
    property Target: TAstIndexAccess read FTarget;
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

  { For-Statement: for i := start to/downto end do stmt }
  TAstFor = class(TAstStmt)
  private
    FVarName: string;
    FStartExpr: TAstExpr;
    FEndExpr: TAstExpr;
    FIsDownto: Boolean;
    FBody: TAstStmt;
  public
    constructor Create(const aVarName: string; aStart, aEnd: TAstExpr;
      aDownto: Boolean; aBody: TAstStmt; aSpan: TSourceSpan);
    destructor Destroy; override;
    property VarName: string read FVarName;
    property StartExpr: TAstExpr read FStartExpr;
    property EndExpr: TAstExpr read FEndExpr;
    property IsDownto: Boolean read FIsDownto;
    property Body: TAstStmt read FBody;
  end;

  { Repeat-Until: repeat { body } until (cond); }
  TAstRepeatUntil = class(TAstStmt)
  private
    FBody: TAstStmt;
    FCond: TAstExpr;
  public
    constructor Create(aBody: TAstStmt; aCond: TAstExpr; aSpan: TSourceSpan);
    destructor Destroy; override;
    property Body: TAstStmt read FBody;
    property Cond: TAstExpr read FCond;
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

  // Funktionsdeklaration: [pub] fn name(params): retType { body }
  TAstFuncDecl = class(TAstNode)
  private
    FName: string;
    FParams: TAstParamList;
    FReturnType: TAurumType;
    FReturnTypeName: string; // for named return types like Self or struct names
    FBody: TAstBlock;
    FIsPublic: Boolean;
    FIsExtern: Boolean;
    FIsVarArgs: Boolean;
    FIsStatic: Boolean; // true for static methods (no self parameter)
  public
    constructor Create(const aName: string; const aParams: TAstParamList;
      aReturnType: TAurumType; aBody: TAstBlock; aSpan: TSourceSpan; aIsPublic: Boolean = False);
    destructor Destroy; override;
    property Name: string read FName;
    property Params: TAstParamList read FParams;
    property ReturnType: TAurumType read FReturnType write FReturnType;
    property ReturnTypeName: string read FReturnTypeName write FReturnTypeName;
    property Body: TAstBlock read FBody write FBody;
    property IsPublic: Boolean read FIsPublic;
    property IsExtern: Boolean read FIsExtern write FIsExtern;
    property IsVarArgs: Boolean read FIsVarArgs write FIsVarArgs;
    property IsStatic: Boolean read FIsStatic write FIsStatic;
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

  { Type-Deklaration (Top-Level): type Name = Type; }
  TAstTypeDecl = class(TAstNode)
  private
    FName: string;
    FDeclType: TAurumType;
    FIsPublic: Boolean;
  public
    constructor Create(const aName: string; aDeclType: TAurumType;
      aPublic: Boolean; aSpan: TSourceSpan);
    property Name: string read FName;
    property DeclType: TAurumType read FDeclType;
    property IsPublic: Boolean read FIsPublic;
  end;

  { Struct/Type-Deklaration mit Feldern und Methoden }
  TStructField = record
    Name: string;
    FieldType: TAurumType;
    FieldTypeName: string; // if named type
    ArrayLen: Integer; // 0 = scalar, >0 static, -1 dynamic
  end;
  TStructFieldList = array of TStructField;
  TMethodList = array of TAstFuncDecl;

  TAstStructDecl = class(TAstNode)
  private
    FName: string;
    FFields: TStructFieldList;
    FMethods: TMethodList; // reuse TAstFuncDecl for method declarations
    FIsPublic: Boolean;
    // layout info (bytes)
    FFieldOffsets: array of Integer; // offset per field
    FSize: Integer; // total size in bytes
    FAlign: Integer; // alignment in bytes
  public
    constructor Create(const aName: string; const aFields: TStructFieldList;
      const aMethods: TMethodList; aPublic: Boolean; aSpan: TSourceSpan);
    destructor Destroy; override;
    property Name: string read FName;
    property Fields: TStructFieldList read FFields;
    property Methods: TMethodList read FMethods;
    property IsPublic: Boolean read FIsPublic;
    property FieldOffsets: TIntArray read FFieldOffsets;
    property Size: Integer read FSize;
    property Align: Integer read FAlign;
    procedure SetLayout(aSize, aAlign: Integer);
  end;

  { Unit-Deklaration: unit path.to.name; }
  TAstUnitDecl = class(TAstNode)
  private
    FUnitPath: string;
  public
    constructor Create(const aPath: string; aSpan: TSourceSpan);
    property UnitPath: string read FUnitPath;
  end;

  { Import-Item }
  TAstImportItem = record
    Name: string;
    Alias: string; // leer wenn kein 'as'
  end;
  TAstImportItemList = array of TAstImportItem;

  { Import-Deklaration: import path [as alias] [{ items }]; }
  TAstImportDecl = class(TAstNode)
  private
    FUnitPath: string;
    FAlias: string;
    FItems: TAstImportItemList;
  public
    constructor Create(const aPath, aAlias: string;
      const aItems: TAstImportItemList; aSpan: TSourceSpan);
    property UnitPath: string read FUnitPath;
    property Alias: string read FAlias;
    property Items: TAstImportItemList read FItems;
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
    atISize:      Result := 'isize';
    atUSize:      Result := 'usize';
    atF32:        Result := 'f32';
    atF64:        Result := 'f64';
    atChar:       Result := 'char';
    atBool:       Result := 'bool';
    atVoid:       Result := 'void';
    atPChar:      Result := 'pchar';
  else
    Result := '<unknown>';
  end;
end;

function StrToAurumType(const s: string): TAurumType;
begin
  case s of
    'int8':   Result := atInt8;
    'int16':  Result := atInt16;
    'int32':  Result := atInt32;
    'int64':  Result := atInt64;
    'int':    Result := atInt64; // alias
    'uint8':  Result := atUInt8;
    'uint16': Result := atUInt16;
    'uint32': Result := atUInt32;
    'uint64': Result := atUInt64;
    'isize':  Result := atISize;
    'usize':  Result := atUSize;
    'f32':    Result := atF32;
    'f64':    Result := atF64;
    'char':   Result := atChar;
    'bool':   Result := atBool;
    'void':   Result := atVoid;
    'pchar':  Result := atPChar;
    'string': Result := atPChar; // map string to pchar for now
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
    nkIntLit:      Result := 'IntLit';
    nkStrLit:      Result := 'StrLit';
    nkBoolLit:     Result := 'BoolLit';
    nkCharLit:     Result := 'CharLit';
    nkIdent:       Result := 'Ident';
    nkBinOp:       Result := 'BinOp';
    nkUnaryOp:     Result := 'UnaryOp';
    nkCall:        Result := 'Call';
    nkArrayLit:    Result := 'ArrayLit';
    nkStructLit:   Result := 'StructLit';
    nkFieldAccess: Result := 'FieldAccess';
    nkIndexAccess: Result := 'IndexAccess';
    nkVarDecl:     Result := 'VarDecl';
    nkAssign:      Result := 'Assign';
    nkFieldAssign: Result := 'FieldAssign';
    nkIndexAssign: Result := 'IndexAssign';
    nkIf:          Result := 'If';
    nkWhile:       Result := 'While';
    nkFor:         Result := 'For';
    nkRepeatUntil: Result := 'RepeatUntil';
    nkReturn:      Result := 'Return';
    nkBreak:       Result := 'Break';
    nkSwitch:      Result := 'Switch';
    nkBlock:       Result := 'Block';
    nkExprStmt:    Result := 'ExprStmt';
    nkFuncDecl:    Result := 'FuncDecl';
    nkConDecl:     Result := 'ConDecl';
    nkTypeDecl:    Result := 'TypeDecl';
    nkStructDecl:  Result := 'StructDecl';
    nkUnitDecl:     Result := 'UnitDecl';
    nkImportDecl:  Result := 'ImportDecl';
    nkProgram:     Result := 'Program';
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

constructor TAstCall.Create(const aName: string; const aArgs: TAstExprList; aSpan: TSourceSpan);
begin
  inherited Create(nkCall, aSpan);
  FName := aName;
  FArgs := aArgs;
end;

procedure TAstCall.SetName(const aName: string);
begin
  FName := aName;
end;

procedure TAstCall.SetArgs(const aArgs: TAstExprList);
var i: Integer;
begin
  // free previous args
  for i := 0 to High(FArgs) do
    FArgs[i].Free;
  FArgs := aArgs;
end;

procedure TAstCall.ReplaceArgs(const aArgs: TAstExprList);
begin
  // Replace args without freeing old ones (caller is responsible)
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
// TAstStmt
// ================================================================

constructor TAstStmt.Create(aKind: TNodeKind; aSpan: TSourceSpan);
begin
  inherited Create(aKind, aSpan);
end;

// ================================================================
// TAstVarDecl
// ================================================================

constructor TAstVarDecl.Create(aStorage: TStorageKlass;
  const aName: string; aDeclType: TAurumType; const aDeclTypeName: string; aArrayLen: Integer; aInitExpr: TAstExpr;
  aSpan: TSourceSpan);
begin
  inherited Create(nkVarDecl, aSpan);
  FStorage := aStorage;
  FName := aName;
  FDeclType := aDeclType;
  FDeclTypeName := aDeclTypeName;
  FArrayLen := aArrayLen;
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
// TAstCharLit
// ================================================================

constructor TAstCharLit.Create(aValue: Char; aSpan: TSourceSpan);
begin
  inherited Create(nkCharLit, aSpan);
  FValue := aValue;
  FResolvedType := atChar;
end;

// ================================================================
// TAstFieldAccess
// ================================================================

constructor TAstFieldAccess.Create(aObj: TAstExpr; const aField: string;
  aSpan: TSourceSpan);
begin
  inherited Create(nkFieldAccess, aSpan);
  FObj := aObj;
  FField := aField;
  FFieldOffset := -1;
  FOwnerName := '';
end;

procedure TAstFieldAccess.SetFieldOffset(aOffset: Integer);
begin
  FFieldOffset := aOffset;
end;

procedure TAstFieldAccess.SetOwnerName(const aName: string);
begin
  FOwnerName := aName;
end;

function TAstFieldAccess.DetachObj: TAstExpr;
begin
  Result := FObj;
  FObj := nil;
end;

destructor TAstFieldAccess.Destroy;
begin
  if Assigned(FObj) then FObj.Free;
  inherited Destroy;
end;

// ================================================================
// TAstIndexAccess
// ================================================================

constructor TAstIndexAccess.Create(aObj: TAstExpr; aIndex: TAstExpr;
  aSpan: TSourceSpan);
begin
  inherited Create(nkIndexAccess, aSpan);
  FObj := aObj;
  FIndex := aIndex;
end;

destructor TAstIndexAccess.Destroy;
begin
  FObj.Free;
  FIndex.Free;
  inherited Destroy;
end;

// ================================================================
// TAstStructLit
// ================================================================

constructor TAstStructLit.Create(const aTypeName: string; const aFields: TStructFieldInitList; aSpan: TSourceSpan);
begin
  inherited Create(nkStructLit, aSpan);
  FTypeName := aTypeName;
  FFields := aFields;
  FStructDecl := nil;
end;

destructor TAstStructLit.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FFields) do
    if Assigned(FFields[i].Value) then
      FFields[i].Value.Free;
  FFields := nil;
  // FStructDecl is not owned by us
  inherited Destroy;
end;

procedure TAstStructLit.SetStructDecl(aDecl: TAstStructDecl);
begin
  FStructDecl := aDecl;
end;

// ================================================================
// TAstFor
// ================================================================

constructor TAstFor.Create(const aVarName: string; aStart, aEnd: TAstExpr;
  aDownto: Boolean; aBody: TAstStmt; aSpan: TSourceSpan);
begin
  inherited Create(nkFor, aSpan);
  FVarName := aVarName;
  FStartExpr := aStart;
  FEndExpr := aEnd;
  FIsDownto := aDownto;
  FBody := aBody;
end;

destructor TAstFor.Destroy;
begin
  FStartExpr.Free;
  FEndExpr.Free;
  FBody.Free;
  inherited Destroy;
end;

// ================================================================
// TAstRepeatUntil
// ================================================================

constructor TAstRepeatUntil.Create(aBody: TAstStmt; aCond: TAstExpr;
  aSpan: TSourceSpan);
begin
  inherited Create(nkRepeatUntil, aSpan);
  FBody := aBody;
  FCond := aCond;
end;

destructor TAstRepeatUntil.Destroy;
begin
  FBody.Free;
  FCond.Free;
  inherited Destroy;
end;

// ================================================================
// TAstTypeDecl
// ================================================================

constructor TAstTypeDecl.Create(const aName: string; aDeclType: TAurumType;
  aPublic: Boolean; aSpan: TSourceSpan);
begin
  inherited Create(nkTypeDecl, aSpan);
  FName := aName;
  FDeclType := aDeclType;
  FIsPublic := aPublic;
end;

constructor TAstStructDecl.Create(const aName: string; const aFields: TStructFieldList;
  const aMethods: TMethodList; aPublic: Boolean; aSpan: TSourceSpan);
var i: Integer;
begin
  inherited Create(nkStructDecl, aSpan);
  FName := aName;
  FFields := aFields;
  SetLength(FMethods, Length(aMethods));
  for i := 0 to High(aMethods) do
    FMethods[i] := aMethods[i];
  FIsPublic := aPublic;
  // default layout unknown
  FSize := 0;
  FAlign := 0;
  SetLength(FFieldOffsets, Length(FFields));
  for i := 0 to High(FFieldOffsets) do
    FFieldOffsets[i] := -1;
end;


destructor TAstStructDecl.Destroy;
var i: Integer;
begin
  for i := 0 to High(FMethods) do
    if Assigned(FMethods[i]) then
      FMethods[i].Free;
  FMethods := nil;
  SetLength(FFields, 0);
  SetLength(FFieldOffsets, 0);
  inherited Destroy;
end;

procedure TAstStructDecl.SetLayout(aSize, aAlign: Integer);
begin
  FSize := aSize;
  FAlign := aAlign;
end;

constructor TAstFieldAssign.Create(aTarget: TAstFieldAccess; aValue: TAstExpr; aSpan: TSourceSpan);
begin
  inherited Create(nkFieldAssign, aSpan);
  FTarget := aTarget;
  FValue := aValue;
end;


destructor TAstFieldAssign.Destroy;
begin
  if Assigned(FTarget) then FTarget.Free;
  if Assigned(FValue) then FValue.Free;
  inherited Destroy;
end;

// ================================================================
// TAstIndexAssign
// ================================================================

constructor TAstIndexAssign.Create(aTarget: TAstIndexAccess; aValue: TAstExpr; aSpan: TSourceSpan);
begin
  inherited Create(nkIndexAssign, aSpan);
  FTarget := aTarget;
  FValue := aValue;
end;

destructor TAstIndexAssign.Destroy;
begin
  if Assigned(FTarget) then FTarget.Free;
  if Assigned(FValue) then FValue.Free;
  inherited Destroy;
end;

// ================================================================
// TAstUnitDecl
// ================================================================

constructor TAstUnitDecl.Create(const aPath: string; aSpan: TSourceSpan);
begin
  inherited Create(nkUnitDecl, aSpan);
  FUnitPath := aPath;
end;

// ================================================================
// TAstImportDecl
// ================================================================

constructor TAstImportDecl.Create(const aPath, aAlias: string;
  const aItems: TAstImportItemList; aSpan: TSourceSpan);
begin
  inherited Create(nkImportDecl, aSpan);
  FUnitPath := aPath;
  FAlias := aAlias;
  FItems := aItems;
end;

// ================================================================
// TAstFuncDecl
// ================================================================

constructor TAstFuncDecl.Create(const aName: string;
  const aParams: TAstParamList; aReturnType: TAurumType;
  aBody: TAstBlock; aSpan: TSourceSpan; aIsPublic: Boolean = False);
begin
  inherited Create(nkFuncDecl, aSpan);
  FName := aName;
  FParams := aParams;
  FReturnType := aReturnType;
  FReturnTypeName := '';
  FBody := aBody;
  FIsPublic := aIsPublic;
  FIsStatic := False;
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

end.
