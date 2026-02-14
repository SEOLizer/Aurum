# Aurum v0.1 – Mini-Spezifikation

Ziel: Minimaler, nativer Compiler für **Linux x86_64 (ELF64)**, erweiterbar durch saubere Trennung von Frontend/IR/Backend.

---

## 1) Lexikalische Regeln

### Whitespace

* Leerzeichen, Tabs, Zeilenumbrüche trennen Tokens.

### Kommentare

* Zeilenkommentar: `//` bis Zeilenende
* Blockkommentar (optional, empfohlen): `/* ... */` (nicht verschachtelt)

### Identifier

* Regex: `[A-Za-z_][A-Za-z0-9_]*`
* case-sensitive

### Literale

* **Int64**: Dezimal: `0` oder `[1-9][0-9]*` (optional führendes `-` als unary operator)
* **Stringliteral**: `" ... "` mit Escapes:

    * `\n`, `\r`, `\t`, `\\`, `\"`, `\0`
    * Ergebnis ist **nullterminiert** im `.rodata`

### Keywords (reserviert)

`fn var let co con if else while return true false extern`

### Operatoren / Trennzeichen

* Zuweisung: `:=`
* Arithmetik: `+ - * / %`
* Vergleich: `== != < <= > >=`
* Logik: `&& || !`
* Sonstiges: `(` `)` `{` `}` `:` `,` `;`

---

## 2) Typen

### Primitive Typen

* `int64`  (signed 64-bit)
* `bool`   (`true` / `false`)
* `void`   (nur als Funktionsrückgabetyp)
* `pchar`  (Pointer, 64-bit; bei Stringliteralen: Adresse auf nullterminierte Bytes)

### Typregeln (v0.1)

* `if`/`while` Bedingungen müssen **bool** sein.
* Vergleichsoperatoren liefern **bool**.
* Arithmetikoperatoren arbeiten auf **int64**.
* Logikoperatoren `&&`, `||`, `!` arbeiten auf **bool**; `&&`/`||` sind **short-circuit**.
* `pchar` ist nur minimal unterstützt (Stringliterale + Übergabe an Builtins).

---

## 3) Konstanten und Variablen

### Speicherklassen

* `var`  : mutable, lokaler Stackslot
* `let`  : immutable, lokaler Stackslot (nur Initialisierung erlaubt)
* `co`   : readonly runtime-constant (immutable nach Init), lokaler Stackslot
* `con`  : compile-time constant (muss konstante Initialisierung haben), **kein** lokaler Slot

> Hinweis: `let` und `co` sind semantisch sehr ähnlich; in v0.1 sind beide nach Init unveränderlich. `co` ist für spätere Erweiterungen reserviert, falls `let` später stärker eingeschränkt wird.

### `con` Regeln

* Initializer muss **ConstExpr** sein (siehe Grammatik)
* `con` existiert auf Top-Level
* `con pchar` muss ein Stringliteral sein

---

## 4) Builtins (v0.1)

Builtins werden vom Compiler als Spezialfälle behandelt (kein normales Linking nötig):

* `exit(code: int64): void`
* `print_str(s: pchar): void`  - druckt bis `\0` (implizites `strlen`/Loop in Runtime-Snippet)
* `print_int(x: int64): void`  - int->ascii (Runtime-Snippet) + `write`

Minimaler Bootstrap (falls du `print_int` erst später willst):

* v0.0.1 kann nur `print_str` + `exit`

---

## 5) Funktionen

* Nur **globale** Funktionen
* Keine closures, keine nested functions (v0.1)

### ABI (Linux x86_64 SysV)

* Return `int64` in `RAX`
* Params 1..6 in `RDI, RSI, RDX, RCX, R8, R9`
* Stack 16-byte aligned vor `call`

### Programmstart

* Compiler generiert `_start`:

    * ruft `main()`
    * ruft `exit(rax)`

---

## 6) Grammatik (EBNF)

### Startsymbol

```
Program     := { TopDecl } ;
```

### Top-Level Deklarationen

```
TopDecl     := FuncDecl | ConDecl ;

ConDecl     := 'con' Ident ':' Type ':=' ConstExpr ';' ;

FuncDecl    := 'fn' Ident '(' [ ParamList ] ')' [ ':' RetType ] Block ;

ParamList   := Param { ',' Param } ;
Param       := Ident ':' Type ;

RetType     := Type | 'void' ;
```

### Blöcke und Statements

```
Block       := '{' { Stmt } '}' ;

Stmt        := VarDecl
            | LetDecl
            | CoDecl
            | AssignStmt
            | IfStmt
            | WhileStmt
            | ReturnStmt
            | ExprStmt
            | Block ;

VarDecl     := 'var' Ident ':' Type ':=' Expr ';' ;
LetDecl     := 'let' Ident ':' Type ':=' Expr ';' ;
CoDecl      := 'co'  Ident ':' Type ':=' Expr ';' ;

AssignStmt  := LValue ':=' Expr ';' ;
LValue      := Ident ;

IfStmt      := 'if' '(' Expr ')' Stmt [ 'else' Stmt ] ;

WhileStmt   := 'while' '(' Expr ')' Stmt ;

ReturnStmt  := 'return' [ Expr ] ';' ;

ExprStmt    := Expr ';' ;
```

### Ausdrücke (Operatorpräzedenz)

Notation: EBNF ist hier als Präzedenzkaskade geschrieben.

```
Expr        := OrExpr ;

OrExpr      := AndExpr { '||' AndExpr } ;
AndExpr     := CmpExpr { '&&' CmpExpr } ;

CmpExpr     := AddExpr [ ( '==' | '!=' | '<' | '<=' | '>' | '>=' ) AddExpr ] ;

AddExpr     := MulExpr { ( '+' | '-' ) MulExpr } ;
MulExpr     := UnaryExpr { ( '*' | '/' | '%' ) UnaryExpr } ;

UnaryExpr   := ( '!' | '-' ) UnaryExpr | Primary ;

Primary     := IntLit
            | BoolLit
            | StringLit
            | Ident
            | Call
            | '(' Expr ')' ;

Call        := Ident '(' [ ArgList ] ')' ;
ArgList     := Expr { ',' Expr } ;

BoolLit     := 'true' | 'false' ;
```

### Konstante Ausdrücke (für `con`)

```
ConstExpr   := ConstOrExpr ;

ConstOrExpr := ConstAndExpr { '||' ConstAndExpr } ;
ConstAndExpr:= ConstCmpExpr { '&&' ConstCmpExpr } ;
ConstCmpExpr:= ConstAddExpr [ ( '==' | '!=' | '<' | '<=' | '>' | '>=' ) ConstAddExpr ] ;
ConstAddExpr:= ConstMulExpr { ( '+' | '-' ) ConstMulExpr } ;
ConstMulExpr:= ConstUnaryExpr { ( '*' | '/' | '%' ) ConstUnaryExpr } ;
ConstUnaryExpr := ( '!' | '-' ) ConstUnaryExpr | ConstPrimary ;

ConstPrimary := IntLit | BoolLit | StringLit | '(' ConstExpr ')' ;
```

> In v0.1 gelten `con`-Ausdrücke als "foldable". Keine Identifiers in ConstExpr (außer du erlaubst später `con`-Referenzen).

---

## 7) Semantikregeln (v0.1)

### Namespaces / Scopes

* Top-Level Scope: Funktionen + `con`
* Funktionsscope: Parameter + lokale `var/let/co`
* Block erzeugt neuen Scope

### Zuweisungsregeln

* Nur `var` ist nach Init erneut zuweisbar
* `let` und `co` dürfen nur bei Deklaration initialisiert werden
* `con` ist compile-time und nicht zuweisbar

### Return-Regeln

* Funktion mit Rückgabetyp `int64/bool/pchar` muss auf allen Pfaden `return <expr>;` liefern (v0.1: einfache syntaktische Prüfung reicht erstmal)
* Funktion mit `void` erlaubt `return;` oder gar keinen return (dann implizit `return;`)

### Bool-Regeln

* `if`/`while` verlangen `bool`
* `&&`/`||` short-circuit (Codegen muss Sprünge setzen)

---

## 8) Codegen-Anforderungen (Backend v0.1)

### Output

* ELF64 Executable, Linux x86_64
* `_start` als Entry

### Minimaler Instruction-Satz (Backend-Emitter)

* `mov reg, imm64`
* `mov reg, [rbp-off]` / `mov [rbp-off], reg` (locals)
* `add/sub/imul/idiv` (int64 arith)
* `cmp` + `setcc`/`cmov` oder Sprünge (bool)
* `jmp`, `je/jne/jl/jle/jg/jge` (control flow)
* `call`, `ret` (Funktionen)
* `syscall` (builtins/entry)

### Runtime-Snippets (eingebettet)

* `print_str`: strlen-loop + write
* `print_int`: itoa + write

---

## 9) Beispiele

### Hello

```aurum
fn main(): int64 {
  print_str("Hello Aurum\n");
  return 0;
}
```

### Variablen + while

```aurum
con LIMIT: int64 := 5;

fn main(): int64 {
  var i: int64 := 0;
  while (i < LIMIT) {
    print_int(i);
    print_str("\n");
    i := i + 1;
  }
  return 0;
}
```
