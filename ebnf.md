# Lyx v0.1.6 – Sprachspezifikation

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
* **Float64**: `[0-9]+ '.' [0-9]+` (z.B. `3.14159`, `2.718`)
* **Stringliteral**: `" ... "` mit Escapes:

    * `\n`, `\r`, `\t`, `\\`, `\"`, `\0`
    * Ergebnis ist **nullterminiert** im `.rodata`

### Keywords (reserviert)

`fn var let co con if else while switch case break default return true false extern unit import pub as array struct static self Self`

> Reserviert für kommende Phasen (noch ohne Semantik): `class`, `extends`, `virtual`.

### Operatoren / Trennzeichen

* Zuweisung: `:=`
* Arithmetik: `+ - * / %`
* Vergleich: `== != < <= > >=`
* Logik: `&& || !`
* Sonstiges: `(` `)` `{` `}` `:` `,` `;`

---

## 2) Typen

### Primitive Typen

* `int64`  (signed 64-bit, bestehender Haupttyp)
* `int8`, `int16`, `int32`, `int64`  (signed Integer-Familie, Kurzform: **int**)
* `uint8`, `uint16`, `uint32`, `uint64`  (unsigned Integer-Familie, Kurzform: **uint**)
* `f32`, `f64`  (Floating-Point Typen: 32-bit und 64-bit)
* `bool`   (`true` / `false`)
* `void`   (nur als Funktionsrückgabetyp)
* `pchar`  (Pointer, 64-bit; bei Stringliteralen: Adresse auf nullterminierte Bytes)
* `array`  (Array-Typ für Stack-allokierte Arrays)

### Namensregeln

* Signed Integers verwenden das Präfix `int`.
* Unsigned Integers verwenden das Präfix `uin` (nicht `u`).
* `int64` bleibt der primäre Standard-Integer der Sprache.

### Typregeln (v0.1.5)

* `if`/`while` Bedingungen müssen **bool** sein.
* Vergleichsoperatoren liefern **bool**.
* Arithmetikoperatoren arbeiten auf Integer-Typen gleicher Breite; implizites Widening zu größeren `int`-Typen ist erlaubt.
* Logikoperatoren `&&`, `||`, `!` arbeiten auf **bool**; `&&`/`||` sind **short-circuit**.
* `pchar` ist nur minimal unterstützt (Stringliterale + Übergabe an Builtins).

---

## 3) Konstanten und Variablen

### Speicherklassen

* `var`  : mutable, lokaler Stackslot
* `let`  : immutable, lokaler Stackslot (nur Initialisierung erlaubt)
* `co`   : readonly runtime-constant (immutable nach Init), lokaler Stackslot
* `con`  : compile-time constant (muss konstante Initialisierung haben), **kein** lokaler Slot

> Hinweis: `let` und `co` sind semantisch sehr ähnlich; in v0.1.3 sind beide nach Init unveränderlich. `co` ist für spätere Erweiterungen reserviert, falls `let` später stärker eingeschränkt wird.

### `con` Regeln

* Initializer muss **ConstExpr** sein (siehe Grammatik)
* `con` existiert auf Top-Level
* `con pchar` muss ein Stringliteral sein

---

## 4) Builtins (v0.1.5)

Builtins werden vom Compiler als Spezialfälle behandelt (kein normales Linking nötig):

* `exit(code: int64): void`
* `PrintStr(s: pchar): void`  - druckt bis `\0` (implizites `strlen`/Loop in Runtime-Snippet)
* `PrintInt(x: int64): void`  - int->ascii (Runtime-Snippet) + `write`

Minimaler Bootstrap (falls du `PrintInt` erst später willst):

* v0.0.1 kann nur `PrintStr` + `exit`

---

## 5) Funktionen

* Nur **globale** Funktionen
* Keine closures, keine nested functions (v0.1.3)

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
Program        := [ UnitDecl ] { ImportDecl } { TopDecl } ;
```

> Konvention: **Eine Datei = ein Unit**. Der Compiler kann den Dateinamen als Default-Unitnamen verwenden, wenn `unit` fehlt (optional), empfohlen ist aber ein explizites `unit`.

### Units und Imports

```
UnitDecl       := 'unit' UnitPath ';' ;
UnitPath       := Ident { '.' Ident } ;          // z.B. std.io oder math.vec3

ImportDecl     := 'import' UnitPath [ VersionSpec ] [ ImportTail ] ';' ;
VersionSpec    := '@' Version ;                  // optional, später
Version        := IntLit '.' IntLit [ '.' IntLit ] ;

ImportTail     := [ 'as' Ident ] [ SelectiveImport ]
               | SelectiveImport [ 'as' Ident ]
               | /* leer */ ;

SelectiveImport:= '{' ImportItem { ',' ImportItem } '}' ;
ImportItem     := Ident [ 'as' Ident ] ;
```

Semantik-Intention:

* **Kein globaler Namespace**: Zugriff auf fremde Symbole nur über `import`.
* `import std.io;` macht Symbole als `std.io.<Name>` verfügbar.
* `import std.io as io;` macht Symbole als `io.<Name>` verfügbar.
* `import std.io { PrintStr as ps, exit };` importiert selektiv als lokale Namen (`ps`, `exit`).

### Top-Level Deklarationen

```
TopDecl        := PubDecl | FuncDecl | ConDecl | TypeDecl ;
PubDecl        := 'pub' ( FuncDecl | ConDecl | TypeDecl ) ;

ConDecl        := 'con' Ident ':' Type ':=' ConstExpr ';' ;

TypeDecl       := 'type' Ident '=' Type ';' ;

FuncDecl       := 'fn' Ident '(' [ ParamList ] ')' [ ':' RetType ] Block ;

ParamList      := Param { ',' Param } ;
Param          := Ident ':' Type ;

RetType        := Type | 'void' ;
```

### Blöcke und Statements

```
Block          := '{' { Stmt } '}' ;

Stmt           := VarDecl
               | LetDecl
               | CoDecl
               | AssignStmt
               | IfStmt
               | WhileStmt
               | ForStmt
               | RepeatUntilStmt
               | ReturnStmt
               | ExprStmt
               | Block ;

VarDecl        := 'var' Ident ':' Type ':=' Expr ';' ;
LetDecl        := 'let' Ident ':' Type ':=' Expr ';' ;
CoDecl         := 'co'  Ident ':' Type ':=' Expr ';' ;

AssignStmt     := LValue ':=' Expr ';' ;
LValue         := Ident { FieldOrIndexSuffix } ;
FieldOrIndexSuffix
               := '.' Ident
               | '[' Expr ']' ;

IfStmt         := 'if' '(' Expr ')' Stmt [ 'else' Stmt ] ;

WhileStmt      := 'while' [ '(' ] Expr [ ')' ] Stmt ;

ForStmt        := 'for' Ident ':=' Expr ( 'to' | 'downto' ) Expr 'do' Stmt ;

RepeatUntilStmt:= 'repeat' Block 'until' '(' Expr ')' ';' ;

ReturnStmt     := 'return' [ Expr ] ';' ;

ExprStmt       := Expr ';' ;
```

### Typen

```
Type           := PrimitiveType
               | PointerType
               | ArrayType
               | StructType
               | NamedType ;

NamedType      := Ident { '.' Ident } ;          // inkl. qualifizierter Typnamen (z.B. std.io.String)

PrimitiveType  := 'int8' | 'int16' | 'int32' | 'int64'
               | 'uint8' | 'uint16' | 'uint32' | 'uint64'
               | 'bool' | 'pchar' | 'void'
               | 'f32' | 'f64'
               | 'usize' | 'isize'
               | 'char' ;

PointerType    := '*' Type ;

ArrayType      := 'array' ;                      // Stack-allokiertes Array
                | '[' IntLit ']' Type ;          // z.B. [4]f64 (geplant)

StructType     := 'struct' '{' { StructMember } '}' ;
StructMember   := FieldDecl | MethodDecl ;
FieldDecl      := Ident ':' Type ';' ;
MethodDecl     := [ 'static' ] 'fn' Ident '(' [ ParamList ] ')' [ ':' RetType ] Block ;
> Hinweis: Der implizite Parameter `self` ist nicht Teil von `ParamList`.
> Statische Methoden mit 'static' haben keinen self-Parameter.
> In Methoden kann 'Self' als Rückgabetyp verwendet werden (wird zum Struct-Typ aufgelöst).
```

### Struktur- und Array-Literale

```
Primary        := IntLit
               | FloatLit
               | BoolLit
               | StringLit
               | CharLit
               | Ident
               | StructLit
               | ArrayLit
               | '(' Expr ')' ;

PostfixExpr    := Primary { PostfixSuffix } ;
PostfixSuffix  := '(' [ ArgList ] ')'            // Funktions- oder Methodenaufruf
               | '[' Expr ']'                   // Indexzugriff
               | '.' Ident                      // Feldzugriff
               | '.' Ident '(' [ ArgList ] ')' ;// Methodenaufruf mit implizitem self

> Beispiel: `player.take_damage(5)` wird als `Primary = Ident` + `PostfixSuffix = '.' Ident '(' ArgList ')'` erkannt.

FloatLit       := [0-9]+ '.' [0-9]+ ;

StructLit      := Type '{' [ FieldInit { ',' FieldInit } ] '}' ;
FieldInit      := Ident ':' Expr ;

ArrayLit       := '[' [ Expr { ',' Expr } ] ']' ;
```

### Ausdrücke (Operatorpräzedenz)

```
Expr           := OrExpr ;

OrExpr         := AndExpr { '||' AndExpr } ;
AndExpr        := CmpExpr { '&&' CmpExpr } ;

CmpExpr        := AddExpr [ ( '==' | '!=' | '<' | '<=' | '>' | '>=' ) AddExpr ] ;

AddExpr        := MulExpr { ( '+' | '-' ) MulExpr } ;
MulExpr        := CastExpr { ( '*' | '/' | '%' ) CastExpr } ;

CastExpr       := UnaryExpr [ 'as' Type ] ;
UnaryExpr      := ( '!' | '-' ) UnaryExpr | PostfixExpr ;

ArgList        := Expr { ',' Expr } ;

BoolLit        := 'true' | 'false' ;
CharLit        := '\'' ( EscapedChar | CharBody ) '\'' ;
```

### Konstante Ausdrücke (für `con`)

```
ConstExpr      := ConstOrExpr ;

ConstOrExpr    := ConstAndExpr { '||' ConstAndExpr } ;
ConstAndExpr   := ConstCmpExpr { '&&' ConstCmpExpr } ;
ConstCmpExpr   := ConstAddExpr [ ( '==' | '!=' | '<' | '<=' | '>' | '>=' ) ConstAddExpr ] ;
ConstAddExpr   := ConstMulExpr { ( '+' | '-' ) ConstMulExpr } ;
ConstMulExpr   := ConstCastExpr { ( '*' | '/' | '%' ) ConstCastExpr } ;
ConstCastExpr  := ConstUnaryExpr [ 'as' Type ] ;
ConstUnaryExpr := ( '!' | '-' ) ConstUnaryExpr | ConstPrimary ;

ConstPrimary   := IntLit | FloatLit | BoolLit | StringLit | CharLit | '(' ConstExpr ')' ;
```

---

## 7) Semantikregeln (v0.1.6)

### Namespaces / Scopes

* Top-Level Scope: Funktionen + `con` + Unit-Deklarationen
* Funktionsscope: Parameter + lokale `var/let/co`  
* Block erzeugt neuen Scope
* Import-Scope: Importierte Symbole werden in qualifizierten Namespace aufgelöst

### Dot-Notation für Structs

* `expr.field` greift auf ein Datenfeld des Struct-Typs zu (Offset-Berechnung zur Compilezeit).
* `expr.method(args…)` wird in einen Funktionsaufruf desugart: `_L_<Struct>_<Method>(&expr, args…)`.
* Der Compiler injiziert den impliziten `self`-Pointer als ersten Parameter; `self` zeigt immer auf die Adresse der Instanz.
* Methoden sind statisch gebunden – es existiert kein virtuelles Dispatching.

### Statische Methoden

* Statische Methoden werden mit `static fn` deklariert.
* Sie haben keinen impliziten `self`-Parameter.
* Aufruf erfolgt mit `TypeName.method(args…)`.
* Der Compiler entfernt den Typnamen aus den Argumenten und ruft `_L_<Struct>_<Method>(args…)` auf.

### Der `Self`-Typ

* `Self` kann in Methoden als Rückgabetyp verwendet werden.
* Wird zur Compilezeit zum umschließenden Struct-Typ aufgelöst.
* Beispiel: `fn new(x: int64): Self { … }` → Rückgabetyp ist der Struct-Typ.

### Zuweisungsregeln

* Nur `var` ist nach Init erneut zuweisbar
* `let` und `co` dürfen nur bei Deklaration initialisiert werden
* `con` ist compile-time und nicht zuweisbar

### Return-Regeln

* Funktion mit Rückgabetyp `int64/bool/pchar/f64` muss auf allen Pfaden `return <expr>;` liefern
* Funktion mit `void` erlaubt `return;` oder gar keinen return (dann implizit `return;`)

### Type-Casting Regeln

* `as`-Casting ist explizit und erlaubt folgende Konvertierungen:
  * `int64 as f64`: Integer zu Float (SSE2 cvtsi2sd)
  * `f64 as int64`: Float zu Integer mit Truncation (SSE2 cvttsd2si)
  * Identity Casts: `int64 as int64`, `f64 as f64` (No-Op)
* Cast-Kompatibilität wird zur Compile-Zeit geprüft
* Ungültige Casts erzeugen Compile-Fehler

### Bool-Regeln

* `if`/`while` verlangen `bool`
* `&&`/`||` short-circuit (Codegen muss Sprünge setzen)

### Builtin-Funktionen Regeln

* Über 30 Builtin-Funktionen sind ohne Import verfügbar
* Builtins werden nicht als externe Symbole behandelt (statische ELF-Generation)
* String-Builtins arbeiten mit null-terminierten pchar
* Math-Builtins nutzen native x86-64 Instruktionen für maximale Performance

---

## 8) Codegen-Anforderungen (Backend v0.1.6)

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

* `PrintStr`: strlen-loop + write
* `PrintInt`: itoa + write

---

## 9) Beispiele

### Hello

```lyx
fn main(): int64 {
  PrintStr("Hello Lyx\n");
  return 0;
}
```

### Variablen + while + Type-Casting

```lyx
con LIMIT: int64 := 5;

fn main(): int64 {
  var i: int64 := 0;
  while (i < LIMIT) {
    PrintInt(i);
    PrintStr("\n");
    
    // Type-Casting Beispiel
    var f: f64 := i as f64;
    var sqrt_val: f64 := sqrt(f);
    var back_to_int: int64 := sqrt_val as int64;
    
    // String-Konvertierung
    var str_val: pchar := int_to_str(i);
    PrintStr("String: ");
    PrintStr(str_val);
    PrintStr("\n");
    
    i := i + 1;
  }
  return 0;
}
```

### Module-System Beispiel

```lyx
unit math.utils;

import std.string;

pub fn factorial(n: int64): int64 {
  if (n <= 1) { return 1; }
  return n * factorial(n - 1);
}

pub fn format_number(x: int64): pchar {
  return int_to_str(abs(x));
}
```

... (rest unchanged)

Diese Erweiterung passt zu Lyxs Pascal-ähnlichem Stil, erweitert um AI/ML-Features aus Sever/A.[web:21][page:0] Der Parser braucht ~10 neue Regeln; Codegen kann LLVM/CUDA-Backend nutzen für GPU (z. B. via FFI zu cuBLAS).[cite:15]

## Implementierungs-Hinweise
- **Tensor-Codegen**: Compiler erzeugt CUDA-Kernels; Fallback zu CPU via OpenBLAS.
- **Probabilistic**: Zero-Cost Abstractions – option/result als Tagged Unions (1 Byte Discriminant).
- **GPU-Setup**: Built-in `@gpu` triggert Linker-Flags (-lcudart); für Linux/RTX3060.[cite:12]
- **Nächster Schritt**: Vollständige EBNF-Datei oder Parser-Test in Rust/Lark generieren?
