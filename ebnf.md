# Aurum v0.1.3 – Mini-Spezifikation

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

`fn var let co con if else while return true false extern unit import pub as array`

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

### Typregeln (v0.1.3)

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

## 4) Builtins (v0.1.3)

Builtins werden vom Compiler als Spezialfälle behandelt (kein normales Linking nötig):

* `exit(code: int64): void`
* `print_str(s: pchar): void`  - druckt bis `\0` (implizites `strlen`/Loop in Runtime-Snippet)
* `print_int(x: int64): void`  - int->ascii (Runtime-Snippet) + `write`

Minimaler Bootstrap (falls du `print_int` erst später willst):

* v0.0.1 kann nur `print_str` + `exit`

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
* `import std.io { print_str as ps, exit };` importiert selektiv als lokale Namen (`ps`, `exit`).

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
LValue         := Ident | FieldAccess | IndexAccess ;

IfStmt         := 'if' '(' Expr ')' Stmt [ 'else' Stmt ] ;

WhileStmt      := 'while' '(' Expr ')' Stmt ;

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

StructType     := 'struct' '{' { FieldDecl } '}' ;
FieldDecl      := Ident ':' Type ';' ;
```

### Struktur- und Array-Literale

```
Primary        := IntLit
               | FloatLit
               | BoolLit
               | StringLit
               | CharLit
               | Ident
               | Call
               | IndexAccess
               | StructLit
               | ArrayLit
               | '(' Expr ')' ;

FloatLit       := [0-9]+ '.' [0-9]+ ;

IndexAccess    := Primary '[' Expr ']' ;         // arr[i], arr[expr]

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
MulExpr        := UnaryExpr { ( '*' | '/' | '%' ) UnaryExpr } ;

UnaryExpr      := ( '!' | '-' ) UnaryExpr | Primary ;

Call           := Ident '(' [ ArgList ] ')' ;
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
ConstMulExpr   := ConstUnaryExpr { ( '*' | '/' | '%' ) ConstUnaryExpr } ;
ConstUnaryExpr := ( '!' | '-' ) ConstUnaryExpr | ConstPrimary ;

ConstPrimary   := IntLit | BoolLit | StringLit | CharLit | '(' ConstExpr ')' ;
```

---

## 7) Semantikregeln (v0.1.3)

### Namespaces / Scopes

* Top-Level Scope: Funktionen + `con`
* Funktionsscope: Parameter + lokale `var/let/co`
* Block erzeugt neuen Scope

### Zuweisungsregeln

* Nur `var` ist nach Init erneut zuweisbar
* `let` und `co` dürfen nur bei Deklaration initialisiert werden
* `con` ist compile-time und nicht zuweisbar

### Return-Regeln

* Funktion mit Rückgabetyp `int64/bool/pchar` muss auf allen Pfaden `return <expr>;` liefern (v0.1.3: einfache syntaktische Prüfung reicht erstmal)
* Funktion mit `void` erlaubt `return;` oder gar keinen return (dann implizit `return;`)

### Bool-Regeln

* `if`/`while` verlangen `bool`
* `&&`/`||` short-circuit (Codegen muss Sprünge setzen)

---

## 8) Codegen-Anforderungen (Backend v0.1.3)

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

1) Erweiterung der Typ-EBNF
Neue Typvariante
text
Type           := PrimitiveType
               | PointerType
               | ArrayType
               | StructType
               | ClassType        // NEU
               | NamedType ;
Klassentypen inkl. Vererbung
text
ClassType      := 'class' [ Inheritance ] '{' { MemberDecl } '}' ;

Inheritance    := '(' NamedType ')' ;   // z.B. class(TObject)

MemberDecl     := FieldDecl
               | MethodDecl
               | CtorDecl
               | DtorDecl ;
2) Methoden, Konstruktor, Destruktor
text
MethodDecl     := 'fn' Ident '(' [ ParamList ] ')' [ ':' RetType ] MethodBlock ;

CtorDecl       := 'constructor' Ident '(' [ ParamList ] ')' MethodBlock ;

DtorDecl       := 'destructor' Ident '(' [ ParamList ] ')' MethodBlock ;

MethodBlock    := Block ;   // wie bisher
Wenn du’s näher an Pascal halten willst, kannst du als Konvention z. B. constructor Create und destructor Destroy erzwingen, das wäre dann eine semantische Regel, nicht Grammatik.

3) Objektinstanzierung und Memberzugriff
Primaries erweitern
text
Primary        := IntLit
               | BoolLit
               | StringLit
               | CharLit
               | Ident
               | Call
               | StructLit
               | ArrayLit
               | MemberAccess        // NEU
               | NewExpr             // NEU
               | '(' Expr ')' ;
Memberzugriff (Feld/Methoden)
text
MemberAccess   := PrimaryNoMember '.' Ident [ '(' [ ArgList ] ')' ] ;

PrimaryNoMember:= IntLit
               | BoolLit
               | StringLit
               | CharLit
               | Ident
               | StructLit
               | ArrayLit
               | NewExpr
               | '(' Expr ')' ;
(Trick: PrimaryNoMember verhindert linksrekursive Definition bei MemberAccess.)

Objekt-Erzeugung
text
NewExpr        := 'new' Type '(' [ ArgList ] ')' ;
Semantisch: für einen class-Typ ruft das den Konstruktor; für Records/Structs könntest du es verbieten oder speziell behandeln.

4) LValues an OO anpassen
text
LValue         := Ident
               | FieldAccess
               | IndexAccess
               | MemberLValue ;

MemberLValue   := PrimaryNoMember '.' Ident ;
Damit kannst du obj.field := 42; oder self.count := self.count + 1; zulassen.

5) Zusatz: implizites self und Vererbung (semantisch)
Rein semantisch (nicht EBNF, aber wichtig für dein Design):

In MethodDecl, CtorDecl, DtorDecl ist self (oder this) ein impliziter Parameter vom Typ der umgebenden ClassType.

Bei Inheritance wird:

die Basisklasse als erster versteckter Parent im Typgraphen hinterlegt,

Member-Lookup entlang der Vererbungskette durchgeführt.

Methoden können überschrieben werden, wenn Signatur kompatibel ist; ob du ein override-Keyword willst, ist eine spätere Erweiterung.
