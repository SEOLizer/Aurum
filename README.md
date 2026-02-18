# Lyx

**Lyx** ist ein nativer Compiler für die gleichnamige Programmiersprache, geschrieben in FreePascal.
Er erzeugt direkt ausführbare **Linux x86_64 ELF64-Binaries** — ohne libc, ohne Linker, rein über Syscalls.

```
Lyx Compiler v0.1.4
Copyright (c) 2026 Andreas Röne. Alle Rechte vorbehalten.

✅ Vollständiges Module System mit Import/Export
✅ Cross-Unit Function Calls und Symbol Resolution  
✅ Standard Library (std.math, std.io)
✅ Robuste Parser mit While/If/Function Support
```

---

## Schnellstart

```bash
# Compiler bauen
make build

# Programm kompilieren und ausführen
./lyxc examples/hello.lyx -o hello
./hello
```

```
Hello Lyx
```

---

## Die Sprache Lyx

Lyx ist **prozedural** und **statisch typisiert** — inspiriert von C und Rust, mit einer eigenen, kompakten Syntax.

### Hello World

```lyx
fn main(): int64 {
  print_str("Hello Lyx\n");
  return 0;
}
```

### Variablen und Arithmetik

Vier Speicherklassen steuern Veränderbarkeit und Lebensdauer:

```lyx
fn main(): int64 {
  var x: int64 := 10;       // mutable
  let y: int64 := 20;       // immutable nach Init
  co  z: int64 := x + y;    // readonly (Runtime-Konstante)

  x := x + 1;               // erlaubt: var ist mutable
  // y := 0;                 // verboten: let ist immutable

  print_int(x + y + z);
  print_str("\n");
  return 0;
}
```

| Keyword | Veränderbar | Compilezeit | Speicher |
|---------|:-----------:|:-----------:|----------|
| `var`   | ja          | —           | Stack    |
| `let`   | nein        | —           | Stack    |
| `co`    | nein        | optional    | Stack    |
| `con`   | nein        | ja          | Immediate / rodata |

### Compile-Time-Konstanten

`con`-Deklarationen existieren auf Top-Level und werden zur Compilezeit aufgelöst — kein Stackslot, direkt als Immediate eingebettet:

```lyx
con LIMIT: int64 := 5;
con MSG: pchar := "Loop\n";

fn main(): int64 {
  var i: int64 := 0;
  while (i < LIMIT) {
    print_str(MSG);
    i := i + 1;
  }
  return 0;
}
```

```
Loop
Loop
Loop
Loop
Loop
```

### Module System und Standard Library

Lyx unterstützt ein vollständiges **Import/Export-System** für die Organisation von Code in wiederverwendbare Module:

```lyx
// std/math.lyx
pub fn abs64(x: int64): int64 {
  if (x < 0) {
    return -x;
  }
  return x;
}

pub fn times_two(x: int64): int64 {
  return x * 2;
}
```

```lyx
// main.lyx
import std.math;

fn main(): int64 {
  let result: int64 := abs64(-42);
  print_int(times_two(result));  // Output: 84
  return 0;
}
```

**Verfügbare Standard Library:**
- `std.math`: Mathematische Funktionen (`abs64`, `min64`, `max64`, `times_two`)
- `std.io`: I/O Funktionen (in Entwicklung)
- `std.string`: String-Manipulation (geplant)

### Typen

| Typ       | Beschreibung                          |
|-----------|---------------------------------------|
| `int64`   | Signed 64-bit Integer                 |
| `int`     | Alias für `int64` (konventionell)     |
| `f32`     | 32-bit Floating-Point (IEEE 754)      |
| `f64`     | 64-bit Floating-Point (IEEE 754)      |
| `bool`    | `true` / `false`                      |
| `void`    | Nur als Funktions-Rückgabetyp         |
| `pchar`   | Pointer auf nullterminierte Bytes     |
| `string`  | Alias für `pchar` (nullterminierte Bytes)
| `array`   | Array-Typ (Stack-allokiert)           |

Hinweis: `int` und `string` sind derzeit Alias-Typen (bzw. Abkürzungen) — `int` wird intern als `int64` behandelt, `string` wird als `pchar` gemappt. Keine impliziten Casts — alle Typen müssen explizit übereinstimmen.

### Float-Literale

Float-Literale werden mit Dezimalpunkt geschrieben und sind vom Typ `f64`:

```lyx
fn main(): int64 {
  var pi: f64 := 3.14159;
  var e: f64 := 2.71828;

  // Float-Konstanten auf Top-Level
  con PI: f64 := 3.1415926535;

  return 0;
}
```

### Arrays

Arrays werden auf dem Stack allokiert und können literale Initialisierung, Lesezugriff und Zuweisung:

```lyx
fn main(): int64 {
  // Array-Literal
  var arr: array := [10, 20, 30];

  // Element lesen
  var first: int64 := arr[0];    // 10

  // Element zuweisen
  arr[0] := 100;

  // Dynamischer Index
  var i: int64 := 1;
  var second: int64 := arr[i];   // 20

  return 0;
}
```

Alle Elemente eines Arrays müssen denselben Typ haben (derzeit `int64`).

### Operatoren

| Priorität | Operatoren           | Beschreibung              |
|:---------:|----------------------|---------------------------|
| 1 (niedrig) | `||`            | Logisches Oder            |
| 2         | `&&`                 | Logisches Und             |
| 3         | `==` `!=` `<` `<=` `>` `>=` | Vergleich (liefert `bool`) |
| 4         | `+` `-`              | Addition, Subtraktion     |
| 5         | `*` `/` `%`          | Multiplikation, Division, Modulo |
| 6 (hoch)  | `!` `-` (unär)       | Logisches NOT, Negation   |

Zuweisung erfolgt mit `:=` (nicht `=`).

### Kontrollfluss

#### if / else

```lyx
fn main(): int64 {
  var x: int64 := 42;
  if (x > 10) {
    print_str("gross\n");
  } else {
    print_str("klein\n");
  }
  return 0;
}
```

#### while-Schleife

```lyx
fn main(): int64 {
  var i: int64 := 0;
  while (i < 5) {
    print_int(i);
    print_str("\n");
    i := i + 1;
  }
  return 0;
}
```

```
0
1
2
3
4
```

#### Bool-Ausdrücke als Bedingung

```lyx
fn main(): int64 {
  var active: bool := true;
  if (active)
    print_str("aktiv\n");
  return 0;
}
```

#### switch / case

switch/case wurde ergänzt und unterstützt nun fallweise sowohl Block‑Bodies als auch einzelne Statements. `break` beendet die nächsthöhere Schleife oder den aktuellen switch‑Fall.

Beispiel (Block‑Bodies):

```lyx
fn classify(x: int64): int64 {
  switch (x % 3) {
    case 0: {
      print_str("divisible by 3\n");
      return 0;
    }
    case 1: {
      print_str("remainder 1\n");
      return 1;
    }
    default: {
      print_str("other\n");
      return 2;
    }
  }
}
```

Beispiel (Single‑Statement‑Bodies):

```lyx
switch (n) {
  case 0: print_str("zero\n");
  case 1: print_str("one\n");
  default: print_str("many\n");
}
```

Hinweis: `case`‑Labels müssen momentan Ganzzahlen (int/int64) sein; Semantik und Codegen behandeln `int` als 64‑Bit.
### Funktionen

Funktionen sind global, folgen der SysV ABI (Parameter in Registern) und unterstützen bis zu 6 Parameter:

```lyx
fn add(a: int64, b: int64): int64 {
  return a + b;
}

fn main(): int64 {
  var x: int64 := add(2, 3);
  print_int(x);
  print_str("\n");
  return 0;
}
```

```
5
```

Funktionen ohne Rückgabetyp sind implizit `void`:

```lyx
fn greet() {
  print_str("Hallo!\n");
}

fn main(): int64 {
  greet();
  return 0;
}
```

### Builtins

Fünf eingebaute Funktionen stehen ohne Import zur Verfügung:

| Funktion          | Signatur               | Beschreibung                        |
|-------------------|------------------------|-------------------------------------|
| `print_str(s)`    | `pchar -> void`        | Gibt String bis `\0` aus            |
| `print_int(x)`    | `int64 -> void`        | Gibt Integer als Dezimalzahl aus    |
| `print_float(x)`  | `f64 -> void`          | Gibt Float aus (Placeholder: `?`)   |
| `strlen(s)`       | `pchar -> int64`       | Berechnet String-Länge              |
| `exit(code)`      | `int64 -> void`        | Beendet das Programm mit Exit-Code  |

### Externe Funktionen (v0.1.4+)

Der Lyx-Compiler unterstützt jetzt Deklarationen externer Funktionen aus System-Libraries:

```lyx
extern fn malloc(size: int64): pchar;
extern fn printf(format: pchar, ...): int64;
extern fn strlen(str: pchar): int64;

fn main(): int64 {
  let ptr: pchar := malloc(64);
  let len: int64 := strlen("Hello");
  printf("Allocated %d bytes, string length: %d\n", 64, len);
  return 0;
}
```

#### Varargs-Unterstützung

Externe Funktionen können variable Argumentlisten mit `...` deklarieren:

```lyx
extern fn printf(fmt: pchar, ...): int64;

fn main(): int64 {
  printf("Int: %d, String: %s, Float: %f\n", 42, "test", 3.14);
  return 0;
}
```

#### Dynamische vs. Statische ELF-Binaries

Der Compiler erkennt automatisch, ob externe Symbole verwendet werden:

- **Statische ELF**: Wenn keine externen Funktionen aufgerufen werden
- **Dynamische ELF**: Automatisch bei Verwendung externer Symbole

```bash
# Statisches Binary (keine externen Aufrufe)
./lyxc hello.lyx -o hello_static
# Output: "Generating static ELF (no external symbols)"

# Dynamisches Binary (mit malloc/printf)
./lyxc extern_example.lyx -o extern_dynamic  
# Output: "Generating dynamic ELF with 2 external symbols"
```

#### Library-Zuordnung

Der Compiler ordnet Symbole automatisch den passenden Libraries zu:

| Symbol | Library |
|--------|---------|
| `printf`, `malloc`, `strlen`, `exit` | `libc.so.6` |
| `sin`, `cos`, `sqrt` | `libm.so.6` |
| Unbekannte Symbole | `libc.so.6` (Fallback) |

#### PLT/GOT-Mechanik

Dynamische Binaries nutzen die **Procedure Linkage Table (PLT)** und **Global Offset Table (GOT)**:

- **PLT-Stubs**: Jede externe Funktion erhält einen PLT-Eintrag
- **GOT-Entries**: Enthalten Runtime-Adressen der Library-Funktionen
- **Relocations**: Dynamic Linker patcht GOT zur Laufzeit

```bash
# ELF-Struktur analysieren
readelf -l extern_binary    # PT_INTERP, PT_DYNAMIC Headers
readelf -d extern_binary    # NEEDED libraries, Symbol tables
```

Standard-Units

Ein Satz von Standard-Units befindet sich im Verzeichnis `std/` und liefert ergonomische Bibliotheksfunktionen:

- std/math.lyx – Integer-Hilfen (abs64, min64, max64, div64, mod64, times_two)
- std/io.lyx   – I/O-Wrappers (print, println, print_intln, exit_proc)
- std/env.lyx  – Environment-API (init, arg_count, arg)

Import-Beispiel

```lyx
import std.math;
import std.io;
import std.env; // optional

fn main(argc: int64, argv: pchar): int64 {
  // Seit der aktuellen Version initialisiert der Compiler automatisch argc/argv,
  // daher ist ein manuelles init nicht mehr zwingend, bleibt jedoch verfügbar.
  // init(argc, argv);

  print_intln(arg_count());
  print_str(arg(0));
  print_str("\n");
  return 0;
}
```

CI / Integrationstests

- Die GitHub Actions CI baut den Compiler, führt Unit-Tests und zusätzlich kompiliert und führt die Beispielprogramme in `examples/` aus, um die Laufzeitintegration zu prüfen.


```lyx
fn main(): int64 {
  print_int(0);
  print_str("\n");
  print_int(12345);
  print_str("\n");
  print_int(-42);
  print_str("\n");
  return 0;
}
```

```
0
12345
-42
```

### String-Escape-Sequenzen

| Escape | Bedeutung       |
|--------|-----------------|
| `\n`   | Zeilenumbruch   |
| `\r`   | Wagenrücklauf   |
| `\t`   | Tabulator        |
| `\\`   | Backslash        |
| `\"`   | Anführungszeichen|
| `\0`   | Null-Byte        |

### Kommentare

```lyx
// Zeilenkommentar

/* Blockkommentar
   über mehrere Zeilen */
```

### Reservierte Keywords

```
fn  var  let  co  con  if  else  while  switch  case  break  default  return  true  false  extern  array
```

`extern` wird für externe Funktionsdeklarationen verwendet (v0.1.4+).

---

## Praktische SO-Library Beispiele (v0.1.4)

### Memory Management mit malloc/free

```lyx
extern fn malloc(size: int64): pchar;
extern fn free(ptr: pchar): void;

fn main(): int64 {
  // Dynamischer Speicher allokieren
  let buffer: pchar := malloc(256);
  
  // Buffer verwenden (vereinfacht)
  print_str("Buffer allocated at: ");
  
  // Speicher wieder freigeben
  free(buffer);
  
  print_str("Memory freed\n");
  return 0;
}
```

### Formatierte Ausgabe mit printf

```lyx
extern fn printf(format: pchar, ...): int64;

fn main(): int64 {
  var count: int64 := 42;
  var pi: f64 := 3.14159;
  var name: pchar := "Lyx";
  
  // Verschiedene Datentypen ausgeben
  printf("Language: %s\n", name);
  printf("Count: %d\n", count);  
  printf("Pi: %.2f\n", pi);
  printf("Mixed: %s has %d features!\n", name, count);
  
  return 0;
}
```

### String-Verarbeitung

```lyx
extern fn strlen(str: pchar): int64;
extern fn strcat(dest: pchar, src: pchar): pchar;
extern fn strcpy(dest: pchar, src: pchar): pchar;

fn main(): int64 {
  var greeting: pchar := "Hello";
  var target: pchar := "World";
  
  var len1: int64 := strlen(greeting);
  var len2: int64 := strlen(target);
  
  printf("'%s' has %d characters\n", greeting, len1);
  printf("'%s' has %d characters\n", target, len2);
  
  return 0;
}
```

### Math-Library Funktionen

```lyx
extern fn sin(x: f64): f64;
extern fn cos(x: f64): f64; 
extern fn sqrt(x: f64): f64;

fn main(): int64 {
  var angle: f64 := 1.5708;  // π/2
  var number: f64 := 16.0;
  
  printf("sin(%.4f) = %.4f\n", angle, sin(angle));
  printf("cos(%.4f) = %.4f\n", angle, cos(angle));
  printf("sqrt(%.1f) = %.4f\n", number, sqrt(number));
  
  return 0;
}
```

## Vollständiges Beispiel

Ein Programm, das alle Kernfeatures kombiniert:

```lyx
con MAX: int64 := 10;
con FIZZ: pchar := "Fizz\n";
con BUZZ: pchar := "Buzz\n";
con FIZZBUZZ: pchar := "FizzBuzz\n";

fn is_divisible(n: int64, d: int64): bool {
  var remainder: int64 := n % d;
  if (remainder == 0)
    return true;
  return false;
}

fn main(): int64 {
  var i: int64 := 1;
  while (i <= MAX) {
    if (is_divisible(i, 15)) {
      print_str(FIZZBUZZ);
    } else {
      if (is_divisible(i, 3)) {
        print_str(FIZZ);
      } else {
        if (is_divisible(i, 5)) {
          print_str(BUZZ);
        } else {
          print_int(i);
          print_str("\n");
        }
      }
    }
    i := i + 1;
  }
  return 0;
}
```

---

## Build & Test

### Voraussetzungen

- **FreePascal Compiler** (FPC 3.2.2+)
- **Linux x86_64** (Zielplattform der erzeugten Binaries)
- GNU Make

### Compiler bauen

```bash
make build          # Release-Build (-O2)
make debug          # Debug-Build mit Checks (-g -gl -Ci -Cr -Co -gh)
```

### Lyx-Programm kompilieren

```bash
./lyxc eingabe.lyx -o ausgabe
./ausgabe
echo $?             # Exit-Code prüfen
```

### Tests

```bash
make test           # Alle Unit-Tests (FPCUnit)
make e2e            # End-to-End Smoke-Tests
```

---

## Architektur

```
Quellcode (.lyx)
      |
  [ Lexer ]         Tokenizer -> TToken-Stream
      |
  [ Parser ]        Recursive-Descent -> AST
      |
  [ Sema ]          Semantische Analyse (Scopes, Typen)
      |
  [ IR Lowering ]   AST -> 3-Address-Code IR
      |
  [ x86_64 Emit ]   IR -> Maschinencode (Bytes)
      |
  [ ELF64 Writer ]  Code + Data -> ausführbares Binary
      |
  Executable (ELF64, statisch, ohne libc)
```

### Projektstruktur

```
lyxc.lpr                  Hauptprogramm
frontend/
  lexer.pas                 Tokenizer
  parser.pas                Recursive-Descent Parser
  ast.pas                   AST-Knotentypen
  sema.pas                  Semantische Analyse
ir/
  ir.pas                    IR-Knotentypen (3-Address-Code)
  lower_ast_to_ir.pas       AST -> IR Transformation
backend/
  x86_64/
    x86_64_emit.pas         x86_64 Instruktions-Encoding
  elf/
    elf64_writer.pas        ELF64 Binary-Writer
util/
  diag.pas                  Diagnostik (Fehler mit Zeile/Spalte)
  bytes.pas                 TByteBuffer (Byte-Encoding + Patching)
tests/                      FPCUnit-Tests
examples/                   Beispielprogramme in Lyx
```

### Design-Prinzipien

- **Frontend/Backend-Trennung**: Kein x86-Code im Frontend, keine AST-Knoten im Backend.
- **IR als Stabilitätsanker**: Die Pipeline ist immer AST -> IR -> Maschinencode.
- **ELF64 ohne libc**: `_start` ruft `main()`, danach `sys_exit`. Kein Linking gegen externe Libraries.
- **Builtins eingebettet**: `print_str`, `print_int`, `print_float`, `strlen` und `exit` werden als Runtime-Snippets direkt ins Binary geschrieben.
- **Jedes Token trägt SourceSpan**: Fehlermeldungen enthalten immer Datei, Zeile und Spalte.

---

## Editor-Hervorhebung & Grammatik

- Eine initiale TextMate/VSCode‑Grammatik liegt im Repo unter `syntaxes/lyx.tmLanguage.json`. Sie deckt Keywords, Typen, Literale, Kommentare und grundlegende Konstrukte ab und wird iterativ verfeinert.
- Kurzfristig wird ein pragmatisches Mapping per `.gitattributes` genutzt, damit GitHub‑Highlighting sofort sichtbar ist: `*.lyx linguist-language=Rust` (Fallback).
- Ziel: Contribution zur GitHub‑Linguist‑Bibliothek mit einer finalen TextMate‑Grammatik, damit `.lyx`‑Dateien nativ auf GitHub hervorgehoben werden.

Hinweis zum Testen lokal (VSCode)
- Öffne das Repo in VSCode und nutze den "Extension Development Host", um `syntaxes/aurum.tmLanguage.json` zu laden.
- Mit "Developer: Inspect TM Scopes" kannst du Token‑Scopes prüfen.

## Grammatik (EBNF)

Die vollständige formale Grammatik befindet sich in [`ebnf.md`](ebnf.md).

```ebnf
Program     := { TopDecl } ;
TopDecl     := FuncDecl | ConDecl ;
ConDecl     := 'con' Ident ':' Type ':=' ConstExpr ';' ;
FuncDecl    := 'fn' Ident '(' [ ParamList ] ')' [ ':' RetType ] Block ;
Block       := '{' { Stmt } '}' ;
Stmt        := VarDecl | LetDecl | CoDecl | AssignStmt
             | IfStmt | WhileStmt | ReturnStmt | ExprStmt | Block ;
Expr        := OrExpr ;
OrExpr      := AndExpr { '||' AndExpr } ;
AndExpr     := CmpExpr { '&&' CmpExpr } ;
CmpExpr     := AddExpr [ CmpOp AddExpr ] ;
AddExpr     := MulExpr { ( '+' | '-' ) MulExpr } ;
MulExpr     := UnaryExpr { ( '*' | '/' | '%' ) UnaryExpr } ;
UnaryExpr   := ( '!' | '-' ) UnaryExpr | Primary ;
Primary     := IntLit | FloatLit | BoolLit | StringLit | ArrayLit
             | Ident | Call | IndexAccess | '(' Expr ')' ;
ArrayLit    := '[' [ Expr { ',' Expr } ] ']' ;
IndexAccess := Primary '[' Expr ']' ;
FloatLit    := [0-9]+ '.' [0-9]+ ;
```

---

## Roadmap

| Version | Features |
|---------|----------|
| **v0.0.1** | `print_str("...")`, `exit(n)`, ELF64 läuft |
| **v0.0.2** | Integer-Ausdrücke, `print_int(expr)` |
| **v0.1.2** | `var`, `let`, `co`, `con`, `if`, `while`, `return`, Funktionen, SysV ABI |
| **v0.1.3** | ✅ Float-Literale (`f32`, `f64`), ✅ Arrays (Literale, Indexing, Zuweisung) |
| **v0.1.4** | ✅ SO-Library Integration, ✅ Dynamic ELF, ✅ PLT/GOT, ✅ Extern Functions, ✅ Varargs |
| **v0.2** | Erweiterte Funktionen, bessere Diagnostik |
| **v1** | Module/Imports, Objectfiles, Linker-Ansteuerung |

---

## Lizenz

Copyright (c) 2026 Andreas Röne. Alle Rechte vorbehalten.
