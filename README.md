# Aurum

**Aurum** ist ein nativer Compiler für die gleichnamige Programmiersprache, geschrieben in FreePascal.
Er erzeugt direkt ausführbare **Linux x86_64 ELF64-Binaries** — ohne libc, ohne Linker, rein über Syscalls.

```
Aurum Compiler v0.1.0
Copyright (c) 2026 Andreas Röne. Alle Rechte vorbehalten.
```

---

## Schnellstart

```bash
# Compiler bauen
make build

# Programm kompilieren und ausführen
./aurumc examples/hello.au -o hello
./hello
```

```
Hello Aurum
```

---

## Die Sprache Aurum

Aurum ist **prozedural** und **statisch typisiert** — inspiriert von C und Rust, mit einer eigenen, kompakten Syntax.

### Hello World

```aurum
fn main(): int64 {
  print_str("Hello Aurum\n");
  return 0;
}
```

### Variablen und Arithmetik

Vier Speicherklassen steuern Veränderbarkeit und Lebensdauer:

```aurum
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

```aurum
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

### Typen

| Typ     | Beschreibung                          |
|---------|---------------------------------------|
| `int64` | Signed 64-bit Integer                 |
| `bool`  | `true` / `false`                      |
| `void`  | Nur als Funktions-Rückgabetyp         |
| `pchar` | Pointer auf nullterminierte Bytes     |

Keine impliziten Casts — alle Typen müssen explizit übereinstimmen.

### Operatoren

| Priorität | Operatoren           | Beschreibung              |
|:---------:|----------------------|---------------------------|
| 1 (niedrig) | `\|\|`            | Logisches Oder            |
| 2         | `&&`                 | Logisches Und             |
| 3         | `==` `!=` `<` `<=` `>` `>=` | Vergleich (liefert `bool`) |
| 4         | `+` `-`              | Addition, Subtraktion     |
| 5         | `*` `/` `%`          | Multiplikation, Division, Modulo |
| 6 (hoch)  | `!` `-` (unär)       | Logisches NOT, Negation   |

Zuweisung erfolgt mit `:=` (nicht `=`).

### Kontrollfluss

#### if / else

```aurum
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

```aurum
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

```aurum
fn main(): int64 {
  var active: bool := true;
  if (active)
    print_str("aktiv\n");
  return 0;
}
```

### Funktionen

Funktionen sind global, folgen der SysV ABI (Parameter in Registern) und unterstützen bis zu 6 Parameter:

```aurum
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

```aurum
fn greet() {
  print_str("Hallo!\n");
}

fn main(): int64 {
  greet();
  return 0;
}
```

### Builtins

Drei eingebaute Funktionen stehen ohne Import zur Verfügung:

| Funktion          | Signatur               | Beschreibung                        |
|-------------------|------------------------|-------------------------------------|
| `print_str(s)`    | `pchar -> void`        | Gibt String bis `\0` aus            |
| `print_int(x)`    | `int64 -> void`        | Gibt Integer als Dezimalzahl aus    |
| `exit(code)`      | `int64 -> void`        | Beendet das Programm mit Exit-Code  |

```aurum
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

```aurum
// Zeilenkommentar

/* Blockkommentar
   über mehrere Zeilen */
```

### Reservierte Keywords

```
fn  var  let  co  con  if  else  while  return  true  false  extern
```

`extern` ist für zukünftige Erweiterungen reserviert.

---

## Vollständiges Beispiel

Ein Programm, das alle Kernfeatures kombiniert:

```aurum
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

### Aurum-Programm kompilieren

```bash
./aurumc eingabe.au -o ausgabe
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
Quellcode (.au)
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
aurumc.lpr                  Hauptprogramm
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
examples/                   Beispielprogramme in Aurum
```

### Design-Prinzipien

- **Frontend/Backend-Trennung**: Kein x86-Code im Frontend, keine AST-Knoten im Backend.
- **IR als Stabilitätsanker**: Die Pipeline ist immer AST -> IR -> Maschinencode.
- **ELF64 ohne libc**: `_start` ruft `main()`, danach `sys_exit`. Kein Linking gegen externe Libraries.
- **Builtins eingebettet**: `print_str`, `print_int` und `exit` werden als Runtime-Snippets direkt ins Binary geschrieben.
- **Jedes Token trägt SourceSpan**: Fehlermeldungen enthalten immer Datei, Zeile und Spalte.

---

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
Primary     := IntLit | BoolLit | StringLit | Ident | Call | '(' Expr ')' ;
```

---

## Roadmap

| Version | Features |
|---------|----------|
| **v0.0.1** | `print_str("...")`, `exit(n)`, ELF64 läuft |
| **v0.0.2** | Integer-Ausdrücke, `print_int(expr)` |
| **v0.1** | `var`, `let`, `co`, `con`, `if`, `while`, `return`, Funktionen, SysV ABI |
| **v0.2** | Erweiterte Funktionen, bessere Diagnostik |
| **v1** | Module/Imports, Objectfiles, Linker-Ansteuerung |

---

## Lizenz

Copyright (c) 2026 Andreas Röne. Alle Rechte vorbehalten.
