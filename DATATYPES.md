# DATATYPES.md – Lyx Datentypen-Übersicht

## Aktuell unterstützte Datentypen

### 1. Ganzzahl-Typen (signiert)

| Typ     | Bits | Wertebereich                                               | Status | Literale    |
|---------|------|------------------------------------------------------------|--------|-------------|
| `int8`  | 8    | -128 bis 127                                               | ✅ Full | `42i8`      |
| `int16` | 16   | -32,768 bis 32,767                                         | ✅ Full | `42i16`     |
| `int32` | 32   | -2,147,483,648 bis 2,147,483,647                           | ✅ Full | `42i32`     |
| `int64` | 64   | -9,223,372,036,854,775,808 bis 9,223,372,036,854,775,807  | ✅ Full | `42` (std)  |
| `int`   | 64   | Alias für `int64`                                          | ✅ Full | `42`        |

### 2. Ganzzahl-Typen (unsigniert)

| Typ      | Bits | Wertebereich                                          | Status | Literale    |
|----------|------|-------------------------------------------------------|--------|-------------|
| `uint8`  | 8    | 0 bis 255                                             | ✅ Full | `42u8`      |
| `uint16` | 16   | 0 bis 65,535                                          | ✅ Full | `42u16`     |
| `uint32` | 32   | 0 bis 4,294,967,295                                   | ✅ Full | `42u32`     |
| `uint64` | 64   | 0 bis 18,446,744,073,709,551,615                     | ✅ Full | `42u64`     |

### 3. Plattform-abhängige Typen

| Typ     | Beschreibung                  | Status    | Anmerkung                         |
|---------|-------------------------------|-----------|-----------------------------------|
| `isize` | Pointer-Größe (signiert)      | ⚠️ Partial | Typ definiert; Tests/ABI prüfen   |
| `usize` | Pointer-Größe (unsigniert)    | ⚠️ Partial | Typ definiert; Tests/ABI prüfen   |

### 4. Fließkomma-Typen

| Typ   | Bits | IEEE 754 | Status | Literale |
|-------|------|----------|--------|----------|
| `f32` | 32   | single   | ✅ Full | `3.14f32` / `3.14` |
| `f64` | 64   | double   | ✅ Full | `3.14` / `3.14f64` |

> Status: Frontend (Lexer, Parser, AST, Sema), IR-Lowering und grundlegende Codegen‑Pfad für f32/f64 sind implementiert. Feinheiten der optimierten Float‑Codegenerierung können noch erweitert werden.

### 5. Zeichen- und String-Typen

| Typ     | Beschreibung               | Status | Literale |
|---------|----------------------------|--------|----------|
| `char`  | Einzelnes Zeichen (ASCII/Unicode codepoint) | ✅ Full | `'a'`, Escape-Sequenzen |
| `pchar` | Null-terminierter String   | ✅ Full | `"hello"` |
| `string`| Alias für `pchar`          | ✅ Full | `"hello"` |

### 6. Sonstige Typen

| Typ    | Beschreibung            | Status | Verwendung |
|--------|-------------------------|--------|------------|
| `bool` | Wahrheitswert           | ✅ Full | `true`, `false` |
| `void` | Kein Rückgabewert       | ✅ Full | Funktionen ohne Return |

### 7. Interne Typen

| Typ            | Verwendung                                 |
|----------------|--------------------------------------------|
| `atUnresolved` | Temporär während Typprüfung                 |

## Status-Legende

- ✅ **Full**: Vollständig implementiert (Parser, Sema, IR, Codegen)
- ⚠️ **Partial**: Teilweise implementiert (fehlende Komponenten oder Tests)
- ❌ **Missing**: Definiert, aber nicht implementiert
- 🔄 **WIP**: Work in Progress

## Implementierungsdetails

### Storage-Klassen-Kompatibilität

| Storage-Klasse | Beschreibung           | Status (HEAD) |
|----------------|------------------------|---------------|
| `var`          | Veränderbar            | ✅ Unterstützt für alle primitiven Typen |
| `let`          | Unveränderbar          | ✅ Unterstützt für alle primitiven Typen |
| `co`           | Compile-time readonly  | ✅ Unterstützt |
| `con`          | Compile-time constant  | ✅ Unterstützt |

**Getestet und funktionsfähig (Frontend + IR + grundlegende Codegen):**
- Integer-Typen (int8..int64, uint8..uint64)
- Boolean (`bool`)
- Char (`char`) inkl. Escape‑Sequenzen
- Strings (`pchar`/`string`) mit Literalunterstützung
- Floating-Point (f32, f64): Literal‑Parsing, Typprüfung, Konvertierungen und Basis‑Codegen
- Array‑Literal‑Parsing und elementare Load/Store-Operationen (Frontend und grundlegender Backend‑Support)

### Typkonvertierung

- **Automatisch**: Zwischen Integer‑Typen verschiedener Breiten
- **Explizit**: Mit Casts (teilweise implementiert)
- **Konstanten‑Folding**: Literale werden beim IR/Codegen auf Zielbreite behandelt

### Code-Generation-Status

#### Vollständig implementiert:
- Integer-Typen (8–64 Bit, signed/unsigned)
- `bool`, `char`, `pchar`, `string`
- Basis‑Floating‑Point‑Operationen (Loads/Stores, cvt, movsd/movss) und Konversionen
- Konstanten‑Folding mit Truncation/Extension
- Load/Store mit korrekter Breite

#### Teilweise implementiert:
- `isize`, `usize`: Typen sind definiert; ABI/Architekturtests fehlen
- Arrays: Frontend (Literals, Typprüfung) ist vollständig; komplexere Array‑Codegen (statische Layouts, Slicing, dynamische Allokation) ist noch in Arbeit

#### Fehlend / noch zu erweitern:
- Strukturen/Records (vollständiger Speicherlayout‑Support)
- Pointer‑Arithmetik (feinere Operationen)
- Union‑Typen

## Status-Update: HEAD (aktueller Stand)

Die aktuellen Änderungen haben folgende Lücken geschlossen und Features hinzugefügt:

- Frontend: Char‑ und Float‑Literal‑Lexing/Parsing implementiert
- IR/Backend: Grundlegende Float‑Operationen (cvtsi2sd, cvttsd2si, movsd) und Array Load/Store-Emissionen implementiert
- Platform‑Types (`isize`/`usize`) sind als Typen vorhanden; Tests/ABI‑Überprüfung stehen noch aus

## Test-Abdeckung (Stand: Februar 2026)

### ✅ Getestet / grün:
- Integer‑Primitiven (int8..int64, uint8..uint64)
- Boolean (`bool`)
- Char‑Literale und Escape‑Sequenzen
- String‑Literale (`pchar`/`string`) in Kombination mit Builtins (z.B. print_str)
- Float‑Literals (f32, f64) — Parsing, Sema, Basiscodierung
- Array‑Literal‑Parsing und elementare Load/Store im Backend

### ⚠️ Zu verifizieren:
- `isize`/`usize` auf mehreren Architekturen
- Edge‑Cases bei Integer‑Overflow/Underflow
- Vollständiger Float‑Codegen (optimierte Sequenzen, ABI‑Konventionen für float‑Returns in SSE regs)

## Roadmap (aktualisiert)

### Kurzfristig
1. Vollständige Float‑Codegen (Rounding, ABI‑Returns in XMM) abschließen
2. Tests für `isize`/`usize` hinzufügen und ABI‑Konformität prüfen
3. Erweiterte Array‑Codegen (statische Arrays, Layouts)

### Mittelfristig
1. Strukturen/Records implementieren (Layout + Feldzugriff)
2. Pointer‑Arithmetik und dereferenzierung vervollständigen

### Langfristig
1. Generics/Templates
2. Union‑Typen
3. Smart‑Pointer

## Beispiele

```lyx
// Integer-Typen
var a: int8 := 127;
let b: uint16 := 65535;

// Floats
var pi: f32 := 3.14159;
let e: f64 := 2.718281828;

// Char & String
var ch: char := '\n';
let msg: pchar := "Hello, World!";

// Arrays (Literal + einfache Load/Store)
var arr := [1, 2, 3];
let first := arr[0];

// Function signatures
fn get_byte(): uint8 { return 255; }
fn get_flag(): bool { return true; }
```
