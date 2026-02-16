# DATATYPES.md – Aurum Datentypen-Übersicht

## Aktuell unterstützte Datentypen

### 1. Ganzzahl-Typen (signiert)

| Typ     | Bits | Wertebereich                    | Status | Literale    |
|---------|------|---------------------------------|--------|-------------|
| `int8`  | 8    | -128 bis 127                   | ✅ Full | `42i8`      |
| `int16` | 16   | -32,768 bis 32,767             | ✅ Full | `42i16`     |
| `int32` | 32   | -2,147,483,648 bis 2,147,483,647| ✅ Full | `42i32`     |
| `int64` | 64   | -9,223,372,036,854,775,808 bis...| ✅ Full | `42` (std)  |
| `int`   | 64   | Alias für `int64`              | ✅ Full | `42`        |

### 2. Ganzzahl-Typen (unsigniert)

| Typ      | Bits | Wertebereich           | Status | Literale    |
|----------|------|------------------------|--------|-------------|
| `uint8`  | 8    | 0 bis 255             | ✅ Full | `42u8`      |
| `uint16` | 16   | 0 bis 65,535          | ✅ Full | `42u16`     |
| `uint32` | 32   | 0 bis 4,294,967,295   | ✅ Full | `42u32`     |
| `uint64` | 64   | 0 bis 18,446,744,073,709,551,615 | ✅ Full | `42u64` |

### 3. Plattform-abhängige Typen

| Typ     | Beschreibung              | Status | Anmerkung           |
|---------|---------------------------|--------|---------------------|
| `isize` | Pointer-Größe (signiert)  | ⚠️ Partial | Definiert, nicht getestet |
| `usize` | Pointer-Größe (unsigniert)| ⚠️ Partial | Definiert, nicht getestet |

### 4. Fließkomma-Typen

| Typ   | Bits | IEEE 754 | Status | Literale |
|-------|------|----------|--------|----------|
| `f32` | 32   | single   | ✅ **REPARIERT** | `3.14` ✅ |
| `f64` | 64   | double   | ✅ **REPARIERT** | `3.14` ✅ |

**Status**: Lexer, Parser, AST und Sema vollständig implementiert. IR-Lowering ist Placeholder (gibt Dummy-Wert 0 zurück).

### 5. Zeichen- und String-Typen

| Typ     | Beschreibung            | Status | Literale |
|---------|-------------------------|--------|----------|
| `char`  | Ein ASCII-Zeichen       | ✅ **REPARIERT** | `'a'` ✅ |
| `pchar` | Null-terminierter String| ✅ Full | `"hello"` |
| `string`| Alias für `pchar`       | ✅ Full | `"hello"` |

### 6. Sonstige Typen

| Typ    | Beschreibung            | Status | Verwendung |
|--------|-------------------------|--------|------------|
| `bool` | Wahrheitswert           | ✅ Full | `true`, `false` |
| `void` | Kein Rückgabewert       | ✅ Full | Funktionen ohne Return |

### 7. Interne Typen

| Typ            | Verwendung                  |
|----------------|-----------------------------|
| `atUnresolved` | Temporär während Typprüfung |

## Status-Legende

- ✅ **Full**: Vollständig implementiert (Parser, Sema, IR, Codegen)
- ⚠️ **Partial**: Teilweise implementiert (fehlende Komponenten)
- ❌ **Missing**: Definiert aber nicht implementiert
- 🔄 **WIP**: Work in Progress

## Implementierungsdetails

### Storage-Klassen-Kompatibilität

| Storage-Klasse | Beschreibung           | Status (HEAD) |
|----------------|------------------------|---------------|
| `var`          | Veränderbar            | ✅ Alle Integer-Typen + bool |
| `let`          | Unveränderbar          | ✅ Alle Integer-Typen + bool |
| `co`           | Compile-time readonly  | ✅ Funktionsfähig |
| `con`          | Compile-time constant  | ✅ Repariert und funktionsfähig |

**Getestet und funktionsfähig:**
- `var` und `let` mit: int8, uint8, int16, uint16, int32, uint32, int64, uint64, bool
- Funktionsrückgabe für alle oben genannten Typen
- Typkonvertierung zwischen Integer-Typen funktioniert automatisch

### Typkonvertierung

- **Automatisch**: Zwischen Integer-Typen verschiedener Breiten
- **Explizit**: Mit Cast-Operatoren (noch nicht implementiert)
- **Konstanten-Folding**: Bei Literalen auf Zieltyp-Breite

### Code-Generation-Status

#### Vollständig implementiert:
- Integer-Typen (8-64 Bit, signed/unsigned)
- `bool`, `char`, `pchar`
- Konstanten-Folding mit Truncation/Extension
- Load/Store mit korrekter Breite

#### Teilweise implementiert:
- `f32`, `f64`: Parser OK, Codegen fehlt
- `isize`, `usize`: Definition OK, Tests fehlen

#### Fehlend:
- Strukturen/Records
- Arrays (dynamisch)
- Pointer-Arithmetik
- Union-Typen

## Status-Update: HEAD-Version (0d50afd) ✅ REPARIERT

### 🎉 **Erfolgreich behoben:**
- ✅ **Syntaxfehler**: 13+ fehlende `end;` Statements in `lower_ast_to_ir.pas` behoben
- ✅ **con-Keyword**: Parser-Bug repariert, `con` funktioniert jetzt 
- ✅ **Bracket-Tokens**: `[` und `]` Tokens zum Lexer hinzugefügt
- ✅ **Kompilierung**: HEAD-Version kompiliert ohne Fehler

### 📊 **Alle Tests bestanden (HEAD-Version):**
- Integer-Typen: int8, uint8, int16, uint16, int32, uint32, int64, uint64 ✅
- Boolean-Typ: true/false Literale ✅
- **Char-Typ: 'x' Literale + Escape-Sequenzen ✅**
- **Float-Typen: f32, f64 mit 3.14 Literalen ✅**
- **Array-Literale: [1, 2, 3] Syntax mit vollständiger Typprüfung ✅ NEU**
- Storage-Klassen: var, let, co, con (für alle primitiven Typen) ✅
- Funktionsrückgabewerte für alle oben genannten Typen + Array-Literale ✅
- Typkonvertierung: Integer ↔ Integer, Char → Integer, Float ↔ Float ✅
- Array-Element-Typ-Konsistenz: Mixed-Type-Fehler-Erkennung ✅

## Bekannte Einschränkungen (Stand: HEAD repariert)

### 1. ✅ Char-Literale (REPARIERT)
- **Problem**: ~~Single-Quote-Literale (`'a'`) werden vom Lexer nicht erkannt~~ **BEHOBEN**
- **Lösung**: Vollständige Char-Literal-Implementierung hinzugefügt
- **Status**: ✅ **Vollständig funktionsfähig**
  - Lexer: `tkCharLit` Token hinzugefügt ✅
  - Parser: `TAstCharLit` AST-Knoten implementiert ✅ 
  - Sema: `atChar` Typ + Typkonvertierung zu Integer ✅
  - IR: Char-zu-ASCII-Code Konvertierung ✅
  - Escape-Sequenzen: `\n`, `\t`, `\r`, `\\`, `\'`, `\0` ✅

### 2. ✅ Fließkomma-Typen (REPARIERT)
- **Problem**: ~~f32/f64 Literale (`3.14`) werden vom Lexer nicht erkannt~~ **BEHOBEN**
- **Lösung**: Vollständige Float-Literal-Implementierung hinzugefügt
- **Status**: ✅ **Frontend vollständig funktionsfähig**
  - Lexer: `tkFloatLit` Token + Punkt-Notation-Parsing ✅
  - Parser: `TAstFloatLit` AST-Knoten implementiert ✅
  - Sema: `atF32`/`atF64` Typen + Typkompatibilität ✅
  - Alle Float-Formate: `0.1`, `3.14`, `999.999` etc. ✅
  - Storage-Klassen: var, let, co mit f32/f64 ✅
  - Funktionsrückgabe: f32/f64 als Return-Typen ✅
- **Einschränkung**: IR-Backend gibt noch Dummy-Werte zurück (echte Float-Codegen TODO)

### 3. ✅ Array-Literale (REPARIERT)
- **Problem**: ~~Statische Array-Syntax `[1, 2, 3]` nicht im Parser implementiert~~ **BEHOBEN**
- **Lösung**: Vollständige Array-Literal-Implementierung hinzugefügt
- **Status**: ✅ **Frontend vollständig funktionsfähig**
  - Lexer: `[` und `]` Tokens bereits vorhanden ✅
  - Parser: `ParseArrayLiteral()` Funktion implementiert ✅
  - AST: `TAstArrayLit` Knoten mit Element-Liste ✅
  - Sema: Vollständige Typprüfung + Element-Typ-Konsistenz ✅
  - Alle Formate: `[1, 2, 3]`, `['a', 'b']`, `[3.14, 2.718]` ✅
  - Typfehler-Erkennung: Mixed-Type Arrays werden erkannt ✅
  - Return-Typ-Matching funktioniert ✅
- **Einschränkung**: IR-Backend gibt noch Dummy-Werte zurück (echte Array-Codegen TODO)

### 4. Platform-Types
- **Problem**: `isize`/`usize` definiert aber ungetestet
- **Auswirkung**: Möglicherweise nicht funktional
- **Status**: Unklarer Implementierungsstand

## Test-Abdeckung (Stand: Februar 2026)

### ✅ Vollständig getestet und funktionsfähig:
- **Integer-Typen**: int8, uint8, int16, uint16, int32, uint32, int64, uint64
  - Als Variablen (`var`, `let`)
  - Als Funktionsrückgabewerte
  - Automatische Typkonvertierung
  - Tests: `test_simple_returns.au`, `test_basic_storage.au`

- **Boolean-Typ**: `bool`
  - Mit `var` und `let` 
  - Als Funktionsrückgabe (true/false Literale)

- **String-Typ**: `pchar`/`string`
  - String-Literale (`"hello"`) funktionieren
  - Als Funktionsparameter für `print_str`

### ⚠️ Definiert aber fehlerhaft:
- **char**: Typ definiert, aber Lexer erkennt `'x'` Literale nicht
- **f32/f64**: In neueren Versionen definiert, in e40795c nicht verfügbar
- **isize/usize**: Definiert aber ungetestet

### ❌ Nicht getestet:
- Edge-Cases bei Integer-Overflow/Underflow  
- Sehr große Integer-Literale (> int64 range)
- Plattform-abhängige Typen (isize/usize auf verschiedenen Architekturen)

## Roadmap

### 🔥 Kritische Frontend-Features:
1. ✅ ~~Aktuelle Version (0d50afd) Syntaxfehler beheben~~ **ERLEDIGT**
2. ✅ ~~co/con Keywords wieder aktivieren~~ **ERLEDIGT**  
3. ✅ ~~Char-Literal-Lexer reparieren (`'x'` Syntax)~~ **ERLEDIGT**
4. ✅ ~~Fließkomma-Lexer implementieren (`3.14` Syntax)~~ **ERLEDIGT**
5. ✅ ~~Array-Literal-Parser implementieren (`[1, 2, 3]` Syntax)~~ **ERLEDIGT**

### 🔧 Backend-Implementierungen (nächste Phase):
1. **Float-IR-Backend** implementieren (echte Float-Codegen)
2. **Array-IR-Backend** implementieren (echte Array-Codegen)
3. **Array-Typ-Deklarationen** erweitern (`int64[3]` Variablen)

### 📋 Kurzfristig (nächste Commits):
1. Fließkomma-Code-Generation (f32/f64) vervollständigen
2. isize/usize Tests schreiben und validieren
3. Edge-Case-Tests für Integer-Overflow

### 📈 Mittelfristig:
1. Explizite Cast-Operatoren (`x as int32`)  
2. Strukturen/Records
3. Statische Arrays (vollständige Implementierung)

### 🚀 Langfristig:
1. Generics/Templates
2. Union-Typen  
3. Smart-Pointer

### 📊 Empfohlene Arbeitsreihenfolge:
1. **Version-Stabilisierung**: Aktuelle HEAD-Version reparieren
2. **Char-Support**: Lexer erweitern für `'x'` Literale
3. **Vollständige Tests**: Alle Datentyp-Kombinationen testen

## Beispiele

```aurum
// Alle Integer-Typen
var a: int8 := 127;
let b: uint16 := 65535;
co c: int32 := 1000000;
con d: int64 := 9223372036854775807;

// Fließkomma (Parser OK, Codegen TODO)
var pi: f32 := 3.14159;
let e: f64 := 2.718281828;

// Zeichen und Strings
var ch: char := 'X';
let msg: pchar := "Hello, World!";

// Boolean
var flag: bool := true;
let result: bool := (a > 0);

// Funktionsrückgabe mit verschiedenen Typen
fn get_byte(): uint8 { return 255; }
fn get_flag(): bool { return true; }
fn do_nothing(): void { print_str("done"); }
```