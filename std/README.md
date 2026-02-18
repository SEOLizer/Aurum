Standard-Units (std)
=====================

Dieses Verzeichnis enthält standardisierte Units, die als umfassende Bibliothek für Lyx-Programme dienen. Die Libraries kombinieren native Builtin-Funktionen mit ergonomischen Wrapper-Funktionen und erweiterten Utilities.

## Vollständig implementiert

### std/math.lyx
**Integer-Mathematik (ergonomische Wrappers):**
- `abs64(x: int64): int64`
- `min64(a: int64, b: int64): int64` 
- `max64(a: int64, b: int64): int64`
- `div64(a: int64, b: int64): int64`
- `mod64(a: int64, b: int64): int64`
- `times_two(x: int64): int64`

*Hinweis: Zusätzlich stehen 22 native Math-Builtins zur Verfügung (siehe Haupt-README).*

### std/io.lyx  
**I/O-Convenience-Funktionen:**
- `print(s: pchar): void` - Wrapper für `print_str`
- `println(s: pchar): void` - Print mit automatischem Newline
- `print_intln(x: int64): void` - Print Integer mit Newline  
- `exit_proc(code: int64): void` - Wrapper für `exit`

### std/string.lyx ⭐ **NEU**
**Umfassende String-Manipulation (20+ Funktionen):**

#### Basis-String-Operations (nutzt native Builtins):
- `str_copy(dest: pchar, src: pchar): pchar`
- `strcmp(a: pchar, b: pchar): int64` (Kompatibilität)
- `strcpy(dest: pchar, src: pchar): pchar` (Kompatibilität)

#### String-Suche und -Tests:
- `str_find(haystack: pchar, needle: pchar): int64`
- `str_safe_char_at(s: pchar, index: int64): int64`
- `str_equals(s1: pchar, s2: pchar): bool`
- `str_starts_with(s: pchar, prefix: pchar): bool`
- `str_ends_with(s: pchar, suffix: pchar): bool`

#### Case-Konvertierung:
- `char_to_lower(c: int64): int64`
- `char_to_upper(c: int64): int64` 
- `str_to_lower(dest: pchar, src: pchar): pchar`
- `str_to_upper(dest: pchar, src: pchar): pchar`

#### String-Manipulation:
- `str_concat(dest: pchar, s1: pchar, s2: pchar): pchar`
- `str_reverse(s: pchar): pchar` 
- `str_trim_whitespace(dest: pchar, src: pchar): pchar`
- `is_whitespace(c: int64): bool`

*Native String-Builtins (`str_length`, `str_char_at`, `str_set_char`, `str_compare`, `str_copy_builtin`) sind direkt ohne Import verfügbar.*

### std/env.lyx
**Environment und Command-Line API:**
- `init(argc: int64, argv: pchar): void` - Explizite env-Initialisierung (optional)
- `arg_count(): int64` - Anzahl Command-Line-Argumente
- `arg(i: int64): pchar` - Zugriff auf Argument i

### std/geo.lyx ⭐ **NEU** 
**Geolocation-Parser für offline GPS-Daten:**
- `parse_lat(s: pchar): int64` - Parst Decimal Degrees zu Microdegrees
- `parse_lon(s: pchar): int64` - Parst Longitude (gleiche Implementierung)

*Konvertiert GPS-Koordinaten wie "52.520008" zu Microdegrees (52520008) für präzise Integer-Arithmetik.*

### std/time.lyx
**Datums- und Zeit-Berechnungen:**
- `is_leap_year(y: int64): bool` - Schaltjahr-Prüfung
- `days_from_civil(y,m,d): int64` - Tage seit Epoche
- `civil_year_from_days(days): int64` - Jahr aus Tage-Zahl
- `civil_month_from_days(days): int64` - Monat aus Tage-Zahl  
- `civil_day_from_days(days): int64` - Tag aus Tage-Zahl
- `day_of_year(y,m,d): int64` - Tag des Jahres (1-366)
- `weekday(y,m,d): int64` - Wochentag (0=Montag...6=Sonntag)
- `iso_week(y,m,d): int64` - ISO-Kalenderwoche
- `iso_year(y,m,d): int64` - ISO-Jahr
- *(format_unix Funktionen sind Platzhalter für zukünftige Versionen)*

## Verwendung

### Basis-Import-Beispiel:

```lyx
import std.math;
import std.io;
import std.string;
import std.env; // optional

fn main(argc: int64, argv: pchar): int64 {
  // env.init ist optional - automatische Initialisierung seit v0.1.5
  print_intln(arg_count());
  println(arg(0));
  
  return 0;
}
```

### String-Library Beispiel:

```lyx
import std.string;

fn process_text(input: pchar): void {
  var len: int64 := str_length(input);      // Native Builtin
  print_str("Text length: ");
  print_int(len);
  print_str("\n");
  
  // Case conversion  
  var lower_buffer: pchar := "                    "; // 20 chars
  str_to_lower(lower_buffer, input);
  println(lower_buffer);
  
  // String search
  var pos: int64 := str_find(input, "test");
  if (pos >= 0) {
    print_str("Found 'test' at position: ");
    print_int(pos);
    print_str("\n");
  }
  
  // Character access
  if (len > 0) {
    var first: int64 := str_char_at(input, 0);
    print_str("First character code: ");
    print_int(first);
    print_str("\n");
  }
}
```

### Geolocation-Parser Beispiel:

```lyx  
import std.geo;
import std.io;

fn parse_coordinates(): void {
  var lat_str: pchar := "52.520008";    // Berlin
  var lon_str: pchar := "13.404954";    
  
  var lat_micro: int64 := parse_lat(lat_str);   // 52520008 microdegrees
  var lon_micro: int64 := parse_lon(lon_str);   // 13404954 microdegrees
  
  print_str("Latitude (microdegrees): ");
  print_int(lat_micro);
  print_str("\n");
  
  print_str("Longitude (microdegrees): ");  
  print_int(lon_micro);
  print_str("\n");
}
```

### Math + String Kombination:

```lyx
import std.math;
import std.string;

fn analyze_numbers(): void {
  var numbers: array := [42, -17, 99];
  
  var i: int64 := 0;
  while (i < 3) {
    var num: int64 := numbers[i];
    var abs_val: int64 := abs64(num);           // std/math wrapper
    var native_abs: int64 := abs(num);          // Native builtin
    
    // String conversion
    var str_val: pchar := int_to_str(num);      // Native builtin
    
    print_str("Number: ");
    print_str(str_val);
    print_str(", Absolute: ");
    print_int(abs_val);
    print_str("\n");
    
    i := i + 1;
  }
}
```

## Native Builtins vs Standard-Library

Lyx bietet **zwei Schichten** für häufig verwendete Funktionen:

### 1. Native Builtins (ohne Import)
Diese sind direkt im Compiler integriert und benötigen **keinen Import**:
- **String**: `str_length`, `str_char_at`, `str_set_char`, `str_compare`, `str_copy_builtin`
- **String-Konvertierung**: `int_to_str`, `str_to_int`  
- **Math**: `abs`, `odd`, `sqrt`, `round`, `pi`, `sin`, `cos`, etc. (22 Funktionen)
- **I/O**: `print_str`, `print_int`, `print_float`, `exit`

### 2. Standard-Library (mit Import)
Diese bauen auf den Builtins auf und bieten **ergonomische Erweiterungen**:
- **std/string**: String-Utilities, Case-Conversion, Search-Functions
- **std/math**: Convenience-Wrappers mit deskriptiveren Namen
- **std/io**: Print-Funktionen mit automatischen Newlines
- **std/env**: Command-Line-Argument-Zugriff
- **std/geo**: Spezielle GPS-Coordinate-Parser
- **std/time**: Datums-Berechnungen

**Beispiel - beide Schichten kombiniert:**
```lyx
import std.string;   // Import für erweiterte String-Funktionen

fn demo(): void {
  var text: pchar := "Hello World";
  
  // Native Builtin (kein Import nötig)
  var len: int64 := str_length(text);
  
  // Standard-Library Funktion (benötigt Import)
  var lower: pchar := "           ";  // Buffer
  str_to_lower(lower, text);         // "hello world"
}
```

## Hinweise

### env-Initialisierung
- Der Compiler initialisiert seit **v0.1.5** automatisch beim Programstart die interne env-Datenstruktur (argc/argv)
- Explizites `init(argc, argv)` ist **optional** und nur für explizite Kontrolle nötig
- Die std/env Unit bleibt vollständig abwärtskompatibel

### Performance
- **Native Builtins** sind direkt in Assembly implementiert → maximale Performance
- **Standard-Library** Funktionen sind in Lyx geschrieben → moderate Performance, aber mehr Features

## Tests und Beispiele

- **Beispiele**: `examples/` Verzeichnis enthält umfassende Anwendungsfälle
  - `test_string_builtins.lyx` - Native String-Builtins
  - `test_string_operations.lyx` - std/string Library  
  - `test_math_builtins.lyx` - Native Math-Funktionen
  - `use_math.lyx`, `use_io.lyx`, `use_env.lyx` - Standard-Libraries
- **CI-Integration**: Automatische Build- und Ausführungs-Tests für alle Standard-Units
- **Unit-Tests**: FPCUnit-Tests im `tests/` Verzeichnis prüfen Core-Funktionalität
