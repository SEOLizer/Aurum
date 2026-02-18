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

### std/crt ⭐ **NEU**
**ANSI-basierte Console-Utility (Turbo-Pascal-ähnlich)**
- Ziel: Farben, Cursorsteuerung, Bildschirmoperationen mit ANSI-ESC-Sequenzen
- **Keine externen Abhängigkeiten** → statische ELFs möglich

Wichtige API-Funktionen (pub):
- `text_color(c: crt_color)` – Vordergrundfarbe setzen
- `text_background(c: crt_color)` – Hintergrundfarbe setzen
- `text_attr(fg,bg)` – Vorder- und Hintergrund zusammen
- `reset_attr()` – Attribute zurücksetzen
- `clrscr()` – Bildschirm löschen und Cursor nach Home
- `clreol()` – Lösche bis EOL
- `gotoxy(col,row)` – Cursor setzen (1-basiert)
- `hide_cursor()`, `show_cursor()` – Cursor sichtbar/unsichtbar
- `write_str_at(col,row,s)` – Schreibe String an Position

Hinweis: `read_char()` in std/crt ist nur ein dokumentierter Platzhalter (kanonisches/blocking input). Für rohe Terminal-Eingaben siehe std/crt_raw.

### std/crt_raw ⭐ **NEU, experimentell**
**Raw-Mode und Nicht-Blocking Input (termios-basierte Erweiterung)**
- Bietet Prototypen: `set_raw_mode(enabled)`, `key_pressed()`, `read_key_raw()`
- Implementiert über `extern`-Deklarationen (tcgetattr/tcsetattr/read/select/ioctl)
- **WICHTIG**: Verwendung dieser Unit führt zu dynamischen ELFs (libc wird benötigt). Die Unit ist experimentell; termios-Struct-Layout ist plattformabhängig.

## Verwendung (crt)

### ANSI-Demo

```lyx
import std.crt;
import std.io;

fn main(): int64 {
  clrscr();
  text_attr(white, blue);
  write_str_at(1,1," Lyx CRT ANSI Demo ");
  reset_attr();
  text_color(red); gotoxy(5,4); print_str("This is red text");
  return 0;
}
```

### Raw-Mode (experimentell)

```lyx
import std.crt_raw;
import std.crt;

fn main(): int64 {
  if (set_raw_mode(true) == 0) {
    // key_pressed/read_key_raw verwenden
    set_raw_mode(false);
  }
  return 0;
}
```

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

... (rest unchanged)
