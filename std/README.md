Standard-Units (std)
=====================

Dieses Verzeichnis enthält standardisierte Units, die als umfassende Bibliothek für Lyx-Programme dienen. Die Libraries kombinieren native Builtin-Funktionen mit ergonomischen Wrapper-Funktionen und erweiterten Utilities.

## Vollständig implementiert

### std/math.lyx
**Integer-Mathematik (ergonomische Wrappers):**
- `Abs64(x: int64): int64`
- `Min64(a: int64, b: int64): int64` 
- `Max64(a: int64, b: int64): int64`
- `Div64(a: int64, b: int64): int64`
- `Mod64(a: int64, b: int64): int64`
- `TimesTwo(x: int64): int64`

*Hinweis: Zusätzlich stehen 22 native Math-Builtins zur Verfügung (siehe Haupt-README).* 

### std/io.lyx  
**I/O-Convenience-Funktionen:**
- `print(s: pchar): void` - Wrapper für `PrintStr`
- `PrintLn(s: pchar): void` - Print mit automatischem Newline
- `PrintLn(x: int64 | f64 | bool): void` - Überladene Varianten für Zahlen/Booleans
- `PrintIntLn(x: int64): void` - Print Integer mit Newline  
- `ExitProc(code: int64): void` - Wrapper für `exit`

**printf (pure-Lyx)**
- `printf(fmt: pchar, ...)` ist eine reine-Lyx-Implementierung, die ohne libc auskommt.
- Unterstützte Platzhalter: `%s` (pchar), `%d` (int64), `%f` (f64), `%%` → literal `%`.
- Maximal unterstützte Platzhalter pro Aufruf: 4
- Standard-Precision für `%f`: 6 Dezimalstellen.
- Automatische Konvertierung: Wrapper für gängige Kombinationen konvertieren `int64` und `f64` automatisch zu `pchar`.
- Fehlende Argumente werden als leerer String ersetzt.

Beispiel:

```lyx
var s_num: pchar := IntToStr(42);
var s_pi: pchar := FloatToStr(3.1415 as f64, 6);
printf("Formatted: %s = %s, pi=%s\n", "answer", s_num, s_pi);
```

### std/string.lyx ⭐ **NEU**
**Umfassende String-Manipulation (20+ Funktionen):**

#### Basis-String-Operations (nutzt native Builtins):
- `StrCopy(dest: pchar, src: pchar): pchar`
- `strcmp(a: pchar, b: pchar): int64` (Kompatibilität)
- `strcpy(dest: pchar, src: pchar): pchar` (Kompatibilität)

#### String-Suche und -Tests:
- `StrFind(haystack: pchar, needle: pchar): int64`
- `StrSafeCharAt(s: pchar, index: int64): int64`
- `StrEquals(s1: pchar, s2: pchar): bool`
- `StrStartsWith(s: pchar, prefix: pchar): bool`
- `StrEndsWith(s: pchar, suffix: pchar): bool`

#### Case-Konvertierung:
- `CharToLower(c: int64): int64`
- `CharToUpper(c: int64): int64` 
- `StrToLower(dest: pchar, src: pchar): pchar`
- `StrToUpper(dest: pchar, src: pchar): pchar`

#### String-Manipulation:
- `StrConcat(dest: pchar, s1: pchar, s2: pchar): pchar`
- `StrReverse(s: pchar): pchar` 
- `StrTrimWhitespace(dest: pchar, src: pchar): pchar`
- `IsWhitespace(c: int64): bool`

*Native String-Builtins (`StrLength`, `StrCharAt`, `StrSetChar`, `StrCompare`, `str_copy_builtin`) sind direkt ohne Import verfügbar.*

### std/env.lyx
**Environment und Command-Line API:**
- `init(argc: int64, argv: pchar): void` - Explizite env-Initialisierung (optional)
- `ArgCount(): int64` - Anzahl Command-Line-Argumente
- `Arg(i: int64): pchar` - Zugriff auf Argument i

### std/geo.lyx ⭐ **NEU** 
**Geolocation-Parser für offline GPS-Daten:**
- `ParseLat(s: pchar): int64` - Parst Decimal Degrees zu Microdegrees
- `ParseLon(s: pchar): int64` - Parst Longitude (gleiche Implementierung)

*Konvertiert GPS-Koordinaten wie "52.520008" zu Microdegrees (52520008) für präzise Integer-Arithmetik.*

### std/time.lyx
**Datums- und Zeit-Berechnungen:**
- `IsLeapYear(y: int64): bool` - Schaltjahr-Prüfung
- `DaysFromCivil(y,m,d): int64` - Tage seit Epoche
- `CivilYearFromDays(days): int64` - Jahr aus Tage-Zahl
- `CivilMonthFromDays(days): int64` - Monat aus Tage-Zahl  
- `CivilDayFromDays(days): int64` - Tag aus Tage-Zahl
- `DayOfYear(y,m,d): int64` - Tag des Jahres (1-366)
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
  text_color(red); gotoxy(5,4); PrintStr("This is red text");
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
  PrintIntLn(ArgCount());
  PrintLn(Arg(0));
  
  return 0;
}
```

... (rest unchanged)
