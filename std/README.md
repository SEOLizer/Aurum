Standard-Units (std)
=====================

Dieses Verzeichnis enthält standardisierte Units, die als Bibliothek für Lyx-Programme dienen. Ziel ist, wiederverwendbare, rein in Lyx geschriebene Funktionen bereitzustellen, die keine externen Laufzeitabhängigkeiten benötigen.

Aktuell implementiert

- std/math.lyx
  - abs64(x: int64): int64
  - min64(a: int64, b: int64): int64
  - max64(a: int64, b: int64): int64
  - div64(a: int64, b: int64): int64
  - mod64(a: int64, b: int64): int64
  - times_two(x: int64): int64

- std/io.lyx
  - print(s: pchar): void
  - println(s: pchar): void
  - print_intln(x: int64): void
  - exit_proc(code: int64): void

- std/env.lyx
  - init(argc: int64, argv: pchar): void  -- Wrapper für interne env-Initialisierung (explizit)
  - arg_count(): int64
  - arg(i: int64): pchar

- std/time.lyx (Entwurf)
  - is_leap_year(y: int64): bool
  - days_from_civil(y,m,d): int64
  - civil_year_from_days(days): int64
  - civil_month_from_days(days): int64
  - civil_day_from_days(days): int64
  - day_of_year(y,m,d): int64
  - weekday(y,m,d): int64    // 0=Monday..6=Sunday
  - iso_week(y,m,d): int64
  - iso_year(y,m,d): int64
  - format_unix(ts, fmt, tz_offset): pchar  // Platzhalter (noch nicht implementiert)
  - format_unix_to_buf(...): int64         // Platzhalter (noch nicht implementiert)

Verwendung

import std.math;
import std.io;
import std.env; // falls benötigt
import std.time; // Entwurf: numerische Datumsfunktionen

fn main(argc: int64, argv: pchar): int64 {
  // env.init ist optional, ab Version mit automatischer _start-Init nicht mehr erforderlich
  init(argc, argv); // optional
  print_intln(arg_count());
  print_str(arg(0));
  print_str("\n");
  return 0;
}

Hinweis zur env-Initialisierung

- Der Compiler initialisiert ab Version X automatisch beim Programstart die interne env-Datenstruktur (argc/argv), sodass explizites `init(argc, argv)` in `main` nicht mehr zwingend notwendig ist.
- Die std/env Unit stellt dennoch Wrapper zur manuellen Verwendung bereit und bleibt abwärtskompatibel.

Tests

- Beispiele befinden sich in `examples/` (use_math.lyx, use_io.lyx, use_env.lyx, use_time_format.lyx).
- CI führt Integrationstests, die diese Beispiele bauen und ausführen.
