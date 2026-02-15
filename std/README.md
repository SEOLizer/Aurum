Standard-Units (std)
=====================

Dieses Verzeichnis enthält standardisierte Units, die als Bibliothek für Aurum-Programme dienen. Ziel ist, wiederverwendbare, rein in Aurum geschriebene Funktionen bereitzustellen, die keine externen Laufzeitabhängigkeiten benötigen.

Aktuell implementiert

- std/math.au
  - abs64(x: int64): int64
  - min64(a: int64, b: int64): int64
  - max64(a: int64, b: int64): int64
  - div64(a: int64, b: int64): int64
  - mod64(a: int64, b: int64): int64
  - times_two(x: int64): int64

- std/io.au
  - print(s: pchar): void
  - println(s: pchar): void
  - print_intln(x: int64): void
  - exit_proc(code: int64): void

- std/env.au
  - init(argc: int64, argv: pchar): void  -- Wrapper für interne env-Initialisierung (explizit)
  - arg_count(): int64
  - arg(i: int64): pchar

Verwendung

import std.math;
import std.io;
import std.env; // falls benötigt

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

- Beispiele befinden sich in `examples/` (use_math.au, use_io.au, use_env.au).
- CI führt Integrationstests, die diese Beispiele bauen und ausführen.
