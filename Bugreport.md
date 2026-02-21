Bugreport — Testläufe (Stand: 2026-02-21)

Zusammenfassung
--------------
Die meisten Probleme wurden behoben. Die Test-Suite zeigt deutliche Verbesserungen.

**Bestandene Tests:**
- test_abi_v020: 9/9 ✅
- test_array_static: 1/1 ✅
- test_bytes: 13/13 ✅
- test_diagnostics: 12/12 ✅
- test_if: 1/1 ✅
- test_lexer: 52/52 ✅
- test_switch: 1/1 ✅
- test_time_format: 5/5 ✅
- test_ir: 2/2 ✅
- test_sema: 13/13 ✅
- test_emit_create ✅
- test_emit_from_lower ✅
- test_emit_main_args ✅
- test_emit_run ✅
- test_codegen_widths: 2/3 ✅ (1 Failure - non-literal truncation)

**Verbleibende Failures (3):**
- test_parser: 2 Failures (Literal Folding für unäre Operatoren)
- test_codegen_widths: 1 Failure (irSExt bei non-literal)

Verbleibende Probleme (Details)
-----------------------------

### 1) test_parser.pas - 2 Failures

**Fehlerquelle:** `TestParseNestedUnary_LiteralFolding`, `TestParseNestedUnary_NonLiteral`

Die Tests erwarten, dass `--5` zu `5` gefaltet wird (doppelte Negation =positiv). Aktuell wird ein Fallback-Wert zurückgegeben.

**Kurzbeschreibung:** Die Literal-Folding-Logik für verschachtelte unäre Operatoren ist nicht vollständig implementiert.

### 2) test_codegen_widths.pas - 1 Failure

**Fehlerquelle:** `TestNonLiteralInitEmitsTruncAndExt`

Der Test erwartet, dass beim Laden einer int8-Variable in eine int64-Variable ein irSExt (Sign Extension) emittiert wird.

**Kurzbeschreibung:** Das IR-Lowering für Sign-Extension bei nicht-literalen Initialisierungen fehlt.

Erledigte Fixes
--------------
- ✅ Varargs-Support für printf in TSema
- ✅ IsVarArgs-Flag in TSymbol
- ✅ printf als Builtin in LowerExpr
- ✅ ArgTemps für Builtin-Aufrufe
- ✅ Parser nil-Checks in ParseUnaryExpr, ParseMulExpr, ParseAddExpr, ParseStmt
- ✅ Duplicate ParseAddExpr entfernt
- ✅ Integer-Width Truncation funktioniert für Konstanten (2/3 Tests bestanden)
