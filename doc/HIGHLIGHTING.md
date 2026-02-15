Aurum Syntax Highlighting

Ziel
----
Diese Dokumentation beschreibt die geplante TextMate/VSCode‑Grammatik für die Programmiersprache Aurum (.au) und erklärt das kurzfristige Fallback.

Status
------
- Es wurde ein initiales Grammar‑Skeleton erstellt: syntaxes/aurum.tmLanguage.json
- Kurzfristiger Fallback via .gitattributes: *.au → Rust (linguist mapping)

Wo liegt die Grammatik?
----------------------
- syntaxes/aurum.tmLanguage.json  (TextMate JSON Grammar)

Kurzbeschreibung der Scopes
---------------------------
- comment.line.double-slash.aurum : // comment
- comment.block.aurum            : /* ... */
- string.quoted.double.aurum     : "string" (mit Escapes)
- constant.numeric.integer.aurum : integer literals
- keyword.control.aurum          : fn, if, else, while, return, extern, true, false
- storage.modifier.aurum         : var, let, co, con
- storage.type.aurum             : int64, bool, void, pchar
- support.function.builtin.aurum : print_str, print_int, exit
- entity.name.function.aurum     : function names after 'fn'
- keyword.operator.aurum         : :=, ==, !=, <=, >=, &&, ||, +, -, *, /, %

How to test locally (VSCode)
----------------------------
1. Open this repository in VSCode.
2. Use "Extension Development Host" (Command Palette: "Developer: Reload Window in Extension Development Host") and load the grammar by opening the file syntaxes/aurum.tmLanguage.json in the host.
3. Open any .au file (e.g. examples/if_test.au) and use "Developer: Inspect TM Scopes" to inspect token scopes.

Fallback
--------
Bis die Grammar upstream in github/linguist gemergt wird, gibt es ein pragmatisches Fallback in .gitattributes, das alle .au Dateien als Rust behandelt. Das sorgt auf GitHub für sichtbares Highlighting, ist aber keine perfekte Lösung.

Nächste Schritte
----------------
- Iteratives Verbessern der Grammar basierend auf VSCode‑Inspektion und Unit‑Beispielen.
- Optional: Packen als lokale VSCode Extension (package.json) für einfachere Tests.
- Nach Review/Feinschliff: Fork & PR zu github/linguist mit Grammar + Beispiel‑Fixtures.
