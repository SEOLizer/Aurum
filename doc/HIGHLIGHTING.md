Lyx Syntax Highlighting

Ziel
----
Diese Dokumentation beschreibt die geplante TextMate/VSCode‑Grammatik für die Programmiersprache Lyx (.lyx) und erklärt das kurzfristige Fallback.

Status
------
- Es wurde ein initiales Grammar‑Skeleton erstellt: syntaxes/lyx.tmLanguage.json
- Kurzfristiger Fallback via .gitattributes: *.lyx → Rust (linguist mapping)

Wo liegt die Grammatik?
----------------------
- syntaxes/lyx.tmLanguage.json  (TextMate JSON Grammar)

Kurzbeschreibung der Scopes
---------------------------
- comment.line.double-slash.lyx : // comment
- comment.block.lyx            : /* ... */
- string.quoted.double.lyx     : "string" (mit Escapes)
- constant.numeric.integer.lyx : integer literals
- keyword.control.lyx          : fn, if, else, while, return, extern, true, false
- storage.modifier.lyx         : var, let, co, con
- storage.type.lyx             : int64, bool, void, pchar
- support.function.builtin.lyx : print_str, print_int, exit
- entity.name.function.lyx     : function names after 'fn'
- keyword.operator.lyx         : :=, ==, !=, <=, >=, &&, ||, +, -, *, /, %

How to test locally (VSCode)
----------------------------
1. Open this repository in VSCode.
2. Use "Extension Development Host" (Command Palette: "Developer: Reload Window in Extension Development Host") and load the grammar by opening the file syntaxes/lyx.tmLanguage.json in the host.
3. Open any .lyx file (e.g. examples/if_test.lyx) and use "Developer: Inspect TM Scopes" to inspect token scopes.

Fallback
--------
Bis die Grammar upstream in github/linguist gemergt wird, gibt es ein pragmatisches Fallback in .gitattributes, das alle .lyx Dateien als Rust behandelt. Das sorgt auf GitHub für sichtbares Highlighting, ist aber keine perfekte Lösung.

Nächste Schritte
----------------
- Iteratives Verbessern der Grammar basierend auf VSCode‑Inspektion und Unit‑Beispielen.
- Optional: Packen als lokale VSCode Extension (package.json) für einfachere Tests.
- Nach Review/Feinschliff: Fork & PR zu github/linguist mit Grammar + Beispiel‑Fixtures.
