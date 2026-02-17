Integer‑Breiten und Semantik in Lyx

Status (aktuell)
- Die Sprache unterstützt namentlich mehrere Integer‑Breiten: int8, int16, int32, int64 sowie uint8, uint16, uint32, uint64.
- Auf der Frontend‑Ebene (Lexer/Parser/AST/Sema) sind diese Typen implementiert und werden als eigene TAurumType‑Werte dargestellt.
- Semantisch gilt derzeit: alle Integer‑Breiten gehören zur "integer family". Operanden aus der Integer‑Familie gelten kompatibel zueinander (TypeEqual betrachtet sie als gleich). Arithmetik‑Operationen werden für die Zeit der Analyse zu int64 promotet (Resultat‑Typ atInt64).

Einschränkungen
- Der Backend‑Stack (IR → Lowering → x86_64 Emitter) behandelt derzeit Integer‑Operationen auf 64‑Bit. Das bedeutet:
  - Operationen auf int8/16/32 werden aktuell arithmetisch als 64‑Bit Operationen ausgeführt.
  - Es gibt noch keine dedizierte Behandlung von unsigned‑Semantik (zero‑extension) vs signed (sign‑extension) im emitted Code.
  - Für genaue, width‑spezifische Codegen (z. B. 8‑Bit Add mit Overflow / proper zero‑extend on load) sind Änderungen im IR und im Emitter erforderlich.

Empfehlung / Roadmap (nächste Schritte)
1) Kurzfristig (konzeptuell, bereits umgesetzt im Frontend):
   - Parser/AST/Sema erkennen und validieren int8..int64, uint8..uint64.
   - Sema akzeptiert Integer‑Familie kompatibel und promotet bei Arithmetik zu int64.
2) Mittelfristig (Backend‑Änderungen, empfohlen):
   - IR erweitern: Ops für Sign‑Extend / Zero‑Extend (irSExt / irZExt) und evtl. typannotierte Temps.
   - Lowering: Bei Loads/Constant→Temp, füge SExt/ZExt je nach Zieltyp ein. Vor BinOps stelle sicher, dass beide Operanden auf gleiche Breite/Sign behandelt werden.
   - Emitter: Generiere für irSExt/irZExt die passenden x86_64 Instruktionen (movsx/movzx). Bei Vergleichen wähle signed/unsigned variant (setl/setg vs setb/seta etc.).
3) Langfristig:
   - Tests: Unit‑Tests für jede Breite inkl. unsigned Comparisons, truncation, overflow‑behavior.
   - Optional: Benennungskonventionen für temporäre Register/Slots mit Typannotation.

Beispiel‑Semantik‑Regeln (Vorschlag)
- Zuweisung: Beim Zuweisen eines größeren Typs in einen kleineren wird das Ziel masked/truncated (z. B. val & ((1<<width)-1)).
- Arithmetik: Operanden werden auf die größte Breite der beteiligten Operanden normalisiert. Interne temporäre Resultate können in 64‑Bit gehalten werden.
- Vergleiche: unsigned comparisons verwenden die unsigned‑Variante der CPU‑Instruktion; signed comparisons die signed‑Variante.

Hinweis für Entwickler
- Solange der Backend‑Support für Breiten fehlt, ist das pragmatischste Programmiermodell: nutze `int`/`int64` für numerische Berechnungen, und `uint*` nur wenn du später die Backend‑Erweiterung planst. Die Frontend‑Syntax erlaubt die schmaleren Typen bereits, die Laufzeit/Semantik kann aber aktuell von 64‑Bit‑Promotions beeinflusst werden.

Kontakt
- Wenn du möchtest, implementiere ich die Backend‑Schritte (IR + Lowering + Emitter). Schätze dafür 2–4 Personentage, abhängig von gewünschter Vollständigkeit (signed/unsigned, truncation, tests).
