# tests/scratch/

Dieses Verzeichnis ist für **temporäre Test-Dateien** während der Entwicklung.

## Verwendung

```bash
# Temporäre Tests erstellen
echo 'fn main(): int64 { return 42; }' > tests/scratch/quick_test.lyx
./lyxc tests/scratch/quick_test.lyx -o tests/scratch/quick_test

# Nach dem Testen aufräumen
rm tests/scratch/quick_test*
```

## Struktur

- **`tests/scratch/*.lyx`**: Temporäre Lyx-Quelldateien für manuelle Tests
- **`tests/scratch/*`** (Binaries): Kompilierte Test-Binaries
- **Nicht committen**: Diese Dateien sollten nur lokal existieren

## .gitignore

Die Dateien in diesem Verzeichnis werden automatisch ignoriert (außer README.md).