FPC      = fpc
FPCFLAGS = -Mobjfpc -Sh -FUlib/ -Fuutil/ -Fufrontend/ -Fuir/ -Fubackend/ -Fubackend/x86_64/ -Fubackend/elf/

# Release-Flags
RELEASE_FLAGS = -O2
# Debug-Flags (Range/Overflow/Stack-Checks, Heaptrace)
DEBUG_FLAGS   = -g -gl -Ci -Cr -Co -gh

# Alle Test-Units (nur existierende .pas Dateien)
TEST_SOURCES = $(wildcard tests/test_*.pas)
TESTS        = $(TEST_SOURCES:.pas=)

.PHONY: build debug test clean

build:
	@mkdir -p lib
	$(FPC) $(FPCFLAGS) $(RELEASE_FLAGS) aurumc.lpr -oaurumc

debug:
	@mkdir -p lib
	$(FPC) $(FPCFLAGS) $(DEBUG_FLAGS) aurumc.lpr -oaurumc

test: $(TESTS)
	@echo "=== Alle Tests ==="
	@fail=0; \
	for t in $(TESTS); do \
		echo "--- $$t ---"; \
		./$$t --all --format=plain || fail=1; \
	done; \
	if [ $$fail -eq 1 ]; then echo "FEHLER: Einige Tests fehlgeschlagen"; exit 1; fi; \
	echo "=== Alle Tests bestanden ==="

tests/test_%: tests/test_%.pas
	@mkdir -p lib
	$(FPC) $(FPCFLAGS) $(DEBUG_FLAGS) $< -o$@

clean:
	rm -f aurumc
	rm -f lib/*.ppu lib/*.o
	rm -f tests/test_bytes tests/test_diag tests/test_lexer tests/test_parser
	rm -f tests/test_sema tests/test_ir tests/test_elf tests/test_codegen
	rm -f tests/*.ppu tests/*.o
	rm -f *.ppu *.o
