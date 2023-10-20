CMAKE       := cmake .. -GNinja
CMAKE_RLS   := $(CMAKE) -DCMAKE_BUILD_TYPE=Release
CMAKE_DBG   := $(CMAKE) -DCMAKE_BUILD_TYPE=Debug
CMAKE_APP   := $(CMAKE) -DMYAPP=TRUE
NINJA       := ninja
MKDIR       := mkdir -p
MKDIR_BUILD := $(MKDIR) build && cd build

.PHONY: release
release:
	$(MKDIR_BUILD) && $(CMAKE_RLS) && $(NINJA)

.PHONY: debug
debug:
	$(MKDIR_BUILD) && $(CMAKE_DBG) && $(NINJA)

.PHONY: test
test:
	$(MKDIR_BUILD) && $(CMAKE_DBG) && $(NINJA) && ctest -VV

.PHONY: valgrind
valgrind: debug
	valgrind --trace-children=yes --track-fds=no --track-origins=yes --leak-check=full --show-leak-kinds=all --show-reachable=no ~/gdb/fortran_debug

.PHONY: install
install:
	cd build && $(NINJA) install

.PHONY: uninstall
uninstall:
	cd build && xargs rm < install_manifest.txt

.PHONY: clean
clean:
	rm -r build

.PHONY: clean_build
clean_build:
	find . -type d -iname build | xargs rm -rf

.PHONY: doc
doc:
	rm -r doc build; ford ford.md

