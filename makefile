BIN := fortran-datetime

DIR_INS=${HOME}/1_Projects/utils
DIR_BIN=${DIR_INS}/bin

CMAKE       := cmake .. -GNinja -DCMAKE_INSTALL_PREFIX=$(DIR_INS)
CMAKE_RLS   := $(CMAKE) -DCMAKE_BUILD_TYPE=Release
CMAKE_DBG   := $(CMAKE) -DCMAKE_BUILD_TYPE=Debug
CMAKE_APP   := $(CMAKE) -DMYAPP=TRUE
NINJA       := ninja
MKDIR       := mkdir -p
MKDIR_BUILD := $(MKDIR) build && cd build

default: release

run: release
	${DIR_BIN}/$(BIN)

debugrun: debug
	${DIR_BIN}/$(BIN)

.PHONY: test
test:
	$(MKDIR_BUILD) && $(CMAKE_DBG) && $(NINJA) && ctest -VV

.PHONY: release
release:
	$(MKDIR_BUILD) && $(CMAKE_RLS) && $(NINJA) && $(NINJA) install

.PHONY: debug
debug:
	$(MKDIR_BUILD) && $(CMAKE_DBG) && $(NINJA) && $(NINJA) install && \
	cp ${DIR_BIN}/$(BIN) ~/gdb/fortran_debug

.PHONY: valgrind
valgrind: debug
	valgrind --trace-children=yes --track-fds=no --track-origins=yes --leak-check=full --show-leak-kinds=all --show-reachable=no ~/gdb/fortran_debug

.PHONY: install
install:
	cd build && $(NINJA) install

.PHONY: uninstall
uninstall:
	cd build && xargs rm < install_manifest.txt

clean:
	rm -r build ; \
	rm ~/gdb/fortran_debug

clean_build:
	find . -type d -iname build | xargs rm -rf
