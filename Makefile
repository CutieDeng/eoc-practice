RACKET = racket
RACO   = raco

.PHONY: test test-java test-aarch64 test-driver test-gccast gccast-plugin clean

# Full suite: every *-test.rkt under src/ and test/ + the aarch64 integration script.
test:
	./scripts/run-tests.sh

test-java:
	$(RACO) test src/component/java/transform/bbs-test.rkt \
	             src/component/java/transform/jvm-to-cfg-test.rkt \
	             src/component/java/transform/ssa-construct-test.rkt \
	             src/component/java/transform/normalize-try-exits-test.rkt \
	             src/component/java/transform/cfg-to-rvsdg-test.rkt \
	             src/pipeline/java/pipeline-test.rkt

test-aarch64:
	$(RACO) test src/component/aarch64/ir/types-test.rkt \
	             src/component/aarch64/ir/cfg-test.rkt \
	             src/component/aarch64/frontend/parser-test.rkt \
	             src/component/aarch64/interp/base-test.rkt \
	             src/component/aarch64/interp/sve-test.rkt \
	             src/component/aarch64/backend/regalloc-test.rkt
	$(RACKET) test/integration/aarch64/basic.rkt

test-driver:
	$(RACO) test src/driver/graph/graph-test.rkt src/kernel/data/idmap.rkt

test-gccast:
	$(RACO) test src/frontend/gccast/reader-test.rkt gccast-plugin/gen/treedef.rkt \
	             src/component/gccast/transform/gimplify-test.rkt

# Build the GCC plugin for the default gcc (override: make gccast-plugin GCC=gcc-15)
GCC ?= gcc-15
gccast-plugin:
	$(RACKET) gccast-plugin/build.rkt --gcc $(GCC) --fixtures test/integration/gccast

clean:
	find src test -type d -name compiled -prune -exec rm -rf {} +
