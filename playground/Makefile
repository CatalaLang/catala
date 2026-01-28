.PHONY: build serve clean typecheck dist grammar test

# Build the web interpreter and copy to playground
build:
	cd .. && dune build compiler/catala_web_interpreter.bc.js
	cp ../_build/default/compiler/catala_web_interpreter.bc.js catala_web_interpreter.js
	@echo "Built catala_web_interpreter.js ($(shell ls -lh catala_web_interpreter.js | awk '{print $$5}'))"

# Type-check JavaScript modules
typecheck:
	npm run typecheck

# Fetch keywords from tree-sitter-catala and generate js/grammar.js
grammar:
	node scripts/fetch-grammar.cjs

# Run integration test (requires playwright: npx playwright install chromium)
test: catala_web_interpreter.js
	node scripts/integration-test.cjs

# Start local development server
serve: catala_web_interpreter.js
	@echo "Starting server at http://localhost:8080"
	python3 -m http.server 8080

# Build missing files, then serve
dev:
	@if [ ! -f catala_web_interpreter.js ]; then $(MAKE) build; fi
	@if [ ! -f js/grammar.js ]; then $(MAKE) grammar; fi
	$(MAKE) serve

clean:
	rm -f catala_web_interpreter.js
	rm -rf node_modules dist

# Create deployable archive
dist: build grammar typecheck
	rm -rf dist
	mkdir -p dist/playground
	rsync -a --exclude='dist' --exclude='node_modules' --exclude='package*.json' --exclude='tsconfig.json' --exclude='Makefile' --exclude='README.md' --exclude='.gitignore' --exclude='scripts' . dist/playground/
	cd dist && zip -r catala-playground.zip playground
	@echo "Created dist/catala-playground.zip ($(shell ls -lh dist/catala-playground.zip | awk '{print $$5}'))"

catala_web_interpreter.js: build
