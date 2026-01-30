# Catala Playground

Interactive web playground for the Catala language.

## Embedding

The playground can be embedded via iframe or linked directly.

### URL Parameters

Parameters are passed as hash fragments (e.g., `index.html#codeUrl=...&checkpointId=...`):

| Parameter | Description |
|-----------|-------------|
| `codeUrl` | URL to fetch initial Catala code from |
| `checkpointId` | Identifier for localStorage key + enables reset button |
| `persist` | Set to `false` to disable localStorage persistence |

### Embedding Modes

**Standalone mode** (default)
```html
<iframe src="playground/index.html"></iframe>
```
- User edits persist to `localStorage['catala-playground-default']`
- No reset button

**Learn mode** (with checkpoint)
```html
<iframe src="playground/index.html#codeUrl=https://example.com/code.catala_en&checkpointId=lesson-1"></iframe>
```
- Loads code from `codeUrl` on first visit
- User edits persist to `localStorage['catala-learn-lesson-1']`
- Reset button appears, allowing user to restore original checkpoint

**Ephemeral mode** (no persistence)
```html
<iframe src="playground/index.html#codeUrl=https://example.com/code.catala_en&persist=false"></iframe>
```
- Loads code from `codeUrl` every time
- No localStorage, no reset button

### Learn Page

`learn.html` provides a side-by-side view with the Catala book:

**English (default)**:
```
learn.html?chapter=2-1-basic-blocks
```

**French**:
```
learn.html?bookBase=https://book.catala-lang.org/fr&chapter=2-1-basic-blocks
```

Query parameters:
- `chapter` — chapter slug (default: `2-1-basic-blocks`)
- `bookBase` — base URL for the book (default: `https://book.catala-lang.org/en`)

Checkpoint mappings are configured in `learn.html`.

## Development

### Prerequisites

- Node.js (v18+ recommended)
- npm (comes with Node.js)

### Setup

```bash
cd playground
npm install
```

### Type Checking

The playground uses TypeScript's JSDoc-based type checking for vanilla JavaScript. This provides type safety without a build step.

```bash
npm run typecheck
```

All JavaScript files in `js/` use JSDoc annotations for type information. The TypeScript compiler validates these without generating any output.

### File Structure

```
playground/
├── index.html              # Main playground page
├── learn.html              # Side-by-side book + playground
├── catala_web_interpreter.js  # Built JSOO interpreter (generated)
├── css/
│   └── style.css           # Styles (navy theme)
├── js/
│   ├── main.js             # Entry point, init logic
│   ├── editor.js           # Monaco editor setup, CodeLens
│   ├── files.js            # Multi-file state management
│   ├── grammar.js          # Keywords for syntax highlighting (generated)
│   ├── i18n.js             # Internationalization (EN/FR)
│   ├── interpreter.js      # Catala interpreter wrapper
│   ├── persistence.js      # localStorage persistence
│   └── ui.js               # UI rendering, output display
├── scripts/
│   └── fetch-grammar.cjs   # Fetches keywords from tree-sitter-catala
├── package.json
├── tsconfig.json
└── README.md
```

### Building the Interpreter

The `catala_web_interpreter.js` file is built from OCaml source using `js_of_ocaml`:

```bash
# From the repository root
dune build compiler/catala_web_interpreter.bc.js
cp _build/default/compiler/catala_web_interpreter.bc.js playground/catala_web_interpreter.js
```

Or using the Makefile:

```bash
cd playground
make build
```

### Updating Syntax Keywords

Keywords for syntax highlighting are fetched from [tree-sitter-catala](https://github.com/CatalaLang/tree-sitter-catala):

```bash
make grammar
```

This generates `js/grammar.js` from the upstream grammar. Run this when Catala adds new keywords.

## Multi-File Support

The playground supports multiple files with proper module resolution:

- Files can declare themselves as modules using `> Module Name` where the name matches the filename
- Files with scopes can be run directly, even if they declare themselves as modules
- Other files are automatically registered as importable modules
- Example:

```catala
# helpers.catala_en (library module)
> Module Helpers
declaration double content integer depends on x content integer equals x * 2

# main.catala_en (can be both a module AND have runnable scopes)
> Module Main
> Using Helpers

declaration scope Test:
  output result content integer

scope Test:
  definition result equals Helpers.double of 21
```

### Running the Playground

```bash
cd playground
make dev
```

This will auto-build any missing files and start a development server on http://localhost:8080.

### Code Style

- Use ES modules (`import`/`export`)
- Add JSDoc type annotations for all functions
- Target ES2020 baseline (modern browsers, 2023-2024)
- No transpilation or bundler required

### CI

Type checking runs automatically in CI. See `.github/workflows/ci.yml`.
