#!/usr/bin/env node
// Basic tests for the Catala web interpreter
// Run with: node test_web_interpreter.js

const fs = require('fs');
const path = require('path');

// Load the interpreter
const interpreterPath = path.join(__dirname, '../../_build/default/compiler/web/catala_web_interpreter.bc.js');
if (!fs.existsSync(interpreterPath)) {
  console.error('ERROR: Interpreter not found at', interpreterPath);
  console.error('Run: dune build compiler/web/catala_web_interpreter.bc.js');
  process.exit(1);
}

const code = fs.readFileSync(interpreterPath, 'utf8');
eval(code);

let passed = 0;
let failed = 0;

function test(name, fn) {
  try {
    fn();
    console.log(`✓ ${name}`);
    passed++;
  } catch (e) {
    console.log(`✗ ${name}`);
    console.log(`  ${e.message || e}`);
    failed++;
  }
}

function assertEquals(actual, expected, msg) {
  if (actual !== expected) {
    throw new Error(`${msg || 'Assertion failed'}: expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`);
  }
}

function assertContains(str, substr, msg) {
  if (!str.includes(substr)) {
    throw new Error(`${msg || 'Assertion failed'}: expected to contain ${JSON.stringify(substr)}`);
  }
}

// Diagnostic helpers
function getErrors(result) { return result.diagnostics.filter(d => d.level === 'error'); }
function getWarnings(result) { return result.diagnostics.filter(d => d.level === 'warning'); }
function getErrorText(result) { return getErrors(result).map(d => d.message).join('\n\n'); }
function getErrorPositions(result) { return getErrors(result).flatMap(d => Array.from(d.positions)); }

// Test cases

test('Basic integer computation', () => {
  const code = `
\`\`\`catala
declaration scope Test:
  output result content integer

scope Test:
  definition result equals 40 + 2
\`\`\`
`;
  const result = exports.interpret({
    files: { 'test.catala_en': code },
    scope: 'Test'
  });
  assertEquals(result.success, true, 'Should succeed');
  assertContains(result.output, 'result = 42', 'Should output 42');
});

test('Date and stdlib', () => {
  const code = `
\`\`\`catala
declaration scope Test:
  output d content date
  output y content integer

scope Test:
  definition d equals |2024-06-15|
  definition y equals Date.get_year of d
\`\`\`
`;
  const result = exports.interpret({
    files: { 'test.catala_en': code },
    scope: 'Test'
  });
  assertEquals(result.success, true, 'Should succeed');
  assertContains(result.output, '2024-06-15', 'Should contain date');
  assertContains(result.output, '2,024', 'Should contain year');
});

test('Decimal.round_to_decimal (Decimal_internal)', () => {
  const code = `
\`\`\`catala
declaration scope Test:
  output result content decimal

scope Test:
  definition result equals Decimal.round_to_decimal of 123.4567, 2
\`\`\`
`;
  const result = exports.interpret({
    files: { 'test.catala_en': code },
    scope: 'Test'
  });
  assertEquals(result.success, true, 'Should succeed');
  assertContains(result.output, '123.46', 'Should round to 2 decimals');
});

test('Money.round_to_decimal (Money_internal)', () => {
  const code = `
\`\`\`catala
declaration scope Test:
  output result content money

scope Test:
  definition result equals Money.round_to_decimal of $123.45, -1
\`\`\`
`;
  const result = exports.interpret({
    files: { 'test.catala_en': code },
    scope: 'Test'
  });
  assertEquals(result.success, true, 'Should succeed');
  assertContains(result.output, '$120', 'Should round to nearest 10');
});

test('List.sequence (List_internal)', () => {
  const code = `
\`\`\`catala
declaration scope Test:
  output result content list of integer

scope Test:
  definition result equals List.sequence of 1, 5
\`\`\`
`;
  const result = exports.interpret({
    files: { 'test.catala_en': code },
    scope: 'Test'
  });
  assertEquals(result.success, true, 'Should succeed');
  assertContains(result.output, '1', 'Should contain 1');
  assertContains(result.output, '4', 'Should contain 4');
});

test('Money formatting (English)', () => {
  const code = `
\`\`\`catala
declaration scope Test:
  output amount content money

scope Test:
  definition amount equals $1,234.56
\`\`\`
`;
  const result = exports.interpret({
    files: { 'test.catala_en': code },
    scope: 'Test'
  });
  assertEquals(result.success, true, 'Should succeed');
  assertContains(result.output, '$1,234.56', 'Should format money');
});

test('Money formatting (French)', () => {
  const code = `
\`\`\`catala
déclaration champ d'application Test:
  résultat montant contenu argent

champ d'application Test:
  définition montant égal à 1 234,56 €
\`\`\`
`;
  const result = exports.interpret({
    files: { 'test.catala_fr': code },
    scope: 'Test',
    language: 'fr'
  });
  assertEquals(result.success, true, 'Should succeed');
  assertContains(result.output, '€', 'Should format money with euro');
});

test('Syntax error gives position', () => {
  const code = `
\`\`\`catala
declaration scope Test:
  output x content integer

scope Test:
  definition x equals 1 +
\`\`\`
`;
  const result = exports.interpret({
    files: { 'test.catala_en': code },
    scope: 'Test'
  });
  assertEquals(result.success, false, 'Should fail');
  assertEquals(getErrorPositions(result).length > 0, true, 'Should have error positions');
});

test('Type error gives position', () => {
  const code = `
\`\`\`catala
declaration scope Test:
  output x content integer

scope Test:
  definition x equals |2024-01-01|
\`\`\`
`;
  const result = exports.interpret({
    files: { 'test.catala_en': code },
    scope: 'Test'
  });
  assertEquals(result.success, false, 'Should fail');
  assertContains(getErrorText(result), 'date', 'Should mention date type');
  assertContains(getErrorText(result), 'integer', 'Should mention integer type');
  assertEquals(getErrorPositions(result).length > 0, true, 'Should have error positions');
});

test('Unknown scope error', () => {
  const code = `
\`\`\`catala
declaration scope Test:
  output x content integer

scope Test:
  definition x equals 42
\`\`\`
`;
  const result = exports.interpret({
    files: { 'test.catala_en': code },
    scope: 'WrongScope'
  });
  assertEquals(result.success, false, 'Should fail');
  assertContains(getErrorText(result), 'WrongScope', 'Should mention wrong scope');
});

test('Boolean output (English)', () => {
  const code = `
\`\`\`catala
declaration scope Test:
  output flag content boolean

scope Test:
  definition flag equals true
\`\`\`
`;
  const result = exports.interpret({
    files: { 'test.catala_en': code },
    scope: 'Test'
  });
  assertEquals(result.success, true, 'Should succeed');
  assertContains(result.output, 'true', 'Should output true');
});

test('Boolean output (French)', () => {
  const code = `
\`\`\`catala
déclaration champ d'application Test:
  résultat drapeau contenu booléen

champ d'application Test:
  définition drapeau égal à vrai
\`\`\`
`;
  const result = exports.interpret({
    files: { 'test.catala_fr': code },
    scope: 'Test',
    language: 'fr'
  });
  assertEquals(result.success, true, 'Should succeed');
  assertContains(result.output, 'vrai', 'Should output vrai');
});

test('Multi-file with module', () => {
  const helperModule = `
> Module Helpers

\`\`\`catala-metadata
declaration double
  content integer
  depends on x content integer
  equals x * 2
\`\`\`
`;

  const mainCode = `
> Using Helpers

\`\`\`catala
declaration scope Test:
  output result content integer

scope Test:
  definition result equals Helpers.double of 21
\`\`\`
`;
  // Main file listed first, becomes entry point by default
  const result = exports.interpret({
    files: {
      'main.catala_en': mainCode,
      'helpers.catala_en': helperModule
    },
    scope: 'Test'
  });
  assertEquals(result.success, true, 'Should succeed');
  assertContains(result.output, 'result = 42', 'Should output 42 (21 * 2)');
});

test('Multi-file with explicit main', () => {
  const helperModule = `
> Module Helpers

\`\`\`catala-metadata
declaration double
  content integer
  depends on x content integer
  equals x * 2
\`\`\`
`;

  const mainCode = `
> Using Helpers

\`\`\`catala
declaration scope Test:
  output result content integer

scope Test:
  definition result equals Helpers.double of 21
\`\`\`
`;
  // Helper listed first, but main explicitly specified
  const result = exports.interpret({
    files: {
      'helpers.catala_en': helperModule,
      'main.catala_en': mainCode
    },
    scope: 'Test',
    main: 'main.catala_en'
  });
  assertEquals(result.success, true, 'Should succeed');
  assertContains(result.output, 'result = 42', 'Should output 42 (21 * 2)');
});

test('Module declaration in main file', () => {
  const mainCode = `
> Module Foo

\`\`\`catala
declaration scope Test:
  output x content integer

scope Test:
  definition x equals 42
\`\`\`
`;
  // Filename matches module declaration
  const result = exports.interpret({
    files: { 'foo.catala_en': mainCode },
    scope: 'Test'
  });
  assertEquals(result.success, true, 'Should succeed with matching filename');
  assertContains(result.output, 'x = 42', 'Should output 42');
});

test('Module declaration mismatch', () => {
  const mainCode = `
> Module Foo

\`\`\`catala
declaration scope Test:
  output x content integer

scope Test:
  definition x equals 42
\`\`\`
`;
  // Filename doesn't match module declaration - should fail
  const result = exports.interpret({
    files: { 'bar.catala_en': mainCode },
    scope: 'Test'
  });
  assertEquals(result.success, false, 'Should fail with mismatched filename');
  assertContains(getErrorText(result), 'Module declared as Foo', 'Should mention module name mismatch');
});

test('Error positions with real filename', () => {
  const code = `
\`\`\`catala
declaration scope Test:
  output x content integer

scope Test:
  definition x equals 1 +
\`\`\`
`;
  const result = exports.interpret({
    files: { 'myfile.catala_en': code },
    scope: 'Test'
  });
  assertEquals(result.success, false, 'Should fail');
  assertEquals(getErrorPositions(result).length > 0, true, 'Should have error positions even with real filename');
});

test('Multi-file: main is a module, scope in main', () => {
  const utilsModule = `
> Module Utils

\`\`\`catala-metadata
declaration triple
  content integer
  depends on x content integer
  equals x * 3
\`\`\`
`;

  // Main file is also a module, and contains the scope we want to run
  const mainModule = `
> Module Main

> Using Utils

\`\`\`catala
declaration scope Compute:
  output result content integer

scope Compute:
  definition result equals Utils.triple of 10
\`\`\`
`;
  const result = exports.interpret({
    files: {
      'main.catala_en': mainModule,
      'utils.catala_en': utilsModule
    },
    scope: 'Compute',
    main: 'main.catala_en'
  });
  assertEquals(result.success, true, 'Should succeed');
  assertContains(result.output, 'result = 30', 'Should output 30 (10 * 3)');
});

test('Multi-file: run scope from module by setting it as main', () => {
  // Module with a scope we want to run
  const mathModule = `
> Module Math

\`\`\`catala
declaration square
  content integer
  depends on x content integer
  equals x * x

declaration scope TestSquare:
  output result content integer

scope TestSquare:
  definition result equals square of 7
\`\`\`
`;

  // Another file that uses the module
  const otherCode = `
> Using Math

\`\`\`catala
declaration scope Other:
  output val content integer

scope Other:
  definition val equals Math.square of 5
\`\`\`
`;
  // To run a scope from a module, set that module as main
  const result = exports.interpret({
    files: {
      'other.catala_en': otherCode,
      'math.catala_en': mathModule
    },
    scope: 'TestSquare',
    main: 'math.catala_en'
  });
  assertEquals(result.success, true, 'Should succeed running scope from module as main');
  assertContains(result.output, 'result = 49', 'Should output 49 (7 * 7)');
});

test('Multi-file: three files with chain of dependencies', () => {
  const baseModule = `
> Module Base

\`\`\`catala-metadata
declaration add_one
  content integer
  depends on x content integer
  equals x + 1
\`\`\`
`;

  const middleModule = `
> Module Middle

> Using Base

\`\`\`catala-metadata
declaration add_two
  content integer
  depends on x content integer
  equals Base.add_one of (Base.add_one of x)
\`\`\`
`;

  const mainCode = `
> Using Middle

\`\`\`catala
declaration scope Test:
  output result content integer

scope Test:
  definition result equals Middle.add_two of 10
\`\`\`
`;
  const result = exports.interpret({
    files: {
      'main.catala_en': mainCode,
      'middle.catala_en': middleModule,
      'base.catala_en': baseModule
    },
    scope: 'Test',
    main: 'main.catala_en'
  });
  assertEquals(result.success, true, 'Should succeed with chain of dependencies');
  assertContains(result.output, 'result = 12', 'Should output 12 (10 + 1 + 1)');
});

// ============================================================================
// Typecheck tests
// ============================================================================

test('Typecheck: basic success', () => {
  const code = `
\`\`\`catala
declaration scope Test:
  output result content integer

scope Test:
  definition result equals 40 + 2
\`\`\`
`;
  const result = exports.typecheck({
    files: { 'test.catala_en': code }
  });
  assertEquals(result.success, true, 'Should succeed');
  assertContains(result.output, 'successful', 'Should indicate success');
});

test('Typecheck: type error gives position', () => {
  const code = `
\`\`\`catala
declaration scope Test:
  output x content integer

scope Test:
  definition x equals |2024-01-01|
\`\`\`
`;
  const result = exports.typecheck({
    files: { 'test.catala_en': code }
  });
  assertEquals(result.success, false, 'Should fail');
  assertContains(getErrorText(result), 'date', 'Should mention date type');
  assertContains(getErrorText(result), 'integer', 'Should mention integer type');
  assertEquals(getErrorPositions(result).length > 0, true, 'Should have error positions');
});

test('Typecheck: syntax error gives position', () => {
  const code = `
\`\`\`catala
declaration scope Test:
  output x content integer

scope Test:
  definition x equals 1 +
\`\`\`
`;
  const result = exports.typecheck({
    files: { 'test.catala_en': code }
  });
  assertEquals(result.success, false, 'Should fail');
  assertEquals(getErrorPositions(result).length > 0, true, 'Should have error positions');
});

test('Typecheck: no scope needed', () => {
  // File with no scopes, just type declarations
  const code = `
\`\`\`catala
declaration structure Person:
  data age content integer
  data is_adult content boolean
\`\`\`
`;
  const result = exports.typecheck({
    files: { 'test.catala_en': code }
  });
  assertEquals(result.success, true, 'Should succeed without scopes');
});

test('Typecheck: multi-file success', () => {
  const helperModule = `
> Module Helpers

\`\`\`catala-metadata
declaration double
  content integer
  depends on x content integer
  equals x * 2
\`\`\`
`;

  const mainCode = `
> Using Helpers

\`\`\`catala
declaration scope Test:
  output result content integer

scope Test:
  definition result equals Helpers.double of 21
\`\`\`
`;
  const result = exports.typecheck({
    files: {
      'main.catala_en': mainCode,
      'helpers.catala_en': helperModule
    }
  });
  assertEquals(result.success, true, 'Should typecheck multi-file');
});

test('Typecheck: multi-file type error', () => {
  const helperModule = `
> Module Helpers

\`\`\`catala-metadata
declaration get_date
  content date
  depends on x content integer
  equals |2024-01-01|
\`\`\`
`;

  const mainCode = `
> Using Helpers

\`\`\`catala
declaration scope Test:
  output result content integer

scope Test:
  # Type error: get_date returns date, not integer
  definition result equals Helpers.get_date of 1
\`\`\`
`;
  const result = exports.typecheck({
    files: {
      'main.catala_en': mainCode,
      'helpers.catala_en': helperModule
    }
  });
  assertEquals(result.success, false, 'Should fail with type error');
  assertContains(getErrorText(result), 'date', 'Should mention date');
  assertContains(getErrorText(result), 'integer', 'Should mention integer');
});

test('Typecheck: three files with chain', () => {
  const baseModule = `
> Module Base

\`\`\`catala-metadata
declaration add_one
  content integer
  depends on x content integer
  equals x + 1
\`\`\`
`;

  const middleModule = `
> Module Middle

> Using Base

\`\`\`catala-metadata
declaration add_two
  content integer
  depends on x content integer
  equals Base.add_one of (Base.add_one of x)
\`\`\`
`;

  const mainCode = `
> Using Middle

\`\`\`catala
declaration scope Test:
  output result content integer

scope Test:
  definition result equals Middle.add_two of 10
\`\`\`
`;
  const result = exports.typecheck({
    files: {
      'main.catala_en': mainCode,
      'middle.catala_en': middleModule,
      'base.catala_en': baseModule
    },
    main: 'main.catala_en'
  });
  assertEquals(result.success, true, 'Should typecheck chain of dependencies');
});

test('Typecheck: French file', () => {
  const code = `
\`\`\`catala
déclaration champ d'application Test:
  résultat montant contenu argent

champ d'application Test:
  définition montant égal à 1 234,56 €
\`\`\`
`;
  const result = exports.typecheck({
    files: { 'test.catala_fr': code },
    language: 'fr'
  });
  assertEquals(result.success, true, 'Should typecheck French');
});

// Summary of hand-written tests
console.log(`\n${passed} passed, ${failed} failed`);

// ============================================================================
// Conformance tests: discover tests/*/good/ and tests/*/bad/ automatically
// ============================================================================

const testsRoot = path.resolve(__dirname, '../../tests');

// Discover test directories dynamically
const testDirs = fs.readdirSync(testsRoot).filter(d => {
  try { return fs.statSync(path.join(testsRoot, d)).isDirectory(); }
  catch (_) { return false; }
}).sort();

// Detect language from file extension
function languageOf(file) {
  const m = file.match(/\.catala_(\w+)$/);
  return m ? m[1] : 'en';
}

// Skip files that require multi-file loading (> Using / > Include)
function needsMultiFile(content) {
  return /^> (?:Using|Include)\b/m.test(content);
}

// Parse test directives with state-tracking that matches clerk's lexer.
// Returns [{kind, scope, expectsError, expectedOutput, contentUpTo}] where
// kind is 'interpret' or 'typecheck'. contentUpTo is the file content before
// the test block — matching how clerk feeds only accumulated lines to catala.
//
// Clerk's lexer only recognises ```catala-test-cli when NOT inside a code
// block, and code blocks only close with ``` at column 0.  We replicate this
// so that files with intentional formatting errors (like extra_space.catala_en)
// are handled identically.
function parseTestDirectives(content) {
  const results = [];
  const lines = content.split('\n');
  let inCodeBlock = false;
  let inTestBlock = false;
  let testBlockLines = [];
  let testBlockStart = 0; // char offset of ```catala-test-cli line
  let charOffset = 0;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const lineStart = charOffset;
    charOffset += line.length + 1; // +1 for \n

    if (inTestBlock) {
      if (/^```\s*$/.test(line)) {
        // End of test block — parse the directive
        const block = testBlockLines.join('\n');
        const contentUpTo = content.substring(0, testBlockStart);
        const firstLine = testBlockLines.find(l => l.trim() !== '') || '';
        const directiveRe = /^\$ catala test-scope (\S+)\s*$/;
        const typecheckRe = /^\$ catala [Tt]ypecheck\s*$/;
        const dm = directiveRe.exec(firstLine);
        const tm = typecheckRe.exec(firstLine);
        if (dm || tm) {
          const afterDirective = block.substring(block.indexOf(firstLine) + firstLine.length);
          results.push({
            kind: dm ? 'interpret' : 'typecheck',
            scope: dm ? dm[1] : null,
            expectsError: block.includes('[ERROR]'),
            expectedOutput: afterDirective.trim(),
            contentUpTo,
          });
        }
        inTestBlock = false;
        testBlockLines = [];
      } else {
        testBlockLines.push(line);
      }
    } else if (inCodeBlock) {
      // Only ``` at column 0 closes a code block (matching clerk's strict lexer)
      if (/^```\s*$/.test(line)) {
        inCodeBlock = false;
      }
    } else {
      // Outside any block
      if (/^```catala-test-cli\s*$/.test(line)) {
        inTestBlock = true;
        testBlockStart = lineStart;
        testBlockLines = [];
      } else if (/^```catala/.test(line)) {
        inCodeBlock = true;
      }
    }
  }
  return results;
}

// --- Conformance test helpers ---

// Check if an error/exception is an unsupported jsoo stub (e.g. Unix calls)
function isUnsupported(x) {
  const msg = x instanceof Error ? x.message : typeof x === 'string' ? x : '';
  return msg.includes('caml_unix_');
}

// Compare actual vs expected output; returns null on match or a failure message
function outputMismatch(actual, expected) {
  if (actual === expected) return null;
  const a = actual.split('\n'), e = expected.split('\n');
  const lines = [];
  for (let i = 0; i < Math.max(a.length, e.length) && lines.length < 3; i++) {
    if (a[i] !== e[i])
      lines.push(`  line ${i + 1}: expected ${JSON.stringify(e[i] || '(missing)')}, got ${JSON.stringify(a[i] || '(missing)')}`);
  }
  return lines.join('\n');
}

// Build the actual output string from a result, to compare against cram expected
function buildActualOutput(result) {
  const warningText = getWarnings(result).map(d => d.message).join('');
  if (result.success) {
    const fullOutput = warningText + result.output.trim();
    return fullOutput.trim();
  } else {
    return (warningText + getErrorText(result)).trim();
  }
}

// Strip the #return code N# trailer that clerk appends for non-zero exits.
// The web interpreter has no exit codes, so we normalise the expected output.
function stripReturnCode(expected) {
  return expected.replace(/\n#return code \d+#$/, '');
}

// Run a single conformance directive; returns 'pass', 'fail', or 'skip'
function runConformanceTest(testName, directive, relPath, language) {
  const { kind, scope, expectsError, expectedOutput, contentUpTo } = directive;
  let result;
  try {
    result = kind === 'interpret'
      ? exports.interpret({ files: { [relPath]: contentUpTo }, scope, language })
      : exports.typecheck({ files: { [relPath]: contentUpTo }, language });
  } catch (e) {
    if (isUnsupported(e)) return 'skip';
    // Uncaught exception counts as failure (expected for bad tests)
    if (expectsError) return 'pass';
    console.log(`✗ ${testName}\n  Exception: ${e.message || e}`);
    return 'fail';
  }

  if (isUnsupported(getErrorText(result))) return 'skip';

  // Check success/failure matches expectation
  const shouldSucceed = !expectsError;
  if (result.success !== shouldSucceed) {
    const what = shouldSucceed ? 'expected success, got failure' : 'expected failure, got success';
    console.log(`✗ ${testName} (${what})`);
    return 'fail';
  }

  // Compare output against cram expected
  if (expectedOutput) {
    const actual = buildActualOutput(result);
    const cleaned = stripReturnCode(expectedOutput);
    const expected = cleaned;
    const diff = outputMismatch(actual, expected);
    if (diff) {
      console.log(`✗ ${testName} (output mismatch)\n${diff}`);
      return 'fail';
    }
  }
  return 'pass';
}

// --- Conformance test loop ---

const counts = { passed: 0, failed: 0, skipped: 0 };

for (const subdir of ['good', 'bad']) {
  let subPassed = 0, subFailed = 0, subSkipped = 0;
  console.log(`\n--- Conformance: ${subdir} tests ---`);

  for (const dir of testDirs) {
    const fullDir = path.join(testsRoot, dir, subdir);
    if (!fs.existsSync(fullDir)) continue;

    const files = fs.readdirSync(fullDir).filter(f => /\.catala_\w+$/.test(f)).sort();
    for (const file of files) {
      const content = fs.readFileSync(path.join(fullDir, file), 'utf8');
      if (needsMultiFile(content)) { subSkipped++; continue; }

      let directives = parseTestDirectives(content);
      // Good tests: only interpret (typecheck-only files have no scope to run)
      if (subdir === 'good') directives = directives.filter(d => d.kind === 'interpret');
      if (directives.length === 0) { subSkipped++; continue; }

      const language = languageOf(file);
      const relPath = `tests/${dir}/${subdir}/${file}`;

      for (const d of directives) {
        const label = d.kind === 'interpret' ? d.scope : 'typecheck';
        const testName = `${dir}/${subdir}/${file} :: ${label}`;
        const outcome = runConformanceTest(testName, d, relPath, language);
        if (outcome === 'pass') subPassed++;
        else if (outcome === 'fail') subFailed++;
        else subSkipped++;
      }
    }
  }
  console.log(`\n${subdir}: ${subPassed} passed, ${subFailed} failed, ${subSkipped} skipped`);
  counts.passed += subPassed;
  counts.failed += subFailed;
  counts.skipped += subSkipped;
}

// Final summary
const totalFailed = failed + counts.failed;
console.log(`\nTotal: ${passed + counts.passed} passed, ${totalFailed} failed`);
process.exit(totalFailed > 0 ? 1 : 0);
