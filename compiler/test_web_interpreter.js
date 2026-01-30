#!/usr/bin/env node
// Basic tests for the Catala web interpreter
// Run with: node test_web_interpreter.js

const fs = require('fs');
const path = require('path');

// Load the interpreter
const interpreterPath = path.join(__dirname, '../_build/default/compiler/catala_web_interpreter.bc.js');
if (!fs.existsSync(interpreterPath)) {
  console.error('ERROR: Interpreter not found at', interpreterPath);
  console.error('Run: dune build compiler/catala_web_interpreter.bc.js');
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
    console.log(`  ${e.message}`);
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
  assertEquals(result.errorPositions.length > 0, true, 'Should have error positions');
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
  assertContains(result.error, 'date', 'Should mention date type');
  assertContains(result.error, 'integer', 'Should mention integer type');
  assertEquals(result.errorPositions.length > 0, true, 'Should have error positions');
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
  assertContains(result.error, 'WrongScope', 'Should mention wrong scope');
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
  assertContains(result.error, 'Module declared as Foo', 'Should mention module name mismatch');
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
  assertEquals(result.errorPositions.length > 0, true, 'Should have error positions even with real filename');
});

test('Multi-file: main is not a module, scope in main', () => {
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
  const result = exports.interpret({
    files: {
      'main.catala_en': mainCode,
      'helpers.catala_en': helperModule
    },
    scope: 'Test',
    main: 'main.catala_en'
  });
  assertEquals(result.success, true, 'Should succeed');
  assertContains(result.output, 'result = 42', 'Should output 42');
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

// Summary
console.log(`\n${passed} passed, ${failed} failed`);
process.exit(failed > 0 ? 1 : 0);
