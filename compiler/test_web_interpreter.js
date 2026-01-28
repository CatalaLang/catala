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
  const result = exports.interpret(code, 'Test', 'en', false, '');
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
  const result = exports.interpret(code, 'Test', 'en', false, '');
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
  const result = exports.interpret(code, 'Test', 'en', false, '');
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
  const result = exports.interpret(code, 'Test', 'fr', false, '');
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
  const result = exports.interpret(code, 'Test', 'en', false, '');
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
  const result = exports.interpret(code, 'Test', 'en', false, '');
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
  const result = exports.interpret(code, 'WrongScope', 'en', false, '');
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
  const result = exports.interpret(code, 'Test', 'en', false, '');
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
  const result = exports.interpret(code, 'Test', 'fr', false, '');
  assertEquals(result.success, true, 'Should succeed');
  assertContains(result.output, 'vrai', 'Should output vrai');
});

test('Multi-file with module', () => {
  // Register helper module
  exports.clearFiles();
  exports.registerFile('helpers.catala_en', `
> Module Helpers

\`\`\`catala-metadata
declaration double
  content integer
  depends on x content integer
  equals x * 2
\`\`\`
`);

  // Main file using the module
  const mainCode = `
> Using Helpers

\`\`\`catala
declaration scope Test:
  output result content integer

scope Test:
  definition result equals Helpers.double of 21
\`\`\`
`;
  const result = exports.interpret(mainCode, 'Test', 'en', false, '');
  assertEquals(result.success, true, 'Should succeed');
  assertContains(result.output, 'result = 42', 'Should output 42 (21 * 2)');
});

test('Module declaration in main file', () => {
  // Files can now declare themselves as modules by passing the filename parameter
  exports.clearFiles();
  const mainCode = `
> Module Foo

\`\`\`catala
declaration scope Test:
  output x content integer

scope Test:
  definition x equals 42
\`\`\`
`;
  // Pass matching filename so module declaration works
  const result = exports.interpret(mainCode, 'Test', 'en', false, 'foo.catala_en');
  assertEquals(result.success, true, 'Should succeed with matching filename');
  assertContains(result.output, 'x = 42', 'Should output 42');
});

test('Module declaration mismatch', () => {
  // Module name must match filename
  exports.clearFiles();
  const mainCode = `
> Module Foo

\`\`\`catala
declaration scope Test:
  output x content integer

scope Test:
  definition x equals 42
\`\`\`
`;
  // Pass wrong filename - should fail
  const result = exports.interpret(mainCode, 'Test', 'en', false, 'bar.catala_en');
  assertEquals(result.success, false, 'Should fail with mismatched filename');
  assertContains(result.error, 'Module declared as Foo', 'Should mention module name mismatch');
});

test('Error positions with real filename', () => {
  // When passing a real filename, errorPositions should still be populated
  const code = `
\`\`\`catala
declaration scope Test:
  output x content integer

scope Test:
  definition x equals 1 +
\`\`\`
`;
  const result = exports.interpret(code, 'Test', 'en', false, 'myfile.catala_en');
  assertEquals(result.success, false, 'Should fail');
  assertEquals(result.errorPositions.length > 0, true, 'Should have error positions even with real filename');
});

// Summary
console.log(`\n${passed} passed, ${failed} failed`);
process.exit(failed > 0 ? 1 : 0);
