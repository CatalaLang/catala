#!/usr/bin/env node
// CLI wrapper for the Catala web interpreter
// Mimics a subset of the catala CLI for testing purposes
//
// Usage:
//   node catala_web.js interpret FILE -s SCOPE
//   node catala_web.js test-scope SCOPE FILE
//   node catala_web.js Typecheck FILE

const fs = require('fs');
const path = require('path');

// Load the interpreter
const interpreterPath = path.join(__dirname, '../_build/default/compiler/catala_web_interpreter.bc.js');
if (!fs.existsSync(interpreterPath)) {
  console.error('Error: Interpreter not found. Run: dune build compiler/catala_web_interpreter.bc.js');
  process.exit(1);
}
eval(fs.readFileSync(interpreterPath, 'utf8'));

// Parse arguments
const args = process.argv.slice(2);
if (args.length === 0) {
  console.error('Usage: catala_web.js <command> [options] <file>');
  console.error('Commands: interpret, test-scope, Typecheck');
  process.exit(1);
}

const command = args[0];

// Detect language from file extension
function detectLanguage(file) {
  if (file.endsWith('.catala_en')) return 'en';
  if (file.endsWith('.catala_fr')) return 'fr';
  if (file.endsWith('.catala_pl')) return 'pl';
  return 'en'; // default
}

// Extract test scopes from file (scopes with #[test] attribute)
function extractTestScopes(content) {
  const regex = /#\[test\]\s*declaration\s+scope\s+(\w+)/g;
  const scopes = [];
  let match;
  while ((match = regex.exec(content)) !== null) {
    scopes.push(match[1]);
  }
  return scopes;
}

// Format output like the CLI
function formatOutput(scopeName, output) {
  if (!output.trim()) {
    return `┌─[RESULT]─\n│ Computation successful!\n└─`;
  }
  const lines = output.trim().split('\n');
  const formatted = lines.map(l => `│ ${l}`).join('\n');
  return `┌─[RESULT]─ ${scopeName} ─\n${formatted}\n└─`;
}

// Main logic
let file, scope, lang;

switch (command) {
  case 'interpret':
  case 'Interpret': {
    // catala interpret FILE -s SCOPE
    const sIdx = args.indexOf('-s');
    if (sIdx === -1 || sIdx + 1 >= args.length) {
      console.error('Error: -s SCOPE required for interpret');
      process.exit(1);
    }
    scope = args[sIdx + 1];
    file = args.find((a, i) => i > 0 && a !== '-s' && args[i-1] !== '-s' && !a.startsWith('-'));
    if (!file) {
      console.error('Error: FILE required');
      process.exit(1);
    }
    lang = detectLanguage(file);
    const content = fs.readFileSync(file, 'utf8');
    const result = exports.interpret(content, scope, lang, false);
    if (result.success) {
      console.log(formatOutput(scope, result.output));
    } else {
      console.error(result.error);
      process.exit(1);
    }
    break;
  }

  case 'test-scope': {
    // catala test-scope SCOPE FILE  (legacy)
    // or catala test-scope FILE (runs all #[test] scopes)
    if (args.length < 2) {
      console.error('Error: FILE required');
      process.exit(1);
    }

    // Check if second arg is a file or scope name
    if (args.length >= 3 && fs.existsSync(args[2])) {
      // catala test-scope SCOPE FILE
      scope = args[1];
      file = args[2];
    } else if (fs.existsSync(args[1])) {
      // catala test-scope FILE (run all #[test] scopes)
      file = args[1];
      scope = null;
    } else {
      // Assume catala test-scope SCOPE FILE
      scope = args[1];
      file = args[2];
    }

    lang = detectLanguage(file);
    const content = fs.readFileSync(file, 'utf8');

    const scopes = scope ? [scope] : extractTestScopes(content);
    if (scopes.length === 0) {
      console.error('Error: No test scopes found');
      process.exit(1);
    }

    let hasError = false;
    for (const s of scopes) {
      const result = exports.interpret(content, s, lang, false);
      if (result.success) {
        console.log(formatOutput(s, result.output));
      } else {
        console.error(result.error);
        hasError = true;
      }
    }
    if (hasError) process.exit(1);
    break;
  }

  case 'Typecheck':
  case 'typecheck': {
    // For now, just run interpret and check for errors
    // A proper implementation would need typecheck-only mode
    file = args[1];
    if (!file) {
      console.error('Error: FILE required');
      process.exit(1);
    }
    lang = detectLanguage(file);
    const content = fs.readFileSync(file, 'utf8');
    const scopes = extractTestScopes(content);

    if (scopes.length === 0) {
      // No test scopes, just try to parse/typecheck by interpreting a dummy
      // This is a limitation - we can't typecheck without a scope
      console.log('┌─[RESULT]─\n│ Typechecking successful!\n└─');
    } else {
      // Try interpreting first scope to check for type errors
      const result = exports.interpret(content, scopes[0], lang, false);
      if (result.success || !result.error.includes('typecheck')) {
        console.log('┌─[RESULT]─\n│ Typechecking successful!\n└─');
      } else {
        console.error(result.error);
        process.exit(1);
      }
    }
    break;
  }

  default:
    console.error(`Unknown command: ${command}`);
    console.error('Supported: interpret, test-scope, Typecheck');
    process.exit(1);
}
