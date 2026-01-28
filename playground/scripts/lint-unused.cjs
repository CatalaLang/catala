#!/usr/bin/env node
/**
 * Lint for unused CSS rules and i18n keys
 * Run: node scripts/lint-unused.cjs
 *
 * NOTE: This is a quickly generated script for this small codebase.
 * If it becomes a maintenance burden (false positives, hard to extend),
 * consider replacing it with proper tools like PurgeCSS, eslint-plugin-unused-imports, etc.
 */

const fs = require('fs');
const path = require('path');

const ROOT = path.join(__dirname, '..');

// ============================================================================
// Unused CSS detection
// ============================================================================

function findUsedClasses() {
  const used = new Set();

  // Scan HTML files
  const htmlFiles = ['index.html', 'learn.html'];
  for (const file of htmlFiles) {
    const content = fs.readFileSync(path.join(ROOT, file), 'utf8');
    // class="..." attributes
    const classMatches = content.matchAll(/class="([^"]+)"/g);
    for (const match of classMatches) {
      match[1].split(/\s+/).forEach(c => used.add(c));
    }
    // id="..." attributes (for #id selectors)
    const idMatches = content.matchAll(/id="([^"]+)"/g);
    for (const match of idMatches) {
      used.add('#' + match[1]);
    }
  }

  // Scan JS files for classList and getElementById
  const jsDir = path.join(ROOT, 'js');
  const jsFiles = fs.readdirSync(jsDir).filter(f => f.endsWith('.js'));
  for (const file of jsFiles) {
    const content = fs.readFileSync(path.join(jsDir, file), 'utf8');
    // classList.add/remove/toggle('class')
    const classListMatches = content.matchAll(/classList\.(add|remove|toggle)\(['"]([^'"]+)['"]\)/g);
    for (const match of classListMatches) {
      used.add(match[2]);
    }
    // className = 'class' or className assignments (including .className = and className:)
    const classNameMatches = content.matchAll(/\.?className\s*[=:]+\s*['"]([^'"]+)['"]/g);
    for (const match of classNameMatches) {
      match[1].split(/\s+/).forEach(c => used.add(c));
    }
    // className with template expression: 'file-tab' + (condition ? ' active' : '')
    const templateConcatMatches = content.matchAll(/className\s*=\s*['"]([^'"]+)['"]\s*\+/g);
    for (const match of templateConcatMatches) {
      match[1].split(/\s+/).forEach(c => used.add(c));
    }
    // getElementById('id')
    const idMatches = content.matchAll(/getElementById\(['"]([^'"]+)['"]\)/g);
    for (const match of idMatches) {
      used.add('#' + match[1]);
    }
    // querySelector with class or id
    const selectorMatches = content.matchAll(/querySelector(?:All)?\(['"]([^'"]+)['"]\)/g);
    for (const match of selectorMatches) {
      // Extract classes and ids from selector
      const classes = match[1].matchAll(/\.([a-zA-Z_-][a-zA-Z0-9_-]*)/g);
      for (const c of classes) used.add(c[1]);
      const ids = match[1].matchAll(/#([a-zA-Z_-][a-zA-Z0-9_-]*)/g);
      for (const id of ids) used.add('#' + id[1]);
    }
    // class="..." in template strings and innerHTML
    const templateClassMatches = content.matchAll(/class=\\?["']([^"']+)\\?["']/g);
    for (const match of templateClassMatches) {
      match[1].split(/\s+/).forEach(c => used.add(c));
    }
    // Monaco decoration class names (e.g., { className: 'x', inlineClassName: 'y' })
    const monacoClassMatches = content.matchAll(/(?:className|inlineClassName|glyphMarginClassName):\s*['"]([^'"]+)['"]/g);
    for (const match of monacoClassMatches) {
      used.add(match[1]);
    }
  }

  return used;
}

function findDefinedCSS() {
  const defined = new Map(); // selector -> line number
  const cssFile = path.join(ROOT, 'css', 'style.css');
  const content = fs.readFileSync(cssFile, 'utf8');
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    // Skip lines that are property values (contain ':' before the selector-like thing)
    // Only look at selector lines (typically don't have ':' or have it after '{')
    const colonIndex = line.indexOf(':');
    const braceIndex = line.indexOf('{');
    const isPropertyLine = colonIndex !== -1 && (braceIndex === -1 || colonIndex < braceIndex);
    if (isPropertyLine) continue;

    // Match class selectors
    const classMatches = line.matchAll(/\.([a-zA-Z_-][a-zA-Z0-9_-]*)/g);
    for (const match of classMatches) {
      if (!defined.has(match[1])) {
        defined.set(match[1], i + 1);
      }
    }
    // Match id selectors (but not hex colors - those have 3, 4, 6, or 8 hex digits)
    const idMatches = line.matchAll(/#([a-zA-Z][a-zA-Z0-9_-]*)/g);
    for (const match of idMatches) {
      // Skip if it looks like a hex color
      if (/^[0-9a-fA-F]{3,8}$/.test(match[1])) continue;
      if (!defined.has('#' + match[1])) {
        defined.set('#' + match[1], i + 1);
      }
    }
  }

  return defined;
}

function checkUnusedCSS() {
  const used = findUsedClasses();
  const defined = findDefinedCSS();
  const unused = [];

  for (const [selector, line] of defined) {
    // Skip pseudo-classes and element selectors mixed in
    if (selector.includes(':')) continue;
    if (!used.has(selector) && !used.has(selector.replace('#', ''))) {
      unused.push({ selector, line });
    }
  }

  return unused;
}

// ============================================================================
// Unused i18n keys detection
// ============================================================================

function findDefinedI18nKeys() {
  const i18nFile = path.join(ROOT, 'js', 'i18n.js');
  const content = fs.readFileSync(i18nFile, 'utf8');

  // Extract keys from the 'en' section (assuming en and fr have same keys)
  const keys = new Map(); // key -> line number
  const lines = content.split('\n');
  let inEnSection = false;
  let braceDepth = 0;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    if (line.includes('en: {')) {
      inEnSection = true;
      braceDepth = 1;
      continue;
    }
    if (inEnSection) {
      braceDepth += (line.match(/{/g) || []).length;
      braceDepth -= (line.match(/}/g) || []).length;
      if (braceDepth <= 0) {
        inEnSection = false;
        continue;
      }
      // Match key: 'value' or key: "value"
      const match = line.match(/^\s*([a-zA-Z_][a-zA-Z0-9_]*):\s*['"]/);
      if (match) {
        keys.set(match[1], i + 1);
      }
    }
  }

  return keys;
}

function findUsedI18nKeys() {
  const used = new Set();
  const jsDir = path.join(ROOT, 'js');
  const jsFiles = fs.readdirSync(jsDir).filter(f => f.endsWith('.js'));

  for (const file of jsFiles) {
    const content = fs.readFileSync(path.join(jsDir, file), 'utf8');
    // t('key') or t("key")
    const matches = content.matchAll(/\bt\(['"]([^'"]+)['"]/g);
    for (const match of matches) {
      used.add(match[1]);
    }
  }

  return used;
}

function checkUnusedI18n() {
  const defined = findDefinedI18nKeys();
  const used = findUsedI18nKeys();
  const unused = [];

  for (const [key, line] of defined) {
    if (!used.has(key)) {
      unused.push({ key, line });
    }
  }

  return unused;
}

// ============================================================================
// Main
// ============================================================================

let hasErrors = false;

console.log('Checking for unused CSS selectors...');
const unusedCSS = checkUnusedCSS();
if (unusedCSS.length > 0) {
  hasErrors = true;
  for (const { selector, line } of unusedCSS) {
    console.log(`  css/style.css:${line} - unused selector: ${selector}`);
  }
} else {
  console.log('  No unused CSS selectors found.');
}

console.log('\nChecking for unused i18n keys...');
const unusedI18n = checkUnusedI18n();
if (unusedI18n.length > 0) {
  hasErrors = true;
  for (const { key, line } of unusedI18n) {
    console.log(`  js/i18n.js:${line} - unused key: ${key}`);
  }
} else {
  console.log('  No unused i18n keys found.');
}

process.exit(hasErrors ? 1 : 0);
