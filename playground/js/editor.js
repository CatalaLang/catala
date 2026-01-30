// @ts-check
/**
 * Monaco editor setup and management
 * @typedef {import('monaco-editor').editor.IStandaloneCodeEditor} IStandaloneCodeEditor
 * @typedef {import('monaco-editor').editor.IModelDeltaDecoration} IModelDeltaDecoration
 * @typedef {import('monaco-editor').IDisposable} IDisposable
 */

import { updateCurrentFile } from './files.js';
import { t, getLang } from './i18n.js';
import { KEYWORDS } from './grammar.js';

/** @type {IStandaloneCodeEditor | null} */
let editor = null;

/** @type {string[]} */
let currentDecorations = [];

/** @type {IDisposable | null} */
let codeLensProvider = null;

/**
 * Debounce helper
 * @param {Function} fn
 * @param {number} delay
 * @returns {Function}
 */
function debounce(fn, delay) {
  /** @type {number | undefined} */
  let timeout;
  return function (/** @type {any[]} */ ...args) {
    clearTimeout(timeout);
    // @ts-ignore - context handling is intentional
    timeout = setTimeout(() => fn.apply(this, args), delay);
  };
}

// ============================================================================
// Scope detection patterns and helpers
// ============================================================================

const SCOPE_DECL_PATTERNS = [
  /declaration\s+scope\s+(\w+)/i,
  /déclaration\s+champ\s+d'application\s+(\w+)/i
];
const SCOPE_USE_PATTERNS = [
  /^scope\s+(\w+)\s*:/i,
  /^champ\s+d'application\s+(\w+)\s*:/i
];
const INPUT_PATTERN = /^\s*(input|entrée)\s+/i;
const END_BLOCK_PATTERN = /^(declaration|déclaration|scope|champ\s+d'application|```)/i;

/**
 * Check if a scope declaration has input fields
 * @param {string[]} lines - All lines of the document
 * @param {number} declLineIdx - Line index of the scope declaration
 * @returns {boolean}
 */
function scopeHasInputs(lines, declLineIdx) {
  for (let i = declLineIdx + 1; i < lines.length; i++) {
    const line = lines[i];
    if (END_BLOCK_PATTERN.test(line.trim())) {
      return false;
    }
    if (INPUT_PATTERN.test(line)) {
      return true;
    }
  }
  return false;
}

/**
 * Find the declaration line index for a scope name
 * @param {string[]} lines - All lines of the document
 * @param {string} scopeName - Name of the scope to find
 * @returns {number} - Line index or -1 if not found
 */
function findScopeDeclLine(lines, scopeName) {
  for (let i = 0; i < lines.length; i++) {
    for (const pattern of SCOPE_DECL_PATTERNS) {
      const match = lines[i].match(pattern);
      if (match && match[1] === scopeName) {
        return i;
      }
    }
  }
  return -1;
}

/**
 * Find scope name at or above given line
 * @param {import('monaco-editor').editor.ITextModel} model - Monaco model
 * @param {number} lineNumber
 * @returns {string | null}
 */
function findScopeAtLine(model, lineNumber) {
  const allPatterns = [...SCOPE_DECL_PATTERNS, ...SCOPE_USE_PATTERNS];
  for (let line = lineNumber; line >= 1; line--) {
    const text = model.getLineContent(line);
    for (const pattern of allPatterns) {
      const match = text.match(pattern);
      if (match) {
        return match[1];
      }
    }
  }
  return null;
}

/**
 * Check if a scope can be run (has no inputs)
 * @param {import('monaco-editor').editor.ITextModel} model - Monaco model
 * @param {string} scopeName - Name of the scope
 * @returns {boolean}
 */
function canRunScope(model, scopeName) {
  const lines = model.getValue().split('\n');
  const declLine = findScopeDeclLine(lines, scopeName);
  if (declLine === -1) return false;
  return !scopeHasInputs(lines, declLine);
}

// ============================================================================
// Language registration
// ============================================================================

/**
 * Get all keywords for syntax highlighting (both EN and FR for mixed-language support)
 * @returns {string[]}
 */
function getAllKeywords() {
  // Combine English and French keywords for highlighting
  // (users may have mixed-language code or switch languages)
  const allKeywords = new Set([
    ...(KEYWORDS.en || []),
    ...(KEYWORDS.fr || []),
    // Multi-word keywords that Monaco can match
    "d'application", "égal", "champ"
  ]);
  return [...allKeywords];
}

/**
 * Register Catala language with Monaco (syntax highlighting)
 * @param {typeof import('monaco-editor')} monaco
 */
function registerCatalaLanguage(monaco) {
  monaco.languages.register({ id: 'catala' });

  monaco.languages.setMonarchTokensProvider('catala', {
    keywords: getAllKeywords(),
    // Include accented characters for French support
    wordPattern: /[a-zA-ZàâäéèêëïîôùûüÿçœæÀÂÄÉÈÊËÏÎÔÙÛÜŸÇŒÆ_][a-zA-ZàâäéèêëïîôùûüÿçœæÀÂÄÉÈÊËÏÎÔÙÛÜŸÇŒÆ_0-9']*/,
    tokenizer: {
      root: [
        [/```catala/, 'keyword.fence'],
        [/```/, 'keyword.fence'],
        [/#.*$/, 'comment'],
        [/\|[\d-]+\|/, 'number.date'],
        [/\$[\d,._]+/, 'number.money'],
        [/\d+[\d,._]*/, 'number'],
        [/"[^"]*"/, 'string'],
        [/[A-ZÀÂÄÉÈÊËÏÎÔÙÛÜŸÇŒÆ][a-zA-ZàâäéèêëïîôùûüÿçœæÀÂÄÉÈÊËÏÎÔÙÛÜŸÇŒÆ_0-9]*/, 'type.identifier'],
        [/[a-zàâäéèêëïîôùûüÿçœæ_][a-zA-ZàâäéèêëïîôùûüÿçœæÀÂÄÉÈÊËÏÎÔÙÛÜŸÇŒÆ_0-9']*/, {
          cases: {
            '@keywords': 'keyword',
            '@default': 'identifier'
          }
        }]
      ]
    }
  });
}

// ============================================================================
// CodeLens provider
// ============================================================================

/**
 * Register CodeLens provider for "Run" buttons on scopes
 * @param {typeof import('monaco-editor')} monaco
 * @param {(scopeName: string) => void} onRunScope
 */
function registerCodeLens(monaco, onRunScope) {
  codeLensProvider = monaco.languages.registerCodeLensProvider('catala', {
    provideCodeLenses: function (model) {
      /** @type {import('monaco-editor').languages.CodeLens[]} */
      const lenses = [];
      const lines = model.getValue().split('\n');

      lines.forEach((line, idx) => {
        for (const pattern of SCOPE_DECL_PATTERNS) {
          const match = line.match(pattern);
          if (match) {
            const scopeName = match[1];
            if (!scopeHasInputs(lines, idx)) {
              lenses.push({
                range: {
                  startLineNumber: idx + 1,
                  startColumn: 1,
                  endLineNumber: idx + 1,
                  endColumn: 1
                },
                command: {
                  id: 'catala.runScope',
                  title: t('runScope', { scope: scopeName }),
                  arguments: [scopeName]
                }
              });
            }
            break;
          }
        }
      });

      return { lenses, dispose: () => {} };
    },
    resolveCodeLens: function (_model, codeLens) {
      return codeLens;
    }
  });

  monaco.editor.registerCommand('catala.runScope', function (_accessor, ...args) {
    const scopeName = /** @type {string} */ (args[0]);
    onRunScope(scopeName);
  });
}

// ============================================================================
// Keybindings
// ============================================================================

/**
 * Register editor keybindings (Ctrl+Enter to run scope)
 * @param {typeof import('monaco-editor')} monaco
 * @param {IStandaloneCodeEditor} ed
 * @param {(scopeName: string) => void} onRunScope
 */
function registerKeybindings(monaco, ed, onRunScope) {
  ed.addAction({
    id: 'catala.runScopeAtCursor',
    label: t('runScopeAtCursor'),
    keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter],
    run: function (editor) {
      const position = editor.getPosition();
      const model = editor.getModel();
      if (!position || !model) return;

      const scopeName = findScopeAtLine(model, position.lineNumber);
      if (scopeName && canRunScope(model, scopeName)) {
        onRunScope(scopeName);
      } else if (scopeName) {
        const status = document.getElementById('status');
        if (status) {
          status.textContent = t('scopeHasInputs');
        }
      }
      // Silent no-op if no scope found
    }
  });
}

// ============================================================================
// Public API
// ============================================================================

/**
 * Initialize Monaco editor
 * @param {string} initialContent
 * @param {(scopeName: string) => void} onRunScope - Callback to run a scope
 * @param {() => void} onContentChange - Callback when content changes
 * @returns {Promise<void>}
 */
export function initializeEditor(initialContent, onRunScope, onContentChange) {
  return new Promise((resolve) => {
    // @ts-ignore - AMD require loaded by Monaco
    globalThis.require.config({ paths: { vs: 'https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.45.0/min/vs' } });

    // @ts-ignore - AMD require loaded by Monaco
    globalThis.require(['vs/editor/editor.main'], function () {
      const monaco = /** @type {typeof import('monaco-editor')} */ (/** @type {any} */ (window).monaco);

      // Setup language
      registerCatalaLanguage(monaco);

      // Create editor
      const container = document.getElementById('editor-container');
      if (!container) throw new Error('Editor container not found');

      editor = monaco.editor.create(container, {
        value: initialContent,
        language: 'catala',
        theme: 'vs-dark',
        fontSize: 14,
        minimap: { enabled: false },
        lineNumbers: 'on',
        scrollBeyondLastLine: false,
        automaticLayout: true,
        wordWrap: 'on',
        padding: { top: 10 }
      });

      // Setup CodeLens and keybindings
      registerCodeLens(monaco, onRunScope);
      registerKeybindings(monaco, editor, onRunScope);

      // Track content changes
      editor.onDidChangeModelContent(/** @type {any} */ (debounce(() => {
        if (!editor) return;
        updateCurrentFile(editor.getValue());
        clearErrorDecorations();
        onContentChange();
      }, 300)));

      resolve();
    });
  });
}

/**
 * Update editor content
 * @param {string} content
 */
export function setEditorContent(content) {
  if (editor) {
    editor.setValue(content);
  }
}

/**
 * Get current editor content
 * @returns {string}
 */
export function getEditorContent() {
  return editor ? editor.getValue() : '';
}

/**
 * Clear error decorations
 */
export function clearErrorDecorations() {
  if (editor) {
    currentDecorations = editor.deltaDecorations(currentDecorations, []);
  }
}

/**
 * Mark error positions in the editor
 * @param {Array<{startLine: number, startColumn: number, endLine: number, endColumn: number, message?: string}>} positions
 */
export function markErrors(positions) {
  if (!editor) return;

  const monaco = /** @type {typeof import('monaco-editor')} */ (/** @type {any} */ (window).monaco);

  /** @type {import('monaco-editor').editor.IModelDeltaDecoration[]} */
  const decorations = positions.map(pos => ({
    range: new monaco.Range(
      pos.startLine,
      pos.startColumn,
      pos.endLine,
      pos.endColumn
    ),
    options: {
      inlineClassName: 'errorHighlight',
      hoverMessage: { value: pos.message || 'Error' },
      className: 'errorLine',
      glyphMarginClassName: 'errorGlyph',
      overviewRuler: {
        color: '#ff0000',
        position: monaco.editor.OverviewRulerLane.Full
      }
    }
  }));

  currentDecorations = editor.deltaDecorations(currentDecorations, decorations);
}
