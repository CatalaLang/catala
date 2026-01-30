// @ts-check
/**
 * Main entry point for Catala playground
 */

import { getCurrentFileContent, switchToFile, updateCurrentFile, loadFromUrl, loadAllFiles, getFileNames, currentFile as getCurrentFile } from './files.js';
import { initializeEditor, setEditorContent, getEditorContent, clearErrorDecorations, markErrors } from './editor.js';
import { loadInterpreter, runScope as executeScope } from './interpreter.js';
import { renderTabs, displayOutput, setLoadingState, setStatus, escapeHtml } from './ui.js';
import { initPersistence, scheduleSave, loadFromStorage, clearStorage } from './persistence.js';
import { t, initLangFromHash, updateStaticText } from './i18n.js';

// ============================================================================
// Checkpoint URLs (set during init)
// ============================================================================

/** @type {string | undefined} */
let checkpointUrl;

/** @type {string | undefined} */
let solutionUrl;

// ============================================================================
// Scope execution
// ============================================================================

/**
 * Run a specific scope
 * @param {string} scopeName
 */
function runScope(scopeName) {
  setLoadingState(true);
  setStatus(t('running', { scope: scopeName }), 'info');
  displayOutput('');
  clearErrorDecorations();

  updateCurrentFile(getEditorContent());

  setTimeout(() => {
    const result = executeScope(scopeName);

    if (result.success) {
      displayOutput(`<span class="success">Scope ${escapeHtml(scopeName)} executed successfully:\n\n</span>${escapeHtml(result.output || '')}`);
      setStatus(t('ready'), 'success');
    } else {
      displayOutput(`<span class="error">${escapeHtml(result.error || 'Unknown error')}</span>`);
      setStatus(t('errorSeeOutput'), 'error');
      if (result.errorPositions && result.errorPositions.length > 0) {
        markErrors(result.errorPositions);
      }
    }
    setLoadingState(false);
  }, 10);
}

// ============================================================================
// File management
// ============================================================================

/**
 * Handle file switching
 * @param {string} filename
 */
function onSwitchFile(filename) {
  // Only save if current file still exists (may have been deleted)
  if (getFileNames().includes(getCurrentFile)) {
    updateCurrentFile(getEditorContent());
  }
  switchToFile(filename, setEditorContent);
  renderTabs(onSwitchFile);
}

// ============================================================================
// Checkpoint operations
// ============================================================================

/**
 * Reset to checkpoint (re-fetch from URL or clear to empty)
 */
async function resetToCheckpoint() {
  if (!confirm(t('confirmReset'))) {
    return;
  }

  clearStorage();

  if (checkpointUrl) {
    setStatus(t('resetting'), 'success');
    const result = await loadFromUrl(checkpointUrl);
    if (result.success) {
      setEditorContent(getCurrentFileContent());
      renderTabs(onSwitchFile);
      setStatus(t('resetComplete'), 'success');
    } else {
      setStatus(t('resetFailed', { error: result.error || '' }), 'error');
    }
  } else {
    loadAllFiles({ files: { 'main.catala_en': '' }, currentFile: 'main.catala_en' });
    setEditorContent('');
    renderTabs(onSwitchFile);
    setStatus(t('resetComplete'), 'success');
  }
}

// ============================================================================
// Solution viewer
// ============================================================================

/** @type {import('monaco-editor').editor.IStandaloneCodeEditor | null} */
let solutionEditor = null;

/** @type {string | null} */
let solutionContent = null;

/** @type {boolean} */
let solutionVisible = false;

/**
 * Toggle solution visibility
 */
async function toggleSolution() {
  const container = document.getElementById('solutionContainer');
  const toggle = document.getElementById('solutionToggle');
  if (!container || !toggle || !solutionUrl) return;

  if (solutionVisible) {
    // Hide solution
    container.style.display = 'none';
    toggle.textContent = t('showSolution');
    solutionVisible = false;
  } else {
    // Show solution
    if (!solutionContent) {
      // Fetch solution on first open
      toggle.textContent = t('loadingSolution');
      try {
        const response = await fetch(solutionUrl);
        if (!response.ok) throw new Error(`HTTP ${response.status}`);
        solutionContent = await response.text();
      } catch (err) {
        setStatus(t('solutionFailed', { error: String(err) }), 'error');
        toggle.textContent = t('showSolution');
        return;
      }
    }

    container.style.display = 'block';
    toggle.textContent = t('hideSolution');
    solutionVisible = true;

    // Create read-only editor if needed
    if (!solutionEditor) {
      // @ts-ignore - Monaco loaded globally
      const monaco = window.monaco;
      solutionEditor = monaco.editor.create(document.getElementById('solution-editor'), {
        value: solutionContent,
        language: 'catala',
        theme: 'vs-dark',
        fontSize: 14,
        minimap: { enabled: false },
        lineNumbers: 'on',
        scrollBeyondLastLine: false,
        automaticLayout: true,
        wordWrap: 'on',
        readOnly: true,
        codeLens: false,
        padding: { top: 10 }
      });
    } else {
      solutionEditor.setValue(solutionContent);
    }
  }
}

// ============================================================================
// Initialization
// ============================================================================

/**
 * Parse hash parameters (e.g., #codeUrl=...&foo=bar)
 * @returns {URLSearchParams}
 */
function getHashParams() {
  const hash = window.location.hash.slice(1);
  return new URLSearchParams(hash);
}

/**
 * Initialize the playground
 */
async function init() {
  // Initialize i18n from hash parameters
  initLangFromHash();
  updateStaticText();

  // Parse URL parameters
  const hashParams = getHashParams();
  const codeUrl = hashParams.get('codeUrl') || undefined;
  const checkpointId = hashParams.get('checkpointId') || undefined;
  solutionUrl = hashParams.get('solutionUrl') || undefined;
  checkpointUrl = codeUrl;

  const persistEnabled = hashParams.get('persist') !== 'false';

  // Initialize persistence
  initPersistence(checkpointId, persistEnabled);

  // Try to load from localStorage first
  const loadedFromStorage = loadFromStorage();

  // If no saved state, load from URL (or start empty)
  if (!loadedFromStorage && codeUrl) {
    setStatus(t('loadingCode'), 'success');
    const result = await loadFromUrl(codeUrl);
    if (!result.success) {
      setStatus(t('loadFailed', { error: result.error || '' }), 'error');
    }
  }

  // Initialize Monaco editor
  await initializeEditor(
    getCurrentFileContent(),
    runScope,
    () => scheduleSave(getEditorContent, updateCurrentFile)
  );

  // Render initial tabs
  renderTabs(onSwitchFile);

  // Setup reset button
  const resetBtn = document.getElementById('resetBtn');
  if (resetBtn) {
    if (checkpointId) {
      resetBtn.style.display = 'inline-block';
      resetBtn.addEventListener('click', resetToCheckpoint);
    } else {
      resetBtn.style.display = 'none';
    }
  }

  // Setup solution toggle
  const solutionSection = document.getElementById('solutionSection');
  const solutionToggle = document.getElementById('solutionToggle');
  if (solutionSection && solutionToggle) {
    if (solutionUrl) {
      solutionSection.style.display = 'block';
      solutionToggle.textContent = t('showSolution');
      solutionToggle.addEventListener('click', toggleSolution);
    }
  }

  // Load interpreter
  loadInterpreter((ready) => {
    if (ready) {
      setStatus(t('interpreterReady'), 'success');
    } else {
      setStatus(t('interpreterFailed'), 'error');
    }
  });

  // Setup clear button
  const clearBtn = document.getElementById('clearBtn');
  if (clearBtn) {
    clearBtn.addEventListener('click', () => {
      displayOutput('');
      clearErrorDecorations();
    });
  }
}

// Start when DOM is ready
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', init);
} else {
  init();
}

// Reload when hash changes (enables exercise switching from learn.html)
window.addEventListener('hashchange', () => {
  location.reload();
});
