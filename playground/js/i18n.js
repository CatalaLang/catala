// @ts-check
/**
 * Internationalization support for Catala playground
 */

/** @type {Record<string, Record<string, string>>} */
const STRINGS = {
  en: {
    // index.html static text
    pageTitle: 'Catala Playground',
    codePanel: 'Catala Code',
    outputPanel: 'Output',
    resetBtn: '↺ Reset',
    clearBtn: 'Clear',
    loadingIndicator: 'Running...',
    loadingInterpreter: 'Loading Catala interpreter...',

    // Status messages
    running: 'Running {scope}...',
    ready: 'Ready',
    errorSeeOutput: 'Error — see output',
    interpreterReady: 'Interpreter ready! Click "▶ Run" above a scope declaration or press Ctrl+Enter',
    interpreterFailed: 'Failed to load interpreter. Make sure catala_web_interpreter.js is in the same folder.',

    // Checkpoint operations
    confirmReset: 'Reset to original checkpoint? Your changes will be lost.',
    resetting: 'Resetting to checkpoint...',
    resetComplete: 'Reset complete! Click "▶ Run" above a scope declaration or press Ctrl+Enter',
    resetFailed: 'Failed to reset: {error}',
    loadingCode: 'Loading code from URL...',
    loadFailed: 'Failed to load: {error}',

    // Solution viewer
    showSolution: 'Show solution',
    hideSolution: 'Hide solution',
    loadingSolution: 'Loading...',
    solutionFailed: 'Failed to load solution: {error}',

    // Editor
    runScope: '▶ Run {scope}',
    runScopeAtCursor: 'Run Scope at Cursor',
    scopeHasInputs: 'Scope has inputs — use a Test scope to run it',

    // File management
    confirmDelete: 'Delete {filename}?',
    promptFilename: 'Enter module filename (e.g., mymodule):',
    addModuleFile: 'Add module file',
  },
  fr: {
    // index.html static text
    pageTitle: 'Bac à sable Catala',
    codePanel: 'Code Catala',
    outputPanel: 'Résultat',
    resetBtn: '↺ Réinitialiser',
    clearBtn: 'Effacer',
    loadingIndicator: 'Exécution...',
    loadingInterpreter: 'Chargement de l\'interpréteur Catala...',

    // Status messages
    running: 'Exécution de {scope}...',
    ready: 'Prêt',
    errorSeeOutput: 'Erreur — voir le résultat',
    interpreterReady: 'Interpréteur prêt ! Cliquez sur "▶ Exécuter" ou appuyez sur Ctrl+Entrée',
    interpreterFailed: 'Échec du chargement de l\'interpréteur. Vérifiez que catala_web_interpreter.js est présent.',

    // Checkpoint operations
    confirmReset: 'Réinitialiser au point de départ ? Vos modifications seront perdues.',
    resetting: 'Réinitialisation...',
    resetComplete: 'Réinitialisation terminée ! Cliquez sur "▶ Exécuter" ou appuyez sur Ctrl+Entrée',
    resetFailed: 'Échec de la réinitialisation : {error}',
    loadingCode: 'Chargement du code...',
    loadFailed: 'Échec du chargement : {error}',

    // Solution viewer
    showSolution: 'Voir la solution',
    hideSolution: 'Masquer la solution',
    loadingSolution: 'Chargement...',
    solutionFailed: 'Échec du chargement de la solution : {error}',

    // Editor
    runScope: '▶ Exécuter {scope}',
    runScopeAtCursor: 'Exécuter le champ d\'application',
    scopeHasInputs: 'Ce champ d\'application a des entrées — utilisez un scope Test',

    // File management
    confirmDelete: 'Supprimer {filename} ?',
    promptFilename: 'Nom du fichier module (ex: monmodule) :',
    addModuleFile: 'Ajouter un fichier module',
  }
};

/** @type {'en' | 'fr'} */
let currentLang = 'en';

/**
 * Set the current language
 * @param {string} lang
 */
export function setLang(lang) {
  currentLang = lang === 'fr' ? 'fr' : 'en';
}

/**
 * Get the current language
 * @returns {'en' | 'fr'}
 */
export function getLang() {
  return currentLang;
}

/**
 * Translate a key, with optional interpolation
 * @param {string} key
 * @param {Record<string, string>} [params]
 * @returns {string}
 */
export function t(key, params) {
  let str = STRINGS[currentLang]?.[key] || STRINGS.en[key] || key;
  if (params) {
    for (const [k, v] of Object.entries(params)) {
      str = str.replace(`{${k}}`, v);
    }
  }
  return str;
}

/**
 * Initialize language from hash parameters
 * Checks for explicit lang param, or infers from file extensions
 */
export function initLangFromHash() {
  const hash = window.location.hash.slice(1);
  const params = new URLSearchParams(hash);

  // Explicit lang param takes precedence
  const langParam = params.get('lang');
  if (langParam) {
    setLang(langParam);
    return;
  }

  // Infer from codeUrl extension
  const codeUrl = params.get('codeUrl');
  if (codeUrl && codeUrl.includes('.catala_fr')) {
    setLang('fr');
    return;
  }

  // Default to English
  setLang('en');
}

/**
 * Update static UI text in index.html based on current language
 */
export function updateStaticText() {
  // Page title
  document.title = t('pageTitle');

  // Panel headers
  const panels = document.querySelectorAll('.panel-header > span:first-child');
  if (panels[0]) panels[0].textContent = t('codePanel');
  if (panels[1]) panels[1].textContent = t('outputPanel');

  // Buttons
  const resetBtn = document.getElementById('resetBtn');
  if (resetBtn) resetBtn.textContent = t('resetBtn');

  const clearBtn = document.getElementById('clearBtn');
  if (clearBtn) clearBtn.textContent = t('clearBtn');

  // Loading indicator
  const loading = document.getElementById('loading');
  if (loading) loading.textContent = t('loadingIndicator');

  // Initial status
  const status = document.getElementById('status');
  if (status && status.textContent?.includes('Loading')) {
    status.textContent = t('loadingInterpreter');
  }
}
