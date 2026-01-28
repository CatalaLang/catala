// @ts-check
/**
 * Catala interpreter wrapper
 * @typedef {import('./catala-jsoo').InterpretResult} InterpretResult
 */

import { files, getProjectLanguage, currentFile } from './files.js';

/** @type {boolean} */
export let interpreterReady = false;

/**
 * Load the interpreter script
 * @param {(ready: boolean) => void} onReady - Callback when interpreter is ready
 * @returns {void}
 */
export function loadInterpreter(onReady) {
  const script = document.createElement('script');
  script.src = 'catala_web_interpreter.js';
  script.onload = () => {
    if (typeof window.interpret === 'function') {
      interpreterReady = true;
      onReady(true);
    } else {
      onReady(false);
    }
  };
  script.onerror = () => {
    onReady(false);
  };
  document.body.appendChild(script);
}

/**
 * Run a specific scope
 * @param {string} scopeName
 * @returns {InterpretResult}
 */
export function runScope(scopeName) {
  if (!interpreterReady) {
    return {
      success: false,
      error: 'Interpreter not ready yet'
    };
  }

  try {
    // Get project language from first file's extension
    const lang = getProjectLanguage();

    // Register all OTHER files as modules (not the current file)
    // The current file is passed directly to interpret() with its actual filename
    window.clearFiles();
    Object.keys(files).forEach(filename => {
      if (filename !== currentFile) {
        window.registerFile(filename, files[filename]);
      }
    });

    // Run the current file as main, passing its actual filename
    // This allows the file to declare "> Module Name" matching its filename
    const result = window.interpret(files[currentFile], scopeName, lang, false, currentFile);
    return result;
  } catch (e) {
    return {
      success: false,
      error: (e instanceof Error ? e.message : String(e))
    };
  }
}
