// @ts-check
/**
 * Multi-file state management for Catala playground
 *
 * Language inference:
 * - The language is inferred from the first file's extension (main.catala_en → "en")
 * - All subsequent files must use the same language extension
 * - Supported: .catala_en, .catala_fr
 */

/**
 * @typedef {Object} FileState
 * @property {Record<string, string>} files - Map of filename to content
 * @property {string} currentFile - Currently active file
 */

/**
 * Module templates per language (Catala syntax varies by language)
 * @type {Record<string, {declaration: string, scope: string, comment: string}>}
 */
const MODULE_TEMPLATES = {
  en: {
    declaration: 'declaration',
    scope: 'scope',
    comment: '# Define your scope here'
  },
  fr: {
    declaration: 'déclaration',
    scope: 'champ d\'application',
    comment: '# Définissez votre champ d\'application ici'
  }
};

/** @type {Record<string, string>} */
export const files = {
  'main.catala_en': ''
};

/** @type {string} */
export let currentFile = 'main.catala_en';

/**
 * Get language code from filename extension
 * @param {string} filename
 * @returns {string | null} - Language code ("en", "fr", "pl") or null if invalid
 */
function getLanguageFromFilename(filename) {
  const match = filename.match(/\.catala_(en|fr)$/);
  return match ? match[1] : null;
}

/**
 * Get the project's language (inferred from first file)
 * @returns {string} - Language code ("en", "fr", "pl")
 */
export function getProjectLanguage() {
  const firstFile = Object.keys(files)[0];
  return getLanguageFromFilename(firstFile) || 'en';
}

/**
 * Get the file extension for the current project language
 * @returns {string} - Extension like ".catala_en"
 */
export function getProjectExtension() {
  const lang = getProjectLanguage();
  return `.catala_${lang}`;
}

/**
 * Switch to a different file
 * @param {string} filename
 * @param {(content: string) => void} updateEditor - Callback to update editor content
 * @returns {void}
 */
export function switchToFile(filename, updateEditor) {
  if (!files[filename]) return;
  currentFile = filename;
  updateEditor(files[filename]);
}

/**
 * Update content of current file
 * @param {string} content
 * @returns {void}
 */
export function updateCurrentFile(content) {
  files[currentFile] = content;
}

/**
 * Add a new file
 * @param {string} name - Base name (with or without extension)
 * @returns {{filename: string} | {error: string}} - Result with filename or error
 */
export function addNewFile(name) {
  const projectExt = getProjectExtension();

  // Auto-add correct extension if not present
  let filename = name;
  if (!name.match(/\.catala_(en|fr)$/)) {
    filename = name + projectExt;
  }

  // Validate extension matches project language
  const fileLang = getLanguageFromFilename(filename);
  const projectLang = getProjectLanguage();
  if (fileLang !== projectLang) {
    return {
      error: `File extension must be ${projectExt} to match project language (${projectLang})`
    };
  }

  if (files[filename]) {
    return { error: 'File already exists' };
  }

  const baseName = filename.replace(/\.catala_\w+$/, '');
  const moduleName = baseName.replace(/^./, c => c.toUpperCase());

  const lang = getProjectLanguage();
  const template = MODULE_TEMPLATES[lang] || MODULE_TEMPLATES.en;

  files[filename] = `> Module ${moduleName}

\`\`\`catala
${template.declaration} ${template.scope} ${moduleName}:
  ${template.comment}
\`\`\``;
  return { filename };
}

/**
 * Delete a file
 * @param {string} filename
 * @returns {boolean} - True if deleted, false if not allowed
 */
export function deleteFile(filename) {
  // Cannot delete the primary (main) file
  if (filename === getMainFile()) return false;
  delete files[filename];
  return true;
}

/**
 * Get all filenames
 * @returns {string[]}
 */
export function getFileNames() {
  return Object.keys(files);
}

/**
 * Get the primary (main) file - the first file in the project
 * @returns {string}
 */
export function getMainFile() {
  return Object.keys(files)[0];
}

/**
 * Get all files as a serializable object
 * @returns {{files: Record<string, string>, currentFile: string}}
 */
export function getAllFiles() {
  return { files: { ...files }, currentFile };
}

/**
 * Load all files from a serialized state
 * @param {{files: Record<string, string>, currentFile: string}} state
 */
export function loadAllFiles(state) {
  // Clear existing files
  for (const key of Object.keys(files)) {
    delete files[key];
  }
  // Load new files
  for (const [name, content] of Object.entries(state.files)) {
    files[name] = content;
  }
  currentFile = state.currentFile || Object.keys(files)[0];
}

/**
 * Get current file content
 * @returns {string}
 */
export function getCurrentFileContent() {
  return files[currentFile];
}

/**
 * Load a single file from a URL, replacing current workspace
 * @param {string} url - URL to fetch
 * @param {string} [filename] - Optional filename (inferred from URL if not provided)
 * @returns {Promise<{success: true, filename: string} | {success: false, error: string}>}
 */
export async function loadFromUrl(url, filename) {
  try {
    const response = await fetch(url);
    if (!response.ok) {
      return { success: false, error: `Failed to fetch: ${response.status} ${response.statusText}` };
    }
    const content = await response.text();

    // Infer filename from URL if not provided
    if (!filename) {
      const urlPath = new URL(url).pathname;
      filename = urlPath.split('/').pop() || 'main.catala_en';
    }

    // Clear existing files and load the new one
    for (const key of Object.keys(files)) {
      delete files[key];
    }
    files[filename] = content;
    currentFile = filename;

    return { success: true, filename };
  } catch (err) {
    return { success: false, error: err instanceof Error ? err.message : String(err) };
  }
}
