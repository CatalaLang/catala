// @ts-check
/**
 * UI management and output rendering
 */

import { getFileNames, switchToFile, addNewFile, deleteFile, currentFile, getMainFile } from './files.js';
import { t } from './i18n.js';

/**
 * Escape HTML special characters
 * @param {string} text
 * @returns {string}
 */
export function escapeHtml(text) {
  const div = document.createElement('div');
  div.textContent = text;
  return div.innerHTML;
}

/**
 * Render file tabs
 * @param {(filename: string) => void} onSwitchFile - Callback when switching files
 * @returns {void}
 */
export function renderTabs(onSwitchFile) {
  const tabsContainer = document.getElementById('fileTabs');
  if (!tabsContainer) return;

  tabsContainer.innerHTML = '';

  getFileNames().forEach(filename => {
    const tab = document.createElement('button');
    tab.className = 'file-tab' + (filename === currentFile ? ' active' : '');
    tab.dataset.file = filename;

    const nameSpan = document.createElement('span');
    nameSpan.textContent = filename;
    tab.appendChild(nameSpan);

    // Add close button for non-main files
    if (filename !== getMainFile()) {
      const closeBtn = document.createElement('span');
      closeBtn.className = 'close-btn';
      closeBtn.textContent = '×';
      closeBtn.onclick = (e) => {
        e.stopPropagation();
        if (confirm(t('confirmDelete', { filename }))) {
          deleteFile(filename);
          if (currentFile === filename) {
            onSwitchFile(getMainFile());
          } else {
            renderTabs(onSwitchFile);
          }
        }
      };
      tab.appendChild(closeBtn);
    }

    tab.onclick = () => onSwitchFile(filename);
    tabsContainer.appendChild(tab);
  });

  // Add button
  const addBtn = document.createElement('button');
  addBtn.className = 'add-file-btn';
  addBtn.textContent = '+';
  addBtn.title = t('addModuleFile');
  addBtn.onclick = () => {
    const name = prompt(t('promptFilename'));
    if (!name) return;
    const result = addNewFile(name);
    if ('error' in result) {
      alert(result.error);
      return;
    }
    onSwitchFile(result.filename);
  };
  tabsContainer.appendChild(addBtn);
}

/**
 * Display output in the output panel
 * @param {string} html - HTML content to display
 * @returns {void}
 */
export function displayOutput(html) {
  const output = document.getElementById('output');
  if (output) {
    output.innerHTML = html;
  }
}

/**
 * Show/hide loading indicator
 * @param {boolean} visible
 * @returns {void}
 */
export function setLoadingState(visible) {
  const loading = document.getElementById('loading');
  if (loading) {
    if (visible) {
      loading.classList.add('active');
    } else {
      loading.classList.remove('active');
    }
  }
}

/**
 * Update status message
 * @param {string} message
 * @param {'success' | 'error' | 'info'} [type='info']
 * @returns {void}
 */
export function setStatus(message, type = 'info') {
  const status = document.getElementById('status');
  if (status) {
    const className = type === 'success' ? 'success' : type === 'error' ? 'error' : '';
    status.innerHTML = `<span class="${className}">${escapeHtml(message)}</span>`;
  }
}
