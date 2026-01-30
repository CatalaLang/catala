#!/usr/bin/env node
/**
 * Integration test for the Catala playground.
 * Launches a browser, loads the playground, and verifies code execution works.
 *
 * Prerequisites:
 *   npm install
 *   npx playwright install chromium
 *   catala_web_interpreter.js must exist
 *
 * Run: node scripts/integration-test.cjs
 * Or:  make test
 */

const { chromium } = require('playwright');
const { spawn } = require('child_process');
const path = require('path');

const PORT = 8081; // Use different port to avoid conflicts
const PLAYGROUND_ROOT = path.join(__dirname, '..');
const TIMEOUT = 30000;

const TEST_CODE = `
\`\`\`catala
declaration scope Test:
  output result content integer

scope Test:
  definition result equals 40 + 2
\`\`\`
`;

const MODULE_CODE = `> Module Helper

\`\`\`catala
declaration scope Helper:
  output value content integer

scope Helper:
  definition value equals 123
\`\`\`
`;

async function startServer() {
  return new Promise((resolve, reject) => {
    const server = spawn('python3', ['-m', 'http.server', String(PORT)], {
      cwd: PLAYGROUND_ROOT,
      stdio: ['ignore', 'pipe', 'pipe']
    });

    server.on('error', reject);

    // Wait for server to be ready
    setTimeout(() => resolve(server), 500);
  });
}

async function runTest() {
  let server;
  let browser;

  try {
    // Start server
    console.log(`Starting server on port ${PORT}...`);
    server = await startServer();

    // Launch browser
    console.log('Launching browser...');
    browser = await chromium.launch({ headless: true });
    const page = await browser.newPage();

    // Load playground
    console.log('Loading playground...');
    await page.goto(`http://localhost:${PORT}/index.html`, { timeout: TIMEOUT });

    // Wait for interpreter to be ready
    console.log('Waiting for interpreter...');
    await page.waitForFunction(
      () => document.getElementById('status')?.textContent?.includes('ready'),
      { timeout: TIMEOUT }
    );
    console.log('✓ Interpreter loaded');

    // Check Monaco editor exists
    const editor = await page.$('.monaco-editor');
    if (!editor) throw new Error('Monaco editor not found');
    console.log('✓ Monaco editor loaded');

    // Clear editor and type test code
    console.log('Entering test code...');
    await page.evaluate((code) => {
      // Access Monaco editor instance
      const editors = window.monaco?.editor?.getEditors();
      if (editors && editors[0]) {
        const editor = editors[0];
        editor.setValue(code);
        // Position cursor inside the scope (line 7)
        editor.setPosition({ lineNumber: 7, column: 1 });
        editor.focus();
      }
    }, TEST_CODE);

    // Small delay for editor to process
    await page.waitForTimeout(500);

    // Focus the editor container and trigger Ctrl+Enter
    console.log('Running code (Ctrl+Enter)...');
    await page.click('.monaco-editor');
    await page.waitForTimeout(100);
    await page.keyboard.press('Control+Enter');

    // Wait for output
    console.log('Waiting for output...');
    await page.waitForFunction(
      () => {
        const output = document.getElementById('output');
        const status = document.getElementById('status');
        return (output?.textContent?.includes('42') ||
                output?.textContent?.includes('result') ||
                status?.textContent?.includes('Error'));
      },
      { timeout: TIMEOUT }
    );

    // Check result
    const output = await page.$eval('#output', el => el.textContent);
    const status = await page.$eval('#status', el => el.textContent);

    if (output.includes('42')) {
      console.log('✓ Code executed successfully, got expected output (42)');
    } else if (status.includes('Error')) {
      throw new Error(`Execution failed: ${output}`);
    } else {
      throw new Error(`Unexpected output: ${output}`);
    }

    // ========================================================================
    // Test: File deletion
    // ========================================================================
    console.log('\n--- Testing file deletion ---');

    // Add a new module file
    console.log('Adding module file...');
    await page.click('.add-file-btn');
    await page.waitForTimeout(100);

    // Handle the prompt dialog
    page.once('dialog', async dialog => {
      await dialog.accept('Helper');
    });
    await page.click('.add-file-btn');
    await page.waitForTimeout(300);

    // Check that Helper.catala_en tab exists
    let tabs = await page.$$eval('.file-tab', els => els.map(el => el.dataset.file));
    if (!tabs.includes('Helper.catala_en')) {
      throw new Error(`Module file not created. Tabs: ${tabs.join(', ')}`);
    }
    console.log('✓ Module file created');

    // Click on Helper tab to make it active
    await page.click('.file-tab[data-file="Helper.catala_en"]');
    await page.waitForTimeout(200);

    // Verify it's active
    const activeTab = await page.$eval('.file-tab.active', el => el.dataset.file);
    if (activeTab !== 'Helper.catala_en') {
      throw new Error(`Expected Helper.catala_en to be active, got ${activeTab}`);
    }
    console.log('✓ Module file is active');

    // Delete the file (click the × button) - handle confirm dialog
    page.once('dialog', async dialog => {
      await dialog.accept();
    });
    await page.click('.file-tab[data-file="Helper.catala_en"] .close-btn');
    await page.waitForTimeout(300);

    // Verify the file is gone
    tabs = await page.$$eval('.file-tab', els => els.map(el => el.dataset.file));
    if (tabs.includes('Helper.catala_en')) {
      throw new Error(`File deletion failed! Helper.catala_en still exists. Tabs: ${tabs.join(', ')}`);
    }
    console.log('✓ Module file deleted successfully');

    // Verify editor switched to main and shows main's content (not Helper's)
    const editorContent = await page.evaluate(() => {
      const editors = window.monaco?.editor?.getEditors();
      return editors?.[0]?.getValue() || '';
    });
    if (editorContent.includes('Module Helper')) {
      throw new Error('Editor still shows deleted file content!');
    }
    if (!editorContent.includes('scope Test')) {
      throw new Error(`Editor should show main file content, got: ${editorContent.slice(0, 100)}...`);
    }
    console.log('✓ Editor shows main file content after deletion');

    console.log('\n✓ All integration tests passed!');

  } finally {
    if (browser) await browser.close();
    if (server) server.kill();
  }
}

runTest().catch(err => {
  console.error('\n✗ Integration test failed:', err.message);
  process.exit(1);
});
