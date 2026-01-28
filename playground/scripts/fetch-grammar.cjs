#!/usr/bin/env node
/**
 * Fetches Catala keywords from the tree-sitter-catala grammar
 * and generates js/grammar.js for the playground's syntax highlighting.
 *
 * Run: node scripts/fetch-grammar.cjs
 * Or:  make grammar
 */

const https = require('https');
const fs = require('fs');
const path = require('path');

const GRAMMAR_URL = 'https://raw.githubusercontent.com/CatalaLang/tree-sitter-catala/master/grammar.js';
const OUTPUT_PATH = path.join(__dirname, '..', 'js', 'grammar.js');

function fetch(url) {
  return new Promise((resolve, reject) => {
    https.get(url, (res) => {
      if (res.statusCode !== 200) {
        reject(new Error(`HTTP ${res.statusCode}`));
        return;
      }
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => resolve(data));
    }).on('error', reject);
  });
}

/**
 * Extract tokens_local object from the grammar source.
 * This is a simple regex-based extraction that works for the current format.
 */
function extractTokensLocal(source) {
  // Find the tokens_local object
  const match = source.match(/const tokens_local = \{([\s\S]*?)\n\}/);
  if (!match) {
    throw new Error('Could not find tokens_local in grammar source');
  }

  const content = match[1];
  const result = { en: {}, fr: {}, pl: {} };

  // Parse each language block
  for (const lang of ['en', 'fr', 'pl']) {
    const langMatch = content.match(new RegExp(`${lang}: \\{([\\s\\S]*?)\\n  \\}`, 'm'));
    if (!langMatch) continue;

    const langContent = langMatch[1];
    // Extract simple string keywords (not regex patterns)
    const keywordMatches = langContent.matchAll(/(\w+): ["']([^"']+)["']/g);
    for (const [, key, value] of keywordMatches) {
      result[lang][key] = value;
    }
  }

  return result;
}

/**
 * Convert tokens to simple keyword arrays for Monaco syntax highlighting.
 * Only includes single-word keywords (no spaces/regex).
 */
function tokensToKeywordArrays(tokens) {
  const result = {};
  for (const lang of Object.keys(tokens)) {
    result[lang] = Object.values(tokens[lang])
      .filter(v => typeof v === 'string' && !v.includes(' ') && !v.includes('\\'))
      .sort();
  }
  return result;
}

async function main() {
  console.log(`Fetching grammar from ${GRAMMAR_URL}...`);

  try {
    const source = await fetch(GRAMMAR_URL);
    const tokens = extractTokensLocal(source);
    const keywords = tokensToKeywordArrays(tokens);

    // Also extract multi-word keywords that we want to support
    const multiWordKeywords = {
      en: [],
      fr: ["champ d'application", "égal à"],
      pl: []
    };

    const output = `// Auto-generated from tree-sitter-catala grammar
// Source: ${GRAMMAR_URL}
// Generated: ${new Date().toISOString()}
// Run "make grammar" or "node scripts/fetch-grammar.cjs" to regenerate

/**
 * Catala keywords by language
 * @type {Record<string, string[]>}
 */
export const KEYWORDS = {
  en: ${JSON.stringify(keywords.en, null, 4).replace(/\n/g, '\n  ')},
  fr: ${JSON.stringify(keywords.fr, null, 4).replace(/\n/g, '\n  ')},
  pl: ${JSON.stringify(keywords.pl, null, 4).replace(/\n/g, '\n  ')}
};

/**
 * Multi-word keywords (require special regex handling)
 * @type {Record<string, string[]>}
 */
export const MULTI_WORD_KEYWORDS = {
  en: ${JSON.stringify(multiWordKeywords.en)},
  fr: ${JSON.stringify(multiWordKeywords.fr)},
  pl: ${JSON.stringify(multiWordKeywords.pl)}
};
`;

    fs.writeFileSync(OUTPUT_PATH, output);
    console.log(`Generated ${OUTPUT_PATH}`);
    console.log(`  English: ${keywords.en.length} keywords`);
    console.log(`  French: ${keywords.fr.length} keywords`);
    console.log(`  Polish: ${keywords.pl.length} keywords`);

  } catch (err) {
    console.error('Failed to fetch/parse grammar:', err.message);
    console.error('The playground will use fallback keywords from editor.js');
    process.exit(1);
  }
}

main();
