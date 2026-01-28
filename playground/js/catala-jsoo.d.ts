/**
 * Type definitions for Catala JSOO (js_of_ocaml) web interpreter
 *
 * These functions are exported by catala_web_interpreter.ml and available
 * on the global scope after loading catala_web_interpreter.js
 */

/**
 * Result from interpreting Catala code
 */
export interface InterpretResult {
  /** Whether interpretation succeeded */
  success: boolean;
  /** Formatted output if successful */
  output?: string;
  /** Error message if failed */
  error?: string;
  /** Array of error positions for highlighting */
  errorPositions?: Array<{
    startLine: number;
    startColumn: number;
    endLine: number;
    endColumn: number;
    message?: string;
  }>;
}

/**
 * Catala interpreter functions exported to JavaScript via JSOO
 */
export interface CatalaInterpreter {
  /**
   * Register a module file in the virtual filesystem
   * @param filename - Name of the file (e.g., "helpers.catala_en")
   * @param contents - Content of the file
   */
  registerFile(filename: string, contents: string): boolean;

  /**
   * Clear all registered user module files
   */
  clearFiles(): boolean;

  /**
   * List all registered user module files
   * @returns Array of filenames
   */
  listFiles(): string[];

  /**
   * Interpret Catala source code and execute a scope
   * @param contents - Source code to interpret
   * @param scope - Name of the scope to execute
   * @param language - Language code ("en", "fr", "pl")
   * @param trace - Whether to enable tracing (not typically used in web)
   * @param filename - Filename for error messages and module resolution (empty string = "-inline-")
   * @returns Result object with success status and output or error
   */
  interpret(
    contents: string,
    scope: string,
    language: string,
    trace: boolean,
    filename: string
  ): InterpretResult;
}

declare global {
  interface Window extends CatalaInterpreter {}

  // Also available on globalThis
  var interpret: CatalaInterpreter['interpret'];
  var registerFile: CatalaInterpreter['registerFile'];
  var clearFiles: CatalaInterpreter['clearFiles'];
  var listFiles: CatalaInterpreter['listFiles'];
}

export {};
