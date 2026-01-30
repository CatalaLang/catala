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
 * Options for the interpret function
 */
export interface InterpretOptions {
  /** All source files as { filename: contents } - first file is main by default */
  files: Record<string, string>;
  /** Name of the scope to execute */
  scope: string;
  /** Language code ("en", "fr", "pl") - defaults to "en" */
  language?: string;
  /** Override which file is the main entry point */
  main?: string;
  /** Enable tracing (not typically used in web) */
  trace?: boolean;
}

/**
 * Catala interpreter functions exported to JavaScript via JSOO
 */
export interface CatalaInterpreter {
  /**
   * Interpret Catala source code and execute a scope
   * @param options - Interpretation options
   * @returns Result object with success status and output or error
   */
  interpret(options: InterpretOptions): InterpretResult;
}

declare global {
  interface Window extends CatalaInterpreter {}

  // Also available on globalThis
  var interpret: CatalaInterpreter['interpret'];
}

export {};
