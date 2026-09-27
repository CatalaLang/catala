module type Language = sig
  val language : Language_t.language

  module Lexer : Surface.Lexer_common.LocalisedLexer
end
