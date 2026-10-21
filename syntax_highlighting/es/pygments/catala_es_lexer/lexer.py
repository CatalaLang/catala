import re

from pygments.lexer import RegexLexer
from pygments.token import Comment, Generic, Keyword, Name, Number, Operator, Text, Whitespace


__all__ = ["CustomLexer"]


class CustomLexer(RegexLexer):
    name = "CatalaEs"
    aliases = ["catala_es"]
    filenames = ["*.catala_es"]
    flags = re.MULTILINE | re.UNICODE

    tokens = {
        "root": [
            (r"^\s*#+.*$", Generic.Heading),
            (r"^```catala(?:-metadata)?$", Text, "code"),
            (r"^```catala-test-cli$", Text, "test"),
            (r"[^`\n\r]", Text),
            (r"\n|\r\n?", Whitespace),
            (r".", Text),
        ],
        "code": [
            (r"^```$", Text, "#pop"),
            (r"^\s*#.*$", Comment.Single),
            (r"\b(?:entrada|salida|interno)(?:\s+salida)?\s+\w+", Keyword.Declaration),
            (r"\b(?:ámbito|consecuencia|dato|depende\s+de|declaración|contexto|decreciente|creciente|lista\s+de|opcional\s+de|contiene|enumeración|suma|cumplida|definición|estado|etiqueta|excepción|igual\s+a|coincide|cualquiera|tipo|con\s+patrón|bajo\s+condición|condición|contenido|estructura|aserción|regla|sea|existe|entre|combinar|transformar\s+cada|ordenar|en\s+orden\s+(?:creciente|decreciente)|inicialmente|imposible)\b", Keyword.Reserved),
            (r"\b(?:si\s+no|si|entonces|o\s+bien|o|y|no|máximo|mínimo|es|pero\s+reemplazando|para|todos|en|tal|que|redondear|número)\b", Keyword.Declaration),
            (r"\b(?:entero|dinero|decimal|fecha|duración|booleano|posición_fuente)\b", Keyword.Type),
            (r"\b(?:verdadero|falso|Presente|Ausente)\b", Keyword.Constant),
            (r"\|\d+-\d+-\d+\|", Number.Integer),
            (r"\b\d+(?:[,.]\d+)?\b", Number),
            (r"€|%|->|[+*/<>=!:-]", Operator),
            (r"\b[A-ZÁÉÍÓÚÜÑ][\wÁÉÍÓÚÜÑáéíóúüñ']*\b", Name.Class),
            (r"\b[\wÁÉÍÓÚÜÑáéíóúüñ']+\b", Name.Variable),
            (r"\n|\r\n?", Whitespace),
            (r".", Text),
        ],
        "test": [
            (r"^```$", Text, "#pop"),
            (r"^\$ catala \S*", Keyword.Constant),
            (r"\n|\r\n?", Whitespace),
            (r".", Text),
        ],
    }
