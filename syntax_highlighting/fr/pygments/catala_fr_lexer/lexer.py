from pygments.lexer import RegexLexer, bygroups
from pygments.token import *

import re

__all__=['CustomLexer']

class CustomLexer(RegexLexer):
    name = 'CatalaFr'
    aliases = ['catala_fr']
    filenames = ['*.catala_fr']
    flags = re.MULTILINE | re.UNICODE

    tokens = {
        'root' : [
            (u'(^\\s*[\\#]+.*)', bygroups(Generic.Heading)),
            (u'(^\\s*[\\#]+\\s*\\[[^\\]\\n\\r]\\s*].*)', bygroups(Generic.Heading)),
            (u'([^`\\n\\r])', bygroups(Text)),
            (u'(^```catala$)', bygroups(Text), 'code'),
            (u'(^```catala-metadata$)', bygroups(Text), 'code'),
            (u'(^```catala-test-cli$)', bygroups(Text), 'test'),
            ('(\n|\r|\r\n)', Whitespace),
            ('.', Text),
        ],
        'code' : [
            (u'(^```$)', bygroups(Text), 'root'),
            (u'(\\s*\\#(|[^[].*)$)', bygroups(Comment.Single)),
            (u'(\\s*\\#\\[([\\\\](.|\n)|[^]\\\\])*\\])', bygroups(Comment.Multi)),
            (u'(contexte|entr\xe9e|r\xe9sultat|interne)(\\s*)(|r\xe9sultat)(\\s+)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xd4\xca\u0152\xc70-9_\\\']*)', bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration, Whitespace, Name.Variable)),
            (u'\\b(selon|sous\\s+forme|mais\\s+en\\s+rempla\xe7ant|fix\xe9|par|d\xe9croissant|croissant|varie|avec|on\\s+a|soit|dans|champ\\s+d\'application|d\xe9pend\\s+de|d\xe9claration|inclusion|contenu|type|r\xe8gle|sous\\s+condition|condition|donn\xe9e|cons\xe9quence|rempli|\xe9gal\\s+\xe0|assertion|d\xe9finition|\xe9tat|\xe9tiquette|exception|n\'importe\\s+quel)\\b', bygroups(Keyword.Reserved)),
            (u'\\b(contient|nombre|somme|tel\\s+que|existe|pour|tout|de|si|alors|sinon|est|liste\\s+vide|parmi|maximum|minimum|arrondi|combine|transforme\\s+chaque|en|initialement|impossible)\\b', bygroups(Keyword.Declaration)),
            (u'(\\|[0-9]+\\-[0-9]+\\-[0-9]+\\|)', bygroups(Number.Integer)),
            (u'\\b(vrai|faux)\\b', bygroups(Keyword.Constant)),
            (u'\\b([0-9]+(,[0-9]*|))\\b', bygroups(Number.Integer)),
            (u'(\\-\\-|\\;|\\.|\\,|\\:|\\(|\\)|\\[|\\]|\\{|\\})', bygroups(Operator)),
            (u'(\\-\\>|\\+\\.|\\+\\@|\\+\\^|\\+\u20ac|\\+|\\-\\.|\\-\\@|\\-\\^|\\-\u20ac|\\-|\\*\\.|\\*\\@|\\*\\^|\\*\u20ac|\\*|/\\.|/\\@|/\u20ac|/|\\!|>\\.|>=\\.|<=\\.|<\\.|>\\@|>=\\@|<=\\@|<\\@|>\u20ac|>=\u20ac|<=\u20ac|<\u20ac|>\\^|>=\\^|<=\\^|<\\^|>|>=|<=|<|=|\u20ac|%)', bygroups(Operator)),
            (u'\\b(non|ou\\s+bien|ou|et|an|mois|jour)\\b', bygroups(Operator)),
            (u'\\b(structure|\xe9num\xe9ration|liste\\s+de|optionnel\\s+de|entier|bool\xe9en|date|dur\xe9e|argent|texte|d\xe9cimal|d\xe9cret|loi|nombre|somme)\\b', bygroups(Keyword.Type)),
            (u'\\b([A-Z\xc9\xc8\xc0\xc2\xd9\xce\xd4\xca\u0152\xc7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xd4\xca\u0152\xc70-9_\\\']*)(\\.)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xd4\xca\u0152\xc70-9_\\\']*)\\b', bygroups(Name.Class, Operator, Name.Variable)),
            (u'\\b([a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xd4\xca\u0152\xc70-9_\\\']*)(\\.)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xd4\xca\u0152\xc70-9_\\\'\\.]*)\\b', bygroups(Name.Variable, Operator, Text)),
            (u'\\b([a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xd4\xca\u0152\xc70-9_\\\']*)\\b', bygroups(Name.Variable)),
            (u'\\b([A-Z\xc9\xc8\xc0\xc2\xd9\xce\xd4\xca\u0152\xc7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xd4\xca\u0152\xc70-9_\\\']*)\\b', bygroups(Name.Class)),
            ('(\n|\r|\r\n)', Whitespace),
            ('.', Text),
        ],
        'test' : [
            (u'(^```$)', bygroups(Text), 'root'),
            (u'(^[$] catala \\S*)', bygroups(Keyword.Constant)),
            ('(\n|\r|\r\n)', Whitespace),
            ('.', Text),
        ]
    }
