from pygments.lexer import RegexLexer, bygroups
from pygments.token import *

import re

__all__=['CustomLexer']

class CustomLexer(RegexLexer):
    name = 'CatalaPl'
    aliases = ['catala_pl']
    filenames = ['*.catala_pl']
    flags = re.MULTILINE | re.UNICODE

    tokens = {
        'root' : [
            (u'(^\s*[\#]+)', bygroups(Generic.Heading), 'main__1'),
            (u'(^\s*[\#]+\s*\[[^\]]\s*])', bygroups(Generic.Heading), 'main__2'),
            (u'([^`\\n\\r])', bygroups(Text)),
            (u'(```catala-metadata)', bygroups(Text), 'code'),
            (u'(```catala)', bygroups(Text), 'code'),
            ('(\n|\r|\r\n)', Text),
            ('.', Text),
        ],
        'code' : [
            (u'(```)', bygroups(Text), 'root'),
            (u'(\\s*\\#.*$)', bygroups(Comment.Single)),
            (u'(kontekst|wej\u015bcie|wyj\u015bcie|wewn\u0119trzny)(\\s*)(|wyj\u015bcie)(\\s+)([a-z\u0105\u0107\u0119\u0142\u0144\xf3\u015b\u017c\u017a][a-z\u0105\u0107\u0119\u0142\u0144\xf3\u015b\u017c\u017aA-Z\u0104\u0106\u0118\u0141\u0143\xd3\u015a\u017b\u01790-9_\\\']*)', bygroups(Keyword.Declaration, Text, Keyword.Declaration, Text, Name.Variable)),
            (u'\\b(pasuje|ze\\s+wzorem|staloprzecinkowa|przez|malejacy|rosnacy|rozna|wraz z|mamy|w|takich ze|istnieje|dla|wszystkie|z|jezeli|wtedy|inaczej|poczatkowy)\\b', bygroups(Keyword.Reserved)),
            (u'\\b(zakres|zalezy\\s+od|deklaracja|kolekcja|typu|opcjonalny|struktura|enumeracja|kontekst|wej\u015bcie|wyj\u015bcie|wewn\u0119trzny|zasada|pod\\s+warunkuem|czas|konsekwencja|spelnione|wynosi|asercja|definicja|stan|etykieta|wyj\u0105tek|cokolwiek)\\b', bygroups(Keyword.Declaration)),
            (u'(\\|[0-9]+\\-[0-9]+\\-[0-9]+\\|)', bygroups(Number.Integer)),
            (u'\\b(prawda|falsz)\\b', bygroups(Keyword.Constant)),
            (u'\\b([0-9]+(,[0-9]*|))\\b', bygroups(Number.Integer)),
            (u'(\\-\\-|\\;|\\.|\\,|\\:|\\(|\\)|\\[|\\]|\\{|\\})', bygroups(Operator)),
            (u'(\\-\\>|\\+\\.|\\+\\@|\\+\\^|\\+\\$|\\+|\\-\\.|\\-\\@|\\-\\^|\\-\\$|\\-|\\*\\.|\\*\\@|\\*\\^|\\*\\$|\\*|/\\.|/\\@|/\\^|/\\$|/|\\!|>\\.|>=\\.|<=\\.|<\\.|>\\@|>=\\@|<=\\@|<\\@|>\\$|>=\\$|<=\\$|<\\$|>\\^|>=\\^|<=\\^|<\\^|>|>=|<=|<|=|nie|lub|xor|i|\\$|%|rok|miesiac|dzien)', bygroups(Operator)),
            (u'\\b(calkowita|zerojedynkowy|czas|czas trwania|pieniądze|warunek|tekst|dziesi\u0119tny|suma)\\b', bygroups(Keyword.Type)),
            (u'\\b([A-Z\u0104\u0106\u0118\u0141\u0143\xd3\u015a\u017b\u0179][a-z\u0105\u0107\u0119\u0142\u0144\xf3\u015b\u017c\u017aA-Z\u0104\u0106\u0118\u0141\u0143\xd3\u015a\u017b\u01790-9_\\\']*)(\\.)([a-z\u0105\u0107\u0119\u0142\u0144\xf3\u015b\u017c\u017a][a-z\u0105\u0107\u0119\u0142\u0144\xf3\u015b\u017c\u017aA-Z\u0104\u0106\u0118\u0141\u0143\xd3\u015a\u017b\u01790-9_\\\']*)\\b', bygroups(Name.Class, Operator, Name.Variable)),
            (u'\\b([a-z\u0105\u0107\u0119\u0142\u0144\xf3\u015b\u017c\u017a][a-z\u0105\u0107\u0119\u0142\u0144\xf3\u015b\u017c\u017aA-Z\u0104\u0106\u0118\u0141\u0143\xd3\u015a\u017b\u01790-9_\\\']*)(\\.)([a-z\u0105\u0107\u0119\u0142\u0144\xf3\u015b\u017c\u017a][a-z\u0105\u0107\u0119\u0142\u0144\xf3\u015b\u017c\u017aA-Z\u0104\u0106\u0118\u0141\u0143\xd3\u015a\u017b\u01790-9_\\\'\\.]*)\\b', bygroups(Name.Variable, Operator, Text)),
            (u'\\b([a-z\u0105\u0107\u0119\u0142\u0144\xf3\u015b\u017c\u017a][a-z\u0105\u0107\u0119\u0142\u0144\xf3\u015b\u017c\u017aA-Z\u0104\u0106\u0118\u0141\u0143\xd3\u015a\u017b\u01790-9_\\\']*)\\b', bygroups(Name.Variable)),
            (u'\\b([A-Z\u0104\u0106\u0118\u0141\u0143\xd3\u015a\u017b\u0179][a-z\u0105\u0107\u0119\u0142\u0144\xf3\u015b\u017c\u017aA-Z\u0104\u0106\u0118\u0141\u0143\xd3\u015a\u017b\u01790-9_\\\']*)\\b', bygroups(Name.Class)),
            ('(\n|\r|\r\n)', Text),
            ('.', Text),
        ],
        'main__1': [
            (u'(\n)', bygroups(Generic.Heading), 'root'),
            (u'(.)', bygroups(Generic.Heading)),
            ('(\n|\r|\r\n)', Text),
            ('.', Text),
        ],
        'main__2': [
            (u'(\n)', bygroups(Generic.Heading), 'root'),
            (u'(.)', bygroups(Generic.Heading)),
            ('(\n|\r|\r\n)', Text),
            ('.', Text),
        ]
    }

