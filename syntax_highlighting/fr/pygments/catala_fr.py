from pygments.lexer import RegexLexer, bygroups
from pygments.token import *

import re

__all__ = ['CatalaFrLexer']


class CatalaFrLexer(RegexLexer):
    name = 'CatalaFr'
    aliases = ['catala_fr']
    filenames = ['*.catala_fr']
    flags = re.MULTILINE | re.UNICODE

    tokens = {
        'root': [
            (u'(@@)', bygroups(Generic.Heading), 'main__1'),
            (u'(@)', bygroups(Generic.Heading), 'main__2'),
            (u'([^\\/\\n\\r])', bygroups(Text)),
            (u'(\\/\\*)', bygroups(Text), 'code'),
            ('(\n|\r|\r\n)', Text),
            ('.', Text),
        ],
        'code': [
            (u'(\\*\\/)', bygroups(Text), 'root'),
            (u'(\\s*\\#.*$)', bygroups(Comment.Single)),
            (u'(contexte)(\\s+)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xd4\xca\u0152\xc70-9_\\\']*)',
             bygroups(Keyword.Declaration, String, Name.Variable)),
            (u'\\b(selon|sous\\s+forme|fix\xe9|par|d\xe9croissante|croissante|varie|avec|on\\s+a|dans|tel\\s+que|existe|pour|tout|de|si|alors|sinon|initial)\\b', bygroups(Keyword.Reserved)),
            (u'\\b(champ\\s+d\'application|si\\s+et\\s+seulement\\s+si|d\xe9pend\\s+de|d\xe9claration|inclus|collection|contenu|optionnel|structure|\xe9num\xe9ration|contexte|r\xe8gle|sous\\s+condition|condition|donn\xe9e|cons\xe9quence|rempli|\xe9gal\\s+\xe0|assertion|d\xe9finition|\xe9tiquette|exception)\\b', bygroups(Keyword.Declaration)),
            (u'(\\|[0-9]+/[0-9]+/[0-9]+\\|)', bygroups(Number.Integer)),
            (u'\\b(vrai|faux)\\b', bygroups(Keyword.Constant)),
            (u'\\b([0-9]+(,[0.9]*|))\\b', bygroups(Number.Integer)),
            (u'(\\-\\-|\\;|\\.|\\,|\\:|\\(|\\)|\\[|\\]|\\{|\\})', bygroups(
                Operator)),
            (u'(\\-\\>|\\+\\.|\\+\\@|\\+\\^|\\+\u20ac|\\+|\\-\\.|\\-\\@|\\-\\^|\\-\u20ac|\\-|\\*\\.|\\*\\@|\\*\\^|\\*\u20ac|\\*|/\\.|/\\@|/\\^|/\u20ac|/|\\!|>\\.|>=\\.|<=\\.|<\\.|>\\@|>=\\@|<=\\@|<\\@|>\u20ac|>=\u20ac|<=\u20ac|<\u20ac|>\\^|>=\\^|<=\\^|<\\^|>|>=|<=|<|=|non|ou|et|\u20ac|%|an|mois|jour)', bygroups(Operator)),
            (u'\\b(entier|bool\xe9en|date|argent|texte|d\xe9cimal|d\xe9cret|loi|nombre|somme)\\b',
             bygroups(Keyword.Type)),
            (u'\\b([A-Z\xc9\xc8\xc0\xc2\xd9\xce\xd4\xca\u0152\xc7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xd4\xca\u0152\xc70-9_\\\']*)(\\.)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xd4\xca\u0152\xc70-9_\\\']*)\\b', bygroups(Name.Class, Operator, Name.Variable)),
            (u'\\b([a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xd4\xca\u0152\xc70-9_\\\']*)(\\.)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xd4\xca\u0152\xc70-9_\\\'\\.]*)\\b', bygroups(Name.Variable, Operator, Text)),
            (u'\\b([a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xd4\xca\u0152\xc70-9_\\\']*)\\b', bygroups(Name.Variable)),
            (u'\\b([A-Z\xc9\xc8\xc0\xc2\xd9\xce\xd4\xca\u0152\xc7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xf4\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xd4\xca\u0152\xc70-9_\\\']*)\\b', bygroups(Name.Class)),
            ('(\n|\r|\r\n)', Text),
            ('.', Text),
        ],
        'main__1': [
            (u'(@@)', bygroups(Generic.Heading), 'root'),
            (u'(.)', bygroups(Generic.Heading)),
            ('(\n|\r|\r\n)', Text),
            ('.', Text),
        ],
        'main__2': [
            (u'(@)', bygroups(Generic.Heading), 'root'),
            (u'(.)', bygroups(Generic.Heading)),
            ('(\n|\r|\r\n)', Text),
            ('.', Text),
        ]
    }
