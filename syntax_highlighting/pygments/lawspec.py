from pygments.lexer import RegexLexer, bygroups
from pygments.token import *

import re

__all__=['LawspecLexer']

class LawspecLexer(RegexLexer):
    name = 'Lawspec'
    aliases = ['lawspec']
    filenames = ['*.lawspec']
    flags = re.MULTILINE | re.UNICODE

    tokens = {
        'root' : [
        #     (u'(@@)', bygroups(Generic.Heading), 'main__1'),
        #     (u'(@)', bygroups(Generic.Heading), 'main__2'),
        #     (u'([^\\/\\n\\r])', bygroups(String.Doc)),
        #     (u'(\\/\\*)', bygroups(Comment.Special), 'code'),
        #     ('(\n|\r|\r\n)', String),
        #     ('.', String),
        # ],
        # 'code' : [
            (u'(\\s*\\#.*$)', bygroups(Comment.Single)),
            (u'(contexte)(\\s+)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)', bygroups(Keyword.Declaration, String, Name.Variable)),
            (u'(donn\xe9e|condition)(\\s+)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)', bygroups(Keyword.Declaration, String, Text)),
            (u'\\b(selon|sous\\s+forme|fix\xe9\\s+par|d\xe9croissante|croissante|varie\\s+avec|on\\s+a|dans|tel\\s+que|pour|tout|existe|de)\\b', bygroups(Keyword)),
            (u'\\b(champ\\s+d\'application|si\\s+et\\s+seulement\\s+si|d\xe9pend\\s+de|d\xe9claration|inclus|collection|contenu|optionnel|structure|\xe9num\xe9ration|contexte|r\xe8gle|sous condition|cons\xe9quence|sinon|rempli|\xe9gal\\s+\xe0|assertion|d\xe9finition)\\b', bygroups(Keyword.Declaration)),
            (u'\\b(vrai|faux)\\b', bygroups(Keyword.Constant)),
            (u'\\b([0-9]+(\\.[0.9]*|))\\b', bygroups(Number.Integer)),
            (u'(\\-\\-|\\;|\\.|\\,|\\:|\\(|\\))', bygroups(Operator)),
            (u'(\\+|\\-|\\*|/|\\!|non|ou|et|=|>|<|\u20ac|%)', bygroups(Operator)),
            (u'\\b(entier|bool\xe9en|date|d\xe9cimal|montant|d\xe9cret|loi|an|mois|jour|nombre|date_aujourd_hui|somme)\\b', bygroups(Keyword.Type)),
            (u'\\b([A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)(\\.)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)\\b', bygroups(Name.Class, Operator, Name.Variable)),
            (u'\\b([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)(\\.)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\'\\.]*)\\b', bygroups(Name.Variable, Operator, Text)),
            (u'\\b([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)\\b', bygroups(Name.Variable)),
            (u'\\b([A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)\\b', bygroups(Name.Class)),
            ('(\n|\r|\r\n)', String),
            ('.', String),
        ],
        # 'main__1' : [
        #     (u'(.)', bygroups(Generic.Heading)),
        #     ('(\n|\r|\r\n)', String),
        #     ('.', String),
        # ],
        # 'main__2' : [
        #     (u'(.)', bygroups(Generic.Heading)),
        #     ('(\n|\r|\r\n)', String),
        #     ('.', String),
        # ]
    }
