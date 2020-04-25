from pygments.lexer import RegexLexer, bygroups
from pygments.token import *

import re

__all__=['CatalaLexer']

class CatalaLexer(RegexLexer):
    name = 'Catala'
    aliases = ['catala']
    filenames = ['*.catala']
    flags = re.MULTILINE | re.UNICODE

    tokens = {
        'root' : [
        #     (u'(@@)', bygroups(Generic.Heading), 'main__1'),
        #     (u'(@)', bygroups(Generic.Heading), 'main__2'),
        #     (u'([^\\/\\n\\r])', bygroups(Text.Doc)),
        #     (u'(\\/\\*)', bygroups(Comment.Special), 'code'),
        #     ('(\n|\r|\r\n)', Text),
        #     ('.', Text),
        # ],
        # 'code' : [
            (u'(\\s*\\#.*$)', bygroups(Comment.Single)),
            (u'(contexte)(\\s+)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)', bygroups(Keyword.Declaration, Text, Name.Variable)),
            (u'(donn\xe9e|condition)(\\s+)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)', bygroups(Keyword.Declaration, Text, Text)),
            (u'\\b(selon|sous\\s+forme|fix\xe9|par|d\xe9croissante|croissante|varie|avec|on\\s+a|dans|tel\\s+que|existe|pour|tout|de|si|alors|sinon)\\b', bygroups(Keyword.Reserved)),
            (u'\\b(champ\\s+d\'application|si\\s+et\\s+seulement\\s+si|d\xe9pend\\s+de|d\xe9claration|inclus|collection|contenu|optionnel|structure|\xe9num\xe9ration|contexte|r\xe8gle|sous condition|cons\xe9quence|rempli|\xe9gal\\s+\xe0|assertion|d\xe9finition)\\b', bygroups(Keyword.Declaration)),
            (u'(\\|[0-9]+/[0-9]+/[0-9]+\\|)', bygroups(Number.Integer)),
            (u'\\b(vrai|faux)\\b', bygroups(Keyword.Constant)),
            (u'\\b([0-9]+(\\.[0.9]*|))\\b', bygroups(Number.Integer)),
            (u'(\\-\\-|\\;|\\.|\\,|\\:|\\(|\\))', bygroups(Operator)),
            (u'(\\-\\>|\\+|\\-|\\*|/|\\!|non|ou|et|=|>|<|\u20ac|%|an|mois|jour)', bygroups(Operator)),
            (u'\\b(entier|bool\xe9en|date|montant|texte|d\xe9cimal|d\xe9cret|loi|nombre|somme|date_aujourd_hui)\\b', bygroups(Keyword.Type)),
            (u'\\b([A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)(\\.)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)\\b', bygroups(Name.Class, Operator, Name.Variable)),
            (u'\\b([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)(\\.)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\'\\.]*)\\b', bygroups(Name.Variable, Operator, Text)),
            (u'\\b([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)\\b', bygroups(Name.Variable)),
            (u'\\b([A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)\\b', bygroups(Name.Class)),
            ('(\n|\r|\r\n)', Text),
            ('.', Text),
        ],
        # 'main__1' : [
        #     (u'(.)', bygroups(Generic.Heading)),
        #     ('(\n|\r|\r\n)', Text),
        #     ('.', Text),
        # ],
        # 'main__2' : [
        #     (u'(.)', bygroups(Generic.Heading)),
        #     ('(\n|\r|\r\n)', Text),
        #     ('.', Text),
        # ]
    }
