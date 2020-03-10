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
        #     (u'(\\/\\*)', bygroups(Generic.Prompt), 'main__3'),
        #     ('(\n|\r|\r\n)', String),
        #     ('.', String),
        # ],
        # 'main__1' : [
        #     (u'(@@)', bygroups(Generic.Heading), '#pop'),
        #     (u'(.)', bygroups(Generic.Heading)),
        #     ('(\n|\r|\r\n)', String),
        #     ('.', String),
        # ],
        # 'main__2' : [
        #     (u'(@)', bygroups(Generic.Heading), '#pop'),
        #     (u'(.)', bygroups(Generic.Heading)),
        #     ('(\n|\r|\r\n)', String),
        #     ('.', String),
        # ],
        # 'main__3' : [
        #     (u'(\\*\\/)', bygroups(Generic.Prompt), '#pop'),
            (u'(\\s*\\#.*$)', bygroups(Comment.Single)),
            (u'(donn\xe9e|constante)(\\s+)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)', bygroups(Keyword.Declaration, String, Name.Variable)),
            (u'\\b(existe|selon|sous\\s+forme|fix\xe9\\s+par|d\xe9croissante|croissante|varie\\s+avec|on\\s+a|tel\\s+que|pour\\s+tout)\\b', bygroups(Keyword)),
            (u'\\b(choix|situation|donn\xe9e|optionnel|source|de|type|r\xe8gle|d\xe9fini|comme|fonction|param\xe8tres|renvoie|assertion|cons\xe9quence|condition|collection|constante)\\b', bygroups(Keyword.Declaration)),
            (u'(\\-\\-|\\;|\\.|\\,|\\:|\\(|\\))', bygroups(Operator)),
            (u'(\\+|\\-|\\*|/|\\!|ou|et|=|>|<)', bygroups(Operator)),
            (u'\\b(entier|bool\xe9en|date|montant|d\xe9cret|loi|an|mois|jour)\\b', bygroups(Keyword.Type)),
            (u'\\b([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)\\b', bygroups(Text)),
            (u'\\b([A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)\\b', bygroups(Name.Class)),
            ('(\n|\r|\r\n)', String),
            ('.', String),
        ]
    }
