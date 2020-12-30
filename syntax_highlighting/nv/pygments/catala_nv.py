from pygments.lexer import RegexLexer, bygroups
from pygments.token import *

import re

__all__ = ['CatalaNvLexer']


class CatalaNvLexer(RegexLexer):
    name = 'CatalaNv'
    aliases = ['catala_nv']
    filenames = ['*.catala']
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
            (u'(\\s*\\#.*$)', bygroups(Comment.Single)),
            (u'(param)(\\s+)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)',
             bygroups(Keyword.Declaration, Text, Name.Variable)),
            (u'\\b(match|with|fixed|by|decreasing|increasing|varies|with\\s+param|we\\s+have|in|such\\s+that|exists|for|all|of|if|then|else)\\b', bygroups(Keyword.Reserved)),
            (u'\\b(scope|fun\\s+of|new|includes|set|content|option|struct|enum|param|rule|condition|data|ok|assert|def|label|exception)\\b',
             bygroups(Keyword.Declaration)),
            (u'(\\|[0-9]+/[0-9]+/[0-9]+\\|)', bygroups(Number.Integer)),
            (u'\\b(true|false)\\b', bygroups(Keyword.Constant)),
            (u'\\b([0-9]+(,[0.9]*|))\\b', bygroups(Number.Integer)),
            (u'(\\-\\-|\\;|\\.|\\,|\\:=|\\:|\\(|\\)|\\[\\||\\|\\]|\\[|\\]|\\{|\\})',
             bygroups(Operator)),
            (u'(\\-\\>|\\+\\.|\\+\\@|\\+\\^|\\+\\$|\\+|\\-\\.|\\-\\@|\\-\\^|\\-\\$|\\-|\\*\\.|\\*\\@|\\*\\^|\\*\\$|\\*|/\\.|/\\@|/\\^|/\\$|/|\\!|>\\.|>=\\.|<=\\.|<\\.|>\\@|>=\\@|<=\\@|<\\@|>\\$|>=\\$|<=\\$|<\\$|>\\^|>=\\^|<=\\^|<\\^|>|>=|<=|<|=|not|or|and|\\$|%|year|month|day)', bygroups(Operator)),
            (u'\\b(int|bool|date|money|text|decimal|number|sum)\\b',
             bygroups(Keyword.Type)),
            (u'\\b([A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)(\\.)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)\\b', bygroups(Name.Class, Operator, Name.Variable)),
            (u'\\b([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)(\\.)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\'\\.]*)\\b', bygroups(Name.Variable, Operator, Text)),
            (u'\\b([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)\\b', bygroups(Name.Variable)),
            (u'\\b([A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)\\b', bygroups(Name.Class)),
            ('(\n|\r|\r\n)', Text),
            ('.', Text),
        ],
        'main__1': [
            (u'(.)', bygroups(Generic.Heading)),
            ('(\n|\r|\r\n)', Text),
            ('.', Text),
        ],
        'main__2': [
            (u'(.)', bygroups(Generic.Heading)),
            ('(\n|\r|\r\n)', Text),
            ('.', Text),
        ]
    }
