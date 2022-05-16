from pygments.lexer import RegexLexer, bygroups
from pygments.token import *

import re

__all__ = ['CatalaEnLexer']


class CatalaEnLexer(RegexLexer):
    name = 'CatalaEn'
    aliases = ['catala_en']
    filenames = ['*.catala_en']
    flags = re.MULTILINE | re.UNICODE

    tokens = {
        'root': [
            (u'(^[\#]+)', bygroups(Generic.Heading), 'main__1'),
            (u'(^[\#]+\s*\[[^\]]\s*])', bygroups(Generic.Heading), 'main__2'),
            (u'([^`\\n\\r])', bygroups(Text)),
            (u'(```catala-metadata)', bygroups(Text), 'code'),
            (u'(```catala)', bygroups(Text), 'code'),
            ('(\n|\r|\r\n)', Text),
            ('.', Text),
        ],
        'code': [
            (u'(```)', bygroups(Text), 'root'),
            (u'(\\s*\\#.*$)', bygroups(Comment.Single)),
            (u'(context|input|output|internal)(\\s*)(|output)(\\s+)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)',
             bygroups(Keyword.Declaration, Text, Keyword.Declaration, Text, Name.Variable)),
            (u'\\b(match|with\\s+pattern|fixed|by|decreasing|increasing|varies|with|we\\s+have|in|such\\s+that|exists|for|all|of|if|then|else|initial)\\b',
             bygroups(Keyword.Reserved)),
            (u'\\b(scope|depends\\s+on|declaration|includes|collection|content|optional|structure|enumeration|context|input|output|internal|rule|under\\s+condition|condition|data|consequence|fulfilled|equals|assertion|definition|state|label|exception|anything)\\b',
             bygroups(Keyword.Declaration)),
            (u'(\\|[0-9]+\\-[0-9]+\\-[0-9]+\\|)', bygroups(Number.Integer)),
            (u'\\b(true|false)\\b', bygroups(Keyword.Constant)),
            (u'\\b([0-9]+(,[0-9]*|))\\b', bygroups(Number.Integer)),
            (u'(\\-\\-|\\;|\\.|\\,|\\:|\\(|\\)|\\[|\\]|\\{|\\})',
             bygroups(Operator)),
            (u'(\\-\\>|\\+\\.|\\+\\@|\\+\\^|\\+\\$|\\+|\\-\\.|\\-\\@|\\-\\^|\\-\\$|\\-|\\*\\.|\\*\\@|\\*\\^|\\*\\$|\\*|/\\.|/\\@|/\\^|/\\$|/|\\!|>\\.|>=\\.|<=\\.|<\\.|>\\@|>=\\@|<=\\@|<\\@|>\\$|>=\\$|<=\\$|<\\$|>\\^|>=\\^|<=\\^|<\\^|>|>=|<=|<|=|not|or|xor|and|\\$|%|year|month|day)',
             bygroups(Operator)),
            (u'\\b(integer|boolean|date|duration|money|text|decimal|number|sum)\\b',
             bygroups(Keyword.Type)),
            (u'\\b([A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)(\\.)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)\\b',
             bygroups(Name.Class, Operator, Name.Variable)),
            (u'\\b([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)(\\.)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\'\\.]*)\\b',
             bygroups(Name.Variable, Operator, Text)),
            (u'\\b([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)\\b',
             bygroups(Name.Variable)),
            (u'\\b([A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)\\b',
             bygroups(Name.Class)),
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
