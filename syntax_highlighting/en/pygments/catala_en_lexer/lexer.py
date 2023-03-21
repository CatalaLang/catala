from pygments.lexer import RegexLexer, bygroups
from pygments.token import *

import re

__all__=['CustomLexer']

class CustomLexer(RegexLexer):
    name = 'CatalaEn'
    aliases = ['catala_en']
    filenames = ['*.catala_en']
    flags = re.MULTILINE | re.UNICODE

    tokens = {
        'root' : [
            (u'(^\\s*[\\#]+.*)', bygroups(Generic.Heading)),
            (u'(^\\s*[\\#]+\\s*\\[[^\\]\\n\\r]\\s*].*)', bygroups(Generic.Heading)),
            (u'([^`\\n\\r])', bygroups(String)),
            (u'(^```catala$)', bygroups(String), 'code'),
            (u'(^```catala-metadata$)', bygroups(String), 'code'),
            (u'(^```catala-test-inline$)', bygroups(String), 'test'),
            ('(\n|\r|\r\n)', String),
            ('.', String),
        ], 
        'code' : [
            (u'(^```$)', bygroups(String), 'root'),
            (u'(\\s*\\#.*$)', bygroups(Comment.Single)),
            (u'(context|input|output|internal)(\\s*)(|output)(\\s+)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)', bygroups(Keyword.Declaration, String, Keyword.Declaration, String, Name.Variable)),
            (u'\\b(match|with\\s+pattern|fixed|by|decreasing|increasing|varies|with|we\\s+have|let|in|scope|depends\\s+on|declaration|includes|content|rule|under\\s+condition|condition|data|consequence|fulfilled|equals|assertion|definition|state|label|exception)\\b', bygroups(Keyword.Reserved)),
            (u'\\b(contains|number|sum|such\\s+that|exists|for|all|of|if|then|else|is|empty|among|maximum|minimum|round)\\b', bygroups(Keyword.Declaration)),
            (u'(\\|[0-9]+\\-[0-9]+\\-[0-9]+\\|)', bygroups(Number.Integer)),
            (u'\\b(true|false)\\b', bygroups(Keyword.Constant)),
            (u'\\b([0-9]+(,[0-9]*|))\\b', bygroups(Number.Integer)),
            (u'(\\-\\-|\\;|\\.|\\,|\\:|\\(|\\)|\\[|\\]|\\{|\\})', bygroups(Operator)),
            (u'(\\-\\>|\\+\\.|\\+\\@|\\+\\^|\\+\\$|\\+|\\-\\.|\\-\\@|\\-\\^|\\-\\$|\\-|\\*\\.|\\*\\@|\\*\\^|\\*\\$|\\*|/\\.|/\\@|/\\$|/|\\!|>\\.|>=\\.|<=\\.|<\\.|>\\@|>=\\@|<=\\@|<\\@|>\\$|>=\\$|<=\\$|<\\$|>\\^|>=\\^|<=\\^|<\\^|>|>=|<=|<|=|not|or|xor|and|\\$|\u20ac|%|year|month|day)', bygroups(Operator)),
            (u'\\b(structure|enumeration|collection|integer|boolean|date|duration|money|text|decimal)\\b', bygroups(Keyword.Type)),
            (u'\\b([A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)(\\.)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)\\b', bygroups(Name.Class, Operator, Name.Variable)),
            (u'\\b([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)(\\.)([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\'\\.]*)\\b', bygroups(Name.Variable, Operator, String)),
            (u'\\b([a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)\\b', bygroups(Name.Variable)),
            (u'\\b([A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc7][a-z\xe9\xe8\xe0\xe2\xf9\xee\xea\u0153\xe7A-Z\xc9\xc8\xc0\xc2\xd9\xce\xca\u0152\xc70-9_\\\']*)\\b', bygroups(Name.Class)),
            ('(\n|\r|\r\n)', String),
            ('.', String),
        ], 
        'test' : [
            (u'(^```$)', bygroups(String), 'root'),
            (u'(^[$] catala \\S*)', bygroups(Keyword.Constant)),
            ('(\n|\r|\r\n)', String),
            ('.', String),
        ]
    }
