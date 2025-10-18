from pygments.lexer import RegexLexer, bygroups
from pygments.token import *

import re

__all__=['CustomLexer']

class CustomLexer(RegexLexer):
    name = 'CatalaRo'
    aliases = ['catala_ro']
    filenames = ['*.catala_ro']
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
            (u'(context|intrare|ieșire|intern)(\\s*)(|ieșire)(\\s+)([a-zăâîșțĂÂÎȘȚA-Z0-9_\\\']*)', bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration, Whitespace, Name.Variable)),
            (u'\\b(potrivește|cu\\s+model|dar\\s+înlocuiește|fixat|de|descrescător|crescător|variază|cu|avem|fie|în|scop|depinde\\s+de|declarație|include|conținut|tip|regulă|în\\s+condiția|condiție|date|consecință|îndeplinit|egal|aserțiune|definiție|stare|etichetă|excepție|orice)\\b', bygroups(Keyword.Reserved)),
            (u'\\b(conține|număr|sumă|astfel\\s+că|există|pentru|toți|de|dacă|atunci|altfel|este|listă\\s+goală|printre|maxim|minim|rotunjește|combină|aplică\\s+fiecare|la|inițial)\\b', bygroups(Keyword.Declaration)),
            (u'(\\|[0-9]+\\-[0-9]+\\-[0-9]+\\|)', bygroups(Number.Integer)),
            (u'\\b(adevărat|fals)\\b', bygroups(Keyword.Constant)),
            (u'\\b([0-9]{1,3}(\\.[0-9]{3})*,[0-9]+)\\b', bygroups(Number.Float)),
            (u'\\b([0-9]{1,3}(\\.[0-9]{3})+)\\b', bygroups(Number.Integer)),
            (u'\\b([0-9]+)\\b', bygroups(Number.Integer)),
            (u'(\\-\\-|\\;|\\.|\\,|\\:|\\(|\\)|\\[|\\]|\\{|\\})', bygroups(Operator)),
            (u'(\\-\\>|\\+\\.|\\+\\@|\\+\\^|\\+\\$|\\+|\\-\\.|\\-\\@|\\-\\^|\\-\\$|\\-|\\*\\.|\\*\\@|\\*\\^|\\*\\$|\\*|/\\.|/\\@|/\\$|/|\\!|>\\.|>=\\.|<=\\.|<\\.|>\\@|>=\\@|<=\\@|<\\@|>\\$|>=\\$|<=\\$|<\\$|>\\^|>=\\^|<=\\^|<\\^|>|>=|<=|<|=|nu|sau|sau\\s+exclusiv|și|\\$|RON|%|an|lună|zi)', bygroups(Operator)),
            (u'\\b(structură|enumerare|listă\\s+de|întreg|boolean|dată|durată|bani|text|zecimal|position_source)\\b', bygroups(Keyword.Type)),
            (u'\\b([A-ZĂÂÎȘȚ][a-zăâîșțA-ZĂÂÎȘȚ0-9_\\\']*)(\\.)([a-zăâîșț][a-zăâîșțA-ZĂÂÎȘȚ0-9_\\\']*)\\b', bygroups(Name.Class, Operator, Name.Variable)),
            (u'\\b([a-zăâîșț][a-zăâîșțA-ZĂÂÎȘȚ0-9_\\\']*)(\\.)([a-zăâîșț][a-zăâîșțA-ZĂÂÎȘȚ0-9_\\\'\\.]*)\\b', bygroups(Name.Variable, Operator, Text)),
            (u'\\b([a-zăâîșț][a-zăâîșțA-ZĂÂÎȘȚ0-9_\\\']*)\\b', bygroups(Name.Variable)),
            (u'\\b([A-ZĂÂÎȘȚ][a-zăâîșțA-ZĂÂÎȘȚ0-9_\\\']*)\\b', bygroups(Name.Class)),
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
