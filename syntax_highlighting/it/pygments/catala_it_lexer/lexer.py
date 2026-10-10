from pygments.lexer import RegexLexer, bygroups
from pygments.token import *

import re

__all__=['CustomLexer']

class CustomLexer(RegexLexer):
    name = 'CatalaIt'
    aliases = ['catala_it']
    filenames = ['*.catala_it']
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
            (u'(contesto|entrata|risultato|interno)(\\s*)(|risultato)(\\s+)([a-zàèéìíîòóùú][a-zàèéìíîòóùúA-ZÀÈÉÌÍÎÒÓÙÚ0-9_\\\']*)', bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration, Whitespace, Name.Variable)),
            (u'\\b(contenuto\\s+in|trasforma\\s+ogni|in\\s+ordine\\s+(de)?crescente|o\\s+se\\s+lista\\s+vuota|e\\s+poi|tale\\s+che)\\b', bygroups(Keyword.Declaration)),
            (u'\\b(secondo|con\\s+forma|ma\\s+sostituendo|per\\s+difetto|per\\s+eccesso|con|abbiamo|sia|in|campo\\s+di\\s+applicazione|dipende\\s+da|dichiarazione|contenuto|tipo|regola|sotto\\s+condizione|condizione|dato|conseguenza|soddisfatta|vale|asserzione|definizione|stato|etichetta|eccezione|qualsiasi)\\b', bygroups(Keyword.Reserved)),
            (u'\\b(contiene|numero|somma|esiste|per|ogni|di|se|allora|altrimenti|è|massimo|minimo|arrotondato|combina|diventa|inizialmente|ordina|impossibile)\\b', bygroups(Keyword.Declaration)),
            (u'(\\|[0-9]+\\-[0-9]+\\-[0-9]+\\|)', bygroups(Number.Integer)),
            (u'\\b(vero|falso)\\b', bygroups(Keyword.Constant)),
            (u'\\b([0-9]+(,[0-9]*|))\\b', bygroups(Number.Integer)),
            (u'(\\-\\-|\\;|\\.|\\,|\\:|\\(|\\)|\\[|\\]|\\{|\\})', bygroups(Operator)),
            (u'(\\-\\>|\\+\\.|\\+\\@|\\+\\^|\\+€|\\+|\\-\\.|\\-\\@|\\-\\^|\\-€|\\-|\\*\\.|\\*\\@|\\*\\^|\\*€|\\*|/\\.|/\\@|/€|/|\\!|>\\.|>=\\.|<=\\.|<\\.|>\\@|>=\\@|<=\\@|<\\@|>€|>=€|<=€|<€|>\\^|>=\\^|<=\\^|<\\^|>|>=|<=|<|=|€|%)', bygroups(Operator)),
            (u'\\b(non|oppure|o|e|anno|anni|mese|mesi|giorno|giorni)\\b', bygroups(Operator)),
            (u'\\b(struttura|enumerazione|lista\\s+di|opzionale\\s+di|intero|booleano|data|durata|somma_di_denaro|posizione_sorgente|decimale)\\b', bygroups(Keyword.Type)),
            (u'\\b(Presente|Assente)\\b', bygroups(Keyword.Constant)),
            (u'\\b([A-ZÀÈÉÌÍÎÒÓÙÚ][a-zàèéìíîòóùúA-ZÀÈÉÌÍÎÒÓÙÚ0-9_\\\']*)(\\.)([a-zàèéìíîòóùú][a-zàèéìíîòóùúA-ZÀÈÉÌÍÎÒÓÙÚ0-9_\\\']*)\\b', bygroups(Name.Class, Operator, Name.Variable)),
            (u'\\b([a-zàèéìíîòóùú][a-zàèéìíîòóùúA-ZÀÈÉÌÍÎÒÓÙÚ0-9_\\\']*)(\\.)([a-zàèéìíîòóùú][a-zàèéìíîòóùúA-ZÀÈÉÌÍÎÒÓÙÚ0-9_\\\'\\.]*)\\b', bygroups(Name.Variable, Operator, Text)),
            (u'\\b([a-zàèéìíîòóùú][a-zàèéìíîòóùúA-ZÀÈÉÌÍÎÒÓÙÚ0-9_\\\']*)\\b', bygroups(Name.Variable)),
            (u'\\b([A-ZÀÈÉÌÍÎÒÓÙÚ][a-zàèéìíîòóùúA-ZÀÈÉÌÍÎÒÓÙÚ0-9_\\\']*)\\b', bygroups(Name.Class)),
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
