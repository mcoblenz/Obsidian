# -*- coding: utf-8 -*-
"""
    pygments.lexers.obsidian
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for the Obsidian language.

    :copyright: Copyright 2019 by Michael Coblenz.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include, bygroups, default, using, \
    this, words, combined
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Other

__all__ = ['ObsidianLexer']


class ObsidianLexer(RegexLexer):
    """
    For Obsidian source code.
    """

    name = 'Obsidian'
    aliases = ['obs', 'Obsidian']
    filenames = ['*.obs']
    mimetypes = []

    flags = re.DOTALL | re.UNICODE | re.MULTILINE


    tokens = {
        'comment-parse-single': [
            (r'\n', Comment.Single, '#pop'),
            (r'[^\n]', Comment.Single),
        ],
        'comment-parse-multi': [
            (r'[^*/]', Comment.Multiline),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ],
        'comments': [
            (r'//', Comment.Single, 'comment-parse-single'),
            (r'/[*]', Comment.Multiline, 'comment-parse-multi'),
        ],
        'keywords-other': [
            (words(( 'Owned', 'Unowned', 'Shared', 'asset', 'interface', 
                    'owned', 'unowned', 'shared', 'available in', 'in'),
                   suffix=r'\b'), Keyword),

            (words(('contract', 'transaction', 'state'),
                   suffix=r'\b'), Keyword.Declaration),

            # built-in constants
            (r'(true|false)\b', Keyword.Constant),
        ],
        'keywords-types': [
            (words(('void', 'int', 
                    'string'),
                   suffix=r'\b'), Keyword.Type),
        ],
        'numbers': [
            (r'0[xX][0-9a-fA-F]+', Number.Hex),
            (r'[0-9][0-9]*\.[0-9]+([eE][0-9]+)?', Number.Float),
            (r'[0-9]+([eE][0-9]+)?', Number.Integer),
        ],
        'string-parse-common': [
            # escapes
            (r'\\(u[0-9a-fA-F]{4}|x..|[^x])', String.Escape),
            # almost everything else is plain characters
            (r'[^\\"\'\n]+', String),
            # line continuation
            (r'\\\n', String),
            # stray backslash
            (r'\\', String)
        ],
        'string-parse-double': [
            (r'"', String, '#pop'),
            (r"'", String)
        ],
        'string-parse-single': [
            (r"'", String, '#pop'),
            (r'"', String)
        ],
        'strings': [
            # usual strings
            (r'"', String, combined('string-parse-common',
                                    'string-parse-double')),
            (r"'", String, combined('string-parse-common',
                                    'string-parse-single'))
        ],
        'whitespace': [
            (r'\s+', Text)
        ],
        'root': [
            include('comments'),
            include('keywords-types'),
            include('keywords-other'),
            include('numbers'),
            include('strings'),
            include('whitespace'),

            (r'>>|@|\*\*|\?|:|~|&&|\|\||=>|==?|!=?|->', Operator),
            (r'(<<|[\-<>+*%&/\|])', Operator),

            (r'[{(\[;,]', Punctuation),
            (r'[})\].]', Punctuation),

            # compiler built-ins
            (r'(this|super)\b', Name.Builtin),
            (r'selector\b', Name.Builtin),

            # everything else is a var/function name
            ('[a-zA-Z_]\w*', Name)
        ] # 'root'
    } # tokens
