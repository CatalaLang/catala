from setuptools import setup, find_packages

setup(
    name='catala_pl_lexer',
    packages=find_packages(),
    entry_points="""
    [pygments.lexers]
    catala_pl_lexer = catala_pl_lexer.lexer:CatalaNvLexer
    """,
)
