from setuptools import setup, find_packages

setup(
    name='catala_nv_lexer',
    packages=find_packages(),
    entry_points="""
    [pygments.lexers]
    catala_nv_lexer = catala_nv_lexer.lexer:CatalaNvLexer
    """,
)
