from setuptools import setup, find_packages

setup(
    name='catala_fr_lexer',
    packages=find_packages(),
    entry_points="""
    [pygments.lexers]
    catala_fr_lexer = catala_fr_lexer.lexer:CatalaFrLexer
    """,
)
