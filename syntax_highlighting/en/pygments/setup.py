from setuptools import setup, find_packages

setup(
    name='catala_en_lexer',
    packages=find_packages(),
    entry_points="""
    [pygments.lexers]
    catala_en_lexer = catala_en_lexer.lexer:CatalaEnLexer
    """,
)
