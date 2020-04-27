#! /usr/bin/env bash

cd "$(dirname "$0")"
ln -s -f $(pwd)/pygments/catala_en.py pygments/pygments/pygments/lexers/catala_en.py
if grep -q "CatalaEnLexer" pygments/pygments/pygments/lexers/_mapping.py
then
  :
else
  sed -i "78i\\    'CatalaEnLexer': ('pygments.lexers.catala_en', 'CatalaEn', ('catala_en'), ('*.catala'), ('text/x-catala-en',))," pygments/pygments/pygments/lexers/_mapping.py
fi
cd pygments/pygments
virtualenv -p python3 env
source env/bin/activate
python3 setup.py install
