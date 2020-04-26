#! /usr/bin/env bash

cd "$(dirname "$0")"
ln -s -f $(pwd)/catala_fr.py pygments/pygments/lexers/catala_fr.py
if grep -q "CatalaFrLexer" pygments/pygments/lexers/_mapping.py
then
  :
else
  sed -i "78i\\    'CatalaFrLexer': ('pygments.lexers.catala_fr', 'CatalaFr', ('catala_fr'), ('*.catala'), ('text/x-catala-fr',))," pygments/pygments/lexers/_mapping.py
fi
cd pygments
virtualenv -p python3 env
source env/bin/activate
python3 setup.py install
