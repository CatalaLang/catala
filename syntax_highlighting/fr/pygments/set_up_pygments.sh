#! /usr/bin/env bash

cd "$(dirname "$0")"
ln -s -f $(pwd)/catala.py pygments/pygments/lexers/catala.py
if grep -q "CatalaLexer" pygments/pygments/lexers/_mapping.py
then
  :
else
  sed -i "78i\\    'CatalaLexer': ('pygments.lexers.catala', 'Catala', ('catala'), ('*.catala'), ('text/x-catala',))," pygments/pygments/lexers/_mapping.py
fi
cd pygments
virtualenv -p python3 env
source env/bin/activate
python3 setup.py install
