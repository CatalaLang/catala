dependencies:
	pip install -r dependencies.txt

type:
	mypy runtime/catala_runtime.py

doc:
	mkdir -p doc
	sphinx-build ./ doc
	ln -sf doc/index.html doc.html
