SOURCES=catala_runtime.py

dependencies:
	pip install -r dependencies.txt

type:
	mypy $(SOURCES)

doc:
	mkdir -p doc
	sphinx-build ./ doc
	ln -sf doc/index.html doc.html
