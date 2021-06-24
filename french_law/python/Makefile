SOURCES=catala_runtime.py allocations_familiales.py

dependencies:
	pip install -r dependencies.txt

type:
	mypy $(SOURCES)

format:
	autopep8 --in-place $(SOURCES)

doc:
	mkdir -p doc
	sphinx-build ./ doc
	ln -sf doc/index.html doc.html
