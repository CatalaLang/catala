SOURCES=src/catala.py src/allocations_familiales.py main.py src/api.py

dependencies:
	pip install -r dependencies.txt

type:
	mypy $(SOURCES)

format:
	autopep8 --in-place $(SOURCES)

bench:
	python main.py bench