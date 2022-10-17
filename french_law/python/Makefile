SOURCES=../../runtimes/python/catala/src/catala/runtime.py \
  src/allocations_familiales.py \
  src/aides_logement.py \
  main.py src/api.py

dependencies:
	pip install -r requirements.txt

type:
	mypy $(SOURCES)

format:
	autopep8 --in-place $(SOURCES)

bench:
	python main.py bench_family
	python main.py bench_housing

show_log:
	python main.py show_log
