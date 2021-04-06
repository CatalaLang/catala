CATALA_LANG=en
SRC=Title17-MotorAndOtherVehicles.catala_en

include ../Makefile.common.mk

Title17_Test.run: SCOPE=Penalty286_83_135 -t

test: Title17_Test.run

.PHONY: gen-web
gen-web:
	dune build
	make Title17_MotorAndOtherVehicles.ml
	cp ../../_build/default/examples/hawaii_lfo/*.js web/
