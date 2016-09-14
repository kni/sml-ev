all:
	@echo "make poly or make mlton for build, or make clean"

poly:
	polyc -o t-poly t-poly.sml

mlton:
	mlton -default-ann 'allowFFI true' t-mlton.mlb

clean:
	rm -rf t-poly t-mlton
