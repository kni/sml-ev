help:
	@echo "make poly or make mlton for build, or make clean"
	@echo "make timer-poly or make timer-mlton"

all: poly timer-poly mlton timer-mlton

poly:
	polyc -o t-poly t-poly.sml

timer-poly:
	polyc -o t-timer-poly t-timer-poly.sml

mlton:
	mlton -default-ann 'allowFFI true' t-mlton.mlb

timer-mlton:
	mlton -default-ann 'allowFFI true' t-timer-mlton.mlb

clean:
	rm -rf t-poly t-mlton t-timer-poly t-timer-mlton
