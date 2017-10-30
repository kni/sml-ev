help:
	@echo "make poly or make mlton for build, or make clean"
	@echo "make timer-poly or make timer-mlton"

all: poly timer-poly mlton timer-mlton

poly:
	polyc -o t-poly t.mlp

timer-poly:
	polyc -o t-timer-poly t-timer.mlp

mlton:
	mlton -default-ann 'allowFFI true' -output t-mlton t.mlb

timer-mlton:
	mlton -default-ann 'allowFFI true' -output t-timer-mlton t-timer.mlb

clean:
	rm -rf t-poly t-mlton t-timer-poly t-timer-mlton
