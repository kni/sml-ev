help:
	@echo "target: poly mlton timer-poly timer-mlton clean"

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
