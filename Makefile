
all: sim
	./sim

sim: FpxxDemo.v FpxxDemoSim.v 
	iverilog -o $@ $^

waves:
	gtkwave waves.gtkw &

clean:
	rm -fr sim waves.vcd
