# Test-bench

This folder contains all the files needed to execute a test bench for the SafeDM module. The file `apb_SafeDM_tb.vhd` is the top VHDL design. In this file the top of SafeDM is instantiated and all its inputs are simulated.

In the `input_sim.vhd` file, a component that randomly generates the inputs for SafeDM (instructions and registers) is defined. The input_sim module also have two inputs for synchronizing the simulated registers and instructions as if both cores where executing the same.

The test-bench passes provided that the cycles where there is lack of diversity coincides more or less with the cycles in which both synchronization (registers and instructions) signals where high.

Changing the constants `lanes` and `read_ports_number` in the file `hdl/diversity_types_pkg.vhd`, the diversity module logic and the test bench will adapt to the specified characteristics. 



---

### Makefile

The compilation and execution of the test is controlled by means of a Makefile. This Makefile has 4 recipes:

* **compile:** It creates the libraries work and safety and compiles all the VHDL files.
* **vsim-launch:** It compiles and launches the simulation in the QuestaSim graphical user interface.
* **vsim:** It compiles and runs the simulation in batch mode.
* **launch-tb:** It compiles, launches the simulation in batch mode and analyze the results of the test-bench.
* **clean:** It removes temporal files.



To finish the testbench, we use the stop procedure which is imported from the STD.ENV package. To work, the testbench has to be compiled in VHDL-2008 or newer. If this is not possible, the time of the simulation can be controlled executing `vsim -voptargs=+acc apb_lightlock_tb -do "run simulation_time"` when launching the simulation.