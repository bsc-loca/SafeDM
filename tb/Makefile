#Simulations results are stored in transcript
#Traces of the simulation are stored in vsim.wlf when Questa is launched with the gui (vsim-launch)

hdl=../hdl
signature_calculator=../hdl/submodules/Signature_calculator

#Compiles the files and creates the library safety
compile:
	vlib bsc
	mkdir -p bsc
	vmap bsc bsc
	vcom -work bsc $(hdl)/diversity_types_pkg.vhd $(hdl)/diversity_components_pkg.vhd $(hdl)/diversity_quantifier_top.vhd  $(signature_calculator)/fifo_instructions.vhd $(signature_calculator)/mem_regs_sign.vhd $(signature_calculator)/signature_calculator.vhd
	vcom $(hdl)/diversity_components_pkg.vhd $(hdl)/diversity_quantifier_top.vhd $(hdl)/diversity_types_pkg.vhd $(signature_calculator)/fifo_instructions.vhd $(signature_calculator)/mem_regs_sign.vhd $(signature_calculator)/signature_calculator.vhd input_sim.vhd apb_SafeDM_tb.vhd 


#Launches the simulation with the graphical interface
vsim-launch: compile
	#vsim -voptargs=+acc apb_lightlock_tb -do "do wave.do" -do "run 21 us"
	vsim -voptargs=+acc apb_SafeDM_tb -do "do wave.do" -do "run -all"

#Launches the simulation in batch mode
vsim: compile
	#vsim -voptargs=+acc apb_lightlock_tb -do "run 21 us" <<! 
	vsim -voptargs=+acc apb_SafeDM_tb -do "run -all" <<! 
    
#Analyzes the results of the simulation
launch-tb : vsim
	@echo "\n\n\n\n\n"
	@errors=`grep 'Errors: 0' transcript` ; \
	echo "$$errors" ; \
	if [ "$$errors" = "" ]; then \
	echo 'TEST FAILED, SOME ERRORS FOUND:' ; \
	grep Error transcript ; \
	else \
	echo 'TEST PASSED WITHOUT ERRORS' ; \
	fi

#Cleans the temporal files
clean:
	rm -rf bsc
	rm -rf work
	rm -f modelsim.ini
	rm -f transcript
	rm -f vsim_stacktrace.vstf  
	rm -f vsim.wlf

