onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -group Input_sim /apb_safedm_tb/rstn
add wave -noupdate -group Input_sim /apb_safedm_tb/input_sim_inst/clk
add wave -noupdate -group Input_sim -expand /apb_safedm_tb/input_sim_inst/valid1
add wave -noupdate -group Input_sim -expand /apb_safedm_tb/input_sim_inst/valid2
add wave -noupdate -group Input_sim -expand /apb_safedm_tb/input_sim_inst/ren1
add wave -noupdate -group Input_sim -expand /apb_safedm_tb/input_sim_inst/ren2
add wave -noupdate -group Input_sim -expand /apb_safedm_tb/input_sim_inst/register1
add wave -noupdate -group Input_sim -expand /apb_safedm_tb/input_sim_inst/register2
add wave -noupdate -group Input_sim -expand /apb_safedm_tb/input_sim_inst/instruction1
add wave -noupdate -group Input_sim -expand /apb_safedm_tb/input_sim_inst/instruction2
add wave -noupdate -group Input_sim -expand /apb_safedm_tb/input_sim_inst/hold
add wave -noupdate /apb_safedm_tb/diversity_quantifier_top_inst/enable
add wave -noupdate /apb_safedm_tb/diversity_quantifier_top_inst/soft_rstn
add wave -noupdate /apb_safedm_tb/diversity_quantifier_top_inst/diversity_lack_o
add wave -noupdate -radix unsigned /apb_safedm_tb/diversity_quantifier_top_inst/diversity_lack_count
add wave -noupdate /apb_safedm_tb/diversity_quantifier_top_inst/apbo_prdata_o
add wave -noupdate /apb_safedm_tb/diversity_quantifier_top_inst/slave_index
add wave -noupdate -expand -group apb_bus /apb_safedm_tb/apbo_prdata
add wave -noupdate -expand -group apb_bus /apb_safedm_tb/apbi_pwrite
add wave -noupdate -expand -group apb_bus /apb_safedm_tb/apbi_pwdata
add wave -noupdate -expand -group apb_bus /apb_safedm_tb/apbi_psel
add wave -noupdate -expand -group apb_bus /apb_safedm_tb/apbi_penable
add wave -noupdate -expand -group apb_bus /apb_safedm_tb/apbi_paddr
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {6304 ns} 0}
quietly wave cursor active 1
configure wave -namecolwidth 427
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits us
update
WaveRestoreZoom {6272 ns} {6340 ns}
