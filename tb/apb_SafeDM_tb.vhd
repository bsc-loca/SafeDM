-------------------------------------------------------------------------------------------------------------------
-- TOP module of the Testbench
-------------------------------------------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library bsc;
use bsc.diversity_types_pkg.all;
use bsc.diversity_components_pkg.SafeDM_top;
-- This procedure stops the simulation
use std.env.stop;



entity apb_SafeDM_tb is
    --port ();
end;


architecture rtl of apb_SafeDM_tb is

    component input_sim 
        port(
            clk : in  std_logic;
            -- Sync signals
            sync_inst : in std_logic;                      -- When it is high, instructions of both cores synchornyze
            sync_regs : in std_logic;                      -- When it is high, registers of both cores synchornyze
            -- Instructions signature
            instructions_o : out instruction_type_vector;  -- Signals to calculate the instruction signature
            -- Registers signatures
            registers_o : out register_type_vector;        -- Signals to calculate the registers signature
            -- hold signal
            hold_o : out std_logic_vector(1 downto 0)
        );
    end component;


    -- Procedure to read in the APB bus
    procedure apb_read(
            constant addr   : in integer;                                 -- Bus address to read from
            constant print  : in string(1 to 30);                         -- Message to print before printing the read data 
            signal compare_value : in integer;                            -- Value to compare with the data read (if not equal an error is raised)
            -- apb signals
            signal apbo_prdata  : in std_logic_vector(31 downto 0);
            signal apbi_psel    : out std_logic;
            signal apbi_penable : out std_logic;
            signal apbi_pwrite  : out std_logic;
            signal apbi_paddr   : out std_logic_vector(31 downto 0);
            signal apbi_pwdata  : out std_logic_vector(31 downto 0);
            signal data : out std_logic_vector(31 downto 0)) is           -- This signal is equal to the read data during once cycle
        begin
            -- First cycle
            apbi_penable <= '0';
            apbi_psel <= '1';
            apbi_paddr <= std_logic_vector(to_unsigned(addr, 32));
            apbi_pwrite <= '0';
            apbi_pwdata <= (others => '0');
            data <= (others => '0');
            wait for 10 ns;
            -- Second cycle
            apbi_penable <= '1';
            data <= apbo_prdata;
            wait for 10 ns;
            -- Third cycle
            report print & integer'image(to_integer(unsigned(apbo_prdata))) & " ~ " & integer'image(compare_value) severity note;
            assert compare_value + 10 > to_integer(unsigned(apbo_prdata)) and compare_value - 10 < to_integer(unsigned(apbo_prdata)) report print & "Cycles without diversity = " & integer'image(to_integer(unsigned(apbo_prdata))) & " does not match the value of the testbench = " & integer'image(compare_value) severity error;
            apbi_penable <= '0';
            apbi_psel <= '0';
            data <= (others => '0');
            wait for 10 ns;
        end apb_read;
            

    -- Procedure to write in the APB bus
    procedure apb_write(
           constant addr   : in integer;                                      -- Bus address to write in
           constant data   : in std_logic_vector(31 downto 0);                -- Data to write
           -- APB signals
           signal apbo_prdata  : in std_logic_vector(31 downto 0);
           signal apbi_psel    : out std_logic;
           signal apbi_penable : out std_logic;
           signal apbi_pwrite  : out std_logic;
           signal apbi_paddr   : out std_logic_vector(31 downto 0);
           signal apbi_pwdata  : out std_logic_vector(31 downto 0)) is
       begin
           -- First cycle
           apbi_penable <= '0';
           apbi_psel <= '1';
           apbi_paddr <= std_logic_vector(to_unsigned(addr, 32));
           apbi_pwrite <= '1';
           apbi_pwdata <= data;
           wait for 10 ns;
           -- Second cycle
           apbi_penable <= '1';
           wait for 10 ns;
           -- Third cycle
           apbi_pwdata <= (others => '0');
           apbi_penable <= '0';
           apbi_psel <= '0';
           wait for 10 ns;
       end apb_write;
           


    -- Signal definition
    signal rstn            : std_ulogic;
    signal clk             : std_ulogic;
    -- APB signals
    signal apbi_psel       : std_logic;                       
    signal apbi_paddr      : std_logic_vector(31 downto 0);                      
    signal apbi_penable    : std_logic;                     
    signal apbi_pwrite     : std_logic;
    signal apbi_pwdata     : std_logic_vector(31 downto 0);                   
    signal apbo_prdata     : std_logic_vector(31 downto 0);                  
    -- Intermediate signals
    signal instructions : instruction_type_vector;
    signal registers : register_type_vector;
    signal hold : std_logic_vector(1 downto 0);
    signal diversity_lack  : std_logic;
    signal sync_inst, sync_regs : std_logic;

    signal cycles_expected : integer; -- Expected cycles where there is like of diversity


    -- Local signals
    signal data_read : std_logic_vector(31 downto 0); -- Signal to read the read data in the bus

begin

    -- This module simulates (randomly generates) the inputs of the diversity module
    -- Simulates the instructions in decode (1 per lane) stage and the signals indicating that this instruction is valid (1 per lane)
    -- Simulates the value of the registers being read from the register file and the signals indicating that the value is valid (1 per register port)
    input_sim_inst : input_sim 
    port map(
        clk => clk,
        -- Sync signals
        sync_inst => sync_inst,           -- When it is high, instructions of both cores synchornyze
        sync_regs => sync_regs,           -- When it is high, registers of both cores synchornyze
        -- Instructions signature
        instructions_o => instructions,   -- Signals to calculate the instruction signature
        -- Registers signatures
        registers_o => registers,         -- Signals to calculate the registers signature
        -- hold signal
        hold_o => hold
    );


    SafeDM_top_inst : SafeDM_top
    generic map(
        coding_method     => 1,--0,                  -- It can use parity, ECC or none to encode the instructions and registers writes
        coding_bits_reg   => 8,--64,                 -- Number of bits saved for each register read 
        coding_bits_inst  => 7,--32,                 -- Number of bits saved for each instruction (concatenation signature)
        regs_FIFO_pos     => 5,                  -- Number of saved (last) register read to calculate the registers signature
        inst_FIFO_pos     => 6                   -- Number of saved (last) instructions to calculate the instruction signature
        )
    port map(
        rstn => rstn,
        clk  => clk,
        -- APB signals --------------------------------------
        apbi_psel_i    => apbi_psel,
        apbi_paddr_i   => apbi_paddr,                       
        apbi_penable_i => apbi_penable,
        apbi_pwrite_i  => apbi_pwrite, 
        apbi_pwdata_i  => apbi_pwdata,                  
        apbo_prdata_o  => apbo_prdata,       
        -----------------------------------------------------
        -- Singals to calculate sigantures ------------------
        -- Instructions signature
        instructions_i => instructions,   -- Signals to calculate the instruction signature
        -- Registers signatures
        registers_i => registers,         -- Signals to calculate the registers signature
        -- hold signals
        hold => hold,                     -- Signal that stalls the pipeline
        -----------------------------------------------------
        diversity_lack_o => diversity_lack             -- It is set high when there is no diversity
     );

    -------------------------------------------------------------------------------------------------------------------------
    -------------------------------------------------------------------------------------------------------------------------
    -------------------------------------------------------------------------------------------------------------------------

    -- Clock generation
    process
    begin
        clk <= '1';
        wait for 5 ns;
        clk <= '0';
        wait for 5 ns;
    end process;


    -- Main process
    -- Here SafeDM is activated through the APB interface
    -- Inputs are alternated between periods in which they are equal for both cores and periods in which
    -- they are different for both cores. (This is forced with signals sync_regs and sync_inst)
    -- The module should count a number of cycles without diversity similar to the number of cycles in 
    -- which the inputs for both cores are equal.
    process is
        variable write_register : std_logic_vector(31 downto 0);
    begin
        sync_regs <= '0';
        sync_inst <= '0';

        -- Configure inputs to reset value
        apbi_psel     <= '0';   
        apbi_paddr    <= (others => '0');  
        apbi_penable  <= '0';   
        apbi_pwrite   <= '0';
        apbi_pwdata   <= (others => '0');

        --Reset
        rstn <= '0';

        -- Keep reset for 10 cycles
        wait for 100 ns;
        rstn <= '1';

        -- SOFT RESET AND ENABLE ----------------------------------------------------------------------------------
        -- apb_write(addr, write_data, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata)
        -- Soft reset
        write_register := x"00000001";
        apb_write(0*4, write_register, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata);
        wait for 10 ns;

        -- Enable SafeDM
        write_register := x"00000001";
        apb_write(1*4, write_register, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata);
        wait for 10 ns;

        ------------------------------------------------------------------------------------------------------------
        -- SYNCHRONIZE AND DESYNCHRONIZE THE SIMULATED CORES
        wait for 1000 ns;

        sync_regs <= '1';
        sync_inst <= '1';

        wait for 1000 ns;

        sync_regs <= '0';
        sync_inst <= '1';

        wait for 1000 ns;

        sync_regs <= '1';
        sync_inst <= '1';

        wait for 1000 ns;

        sync_regs <= '1';
        sync_inst <= '0';
        
        wait for 1000 ns;

        sync_regs <= '0';
        sync_inst <= '0';
        
        wait for 1000 ns;
        ---------------------------------------------------------------------

        -- STOP SAFEDM --------------------------------------------------------------------------------------------
        -- Stop critical section core1
        write_register := x"00000000";
        apb_write(1*4, write_register, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata);
        wait for 80 ns;
        ------------------------------------------------------------------------------------------------------------

        ---- GATHER RESULTS ------------------------------------------------------------------------------------------
        cycles_expected <= 170;
        ---- apb_read(addr, print, compare value, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata)
        apb_read(2*4, "Cycles without diversity:     ",cycles_expected , apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata, data_read);
        wait for 10 ns;
        ------------------------------------------------------------------------------------------------------------

        report "Test finished";
        stop;
        
        wait;
    end process;

        


end; 
