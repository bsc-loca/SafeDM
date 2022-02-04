library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library bsc;
use bsc.diversity_types_pkg.all;


entity apb_lightlock_tb is
    --port ();
end;


architecture rtl of apb_lightlock_tb is


    -- Top component declaration
    component diversity_quantifier_top 
        generic (
            coding_method         : integer := 2;   -- It can use parity, ECC or none to encode the instructions and registers writes
            coding_bits_reg       : integer := 64;  -- Number of bits saved for each register read 
            coding_bits_inst_conc : integer := 32;  -- Number of bits saved for each instruction (concatenation signature)
            regs_number           : integer := 5;   -- Number of saved (last) register read to calculate the registers signature
            saved_inst            : integer := 6    -- Number of saved (last) instructions to calculate the instruction signature
            );
        port (
            rstn           : in  std_ulogic;
            clk            : in  std_ulogic;
            -- APB signals --------------------------------------
            apbi_psel_i    : in  std_logic;                       
            apbi_paddr_i   : in  std_logic_vector(31 downto 0);                      
            apbi_penable_i : in  std_logic;                     
            apbi_pwrite_i  : in  std_logic;
            apbi_pwdata_i  : in  std_logic_vector(31 downto 0);                   
            apbo_prdata_o  : out std_logic_vector(31 downto 0);     
            -----------------------------------------------------
            -- Singals to calculate sigantures ------------------
            -- Instructions signature
            instructions_i : in instruction_type_vector;  -- Signals to calculate the instruction signature
            -- Registers signatures
            registers_i : in register_type_vector;        -- Signals to calculate the registers signature
            -- hold signals
            hold : in std_logic_vector(1 downto 0);       -- Signal that stalls the pipeline
            -----------------------------------------------------
            diversity_lack_o : out std_logic             -- It is set high when there is no diversity
         );
    end component;

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
            constant print  : in string(1 to 30);                         -- Message to print before printing the data read
            signal compare_value : in unsigned(31 downto 0);              -- Value to compare with the data read (if not equal an error is raised
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
            report print & integer'image(to_integer(unsigned(apbo_prdata))) & " =? " & integer'image(to_integer(compare_value)) severity note;
            assert compare_value = unsigned(apbo_prdata) report print & "Lightlock register value = " & integer'image(to_integer(unsigned(apbo_prdata))) & " does not match the value of the testbench = " & integer'image(to_integer(compare_value)) severity error;
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


    -- Local signals
    signal data_read : std_logic_vector(31 downto 0); -- Signal to read the read data in the bus

    signal enable_core1, enable_core2, r_enable_core1, r_enable_core2, enable : std_logic;

    -- Signal for statistics
    signal r_total_cycles, r_executed_inst1, r_executed_inst2, r_times_stalled_core1, r_times_stalled_core2, r_cycles_stalled_core1, r_cycles_stalled_core2, r_max_inst_diff, r_min_inst_diff : unsigned(31 downto 0);
    signal n_total_cycles, n_executed_inst1, n_executed_inst2, n_times_stalled_core1, n_times_stalled_core2, n_cycles_stalled_core1, n_cycles_stalled_core2, n_max_inst_diff, n_min_inst_diff : unsigned(31 downto 0);
    signal core1_ahead_core2 : std_logic;

    signal r_stall1, r_stall2, f_stall1, f_stall2 : std_logic;

    constant fill_zeros : unsigned(16 downto 0) := to_unsigned(0, 17);
    signal instruction_difference : unsigned(14 downto 0);

    signal r_activate_minimum_inst_comp : std_logic;    


begin

    input_sim_inst : input_sim 
    port map(
        clk => clk
        -- Sync signals
        sync_inst => sync_inst            -- When it is high, instructions of both cores synchornyze
        sync_regs => sync_regs            -- When it is high, registers of both cores synchornyze
        -- Instructions signature
        instructions_o => instructions,   -- Signals to calculate the instruction signature
        -- Registers signatures
        registers_o => registers,         -- Signals to calculate the registers signature
        -- hold signal
        hold_o => hold
    );


    diversity_quantifier_top_inst : diversity_quatifier_top
    generic map(
        coding_method         => 2;   -- It can use parity, ECC or none to encode the instructions and registers writes
        coding_bits_reg       => 64;  -- Number of bits saved for each register read 
        coding_bits_inst_conc => 32;  -- Number of bits saved for each instruction (concatenation signature)
        regs_number           => 5;   -- Number of saved (last) register read to calculate the registers signature
        saved_inst            => 6    -- Number of saved (last) instructions to calculate the instruction signature
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
        apbo_prdata_o  => data_read,       
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

    -- icnt generation
    -- It generates them randomly
    -- TODO: adapt it for more lanes
    icnt_generation : inst_count_sim 
    port map(
        clk      => clk,         
        stall2_i => stall2,  
        stall1_i => stall1,
        icnt1_o  => icnt1,
        icnt2_o  => icnt2
        );

    -- Main process
    process is
        variable write_register : std_logic_vector(31 downto 0);
    begin
        enable_core1 <= '0';
        enable_core2 <= '0';

        -- Configure inputs to reset value
        apbi_psel     <= '0';   
        apbi_paddr    <= (others => '0');  
        apbi_penable  <= '0';   
        apbi_pwrite   <= '0';
        apbi_pwdata   <= (others => '0');
        data_read     <= (others => '0');

        --Reset
        rstn <= '0';

        -- Keep reset for 10 cycles
        wait for 100 ns;
        rstn <= '1';

        -- CONFIGURE LIGHTLOCK INTERNAL REGISTERS ------------------------------------------------------------------
        -- apb_write(addr, write_data, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata)
        -- Soft reset
        write_register := x"80000000";
        apb_write(0, write_register, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata);
        wait for 10 ns;

        -- Global enable and configure min and max staggering (10,20)
        write_register := x"400a000a";
        apb_write(0, write_register, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata);
        wait for 10 ns;

        -- Start critical section core1
        enable_core1 <= '1' after 11 ns;
        write_register := x"00000001";
        apb_write(1*4, write_register, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata);
        wait for 80 ns;

        -- Start critical section core2
        enable_core2 <= '1' after 11 ns;
        write_register := x"00000001";
        apb_write(2*4, write_register, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata);
        wait for 10 ns;
        ------------------------------------------------------------------------------------------------------------

        -- SET A DURATION FOR THE TEST --------------------------------------
        wait for 20000 ns;
        ---------------------------------------------------------------------

        -- STOP CRITICAL SECTIONS ----------------------------------------------------------------------------------
        -- Stop critical section core1
        enable_core1 <= '0' after 11 ns;
        write_register := x"00000000";
        apb_write(1*4, write_register, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata);
        wait for 80 ns;

        -- Stop critical section core2
        enable_core2 <= '0' after 11 ns;
        write_register := x"00000000";
        apb_write(2*4, write_register, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata);
        wait for 10 ns;
        ------------------------------------------------------------------------------------------------------------

        -- GATHER RESULTS ------------------------------------------------------------------------------------------
        -- apb_read(addr, write_data, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata)
        apb_read(3*4, "Total cycles:                 ", r_total_cycles, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata, data_read);
        wait for 10 ns;

        apb_read(4*4, "Executed instructions core1:  ", r_executed_inst1, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata, data_read);
        wait for 10 ns;

        apb_read(5*4, "Executed instructions core2:  ", r_executed_inst2, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata, data_read);
        wait for 10 ns;

        apb_read(6*4, "Times core1 has been stalled: ", r_times_stalled_core1, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata, data_read);
        wait for 10 ns;

        apb_read(7*4, "Times core2 has been stalled: ", r_times_stalled_core2, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata, data_read);
        wait for 10 ns;

        apb_read(8*4, "Cycles core1 has been stalled:", r_cycles_stalled_core1, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata, data_read);
        wait for 10 ns;

        apb_read(9*4, "Cycles core2 has been stalled:", r_cycles_stalled_core2, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata, data_read);
        wait for 10 ns;

        apb_read(10*4, "Max instructions difference:  ", r_max_inst_diff, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata, data_read);
        wait for 10 ns;

        apb_read(12*4, "Min instructions difference:  ", r_min_inst_diff, apbo_prdata, apbi_psel, apbi_penable, apbi_pwrite, apbi_paddr, apbi_pwdata, data_read);
        wait for 10 ns;
        ------------------------------------------------------------------------------------------------------------

        report "Test finished";
        stop;
        
        -- report "Test finished" severity error;
        -- assert 1 = 2  report "Test finished." severity failure;
        wait;
    end process;

        


end; 
