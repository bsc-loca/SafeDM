library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library bsc;
use bsc.diversity_types_pkg.all;
use bsc.diversity_components_pkg.all;

entity diversity_quantifier_top is
    generic (
        coding_method         : integer := 2;   -- It can use parity, ECC or none to encode the instructions and registers writes
        coding_bits_reg       : integer := 64;  -- Number of bits saved for each register read 
        coding_bits_inst_sum  : integer := 7;   -- Number of bits saved for each instruction (sumation signature)
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
        -- Signals to measure the staggering ----------------
        -- Instruction counters
        icnt1_i : in std_logic_vector(1 downto 0);
        icnt2_i : in std_logic_vector(1 downto 0);
        -----------------------------------------------------
        diversity_lack_o : out std_logic;             -- It is set high when there is no diversity
        logan_o : out std_logic_vector(189 downto 0); -- Signals that go into the integrated logic analyzer
        -- To stall pipelines to force lack of diversity
        stall_o : out std_logic_vector(1 downto 0)
     );
end;


architecture rtl of diversity_quantifier_top is
    -- Number of bits for the signals of the signatures -----------------------------------------------------------------
    constant REG_SIG_PORT_BITS : integer := regs_number*coding_bits_reg;
    constant REG_SIG_BITS : integer := REG_SIG_PORT_BITS*4;
    constant INST_SUM_SIG_BITS : integer := integer(ceil(log2(real(((2 ** coding_bits_inst_sum)-1)*saved_inst*2))));
    constant INST_CONC_SIG_BITS : integer := coding_bits_inst_conc*saved_inst*2;
    --constant INST_SIG_REGISTERS : integer := saved_inst*2;
    constant READ_POSITIONS : integer := 7; -- Memory positions before the histsograms used to read statistics
    ---------------------------------------------------------------------------------------------------------------------


    -- Singals for the signatures ---------------------------------------------------------------------------------------
    type reg_signature_array is array (natural range <>) of std_logic_vector(REG_SIG_BITS-1 downto 0);
    signal reg_signature : reg_signature_array(1 downto 0);

    type inst_signature_sum_array is array (natural range <>) of std_logic_vector(INST_SUM_SIG_BITS-1 downto 0);
    signal inst_signature_sum : inst_signature_sum_array(1 downto 0);

    type inst_signature_conc_array is array (natural range <>) of std_logic_vector(INST_CONC_SIG_BITS-1 downto 0);
    signal inst_signature_conc   : inst_signature_conc_array(1 downto 0);
    ---------------------------------------------------------------------------------------------------------------------

    -- Signal and constant for the differences of the signatures --------------------------------------------------------
    constant INST_SUM_SIGNATURE_DIFF  : integer := ((2 ** coding_bits_inst_sum)-1)*saved_inst*2;
    constant MAX_REG_SIGNATURE_DIFF   : integer := regs_number*4;
    constant REG_SIGNATURE_DIFF_BITS  : integer := integer(ceil(log2(real(MAX_REG_SIGNATURE_DIFF))));
    constant MAX_CONC_SIGNATURE_DIFF  : integer := saved_inst*2;
    constant CONC_SIGNATURE_DIFF_BITS : integer := integer(ceil(log2(real(MAX_CONC_SIGNATURE_DIFF))));
    signal reg_signature_diff, r_reg_signature_diff : std_logic_vector(REG_SIGNATURE_DIFF_BITS-1 downto 0);
    signal inst_signature_conc_diff, r_inst_signature_conc_diff : std_logic_vector(CONC_SIGNATURE_DIFF_BITS-1 downto 0);
    signal inst_signature_sum_diff, r_inst_signature_sum_diff : unsigned(INST_SUM_SIG_BITS-1  downto 0); -- The bits of the signatures are calculated thinking in the maximum posible difference
    ---------------------------------------------------------------------------------------------------------------------

    -- Instructions difference between cores
    signal inst_diff, r_inst_diff : std_logic_vector(31 downto 0);
    -- Statistics
    signal max_stag_core1, min_stag_core1, max_stag_core2, min_stag_core2,pass_counter, last_pass : std_logic_vector(31 downto 0);

    -- Histograms memorie signals
    signal histogram_read_addr : unsigned(13 downto 0);
    signal histogram_mem_out   : std_logic_vector(31 downto 0);
    
    -- Enable signals
    signal cores_enable : std_logic_vector(1 downto 0);
    signal enable, r_enable : std_logic;

    -- Logic analyzer (LOGAN)
    type decode_instructions is array (natural range <>) of std_logic_vector(63 downto 0);
    signal decode_fifo_input : decode_instructions(1 downto 0);
    signal ex_inst_core1 : std_logic_vector(31 downto 0);
    signal core1_ahead_core2 : std_logic;
    

    -- APB bus ----------------------------------------------------------------------------------------------------------
    -- The number or registers can be changed but has to be bigger than 2 for the rest of the design to automatically adapt
    type registers_vector is array (natural range <>) of std_logic_vector(31 downto 0);
    constant REGISTERS_NUMBER : integer := 4; -- 6+2*INST_SIG_REGISTERS;
    signal r, rin      : registers_vector(REGISTERS_NUMBER-1 downto 0) ;
    signal slave_index : unsigned(13 downto 0);

    -- soft reset through APB bus
    signal soft_rstn : std_ulogic;
    signal internal_rstn : std_ulogic;
    ---------------------------------------------------------------------------------------------------------------------

begin

    --assert (instructions_i(0).opcode(0) /= x"02895433")  report "point reached" severity warning;
    --assert (instructions_i(0).opcode(0) /= x"00000013")  report "nop reached" severity failure;

    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- SIGNATURE CACLCULATION MODULES --------------------------------------------------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- If hold is set, the registers have to shift so the instructions are always compared in the rigth order
    -- Two modules to calculate the signatures are instanciated, one for each core
    signature_calc_inst : for n in 0 to 1 generate
        signature_calculator_inst : signature_calculator
        generic map(
            coding_method         => coding_method,
            coding_bits_reg       => coding_bits_reg,
            coding_bits_inst_sum  => coding_bits_inst_sum,
            coding_bits_inst_conc => coding_bits_inst_conc,
            regs_number           => regs_number,
            saved_inst            => saved_inst, 
            REG_SIG_PORT_BITS     => REG_SIG_PORT_BITS,
            REG_SIG_BITS          => REG_SIG_BITS,
            INST_SUM_SIG_BITS     => INST_SUM_SIG_BITS,
            INST_CONC_SIG_BITS    => INST_CONC_SIG_BITS
            )
        port map(
            rstn   => internal_rstn, 
            clk    => clk, 
            enable => enable,
            hold_i => hold(n),
            -- Instructions signature
            instructions_i => instructions_i(n),
            -- Registers signatures
            registers_i => registers_i(n),
            -- Outputs
            reg_signature_o        => reg_signature(n),
            inst_signature_sum_o   => inst_signature_sum(n),
            inst_signature_conc_o  => inst_signature_conc(n),
            fifo_input_conc_o      => decode_fifo_input(n)    
            );
    end generate signature_calc_inst; 

    -- In this process, the diference between the signatures of both cores are computed
    process(reg_signature, inst_signature_sum, inst_signature_conc, inst_signature_conc_diff, reg_signature_diff)
        variable temp_regs      : unsigned(REG_SIGNATURE_DIFF_BITS-1 downto 0);
        variable temp_inst_conc : unsigned(inst_signature_conc_diff'HIGH downto 0);
    begin
        -- REGISTER SIGNATURE
        -- Number of diferent registers in the registers signature
        -- Registers value stored in the different positions of the regsiters signature are compared one by one
        temp_regs := (others => '0');
        for i in regs_number*4 downto 1 loop
            if reg_signature(0)(i*coding_bits_reg-1 downto (i-1)*coding_bits_reg) /= reg_signature(1)(i*coding_bits_reg-1 downto (i-1)*coding_bits_reg) then -- It compares registers one by one 
                temp_regs := temp_regs + 1;                                                                                                         -- Independently of the coding bits and regs number
            end if;
        end loop;
        reg_signature_diff <= std_logic_vector(temp_regs);

        -- Number of different instructions in insts sum signature (concatenation). 
        temp_inst_conc := (others => '0');
        for i in saved_inst*2 downto 1 loop
            if (inst_signature_conc(0)(i*coding_bits_inst_conc-1 downto (i-1)*coding_bits_inst_conc) /= inst_signature_conc(1)(i*coding_bits_inst_conc-1 downto (i-1)*coding_bits_inst_conc)) then
                temp_inst_conc := temp_inst_conc + 1;
            end if;
        end loop;
        inst_signature_conc_diff <= std_logic_vector(temp_inst_conc);
 
        -- INSTRUCTION SIGNATURE
        -- Absolute numeric difference between both instructions signatures
        if unsigned(inst_signature_sum(0)) > unsigned(inst_signature_sum(1)) then
            inst_signature_sum_diff <= unsigned(inst_signature_sum(0)) - unsigned(inst_signature_sum(1)); 
        else
            inst_signature_sum_diff <= unsigned(inst_signature_sum(1)) - unsigned(inst_signature_sum(0)); 
        end if;


        -- LACK OF DIVERSITY OUTPUT
        if unsigned(inst_signature_conc_diff) = 0  and unsigned(reg_signature_diff) = 0 then
            diversity_lack_o <= '1';
        else
            diversity_lack_o <= '0';
        end if;

    end process;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- INSTRUCTION DIFFERENCE CALCULATOR -----------------------------------------------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- This module calculates the difference of commited instructions between both cores
    -- It each core has to be enable (enable_core1_i and enable_core2_i) in the same position of the code 
    inst_diff_calculator_inst : inst_diff_calculator
    port map(
        rstn => internal_rstn, 
        clk  => clk, 
        enable_core1_i    => cores_enable(0),
        enable_core2_i    => cores_enable(1),
        icnt1_i           => icnt1_i,
        icnt2_i           => icnt2_i,
        inst_diff_o       => inst_diff,
        -- Remove diversity
        remove_diversity_i => r(3),
        -- Outputs for statistics
        max_stag_core1_o  => max_stag_core1,    
        min_stag_core1_o  => min_stag_core1,    
        max_stag_core2_o  => max_stag_core2,    
        min_stag_core2_o  => min_stag_core2,    
        pass_counter_o    => pass_counter,  
        last_pass_o       => last_pass,     
        -- Stall outpus
        stall_o => stall_o,
        -- Outputs for LOGAN
        core1_ahead_core2_o => core1_ahead_core2,
        ex_inst_core1_o     => ex_inst_core1
        );
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- DISTRIBUTED RAM TO KEEP THE HISTOGRAMS DATA -------------------------------------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- This module counts with three memories of different sizes. Each memorie keep information for different histograms. They are arranged as folows:
    -----------------------------------------------------------------------------------------------------------------------------------------------------
    -- First histogram:
    -- Histogram 1: histogram of the instructions difference between cores.
    -----------------------------------------------------------------------------------------------------------------------------------------------------
    -- Histograms from 2 to 1+n being n the number of intervals in the x-axis in the previous histogram 
    -- Histogram 2: value of the difference between the instructions signatures when instruction difference between the cores is in the 1st interval
    -- Histogram 3: value of the difference between the instructions signatures when instruction difference between the cores is in the 2nd interval
    -- Histogram 4: value of the difference between the instructions signatures when instruction difference between the cores is in the 3rd interval
    -- ..............................
    -- Histogram 1+n: value of the difference between the instructions signatures when instruction difference between the cores is in the n interval
    -----------------------------------------------------------------------------------------------------------------------------------------------------
    -- Histograms from 2+n to 1+2*n  
    -- Histogram 2+n: value of the difference between the registers signatures when instruction difference between the cores is in the 1st interval
    -- Histogram 3+n: value of the difference between the registers signatures when instruction difference between the cores is in the 2nd interval
    -- Histogram 4+n: value of the difference between the registers signatures when instruction difference between the cores is in the 3rd interval
    -- ..............................
    -- Histogram 1+2*n: value of the difference between the registers signatures when instruction difference between the cores is in the n interval
    -----------------------------------------------------------------------------------------------------------------------------------------------------
    -- All this information can be read from the module as in a normal memory operation. All the histograms memory positions are adjacent
    histograms_memory_inst : histograms_memory
    generic map(
        INST_SIGNATURE_DIFF_BITS => INST_SUM_SIG_BITS,
        REG_SIGNATURE_DIFF_BITS  => REG_SIGNATURE_DIFF_BITS,
        CONC_SIGNATURE_DIFF_BITS => CONC_SIGNATURE_DIFF_BITS,
        MAX_INST_SIGNATURE_DIFF  => INST_SUM_SIGNATURE_DIFF,
        MAX_REG_SIGNATURE_DIFF   => MAX_REG_SIGNATURE_DIFF,
        MAX_CONC_SIGNATURE_DIFF  => MAX_CONC_SIGNATURE_DIFF     
        )
    port map(
        rstn   => internal_rstn, 
        clk    => clk, 
        enable => r_enable,
        -- Data to be stored
        inst_diff_i           => r_inst_diff,
        inst_signature_diff_i => std_logic_vector(r_inst_signature_sum_diff),
        reg_signature_diff_i  => r_reg_signature_diff,
        conc_signature_diff_i => r_inst_signature_conc_diff,
        -- Memory read
        addr_i    => std_logic_vector(histogram_read_addr),
        -- Memory out
        data_o => histogram_mem_out
        );

    -- Break the path of the inputs with a register for timing issues
    process(clk)
    begin
        if rising_edge(clk) then 
            if rstn = '0' then
                r_inst_diff                <= (others => '0');
                r_inst_signature_sum_diff  <= (others => '0');
                r_reg_signature_diff       <= (others => '0');
                r_inst_signature_conc_diff <= (others => '0');
                r_enable                   <= '0';
            else
                r_inst_diff                <= inst_diff;
                r_inst_signature_sum_diff  <= inst_signature_sum_diff;
                r_reg_signature_diff       <= reg_signature_diff;
                r_inst_signature_conc_diff <= inst_signature_conc_diff;
                r_enable                   <= enable;
            end if;
        end if;
    end process;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    -- Logic analyzer input
    logan_o <= decode_fifo_input(1) &        -- 64
               decode_fifo_input(0) &        -- 64
               clk &                         -- 1
               core1_ahead_core2 &           -- 1
               hold &                        -- 2
               r_enable &                    -- 1
               reg_signature_diff &          -- 5
               inst_signature_conc_diff &    -- 4
               ex_inst_core1 &               -- 32
               inst_diff(15 downto 0);       -- 16
                                             -- Total 190 bits

    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- APB BUS HANDLE ------------------------------------------------------------------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- There are 3 registers, the two first are to enable each core
    -- The last register is for the soft reset
    regs : process(clk)
    begin
        if rising_edge(clk) then r <= rin; end if;
    end process;

    -- The salve index is computed from the apb address
    slave_index <= unsigned(apbi_paddr_i(15 downto 2));
    -- The address to read from the histograms memory is also computed.
    -- From the third position onwards it drives the apb bus read address to the histograms memory address signal.
    histogram_read_addr <= slave_index-REGISTERS_NUMBER-READ_POSITIONS; 

    comb : process(rstn, apbi_penable_i, apbi_psel_i, apbi_pwrite_i, apbi_pwdata_i, slave_index, histogram_mem_out, r, max_stag_core1, max_stag_core2, min_stag_core1, min_stag_core2, pass_counter, last_pass, ex_inst_core1) is 
        variable v : registers_vector(REGISTERS_NUMBER-1 downto 0);
        variable slave_index_int : integer;
    begin
        slave_index_int := to_integer(slave_index);
        v := r;
        -- Write registers -------------------------------------------------------------- 
        if (apbi_psel_i and apbi_pwrite_i) = '1' and slave_index = 0 then
            -- First core enable
            v(slave_index_int) := apbi_pwdata_i;
        elsif (apbi_psel_i and apbi_pwrite_i) = '1' and slave_index = 1 then
            -- Second core enable
            v(slave_index_int) := apbi_pwdata_i;
        elsif (apbi_psel_i and apbi_pwrite_i) = '1' and slave_index = 2 then
            -- First bit reset
            v(slave_index_int) := apbi_pwdata_i;
        elsif (apbi_psel_i and apbi_pwrite_i) = '1' and slave_index = 3 then
            -- Instruction remove diversity
            v(slave_index_int) := apbi_pwdata_i;
        end if;
        -- APB read -------------------------------------------------------------------------------
        -- If memory positions are added chan READ_POSITIONS constant **************
        apbo_prdata_o <= (others => '0');
        if (apbi_psel_i and apbi_penable_i) = '1' and apbi_pwrite_i = '0' and slave_index = REGISTERS_NUMBER then
            -- Read maximum staggering when the core1 is ahead
            apbo_prdata_o <= max_stag_core1;
        elsif (apbi_psel_i and apbi_penable_i) = '1' and apbi_pwrite_i = '0' and slave_index = REGISTERS_NUMBER+1  then
            -- Read minimum staggering when the core1 is ahead
            apbo_prdata_o <= min_stag_core1;
        elsif (apbi_psel_i and apbi_penable_i) = '1' and apbi_pwrite_i = '0' and slave_index = REGISTERS_NUMBER+2 then
            -- Read maximum staggering when the core2 is ahead
            apbo_prdata_o <= max_stag_core2;
        elsif (apbi_psel_i and apbi_penable_i) = '1' and apbi_pwrite_i = '0' and slave_index = REGISTERS_NUMBER+3 then
            -- Read minimum staggering when the core2 is ahead
            apbo_prdata_o <= min_stag_core2;
        elsif (apbi_psel_i and apbi_penable_i) = '1' and apbi_pwrite_i = '0' and slave_index = REGISTERS_NUMBER+4 then
            -- Read how many times the trail core becomes the head core
            apbo_prdata_o <= pass_counter;
        elsif (apbi_psel_i and apbi_penable_i) = '1' and apbi_pwrite_i = '0' and slave_index = REGISTERS_NUMBER+5 then
            -- Read the executed instructions the las time the trail core became the head core
            apbo_prdata_o <= last_pass;
        elsif (apbi_psel_i and apbi_penable_i) = '1' and apbi_pwrite_i = '0' and slave_index = REGISTERS_NUMBER+6 then
            -- Read the executed instructions the las time the trail core became the head core
            apbo_prdata_o <= ex_inst_core1;
        --if (apbi_psel_i and apbi_penable_i) = '1' and apbi_pwrite_i = '0' and slave_index = 3 then
        --    -- Read instruction difference
        --    apbo_prdata_o(r_inst_diff'LENGTH-1 downto 0) <= r_inst_diff;
        --elsif (apbi_psel_i and apbi_penable_i) = '1' and apbi_pwrite_i = '0' and slave_index = 4 then
        --    -- Read register signature difference
        --    apbo_prdata_o(r_reg_signature_diff'LENGTH-1 downto 0) <= r_reg_signature_diff;
        --elsif (apbi_psel_i and apbi_penable_i) = '1' and apbi_pwrite_i = '0' and slave_index = 5 then
        --    -- Read instruction (conc) signature difference
        --    apbo_prdata_o(r_inst_signature_conc_diff'LENGTH-1 downto 0) <= r_inst_signature_conc_diff;
        --elsif (apbi_psel_i and apbi_penable_i) = '1' and apbi_pwrite_i = '0' and (slave_index > 5) and (slave_index <= 5+INST_SIG_REGISTERS) then
        --    -- Read instruction signature core1 (several registers)
        --    apbo_prdata_o <= r_inst_signature_conc(0)(32*(slave_index_int-5)-1 downto 32*(slave_index_int-6));
        --elsif (apbi_psel_i and apbi_penable_i) = '1' and apbi_pwrite_i = '0' and (slave_index > 5+INST_SIG_REGISTERS) and (slave_index <= 5+2*INST_SIG_REGISTERS) then
        --    -- Read instruction signature core2 (several registers)
        --    apbo_prdata_o <= r_inst_signature_conc(1)(32*(slave_index_int-(5+INST_SIG_REGISTERS))-1 downto 32*(slave_index_int-(6+INST_SIG_REGISTERS)));
        elsif (apbi_psel_i and apbi_penable_i) = '1' and apbi_pwrite_i = '0' then
            -- Read histograms
            apbo_prdata_o <= histogram_mem_out;
        end if;
        -------------------------------------------------------------------------------------------

        -- update registers
        if rstn = '0' then
        -- if systems reset set all registers to 0
            rin <= (others => (others => '0'));
        elsif v(2)(0) = '1' then
        -- if rst bit is set, data from slack handler and reset bit are set to 0
            rin <= (others => (others => '0'));
        else
            rin <= v;
        end if;
        -- soft reset
        soft_rstn <= not v(2)(0);
    end process;
    -- If soft reset or regular reset is risen, all component resets
    internal_rstn <= soft_rstn and rstn;
    -- Core enables
    cores_enable(0) <= r(0)(0);
    cores_enable(1) <= r(1)(0);
    enable <= cores_enable(0) and cores_enable(1);
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

end;
