library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library bsc;
use bsc.diversity_types_pkg.all;
use bsc.diversity_components_pkg.all;

entity diversity_quantifier_top is
    generic (
        coding_method    : integer := 2;
        coding_bits_reg  : integer := 8;
        coding_bits_inst : integer := 7;
        regs_number      : integer := 32;
        saved_inst       : integer := 8
        );
    port (
        rstn           : in  std_ulogic;
        clk            : in  std_ulogic;
        -- apb signals
        apbi_psel_i    : in  std_logic;                       
        apbi_paddr_i   : in  std_logic_vector(31 downto 0);                      
        apbi_penable_i : in  std_logic;                     
        apbi_pwrite_i  : in  std_logic;
        apbi_pwdata_i  : in  std_logic_vector(31 downto 0);                   
        apbo_prdata_o  : out std_logic_vector(31 downto 0);     
        -- Singals to calculate sigantures
        -- Instructions signature
        instructions_i : in instruction_type_vector;
        -- Registers signatures
        registers_i : in register_type_vector;
        -- Instruction counters
        icnt1_i : in std_logic_vector(1 downto 0);
        icnt2_i : in std_logic_vector(1 downto 0)
     );
end;


architecture rtl of diversity_quantifier_top is
    -- Number of bits for the signals of the signatures -----------------------------------------------------------------
    constant REG_SIG_BITS : integer := coding_bits_reg*regs_number;
    constant INST_SUM_SIG_BITS : integer := integer(ceil(log2(real(((2 ** coding_bits_inst)-1)*saved_inst*2))));
    constant INST_CONC_SIG_BITS : integer := coding_bits_inst*saved_inst*2;
    ---------------------------------------------------------------------------------------------------------------------


    -- Singals for the signatures ---------------------------------------------------------------------------------------
    type reg_signature_array is array (natural range <>) of std_logic_vector(REG_SIG_BITS-1 downto 0);
    signal reg_signature : reg_signature_array(1 downto 0);

    type inst_signature_sum_array is array (natural range <>) of std_logic_vector(INST_SUM_SIG_BITS-1 downto 0);
    signal inst_signature_sum : inst_signature_sum_array(1 downto 0);

    type inst_signature_conc_array is array (natural range <>) of std_logic_vector(INST_CONC_SIG_BITS-1 downto 0);
    signal inst_signature_conc : inst_signature_conc_array(1 downto 0);
    ---------------------------------------------------------------------------------------------------------------------

    -- Signal and constant for the differences of the signatures --------------------------------------------------------
    constant MAX_INST_SIGNATURE_DIFF : integer := ((2 ** coding_bits_inst)-1)*saved_inst*2;
    constant MAX_REG_SIGNATURE_DIFF : integer := regs_number;
    constant MAX_REG_SIGNATURE_DIFF_BITS : integer := integer(ceil(log2(real(regs_number+1))));
    signal reg_signature_diff, r_reg_signature_diff : std_logic_vector(MAX_REG_SIGNATURE_DIFF_BITS-1 downto 0);
    signal inst_signature_conc_diff : std_logic_vector(INST_CONC_SIG_BITS-1 downto 0);
    signal inst_signature_sum_diff, r_inst_signature_sum_diff : unsigned(INST_SUM_SIG_BITS-1  downto 0); -- The bits of the signatures are calculated thinking in the maximum posible difference
    ---------------------------------------------------------------------------------------------------------------------

    -- instructions difference between cores
    signal inst_diff, r_inst_diff : std_logic_vector(15 downto 0);

    -- Histograms memorie signals
    signal histogram_read_addr : unsigned(13 downto 0);
    signal histogram_mem_out   : std_logic_vector(31 downto 0);
    
    -- Enable signals
    signal cores_enable : std_logic_vector(1 downto 0);
    signal enable, r_enable : std_logic;

    -- APB bus ----------------------------------------------------------------------------------------------------------
    -- The number or registers can be changed but has to be bigger than 2 for the rest of the design to automatically adapt
    type registers_vector is array (natural range <>) of std_logic_vector(31 downto 0);
    constant REGISTERS_NUMBER : integer := 3;
    signal r, rin      : registers_vector(REGISTERS_NUMBER-1 downto 0) ;
    signal slave_index : unsigned(13 downto 0);

    -- soft reset through APB bus
    signal soft_rstn : std_ulogic;
    signal internal_rstn : std_ulogic;
    ---------------------------------------------------------------------------------------------------------------------

begin

    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- SIGNATURE CACLCULATION MODULES --------------------------------------------------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- Two modules to calculate the signatures are instanciated, one for each core
    signature_calc_inst : for n in 0 to 1 generate
        signature_calculator_inst : signature_calculator
        generic map(
            coding_method      => coding_method,
            coding_bits_reg    => coding_bits_reg,
            coding_bits_inst   => coding_bits_inst,
            regs_number        => regs_number,
            saved_inst         => saved_inst, 
            REG_SIG_BITS       => REG_SIG_BITS,
            INST_SUM_SIG_BITS  => INST_SUM_SIG_BITS,
            INST_CONC_SIG_BITS => INST_CONC_SIG_BITS
            )
        port map(
            rstn   => internal_rstn, 
            clk    => clk, 
            enable => cores_enable(n),
            -- Instructions signature
            instructions_i => instructions_i(n),
            -- Registers signatures
            registers_i => registers_i(n),
            -- Outputs
            reg_signature_o        => reg_signature(n),
            inst_signature_sum_o   => inst_signature_sum(n),
            inst_signature_conc_o  => inst_signature_conc(n)
            );
    end generate signature_calc_inst; 

    -- In this process, the diference between the signatures of both cores are computed
    process(reg_signature, inst_signature_sum, inst_signature_conc)
        variable temp_xor       : std_logic;
        variable temp_regs      : unsigned(MAX_REG_SIGNATURE_DIFF_BITS-1 downto 0);
        variable temp_inst_conc : unsigned(INST_CONC_SIG_BITS-1 downto 0);
    begin
        -- REGISTER SIGNATURE
        -- Number of diferent registers in the registers signature
        -- Registers value stored in the different positions of the regsiters signature are compared one by one
        temp_regs := (others => '0');
        for i in integer(regs_number) downto 1 loop
            if reg_signature(0)(i*coding_bits_reg-1 downto (i-1)*coding_bits_reg) /= reg_signature(1)(i*coding_bits_reg-1 downto (i-1)*coding_bits_reg) then -- It compares registers one by one 
                temp_regs := temp_regs + 1;                                                                                                         -- Independently of the coding bits and regs number
            end if;
        end loop;
        reg_signature_diff <= std_logic_vector(temp_regs);

        -- THIS SIGNATURE IS NOT USED AT THE MOMENT!! -------
        -- Number of different bits in insts sum signature (concatenation). 
        temp_inst_conc := (others => '0');
        for i in temp_inst_conc'HIGH downto 0 loop
            temp_xor := inst_signature_conc(0)(i) xor inst_signature_conc(1)(i);
            if temp_xor = '1' then
                temp_inst_conc := temp_inst_conc + 1;
            end if;
        end loop;
        inst_signature_conc_diff <= std_logic_vector(temp_inst_conc);
 
        -- INSTRUCTION SIGNATURE
        -- Asolute numeric difference between both instructions signatures
        if unsigned(inst_signature_sum(0)) > unsigned(inst_signature_sum(1)) then
            inst_signature_sum_diff <= unsigned(inst_signature_sum(0)) - unsigned(inst_signature_sum(1)); 
        else
            inst_signature_sum_diff <= unsigned(inst_signature_sum(1)) - unsigned(inst_signature_sum(0)); 
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
        enable_core1_i => cores_enable(0),
        enable_core2_i => cores_enable(1),
        icnt1_i        => icnt1_i,
        icnt2_i        => icnt2_i,
        inst_diff_o    => inst_diff
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
        REG_SIGNATURE_DIFF_BITS  => MAX_REG_SIGNATURE_DIFF_BITS,
        MAX_INST_SIGNATURE_DIFF  => MAX_INST_SIGNATURE_DIFF,
        MAX_REG_SIGNATURE_DIFF   => MAX_REG_SIGNATURE_DIFF
        )
    port map(
        rstn   => internal_rstn, 
        clk    => clk, 
        enable => r_enable,
        -- Data to be stored
        inst_diff_i           => r_inst_diff,
        inst_signature_diff_i => std_logic_vector(r_inst_signature_sum_diff),
        reg_signature_diff_i  => r_reg_signature_diff,
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
                r_inst_diff               <= (others => '0');
                r_inst_signature_sum_diff <= (others => '0');
                r_reg_signature_diff      <= (others => '0');
                r_enable                  <= '0';
            else
                r_inst_diff               <= inst_diff;
                r_inst_signature_sum_diff <= inst_signature_sum_diff;
                r_reg_signature_diff      <= reg_signature_diff;
                r_enable                  <= enable;
            end if;
        end if;
    end process;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    

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
    histogram_read_addr <= slave_index-REGISTERS_NUMBER; 

    comb : process(rstn, apbi_penable_i, apbi_psel_i, apbi_pwrite_i, apbi_pwdata_i, slave_index, histogram_mem_out, r) is 
        variable v : registers_vector(REGISTERS_NUMBER-1 downto 0);
    begin
        v := r;
        -- From the reads the answer will be the answer of the histograms memory whose 
        -- input is driven by the address of the apb bus.
        apbo_prdata_o <= (others => '0');
        if (apbi_psel_i and apbi_penable_i) = '1' and apbi_pwrite_i = '0' then
            apbo_prdata_o <= histogram_mem_out;
        end if;
        -- Write registers -------------------------------------------------------------- 
        if (apbi_psel_i and apbi_pwrite_i) = '1' and unsigned(slave_index) = 0 then
            -- First core enable
            v(to_integer(slave_index)) := apbi_pwdata_i;
        elsif (apbi_psel_i and apbi_pwrite_i) = '1' and unsigned(slave_index) = 1 then
            -- Second core enable
            v(to_integer(slave_index)) := apbi_pwdata_i;
        elsif (apbi_psel_i and apbi_pwrite_i) = '1' and unsigned(slave_index) = 2 then
            -- First bit reset
            v(to_integer(slave_index)) := apbi_pwdata_i;
        end if;
        ---------------------------------------------------------------------------------

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
