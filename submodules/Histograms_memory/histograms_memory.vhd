library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library bsc;
use bsc.diversity_components_pkg.all;


entity histograms_memory is
    generic(
        INST_SIGNATURE_DIFF_BITS : integer := 6;   -- Bits needed to store the maximum difference between instructions signatrues
        REG_SIGNATURE_DIFF_BITS  : integer := 6;   -- Bits needed to store the maximum difference between registers signatrues
        CONC_SIGNATURE_DIFF_BITS : integer := 6;   -- 
        MAX_INST_SIGNATURE_DIFF  : integer := 32;  -- Biggest possible difference between instructions signatures
        MAX_REG_SIGNATURE_DIFF   : integer := 32;  -- Biggest possible difference between registers signatures
        MAX_CONC_SIGNATURE_DIFF  : integer := 32   -- 
        );
    port (
        rstn   : in  std_ulogic;
        clk    : in  std_ulogic;
        enable : in  std_logic;
        -- Data to be stored
        inst_diff_i           : in std_logic_vector(15 downto 0);                             -- Instructions difference between both cores 
        inst_signature_diff_i : in std_logic_vector(INST_SIGNATURE_DIFF_BITS-1 downto 0);     -- differenrence between both instructions signatures 
        reg_signature_diff_i  : in std_logic_vector(REG_SIGNATURE_DIFF_BITS-1 downto 0);      -- differenrence between both registers signatures  
        conc_signature_diff_i : in std_logic_vector(CONC_SIGNATURE_DIFF_BITS-1 downto 0);   
        -- Memory read
        addr_i    : in std_logic_vector(13 downto 0); --TODO: change it to adapt              -- Address to read the histograms
        -- Memory out
        data_o : out std_logic_vector(31 downto 0)                                            -- Data from the memories (histograms)
    );
end;

architecture rtl of histograms_memory is




    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- CONSTANTS -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    -- INSTRUCTIONS DIFFERENCE BETWEEN CORES------------------------------------------------------------------------------------

    -- ** This intervals can be changed anytime and the rest of the VHDL design will adapt **
    -- Define number of intervals for instructions difference between cores
    constant inst_intervals_number : integer := 12;
    -- Define the limits of the intervals
    type intervals_type is array (natural range <>) of integer;
    constant intervals : intervals_type(inst_intervals_number-1 downto 0) :=
        ( 0 => 0, 1 => 1, 2 => 2, 3 => 3, 4 => 4, 5 => 5, 6 => 11, 7 => 21, 8 => 51, 9 => 101, 10 => 301, 11 => 1001);

    -- Number of bits to address all possible intructions difference intervals
    constant INSTRUCTION_INTERVAL_BITS : integer := integer(ceil(log2(real(inst_intervals_number))));
    -----------------------------------------------------------------------------------------------------------------------------



    -- INSTRUCTIONS (SUM) -------------------------------------------------------------------------------------------------------

    -- ** The number of sections, its limits and its granularity can be change anytime and the rest of the VDHL design will adapt **
    -- For the instructions uneven sections of even intervals are defined. That is to say, each section contains n intervals with the same granularity.
    -- Define the number of sections of intervals in which the granularity is different
    constant inst_sig_interval_sections : integer := 6; 
    type inst_sig_diff_section_limit_type is array (0 to inst_sig_interval_sections-1) of integer;
    type inst_sig_diff_section_granularity_type is array (0 to inst_sig_interval_sections-2) of integer;

    -- These constatns of integer arrays denife the limits between sections and its granularity:
    constant inst_sig_diff_section_limits : inst_sig_diff_section_limit_type            := ( 0 => 0 , 1 => 30 , 2 => 100 , 3 => 400 , 4 => 1000, 5 => 10000);  --|=> number of intervals
    constant inst_sig_diff_section_granularity : inst_sig_diff_section_granularity_type := ( 0 => 1 , 1 => 5  , 2 => 20  , 3 => 50  , 4 => 1000);              --|          79
    -- Section 1 will have 30 intervals of 1, from 0 to 29. 0,1,2,3,4,...,29
    -- Section 2 will have 14 intervals of 5, from 30 to 100. 30-34,35-39,...,95-99
    -- ......
    
    -- This function is used to calculate the interval that correspond to a given instructions signature difference
    -- It is also used to calculate how many intervals of instructions signature difference there are
    function calculate_inst_diff_intervals(inst_sig_diff_section_limits : inst_sig_diff_section_limit_type ; inst_sig_diff_section_granularity : inst_sig_diff_section_granularity_type 
    ; diff_max : integer) return integer is
        variable intervals_number : integer;
        variable rest : integer;
    begin
        -- Given the sections limits and the granularity of each interval, given a difference between signatures instructions the interval can be calculated
        intervals_number := 0;
        rest := diff_max;
        for i in 0 to inst_sig_diff_section_limits'LENGTH-2 loop
            -- First we determine in which section the interval is
            if diff_max >= inst_sig_diff_section_limits(i) and diff_max < inst_sig_diff_section_limits(i+1) then
                -- If the value of diff_max corresponds to the section, we calculate the interval within the section
                -- To do so, we use the variable rest that contains the difference between diff_max and the section limit
                for j in 0 to ((inst_sig_diff_section_limits(i+1)-inst_sig_diff_section_limits(i))/inst_sig_diff_section_granularity(i))-1 loop 
                    if rest >= j*inst_sig_diff_section_granularity(i) and rest < (j+1)*inst_sig_diff_section_granularity(i) then
                        intervals_number := intervals_number + j; -- As it starts from 0 we have to add 1
                    end if;
                end loop;
            elsif diff_max >= inst_sig_diff_section_limits(i+1) then
                -- If the analyzed section does not correspond with the section of the interval, we add the total intervals in that section to the variable intervals number
                intervals_number := intervals_number + (inst_sig_diff_section_limits(i+1)-inst_sig_diff_section_limits(i))/inst_sig_diff_section_granularity(i); 
                -- We substract the instructions 
                rest := rest - (inst_sig_diff_section_limits(i+1)-inst_sig_diff_section_limits(i));
            end if;
        end loop;
        return intervals_number;
    end function;

    -- Define the number of intervals for instructions signature differences. As 0 is an interval we have to add 1
    constant inst_diff_sig_intervals_number : integer := calculate_inst_diff_intervals(inst_sig_diff_section_limits, inst_sig_diff_section_granularity, MAX_INST_SIGNATURE_DIFF)+1; 
    constant INST_DIFF_SIG_INTERVALS_NUMBER_BITS : integer := integer(ceil(log2(real(inst_diff_sig_intervals_number))));
    -- Constants for the memories containing the instructions signatures differences and the registers signatures differences histograms.
    constant inst_sig_mem_counters  : integer := inst_diff_sig_intervals_number*inst_intervals_number;
    constant INST_SIG_MEM_ADDR_BITS : integer := integer(ceil(log2(real(inst_sig_mem_counters)))); 
    -----------------------------------------------------------------------------------------------------------------------------

    -- INSTRUCTIONS (CONC) ------------------------------------------------------------------------------------------------------
    -- For the number of intervals for registers signature differences, we have to take on account the possibility of both signatures being equal
    constant conc_diff_sig_intervals_number : integer  := MAX_CONC_SIGNATURE_DIFF+1; 
    -- Constants for the memories containing the instructions signatures differences and the registers signatures differences histograms.
    constant conc_sig_mem_counters   : integer := conc_diff_sig_intervals_number*inst_intervals_number;
    constant CONC_SIG_MEM_ADDR_BITS  : integer := integer(ceil(log2(real(conc_sig_mem_counters)))); 
    -----------------------------------------------------------------------------------------------------------------------------
    
    -- REGISTERS ----------------------------------------------------------------------------------------------------------------
    -- For the number of intervals for registers signature differences, we have to take on account the possibility of both signatures being equal
    constant reg_diff_sig_intervals_number : integer  := MAX_REG_SIGNATURE_DIFF+1; 
    -- Constants for the memories containing the instructions signatures differences and the registers signatures differences histograms.
    constant reg_sig_mem_counters   : integer := reg_diff_sig_intervals_number*inst_intervals_number;
    constant REG_SIG_MEM_ADDR_BITS  : integer := integer(ceil(log2(real(reg_sig_mem_counters)))); 
    -----------------------------------------------------------------------------------------------------------------------------

    -- REGISTERS + INSTRUCTIONS (CONC) ------------------------------------------------------------------------------------------
    -- For the number of intervals for registers+inst signatures differences, we have to take on account the possibility of both signatures being equal
    constant comb_diff_sig_intervals_number : integer  := MAX_REG_SIGNATURE_DIFF+MAX_CONC_SIGNATURE_DIFF+1; 
    constant COMB_DIFF_SIG_INTERVALS_BITS : integer := integer(ceil(log2(real(comb_diff_sig_intervals_number))));
    -- Constants for the memories containing the instructions signatures differences and the registers signatures differences histograms.
    constant comb_sig_mem_counters   : integer := comb_diff_sig_intervals_number*inst_intervals_number;
    constant COMB_SIG_MEM_ADDR_BITS  : integer := integer(ceil(log2(real(comb_sig_mem_counters)))); 
    -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 


    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- SIGNALS --------------------------------------------------------------------------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- DIFFERENCE OF INSTRUCTIONS between both cores -----------------------------
    signal inst_diff : unsigned(15 downto 0);
    -- Difference of instruction interval that has to be incremented
    signal instructions_interval, r_instructions_interval : unsigned(INSTRUCTION_INTERVAL_BITS-1 downto 0);
    ------------------------------------------------------------------------------

    -- INSTRUCTIONS (SUM) --------------------------------------------------------
    -- Signal to select the difference of instructions signature interval which correspond to the address of the register of the memory that is incremented
    signal inst_diff_sig_interval, r_inst_diff_sig_interval : unsigned(INST_DIFF_SIG_INTERVALS_NUMBER_BITS-1 downto 0);
    -- Addresses to increment the proper memory register
    signal inst_sig_mem_addr : unsigned(INST_SIG_MEM_ADDR_BITS-1 downto 0);

    -- REGISTERS -----------------------------------------------------------------
    -- Signal to register the difference of registers signature interval which correspond to the address of the register of the memory that is incremented
    signal r_reg_diff_sig_interval : std_logic_vector(reg_signature_diff_i'HIGH downto 0);
    -- Addresses to increment the proper memory register
    signal reg_sig_mem_addr  : unsigned(REG_SIG_MEM_ADDR_BITS-1 downto 0);  
    ------------------------------------------------------------------------------

    -- INSTRUCTIONS (CONC) -------------------------------------------------------
    -- Signal to register the difference of registers signature interval which correspond to the address of the register of the memory that is incremented
    signal r_conc_diff_sig_interval : std_logic_vector(conc_signature_diff_i'HIGH downto 0);
    -- Addresses to increment the proper memory register
    signal conc_sig_mem_addr  : unsigned(CONC_SIG_MEM_ADDR_BITS-1 downto 0);  
    -- To match the width of the addition
    signal temp_reg_signature_diff_i : std_logic_vector(reg_signature_diff_i'HIGH+1 downto 0);
    ------------------------------------------------------------------------------
    
    -- REGISTERS + INSTRUCTIONS (CONC) -------------------------------------------
    -- Signal to register the difference of registers and instruction signatures interval which correspond to the address of the register of the memory that is incremented
    signal r_comb_diff_sig_interval : unsigned(COMB_DIFF_SIG_INTERVALS_BITS-1 downto 0);
    -- Addresses to increment the proper memory register
    signal comb_sig_mem_addr  : unsigned(COMB_SIG_MEM_ADDR_BITS-1 downto 0);  

    -- Signals to read memories -------------------------------------------------------------------
    signal output_inst_histogram     : std_logic_vector(31 downto 0);
    signal output_inst_sig_histogram : std_logic_vector(31 downto 0);
    signal output_reg_sig_histogram  : std_logic_vector(31 downto 0);
    signal output_conc_sig_histogram : std_logic_vector(31 downto 0);
    signal output_comb_sig_histogram : std_logic_vector(31 downto 0);

    signal r_addr    : std_logic_vector(addr_i'LENGTH-1 downto 0);
    signal read_addr : std_logic_vector(INST_SIG_MEM_ADDR_BITS-1 downto 0);
    -----------------------------------------------------------------------------------------------

    -- Signal to resgister and delay 1 cycle enable
    signal r_enable : std_logic;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

begin
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- It is necessary to register the selected intervals to avoid timing issues -------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- Needed to match the addition lengths
    temp_reg_signature_diff_i  <= '0' & reg_signature_diff_i;
    -- Since the signals to calculate the addresses of the memories are delayed one cycle, enable has to be delayed one cycle to avoid wrong readings
    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                r_instructions_interval  <= (others => '0');
                r_inst_diff_sig_interval <= (others => '0');
                r_reg_diff_sig_interval  <= (others => '0');
                r_conc_diff_sig_interval <= (others => '0');
                r_comb_diff_sig_interval <= (others => '0');
                r_enable                 <= '0';
            else
                r_instructions_interval  <= instructions_interval;
                r_inst_diff_sig_interval <= inst_diff_sig_interval;
                r_reg_diff_sig_interval  <= reg_signature_diff_i;
                r_conc_diff_sig_interval <= conc_signature_diff_i;
                r_comb_diff_sig_interval <= unsigned(temp_reg_signature_diff_i) + unsigned(conc_signature_diff_i);
                r_enable                 <= enable;
            end if;
        end if;
    end process;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- HISTOGRAM OF THE INSTRUCTIONS DIFFERENCE BETWEEN BOTH CORES ---------------------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    inst_diff <= unsigned(inst_diff_i);
    process(inst_diff)
    begin
        -- From the difference of instructions between cores, the interval that correspond to this difference is caculated based on the intervals previously defined
        instructions_interval <= (others => '0');
        -- To determine its interval we check every interval
        for i in 0 to inst_intervals_number-2 loop
            if inst_diff >= intervals(i) and inst_diff < intervals(i+1) then
                instructions_interval <= to_unsigned(i,instructions_interval'LENGTH);
            end if;
        end loop;
        -- If is bigger than the biggest interval it correspond to the last interval
        if inst_diff >= intervals(inst_intervals_number-1) then
            instructions_interval <= to_unsigned(inst_intervals_number-1,instructions_interval'LENGTH);
        end if;
    end process;
  
    -- A memory to store the data for the histogram is instanciated: it has as many registers(counters) as intervals has the histogram
    instructions_difference_histogram : counters_memory
    generic map(
        counters_number => inst_intervals_number
        )
    port map(
        rstn   => rstn,
        clk    => clk,
        enable => r_enable,
        read_addr      => read_addr(INSTRUCTION_INTERVAL_BITS-1 downto 0),
        increment_addr => std_logic_vector(r_instructions_interval),
        data_o         => output_inst_histogram 
        );
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- HISTOGRAM OF THE REGISTERS SIGNATURE DIFFERENCE BETWEEN BOTH CORES --------------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- The addrs of the interval that has to be incremented is calculated using the interval of instructions difference and the interval of registers signature difference.
    reg_sig_mem_addr <= to_unsigned(reg_diff_sig_intervals_number*to_integer(r_instructions_interval) + to_integer(unsigned(r_reg_diff_sig_interval)), reg_sig_mem_addr'LENGTH);   

    -- We want to generate as many histograms as instructions difference intervals exists. For each histogram we want to save how many times take place each value of 
    -- registers signature difference. Therefore we need a memory with instrucionts_difference_intervals*registers_signature_difference_intervals
    registers_sig_histogram : counters_memory
    generic map(
        counters_number => reg_sig_mem_counters
        )
    port map(
        rstn   => rstn,
        clk    => clk,
        enable => r_enable,
        read_addr      => read_addr(REG_SIG_MEM_ADDR_BITS-1 downto 0),  
        increment_addr => std_logic_vector(reg_sig_mem_addr), 
        data_o         => output_reg_sig_histogram
        );       
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- HISTOGRAM OF THE INSTRUCTIONS SIGNATURE (CONC) DIFFERENCE BETWEEN BOTH CORES ----------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- The addrs of the interval that has to be incremented is calculated using the interval of instructions difference and the interval of registers signature difference.
    conc_sig_mem_addr <= to_unsigned(conc_diff_sig_intervals_number*to_integer(r_instructions_interval) + to_integer(unsigned(r_conc_diff_sig_interval)), conc_sig_mem_addr'LENGTH);   

    -- We want to generate as many histograms as instructions difference intervals exists. For each histogram we want to save how many times take place each value of 
    -- registers signature difference. Therefore we need a memory with instrucionts_difference_intervals*registers_signature_difference_intervals
    instruction_conc_histogram : counters_memory
    generic map(
        counters_number => conc_sig_mem_counters
        )
    port map(
        rstn   => rstn,
        clk    => clk,
        enable => r_enable,
        read_addr      => read_addr(CONC_SIG_MEM_ADDR_BITS-1 downto 0),  
        increment_addr => std_logic_vector(conc_sig_mem_addr), 
        data_o         => output_conc_sig_histogram
        );   

    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- HISTOGRAM OF THE REGISTER+INSTRUCTIONS(CONC) DIFFERENCE BETWEEN BOTH CORES ------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- The addrs of the interval that has to be incremented is calculated using the interval of instructions difference and the interval of registers signature difference.
    comb_sig_mem_addr <= to_unsigned(comb_diff_sig_intervals_number*to_integer(r_instructions_interval) + to_integer(r_comb_diff_sig_interval), comb_sig_mem_addr'LENGTH);   

    -- We want to generate as many histograms as instructions difference intervals exists. For each histogram we want to save how many times take place each value of 
    -- registers signature difference. Therefore we need a memory with instrucionts_difference_intervals*registers_signature_difference_intervals
    instruction_comb_histogram : counters_memory
    generic map(
        counters_number => comb_sig_mem_counters
        )
    port map(
        rstn   => rstn,
        clk    => clk,
        enable => r_enable,
        read_addr      => read_addr(COMB_SIG_MEM_ADDR_BITS-1 downto 0),  
        increment_addr => std_logic_vector(comb_sig_mem_addr), 
        data_o         => output_comb_sig_histogram
        );   

    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- HISTOGRAM OF THE INSTRUCTIONS SIGNATURE (SUM) DIFFERENCE BETWEEN BOTH CORES -----------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- In this process we use the function calculate_inst_diff_intervals to calculate to which interval corresponds the value of instructions singature difference
    process(inst_signature_diff_i)
        variable inst_sig_diff_integer : integer;
        variable inst_sig_diff_interval_integer : integer;
    begin
        inst_sig_diff_integer := to_integer(unsigned(inst_signature_diff_i));
        inst_sig_diff_interval_integer := calculate_inst_diff_intervals(inst_sig_diff_section_limits, inst_sig_diff_section_granularity, inst_sig_diff_integer);
        -- As the function calculates the number of intervals needed for a difference, we have to substract one to the result of the function
        inst_diff_sig_interval <= to_unsigned(inst_sig_diff_interval_integer, inst_diff_sig_interval'LENGTH); 
    end process;

    -- The addrs of the interval that has to be incremented is calculated using the interval of instructions difference and the interval of instructions signature difference.
    inst_sig_mem_addr <= to_unsigned(inst_diff_sig_intervals_number*to_integer(r_instructions_interval) + to_integer(r_inst_diff_sig_interval), inst_sig_mem_addr'LENGTH);   

    -- We want to generate as many histograms as instructions difference intervals exists. For each histogram we want to save how many times take place each value of 
    -- instructions signature difference. Therefore we need a memory with instrucionts_difference_intervals*instructions_signature_difference_intervals
    instructions_sig_histogram : counters_memory
    generic map(
        counters_number => inst_sig_mem_counters 
        )
    port map(
        rstn   => rstn,
        clk    => clk,
        enable => r_enable, 
        read_addr      => read_addr,
        increment_addr => std_logic_vector(inst_sig_mem_addr),
        data_o         => output_inst_sig_histogram 
        );       
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- MEMORY READ ------------- -------------------------------------------------------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- To read the information of the histograms, the input address is used to read the memories. The first positions will read the difference of instructions histogram,
    -- adjacent positions will correspond to the instructions signature difference histograms, the last positions will correspond to the registers signature difference histograms.
    -- Therefore, we have to multiplex the output dependeng on the address. Since the memory has a one cycle latency, we have to delay one cycle the address used to read the data.
    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                r_addr  <= (others => '0');
            else
                r_addr  <= addr_i; 
            end if;
        end if;
    end process;

    -- In this process we multiplex the delayed output and we "addapt" the read_addr to be consistent witch each module address input
    process(r_addr, output_inst_histogram, output_inst_sig_histogram, output_reg_sig_histogram, output_conc_sig_histogram, output_comb_sig_histogram, addr_i) is
        constant inst_histogram_limit : integer := inst_intervals_number;
        constant reg_diff_histogram_limit  : integer := inst_histogram_limit      + inst_intervals_number*reg_diff_sig_intervals_number;
        constant conc_diff_histogram_limit : integer := reg_diff_histogram_limit  + inst_intervals_number*conc_diff_sig_intervals_number;
        constant comb_diff_histogram_limit : integer := conc_diff_histogram_limit + inst_intervals_number*comb_diff_sig_intervals_number;
        constant inst_diff_histogram_limit : integer := comb_diff_histogram_limit + inst_intervals_number*inst_diff_sig_intervals_number;
        variable addr : integer;
    begin
        addr := to_integer(unsigned(r_addr));
        if addr < inst_histogram_limit then
            -- Multiplex memory outputs
            data_o <= output_inst_histogram; 
            -- Calculate read_addr (input address of the memories)
            read_addr <= addr_i(INST_SIG_MEM_ADDR_BITS-1 downto 0);
        elsif (addr < reg_diff_histogram_limit) then
            -- Multiplex memory outputs
            data_o <= output_reg_sig_histogram; 
            -- Calculate read_addr (input address of the memories)
            addr := addr-inst_histogram_limit;
            read_addr <= std_logic_vector(to_unsigned(addr, read_addr'LENGTH));
        elsif (addr < conc_diff_histogram_limit) then
            -- Multiplex memory outputs
            data_o <= output_conc_sig_histogram; 
            -- Calculate read_addr (input address of the memories)
            addr := addr-reg_diff_histogram_limit;
            read_addr <= std_logic_vector(to_unsigned(addr, read_addr'LENGTH));
        elsif (addr < comb_diff_histogram_limit) then
            -- Multiplex memory outputs
            data_o <= output_comb_sig_histogram; 
            -- Calculate read_addr (input address of the memories)
            addr := addr-conc_diff_histogram_limit;
            read_addr <= std_logic_vector(to_unsigned(addr, read_addr'LENGTH));
        else -- if addr < inst_diff_histogram_limit then
            -- Multiplex memory outputs
            data_o <= output_inst_sig_histogram; 
            -- Calculate read_addr (input address of the memories)
            addr := addr-comb_diff_histogram_limit;
            read_addr <= std_logic_vector(to_unsigned(addr, read_addr'LENGTH));
        end if;
    end process;

end;
