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
        MAX_INST_SIGNATURE_DIFF  : integer := 32;  -- Biggest possible difference between instructions signatures
        MAX_REG_SIGNATURE_DIFF   : integer := 32   -- Biggest possible difference between registers signatures
        );
    port (
        rstn   : in  std_ulogic;
        clk    : in  std_ulogic;
        enable : in  std_logic;
        -- Data to be stored
        inst_diff_i           : in std_logic_vector(15 downto 0);                             -- Instructions difference between both cores 
        inst_signature_diff_i : in std_logic_vector(INST_SIGNATURE_DIFF_BITS-1 downto 0);     -- differenrence between both instructions signatures 
        reg_signature_diff_i  : in std_logic_vector(REG_SIGNATURE_DIFF_BITS-1 downto 0);      -- differenrence between both registers signatures  
        -- Memory read
        read_en_i : in std_logic; -- Unnecesary¿?
        addr_i    : in std_logic_vector(20 downto 0); --TODO: change it to adapt              -- Address to read the histograms
        -- Memory out
        data_o : out std_logic_vector(31 downto 0)                                            -- Data from the memories (histograms)
    );
end;

architecture rtl of histograms_memory is

    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- INSTRUCTIONS DIFFERENCE BETWEEN CORES INTERVALS DEFINITION (CONSTANTS) ----------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- Define number of intervals for differents instructions differences between cores
    constant inst_intervals_number : integer := 11;
    -- Define the range of the intervals
    type intervals_type is array (natural range <>) of integer;
    -- Define the intervals
    constant intervals : intervals_type(inst_intervals_number downto 0) :=
        ( 0 => 0, 1 => 1, 2 => 2, 3 => 3, 4 => 4, 5 => 5, 6 => 11, 7 => 21, 8 => 51, 9 => 101, 10 => 301, 11 => 1001);

    -- Number of bits to address all possible intructions difference between cores intervals
    constant INSTRUCTION_INTERVAL_BITS : integer := integer(ceil(log2(real(inst_intervals_number))));
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- INSTRUCTIONS SIGNATURE DIFFERENCE INTERVALS AND SECTIONS DEFINITION (CONSTANTS) -------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- Number of sections of intervals in which the granularity is different
    constant inst_sig_interval_sections : integer := 4; 
    type inst_sig_diff_section_limit_type is array (0 to inst_sig_interval_sections-1) of integer;
    type inst_sig_diff_section_granularity_type is array (0 to inst_sig_interval_sections-1) of integer;

    -- These constatns of integer arrays denife the limits between sections and its granularity:
                                                                                         -- Section1  Section2  Section3   Section4
    constant inst_sig_diff_section_limits : inst_sig_diff_section_limit_type            := ( 0 => 0 , 1 => 30 , 2 => 100 , 3 => 200 ); 
    constant inst_sig_diff_section_granularity : inst_sig_diff_section_granularity_type := ( 0 => 1 , 1 => 5  , 2 => 20  , 3 => 50 );

    -- If the difference between the limits of two sections is 30 and the granularity of that section is 2, then, there are 15 intervals in that sections. I the maximum posible difference
    -- between the instructions signatures is 25 and there is just one section being 30 and its granularity 2 the number of intervals will be 13. 
    -- The limits granularities and the number of intervals can be changed here without any further change in any other part of the design.

    -- Function to caltulate the number of intervals given to a maximum possible instructions signatures difference
    -- Given a instructions difference, this function also calculates which interval has to be incremented
    function calculate_inst_diff_intervals(inst_sig_diff_section_limits : inst_sig_diff_section_limit_type ; inst_sig_diff_section_granularity : inst_sig_diff_section_granularity_type 
    ; diff_max_1 : integer) return integer is
        variable intervals_number : integer;
        variable rest : integer;
        variable diff_max :integer;
    begin
        -- Given the sections limits and the granularity of each interval, given a difference between signatures instructions the interval can be calculated
        diff_max := diff_max_1 + 1; -- 1 is added since the interval of 0 (no difference) has to be taken on account
        intervals_number := 0;
        rest := diff_max;
        for i in 0 to inst_sig_diff_section_limits'LENGTH-2 loop
            if diff_max >= inst_sig_diff_section_limits(i) and diff_max < inst_sig_diff_section_limits(i+1) then
                intervals_number := intervals_number + integer(ceil(real(rest/inst_sig_diff_section_granularity(i))));
            elsif diff_max >= inst_sig_diff_section_limits(i+1) then
                intervals_number := intervals_number + integer(ceil(real(inst_sig_diff_section_limits(i+1)/inst_sig_diff_section_granularity(i))));
                rest := rest - inst_sig_diff_section_limits(i+1);
            end if;
        end loop;
        if diff_max >= inst_sig_diff_section_limits(inst_sig_diff_section_limits'LENGTH-1) then
            intervals_number := intervals_number + integer(ceil(real(rest/inst_sig_diff_section_granularity(inst_sig_diff_section_limits'LENGTH-1))));
        end if;
        return intervals_number;
    end function;
    
    -- Define the number of intervals for differences in the signatures
    constant inst_diff_sig_intervals_number : integer := calculate_inst_diff_intervals(inst_sig_diff_section_limits, inst_sig_diff_section_granularity, MAX_INST_SIGNATURE_DIFF);
    constant reg_diff_sig_intervals_number : integer  := MAX_REG_SIGNATURE_DIFF;  
    constant INST_DIFF_SIG_INTERVALS_NUMBER_BITS : integer := integer(ceil(log2(real(inst_diff_sig_intervals_number))));
    -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- SIGNALS --------------------------------------------------------------------------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- Signals to write memories ---
    -- Difference of instructions between both cores
    signal inst_diff : unsigned(15 downto 0);
    -- Difference of instruction interval that has to be incremented
    signal instructions_interval : unsigned(INSTRUCTION_INTERVAL_BITS-1 downto 0);

    -- Signal to select the ram(instruction difference interval) when writing the difference in the signature 
    signal mem_selection : unsigned(inst_intervals_number-1 downto 0); 

    -- Signal to select the difference of instructions signature interval which correspond to the address of the register of the memory that is incremented
    signal inst_diff_sig_interval : unsigned(INST_DIFF_SIG_INTERVALS_NUMBER_BITS-1 downto 0);


    -- Signals to read memories ---
    type memory_output is array (natural range <>) of std_logic_vector(31 downto 0);
    signal output_inst_histogram     : std_logic_vector(31 downto 0);
    signal output_inst_sig_histogram : memory_output(inst_intervals_number-1 downto 0);
    signal output_reg_sig_histogram  : memory_output(inst_intervals_number-1 downto 0);

    signal read_addr : std_logic_vector(INST_DIFF_SIG_INTERVALS_NUMBER_BITS-1 downto 0);
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

begin

    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- HISTOGRAM OF THE INSTRUCTIONS DIFFERENCE BETWEEN BOTH CORES ---------------------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    inst_diff <= unsigned(inst_diff_i);
    process(inst_diff, instructions_interval)
    begin
        -- From the difference of instructions between cores, the interval that correspond to this difference is caculated based on the intervals previously defined
        for i in 0 to inst_intervals_number-2 loop
            if inst_diff >= intervals(i) and inst_diff < intervals(i+1) then
                instructions_interval <= to_unsigned(i,instructions_interval'LENGTH);
            end if;
        end loop;
        if inst_diff >= intervals(inst_intervals_number-1) then
            instructions_interval <= to_unsigned(inst_intervals_number-1,instructions_interval'LENGTH);
        end if;

        -- Change instructions_interval to one hot encoding to enable the memory that we want to be written. 
        for i in inst_intervals_number-1 downto 0 loop
            if instructions_interval = i then
                mem_selection(i) <= '1';
            else
                mem_selection(i) <= '0';
            end if;
        end loop;
    end process;
  
    -- A memory to store the data for the histogram is instanciated: it has as many registers as intervals has the histogram
    instructions_difference_histogram : counters_memory
    generic map(
        counters_number => inst_intervals_number
        )
    port map(
        rstn   => rstn,
        clk    => clk,
        enable => '1',
        read_addr      => read_addr(INSTRUCTION_INTERVAL_BITS-1 downto 0),
        increment_addr => std_logic_vector(instructions_interval),
        data_o         => output_inst_histogram 
        );
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- HISTOGRAM OF THE INSTRUCTIONS SIGNATURE DIFFERENCE BETWEEN BOTH CORES -----------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- This process calculates the inputs of the instrucions signatures histograms memories
    process(inst_signature_diff_i)
        variable inst_sig_diff_integer : integer;
        variable inst_sig_diff_interval_integer : integer;
    begin
        -- It calculates the interval of difference o instructions signatures that has to be incremented
        inst_sig_diff_integer := to_integer(unsigned(inst_signature_diff_i));
        inst_sig_diff_interval_integer := calculate_inst_diff_intervals(inst_sig_diff_section_limits, inst_sig_diff_section_granularity, inst_sig_diff_integer);
        -- As the function calculates the number of intervals needed for a difference, we have to substract one to the result of the function
        inst_diff_sig_interval <= to_unsigned(inst_sig_diff_interval_integer-1, inst_diff_sig_interval'LENGTH); 
    end process;


    -- We want to generate one histogram per instructions difference interval so we need to instanciate as many mameories as instructions signature difference intervals (x)
    -- Each memory will have as many registers as intervals of instructions differences in the signature (y) 
    -- Therefore we have x memories of y positions
    signature_calc_inst : for n in inst_intervals_number-1 downto 0 generate
        instructions_histogram : counters_memory
        generic map(
            counters_number => inst_diff_sig_intervals_number 
            )
        port map(
            rstn   => rstn,
            clk    => clk,
            enable => mem_selection(n),
            read_addr      => read_addr,
            increment_addr => std_logic_vector(inst_diff_sig_interval),
            data_o         => output_inst_sig_histogram(n) 
            );       
    end generate signature_calc_inst;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- HISTOGRAM OF THE REGISTERS SIGNATURE DIFFERENCE BETWEEN BOTH CORES --------------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- We want to generate one histogram per registers difference interval so we need to instanciate as many mameories as registers difference interval intervals (x)
    -- Each memory will have as many registers as intervals of registers differences in the signature (y) 
    -- Therefore we have x memories of y positions
    signature_calc_regs : for n in inst_intervals_number-1 downto 0 generate
        registers_histogram : counters_memory
        generic map(
            counters_number => MAX_REG_SIGNATURE_DIFF
            )
        port map(
            rstn   => rstn,
            clk    => clk,
            enable => mem_selection(n),
            read_addr      => read_addr(REG_SIGNATURE_DIFF_BITS-1 downto 0),  
            increment_addr => reg_signature_diff_i, -- The number of interval it is the difference in the signature
            data_o         => output_reg_sig_histogram(n)
            );       
    end generate signature_calc_regs;
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- MEMORY OUTPUT MULTIPLEXER -------------------------------------------------------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- Based on the input address chose the correct memory output to be forwarded to the output of the component
    process(addr_i, output_inst_histogram, output_inst_sig_histogram, output_reg_sig_histogram) is
        variable read_addr_integer : integer;

        constant inst_histogram_limit : integer := inst_intervals_number;
        constant inst_diff_histogram_limit : integer := inst_histogram_limit + inst_intervals_number*inst_diff_sig_intervals_number;
        constant reg_diff_histogram_limit : integer  := inst_diff_histogram_limit + inst_intervals_number*reg_diff_sig_intervals_number;
        variable addr : integer;
    begin
        addr := to_integer(unsigned(addr_i));
        if addr < inst_histogram_limit then
            -- Multiplex memory outputs
            data_o <= output_inst_histogram; 
            -- Calculate read_addr (input address of the memories)
            read_addr <= addr_i;
        elsif addr < inst_diff_histogram_limit then
            -- Multiplex memory outputs
            for n in 0 to inst_intervals_number-1 loop
                if addr-inst_histogram_limit-1 >= n*inst_diff_sig_intervals_number and addr-inst_histogram_limit-1 < (n+1)*inst_diff_sig_intervals_number then
                    data_o <= output_inst_sig_histogram(n);
                end if
            end loop;
            -- Calculate read_addr (input address of the memories)
            for n in 0 to inst_intervals_number-1 loop
                if addr_i-inst_histogram_limit-1 >= n*inst_diff_sig_intervals_number and addr-inst_histogram_limit-1 < (n+1)*inst_diff_sig_intervals_number then
                    data_o <= output_inst_sig_histogram(n);
                end if
            end loop;
            read_addr_integer := (addr-inst_histogram_limit) rem inst_diff_sig_intervals_number; 
            read_addr <= std_logic_vector(to_unsigned(read_addr_integer, read_addr'LENGTH));
        else --if (addr < reg_diff_histogram_limit)
            -- Multiplex memory outputs
            for n in 0 to inst_intervals_number-1 loop
                if addr-inst_diff_histogram_limit-1 >= n*reg_diff_sig_intervals_number and addr-inst_diff_histogram_limit-1 < (n+1)*reg_diff_sig_intervals_number then
                    data_o <= output_reg_sig_histogram(n);
                end if
            end loop;
            -- Calculate read_addr (input address of the memories)
            read_addr_integer := (addr-inst_diff_histogram_limit) rem reg_diff_sig_intervals_number; 
            read_addr <= std_logic_vector(to_unsigned(read_addr_integer, read_addr'LENGTH));
        end if;
    end process;

end;
