library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity fifo_instructions is
    generic (
        SUMMATION      : integer := 0;  -- Has to be set to one if the signature is calculated adding all the positions of the FIFO
        CONCATENATION  : integer := 0;  -- Has to be set to one if the signature is calculated concatenating all the positions of the FIFO
        repetition     : integer := 1;  -- Number of instructions stored in each position of the FIFO
        fifo_positions : integer := 32; -- Number of memory positions in the FIFO
        coding_bits    : integer := 1;  -- Number of bits of each position
        SUM_SIG_BITS   : integer := 6;  -- Number of summation signature bits
        CONC_SIG_BITS  : integer := 64  -- Number of concatenation signature bits
    );
    port (
        rstn   : in std_ulogic;
        clk    : in std_ulogic;
        enable : in std_logic;
        shift  : in std_logic;                                                -- When this input is set to 1, the FIFO acts as a shift register
        fifo_input : in std_logic_vector(coding_bits*repetition-1 downto 0);  -- Values that are stored
        signature_sum  : out std_logic_vector(SUM_SIG_BITS-1 downto 0);       -- Summaiton signature output
        signature_conc : out std_logic_vector(CONC_SIG_BITS-1 downto 0)       -- Concatenation signature output
    );
end;

architecture rtl of fifo_instructions is
    
    -- FIFO signals
    type fifo_type is array (natural range <>) of std_logic_vector(coding_bits*repetition-1 downto 0); 
    signal fifo_mem, fifo_mem_n : fifo_type(fifo_positions-1 downto 0);

    constant FIFO_COUNTER_BITS : integer := integer(ceil(log2(real(fifo_positions))));
    signal fifo_counter : unsigned(FIFO_COUNTER_BITS-1 downto 0);
begin

    -- This is a FIFO with many positions as 'sabed_inst'. Every position of the fifo has space for two instructions, one of each lane.

    -- FIFO ---------------------------------------------------------------------------------------------------------------------------------
    -- Every cycle new pair of instructions is stored and the less recently stored are discarded.
    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                fifo_counter <= (others => '0');
                fifo_mem <= ( others =>(others => '0' ));
            else
                if enable = '1' then
                    fifo_mem <= fifo_mem_n;
                    if fifo_counter = fifo_positions-1 then
                        fifo_counter <= (others => '0');
                    else
                        fifo_counter <= fifo_counter +1;
                    end if;
                end if;
            end if;
        end if;
    end process;
    -----------------------------------------------------------------------------------------------------------------------------------------

    -- The shift signal is driven by the hold signal. When the hold signal is active all the values in the registers
    -- have to shift. In this way, the last 6 executed instructions are always compared in the rigth order.
    process(fifo_input, fifo_mem, fifo_counter, shift) 
        variable temp_sum  : unsigned(SUM_SIG_BITS-1 downto 0);
    begin
        if shift = '0' then
            fifo_mem_n <= fifo_mem;
            fifo_mem_n(to_integer(fifo_counter)) <= fifo_input;
        else 
            fifo_mem_n(0) <= fifo_mem(fifo_positions-1);
            for i in 1 to fifo_positions-1 loop
                fifo_mem_n(i) <= fifo_mem(i-1);
            end loop;
        end if;
    end process;

    -- Signature calculation ----------------------------------------------------------------------------------------------------------------
    -- The signature is calculated as the sumation of all the stored instructions.
    sum_signature: if SUMMATION = 1 generate
        process(fifo_input, fifo_mem, fifo_counter) 
            variable temp_sum  : unsigned(SUM_SIG_BITS-1 downto 0);
        begin
            temp_sum := (others => '0');
            -- TODO change the loop to work in funciton of repetition
            for j in fifo_positions-1 downto 0 loop
                temp_sum := temp_sum + unsigned(fifo_mem(j)(coding_bits*repetition-1 downto coding_bits)) + unsigned(fifo_mem(j)(coding_bits-1 downto 0));
            end loop;
            signature_sum  <= std_logic_vector(temp_sum);
            signature_conc <= (others => '0');
        end process;
    end generate sum_signature;



    -- The signature is calculated as the concatenation of all the stored instructions.
    conc_signature: if CONCATENATION = 1 generate
        process(fifo_input, fifo_mem, fifo_counter) 
            variable temp_conc : std_logic_vector(CONC_SIG_BITS-1 downto 0);
        begin
            for i in fifo_positions downto 1 loop
                temp_conc(i*coding_bits*repetition-1 downto (i-1)*coding_bits*repetition) := fifo_mem(i-1);
            end loop;
            signature_conc <= temp_conc;
            signature_sum  <= (others => '0');
        end process;
    end generate conc_signature;

    -----------------------------------------------------------------------------------------------------------------------------------------

end;
