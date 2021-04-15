library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity fifo_instructions is
    generic (
        SUMMATION      : integer := 0;
        CONCATENATION  : integer := 0;
        repetition     : integer := 1;
        fifo_positions : integer := 32;
        coding_bits    : integer := 1;
        SUM_SIG_BITS   : integer := 6;
        CONC_SIG_BITS  : integer := 64
    );
    port (
        rstn   : in std_ulogic;
        clk    : in std_ulogic;
        enable : in std_logic;
        fifo_input : in std_logic_vector(coding_bits*repetition-1 downto 0);
        signature_sum  : out std_logic_vector(SUM_SIG_BITS-1 downto 0); -- max value is maximum value per register * number of registers;
        signature_conc : out std_logic_vector(CONC_SIG_BITS-1 downto 0)
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

    -- Signature calculation ----------------------------------------------------------------------------------------------------------------
    -- The signature is calculated as the sumation of all the stored instructions.
    sum_signature: if SUMMATION = 1 generate
        process(fifo_input, fifo_mem, fifo_counter) 
            variable temp_sum  : unsigned(SUM_SIG_BITS-1 downto 0);
        begin
            fifo_mem_n <= fifo_mem;
            fifo_mem_n(to_integer(fifo_counter)) <= fifo_input;

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
            fifo_mem_n <= fifo_mem;
            fifo_mem_n(to_integer(fifo_counter)) <= fifo_input;

            for i in fifo_positions downto 1 loop
                temp_conc(i*coding_bits*repetition-1 downto (i-1)*coding_bits*repetition) := fifo_mem(i-1);
            end loop;
            signature_conc <= temp_conc;
            signature_sum  <= (others => '0');
        end process;
    end generate conc_signature;

    -----------------------------------------------------------------------------------------------------------------------------------------

end;
