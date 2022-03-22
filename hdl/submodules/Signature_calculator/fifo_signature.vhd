-----------------------------------------------------------------------------------------------------------------------------------------------
--
--   Project              :  SafeDM
--   File name            :  fifo_signature.vhd
--   Title                :  fifo_signature
--   Description          :  This module implements a FIFO which also generates as the output the concatenated vector of all its positions 
--   Design library       :  bsc
--   Analysis dependency  :  None
--   Initialization       :  Initialized by RESET
--   Notes                :  None
--   Simulator(s)         :  QuestaSim-64 10.7c
-----------------------------------------------------------------------------------------------------------------------------------------------
--    Revisions :
--           Date           Author        Revision             Comments
--        03/17/2022     Francisco Bas      1.0         Finished first version
--                       francisco.basjalon@bsc.es
-----------------------------------------------------------------------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity fifo_signature is
    generic (
        repetition     : integer := 1;  -- Number of bits in a FIFO position is coding_bits multiplied by coding_bits (useful for multilane processors)
        fifo_positions : integer := 32; -- Number of memory positions in the FIFO
        coding_bits    : integer := 1;  -- Number of bits of each position
        sig_bits       : integer := 64  -- Resulting number of bits when all the bist in all the positions are added
    );
    port (
        rstn   : in std_ulogic;
        clk    : in std_ulogic;
        enable : in std_logic;
        shift  : in std_logic;                                                -- When this input is set to 1, the FIFO acts as a shift register (used for the hold signal)
        fifo_input : in std_logic_vector(coding_bits*repetition-1 downto 0);  -- Values that are stored
        signature : out std_logic_vector(sig_bits-1 downto 0)            -- Concatenation signature output
    );
end;

architecture rtl of fifo_signature is
    
    -- FIFO signals
    type fifo_type is array (natural range <>) of std_logic_vector(coding_bits*repetition-1 downto 0); 
    signal fifo_mem, fifo_mem_n : fifo_type(fifo_positions-1 downto 0);

    constant fifo_counter_bits : integer := integer(ceil(log2(real(fifo_positions))));
    signal fifo_counter : unsigned(fifo_counter_bits-1 downto 0); -- Indicates which position of the vector has to be uploaded each time
begin


    -- FIFO ---------------------------------------------------------------------------------------------------------------------------------
    -- Every cycle a new input is stored and the oldest stored input is discarded.
    -- Each cycle the position indicated by fifo_counter is uploaded
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
    -- If the hold signal is low, just the vector position pointed by fifo_counter is modified.
    process(fifo_input, fifo_mem, fifo_counter, shift) 
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


    -- The signature is calculated as the concatenation of all the stored instructions.
    process(fifo_input, fifo_mem, fifo_counter) 
        variable temp_conc : std_logic_vector(SIG_BITS-1 downto 0);
    begin
        for i in fifo_positions downto 1 loop
            temp_conc(i*coding_bits*repetition-1 downto (i-1)*coding_bits*repetition) := fifo_mem(i-1);
        end loop;
        signature <= temp_conc;
    end process;

    -----------------------------------------------------------------------------------------------------------------------------------------

end;
