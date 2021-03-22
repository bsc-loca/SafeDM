---------------------------------------------------------------------------------------------------------
-- This module act as a memory with n position of 32 bits. Each position can be incremented by one.    --
-- To increment any position of the memory the address of the desire position has to be driven to      --
-- the input increment_addr. Any of the positions can be read as a normal memory using the signal      --
-- read_addr.                                                                                          --
---------------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity counters_memory is
    generic(
        counters_number : integer := 11
        );
    port (
        rstn   : in  std_ulogic;
        clk    : in  std_ulogic;
        enable : in  std_logic;
        -- Data to be stored
        read_addr      : in std_logic_vector( integer(ceil(log2(real(counters_number))))-1 downto 0); 
        increment_addr : in std_logic_vector( integer(ceil(log2(real(counters_number))))-1 downto 0); 
        data_o         : out std_logic_vector(31 downto 0)
    );
end;

architecture rtl of counters_memory is
    
    type mem_type is array (natural range <>) of unsigned(31 downto 0);
    signal mem, mem_n : mem_type(counters_number-1 downto 0);

begin
    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                mem <= (others => (others => '0'));
            else
                mem <= mem_n;
            end if;
        end if;
    end process;

    process(mem, mem_n, read_addr, increment_addr, enable)
    begin
        mem_n <= mem;
        if enable = '1' then
            mem_n(to_integer(unsigned(increment_addr))) <= mem(to_integer(unsigned(increment_addr))) + 1;
        end if;
        -- Could happen that read_addr is not a valid index, so we have to check wether it is or not
        if unsigned(read_addr) < counters_number then
            data_o <= std_logic_vector(mem(to_integer(unsigned(read_addr))));
        else
            data_o <= (others => '0');
        end if;
    end process;
end;
