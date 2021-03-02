library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library bsc;
use bsc.diversity_types_pkg.all;
use bsc.diversity_components_pkg.all;

entity mem_regs_sign is
    generic (
        regs_number : integer := 32;
        coding_bits : integer := 1
    );
    port (
        rstn    : in  std_ulogic;
        clk     : in  std_ulogic;
        -- Port 1
        we_1    : in std_logic;   
        wdata_1 : in std_logic_vector(coding_bits-1 downto 0); 
        addr_1  : in std_logic_vector(4 downto 0); 
        -- Port 2
        we_2    : in std_logic; 
        wdata_2 : in std_logic_vector(coding_bits-1 downto 0);  
        addr_2  : in std_logic_vector(4 downto 0); 
        -- Output
        reg_signature : out std_logic_vector(regs_number*coding_bits-1 downto 0)
     );
end;


architecture rtl of mem_regs_sign is

    -- Signals
    type memory is array (natural range <>) of std_logic_vector(coding_bits-1 downto 0); 
    signal memory_regs, memory_regs_n : memory(regs_number-1 downto 0);

begin

    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                memory_regs <= (others => (others => '0'));
            else
                memory_regs <= memory_regs_n;
            end if;
        end if;
    end process;

    process(memory_regs, we_1, we_2, wdata_1, wdata_2, addr_1, addr_2)
    begin
        memory_regs_n <= memory_regs;
        -- Port 1
        if we_1 = '1' then
            memory_regs_n(to_integer(unsigned(addr_1))) <= wdata_1;
        end if;
        -- Port 2
        if we_2 = '1' then
            memory_regs_n(to_integer(unsigned(addr_2))) <= wdata_2;
        end if;
    end process;
           
    
    process(memory_regs)
        variable temp : std_logic_vector(regs_number*coding_bits-1 downto 0) := (others => '0');
    begin
        for i in regs_number to 1 loop
            temp(i*coding_bits-1 downto (i-1)*coding_bits) := memory_regs(i);
        end loop;
        reg_signature <= temp;
    end process;



    
end;
