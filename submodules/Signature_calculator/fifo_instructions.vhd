library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity fifo_instructions is
    generic (
        saved_inst : integer := 32;
        coding_bits : integer := 1
    );
    port (
        rstn       : in std_ulogic;
        clk        : in std_ulogic;
        fifo_input : in std_logic_vector(coding_bits*2-1 downto 0);
        inst_signature_sum  : out std_logic_vector(integer(floor(log2(real(((2 ** coding_bits)-1)*saved_inst*2)))) downto 0); -- max value is maximum value per register * number of registers;
        inst_signature_conc : out std_logic_vector(coding_bits*saved_inst*2-1 downto 0)
        );
end;

architecture rtl of fifo_instructions is
    
    -- FIFO signals
    type fifo_type is array (natural range <>) of std_logic_vector(coding_bits*2-1 downto 0); 
    signal fifo_mem, fifo_mem_n : fifo_type(saved_inst-1 downto 0);

    signal fifo_counter : unsigned(integer(floor(log2(real(saved_inst)))) downto 0);
begin

    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                fifo_counter <= (others => '0');
                fifo_mem <= ( others =>(others => '0' ));
            else
                fifo_mem <= fifo_mem_n;
                if fifo_counter = saved_inst-1 then
                    fifo_counter <= (others => '0');
                else
                    fifo_counter <= fifo_counter +1;
                end if;
            end if;
        end if;
    end process;

    process(fifo_input, fifo_mem, fifo_counter) 
        variable temp_conc : std_logic_vector(coding_bits*saved_inst*2-1 downto 0);
        variable temp_sum  : unsigned(integer(floor(log2(real(((2 ** coding_bits)-1)*saved_inst*2)))) downto 0);
    begin
        fifo_mem_n <= fifo_mem;
        fifo_mem_n(to_integer(fifo_counter)) <= fifo_input;

        for i in saved_inst downto 1 loop
            temp_conc(i*coding_bits*2-1 downto (i-1)*coding_bits*2) := fifo_mem(i-1);
        end loop;
        inst_signature_conc <= temp_conc;

        temp_sum := (others => '0');
        for j in saved_inst-1 downto 0 loop
            temp_sum := temp_sum + unsigned(fifo_mem(j)(coding_bits*2-1 downto coding_bits)) + unsigned(fifo_mem(j)(coding_bits-1 downto 0));
        end loop;
        inst_signature_sum <= std_logic_vector(temp_sum);

    end process;

end;
