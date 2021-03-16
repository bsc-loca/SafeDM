library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity inst_diff_calculator is
    port(
        clk            : in  std_logic;
        rstn           : in  std_logic;
        enable_core1_i : in  std_logic;
        enable_core2_i : in  std_logic;
        icnt1_i        : in  std_logic_vector(1 downto 0);                  -- Instruction counter from the first core
        icnt2_i        : in  std_logic_vector(1 downto 0);                  -- Instruction counter from the second core
        inst_diff_o    : out std_logic_vector(15 downto 0)
        );  
end;


architecture rtl of inst_diff_calculator is
    -- Signals to calculate the number of executed instructions
    signal r_executed_inst1, n_executed_inst1 : unsigned(31 downto 0);
    signal r_executed_inst2, n_executed_inst2 : unsigned(31 downto 0);
    signal inst_diff : unsigned(15 downto 0);
begin

    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                r_executed_inst1 <= (others => '0');
                r_executed_inst2 <= (others => '0');
            else
                r_executed_inst1 <= n_executed_inst1;  
                r_executed_inst2 <= n_executed_inst2;  
            end if;
        end if;
    end process;
 
    -- It calculates the number of executed instructions for both cores using the insturction counter
    -- intruction counter icnt has a bit per lane (both bits)
    n_executed_inst1 <= (others => '0') when r_executed_inst2 = X"FFFFFFFF" else
                        r_executed_inst1 - r_executed_inst2 when r_executed_inst1 = X"FFFFFFFF" else
                        r_executed_inst1 + 2 when (icnt1_i(0) and icnt1_i(1)  and enable_core1_i) = '1' else 
                        r_executed_inst1 + 1 when ((icnt1_i(0) or icnt1_i(1)) and enable_core1_i) = '1' else
                        r_executed_inst1;
 
    n_executed_inst2 <= (others => '0') when r_executed_inst1 = X"FFFFFFFF" else
                        r_executed_inst2 - r_executed_inst1 when r_executed_inst2 = X"FFFFFFFF" else
                        r_executed_inst2 + 2 when (icnt2_i(0) and icnt2_i(1)  and enable_core2_i) = '1' else 
                        r_executed_inst2 + 1 when ((icnt2_i(0) or icnt2_i(1)) and enable_core2_i) = '1' else
                        r_executed_inst2;

    -- The instruction different is calculated
    inst_diff <= n_executed_inst1(15 downto 0) - n_executed_inst2(15 downto 0) when n_executed_inst1 > n_executed_inst2  else
                 n_executed_inst2(15 downto 0) - n_executed_inst1(15 downto 0);
    inst_diff_o <= std_logic_vector(inst_diff);


end;

