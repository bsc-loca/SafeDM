library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library bsc;
use bsc.diversity_types_pkg.all;
use bsc.diversity_components_pkg.all;

entity diversity_quantifier_top is
    generic (
        lanes : integer := 2;        
        write_ports : integer := 2 
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
        registers_i : in register_type_vector
     );
end;


architecture rtl of diversity_quantifier_top is
begin

    signature_calculator_inst : signature_calculator
    --generic map()
    port map(
        rstn          => rstn, 
        clk           => clk, 
        -- Instructions signature
        instructions_i => instructions_i(0),
        -- Registers signatures
        registers_i => registers_i(0),
        reg_signature_o    => open,
        reg_instructions_o => open
        );

end;
