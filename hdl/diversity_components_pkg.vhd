library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;
library bsc;
use bsc.diversity_types_pkg.all;

package diversity_components_pkg is

    component diversity_quantifier_top is
        generic (
            coding_method : integer := 1;
            coding_bits   : integer := 1;
            regs_number   : integer := 32;
            saved_inst    : integer := 32
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
    end component diversity_quantifier_top; 


    component signature_calculator is
         generic (
             coding_method : integer := 1;
             coding_bits : integer := 1;
             regs_number : integer := 32;
             saved_inst  : integer := 32
         );
         port (
             rstn           : in  std_ulogic;
             clk            : in  std_ulogic;
             -- Instructions signature
             instructions_i : in instruction_type;
             -- Registers signatures
             registers_i : in register_type;
             reg_signature_o    : out std_logic_vector(coding_bits*regs_number-1 downto 0);
             inst_signature_sum_o   : out std_logic_vector(integer(floor(log2(real(((2 ** coding_bits)-1)*saved_inst*2)))) downto 0); -- max value is maximum value per register * number of saved insts
             inst_signature_conc_o : out std_logic_vector(coding_bits*saved_inst*2-1 downto 0)
          );
    end component signature_calculator;

    component mem_regs_sign is
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
    end component mem_regs_sign;  

    component fifo_instructions is
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
    end component fifo_instructions;



end diversity_components_pkg;
