library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;
library bsc;
use bsc.diversity_types_pkg.all;

package diversity_components_pkg is

    component diversity_quantifier_top is
        generic (
            coding_method    : integer := 1;
            coding_bits_reg  : integer := 1;
            coding_bits_inst : integer := 1;
            regs_number      : integer := 32;
            saved_inst       : integer := 32
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
            registers_i : in register_type_vector;
            -- Instruction counters
            icnt1_i : in std_logic_vector(1 downto 0);
            icnt2_i : in std_logic_vector(1 downto 0)
        );
    end component diversity_quantifier_top; 


    component signature_calculator is
         generic (
             coding_method      : integer := 1;
             coding_bits_reg    : integer := 1;
             coding_bits_inst   : integer := 1;
             regs_number        : integer := 32;
             saved_inst         : integer := 32;
             REG_SIG_BITS       : integer := 32;
             INST_SUM_SIG_BITS  : integer := 6;
             INST_CONC_SIG_BITS : integer := 64
         );
         port (
             rstn   : in  std_ulogic;
             clk    : in  std_ulogic;
             enable : in std_logic;
             -- Instructions signature
             instructions_i : in instruction_type;
             -- Registers signatures
             registers_i : in register_type; 
             -- Signatures
             reg_signature_o       : out std_logic_vector(REG_SIG_BITS-1 downto 0);
             inst_signature_sum_o  : out std_logic_vector(INST_SUM_SIG_BITS-1 downto 0);
             inst_signature_conc_o : out std_logic_vector(INST_CONC_SIG_BITS-1 downto 0)
          );
    end component signature_calculator;

    component mem_regs_sign is
        generic (
            regs_number  : integer := 32;
            coding_bits  : integer := 1
        );
        port (
            rstn    : in std_ulogic;
            clk     : in std_ulogic;
            enable  : in std_logic;
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
            saved_inst  : integer := 32;
            coding_bits : integer := 1;
            INST_SUM_SIG_BITS  : integer := 6;
            INST_CONC_SIG_BITS : integer := 64
        );
        port (
            rstn       : in std_ulogic;
            clk        : in std_ulogic;
            enable     : in std_logic;
            fifo_input : in std_logic_vector(coding_bits*2-1 downto 0);
            inst_signature_sum  : out std_logic_vector(integer(floor(log2(real(((2 ** coding_bits)-1)*saved_inst*2)))) downto 0); -- max value is maximum value per register * number of registers;
            inst_signature_conc : out std_logic_vector(coding_bits*saved_inst*2-1 downto 0)
            );
    end component fifo_instructions;

    component inst_diff_calculator is
        port(
            clk            : in  std_logic;                      
            rstn           : in  std_logic;
            enable_core1_i : in  std_logic;
            enable_core2_i : in  std_logic;
            icnt1_i        : in  std_logic_vector(1 downto 0);                  -- Instruction counter from the first core
            icnt2_i        : in  std_logic_vector(1 downto 0);                  -- Instruction counter from the second core
            inst_diff_o    : out std_logic_vector(15 downto 0)
            );  
    end component inst_diff_calculator;

    component histograms_memory is
        generic(
             INST_SIGNATURE_DIFF_BITS : integer := 6;
             REG_SIGNATURE_DIFF_BITS : integer := 6;
             MAX_INST_SIGNATURE_DIFF : integer := 32;
             MAX_REG_SIGNATURE_DIFF  : integer := 32
             );
        port (
            rstn   : in  std_ulogic;
            clk    : in  std_ulogic;
            enable : in  std_logic;
            -- Data to be stored
            inst_diff_i           : in std_logic_vector(15 downto 0);
            inst_signature_diff_i : in std_logic_vector(INST_SIGNATURE_DIFF_BITS-1 downto 0);
            reg_signature_diff_i  : in std_logic_vector(REG_SIGNATURE_DIFF_BITS-1 downto 0); 
            -- Memory read
            addr_i    : in std_logic_vector(13 downto 0); --TODO: change it to adapt
            -- Memory out
            data_o : out std_logic_vector(31 downto 0)
        );  
    end component histograms_memory;

    component counters_memory is
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
    end component counters_memory;    



end diversity_components_pkg;
