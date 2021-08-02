library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;
library bsc;
use bsc.diversity_types_pkg.all;

package diversity_components_pkg is

    component diversity_quantifier_top is
        generic (
            coding_method         : integer := 1;
            coding_bits_reg       : integer := 1;
            coding_bits_inst_sum  : integer := 1;
            coding_bits_inst_conc : integer := 1;
            regs_number           : integer := 32;
            saved_inst            : integer := 32
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
            -- hold signal
            hold : in std_logic_vector(1 downto 0);
            -- Instruction counters
            icnt1_i : in std_logic_vector(1 downto 0);
            icnt2_i : in std_logic_vector(1 downto 0);
            diversity_lack_o : out std_logic;
            logan_o : out std_logic_vector(189 downto 0);
            -- To stall pipelines to force lack of diversity
            stall_o : out std_logic_vector(1 downto 0)
        );
    end component diversity_quantifier_top; 


    component signature_calculator is
        generic (
            coding_method         : integer := 1;
            coding_bits_reg       : integer := 1;
            coding_bits_inst_sum  : integer := 1;
            coding_bits_inst_conc : integer := 32;
            regs_number           : integer := 32;
            saved_inst            : integer := 32;
            REG_SIG_PORT_BITS     : integer := 4;
            REG_SIG_BITS          : integer := 32;
            INST_SUM_SIG_BITS     : integer := 6;
            INST_CONC_SIG_BITS    : integer := 64
        );
        port (
            rstn   : in  std_ulogic;
            clk    : in  std_ulogic;
            enable : in std_logic;
            -- Hold signal
            hold_i : in std_logic;
            -- Instructions signature
            instructions_i : in instruction_type;
            -- Registers signatures
            registers_i : in register_type; 
            -- Signatures
            reg_signature_o       : out std_logic_vector(REG_SIG_BITS-1 downto 0);
            inst_signature_sum_o  : out std_logic_vector(INST_SUM_SIG_BITS-1 downto 0);
            inst_signature_conc_o : out std_logic_vector(INST_CONC_SIG_BITS-1 downto 0);
            fifo_input_conc_o     : out std_logic_vector(coding_bits_inst_conc*2-1 downto 0)
         );
    end component signature_calculator;

    --component mem_regs_sign is
    --    generic (
    --        regs_number  : integer := 32;
    --        coding_bits  : integer := 1
    --    );
    --    port (
    --        rstn    : in std_ulogic;
    --        clk     : in std_ulogic;
    --        enable  : in std_logic;
    --        -- Port 1 read
    --        ren1  : in std_logic;
    --        data1 : in std_logic_vector(coding_bits-1 downto 0);
    --        -- Port 2 read
    --        ren2  : in std_logic_vector(coding_bits-1 downto 0);
    --        data2 : in std_logic;
    --        -- Port 3 read
    --        ren3  : in std_logic_vector(coding_bits-1 downto 0);
    --        data3 : in std_logic;
    --        -- Port 4 read
    --        ren4  : in std_logic_vector(coding_bits-1 downto 0);
    --        data4 : in std_logic;
    --        -- Port 1
    --        we_1    : in std_logic;
    --        wdata_1 : in std_logic_vector(coding_bits-1 downto 0);
    --        -- Port 2
    --        we_2    : in std_logic;
    --        wdata_2 : in std_logic_vector(coding_bits-1 downto 0);
    --        -- Output
    --        reg_signature : out std_logic_vector(regs_number*coding_bits-1 downto 0)
    --     );
    --end component mem_regs_sign;  

    component fifo_instructions is
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
            rstn       : in std_ulogic;
            clk        : in std_ulogic;
            enable     : in std_logic;
            shift      : in std_logic;
            fifo_input : in std_logic_vector(coding_bits*repetition-1 downto 0);
            signature_sum  : out std_logic_vector(SUM_SIG_BITS-1  downto 0); -- max value is maximum value per register * number of registers;
            signature_conc : out std_logic_vector(CONC_SIG_BITS-1 downto 0)
            );
    end component fifo_instructions;

    component inst_diff_calculator is
        port(
            clk               : in  std_logic;                      
            rstn              : in  std_logic;
            enable_core1_i    : in  std_logic;
            enable_core2_i    : in  std_logic;
            icnt1_i           : in  std_logic_vector(1 downto 0);                  -- Instruction counter from the first core
            icnt2_i           : in  std_logic_vector(1 downto 0);                  -- Instruction counter from the second core
            -- Signal to remove diversity
            remove_diversity_i  : in  std_logic_vector(31 downto 0);               -- Number of executed instructions until remove diversity
            --decode_pc1_i        : in  std_logic_vector(31 downto 0);               -- PC of core1
            --decode_pc2_i        : in  std_logic_vector(31 downto 0);               -- PC of core2
            -- Signal to staggering statistics
            inst_diff_o       : out std_logic_vector(31 downto 0);
            max_stag_core1_o  : out std_logic_vector(31 downto 0);                 -- Biggest staggering during the execution when core1 is ahead
            min_stag_core1_o  : out std_logic_vector(31 downto 0);                 -- Smallest staggering during the execution when core1 is ahead
            max_stag_core2_o  : out std_logic_vector(31 downto 0);                 -- Biggest staggering during the execution when core2 is ahead
            min_stag_core2_o  : out std_logic_vector(31 downto 0);                 -- Smallest staggering during the execution when core2 is ahead
            pass_counter_o    : out std_logic_vector(31 downto 0);                 -- Counts the number of times that the trail core passes the head core
            last_pass_o       : out std_logic_vector(31 downto 0);                 -- The number of executed instructions by core1 when the trail core changed to head core the last time
            -- Output to stall one of the cores to remove diversity
            stall_o           : out std_logic_vector(1 downto 0);
            -- LOGAN signals
            core1_ahead_core2_o : out std_logic;
            ex_inst_core1_o : out std_logic_vector(31 downto 0)
            );  
    end component inst_diff_calculator;

    component histograms_memory is
        generic(
            INST_SIGNATURE_DIFF_BITS : integer := 6;
            REG_SIGNATURE_DIFF_BITS : integer := 6;
            CONC_SIGNATURE_DIFF_BITS : integer := 6;
            MAX_INST_SIGNATURE_DIFF : integer := 32;
            MAX_REG_SIGNATURE_DIFF  : integer := 32;
            MAX_CONC_SIGNATURE_DIFF  : integer := 32
            );
        port (
            rstn   : in  std_ulogic;
            clk    : in  std_ulogic;
            enable : in  std_logic;
            -- Data to be stored
            inst_diff_i           : in std_logic_vector(31 downto 0);
            inst_signature_diff_i : in std_logic_vector(INST_SIGNATURE_DIFF_BITS-1 downto 0);
            reg_signature_diff_i  : in std_logic_vector(REG_SIGNATURE_DIFF_BITS-1 downto 0); 
            conc_signature_diff_i : in std_logic_vector(CONC_SIGNATURE_DIFF_BITS-1 downto 0);   
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
