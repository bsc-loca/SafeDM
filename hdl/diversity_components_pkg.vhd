library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;
library bsc;
use bsc.diversity_types_pkg.all;

package diversity_components_pkg is

    component diversity_quantifier_top is
    generic (
        coding_method    : integer := 2;   -- It can use none (0), parity (1) or ECC to encode the instructions and registers writes
        read_ports       : integer := 4;   -- Number of ports in the file register
        lanes_number     : integer := 2;   -- Number of lanes of the cores
        coding_bits_reg  : integer := 64;  -- Number of bits saved for each register, if register is encoded can be less bits than register bits  
        coding_bits_inst : integer := 32;  -- Number of bits saved for each instruction, if instruction is encoded can be less bits than instruction bits   
        regs_FIFO_pos    : integer := 5;   -- Number of read registers FIFO positions to calculate the registers signature
        inst_FIFO_pos    : integer := 6    -- Number of instructions FIFO positions to calculate the instruction signature
        );
    port (
        rstn           : in  std_ulogic;
        clk            : in  std_ulogic;
        -- APB signals --------------------------------------
        apbi_psel_i    : in  std_logic;                       
        apbi_paddr_i   : in  std_logic_vector(31 downto 0);                      
        apbi_penable_i : in  std_logic;                     
        apbi_pwrite_i  : in  std_logic;
        apbi_pwdata_i  : in  std_logic_vector(31 downto 0);                   
        apbo_prdata_o  : out std_logic_vector(31 downto 0);     
        -----------------------------------------------------
        -- Singals to calculate sigantures ------------------
        -- Instructions signature
        instructions_i : in instruction_type_vector;  -- Signals to calculate the instruction signature
        -- Registers signatures
        registers_i : in register_type_vector;        -- Signals to calculate the registers signature
        -- hold signals
        hold : in std_logic_vector(1 downto 0);       -- Signal that stalls the pipeline
        -----------------------------------------------------
        diversity_lack_o : out std_logic              -- It is set high when there is no diversity
     );
    end component diversity_quantifier_top; 


    component signature_calculator is
        generic (
            lanes_number      : integer := 2;  
            read_ports        : integer := 4;  
            coding_method     : integer := 1;
            coding_bits_reg   : integer := 1;
            coding_bits_inst  : integer := 32;
            regs_FIFO_pos     : integer := 32;
            inst_FIFO_pos     : integer := 32;
            REG_SIG_PORT_BITS : integer := 4;
            REG_SIG_BITS      : integer := 32;
            INST_SIG_BITS     : integer := 64
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
            inst_signature_o : out std_logic_vector(INST_SIG_BITS-1 downto 0) 
         );
    end component signature_calculator;

    --component mem_regs_sign is
    --    generic (
    --        regs_FIFO_pos  : integer := 32;
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
            clk                 : in  std_logic;
            rstn                : in  std_logic;
            enable_core1_i      : in  std_logic;
            enable_core2_i      : in  std_logic;
            icnt1_i             : in  std_logic_vector(1 downto 0);                  -- Instruction counter from the first core (one bit per lane)
            icnt2_i             : in  std_logic_vector(1 downto 0);                  -- Instruction counter from the second core (one bit per lane)
            inst_diff_o         : out std_logic_vector(31 downto 0);                 -- Difference of instructions between both cores (staggering)
            -- Signal to remove diversity
            remove_diversity_i  : in  std_logic_vector(31 downto 0);                 -- Number of executed instructions until remove diversity
            -- Inputs for statistics
            wbuffer_full_i      : in  std_logic_vector(1 downto 0);                  -- high when pipeline hold due to write buffer full
            dcmiss_pend_i       : in  std_logic_vector(1 downto 0);                  -- high when pipeline hold due to a data cache miss
            icmiss_pend_i       : in  std_logic_vector(1 downto 0);                  -- high when pipeline hold due to a instruction cache miss
            hold_i              : in  std_logic_vector(1 downto 0);                  -- high when pipeline hold 
            -- Outputs for statistics
            max_stag_core1_o     : out std_logic_vector(31 downto 0);                 -- Biggest staggering during the execution when core1 is ahead
            min_stag_core1_o     : out std_logic_vector(31 downto 0);                 -- Smallest staggering during the execution when core1 is ahead
            max_stag_core2_o     : out std_logic_vector(31 downto 0);                 -- Biggest staggering during the execution when core2 is ahead
            min_stag_core2_o     : out std_logic_vector(31 downto 0);                 -- Smallest staggering during the execution when core2 is ahead
            pass_counter_o       : out std_logic_vector(31 downto 0);                 -- Counts the number of times that the trail core passes the head core
            last_pass_o          : out std_logic_vector(31 downto 0);                 -- The number of executed instructions by core1 when the trail core changed to head core the last time
            ex_inst_core1_o      : out std_logic_vector(31 downto 0);                 -- Number of instructions executed by core1
            ex_inst_core2_o      : out std_logic_vector(31 downto 0);                 -- Number of instructions executed by core2
            cycles_wbuff_full1_o : out std_logic_vector(31 downto 0);                 -- Number of cycles that the write buffer is full
            cycles_wbuff_full2_o : out std_logic_vector(31 downto 0);                 -- Number of cycles that the write buffer is full
            cycles_dcmiss1_o     : out std_logic_vector(31 downto 0);                 -- Number of cycles that the pipeline is hold due to a data cache miss
            cycles_dcmiss2_o     : out std_logic_vector(31 downto 0);                 -- Number of cycles that the pipeline is hold due to a data cache miss
            cycles_icmiss1_o     : out std_logic_vector(31 downto 0);                 -- Number of cycles that the pipeline is hold due to a instruction cache miss
            cycles_icmiss2_o     : out std_logic_vector(31 downto 0);                 -- Number of cycles that the pipeline is hold due to a instruction cache miss
            cycles_hold1_o       : out std_logic_vector(31 downto 0);                 -- Number of cycles that the pipeline is hold
            cycles_hold2_o       : out std_logic_vector(31 downto 0);                 -- Number of cycles that the pipeline is hold
            wbfull_count1_o      : out std_logic_vector(31 downto 0);                 -- Times the write buffer gets full
            wbfull_count2_o      : out std_logic_vector(31 downto 0);                 -- Times the write buffer gets full
            dcmiss_count1_o     : out std_logic_vector(31 downto 0);                 -- Number of data cache misses
            dcmiss_count2_o     : out std_logic_vector(31 downto 0);                 -- Number of data cache misses
            icmiss_count1_o     : out std_logic_vector(31 downto 0);                 -- Number of instruction cache misses
            icmiss_count2_o     : out std_logic_vector(31 downto 0);                 -- Number of instruction cache misses
            -- Output to stall one of the cores to remove diversity
            stall_o             : out std_logic_vector(1 downto 0);
            -- Outputs for LOGAN
            core1_ahead_core2_o : out std_logic     
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
