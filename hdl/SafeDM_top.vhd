-----------------------------------------------------------------------------------------------------------------------------------------------
-- Design top:
--
-- This module uses the instructions in the decode stage of two cores and the read registers from the file register to determine whether
-- there is or not diversity between two cores
-----------------------------------------------------------------------------------------------------------------------------------------------

library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
-- Components and types of the design are defined in the bsc library
library bsc;
use bsc.diversity_types_pkg.all;
use bsc.diversity_components_pkg.all;

entity SafeDM_top is
    generic (
        coding_method     : integer := 0;                   -- It can use none (0) or ECC (1) to encode the instructions and registers writes
        coding_bits_reg   : integer := 64;                  -- Number of bits saved for each register, if register is encoded with ECC the correct number of bits has to be selected
        coding_bits_inst  : integer := 32;                  -- Number of bits saved for each instruction, if instruction is encoded with ECC the correct number of bits has to be selected
        regs_FIFO_pos     : integer := 5;                   -- Number of read registers FIFO positions to calculate the registers signature
        inst_FIFO_pos     : integer := 6                    -- Number of instructions FIFO positions to calculate the instruction signature
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
        hold : in std_logic_vector(1 downto 0);       -- Signal that stalls the pipeline (1 bit per core)
        -----------------------------------------------------
        diversity_lack_o : out std_logic              -- It is set high when there is no diversity
     );
end;


architecture rtl of SafeDM_top is
    -- Number of bits for the signals of the signatures -----------------------------------------------------------------
    constant reg_sig_port_bits : integer := regs_fifo_pos*coding_bits_reg;  -- total number of bits of each fifo of each port of the register file
    constant reg_sig_bits : integer := reg_sig_port_bits*read_ports;        -- total bits of all the fifos of all the ports (=register instruction bits)
    constant inst_sig_bits : integer := coding_bits_inst*inst_fifo_pos*lanes_number; -- total number of bits in all the positions of the fifo where the instructions are stored
    ---------------------------------------------------------------------------------------------------------------------


    -- Singals for the signatures ---------------------------------------------------------------------------------------
    type reg_signature_array is array (natural range <>) of std_logic_vector(reg_sig_bits-1 downto 0);
    signal reg_signature : reg_signature_array(1 downto 0);

    type inst_signature_array is array (natural range <>) of std_logic_vector(inst_sig_bits-1 downto 0);
    signal inst_signature   : inst_signature_array(1 downto 0);
    ---------------------------------------------------------------------------------------------------------------------

    -- Enable signals
    signal enable : std_logic;

    -- Diversity lack count
    signal diversity_lack : std_logic;
    signal diversity_lack_count : unsigned(31 downto 0);

    -- APB bus ----------------------------------------------------------------------------------------------------------
    -- The number or registers can be changed but has to be bigger or equal to 2 for the rest of the design to automatically adapt
    type registers_vector is array (natural range <>) of std_logic_vector(31 downto 0);
    constant registers_number : integer := 2; -- one to activate safedm the other to perform a soft reset through the apb bus 
    signal r, rin      : registers_vector(registers_number-1 downto 0) ;
    signal slave_index : unsigned(13 downto 0);

    -- soft reset through APB bus
    signal soft_rstn, r_soft_rstn : std_logic;
    signal internal_rstn : std_logic;
    ---------------------------------------------------------------------------------------------------------------------

begin

    -----------------------------------------------------------------------------------------------------------------------------------------------------------
    -- SIGNATURE CACLCULATION MODULES -------------------------------------------------------------------------------------------------------------------------
    -----------------------------------------------------------------------------------------------------------------------------------------------------------
    -- If hold is set, the registers have to shift so the instructions are always compared in the rigth order
    -- Two modules to calculate the signatures are instanciated, one for each core, it could be scalated to N cores
    signature_calc_inst : for n in 0 to 1 generate
        signature_calculator_inst : signature_calculator
        generic map(
            coding_method      => coding_method,
            coding_bits_reg    => coding_bits_reg,
            coding_bits_inst   => coding_bits_inst,
            regs_FIFO_pos      => regs_FIFO_pos,
            inst_FIFO_pos      => inst_FIFO_pos, 
            REG_SIG_PORT_BITS  => REG_SIG_PORT_BITS,
            REG_SIG_BITS       => REG_SIG_BITS,
            INST_SIG_BITS      => INST_SIG_BITS
            )
        port map(
            rstn   => internal_rstn, 
            clk    => clk, 
            enable => enable,
            hold_i => hold(n),
            -- Instructions signature
            instructions_i => instructions_i(n),
            -- Registers signatures
            registers_i => registers_i(n),
            -- Outputs
            reg_signature_o   => reg_signature(n),
            inst_signature_o  => inst_signature(n)
            );
    end generate signature_calc_inst; 



    -- Compare both signatures to see if there is diversity
    process(inst_signature, reg_signature)
    begin
        -- LACK OF DIVERSITY OUTPUT
        if inst_signature(0) = inst_signature(1) and reg_signature(0) = reg_signature(1) then
            diversity_lack <= '1';
        else
            diversity_lack <= '0';
        end if;
    end process;
    diversity_lack_o <= diversity_lack and enable;

    -- Count how many cycles there has been lack of diversity
    process(clk)
    begin
        if rising_edge(clk) then    
            if rstn = '0' then
                diversity_lack_count <= (others => '0');
            else 
                if diversity_lack = '1' and enable = '1' then
                    diversity_lack_count <= diversity_lack_count + 1;
                end if;
            end if;
        end if;
    end process;




    -----------------------------------------------------------------------------------------------------------------------------------------------------------
    -- APB BUS HANDLE -----------------------------------------------------------------------------------------------------------------------------------------
    -----------------------------------------------------------------------------------------------------------------------------------------------------------
    -- There are 2 registers: The first one is to perform a soft reset while the other is to enable and desable SafeDM
    -- The last register is for the soft reset
    regs : process(clk)
    begin
        if rising_edge(clk) then 
            r <= rin; 
            r_soft_rstn <= soft_rstn;
        end if;
    end process;

    -- The salve index is computed from the apb address
    slave_index <= unsigned(apbi_paddr_i(15 downto 2));

    comb : process(rstn, apbi_penable_i, apbi_psel_i, apbi_pwrite_i, apbi_pwdata_i, slave_index, r, diversity_lack_count) is 
        variable v : registers_vector(registers_number-1 downto 0);
        variable slave_index_int : integer;
    begin
        slave_index_int := to_integer(slave_index);
        v := r;
        -- Write registers -------------------------------------------------------------- 
        if (apbi_psel_i and apbi_pwrite_i) = '1' and slave_index = 0 then
            -- Soft reset
            v(slave_index_int) := apbi_pwdata_i;
        elsif (apbi_psel_i and apbi_pwrite_i) = '1' and slave_index = 1 then
            -- Enable
            v(slave_index_int) := apbi_pwdata_i;
        end if;
        -- APB read -------------------------------------------------------------------------------
        -- Read register containing the cycles that there has been lack of diversity
        if (apbi_psel_i and apbi_penable_i) = '1' and apbi_pwrite_i = '0' and slave_index = registers_number then
            apbo_prdata_o <= std_logic_vector(diversity_lack_count);
        else
            apbo_prdata_o <= (others => '0');
        end if;
        -------------------------------------------------------------------------------------------

        -- update registers
        if rstn = '0' then
        -- if systems reset set all registers to 0
            rin <= (others => (others => '0'));
        elsif v(0)(0) = '1' then
        -- if rst bit is set, reset and enable bits are set to 0
            rin <= (others => (others => '0'));
        else
            rin <= v;
        end if;
        -- soft reset
        soft_rstn <= not v(0)(0);
    end process;
    -- If soft reset or regular reset is risen, all component resets
    internal_rstn <= r_soft_rstn and rstn;
    -- Enable
    enable <= r(1)(0);
    -----------------------------------------------------------------------------------------------------------------------------------------------------------

end;
