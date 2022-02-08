library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library bsc;
use bsc.diversity_types_pkg.all;
use bsc.diversity_components_pkg.all;

entity diversity_quantifier_top is
    generic (
        coding_method         : integer := 2;   -- It can use parity, ECC or none to encode the instructions and registers writes
        coding_bits_reg       : integer := 64;  -- Number of bits saved for each register read 
        coding_bits_inst_conc : integer := 32;  -- Number of bits saved for each instruction (concatenation signature)
        regs_number           : integer := 5;   -- Number of saved (last) register read to calculate the registers signature
        saved_inst            : integer := 6    -- Number of saved (last) instructions to calculate the instruction signature
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
        -----------------------------------------------------
        -- Singals to calculate sigantures ------------------
        -- Instructions signature
        instructions_i : in instruction_type_vector;  -- Signals to calculate the instruction signature
        -- Registers signatures
        registers_i : in register_type_vector;        -- Signals to calculate the registers signature
        -- hold signals
        hold : in std_logic_vector(1 downto 0);       -- Signal that stalls the pipeline
        -----------------------------------------------------
        apbo_prdata_o  : out std_logic_vector(31 downto 0);     
        diversity_lack_o : out std_logic             -- It is set high when there is no diversity
     );
end;


architecture rtl of diversity_quantifier_top is
    -- Number of bits for the signals of the signatures -----------------------------------------------------------------
    constant REG_SIG_PORT_BITS : integer := regs_number*coding_bits_reg;
    constant REG_SIG_BITS : integer := REG_SIG_PORT_BITS*4;
    constant INST_CONC_SIG_BITS : integer := coding_bits_inst_conc*saved_inst*2;
    --constant INST_SIG_REGISTERS : integer := saved_inst*2;
    ---------------------------------------------------------------------------------------------------------------------


    -- Singals for the signatures ---------------------------------------------------------------------------------------
    type reg_signature_array is array (natural range <>) of std_logic_vector(REG_SIG_BITS-1 downto 0);
    signal reg_signature : reg_signature_array(1 downto 0);

    type inst_signature_conc_array is array (natural range <>) of std_logic_vector(INST_CONC_SIG_BITS-1 downto 0);
    signal inst_signature_conc   : inst_signature_conc_array(1 downto 0);
    ---------------------------------------------------------------------------------------------------------------------

    -- Enable signals
    signal enable : std_logic;

    -- Diversity lack count
    signal diversity_lack : std_logic;
    signal diversity_lack_count : unsigned(31 downto 0);

    -- APB bus ----------------------------------------------------------------------------------------------------------
    -- The number or registers can be changed but has to be bigger than 2 for the rest of the design to automatically adapt
    type registers_vector is array (natural range <>) of std_logic_vector(31 downto 0);
    constant REGISTERS_NUMBER : integer := 2; 
    signal r, rin      : registers_vector(REGISTERS_NUMBER-1 downto 0) ;
    signal slave_index : unsigned(13 downto 0);

    -- soft reset through APB bus
    signal soft_rstn, r_soft_rstn : std_logic;
    signal internal_rstn : std_logic;
    ---------------------------------------------------------------------------------------------------------------------

begin

    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- SIGNATURE CACLCULATION MODULES --------------------------------------------------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- If hold is set, the registers have to shift so the instructions are always compared in the rigth order
    -- Two modules to calculate the signatures are instanciated, one for each core
    signature_calc_inst : for n in 0 to 1 generate
        signature_calculator_inst : signature_calculator
        generic map(
            coding_method         => coding_method,
            coding_bits_reg       => coding_bits_reg,
            coding_bits_inst_conc => coding_bits_inst_conc,
            regs_number           => regs_number,
            saved_inst            => saved_inst, 
            REG_SIG_PORT_BITS     => REG_SIG_PORT_BITS,
            REG_SIG_BITS          => REG_SIG_BITS,
            INST_CONC_SIG_BITS    => INST_CONC_SIG_BITS
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
            reg_signature_o        => reg_signature(n),
            inst_signature_conc_o  => inst_signature_conc(n)
            );
    end generate signature_calc_inst; 



    -- Compare both signatures to see if there is diversity
    process(inst_signature_conc, reg_signature)
    begin
        -- LACK OF DIVERSITY OUTPUT
        if inst_signature_conc(0) = inst_signature_conc(1) and reg_signature(0) = reg_signature(1) then
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




    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- APB BUS HANDLE ------------------------------------------------------------------------------------------------------------------------------------------------------------
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- There are 3 registers, the two first are to enable each core
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
        variable v : registers_vector(REGISTERS_NUMBER-1 downto 0);
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
        if (apbi_psel_i and apbi_penable_i) = '1' and apbi_pwrite_i = '0' and slave_index = REGISTERS_NUMBER then
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
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

end;
