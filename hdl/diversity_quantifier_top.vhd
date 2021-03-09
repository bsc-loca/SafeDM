library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library bsc;
use bsc.diversity_types_pkg.all;
use bsc.diversity_components_pkg.all;

entity diversity_quantifier_top is
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
end;


architecture rtl of diversity_quantifier_top is
    -- Signals
    constant REG_SIG_BITS : integer := coding_bits*regs_number;
    constant INST_SUM_SIG_BITS : integer := integer(ceil(log2(real(((2 ** coding_bits)-1)*saved_inst*2))));
    constant INST_CONC_SIG_BITS : integer := coding_bits*saved_inst*2;

    type reg_signature_array is array (natural range <>) of std_logic_vector(REG_SIG_BITS-1 downto 0);
    signal reg_signature : reg_signature_array(1 downto 0);

    type inst_signature_sum_array is array (natural range <>) of std_logic_vector(INST_SUM_SIG_BITS-1 downto 0);
    signal inst_signature_sum : inst_signature_sum_array(1 downto 0);

    type inst_signature_conc_array is array (natural range <>) of std_logic_vector(INST_CONC_SIG_BITS-1 downto 0);
    signal inst_signature_conc : inst_signature_conc_array(1 downto 0);

    signal reg_signature_diff       : std_logic_vector(REG_SIG_BITS-1 downto 0);
    signal inst_signature_conc_diff : std_logic_vector(INST_CONC_SIG_BITS-1 downto 0);
    signal inst_signature_sum_diff  : unsigned(INST_SUM_SIG_BITS-1  downto 0);

begin

    signature_calc_inst : for n in 0 to 1 generate
        signature_calculator_inst : signature_calculator
        generic map(
            coding_method => 1,
            coding_bits   => 1,
            regs_number   => 32,
            saved_inst    => 32
            )
        port map(
            rstn => rstn, 
            clk  => clk, 
            -- Instructions signature
            instructions_i => instructions_i(n),
            -- Registers signatures
            registers_i => registers_i(n),
            -- Outputs
            reg_signature_o        => reg_signature(n),
            inst_signature_sum_o   => inst_signature_sum(n),
            inst_signature_conc_o  => inst_signature_conc(n)
            );
    end generate signature_calc_inst; 

    process(reg_signature, inst_signature_sum, inst_signature_conc)
        variable temp_xor       : std_logic;
        variable temp_regs      : unsigned(REG_SIG_BITS-1 downto 0);
        variable temp_inst_conc : unsigned(INST_CONC_SIG_BITS-1 downto 0);
    begin
        -- Number of diferent bits in regs signature
        temp_regs := (others => '0');
        for i in temp_regs'HIGH downto 0 loop
            temp_xor := reg_signature(0)(i) xor reg_signature(1)(i);
            if temp_xor = '1' then
                temp_regs := temp_regs + 1;
            end if;
        end loop;
        reg_signature_diff <= std_logic_vector(temp_regs);

        -- Number of different bits in insts sum signature
        temp_inst_conc := (others => '0');
        for i in temp_inst_conc'HIGH downto 0 loop
            temp_xor := inst_signature_conc(0)(i) xor inst_signature_conc(1)(i);
            if temp_xor = '1' then
                temp_inst_conc := temp_inst_conc + 1;
            end if;
        end loop;
        inst_signature_conc_diff <= std_logic_vector(temp_inst_conc);
 
        -- Numeric difference between insts sum signatures
        if unsigned(inst_signature_sum(0)) > unsigned(inst_signature_sum(1)) then
            inst_signature_sum_diff <= unsigned(inst_signature_sum(0)) - unsigned(inst_signature_sum(1)); 
        else
            inst_signature_sum_diff <= unsigned(inst_signature_sum(1)) - unsigned(inst_signature_sum(0)); 
        end if;

    end process;

end;
