library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library bsc;
use bsc.diversity_types_pkg.all;
use bsc.diversity_components_pkg.all;

entity signature_calculator is
    generic (
        coding_method      : integer := 1;
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
        -- Signatures
        reg_signature_o    : out std_logic_vector(coding_bits*regs_number-1 downto 0);
        reg_instructions_o : out std_logic_vector(integer(floor(log2(real(((2 ** coding_bits)-1)*saved_inst)))) downto 0) -- max value is maximum value per register * number of registers
     );
end;


architecture rtl of signature_calculator is

    -- FUNCTIONS DECLARATION 

    function parity (input_vector: std_logic_vector) return std_logic_vector is
        variable parity_bit : std_logic_vector(0 downto 0) := "0";
    begin
        for i in input_vector'range loop
            parity_bit(0) := parity_bit(0) xor input_vector(i);
        end loop;
        return parity_bit;
    end function;


    -- SIGNALS DECLARATION

    -- Registers signature signals
    signal reg_coding_lane1, reg_coding_lane2 : std_logic_vector(coding_bits-1 downto 0);
    signal reg_signature : std_logic_vector(coding_bits*regs_number-1 downto 0);

    

begin
    method_parity : if (coding_method = 1) generate
        reg_coding_lane1 <= parity(registers_i.value(0));
        reg_coding_lane2 <= parity(registers_i.value(1));
    end generate method_parity;


    mem_regs_sign_inst : mem_regs_sign
        generic map(
            regs_number => 32,
            coding_bits => 1
            )
        port map(
            rstn         => rstn,
            clk          => clk,
            -- Port 1
            we_1     => registers_i.we(0),  
            wdata_1  => reg_coding_lane1,
            addr_1   => registers_i.addr(0),
            -- Port 2
            we_2     => registers_i.we(1), 
            wdata_2  => reg_coding_lane2,
            addr_2   => registers_i.addr(1),
            -- Output
            reg_signature => reg_signature
            );




end;

