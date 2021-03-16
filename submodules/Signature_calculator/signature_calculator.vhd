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
        coding_bits        : integer := 1;
        regs_number        : integer := 32;
        saved_inst         : integer := 32;
        REG_SIG_BITS       : integer := 32;
        INST_SUM_SIG_BITS  : integer := 6;
        INST_CONC_SIG_BITS : integer := 64
    );
    port (
        rstn   : in std_ulogic;
        clk    : in std_ulogic;
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
end;


architecture rtl of signature_calculator is

    -- FUNCTIONS DECLARATION 

    function parity (input_vector: std_logic_vector) return std_logic_vector is
        variable parity_bit : std_logic_vector(0 downto 0);
    begin
        parity_bit(0 downto 0) := "0";
        for i in input_vector'range loop
            parity_bit(0) := parity_bit(0) xor input_vector(i);
        end loop;
        return parity_bit;
    end function;


    -- SIGNALS DECLARATION

    -- Registers signature signals
    signal reg_coding_port1, reg_coding_port2 : std_logic_vector(coding_bits-1 downto 0);

    -- Instructions signature signals
    signal inst_coding_lane1, inst_coding_lane2, coding_register_lane1, coding_register_lane2: std_logic_vector(coding_bits-1 downto 0);
    signal fifo_input : std_logic_vector(coding_bits*2-1 downto 0);
    

begin

    -- REGISTERS SIGNATURE ------------------------------------------
    method_parity_regs : if (coding_method = 1) generate
        reg_coding_port1 <= parity(registers_i.value(0));
        reg_coding_port2 <= parity(registers_i.value(1));
    end generate method_parity_regs;


    mem_regs_sign_inst : mem_regs_sign
        generic map(
            regs_number  => regs_number,
            coding_bits  => coding_bits
            )
        port map(
            rstn     => rstn,
            clk      => clk,
            enable   => enable,
            -- Port 1
            we_1     => registers_i.we(0),  
            wdata_1  => reg_coding_port1,
            addr_1   => registers_i.addr(0),
            -- Port 2
            we_2     => registers_i.we(1), 
            wdata_2  => reg_coding_port2,
            addr_2   => registers_i.addr(1),
            -- Output
            reg_signature => reg_signature_o
            );
    ------------------------------------------------------------------


    -- INSTRUCTIONS SIGNATURE ----------------------------------------
    method_parity_insts : if (coding_method = 1) generate
        inst_coding_lane1 <= parity(instructions_i.opcode(0));
        inst_coding_lane2 <= parity(instructions_i.opcode(1));
    end generate method_parity_insts;


    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                coding_register_lane1 <= (others => '0');
                coding_register_lane2 <= (others => '0');
            else
                if instructions_i.valid(0) = '1' then 
                    coding_register_lane1 <= inst_coding_lane1;
                end if;
                if instructions_i.valid(1) = '1' then 
                    coding_register_lane2 <= inst_coding_lane2;
                end if;
            end if;
        end if;
    end process;

    fifo_input <= (coding_register_lane1 & coding_register_lane2) when instructions_i.valid(0) = '0' and  instructions_i.valid(1) = '0' else
                  (inst_coding_lane1 & coding_register_lane2) when instructions_i.valid(0) = '1' and  instructions_i.valid(1) = '0' else
                  (coding_register_lane1 & inst_coding_lane2 ) when instructions_i.valid(0) = '0' and  instructions_i.valid(1) = '1' else
                  (inst_coding_lane2 & inst_coding_lane2);


    fifo_instructions_inst : fifo_instructions
        generic map(
            saved_inst  => saved_inst,
            coding_bits => coding_bits,
            INST_SUM_SIG_BITS  => INST_SUM_SIG_BITS,
            INST_CONC_SIG_BITS => INST_CONC_SIG_BITS
            )
        port map(
            rstn    => rstn,
            clk     => clk,
            enable  => enable,
            fifo_input => fifo_input,
            inst_signature_sum  => inst_signature_sum_o,
            inst_signature_conc => inst_signature_conc_o
        );


end;

