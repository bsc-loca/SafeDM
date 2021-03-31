library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library bsc;
use bsc.diversity_types_pkg.all;
use bsc.diversity_components_pkg.all;

entity signature_calculator is
    generic (
        coding_method      : integer := 1;    -- This generic determines which method is used to encode the elements of the signatures
        coding_bits_reg    : integer := 1;    -- Number of bits of each register signature element
        coding_bits_inst   : integer := 1;    -- Number of bits of each instruction signature element
        regs_number        : integer := 32;   -- Number of registers in the signature
        saved_inst         : integer := 16;   -- Number of saved instructions
        REG_SIG_BITS       : integer := 32;   -- Number of bits for the register signature
        INST_SUM_SIG_BITS  : integer := 6;    -- Number of bits for the instructions signature (sumation)
        INST_CONC_SIG_BITS : integer := 64    -- Number of bits for the instructions signature (concatenation) 
    );
    port (
        rstn   : in std_ulogic;
        clk    : in std_ulogic;
        enable : in std_logic;
        -- Instructions signature
        instructions_i : in instruction_type;    -- Core decode instructions signals
        -- Registers signatures
        registers_i : in register_type;          -- Core registers write signals
        -- Signatures
        reg_signature_o       : out std_logic_vector(REG_SIG_BITS-1 downto 0);
        inst_signature_sum_o  : out std_logic_vector(INST_SUM_SIG_BITS-1 downto 0);
        inst_signature_conc_o : out std_logic_vector(INST_CONC_SIG_BITS-1 downto 0)
     );
end;


architecture rtl of signature_calculator is
    ---------------------------------------------------------------------------------------------------------------------------------------------
    -- FUNCTIONS DECLARATION --------------------------------------------------------------------------------------------------------------------
    ---------------------------------------------------------------------------------------------------------------------------------------------
    -- Function with the coding methods are added

    -- Single bit parity method (1)
    function parity (input_vector: std_logic_vector) return std_logic_vector is
        variable parity_bit : std_logic_vector(0 downto 0);
    begin
        parity_bit(0 downto 0) := "0";
        for i in input_vector'range loop
            parity_bit(0) := parity_bit(0) xor input_vector(i);
        end loop;
        return parity_bit;
    end function;


    -- Hamming code
    function hamming_code (input_vector: std_logic_vector) return std_logic_vector is
        -- The code length works only if the input vector is a power of 2
        -- Otherwise more complex calculations should be employed
        variable code_length : integer := integer(log2(real(input_vector'LENGTH)))+1;
        variable compound_vector : std_logic_vector(input_vector'LENGTH+code_length-1 downto 0);
        -- One bit of the hamming vector is reserved to the parity bit for double error detecion
        variable hamming_vector : std_logic_vector(code_length downto 0);
        -- For
        variable j, k : integer;
        variable position : integer;
        -- for
        variable bit_result : std_logic;
        variable group_size : integer;
        -- others
        variable parity_bit : std_logic;
    begin
        -- Vector formation
        j := 0;
        k := 0;
        position := 1;
        for i in compound_vector'REVERSE_RANGE loop
            if i = position-1 then
                compound_vector(i) := '0';
                j := j + 1;
                position := 2**j;
            else
                compound_vector(i) := input_vector(k);
                k := k + 1;
            end if;
        end loop;
        -- coding calculation
        for i in 0 to code_length-1 loop
            group_size := 2**i;
            position := group_size-1;
            bit_result := '0';
            while position < compound_vector'LENGTH loop
                for j in 1 to group_size loop
                    if position < compound_vector'LENGTH then
                        bit_result := bit_result xor compound_vector(position);
                        position := position + 1;
                    end if;
                end loop;
                for k in 1 to group_size loop
                    position := position + 1;
                end loop;
            end loop;
            hamming_vector(i) := bit_result;
            compound_vector(group_size-1) := bit_result;
        end loop;

        parity_bit := '0';
        for i in compound_vector'range loop
            parity_bit := parity_bit xor compound_vector(i);
        end loop;
        hamming_vector(code_length) := parity_bit;

        return hamming_vector;
    end function;
    ---------------------------------------------------------------------------------------------------------------------------------------------


    ---------------------------------------------------------------------------------------------------------------------------------------------
    -- SIGNALS DECLARATION ----------------------------------------------------------------------------------------------------------------------
    ---------------------------------------------------------------------------------------------------------------------------------------------
    -- Register signature signals ------------------------------------------
    signal reg_coding_port1, reg_coding_port2 : std_logic_vector(coding_bits_reg-1 downto 0);
    -- Instruction signature signals ---------------------------------------
    signal inst_coding_lane1, inst_coding_lane2, fifo_input_low, fifo_input_high: std_logic_vector(coding_bits_inst-1 downto 0);
    signal r_instruction_lane1, r_instruction_lane2 : std_logic_vector(31 downto 0);
    signal fifo_input : std_logic_vector(coding_bits_inst*2-1 downto 0);
    signal wen : std_logic;
    ---------------------------------------------------------------------------------------------------------------------------------------------


begin

    ---------------------------------------------------------------------------------------------------------------------------------------------
    -- ENCODING  --------------------------------------------------------------------------------------------------------------------------------
    ---------------------------------------------------------------------------------------------------------------------------------------------
    method_parity : if (coding_method = 1) generate
    -- We encode every register value in the input of the file register with the parity method
        reg_coding_port1 <= parity(registers_i.value(0));
        reg_coding_port2 <= parity(registers_i.value(1));

    -- We encode every instruction code in the decode stage with the parity method
        inst_coding_lane1 <= parity(instructions_i.opcode(0));
        inst_coding_lane2 <= parity(instructions_i.opcode(1));
    end generate method_parity;

    method_hamming : if (coding_method = 2) generate
    -- We encode every register value in the input of the file register with the hamming code algorithm SEDC-DED 
    -- (Single Bit Error Detection and Correction-Double Bit Error Detection).
        reg_coding_port1 <= hamming_code(registers_i.value(0));
        reg_coding_port2 <= hamming_code(registers_i.value(1));

    -- We encode every instruction code in the decode stage with the hamming code algorithm SEDC-DED 
    -- (Single Bit Error Detection and Correction-Double Bit Error Detection).
        inst_coding_lane1 <= hamming_code(instructions_i.opcode(0));
        inst_coding_lane2 <= hamming_code(instructions_i.opcode(1));
    end generate method_hamming;
    ---------------------------------------------------------------------------------------------------------------------------------------------



    ---------------------------------------------------------------------------------------------------------------------------------------------
    -- REGISTER SIGNATURE  ----------------------------------------------------------------------------------------------------------------------
    ---------------------------------------------------------------------------------------------------------------------------------------------
    -- Memory with 2 ports (same as file register).
    -- This memory stores the value of each register codification.
    -- From the moment it is activated, every time the core modifies a register in the file register, this value is encoded and stored.
    -- The register value is encoded in the same address as in the register file.
    -- From all the register values, the value of the signature is computed.
    mem_regs_sign_inst : mem_regs_sign
        generic map(
            regs_number  => regs_number,
            coding_bits  => coding_bits_reg
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
    ---------------------------------------------------------------------------------------------------------------------------------------------


    ---------------------------------------------------------------------------------------------------------------------------------------------
    -- INTRUCTION SIGNATURE ---------------------------------------------------------------------------------------------------------------------
    ---------------------------------------------------------------------------------------------------------------------------------------------

    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                r_instruction_lane1 <= (others => '0');
                r_instruction_lane2 <= (others => '0');
            else
                if (enable = '1' and wen = '1') then
                    r_instruction_lane1  <= instructions_i.opcode(0);
                    r_instruction_lane2 <= instructions_i.opcode(1);
                end if;
            end if;
        end if;
    end process;

    fifo_input_low  <= inst_coding_lane1 when (instructions_i.valid(0) = '1' and instructions_i.opcode(0) /= r_instruction_lane1) else
                       (others => '0');
    fifo_input_high <= inst_coding_lane2 when (instructions_i.valid(1) = '1' and instructions_i.opcode(1) /= r_instruction_lane2) else
                       (others => '0');
    fifo_input <= fifo_input_high & fifo_input_low; 

    wen <= '1' when (instructions_i.valid /= "00" and instructions_i.opcode(0) /= r_instruction_lane1 and instructions_i.opcode(1) /= r_instruction_lane2) else
           '0';


    -- This is a FIFO with many positions as 'sabed_inst'. Every position of the fifo has space for two instructions, one of each lane.
    -- Every cycle new pair of instructions is stored and the less recently stored are discarded.
    -- The signature is calculated as the sumation of all the stored instructions.
    fifo_instructions_inst : fifo_instructions
        generic map(
            saved_inst  => saved_inst,
            coding_bits => coding_bits_inst,
            INST_SUM_SIG_BITS  => INST_SUM_SIG_BITS,
            INST_CONC_SIG_BITS => INST_CONC_SIG_BITS
            )
        port map(
            rstn    => rstn,
            clk     => clk,
            enable  => enable,
            wen     => wen,
            fifo_input => fifo_input,
            inst_signature_sum  => inst_signature_sum_o,
            inst_signature_conc => inst_signature_conc_o
        );
    ---------------------------------------------------------------------------------------------------------------------------------------------

end;

