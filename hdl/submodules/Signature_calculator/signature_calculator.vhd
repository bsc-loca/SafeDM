library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library bsc;
use bsc.diversity_types_pkg.all;
use bsc.diversity_components_pkg.all;

entity signature_calculator is
    generic (
        lanes_number      : integer := 2;       -- Number of lanes
        read_ports        : integer := 4;       -- Number of register file read ports
        coding_method     : integer := 1;       -- Determines which method is used to encode the elements of the signatures
        coding_bits_reg   : integer := 1;       -- Number of bits of each register signature element
        coding_bits_inst  : integer := 32;      -- Number of bits of each coded instruction 
        regs_FIFO_pos     : integer := 5;       -- Number of registers in the signature
        inst_FIFO_pos     : integer := 6;       -- Number of saved instructions
        REG_SIG_PORT_BITS : integer := 64*5;    -- Total number of bits of each FIFO of each port of the register file
        REG_SIG_BITS      : integer := 64*5*4;  -- Total bits of all the FIFOs of all the ports (=register instruction bits)
        INST_SIG_BITS     : integer := 32       -- Total number of bits in all the positions of the FIFO where the instructions are stored
    );
    port (
        rstn   : in std_ulogic;
        clk    : in std_ulogic;
        enable : in std_logic;
        -- Hold signal
        hold_i : in std_logic;                   -- Signal to stop the pipeline
        -- Instructions signature
        instructions_i : in instruction_type;    -- Core decode instructions signals
        -- Registers signatures
        registers_i : in register_type;          -- Core registers write signals
        -- Signatures
        reg_signature_o   : out std_logic_vector(REG_SIG_BITS-1 downto 0);       -- Signature from reading registers from the file register
        inst_signature_o  : out std_logic_vector(INST_SIG_BITS-1 downto 0)       -- Signature from instructions executed (concatenation of all the instructions)
     );
end;


architecture rtl of signature_calculator is
    ---------------------------------------------------------------------------------------------------------------------------------------------
    -- FUNCTIONS DECLARATION --------------------------------------------------------------------------------------------------------------------
    ---------------------------------------------------------------------------------------------------------------------------------------------
    -- Function with the coding methods are added

    ---- Single bit parity method (1)
    --function parity (input_vector: std_logic_vector) return std_logic_vector is
    --    variable parity_bit : std_logic_vector(0 downto 0);
    --begin
    --    parity_bit(0 downto 0) := "0";
    --    for i in input_vector'range loop
    --        parity_bit(0) := parity_bit(0) xor input_vector(i);
    --    end loop;
    --    return parity_bit;
    --end function;


    -- This function calculates the extended hamming code of the input vector
    function hamming_code (input_vector: std_logic_vector) return std_logic_vector is
        -- The code length works only if the input vector length is a power of 2 or it is close
        -- to a power of 2.
        -- Otherwise more complex calculations should be employed
        -- Real relations is 2^P ≥ X + P + 1 being P the parity bits and X the bits 
        -- code_length is the number of parity bits
        variable code_length : integer := integer(log2(real(input_vector'LENGTH)))+1;
        -- compound vector is the vector contining the original vector and the parity bits in their positions
        variable compound_vector : std_logic_vector(input_vector'LENGTH+code_length-1 downto 0);
        -- One bit of the hamming vector is reserved to the parity bit for double error detecion
        -- Hamming vector is the vector with the parity bits
        variable hamming_vector : std_logic_vector(code_length downto 0);
        -- Temporal variables used in the loops
        variable j, k : integer;
        variable position : integer;
        variable bit_result : std_logic;
        variable group_size : integer;
        -- Result of the parity bit of the compound vector
        variable parity_bit : std_logic;
    begin
        -- Vector formation
        -- The compound vector is formed. Parity bits have value 0.
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

        -- The values of the parity bits are calculated
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

        -- Once the parity bits of the compound vector have the correct
        -- value, the last parity bit is calculated.
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
    type reg_port_value_vector is array (read_ports-1 downto 0) of std_logic_vector(coding_bits_reg-1 downto 0); 
    signal r_reg_coding, reg_coding : reg_port_value_vector;
    signal fifo_input_port : reg_port_value_vector;
    type reg_port_signatures_vector is array (read_ports-1 downto 0) of std_logic_vector(REG_SIG_PORT_BITS-1 downto 0); 
    signal reg_signature_port : reg_port_signatures_vector;
    signal r_ren : std_logic_vector(read_ports-1 downto 0);
    -- Instruction signature signals ---------------------------------------
    type inst_lane_vector is array (lanes_number-1 downto 0) of std_logic_vector(coding_bits_inst-1 downto 0); 
    signal inst_coding_lane, coding_register_lane, fifo_input_lane : inst_lane_vector;
    --signal inst_coding_lane1_conc, inst_coding_lane2_conc, coding_register_lane1_conc, coding_register_lane2_conc: std_logic_vector(coding_bits_inst_conc-1 downto 0);
    signal fifo_input_inst : std_logic_vector(coding_bits_inst*2-1 downto 0);
    ---------------------------------------------------------------------------------------------------------------------------------------------


begin

    ---------------------------------------------------------------------------------------------------------------------------------------------
    -- ENCODING  --------------------------------------------------------------------------------------------------------------------------------
    ---------------------------------------------------------------------------------------------------------------------------------------------
    -- No encode is performed for the signatures
    no_coding : if (coding_method = 0) generate
        reg_inputs : for n in 0 to read_ports-1 generate
            reg_coding(n) <= registers_i.value(n);
        end generate reg_inputs;
        inst_inputs : for n in 0 to lanes_number-1 generate
            inst_coding_lane(n) <= instructions_i.inst_value(n);
        end generate inst_inputs;
    end generate no_coding;

    method_hamming : if (coding_method = 1) generate
        reg_inputs : for n in 0 to read_ports-1 generate
            reg_coding(n) <= hamming_code(registers_i.value(n));
        end generate reg_inputs;
        inst_inputs : for n in 0 to lanes_number-1 generate
            inst_coding_lane(n) <= hamming_code(instructions_i.inst_value(n));
        end generate inst_inputs;
    end generate method_hamming;

    --method_hamming : if (coding_method = 2) generate
    ------ We encode every register value in the input of the file register with the hamming code algorithm SEDC-DED 
    ------ (Single Bit Error Detection and Correction-Double Bit Error Detection).
    ----    equal : for n in 0 to read_ports-1 generate
    ----        reg_coding(n) <= registers_i.value(n);
    ----    end generate equal;

    ------ No encode is performed for the concatenation signature
    ------ TODO: change inst_value
    ----    inst_coding_lane1_conc <= instructions_i.inst_value(0);
    ----    inst_coding_lane2_conc <= instructions_i.inst_value(1);
    --end generate method_hamming;
    ---------------------------------------------------------------------------------------------------------------------------------------------



    ---------------------------------------------------------------------------------------------------------------------------------------------
    -- REGISTER SIGNATURE  ----------------------------------------------------------------------------------------------------------------------
    ---------------------------------------------------------------------------------------------------------------------------------------------
    
    -- One signature is generated for each read port
    -- Later, this signatures are added.
    register_signature : for n in 0 to read_ports-1 generate
        process(clk)
        begin
            if rising_edge(clk) then
                if rstn = '0' then
                    r_reg_coding(n) <= (others => '0');
                    r_ren(n) <= '0';
                else
                    if r_ren(n) = '1' then 
                        r_reg_coding(n) <= reg_coding(n);
                    end if;
                    -- ren of each port is delayed 1 cycle
                    r_ren(n) <= registers_i.ren(n);
                end if;
            end if;
        end process;

        fifo_input_port(n) <= r_reg_coding(n) when r_ren(n) = '0' else 
                             reg_coding(n);


        fifo_regs : fifo_instructions
            generic map(
                CONCATENATION   => 1,
                fifo_positions  => regs_FIFO_pos,
                coding_bits     => coding_bits_reg,
                CONC_SIG_BITS   => REG_SIG_PORT_BITS
                )
            port map(
                rstn    => rstn,
                clk     => clk,
                enable  => enable,
                shift   => hold_i,
                fifo_input => fifo_input_port(n),
                signature_conc  => reg_signature_port(n)
            );
    end generate register_signature;
    
    -- It concatenates all the signatures of all of the read ports (It performs the concatenation independently of the number of read ports)
    -- Equivalently for 4 read ports: reg_signature_o <= reg_signature_port(0) & reg_signature_port(1) & reg_signature_port(2) & reg_signature_port(3);
    process(reg_signature_port) is
        variable conc_temp : std_logic_vector(REG_SIG_BITS-1 downto 0);
    begin
        conc_temp(REG_SIG_PORT_BITS-1 downto 0) := reg_signature_port(0);
        for N in 1 to read_ports-1 loop 
            conc_temp(REG_SIG_PORT_BITS*(N+1)-1 downto 0) := conc_temp(REG_SIG_PORT_BITS*N-1 downto 0) & reg_signature_port(N);
        end loop;
        reg_signature_o <= conc_temp;
    end process;
    
    ---------------------------------------------------------------------------------------------------------------------------------------------



    ---------------------------------------------------------------------------------------------------------------------------------------------
    -- INTRUCTION SIGNATURE ---------------------------------------------------------------------------------------------------------------------
    ---------------------------------------------------------------------------------------------------------------------------------------------
    -- If the value of the instruction in decode is not valid, we should use the previous value until it is valid. To do so we should register
    -- each lanes instruction and dependeding on the valid signal of each line use the register value or the acutal one.
    instructions_signature : for n in 0 to lanes_number-1 generate
        process(clk)
        begin
            if rising_edge(clk) then
                if rstn = '0' then
                    coding_register_lane(n) <= (others => '0');
                else
                    if instructions_i.valid(n) = '1' then 
                        coding_register_lane(n) <= inst_coding_lane(n);
                    end if;
                end if;
            end if;
        end process;
        fifo_input_lane(n)  <= coding_register_lane(n) when instructions_i.valid(n) = '0' else
                               inst_coding_lane(n);
    end generate instructions_signature;

    -- It concatenates all the inputs of all the lanes (It performs the concatenation independently of the number of lanes)
    -- Equivalently for 2 lanes: fifo_input_inst <= fifo_input_lane(1) & fifo_input_lane(0); 
    process(reg_signature_port) is
        variable conc_temp : std_logic_vector(coding_bits_inst*lanes_number-1 downto 0);
    begin
        conc_temp(coding_bits_inst-1 downto 0) := fifo_input_lane(0);
        for N in 1 to lanes_number-1 loop 
            conc_temp(coding_bits_inst*(N+1)-1 downto 0) := conc_temp(coding_bits_inst*N-1 downto 0) & fifo_input_lane(N);
        end loop;
        fifo_input_inst <= conc_temp;
    end process;

    --fifo_input_inst <= fifo_input_high_conc & fifo_input_low_conc;

--    process(clk)
--    begin
--        if rising_edge(clk) then
--            if rstn = '0' then
--                coding_register_lane(0) <= (others => '0');
--                coding_register_lane(1) <= (others => '0');
--            else
--                if instructions_i.valid(0) = '1' then 
--                    coding_register_lane(0) <= inst_coding_lane(0);
--                end if;
--                if instructions_i.valid(1) = '1' then 
--                    coding_register_lane(1) <= inst_coding_lane(1);
--                end if;
--            end if;
--        end if;
--    end process;

--    -- TODO: make it parametrizable
--    fifo_input_low_conc  <= coding_register_lane(0) when instructions_i.valid(0) = '0' else
--                           inst_coding_lane(0);
--    fifo_input_high_conc <= coding_register_lane(1) when instructions_i.valid(1) = '0' else
--                           inst_coding_lane(1);
--    fifo_input <= fifo_input_high_conc & fifo_input_low_conc;


    -- This is a FIFO with many positions as 'sabed_inst'. Every position of the fifo has space for two instructions, one of each lane.
    -- Every cycle new pair of instructions is stored and the less recently stored are discarded.
    -- The signature is calculated as the sumation of all the stored instructions.
    fifo_instructions_inst2 : fifo_instructions
        generic map(
            CONCATENATION  => 1,
            repetition     => 2,
            fifo_positions => inst_FIFO_pos,
            coding_bits    => coding_bits_inst,
            CONC_SIG_BITS  => INST_SIG_BITS
            )
        port map(
            rstn       => rstn,
            clk        => clk,
            enable     => enable,
            shift      => hold_i,
            fifo_input => fifo_input_inst,
            --inst_signature_sum  => open,
            signature_conc => inst_signature_o -- It can calculate the signature of concatenated instrucctions but not with the whole instruction but with the codification
        );
    ---------------------------------------------------------------------------------------------------------------------------------------------

end;

