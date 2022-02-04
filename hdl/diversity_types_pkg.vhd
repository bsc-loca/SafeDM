library ieee;
use ieee.std_logic_1164.all;

package diversity_types_pkg is

    constant lanes : integer := 2;
    constant ports_number : integer := 4;

    -- Types for instructions signature ---------------------------------------------
    type opcode_vector  is array (natural range <>) of std_logic_vector(31 downto 0);
    type instruction_type is record
        opcode : opcode_vector(lanes-1 downto 0);    -- not opcode but the whole instruction TODO: change it
        valid  : std_logic_vector(lanes-1 downto 0); -- It is 1 when the instruction is valid
    end record;

    type instruction_type_vector is array (1 downto 0) of instruction_type;
    ---------------------------------------------------------------------------------

    -- Types for registers signature ------------------------------------------------
    type value_port_vector  is array (ports_number-1 downto 0) of std_logic_vector(63 downto 0);
    --type addr_vector  is array (natural range <>) of std_logic_vector(4 downto 0);
    type register_type is record
        value : value_port_vector;                          -- value of the register
        ren   : std_logic_vector(ports_number-1 downto 0);  -- It is 1 when the register is read (1 bit for each read port)
    end record;
    
    type register_type_vector is array (1 downto 0) of register_type;
    ---------------------------------------------------------------------------------

end diversity_types_pkg;
