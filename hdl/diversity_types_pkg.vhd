library ieee;
use ieee.std_logic_1164.all;

package diversity_types_pkg is

    constant lanes : integer := 2;
    constant ports_number : integer := 2;

    -- Types for instructions signature ---------------------------------------------
    type opcode_vector  is array (natural range <>) of std_logic_vector(31 downto 0);
    type instruction_type is record
        opcode : opcode_vector(lanes-1 downto 0);
        valid  : std_logic_vector(lanes-1 downto 0);
    end record;

    type instruction_type_vector is array (1 downto 0) of instruction_type;
    ---------------------------------------------------------------------------------

    -- Types for registers signature ------------------------------------------------
    type value_vector  is array (natural range <>) of std_logic_vector(63 downto 0);
    type addr_vector  is array (natural range <>) of std_logic_vector(4 downto 0);
    type register_type is record
        value : value_vector(ports_number-1 downto 0); 
        addr  : addr_vector(ports_number-1 downto 0);
        we    : std_logic_vector(ports_number-1 downto 0);
    end record;
    
    type register_type_vector is array (1 downto 0) of register_type;
    ---------------------------------------------------------------------------------

end diversity_types_pkg;
