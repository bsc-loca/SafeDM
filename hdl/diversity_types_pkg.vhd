library ieee;
use ieee.std_logic_1164.all;

package diversity_types_pkg is

    -- Change here the number of lanes and read_ports of the design
    constant lanes_number : integer := 2;
    constant read_ports : integer := 4;

    -- Types for instructions signature ---------------------------------------------
    type instruction_vector  is array (natural range <>) of std_logic_vector(31 downto 0);
    type instruction_type is record
        inst_value : instruction_vector(lanes_number-1 downto 0);    
        valid  : std_logic_vector(lanes_number-1 downto 0); -- It is 1 when the instruction is valid
    end record;

    type instruction_type_vector is array (1 downto 0) of instruction_type;
    ---------------------------------------------------------------------------------

    -- Types for registers signature ------------------------------------------------
    type value_port_vector  is array (read_ports-1 downto 0) of std_logic_vector(63 downto 0);
    --type addr_vector  is array (natural range <>) of std_logic_vector(4 downto 0);
    type register_type is record
        value : value_port_vector;                          -- value of the register
        ren   : std_logic_vector(read_ports-1 downto 0);  -- It is 1 when the register is read (1 bit for each read port)
    end record;
    
    type register_type_vector is array (1 downto 0) of register_type;
    ---------------------------------------------------------------------------------

end diversity_types_pkg;
