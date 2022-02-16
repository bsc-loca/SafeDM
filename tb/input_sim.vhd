-------------------------------------------------------------------------------------------------------------------
-- This module is part of the testbench and is in charge of generating the inputs for SafeDM
-------------------------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
library bsc;
use bsc.diversity_types_pkg.all;


entity input_sim is
    port(
        clk : in  std_logic;
        -- Sync signals
        sync_inst : in std_logic;                      -- When it is high, instructions of both cores synchornyze
        sync_regs : in std_logic;                      -- When it is high, registers of both cores synchornyze
        -- Instructions signature
        instructions_o : out instruction_type_vector;  -- Signals to calculate the instruction signature
        -- Registers signatures
        registers_o : out register_type_vector;        -- Signals to calculate the registers signature
        -- hold signal
        hold_o : out std_logic_vector(1 downto 0)
    );
end;

architecture rtl of input_sim is

    -- SIGNALS FOR INSTRUCTION GENERATION ------------------------
    -- Random numbers to generate the instructions (32 bits)
    signal instruction1, instruction2 : instruction_vector(lanes_number-1 downto 0);

    -- Random numbers to generate the hold and the valid signals
    signal hold : std_logic_vector(1 downto 0);
    signal valid1, valid2 : std_logic_vector(lanes_number-1 downto 0);
    --------------------------------------------------------------


    -- SIGNALS FOR REGISTERS GENERATION ------------------------
    signal ren1, ren2 : std_logic_vector(read_ports-1 downto 0);  
    signal register1 : value_port_vector;
    signal register2 : value_port_vector;
    --------------------------------------------------------------

begin
    -- This process generates the signals realted with the instructions signature
    process(instruction1, instruction2, valid1, valid2, hold)
    begin
        if sync_inst = '1' then
            -- If sync_inst is activated same inputs are generated for both cores
            for N in 0 to lanes_number-1 loop
                -- CORE 1
                instructions_o(0).inst_value(N) <= instruction1(N); 
                instructions_o(0).valid(N) <= valid1(N);
                -- CORE 2
                instructions_o(1).inst_value(N) <= instruction1(N); 
                instructions_o(1).valid(N) <= valid1(N);
            end loop;
            hold_o(0) <= hold(0);
            hold_o(1) <= hold(0);
        else 
            for N in 0 to lanes_number-1 loop
                -- CORE 1
                instructions_o(0).inst_value(N) <= instruction1(N); 
                instructions_o(0).valid(N) <= valid1(N);
                -- CORE 2
                instructions_o(1).inst_value(N) <= instruction2(N); 
                instructions_o(1).valid(N) <= valid2(N);
            end loop;
            hold_o(0) <= hold(0);
            hold_o(1) <= hold(1);
        end if;
    end process;

    -- This process generates the signals realted with the instructions signature
    process(register1, register2, ren1, ren2)
    begin
        if sync_regs = '1' then
            -- If sync_regs is activated same inputs are generated for both cores
            for N in 0 to read_ports-1 loop
                -- CORE 1
                registers_o(0).value(N) <= register1(N); 
                registers_o(0).ren(N) <= ren1(N); 
                -- CORE 2
                registers_o(1).value(N) <= register1(N); 
                registers_o(1).ren(N) <= ren1(N); 
            end loop;
        else 
            for N in 0 to read_ports-1 loop
                -- CORE 1
                registers_o(0).value(N) <= register1(N); 
                registers_o(0).ren(N) <= ren1(N); 
                -- CORE 2
                registers_o(1).value(N) <= register2(N); 
                registers_o(1).ren(N) <= ren2(N); 
            end loop;
        end if;
    end process;

    -- Generates instruction core0
    INST1_GEN: for I in 0 to lanes_number-1 generate
        process is
            variable seed1 : positive := 4 + I;
            variable seed2 : positive := 7 + I;
            variable x : real;
            variable y : integer;
        begin
            uniform(seed1, seed2, x); 
            y := integer(floor(x * 2147483647.0));
            instruction1(I) <= std_logic_vector(to_unsigned(y, 32));
            wait for 10 ns;
        end process;
    end generate INST1_GEN;

    -- Generates instruction core1
    INST2_GEN: for I in 0 to lanes_number-1 generate
        process is
            variable seed1 : positive := 3 + I;
            variable seed2 : positive := 43 + I;
            variable x : real;
            variable y : integer;
        begin
            uniform(seed1, seed2, x); 
            y := integer(floor(x * 2147483647.0));
            instruction2(I) <= std_logic_vector(to_unsigned(y, 32));
            wait for 10 ns;
        end process;
    end generate INST2_GEN;

    -- Generates hold signals for both cores
    HOLD_GEN: for I in 0 to 1 generate
        process is
            variable seed1 : positive := 9 + I;
            variable seed2 : positive := 5 + I;
            variable x : real;
            variable y : integer;
        begin
            uniform(seed1, seed2, x); 
            y := integer(floor(x * 20.0));
            if y = 10 then
                hold(I) <= '1';
            else
                hold(I) <= '0';
            end if;
            wait for 100 ns;
        end process;
    end generate HOLD_GEN;

    -- Generates valid signals for each lane of the core1
    VALID1_GEN: for I in 0 to lanes_number-1 generate
        process is
            variable seed1 : positive := 19 + I;
            variable seed2 : positive := 15 + I;
            variable x : real;
            variable y : integer;
        begin
            uniform(seed1, seed2, x); 
            y := integer(floor(x * 2.0));
            if y = 1 then
                valid1(I) <= '1';
            else
                valid1(I) <= '0';
            end if;
            wait for 10 ns;
        end process;
    end generate VALID1_GEN;

    -- Generates valid signals for each lane of the core2
    VALID2_GEN: for I in 0 to lanes_number-1 generate
        process is
            variable seed1 : positive := 19 + I;
            variable seed2 : positive := 15 + I;
            variable x : real;
            variable y : integer;
        begin
            uniform(seed1, seed2, x); 
            y := integer(floor(x * 2.0));
            if y = 1 then
                valid2(I) <= '1';
            else
                valid2(I) <= '0';
            end if;
            wait for 10 ns;
        end process;
    end generate VALID2_GEN;


    -- Generates ren core1
    REN1_GEN: for I in 0 to read_ports-1 generate
        process is
            variable seed1 : positive := 23 + I;
            variable seed2 : positive := 40 + I;
            variable x : real;
            variable y : integer;
        begin
            uniform(seed1, seed2, x); 
            y := integer(floor(x * 4.0));
            if y = 3 then
                ren1(I) <= '1';
            else
                ren1(I) <= '0';
            end if;
            wait for 10 ns;
        end process;
    end generate REN1_GEN;


    -- Generates ren core2
    REN2_GEN: for I in 0 to read_ports-1 generate
        process is
            variable seed1 : positive := 15 + I;
            variable seed2 : positive := 16 + I;
            variable x : real;
            variable y : integer;
        begin
            uniform(seed1, seed2, x); 
            y := integer(floor(x * 4.0));
            if y = 3 then
                ren2(I) <= '1';
            else
                ren2(I) <= '0';
            end if;
            wait for 10 ns;
        end process;
    end generate REN2_GEN;

    -- Generates registers core1
    REG1_GEN: for I in 0 to read_ports-1 generate
        process is
            variable seed1 : positive := 4 + I;
            variable seed2 : positive := 44 + I;
            variable x : real;
            variable y : integer;
            variable intermediate_value : std_logic_vector(31 downto 0);
        begin
            uniform(seed1, seed2, x); 
            y := integer(floor(x * 2147483647.0));
            intermediate_value := std_logic_vector(to_unsigned(y, 32));
            register1(I) <= intermediate_value & intermediate_value;
            wait for 10 ns;
        end process;
    end generate REG1_GEN;


    -- Generates registers core2
    REG2_GEN: for I in 0 to read_ports-1 generate
        process is
            variable seed1 : positive := 1 + I;
            variable seed2 : positive := 34 + I;
            variable x : real;
            variable y : integer;
            variable intermediate_value :std_logic_vector(31 downto 0);
        begin
            uniform(seed1, seed2, x); 
            y := integer(floor(x * 2147483647.0));
            intermediate_value := std_logic_vector(to_unsigned(y, 32));
            register2(I) <= intermediate_value & intermediate_value;
            wait for 10 ns;
        end process;
    end generate REG2_GEN;
end;


