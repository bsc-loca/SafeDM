-- -----------------------------------------------
-- Project Name   : De-RISC
-- File           : testbench.vhd
-- Organization   : Barcelona Supercomputing Center
-- Author(s)      : Francisco Bas
-- Email(s)       : francisco.basjalon@bsc.es
-- References     : 
-- -----------------------------------------------
-- Revision History
--  Revision   | Author        | Commit | Description
--  1.0        | Francisco Bas | 000000 | Contribution
-- -----------------------------------------------
--


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

architecture rtl of inst_count_sim is

    -- SIGNALS FOR INSTRUCTION GENERATION ------------------------
    -- Random numbers to generate the instructions (32 bits)
    signal random_number1, random_number2 : unsigned(31 downto 0);

    -- Random numbers to generate the hold and the valid signals
    signal hold0, hold1 : std_logic
    signal valid0, valid1 : std_logic
    --------------------------------------------------------------


    -- SIGNALS FOR REGISTERS GENERATION ------------------------
    signal ren_random_vector1, ren_random_vector2 : unsigned(7 downto 0);
    signal ren0_0, ren0_1, ren0_2, ren0_3 : std_logic;  
    signal ren1_0, ren1_1, ren1_2, ren1_3 : std_logic;  
    signal register0 : value_port_vector;
    signal register1 : value_port_vector;
    --------------------------------------------------------------

begin
    -- This process generates the signals realted with the instructions signature
    process(instruction1, instruction2, valid0, valid1, hold0, hold1)
    begin
        if sync_inst = '1' then
            instructions_o(0).opcode <= std_logic_vector(instruction1); 
            instructions_o(1).opcode <= std_logic_vector(instruction1); 
            instructions_o(0).valid <= valid0;
            instructions_o(1).valid <= valid0;
            hold_o(0) <= hold0;
            hold_o(1) <= hold0;
        else 
            instructions_o(0).opcode <= std_logic_vector(instruction1);  
            instructions_o(1).opcode <= std_logic_vector(instruction2); 
            instructions_o(0).valid <= valid0;
            instructions_o(1).valid <= valid1;
            hold_o(0) <= hold0;
            hold_o(1) <= hold1;
        end if;
    end process;

    -- This process generates the signals realted with the instructions signature
    process()
    begin
        if sync_regs = '1' then
            -- Core0
            registers_o(0).value(0) <= register0(0); 
            registers_o(0).value(1) <= register0(1); 
            registers_o(0).value(2) <= register0(2); 
            registers_o(0).value(3) <= register0(3); 
            registers_o(0).value(0) <= register0(4); 
            registers_o(0).ren(0) <= ren0_0; 
            registers_o(0).ren(1) <= ren0_1; 
            registers_o(0).ren(2) <= ren0_2; 
            registers_o(0).ren(3) <= ren0_3; 
            -- Core1
            registers_o(1).value(0) <= register0(0); 
            registers_o(1).value(1) <= register0(1); 
            registers_o(1).value(2) <= register0(2); 
            registers_o(1).value(3) <= register0(3); 
            registers_o(1).ren(0) <= ren0_0; 
            registers_o(1).ren(1) <= ren0_1; 
            registers_o(1).ren(2) <= ren0_2; 
            registers_o(1).ren(3) <= ren0_3; 
        else 
            -- Core0
            registers_o(0).value(0) <= register0(0); 
            registers_o(0).value(1) <= register0(1); 
            registers_o(0).value(2) <= register0(2); 
            registers_o(0).value(3) <= register0(3); 
            registers_o(0).ren(0) <= ren0_0; 
            registers_o(0).ren(1) <= ren0_1; 
            registers_o(0).ren(2) <= ren0_2; 
            registers_o(0).ren(3) <= ren0_3; 
            -- Core1
            registers_o(1).value(0) <= register1(0); 
            registers_o(1).value(1) <= register1(1); 
            registers_o(1).value(2) <= register1(2); 
            registers_o(1).value(3) <= register1(3); 
            registers_o(1).ren(0) <= ren1_0; 
            registers_o(1).ren(1) <= ren1_1; 
            registers_o(1).ren(2) <= ren1_2; 
            registers_o(1).ren(3) <= ren1_3; 
        end if;
    end process;

    -- Generates instruction core0
    process is
        variable seed1 : positive := 4;
        variable seed2 : positive := 7;
        variable x : real;
        variable y : integer;
    begin
        uniform(seed1, seed2, x); 
        y := integer(floor(x * 4294967295.0));
        instruction1 <= to_unsigned(y, 2);
        wait for 10 ns;
    end process;

    -- Generates instruction core1
    process is
        variable seed1 : positive := 3;
        variable seed2 : positive := 43;
        variable x : real;
        variable y : integer;
    begin
        uniform(seed1, seed2, x); 
        y := integer(floor(x * 4294967295.0));
        instruction2 <= to_unsigned(y, 32);
        wait for 10 ns;
    end process;

    -- Generates hold core0
    process is
        variable seed1 : positive := 9;
        variable seed2 : positive := 5;
        variable x : real;
        variable y : integer;
    begin
        uniform(seed1, seed2, x); 
        y := integer(floor(x * 20.0));
        if y = 10 then
            hold0 <= '1';
        else
            hold0 <= '0';
        end if;
        wait for 100 ns;
    end process;

    -- Generates hold core1
    process is
        variable seed1 : positive := 1;
        variable seed2 : positive := 2;
        variable x : real;
        variable y : integer;
    begin
        uniform(seed1, seed2, x); 
        y := integer(floor(x * 20.0));
        if y = 10 then
            hold1 <= '1';
        else
            hold1 <= '0';
        end if;
        wait for 100 ns;
    end process;

    -- Generates valid core0
    process is
        variable seed1 : positive := 19;
        variable seed2 : positive := 15;
        variable x : real;
        variable y : integer;
    begin
        uniform(seed1, seed2, x); 
        y := integer(floor(x * 4.0));
        if y = 1 then
            valid0 <= '1';
        else
            valid0 <= '0';
        end if;
        wait for 10 ns;
    end process;

    -- Generates valid core1
    process is
        variable seed1 : positive := 11;
        variable seed2 : positive := 12;
        variable x : real;
        variable y : integer;
    begin
        uniform(seed1, seed2, x); 
        y := integer(floor(x * 4.0));
        if y = 1 then
            valid1 <= '1';
        else
            valid1 <= '0';
        end if;
        wait for 10 ns;
    end process;

    -- Generates ren core0
    process is
        variable seed1 : positive := 13;
        variable seed2 : positive := 14;
        variable x : real;
        variable y : integer;
    begin
        uniform(seed1, seed2, x); 
        y := integer(floor(x * 256.0));
        ren_random_vector1 <= to_unsigned(y, 8)
        wait for 10 ns;
    end process;
    ren0_0 <= '1' when ren_random_vector1(7 downto 6) = "00" else
              '0'; 
    ren0_1 <= '1' when ren_random_vector1(5 downto 4) = "00" else
              '0'; 
    ren0_2 <= '1' when ren_random_vector1(3 downto 2) = "00" else
              '0';
    ren0_3 <= '1' when ren_random_vector1(1 downto 0) = "00" else
              '0'; 

    -- Generates ren core1
    process is
        variable seed1 : positive := 15;
        variable seed2 : positive := 16;
        variable x : real;
        variable y : integer;
    begin
        uniform(seed1, seed2, x); 
        y := integer(floor(x * 256.0));
        ren_random_vector2 <= to_unsigned(y, 8)
        wait for 10 ns;
    end process;
    ren1_0 <= '1' when ren_random_vector2(7 downto 6) = "00" else
              '0'; 
    ren1_1 <= '1' when ren_random_vector2(5 downto 4) = "00" else
              '0'; 
    ren1_2 <= '1' when ren_random_vector2(3 downto 2) = "00" else
              '0';
    ren1_3 <= '1' when ren_random_vector2(1 downto 0) = "00" else
              '0';

    -- Generates registers core0
    REG_GEN: for I in 0 to 3 generate
        process is
            variable seed1 : positive := 4 + I;
            variable seed2 : positive := 44 + I;
            variable x : real;
            variable y : integer;
            variable intermediate_value : unsigned(31 downto 0);
        begin
            uniform(seed1, seed2, x); 
            y := integer(floor(x * 4294967295.0));
            intermediate_value <= to_unsigned(y, 32);
            register0(I) <= intermediate_value & intermediate_value;
            wait for 10 ns;
        end process;
    end generate REG_GEN;


    -- Generates registers core0
    REG_GEN: for I in 0 to 3 generate
        process is
            variable seed1 : positive := 1 + I;
            variable seed2 : positive := 34 + I;
            variable x : real;
            variable y : integer;
            variable intermediate_value : unsigned(31 downto 0);
        begin
            uniform(seed1, seed2, x); 
            y := integer(floor(x * 4294967295.0));
            intermediate_value <= to_unsigned(y, 32);
            register1(I) <= intermediate_value & intermediate_value;
            wait for 10 ns;
        end process;
    end generate REG_GEN;
end;


