library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity inst_diff_calculator is
    port(
        clk                 : in  std_logic;
        rstn                : in  std_logic;
        enable_core1_i      : in  std_logic;
        enable_core2_i      : in  std_logic;
        icnt1_i             : in  std_logic_vector(1 downto 0);                  -- Instruction counter from the first core (one bit per lane)
        icnt2_i             : in  std_logic_vector(1 downto 0);                  -- Instruction counter from the second core (one bit per lane)
        inst_diff_o         : out std_logic_vector(31 downto 0);                 -- Difference of instructions between both cores (staggering)
        -- Signal to remove diversity
        remove_diversity_i  : in  std_logic_vector(31 downto 0);                 -- Number of executed instructions until remove diversity
        --decode_pc1_i        : in  std_logic_vector(31 downto 0);                 -- PC of core1
        --decode_pc2_i        : in  std_logic_vector(31 downto 0);                 -- PC of core2
        -- Outputs for statistics
        max_stag_core1_o    : out std_logic_vector(31 downto 0);                 -- Biggest staggering during the execution when core1 is ahead
        min_stag_core1_o    : out std_logic_vector(31 downto 0);                 -- Smallest staggering during the execution when core1 is ahead
        max_stag_core2_o    : out std_logic_vector(31 downto 0);                 -- Biggest staggering during the execution when core2 is ahead
        min_stag_core2_o    : out std_logic_vector(31 downto 0);                 -- Smallest staggering during the execution when core2 is ahead
        pass_counter_o      : out std_logic_vector(31 downto 0);                 -- Counts the number of times that the trail core passes the head core
        last_pass_o         : out std_logic_vector(31 downto 0);                 -- The number of executed instructions by core1 when the trail core changed to head core the last time
        -- Output to stall one of the cores to remove diversity
        stall_o             : out std_logic_vector(1 downto 0);
        -- Outputs for LOGAN
        core1_ahead_core2_o : out std_logic;                                     -- Signal that goes into ILA (Integrated Logic Analyzer)
        ex_inst_core1_o     : out std_logic_vector(31 downto 0)                  -- Signal that goes into ILA
        );  
end;


architecture rtl of inst_diff_calculator is
    -- Signals to calculate the number of executed instructions
    signal r_executed_inst1, n_executed_inst1 : unsigned(31 downto 0);
    signal r_executed_inst2, n_executed_inst2 : unsigned(31 downto 0);
    signal core1_ahead_core2 : std_logic;
    signal inst_diff : unsigned(31 downto 0);
    -- Statistics
    signal r_max_stag_core1, r_min_stag_core1, r_max_stag_core2, r_min_stag_core2 : unsigned(31 downto 0); 
    signal r_pass_counter, r_last_pass : unsigned(31 downto 0); 
    signal r_core1_ahead_core2 : std_logic;
    -- FSM to remove diversity (make both cores execute the same instructions at the same time)
    signal stall : std_logic_vector(1 downto 0);
    type state_type is (not_stalled, stalled); -- Define the states 
    signal n_stall_fsm, stall_fsm : state_type;
    signal r_diversity_activated, diversity_activated : std_logic;

begin

    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                r_executed_inst1 <= (others => '0');
                r_executed_inst2 <= (others => '0');
            else
                r_executed_inst1 <= n_executed_inst1;  
                r_executed_inst2 <= n_executed_inst2;  
            end if;
        end if;
    end process;
 
    -- It calculates the number of executed instructions for both cores using the insturction counter
    -- intruction counter icnt has a bit per lane (both bits)
    n_executed_inst1 <= (others => '0') when r_executed_inst2 = X"FFFFFFFF" else
                        r_executed_inst1 - r_executed_inst2 when r_executed_inst1 = X"FFFFFFFF" else
                        r_executed_inst1 + 2 when (icnt1_i(0) and icnt1_i(1)  and enable_core1_i) = '1' else 
                        r_executed_inst1 + 1 when ((icnt1_i(0) or icnt1_i(1)) and enable_core1_i) = '1' else
                        r_executed_inst1;
 
    n_executed_inst2 <= (others => '0') when r_executed_inst1 = X"FFFFFFFF" else
                        r_executed_inst2 - r_executed_inst1 when r_executed_inst2 = X"FFFFFFFF" else
                        r_executed_inst2 + 2 when (icnt2_i(0) and icnt2_i(1)  and enable_core2_i) = '1' else 
                        r_executed_inst2 + 1 when ((icnt2_i(0) or icnt2_i(1)) and enable_core2_i) = '1' else
                        r_executed_inst2;

    core1_ahead_core2 <= '1' when r_executed_inst1 > r_executed_inst2 else
                         '0';

    -- The instruction different is calculated
    inst_diff <= r_executed_inst1 - r_executed_inst2 when core1_ahead_core2 = '1' else
                 r_executed_inst2 - r_executed_inst1;
    inst_diff_o <= std_logic_vector(inst_diff);

    -- It calculates the maximum and minimum staggering during the execution
    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                r_max_stag_core1 <= (others => '0');
                r_min_stag_core1 <= (others => '1');
                r_max_stag_core2 <= (others => '0');
                r_min_stag_core2 <= (others => '1');
            else
                if enable_core1_i = '1' and enable_core2_i = '1' then
                    case core1_ahead_core2 is
                        when '1' =>
                            if inst_diff > r_max_stag_core1 then
                                r_max_stag_core1 <= inst_diff;
                            elsif inst_diff < r_min_stag_core1 then
                                r_min_stag_core1 <= inst_diff;
                            end if;
                        when '0' => 
                            if inst_diff > r_max_stag_core2 then
                                r_max_stag_core2 <= inst_diff;
                            elsif inst_diff < r_min_stag_core2 then
                                r_min_stag_core2 <= inst_diff;
                            end if;
                        when others => -- Not neede but it doesn't work without it
                    end case;
                end if;
            end if;
        end if;
    end process;
    
    max_stag_core1_o <= std_logic_vector(r_max_stag_core1); 
    min_stag_core1_o <= std_logic_vector(r_min_stag_core1);
    max_stag_core2_o <= std_logic_vector(r_max_stag_core2);
    min_stag_core2_o <= std_logic_vector(r_min_stag_core2);    


    -- It calculates the number of times that the core that is ahead change during the execution
    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                r_pass_counter <= (others => '0');
                r_last_pass <= (others => '0');
                r_core1_ahead_core2 <= '0';
            else
                r_core1_ahead_core2 <= core1_ahead_core2;
                if enable_core1_i = '1' and enable_core2_i = '1' then
                    if r_core1_ahead_core2 /= core1_ahead_core2 then
                        r_pass_counter <= r_pass_counter + 1;
                        r_last_pass <= r_executed_inst1; 
                    end if;
                end if;
            end if;
        end if;
    end process;

    pass_counter_o <= std_logic_vector(r_pass_counter);
    last_pass_o    <= std_logic_vector(r_last_pass); 



    -- It stalls one core until both execute the same pc (no diversity)
    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                stall_fsm <= not_stalled;
                stall_o <= "00";
                r_diversity_activated <= '0';
            else
                stall_fsm <= n_stall_fsm;
                stall_o <= stall;
                if diversity_activated = '1' then
                    r_diversity_activated <= '1';
                end if;
            end if;
        end if;
    end process;

    process(stall_fsm, remove_diversity_i, n_executed_inst1, n_executed_inst2, core1_ahead_core2, r_diversity_activated, r_executed_inst1)
    begin
        n_stall_fsm <= stall_fsm;
        stall <= "00";
        diversity_activated <= '0';
        case stall_fsm is
            when not_stalled =>
                --if ((unsigned(remove_diversity_i) = n_executed_inst1 or unsigned(remove_diversity_i) = n_executed_inst1+1) and unsigned(remove_diversity_i) /= 0 and r_executed_inst1 /= r_executed_inst2) then
                if (unsigned(remove_diversity_i) = n_executed_inst1 or unsigned(remove_diversity_i)+1 = n_executed_inst1) and unsigned(remove_diversity_i) /= 0 and r_diversity_activated = '0' then
                    n_stall_fsm <= stalled;
                    diversity_activated <= '1';
                    if core1_ahead_core2 = '1' then
                        stall(0) <= '1';
                    else
                        stall(1) <= '1';
                    end if;
                end if;
            when stalled =>
                if core1_ahead_core2 = '1' then
                    stall(0) <= '1';
                    if (unsigned(remove_diversity_i) = n_executed_inst2 or unsigned(remove_diversity_i)+1 = n_executed_inst2) then
                        stall(0) <= '0';
                        n_stall_fsm <= not_stalled;
                    end if;
                else
                    stall(1) <= '1';
                    if (unsigned(remove_diversity_i) = n_executed_inst1 or unsigned(remove_diversity_i)+1 = n_executed_inst2) then
                        stall(1) <= '0';
                        n_stall_fsm <= not_stalled;
                    end if;
                end if;
            when others =>
        end case;
    end process;


    -- Output to the Logic analyzer (LOGAN)
    ex_inst_core1_o <= std_logic_vector(r_executed_inst1);
    core1_ahead_core2_o <= core1_ahead_core2;


end;

