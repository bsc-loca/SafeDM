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
        -- Inputs for statistics
        wbuffer_full_i      : in  std_logic_vector(1 downto 0);                  -- high when pipeline hold due to write buffer full
        dcmiss_pend_i       : in  std_logic_vector(1 downto 0);                  -- high when pipeline hold due to a data cache miss
        icmiss_pend_i       : in  std_logic_vector(1 downto 0);                  -- high when pipeline hold due to a instruction cache miss
        hold_i              : in  std_logic_vector(1 downto 0);                  -- high when pipeline hold 
        -- Outputs for statistics
        max_stag_core1_o     : out std_logic_vector(31 downto 0);                 -- Biggest staggering during the execution when core1 is ahead
        min_stag_core1_o     : out std_logic_vector(31 downto 0);                 -- Smallest staggering during the execution when core1 is ahead
        max_stag_core2_o     : out std_logic_vector(31 downto 0);                 -- Biggest staggering during the execution when core2 is ahead
        min_stag_core2_o     : out std_logic_vector(31 downto 0);                 -- Smallest staggering during the execution when core2 is ahead
        pass_counter_o       : out std_logic_vector(31 downto 0);                 -- Counts the number of times that the trail core passes the head core
        last_pass_o          : out std_logic_vector(31 downto 0);                 -- The number of executed instructions by core1 when the trail core changed to head core the last time
        ex_inst_core1_o      : out std_logic_vector(31 downto 0);                 -- Number of instructions executed by core1
        ex_inst_core2_o      : out std_logic_vector(31 downto 0);                 -- Number of instructions executed by core2
        cycles_wbuff_full1_o : out std_logic_vector(31 downto 0);                 -- Number of cycles that the write buffer is full
        cycles_wbuff_full2_o : out std_logic_vector(31 downto 0);                 -- Number of cycles that the write buffer is full
        cycles_dcmiss1_o     : out std_logic_vector(31 downto 0);                 -- Number of cycles that the pipeline is hold due to a data cache miss
        cycles_dcmiss2_o     : out std_logic_vector(31 downto 0);                 -- Number of cycles that the pipeline is hold due to a data cache miss
        cycles_icmiss1_o     : out std_logic_vector(31 downto 0);                 -- Number of cycles that the pipeline is hold due to a instruction cache miss
        cycles_icmiss2_o     : out std_logic_vector(31 downto 0);                 -- Number of cycles that the pipeline is hold due to a instruction cache miss
        cycles_hold1_o       : out std_logic_vector(31 downto 0);                 -- Number of cycles that the pipeline is hold
        cycles_hold2_o       : out std_logic_vector(31 downto 0);                 -- Number of cycles that the pipeline is hold
        wbfull_count1_o      : out std_logic_vector(31 downto 0);                 -- Times the write buffer gets full
        wbfull_count2_o      : out std_logic_vector(31 downto 0);                 -- Times the write buffer gets full
        dcmiss_count1_o     : out std_logic_vector(31 downto 0);                 -- Number of data cache misses
        dcmiss_count2_o     : out std_logic_vector(31 downto 0);                 -- Number of data cache misses
        icmiss_count1_o     : out std_logic_vector(31 downto 0);                 -- Number of instruction cache misses
        icmiss_count2_o     : out std_logic_vector(31 downto 0);                 -- Number of instruction cache misses
        -- Output to stall one of the cores to remove diversity
        stall_o             : out std_logic_vector(1 downto 0);
        -- Outputs for LOGAN
        core1_ahead_core2_o : out std_logic                                     -- Signal that goes into ILA (Integrated Logic Analyzer)
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
    signal r_cycles_wbuff_full1, r_cycles_dcmiss1, r_cycles_icmiss1, r_cycles_hold1 : unsigned(31 downto 0);    
    signal r_cycles_wbuff_full2, r_cycles_dcmiss2, r_cycles_icmiss2, r_cycles_hold2 : unsigned(31 downto 0);    
    signal r_wbuffer_full_i, r_dcmiss_pend_i, r_icmiss_pend_i, r_hold_i : std_logic_vector(1 downto 0); --Registers to calculate flanks       
    signal r_wbfull_count1, r_wbfull_count2, r_dcmiss_count1, r_dcmiss_count2, r_icmiss_count1, r_icmiss_count2 : unsigned(31 downto 0);

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

    process(stall_fsm, remove_diversity_i, n_executed_inst1, n_executed_inst2, core1_ahead_core2, r_diversity_activated, r_executed_inst1, r_executed_inst2)
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



    -- Pipeline hold statistics
    process(clk)
    begin
        if rising_edge(clk) then
            if rstn = '0' then
                r_wbuffer_full_i <= "00";   
                r_dcmiss_pend_i  <= "00"; 
                r_icmiss_pend_i  <= "00";
                -- Cycle count
                r_cycles_wbuff_full1 <= (others => '0');
                r_cycles_wbuff_full2 <= (others => '0');
                r_cycles_dcmiss1     <= (others => '0'); 
                r_cycles_dcmiss2     <= (others => '0'); 
                r_cycles_icmiss1     <= (others => '0');
                r_cycles_icmiss2     <= (others => '0');
                r_cycles_hold1       <= (others => '0');
                r_cycles_hold2       <= (others => '0');
                -- Occurrencies count
                r_wbfull_count1 <= (others => '0');
                r_wbfull_count2 <= (others => '0');
                r_dcmiss_count1 <= (others => '0');
                r_dcmiss_count2 <= (others => '0');
                r_icmiss_count1 <= (others => '0');
                r_icmiss_count2 <= (others => '0');
            else
                r_wbuffer_full_i <= wbuffer_full_i;
                r_dcmiss_pend_i  <= dcmiss_pend_i; 
                r_icmiss_pend_i  <= icmiss_pend_i; 
                if enable_core1_i = '1' then     
                    -- Cycle count
                    if wbuffer_full_i(0) = '1' then
                        r_cycles_wbuff_full1 <= r_cycles_wbuff_full1 + 1;
                    end if;
                    if dcmiss_pend_i(0)  = '1' then
                        r_cycles_dcmiss1 <= r_cycles_dcmiss1 + 1;
                    end if;
                    if icmiss_pend_i(0)  = '1' then
                        r_cycles_icmiss1 <= r_cycles_icmiss1 + 1;
                    end if;
                    if hold_i(0)           = '1' then
                        r_cycles_hold1 <= r_cycles_hold1 + 1;
                    end if;
                    -- Occurrencies count
                    if wbuffer_full_i(0) = '1' and r_wbuffer_full_i(0) = '0' then
                        r_wbfull_count1 <= r_wbfull_count1 + 1;
                    end if;
                    if dcmiss_pend_i(0) = '1' and r_dcmiss_pend_i(0) = '0' then
                        r_dcmiss_count1 <= r_dcmiss_count1 + 1;
                    end if;
                    if icmiss_pend_i(0) = '1' and r_icmiss_pend_i(0) = '0' then
                        r_icmiss_count1 <= r_icmiss_count1 + 1;
                    end if;
                end if;
                if enable_core2_i = '1' then     
                    -- Cycle count
                    if wbuffer_full_i(1) = '1' then
                        r_cycles_wbuff_full2 <= r_cycles_wbuff_full2 + 1;
                    end if;
                    if dcmiss_pend_i(1)  = '1' then
                        r_cycles_dcmiss2 <= r_cycles_dcmiss2 + 1;
                    end if;
                    if icmiss_pend_i(1)  = '1' then
                        r_cycles_icmiss2 <= r_cycles_icmiss2 + 1;
                    end if;
                    if hold_i(1)           = '1' then
                        r_cycles_hold2 <= r_cycles_hold2 + 1;
                    end if;
                    -- Occurrencies count
                    if wbuffer_full_i(1) = '1' and r_wbuffer_full_i(1) = '0' then
                        r_wbfull_count2 <= r_wbfull_count2 + 1;
                    end if;
                    if dcmiss_pend_i(1) = '1' and r_dcmiss_pend_i(1) = '0' then
                        r_dcmiss_count2 <= r_dcmiss_count2 + 1;
                    end if;
                    if icmiss_pend_i(1) = '1' and r_icmiss_pend_i(1) = '0' then
                        r_icmiss_count2 <= r_icmiss_count2 + 1;
                    end if;
                end if;
            end if;
        end if;
    end process;

    -- Cycle count
    cycles_wbuff_full1_o <= std_logic_vector(r_cycles_wbuff_full1);
    cycles_wbuff_full2_o <= std_logic_vector(r_cycles_wbuff_full2);
    cycles_dcmiss1_o     <= std_logic_vector(r_cycles_dcmiss1); 
    cycles_dcmiss2_o     <= std_logic_vector(r_cycles_dcmiss2); 
    cycles_icmiss1_o     <= std_logic_vector(r_cycles_icmiss1);
    cycles_icmiss2_o     <= std_logic_vector(r_cycles_icmiss2);
    cycles_hold1_o       <= std_logic_vector(r_cycles_hold1);
    cycles_hold2_o       <= std_logic_vector(r_cycles_hold2);
    -- Occurrencies count
    wbfull_count1_o  <= std_logic_vector(r_wbfull_count1); 
    wbfull_count2_o  <= std_logic_vector(r_wbfull_count2); 
    dcmiss_count1_o <= std_logic_vector(r_dcmiss_count1);
    dcmiss_count2_o <= std_logic_vector(r_dcmiss_count2);
    icmiss_count1_o <= std_logic_vector(r_icmiss_count1);
    icmiss_count2_o <= std_logic_vector(r_icmiss_count2);

    -- Output to the Logic analyzer (LOGAN)
    ex_inst_core1_o <= std_logic_vector(r_executed_inst1);
    ex_inst_core2_o <= std_logic_vector(r_executed_inst2);
    core1_ahead_core2_o <= core1_ahead_core2;


end;

