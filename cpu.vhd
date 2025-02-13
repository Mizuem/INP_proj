-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2024 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Denys Malytskyi <xmalytd00 AT stud.fit.vutbr.cz>
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (1) / zapis (0)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_INV  : out std_logic;                      -- pozadavek na aktivaci inverzniho zobrazeni (1)
   OUT_WE   : out std_logic;                      -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'

   -- stavove signaly
   READY    : out std_logic;                      -- hodnota 1 znamena, ze byl procesor inicializovan a zacina vykonavat program
   DONE     : out std_logic                       -- hodnota 1 znamena, ze procesor ukoncil vykonavani programu (narazil na instrukci halt)
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is
  signal pc_inc: std_logic;
  signal pc_dec: std_logic;
  signal pc_out: std_logic_vector(12 downto 0);

  signal ptr_inc: std_logic;
  signal ptr_dec: std_logic;
  signal ptr_out: std_logic_vector(12 downto 0);

  signal tmp_load: std_logic;
  signal tmp_reg: std_logic_vector(7 downto 0);
  

  signal cnt_inc: std_logic;
  signal cnt_dec: std_logic;
  signal cnt_load: std_logic;
  signal cnt_out: std_logic_vector(7 downto 0);
  
  signal mux1_select: std_logic;
  signal mux2_select: std_logic_vector(1 downto 0);

  type fsm_state is (
    FSM_START, 
    FSM_INIT,
    FSM_HALT, 
    FSM_FETCH, 
    FSM_DECODE, 

    FSM_PTR_NEXT, 
    FSM_PTR_PREV, 
    
    FSM_INC_DATA_READ, 
    FSM_INC_DATA_WRITE, 
    FSM_INC_DATA_DONE,
    
    FSM_DEC_DATA_READ, 
    FSM_DEC_DATA_WRITE,
    FSM_DEC_DATA_DONE,
    
    FSM_PRINT_READ, 
    FSM_PRINT_WRITE, 

    FSM_INPUT,

    FSM_WHILE_OPEN, 
    FSM_WHILE_OPEN_2, 
    FSM_WHILE_OPEN_3, 
    FSM_WHILE_OPEN_4, 
    FSM_WHILE_OPEN_5,

    FSM_WHILE_END,  
    FSM_WHILE_END_2, 
    FSM_WHILE_END_3, 
    FSM_WHILE_END_4, 
    FSM_WHILE_END_5, 

    FSM_TMP_IN_READ,
    FSM_TMP_IN_WRITE,
    FSM_TMP_IN_DONE,

    FSM_TMP_OUT_WRITE,
    FSM_TMP_OUT_DONE,

    FSM_OTHER

  );

  signal STATE_CURRENT: fsm_state;
  signal STATE_NEXT: fsm_state;
begin

-- ----------------------------------------------------------------------------
--                      PC Register
-- ----------------------------------------------------------------------------
  process(CLK, RESET, pc_inc, pc_dec)
  begin
    if RESET = '1' then
      pc_out <= (others => '0');
    elsif rising_edge(CLK) then
      if pc_dec = '1' then
        pc_out <= pc_out - 1;
      elsif pc_inc = '1' then
        pc_out <= pc_out + 1;
      end if;
    end if;
  end process;
-- ----------------------------------------------------------------------------
--                      PTR Register
-- ----------------------------------------------------------------------------
  process(CLK, RESET, ptr_inc, ptr_dec)
  begin
    if RESET = '1' then
      ptr_out <= (others => '0');
    elsif rising_edge(CLK) then
      if ptr_dec = '1' then
        ptr_out <= ptr_out - 1;
      elsif ptr_inc = '1' then
        ptr_out <= ptr_out + 1;
      end if;
    end if;
  end process;
-- ----------------------------------------------------------------------------
--                      CNT Register
-- ----------------------------------------------------------------------------
  process(CLK, RESET, cnt_inc, cnt_dec)
  begin
    if RESET = '1' then
      cnt_out <= (others => '0');
    elsif rising_edge(CLK) then
      if cnt_dec = '1' then
        cnt_out <= cnt_out - 1;
      elsif cnt_inc = '1' then
        cnt_out <= cnt_out + 1;
      elsif cnt_load = '1' then
        cnt_out <= "00000001";
      end if;
    end if;
  end process;
-- ----------------------------------------------------------------------------
--                      TMP Register
-- ----------------------------------------------------------------------------
process (CLK, RESET, tmp_load)
begin
  if RESET = '1' then
    TMP_reg <= (others => '0'); 
  elsif rising_edge(CLK) then
    if EN = '1' then
    
      if tmp_load = '1' then
        tmp_reg <= DATA_RDATA; 
      end if;
    end if;
  end if;
end process;
-- ----------------------------------------------------------------------------
--                          MUX1 
-- ----------------------------------------------------------------------------
  process(mux1_select, pc_out, ptr_out)
  begin
      if mux1_select = '0' then
          DATA_ADDR <= pc_out; 
      else
          DATA_ADDR <= ptr_out;  
      end if;
  end process;

-- ----------------------------------------------------------------------------
--                          MUX2 
-- ----------------------------------------------------------------------------
  process(IN_DATA, DATA_RDATA, mux2_select)
  begin
      case mux2_select is
          when "00" =>
              DATA_WDATA <= tmp_reg;                     
          when "01" =>
              DATA_WDATA <= DATA_RDATA + 1;             
          when "10" =>
              DATA_WDATA <= DATA_RDATA - 1;              
          when "11" =>
              DATA_WDATA <= IN_DATA;                     
          when others =>
              DATA_WDATA <= (others => '0');             
      end case;
  end process;

-- ----------------------------------------------------------------------------
--                      Present State Update
-- ----------------------------------------------------------------------------
  process(CLK, RESET)
  begin
    if RESET = '1' then
      STATE_CURRENT <= FSM_START;
    elsif rising_edge(CLK) then
      if (EN = '1') then
        STATE_CURRENT <= STATE_NEXT;
      end if;
    end if;
  end process;
-- ----------------------------------------------------------------------------
--                              FSM
-- ----------------------------------------------------------------------------
  process(STATE_CURRENT, DATA_RDATA, IN_VLD, OUT_BUSY)
  begin
    DATA_RDWR <= '1';
    DATA_EN <= '0';
    IN_REQ <= '0';
    OUT_WE <= '0';
    
    READY <= '1';
    DONE <= '0';

    pc_dec <= '0';
    pc_inc <= '0';

    ptr_dec <= '0';
    ptr_inc <= '0';

    cnt_dec <= '0';
    cnt_inc <= '0';
    cnt_load <= '0';
    
    tmp_load <= '0';

    mux1_select <= '0';
    mux2_select <= "11";

  case STATE_CURRENT is
    when FSM_START =>
      mux1_select <= '1';
      DATA_EN <= '1';
      READY <= '0';
      DONE <= '0';
      
      STATE_NEXT <= FSM_INIT;

    when FSM_INIT =>
      case DATA_RDATA is 
        when x"40" => -- @
          STATE_NEXT <= FSM_FETCH;
        when others =>
          mux1_select <= '1';
          READY <= '0';
          DATA_EN <= '1';
          ptr_inc <= '1';
          STATE_NEXT <= FSM_INIT;
      end case;

    when FSM_FETCH =>
      mux1_select <= '0';
      DATA_RDWR <= '1';
      DATA_EN <= '1';

      STATE_NEXT <= FSM_DECODE;

    when FSM_DECODE =>
      case DATA_RDATA is
        when x"2B" =>                  -- '+'
          STATE_NEXT <= FSM_INC_DATA_READ;
        when x"2D" =>                  -- '-' 
          STATE_NEXT <= FSM_DEC_DATA_READ;
        when x"3E" =>             -- '>'
          STATE_NEXT <= FSM_PTR_NEXT;
        when x"3C" =>             -- '<'
          STATE_NEXT <= FSM_PTR_PREV;
        when x"2E" =>                  -- '.'
          STATE_NEXT <= FSM_PRINT_READ;
        when x"2C" =>                  -- ','
          STATE_NEXT <= FSM_INPUT;
        when x"5B" =>                  -- '['
          STATE_NEXT <= FSM_WHILE_OPEN;
        when x"5D" =>                  -- ']'
          STATE_NEXT <= FSM_WHILE_END;
        when x"24" =>                  -- '$'
          STATE_NEXT <= FSM_TMP_IN_READ;
        when x"21" =>                  -- '!'
          STATE_NEXT <= FSM_TMP_OUT_WRITE;
        when x"40" =>             -- @
          STATE_NEXT <= FSM_HALT;
        when others =>
          STATE_NEXT <= FSM_OTHER;
      end case;

    -- '>''
    when FSM_PTR_NEXT =>
        mux1_select <= '1';
        pc_inc <= '1';
        ptr_inc <= '1';
        STATE_NEXT <= FSM_FETCH;

    -- '<'
    when FSM_PTR_PREV =>
        mux1_select <= '1';
        pc_inc <= '1';
        ptr_dec <= '1';
        STATE_NEXT <= FSM_FETCH;
   
    -- '+'
    when FSM_INC_DATA_READ =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        mux1_select <= '1'; 
        STATE_NEXT <= FSM_INC_DATA_WRITE;
    when FSM_INC_DATA_WRITE => 
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        mux1_select <= '1';
        mux2_select <= "01";
        STATE_NEXT <= FSM_INC_DATA_DONE;
    when FSM_INC_DATA_DONE =>
        DATA_RDWR <= '1';
        pc_inc <= '1';
        STATE_NEXT <= FSM_FETCH;

    -- '-'
    when FSM_DEC_DATA_READ =>
        mux1_select <= '1'; 
        DATA_EN <= '1';
        STATE_NEXT <= FSM_DEC_DATA_WRITE;
    when FSM_DEC_DATA_WRITE =>
        mux1_select <= '1'; 
        mux2_select <= "10"; 
        DATA_RDWR <= '0'; 
        DATA_EN <= '1';
        STATE_NEXT <= FSM_DEC_DATA_DONE;
    when FSM_DEC_DATA_DONE =>
        pc_inc <= '1';
        DATA_RDWR <= '1';
        STATE_NEXT <= FSM_FETCH;

    -- '.'
    when FSM_PRINT_READ =>
        mux1_select <= '1';
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        STATE_NEXT <= FSM_PRINT_WRITE;
    when FSM_PRINT_WRITE =>
        if OUT_BUSY = '1' then
            STATE_NEXT <= FSM_PRINT_READ;
        else 
            OUT_DATA <= DATA_RDATA;
            OUT_WE <= '1';
            pc_inc <= '1';
            STATE_NEXT <= FSM_FETCH;
        end if;

    -- ','
    when FSM_INPUT =>
        IN_REQ <= '1';
        if IN_VLD = '1' then
            IN_REQ <= '0';
            mux1_select <= '1';
            mux2_select <= "11";
            DATA_RDWR <= '0';
            DATA_EN <= '1';
            pc_inc <= '1';
            STATE_NEXT <= FSM_FETCH;
        else 
            STATE_NEXT <= FSM_INPUT;
        end if;

    -- '$'
    when FSM_TMP_IN_READ =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        mux1_select <= '1';
        STATE_NEXT <= FSM_TMP_IN_WRITE; 
    when FSM_TMP_IN_WRITE =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        tmp_load <= '1';
        mux1_select <= '1';
        STATE_NEXT <= FSM_TMP_IN_DONE;
    when FSM_TMP_IN_DONE =>
        DATA_RDWR <= '1';
        pc_inc <= '1';
        tmp_load <= '0';
        STATE_NEXT <= FSM_FETCH;
    -- '!'
    when FSM_TMP_OUT_WRITE =>
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        mux1_select <= '1';
        mux2_select <= "00";
        STATE_NEXT <= FSM_TMP_OUT_DONE;
    when FSM_TMP_OUT_DONE =>
        DATA_RDWR <= '1';
        pc_inc <= '1';
        STATE_NEXT <= FSM_FETCH;

    -- '['
    when FSM_WHILE_OPEN =>
        pc_inc <= '1';
        mux1_select <= '1';
        DATA_RDWR <= '1';
        DATA_EN <= '1';
        STATE_NEXT <= FSM_WHILE_OPEN_2;
    when FSM_WHILE_OPEN_2 =>
        if DATA_RDATA = X"00" then
          mux1_select <= '0';
          DATA_EN <= '1';
          STATE_NEXT <= FSM_WHILE_OPEN_3;
        else 
          STATE_NEXT <= FSM_FETCH;
        end if;
    when FSM_WHILE_OPEN_3 =>
        mux1_select <= '0';
        DATA_RDWR <= '1';
        DATA_EN <= '1';
        STATE_NEXT <= FSM_WHILE_OPEN_4;
    when FSM_WHILE_OPEN_4 =>
        if DATA_RDATA = x"5D" then 
          cnt_dec <= '1';
        end if;
        STATE_NEXT <= FSM_WHILE_OPEN_5;
    when FSM_WHILE_OPEN_5 =>
        pc_inc <= '1';
        if cnt_out = "00000000" then
          STATE_NEXT <= FSM_FETCH;
        else 
          STATE_NEXT <= FSM_WHILE_OPEN_3;
        end if;

    -- ']'
    when FSM_WHILE_END =>
        mux1_select <= '1';
        DATA_RDWR <= '1';
        DATA_EN <= '1';
        STATE_NEXT <= FSM_WHILE_END_2;
    when FSM_WHILE_END_2 =>
        if DATA_RDATA = x"00" then
          pc_inc <= '1';
          STATE_NEXT <= FSM_FETCH;
        else 
          pc_dec <= '1';
          STATE_NEXT <= FSM_WHILE_END_3;
        end if;
    when FSM_WHILE_END_3 =>
        cnt_load <= '1';
        mux1_select <= '0';
        DATA_RDWR <= '1';
        DATA_EN <= '1';
        STATE_NEXT <= FSM_WHILE_END_4;
    when FSM_WHILE_END_4 =>
        if DATA_RDATA = x"5B" then 
          cnt_dec <= '1';
        end if;
        STATE_NEXT <= FSM_WHILE_END_5;
    when FSM_WHILE_END_5 =>
        if cnt_out = "00000000" then
          pc_inc <= '1';
          STATE_NEXT <= FSM_FETCH;
        else 
          pc_dec <= '1';
          STATE_NEXT <= FSM_WHILE_END_3;
        end if;

    -- '@'
    when FSM_HALT =>
        DONE <= '1';
        STATE_NEXT <= FSM_HALT;
        
    --helper state
    when FSM_OTHER =>
        pc_inc <= '1';
        STATE_NEXT <= FSM_FETCH;
  end case;
  end process;
end behavioral;

