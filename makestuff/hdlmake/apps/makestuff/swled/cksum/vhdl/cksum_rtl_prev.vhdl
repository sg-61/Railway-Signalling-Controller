--
-- Copyright (C) 2009-2012 Chris McClelland
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture rtl of swled is
	-- Flags for display on the 7-seg decimal points
	signal flags                   : std_logic_vector(3 downto 0);

	-- Registers implementing the channels
	signal checksum, checksum_next : std_logic_vector(15 downto 0);
	signal reg0, reg0_next         : std_logic_vector(7 downto 0);
    signal input_64                : std_logic_vector(39 downto 0);
    signal input_data              : std_logic_vector(63 downto 0);
    signal input_data_dec          : std_logic_vector(63 downto 0);
    signal read_data               : std_logic_vector(7 downto 0);
    signal read_byte               : std_logic;
    signal cnt                     : integer range -5 to 800000000;
--    signal cntAck                  : integer range 0 to 1000 := 0;
    signal state                   : integer range 0 to 1000;
    signal writeHost                : std_logic_vector(7 downto 0);
	signal data :  STD_LOGIC_VECTOR(39 downto 0);
    signal n : STD_LOGIC_VECTOR(7 downto 0);
    signal temp :   STD_LOGIC_VECTOR(7 downto 0);

    signal data_read :   STD_LOGIC;
    signal coordinate : STD_LOGIC_VECTOR(31 downto 0);
    signal coordinate_enc : STD_LOGIC_VECTOR(31 downto 0);
    signal coordinateReceived : STD_LOGIC_VECTOR(31 downto 0);
    signal resetDecrypt1 : STD_LOGIC;
    signal enableDecrypt1 : STD_LOGIC;
    signal resetDecrypt2 : STD_LOGIC;
    signal enableDecrypt2 : STD_LOGIC;
    signal resetEncrypt : STD_LOGIC;
    signal enableEncrypt : STD_LOGIC;

    signal ack2Receive : STD_LOGIC_VECTOR(31 downto 0);
    signal ack2DecEnbale : STD_LOGIC;
    signal ack2DecReset : STD_LOGIC;
    signal ack2Decryted : STD_LOGIC_VECTOR(31 downto 0);

    signal ack1Encrypted : STD_LOGIC_VECTOR(31 downto 0);
    signal ack1EncEnbale : STD_LOGIC;
    signal ack1EncReset : STD_LOGIC;
    signal ack2Encrypted : STD_LOGIC_VECTOR(31 downto 0);
    signal ack2EncEnbale : STD_LOGIC;
    signal ack2EncReset : STD_LOGIC;

    signal wrote_data : STD_LOGIC;

    signal readChan : STD_LOGIC_VECTOR(6 downto 0);
    signal writeChan : STD_LOGIC_VECTOR(6 downto 0);
--    signal key : STD_LOGIC_VECTOR(31 downto 0) := "11001100110011001100110011000001";
    signal key : STD_LOGIC_VECTOR(31 downto 0) := "10000011001100110011001100110011";
    signal ack1 : STD_LOGIC_VECTOR(31 downto 0);
    signal ack2 : STD_LOGIC_VECTOR(31 downto 0);

    signal to_wait_clk : STD_LOGIC;
    signal to_wait_sec : STD_LOGIC;
    signal stop_wait_sec : STD_LOGIC;
    signal waitCnt                   : integer range 0 to 768000000;
    signal waitLimit                 : integer range 0 to 768000000;
    signal waitSecCnt                   : integer range 0 to 500;
    signal waitSecLimit                 : integer range 0 to 500;

    signal data_in_buffer : STD_LOGIC_VECTOR(31 downto 0);
    signal read_done : STD_LOGIC;
    signal to_read : STD_LOGIC;
    signal cntRead                   : integer range 0 to 5;

    signal sendAck1 : STD_LOGIC;

    signal switch_info : STD_LOGIC_VECTOR(7 downto 0);
begin                                                                     --BEGIN_SNIPPET(registers)
	-- Infer registers
--	with chanAddr_in select f2hData_out <=
--		sw_in when "0000001",
--        read_data when "0000010",
--		x"00" when others;

--    read_data <= 
--            std_logic_vector(unsigned(h2fData_in))
--    	    	when chanAddr_in = "0000001" and h2fValid_in = '1'
--                else  read_data; 

	dec1 : entity work.decrypter
		port map(
			clock => clk_in,
			K => key,
			C => input_data(31 downto 0),
			P => input_data_dec(31 downto 0),
			reset => resetDecrypt1,
			enable => enableDecrypt1
		);

	dec2 : entity work.decrypter
		port map(
			clock => clk_in,
			K => key,
			C => input_data(63 downto 32),
			P => input_data_dec(63 downto 32),
			reset => resetDecrypt2,
            enable => enableDecrypt2
		);

    decAck2 : entity work.decrypter
		port map(
			clock => clk_in,
			K => key,
			C => ack2Receive,
			P => ack2Decryted,
			reset => ack2DecReset,
            enable => ack2DecEnbale
		);

	enc : entity work.encrypter
		port map(
			clock => clk_in,
			K => key,
			C => coordinate_enc,
			P => coordinate,
			reset => resetEncrypt,
			enable => enableEncrypt
		);

    encAck1 : entity work.encrypter
		port map(
			clock => clk_in,
			K => key,
			C => ack1Encrypted,
			P => ack1,
			reset => ack1EncReset,
			enable => ack1EncEnbale
		);

    encAck2 : entity work.encrypter
		port map(
			clock => clk_in,
			K => key,
			C => ack2Encrypted,
			P => ack2,
			reset => ack2EncReset,
			enable => ack2EncEnbale
		);

    process(clk_in)
    begin
    if(rising_edge(clk_in)) then
        if(reset_in = '1') then
            state <= 0;
            reg0 <= "10101010";
        else
		    reg0 <= reg0_next;

            if(f2hReady_in = '1') then 
                f2hData_out <= writeHost;
            end if; 
----------------------------------------
--------        Wait for given clocks
----------------------------------------
            if(to_wait_clk = '1') then
                if(waitCnt < waitLimit) then
                    waitCnt <= waitCnt + 1;
                else
                    to_wait_clk <= '0';
                    waitCnt <= 0;
                end if;
            else
                waitCnt <= 0;
            end if;

----------------------------------------
--------        Wait for given Seconds
----------------------------------------
            if(to_wait_sec = '1') then
                if(stop_wait_sec = '1') then
                    to_wait_sec <= '0';
                    waitSecCnt <= 0;
                    to_wait_clk <= '0';
                    stop_wait_sec <= '0';
                elsif(waitSecCnt < waitSecLimit) then
                    if(to_wait_clk = '0') then
                        to_wait_clk <= '1';
                        waitCnt <= 1;
                        waitLimit <= 48000000;
                        waitSecCnt <= waitSecCnt + 1;
                    end if;
                else
                    to_wait_sec <= '0';
                    waitSecCnt <= 0;
                end if;
            else
                waitSecCnt <= 0;
                to_wait_clk <= '0';
            end if;


--------------------------------------
------------        Read 4 bytes
--------------------------------------
            if(to_read = '1') then
                to_read <= '0';
                cntRead <= 0;
                data_read <= '0';
                read_done <= '0';
            else
                if(cntRead < 4) then
                    if(unsigned(chanAddr_in) = unsigned(readChan) and h2fValid_in = '1') then
        --            if(chanAddr_in = "00000010" and h2fValid_in = '1') then
                        data_read <= '1';
                        read_data <= std_logic_vector(unsigned(h2fData_in));
                    else
                        data_read <= '0';
                    end if;
                    if(data_read = '1') then
                        data_in_buffer((cntRead*8 + 7) downto (cntRead*8)) <= read_data(7 downto 0);
                        cntRead <= cntRead + 1;
                    end if;
                else
                    read_done <= '1';
                end if;
            end if;

--------------------------------------
------------        Send Ack1
--------------------------------------
            if(sendAck1 = '1') then
                reg0_next <= "10100101";
                if(cnt < 4) then
                    if(unsigned(chanAddr_in) = unsigned(writeChan) and f2hReady_in = '1') then -- and wrote_data = '0') then
                       -- f2hValid_out <= '1';
                       -- wrote_data <= '1';
                        writeHost <= ack1Encrypted((cnt*8 + 7) downto (cnt*8));
                    --elsif( wrote_data = '1' ) then
                        --f2hValid_out <= '0';
                        cnt <= cnt +1 ;
                       -- wrote_data <= '0';
                    end if;
                elsif(cnt = 4) then
                    sendAck1 <= '0';
                    cnt <= cnt + 1;
                    --f2hValid_out <= '1';
                    writeHost <= "00000000";
                end if;
            end if;
    
    
--------------------------------------
------------        Reset
--------------------------------------
            if(state = 0) then
                    reg0_next <= "11000000";
                	checksum_next <= (others => '0');
                	checksum <= (others => '0');
                	reg0_next <= (others => '0');
                    input_64 <= (others => '0');
                    input_data <= (others => '0');
                    read_data <= (others => '0');
    
                    data_read <= '0';
                    coordinate <= "11110000111100001111000000100010";
                    resetDecrypt1 <= '1';
                    enableDecrypt1 <= '0';
                    resetDecrypt2 <= '1';
                    enableDecrypt2 <= '0';
                    resetEncrypt <= '1';
                    enableEncrypt <= '0';
                    writeHost <= "00000000";
                
                    ack2DecEnbale <= '0';
                    ack2DecReset <= '1';
                
                    ack1EncEnbale <= '0';
                    ack1EncReset <= '1';
                    ack2EncEnbale <= '0';
                    ack2EncReset <= '1';
                    wrote_data <= '0';
                    f2hValid_out <= '1';
                
                    readChan <= "0000011";
                    writeChan <= "0000010";
                    key <= "10000011001100110011001100110011";
                    ack1 <= "00001111000011110000111100001111";
                    ack2 <= "11110000111100001111000011110000";
    
                    to_wait_clk <= '0';
                    waitCnt <= 0;
                    waitSecCnt <= 0;
                    to_wait_sec <= '0';
                    stop_wait_sec <= '1';
    
                    to_read  <= '1';
                    sendAck1 <= '0';
    
    
                    state <= 1;
                    cnt <= 0;
    
--------------------------------------
------------        Start encryption
--------------------------------------
            elsif(state = 1) then
                reg0_next <= "11000001";
                if(cnt = 0) then
                    reg0_next <= "10000000";
                    resetEncrypt <= '0';
                    enableEncrypt <= '1';
                    ack1EncEnbale <= '1';
                    ack1EncReset <= '0';
                    ack2EncEnbale <= '1';
                    ack2EncReset <= '0';
--                    data_read <= '0';
                    cnt <= 1;
                elsif(cnt < 40) then
                    cnt <= cnt + 1;
                else
                    cnt <= 0;
                    state <= 2;
                    wrote_data <= '0';
                end if;
    
--------------------------------------
------------        Send Coordinates
--------------------------------------
            elsif(state = 2) then
                reg0_next <= "11000010";
                if(cnt < 4) then
                    reg0_next <= "11111111";
                    if((chanAddr_in = writeChan) and f2hReady_in = '1') then -- and (wrote_data = '0') ) then
                        reg0_next <= "11000001";
                        --f2hValid_out <= '1';
                       -- wrote_data <= '1';
                        writeHost <= coordinate_enc((cnt*8 + 7) downto (cnt*8));
                    --end if;
                    --if(wrote_data = '1') then
                       -- wrote_data <= '0';
                        --f2hValid_out <= '0';
                        cnt <= cnt + 1;
                    end if;
                else
                    cnt <= 0;
                    state <= 3;
                    reg0_next <= "10000010";
                    --f2hValid_out <= '1';
                    writeHost <= "00000000";
                end if;
    
--------------------------------------
------------        receive coordinates
--------------------------------------
            elsif(state = 3) then
                reg0_next <= "11000011";
                if(cnt = 0) then
--                    to_read <= '1';
                    cnt <= cnt + 1;
                    to_wait_sec <= '1';
                    waitSecLimit <= 257;
                    stop_wait_sec <= '0';
                elsif(cnt = 1) then
                    if(to_wait_sec = '0') then
                        state <= 0;
                        cnt <= 0;
                    elsif(read_done = '1') then
                        stop_wait_sec <= '1';
                        coordinateReceived(31 downto 0) <= data_in_buffer(31 downto 0);
--                        to_read <= '1';
                        cnt <= 0;
                        state <= 5;
                    end if;
                end if;
    
           -- elsif(state = 4) then
            --    reg0_next <= "11000100";
             --   if(coordinateReceived = coordinate_enc) then
        --            state <= 5;
         --      else
          --          state <= 0;
           --     end if;
    
--------------------------------------
------------        Send Ack1
--------------------------------------
            elsif(state = 5) then
                reg0_next <= "11000101";
                if(cnt = 0) then
                    to_read <= '1';
                    sendAck1 <= '1';
                elsif(cnt > 4) then
                    sendAck1 <= '0';
                    state <= 6;
                    cnt <= 0;
                end if;
    
--------------------------------------
------------        Receive Ack2
--------------------------------------
            elsif(state = 6) then
                reg0_next <= "11000110";
                if(cnt = 0) then
--                  to_read <= '1';
                    cnt <= cnt + 1;
                    to_wait_sec <= '1';
                    waitSecLimit <= 257;
                    stop_wait_sec <= '0';
                elsif(cnt = 1) then
                    if(to_wait_sec = '0') then
                        state <= 0;
                        cnt <= 0;
                    elsif(read_done = '1') then
                        stop_wait_sec <= '1';
                        ack2Receive(31 downto 0) <= data_in_buffer(31 downto 0);
                        to_read <= '1';
                        cnt <= 0;
                        state <= 8;
                    end if;
                end if;
    
            --elsif(state = 7) then
            --    reg0_next <= "11000111";
             --   if(ack2Encrypted = ack2Receive) then
            --        state <= 8;
           --     else
           --         state <= 0;
           --     end if;
    
------------------------------------------
------------        Receive First 4 bytes
------------------------------------------
            elsif(state = 8) then
                reg0_next <= "11001000";
                if(cnt = 0) then
--                    to_read <= '1';
                    cnt <= cnt + 1;
                elsif(cnt = 1) then
                    if(read_done = '1') then
                        input_data(31 downto 0) <= data_in_buffer(31 downto 0);
                        to_read <= '1';
                        cnt <= 0;
                        state <= 9;
                    end if;
                end if;
    
------------------------------------------
------------        Decrypt first 4 bytes
------------------------------------------
            elsif(state = 9) then
                reg0_next <= "11001001";
                if(cnt = 0) then
                    reg0_next <= "10000000";
                    resetDecrypt1 <= '0';
                    enableDecrypt1 <= '1';
                    cnt <= 1;
                elsif(cnt < 40) then
                    cnt <= cnt + 1;
                else
                    cnt <= 0;
                    state <= 10;
                end if;
    
--------------------------------------
------------        Send Ack1
--------------------------------------
            elsif(state = 10) then
                reg0_next <= "11001010";
                if(cnt = 0) then
                    sendAck1 <= '1';
                elsif(cnt > 4) then
                    sendAck1 <= '0';
                    state <= 11;
                    cnt <= 0;
                end if;
    
----------------------------------------
------------      Receive next 4 bytes
----------------------------------------
            elsif(state = 11) then
                reg0_next <= "11001011";
                if(cnt = 0) then
                    to_read <= '1';
                    cnt <= cnt + 1;
                elsif(cnt = 1) then
                    if(read_done = '1') then
                        input_data(63 downto 32) <= data_in_buffer(31 downto 0);
                        to_read <= '1';
                        cnt <= 0;
                        state <= 12;
                    end if;
                end if;
    
-----------------------------------------
------------        Decrypt next 4 bytes
-----------------------------------------
            elsif(state = 12) then
                reg0_next <= "11001100";
                if(cnt = 0) then
                    reg0_next <= "10000000";
                    resetDecrypt2 <= '0';
                    enableDecrypt2 <= '1';
                    cnt <= 1;
                elsif(cnt < 40) then
                    cnt <= cnt + 1;
                else
                    cnt <= 0;
                    state <= 13;
                end if;
    
--------------------------------------
------------        Send Ack1
--------------------------------------
            elsif(state = 13) then
                reg0_next <= "11001101";
                if(cnt = 0) then
                    sendAck1 <= '1';
                elsif(cnt > 4) then
                    sendAck1 <= '0';
                    state <= 14;
                    cnt <= 0;
                end if;
    
--------------------------------------
------------        Receive Ack2
--------------------------------------
            elsif(state = 14) then
                reg0_next <= "11001110";
                if(cnt = 0) then
--                    to_read <= '1';
                    cnt <= cnt + 1;
                    to_wait_sec <= '1';
                    waitSecLimit <= 257;
                    stop_wait_sec <= '0';
                elsif(cnt = 1) then
                    if(to_wait_sec = '0') then
                        state <= 0;
                        cnt <= 0;
                    elsif(read_done = '1') then
                        stop_wait_sec <= '1';
                        ack2Receive <= data_in_buffer(31 downto 0);
                        to_read <= '1';
                        cnt <= 0;
                        state <= 16;   
                    end if;
                end if;
    
        --    elsif(state = 15) then
        --        reg0_next <= "11001111";
        --        if(ack2Encrypted = ack2Receive) then
        --            state <= 16;
        --        else
        --            state <= 0;
        --        end if;
    
            elsif(state = 16) then
                reg0_next <= "11010000";
                switch_info <= sw_in(7 downto 0);
                state <= 17;
    
            elsif(state = 17) then
                reg0_next <= "11010001";
                if (cnt < 8) then
                    reg0_next <= "00111100";
                  	if (input_data_dec((cnt*8+5) downto (cnt*8+3)) = "000") then
            		    input_64(1 downto 0) <= input_data_dec((cnt*8+7) downto (cnt*8+6));
            		    input_64(4 downto 2) <= input_data_dec((cnt*8+2) downto (cnt*8));
            		elsif (input_data_dec((cnt*8+5) downto (cnt*8+3)) = "001") then
            		    input_64(6 downto 5) <= input_data_dec((cnt*8+7) downto (cnt*8+6));
            		    input_64(9 downto 7) <= input_data_dec((cnt*8+2) downto (cnt*8));
            		elsif (input_data_dec((cnt*8+5) downto (cnt*8+3)) = "010") then
            		    input_64(11 downto 10) <= input_data_dec((cnt*8+7) downto (cnt*8+6));
            		    input_64(14 downto 12) <= input_data_dec((cnt*8+2) downto (cnt*8));
            		elsif (input_data_dec((cnt*8+5) downto (cnt*8+3)) = "011") then
            		    input_64(16 downto 15) <= input_data_dec((cnt*8+7) downto (cnt*8+6));
            		    input_64(19 downto 17) <= input_data_dec((cnt*8+2) downto (cnt*8));
            		elsif (input_data_dec((cnt*8+5) downto (cnt*8+3)) = "100") then
            		    input_64(21 downto 20) <= input_data_dec((cnt*8+7) downto (cnt*8+6));
            		    input_64(24 downto 22) <= input_data_dec((cnt*8+2) downto (cnt*8));
            		elsif (input_data_dec((cnt*8+5) downto (cnt*8+3)) = "101") then
            		    input_64(26 downto 25) <= input_data_dec((cnt*8+7) downto (cnt*8+6));
            		    input_64(29 downto 27) <= input_data_dec((cnt*8+2) downto (cnt*8));
            		elsif (input_data_dec((cnt*8+5) downto (cnt*8+3)) = "110") then
            		    input_64(31 downto 30) <= input_data_dec((cnt*8+7) downto (cnt*8+6));
            		    input_64(34 downto 32) <= input_data_dec((cnt*8+2) downto (cnt*8));
            		elsif (input_data_dec((cnt*8+5) downto (cnt*8+3)) = "111") then
            		    input_64(36 downto 35) <= input_data_dec((cnt*8+7) downto (cnt*8+6));
            		    input_64(39 downto 37) <= input_data_dec((cnt*8+2) downto (cnt*8));
            		end if;
                    cnt <= cnt + 1;
                else
                    state <= 18;
                    cnt <= 0;
                end if;
    
            elsif(state = 18) then
--              reg0_next <= "11010010";
                if (cnt = 0 ) then 
        --             proc <= not proc; 
        --             cnt <= -1;
--        			enableDecrypt1 <= '0';
--                    resetDecrypt1 <= '1';
--        			enableDecrypt2 <= '0';
--                    resetDecrypt2 <= '1';
        
        			if((input_64(0) = '1') and (input_64(1)= '1') and (unsigned(input_64(4 downto 2))) > 1) then
        				reg0_next <= "00000100";
        			elsif((input_64(0) = '1') and (input_64(1)= '1') and (unsigned(input_64(4 downto 2))) < 2) then
            			reg0_next <= "00000010";
        			else
        				reg0_next <= "00000001";
        			end if;
                    cnt <= cnt + 1;
                elsif(cnt = 1 and to_wait_sec = '0') then
                    to_wait_sec <= '1';
                    waitSecLimit <= 4;
                    waitSecCnt <= 0;
                    stop_wait_sec <= '0';
                    cnt <= 2;
                elsif(cnt = 2 and to_wait_sec = '0') then
                    state <= 19;
                    cnt <= 0;
                end if;
    
        
            elsif(state = 19) then
                if (cnt = 0 ) then 
                    cnt <= cnt + 1;
        			if((input_64(5) = '1') and (input_64(6)= '1') and (unsigned(input_64(9 downto 7))) > 1) then
        				reg0_next <= "10000100";
        			elsif((input_64(5) = '1') and (input_64(6)= '1') and (unsigned(input_64(9 downto 7))) < 2)  then
        				reg0_next <= "10000010";
        			else
        				reg0_next <= "10000001";
        			end if;
                elsif(cnt = 1 and to_wait_sec = '0') then
                    to_wait_sec <= '1';
                    waitSecLimit <= 4;
                    waitSecCnt <= 0;
                    stop_wait_sec <= '0';
                    cnt <= 2;
                elsif(cnt = 2 and to_wait_sec = '0') then
                    state <= 20;
                    cnt <= 0;
                end if;
    
        
            elsif(state = 20) then
                if (cnt = 0 ) then 
                    cnt <= cnt + 1;
        			if((input_64(10) = '1') and (input_64(11)= '1') and (unsigned(input_64(14 downto 12))) > 1) then
        				reg0_next <= "01000100";
        			elsif((input_64(10) = '1') and (input_64(11)= '1') and (unsigned(input_64(14 downto 12))) < 2)  then
        				reg0_next <= "01000010";
        			else
        				reg0_next <= "01000001";
        			end if;
                elsif(cnt = 1 and to_wait_sec = '0') then
                    to_wait_sec <= '1';
                    waitSecLimit <= 4;
                    waitSecCnt <= 0;
                    stop_wait_sec <= '0';
                    cnt <= 2;
                elsif(cnt = 2 and to_wait_sec = '0') then
                    state <= 21;
                    cnt <= 0;
                end if;
    
        
            elsif(state = 21) then
                if (cnt = 0 ) then 
                  cnt <= cnt + 1;
        			if((input_64(15) = '1') and (input_64(16)= '1') and (unsigned(input_64(19 downto 17))) > 1) then
        				reg0_next <= "11000100";
        			elsif((input_64(15) = '1') and (input_64(16)= '1') and (unsigned(input_64(19 downto 17))) < 2)  then
        				reg0_next <= "11000010";
        			else
        				reg0_next <= "11000001";
        			end if;
                elsif(cnt = 1 and to_wait_sec = '0') then
                    to_wait_sec <= '1';
                    waitSecLimit <= 4;
                    waitSecCnt <= 0;
                    stop_wait_sec <= '0';
                    cnt <= 2;
                elsif(cnt = 2 and to_wait_sec = '0') then
                    state <= 22;
                    cnt <= 0;
                end if;
    
        	
            elsif(state = 22) then
                if (cnt = 0 ) then 
                  cnt <= cnt + 1;
        			if((input_64(20) = '1') and (input_64(21)= '1') and (unsigned(input_64(24 downto 22))) > 1) then
        				reg0_next <= "00100100";
        			elsif((input_64(20) = '1') and (input_64(21)= '1') and (unsigned(input_64(24 downto 22))) < 2)  then
        				reg0_next <= "00100010";
        			else
        				reg0_next <= "00100001";
        			end if;
                elsif(cnt = 1 and to_wait_sec = '0') then
                    to_wait_sec <= '1';
                    waitSecLimit <= 4;
                    waitSecCnt <= 0;
                    stop_wait_sec <= '0';
                    cnt <= 2;
                elsif(cnt = 2 and to_wait_sec = '0') then
                    state <= 23;
                    cnt <= 0;
                end if;
        	
            elsif(state = 23) then
                if (cnt = 0 ) then 
                  cnt <= cnt + 1;
        			if((input_64(25) = '1') and (input_64(26)= '1') and (unsigned(input_64(29 downto 27))) > 1) then
        				reg0_next <= "10100100";
        			elsif((input_64(25) = '1') and (input_64(26)= '1') and (unsigned(input_64(29 downto 27))) < 2)  then
        				reg0_next <= "10100010";
        			else
        				reg0_next <= "10100001";
        			end if;
                elsif(cnt = 1 and to_wait_sec = '0') then
                    to_wait_sec <= '1';
                    waitSecLimit <= 4;
                    waitSecCnt <= 0;
                    stop_wait_sec <= '0';
                    cnt <= 2;
                elsif(cnt = 2 and to_wait_sec = '0') then
                    state <= 24;
                    cnt <= 0;
                end if;
    
            elsif(state = 24) then
                if (cnt = 0 ) then 
                  cnt <= cnt + 1;
        			if((input_64(30) = '1') and (input_64(31)= '1') and (unsigned(input_64(34 downto 32))) > 1) then
        				reg0_next <= "01100100";
        			elsif((input_64(30) = '1') and (input_64(31)= '1') and (unsigned(input_64(34 downto 32))) < 2)  then
        				reg0_next <= "01100010";
        			else
        				reg0_next <= "01100001";
        			end if;
                elsif(cnt = 1 and to_wait_sec = '0') then
                    to_wait_sec <= '1';
                    waitSecLimit <= 4;
                    waitSecCnt <= 0;
                    stop_wait_sec <= '0';
                    cnt <= 2;
                elsif(cnt = 2 and to_wait_sec = '0') then
                    state <= 25;
                    cnt <= 0;
                end if;
        	
            elsif(state = 25) then
                if (cnt = 0 ) then 
                  cnt <= cnt + 1;
        			if((input_64(35) = '1') and (input_64(36)= '1') and (unsigned(input_64(39 downto 37))) > 1) then
        				reg0_next <= "11100100";
        			elsif((input_64(35) = '1') and (input_64(36)= '1') and (unsigned(input_64(39 downto 37))) < 2)  then
        				reg0_next <= "11100010";
        			else
        				reg0_next <= "11100001";
        			end if;
                elsif(cnt = 1 and to_wait_sec = '0') then
                    to_wait_sec <= '1';
                    waitSecLimit <= 4;
                    waitSecCnt <= 0;
                    stop_wait_sec <= '0';
                    cnt <= 2;
                elsif(cnt = 2 and to_wait_sec = '0') then
                    state <= 26;
                    cnt <= 0;
                end if;
        
        --        elsif(cnt = 336000009) then
        --            cnt <= cnt + 1;
        --            coordinate <= sw_in;
            elsif(state = 26) then
        --            coordinate <= "00000000";
        		reg0_next <= "11110000";
                if(cnt = 0 and to_wait_sec = '0') then
                    to_wait_sec <= '1';
                    waitSecLimit <= 9;
                    waitSecCnt <= 0;
                    stop_wait_sec <= '0';
                    cnt <= cnt + 1;
                elsif(cnt = 1 and to_wait_sec = '0') then
                    state <= 0;
                    cnt <= 0;
                end if;
            end if;
        end if;
    end if;
--			wait 8000000000ns;
    end process;


--	process(clk_in)
--	begin
--		if ( rising_edge(clk_in) ) then
--			if ( reset_in = '1' ) then
--				reg0 <= (others => '0');
--			else
--				reg0 <= reg0_next;
--			end if;
--		end if;
--	end process;

	-- Drive register inputs for each channel when the host is writing
--	reg0_next <=
--		h2fData_in when chanAddr_in = "0000000" and h2fValid_in = '1'
--		else reg0;
--	checksum_next <=
--		std_logic_vector(unsigned(checksum) + unsigned(h2fData_in))
--			when chanAddr_in = "0000000" and h2fValid_in = '1'
--		else h2fData_in & checksum(7 downto 0)
--			when chanAddr_in = "0000001" and h2fValid_in = '1'
--		else checksum(15 downto 8) & h2fData_in
--			when chanAddr_in = "0000010" and h2fValid_in = '1'
--		else checksum;
	
	-- Select values to return for each channel when the host is reading
--	with chanAddr_in select f2hData_out <=
--		sw_in                 when "0000000",
--		checksum(15 downto 8) when "0000001",
--		checksum(7 downto 0)  when "0000010",
--		x"00" when others;

	-- Assert that there's always data for reading, and always room for writing
--	f2hValid_out <= '1';
	h2fReady_out <= '1';                                                     --END_SNIPPET(registers)

	-- LEDs and 7-seg display
	led_out <= reg0;
	flags <= "00" & f2hReady_in & reset_in;
	seven_seg : entity work.seven_seg
		port map(
			clk_in     => clk_in,
			data_in    => checksum,
			dots_in    => flags,
			segs_out   => sseg_out,
			anodes_out => anode_out
		);
end architecture;
