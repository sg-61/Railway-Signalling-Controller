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
	signal checksum, checksum_next : std_logic_vector(15 downto 0) := (others => '0');
	signal reg0, reg0_next         : std_logic_vector(7 downto 0)  := (others => '0');
    signal input_64                : std_logic_vector(39 downto 0) := (others => '0');
    signal read_data               : std_logic_vector(7 downto 0)  := (others => '0');
    signal read_byte               : std_logic;
    signal cnt                     : integer range -1 to 480000000 := -1;

	signal proc : STD_LOGIC ;
	signal data :  STD_LOGIC_VECTOR(39 downto 0);
    signal n : STD_LOGIC_VECTOR(7 downto 0);
    signal ne :   STD_LOGIC_VECTOR(7 downto 0);
    signal  e :   STD_LOGIC_VECTOR(7 downto 0); 
    signal se :   STD_LOGIC_VECTOR(7 downto 0);
    signal  s :   STD_LOGIC_VECTOR(7 downto 0);
   	signal sw :   STD_LOGIC_VECTOR(7 downto 0);
    signal w :   STD_LOGIC_VECTOR(7 downto 0);
    signal nw :   STD_LOGIC_VECTOR(7 downto 0);
	signal wrote: STD_LOGIC; 
    signal temp :   STD_LOGIC_VECTOR(7 downto 0);
begin                                                                     --BEGIN_SNIPPET(registers)
	-- Infer registers
	process(clk_in)
	begin
		if(rising_edge(clk_in)) then 
			if(cnt>250000000 and cnt < 250000009) then
					if(cnt = 250000001) then
							reg0_next <= "00000001"; 
					end if;
					if(cnt = 250000003) then
							reg0_next <= "00000011"; 
					end if; 
					if(cnt = 250000005) then
							reg0_next <= "00000111"; 
					end if; 
					if(cnt = 250000007) then
							reg0_next <= "00001111"; 
					end if; 
					if(cnt = 250000008) then
							reg0_next <= "00011111"; 
					end if; 
					if(wrote = '0' and f2hReady_in = '1' and chanAddr_in = "0000010") then
							wrote <= '1'; 
							if(cnt = 250000001) then
								f2hData_out <= "00000001"; 
							elsif ( cnt= 250000003) then
								f2hData_out <= "00000010"; 
							elsif ( cnt = 250000005) then
								f2hData_out <= "00000011"; 
							else 
								f2hData_out <= "00000100"; 
							end if; 
							cnt<= cnt+1; 
					elsif( wrote ='1') then
							f2hData_out <= "11000011"; 
							wrote <= '0'; 
							cnt <= cnt+1; 
					end if;
			elsif( cnt = 250000009) then
					f2hData_out <= "11000011"; 
					wrote <= '0'; 
					reg0_next <= "11110000"; 
					cnt<= -1; 
			else 
					f2hData_out <= "11000011"; 
					wrote <= '0'; 
					reg0_next <= "11110000"; 
					cnt <= cnt+1; 
			end if; 
		end if; 
	end process; 
				
        
	f2hValid_out <= '1'; 
	h2fReady_out <= '1';                                                     --END_SNIPPET(registers)
	process(clk_in)
	begin
		if ( rising_edge(clk_in) ) then
			if ( reset_in = '1' ) then
				reg0 <= (others => '0');
			else
				reg0 <= reg0_next;
			end if;
		end if;
	end process;


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
