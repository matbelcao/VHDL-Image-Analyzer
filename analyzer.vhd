-- Prova finale di reti logiche, scaglione 1 (Fornaciari)

------------- AUTORI--------------
-- Giuseppe Bertolini (matr. 847163) : 10493758
-- Matteo Belcao 	  (matr. 845180) : 10503336

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity project_reti_logiche is
	port (
		i_clk     : in 	std_logic;
		i_start   : in 	std_logic;
		i_rst     : in 	std_logic;
		i_data    : in 	std_logic_vector ( 7 downto 0 );
		o_address : out std_logic_vector ( 15 downto 0 );
		o_done    : out std_logic;
		o_en      : out std_logic;
		o_we      : out std_logic;
		o_data    : out std_logic_vector ( 7 downto 0 )
	);
end project_reti_logiche;


architecture MAIN of project_reti_logiche is

	component fsm_controller
	port (
		i_clk          : in  std_logic;
		i_start        : in  std_logic;
		i_rst          : in  std_logic;
		i_state_end    : in  std_logic;
		i_isempty      : in  std_logic;
		o_fsm_en_state : out std_logic_vector ( 0 to 2 );
		o_done         : out std_logic
	);
	end component fsm_controller;

	component loader
	port (
		i_clk          : in  std_logic;
		i_fsm_en_state : in  std_logic_vector ( 0 to 2 );
		i_data         : in  std_logic_vector ( 7 downto 0 );
		o_state_end    : out std_logic;
		o_en           : out std_logic;
		o_we           : out std_logic;
		o_mem_addr     : out std_logic_vector ( 15 downto 0 );
		o_NC           : out std_logic_vector ( 7 downto 0 );
		o_NR           : out std_logic_vector ( 7 downto 0 );
		o_SOGLIA       : out std_logic_vector ( 7 downto 0 )
	);
	end component loader;


	component inspector
	port (
		i_clk          : in  std_logic;
		i_fsm_en_state : in  std_logic_vector ( 0 to 2 );
		i_data         : in  std_logic_vector ( 7 downto 0 );
		i_nr           : in  std_logic_vector ( 7 downto 0 );
		i_nc           : in  std_logic_vector ( 7 downto 0 );
		i_soglia       : in  std_logic_vector ( 7 downto 0 );
		o_height	   : out std_logic_vector ( 7 downto 0);
        o_width        : out std_logic_vector ( 7 downto 0);
		o_isempty      : out std_logic;
		o_state_end    : out std_logic;
		o_en           : out std_logic;
		o_we           : out std_logic;
		o_mem_addr     : out std_logic_vector ( 15 downto 0 )
	);
	end component inspector;


	component multiplier
	port (
		i_clk          : in  std_logic;
		i_fsm_en_state : in  std_logic_vector ( 0 to 2 );
		i_isempty      : in  std_logic;
		i_height	   : in 	std_logic_vector ( 7 downto 0);
		i_width		   : in 	std_logic_vector ( 7 downto 0);
		o_state_end    : out std_logic;
		o_en           : out std_logic;
		o_we           : out std_logic;
		o_data         : out std_logic_vector ( 7 downto 0);
		o_mem_addr     : out std_logic_vector ( 15 downto 0 )
	);
	end component multiplier;

	signal fsm_en_state 							: std_logic_vector ( 2 downto 0 );
	signal LD_END, ISP_END, MT_END, isempty			: std_logic;
    signal LD_MEM_ADDR, ISP_MEM_ADDR, MT_MEM_ADDR 	: std_logic_vector ( 15 downto 0 );
    signal LD_O_EN, ISP_O_EN, MT_O_EN, state_end	: std_logic;
    signal LD_O_WE, ISP_O_WE, MT_O_WE				: std_logic;
	signal nr, nc, soglia, mt_height,mt_width 		: std_logic_vector ( 7 downto 0 );

begin

    state_end <= LD_END  or ISP_END  or MT_END;
    o_address <= LD_MEM_ADDR or ISP_MEM_ADDR or MT_MEM_ADDR;
    o_en      <= LD_O_EN or ISP_O_EN or MT_O_EN;
    o_we      <= LD_O_WE or ISP_O_WE or MT_O_WE;

	fsm_controller_i : fsm_controller
	port map (
		i_clk          => i_clk,
		i_start        => i_start,
		i_rst          => i_rst,
		i_state_end    => state_end,
		i_isempty      => isempty,
		o_fsm_en_state => fsm_en_state,
		o_done         => o_done
	);

	loader_i : loader
	port map (
		i_clk          => i_clk,
		i_fsm_en_state => fsm_en_state,
		i_data         => i_data,
		o_state_end    => ld_end,
		o_en           => ld_o_en,
		o_we           => ld_o_we,
		o_mem_addr     => ld_mem_addr,
		o_NC           => NC,
		o_NR           => NR,
		o_SOGLIA       => SOGLIA
	);


	inspector_i : inspector
	port map (
		i_clk          => i_clk,
		i_fsm_en_state => fsm_en_state,
		i_data         => i_data,
		i_nr           => nr,
		i_nc           => nc,
		i_soglia       => soglia,
		o_height	   => mt_height,
		o_width		   => mt_width,
		o_isempty      => isempty,
		o_state_end    => isp_end,
		o_en           => isp_o_en,
		o_we           => isp_o_we,
		o_mem_addr     => isp_mem_addr
	);


	multiplier_i : multiplier
	port map (
		i_clk          => i_clk,
		i_fsm_en_state => fsm_en_state,
		i_isempty      => isempty,
		i_height	   => mt_height,
		i_width		   => mt_width,
		o_state_end    => mt_end,
		o_en           => mt_o_en,
		o_we           => mt_o_we,
		o_data         => o_data,
		o_mem_addr     => mt_mem_addr
	);

end MAIN;


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity fsm_controller is
	port (
		i_clk          	: in 	std_logic;
		i_start        	: in 	std_logic;
		i_rst          	: in 	std_logic;
		i_state_end   	: in 	std_logic;
		i_isempty      	: in 	std_logic;
		o_fsm_en_state 	: out 	std_logic_vector ( 0 to 2 );
		o_done		   	: out 	std_logic
	);
end fsm_controller;
architecture fsm of fsm_controller is
    type STATUS is ( RST, LOAD, LFT, RGT, UP, DOWN, MUL );
    signal r_CS, r_NS       : STATUS;
    signal ES               : std_logic_vector( 0 to 2 );
    signal D                : std_logic;
begin
-- FUNZIONE di STATO PROSSIMO
    FSM1 : process( r_CS, i_start, i_state_end)
    begin
        case r_CS is
            when RST =>
                if( i_start = '1' ) then
                    r_NS <= LOAD;
                    ES   <= "001";
                    D    <= '0';
                else
                    r_NS <= RST;
                    ES   <= "000";
                    D    <= '0';
                end if;
            when LOAD =>
                if( i_state_end = '1' ) then
                    r_NS <= LFT;
                    ES 	 <= "010";
                    D    <= '0';
                else
                    r_NS <= LOAD;
                    ES   <= "001";
                    D    <= '0';
                end if;
            when LFT =>
                if( i_state_end = '1' ) then
                    if ( i_ISEMPTY = '1' ) then
                        r_NS <= MUL;
                        ES   <= "110";
                        D    <= '0';
                    else
                        r_NS <= RGT;
                        ES   <= "011";
                        D    <= '0';
                    end if;
                else
                    r_NS <= LFT;
                    ES   <= "010";
                    D    <= '0';
                end if;
            when RGT =>
                if( i_state_end = '1' ) then
                    r_NS <= UP;
                    ES   <= "100";
                    D    <= '0';
                else
                    r_NS <= RGT;
                    ES   <= "011";
                    D    <= '0';
                end if;
            when UP =>
                if( i_state_end = '1' ) then
                    r_NS <= DOWN;
                    ES   <= "101";
                    D    <= '0';
                else
                    r_NS <= UP;
                    ES   <= "100";
                    D    <= '0';
                end if;
            when DOWN =>
                if( i_state_end = '1' ) then
                    r_NS <= MUL;
                    ES   <= "110";
                    D    <= '0';
                else
                    r_NS <= DOWN;
                    ES   <= "101";
                    D    <= '0';
                end if;
            when MUL =>
                if( i_state_end = '1' ) then
                    r_NS <= RST;
                    ES   <= "000";
                    D    <= '1';
                else
                    r_NS <= MUL;
                    ES   <= "110";
                    D    <= '0';
                end if;

            when others =>
                    r_NS <= RST;
                    ES   <= "000";
                    D    <= '0';
        end case;
    end process;

-- STATE REGISTER
    FSM2 : process( i_clk )
    begin
        if( rising_edge( i_clk ) ) then
            if( i_rst = '1' ) then
                r_CS <= RST;
            else
                r_CS <= r_NS;
            end if;
        end if;
    end process;

-- OUTPUT REGISTER
    FSM3 : process( i_clk )
    begin
        if( rising_edge( i_clk ) ) then
            if( i_rst = '1' ) then
                o_fsm_en_state 	<= "000";
                o_done       	<= '0';
            else
                o_fsm_en_state 	<= ES;
                o_done       	<= D;
            end if;
        end if;
    end process;
end fsm;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity loader is
	port (
		i_clk          			: in 	std_logic;
		i_fsm_en_state			: in	std_logic_vector ( 0 to 2 );
		i_data          		: in 	std_logic_vector ( 7 downto 0 );
		o_state_end   			: out 	std_logic;
		o_en 					: out 	std_logic;
		o_we 					: out 	std_logic;
		o_mem_addr				: out 	std_logic_vector ( 15 downto 0 );
		o_NC					: out 	std_logic_vector ( 7 downto 0 );
		o_NR					: out 	std_logic_vector ( 7 downto 0 );
		o_SOGLIA				: out 	std_logic_vector ( 7 downto 0 )
	);
end loader;

architecture load of loader is
	signal LD_MEM_ADDR      : std_logic_vector ( 15 downto 0 );
    signal LD_TEMP          : std_logic_vector ( 2 downto 0 );
begin
-- MAPPATURA SEGNALI
	o_mem_addr 	<= ld_mem_addr;

-- CARICAMENTO dei REGISTRI
    LOADING : process( i_clk , i_fsm_en_state )
    begin
        if ( falling_edge( i_clk ) ) then
            if ( i_fsm_en_state = "001" ) then
                case LD_TEMP is
                    when "010" =>
                        O_EN  		<= '1';
                        O_WE  		<= '0';
                    when "011" =>
                        o_NC     	<= i_data;
                    when "100" =>
                        o_NR     	<= i_data;
                    when "101" =>
                        o_SOGLIA 	<= i_data;
                        O_EN  		<= '0';
                        o_state_end <= '1';
                    when "111" =>  --fine caricamento dati, aspetto cambio di stato fsm_controller
                        O_EN  		<= '0';
                        o_state_end <= '1';
                    when others =>
                        O_EN  <= '0';
                        o_state_end   <= '0';
                end case;
                if( LD_TEMP < "101" ) then
                    LD_MEM_ADDR <= "0000000000000" & LD_TEMP;
                    LD_TEMP     <= LD_TEMP + "001";
                else
                    LD_MEM_ADDR <= "0000000000000000";
                    LD_TEMP     <= "111";  -- fine conteggio
                end if;
	        else     -- MODULO DISATTIVATO / RESET
	            LD_TEMP     <= "010";
	            O_WE        <= '0';
	            O_EN        <= '0';
	            o_state_end <= '0';
	            LD_MEM_ADDR <= "0000000000000000";
			end if;
        end if;
    end process;
end load;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity inspector is
	port (
		i_clk          			: in 	std_logic;
		i_fsm_en_state			: in	std_logic_vector ( 0 to 2 );
		i_data          		: in 	std_logic_vector ( 7 downto 0 );
		i_nr					: in	std_logic_vector ( 7 downto 0 );
		i_nc 					: in	std_logic_vector ( 7 downto 0 );
		i_soglia 				: in	std_logic_vector ( 7 downto 0 );
		o_height				: out 	std_logic_vector ( 7 downto 0);
		o_width					: out 	std_logic_vector ( 7 downto 0);
		o_isempty				: out 	std_logic;
		o_state_end   			: out 	std_logic;
		o_en 					: out 	std_logic;
		o_we 					: out 	std_logic;
		o_mem_addr				: out 	std_logic_vector ( 15 downto 0 )
	);
end inspector;

architecture inspect of inspector is
	signal ISP_MEM_ADDR     : std_logic_vector ( 15 downto 0 );
	signal ISP_WORK         : std_logic;
	signal r_NC, r_NR, r_SOGLIA, r_R1, r_C1, r_R2, r_C2, r_RTEMP, r_CTEMP : std_logic_vector ( 7 downto 0);
begin
-- MAPPATURA SEGNALI
	o_mem_addr 	<= isp_mem_addr;
	r_NC 		<= i_nc;
	r_NR 		<= i_nr;
	r_SOGLIA 	<= i_soglia;

-- ISPEZIONE dell'IMMAGINE
    INSPECTING : process( i_clk )
    begin
      	if( falling_edge( i_clk ) ) then
           	case (i_fsm_en_state & ISP_WORK) is

    -- LEFT
                when "0100" =>
        -- PRESET
                    ISP_MEM_ADDR <= "0000000000000101"; -- ADDR start

                    o_ISEMPTY    <= '0';
                    o_we         <= '0';
                    o_en         <= '1' ;

                    r_R1         <= "00000001";
                    r_C1         <= "00000001";
                    ISP_WORK     <= '1';
                    o_state_end  <= '0';
                when "0101" =>
		-- ISPEZIONE
                --pixel no buono, fine di una colonna -> scorro a dx di una colonna
                    if( i_data < r_SOGLIA and r_C1 < r_NC  and r_R1 = r_NR ) then
                        r_R1 			 <= "00000001";
                        r_C1 			 <= r_C1 + "00000001";
                        ISP_MEM_ADDR 	 <= ("00000000" & (r_C1 + "00000101")); -- ADDR = r_C1 + 5
                    end if;
                --pixel no buono, scendo di una riga
                    if( i_data < r_SOGLIA and (r_R1 < r_NR) )then
                        r_R1 			<= r_R1 + "00000001";
                        ISP_MEM_ADDR 	<= ISP_MEM_ADDR + ("00000000" & r_NC); -- ADDR = ADDR + r_NC
                    end if;
                --immagine vuota
                    if ( i_data < r_SOGLIA and r_C1 = r_NC and r_R1 = r_NR ) then
                        o_ISEMPTY   <= '1';
                        o_state_end <= '1';
                        ISP_WORK    <= '0';
                    end if;
                --pixel buono
                    if(i_data >= r_SOGLIA) then
                        o_state_end  <= '1';
                        ISP_WORK 	 <= '0';
                    end if;

    -- RIGHT
                when "0110" =>
        -- PRESET
                    ISP_MEM_ADDR <= ( "00000000" & (r_NC +"00000100") ); -- ADDR start
                    o_we        <= '0';
                    o_en        <= '1' ;
                    r_R2        <= "00000001";
                    r_C2        <= r_NC;
                    ISP_WORK    <= '1';
                    o_state_end <= '0';
				when "0111" =>
        -- ISPEZIONE
                --caso solo punti buoni nell'ultima colonna
                    if ( r_NC = r_C1 ) then --equivale a r_C2 = r_C1
                        r_R2        <= r_R1;
                        o_state_end <= '1';
                        ISP_WORK    <= '0';
                    end if;
                -- pixel no buono, fine colonna, a sx ho r_c1
                    if ( i_data < r_SOGLIA  and r_R2 = r_NR and r_C2 = (r_C1 + "00000001") ) then
                        r_C2        <= r_C1; -- NON RIGUARDO COLONNA VISTA DA LEFT MA SEMPLICEMENTE AGGIORNO r_C2
                        r_R2        <= r_R1;
                        o_state_end <= '1';
                        ISP_WORK    <= '0';
                    end if;
                -- pixel no buono, fine colonna, ho ancora colonne non già viste a sx
                    if (i_data < r_SOGLIA and r_R2 = r_NR and r_C2 > (r_C1 + "00000001")) then--cambio colonna
                        r_R2 			<= "00000001";
                        r_C2 			<= r_C2 - "00000001";
                        ISP_MEM_ADDR 	<= ("00000000" & (r_C2 + "00000011")); -- ADDR = (r_C2-1) + 5 supponendo r_c2 già aggiornato
                    end if;
                --pixel no buono, semplicemente scendo di una riga
                    if( i_data < r_SOGLIA and r_C2 > r_C1  and r_R2 < r_NR) then
                        r_R2 		 <= r_R2 + "00000001";
                        ISP_MEM_ADDR <= ISP_MEM_ADDR + ("00000000" & r_NC); -- ADDR = ADDR + r_NC
                    end if;

                --pixel buono
                    if( i_data >= r_SOGLIA and r_C2 > r_C1 ) then --PIXEL BUONO
                        --riordino r_R1 e r_R2 perchè sia r_R1 <= r_R2
                        if ( r_R1 > r_R2 ) then
                            r_R1 <= r_R2;
                            r_R2 <= r_R1;
                        end if;
                        o_state_end <= '1';
                        ISP_WORK 	<= '0';
                    end if;

    -- UP
				when "1000" =>
		-- PRESET
						ISP_MEM_ADDR <= "00000000" & (r_C1 + "00000101")  ;   -- ADDR start
						o_we         <= '0';
						o_en         <= '1' ;
						r_RTEMP      <= "00000001";
						r_CTEMP      <= "00000001" + r_C1;                    -- parto dalla prima riga dalla colonna dopo r_C1;
						ISP_WORK     <= '1';
						o_state_end  <= '0';

                when "1001" =>
        -- ISPEZIONE
				--già ispezionato sopra con left e right
                    if( r_C1 >= r_C2 - "00000001" or r_R1 = "00000001"  ) then
                        o_state_end  	<= '1';
                        ISP_WORK 		<= '0';
                    else
					--pixel no buono, fine riga, scorrimento BASSO riga
                        if ( i_data < r_SOGLIA and r_R1 > r_RTEMP + "00000001" and ( r_CTEMP + "00000001" ) = r_C2 ) then
                            r_CTEMP 		<= r_C1 + "00000001";
                            r_RTEMP 		<= r_RTEMP + "00000001";
                            ISP_MEM_ADDR  	<= ISP_MEM_ADDR + ( "00000000" & ( r_NC - r_C2 + r_C1  + "00000010" )); -- ADDR = ADDR + ( r_NC - ( r_C2 -r_C1 ) + 2 )
						end if;
					--pixel no buono, fine riga e la prossima riga è R1
						if ( i_data < r_SOGLIA and r_R1 = r_RTEMP + "00000001" and ( r_CTEMP + "00000001" ) = r_C2 ) then
                            o_state_end <= '1';
                            ISP_WORK 	<= '0';
						end if;
                    --pixel no buono, normale scorrimento a dx
						if ( i_data < r_SOGLIA and ( r_CTEMP + "00000001" ) < r_C2 ) then
                            r_CTEMP 		<= r_CTEMP + "00000001";
                            ISP_MEM_ADDR  	<= ISP_MEM_ADDR + "0000000000000001"; -- ADDR = ADDR + 1
                        end if;

					--pixel buono
	                    if (i_data >= r_SOGLIA) then
	                        r_R1 			<= r_RTEMP;
	                        o_state_end 	<= '1';
	                        ISP_WORK 		<= '0';
	                    end if;
                    end if;

   	-- DOWN
				when "1010" =>
		-- PRESET
					ISP_MEM_ADDR <= ( r_NC * ( r_NR - "00000001" ) ) + ( "00000000" & (r_C1 + "00000100"));  -- ADDR start = r_NC * (r_NR - 1) + r_C1 + 4
					o_we         <= '0';
					o_en         <= '1' ;
					r_RTEMP      <= r_NR;
					r_CTEMP      <= r_C1;
					ISP_WORK     <= '1';
					o_state_end  <= '0';
                when "1011" =>
		-- ISPEZIONE
				--pixel no buono, fine riga, scorrimento in su di una riga
                    if ( i_data < r_SOGLIA and r_R2 < r_RTEMP - "00000001" and r_CTEMP = r_C2 ) then
                        r_CTEMP   		<= r_C1; --reset r_CTEMP
                        r_RTEMP  		<= r_RTEMP - "00000001";
                        ISP_MEM_ADDR 	<= ISP_MEM_ADDR - ( "00000000" & ( r_NC + r_C2 - r_C1 ) ); -- ADDR = ADDR - ( r_NC - ( r_C2 - r_C1 ) ))
					end if;
				--pixel no buono, fine riga, quella sopra è R2 -> end
					if ( i_data < r_SOGLIA and (r_R2 = r_NR or (r_R2 = r_RTEMP - "00000001" and r_CTEMP = r_C2 )) ) then
                    --anticipo calcolo dimensione per area
                        o_height <= ( r_R2 - r_R1 + "00000001");
                        o_width  <= ( r_C2 - r_C1 + "00000001" );
					--termino down
						o_state_end  <= '1';
						ISP_WORK <= '0';
					end if;
				--pixel no buono, scorro a dx
                    if ( i_data < r_SOGLIA and r_CTEMP < r_C2 and r_R2 < r_NR ) then
                        r_CTEMP      <= r_CTEMP + "00000001";
                        ISP_MEM_ADDR <= ISP_MEM_ADDR + "0000000000000001"; -- ADDR = ADDR +1
                    end if;
                --pixel buono -> end
					if ( i_data >= r_SOGLIA ) then
                    --anticipo calcolo dimensione per area
                        o_height 	<= ( r_RTEMP - r_R1 + "00000001");
                        o_width 	<= ( r_C2 - r_C1 + "00000001" );
					--termino down
						r_R2        <= r_RTEMP;
                        o_state_end     <= '1';
                        ISP_WORK 	<= '0';
					end if;

	-- MODULO DISATTIVATO / RESET
                when others =>
                  	o_we         <= '0';
                  	o_en         <= '0';
                  	o_state_end  <= '0';
                  	ISP_MEM_ADDR <= "0000000000000000";
                  	ISP_WORK     <= '0';
                    if ( i_fsm_en_state = "000" ) then
                        o_ISEMPTY  <= '0';
                    end if;
            end case;
      	end if;
   	end process;

end inspect;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity multiplier is
	port (
		i_clk          	: in 	std_logic;
		i_fsm_en_state	: in	std_logic_vector ( 0 to 2 );
		i_isempty		: in 	std_logic;
		i_height		: in 	std_logic_vector ( 7 downto 0);
		i_width			: in 	std_logic_vector ( 7 downto 0);
		o_state_end   	: out 	std_logic;
		o_en 			: out 	std_logic;
		o_we 			: out 	std_logic;
		o_data			: out 	std_logic_vector ( 7 downto 0);
		o_mem_addr		: out 	std_logic_vector ( 15 downto 0 )
	);
end multiplier;

architecture multiplying of multiplier is
	signal MT_WORK          : std_logic;
    signal MT_WEN     		: std_logic;
begin

-- MOLTIPLICAZIONE e SCRITTURA in MEMORIA
   	MULTIPLIER : PROCESS( i_clk, i_fsm_en_state )
   		variable RIS : std_logic_vector(15 downto 0);
   	begin
       	if ( falling_edge( i_clk ) ) then
           	case (i_FSM_en_state & MT_WEN) is
           	    when "1100" =>
    -- MOLTIPLICAZIONE
                    case i_ISEMPTY is
                        when '1'    =>
                            RIS  := "0000000000000000";
                        when others =>
                            RIS  := i_width * i_height;
                    end case;
               	-- preparazione scrittura su memoria
                    MT_WEN 	<= '1';
                    O_WE <= '1';
                    O_EN <= '1';

                when "1101" =>
	-- SCRITTURA SU MEMORIA
                    case MT_WORK is
                        when '0' =>
                            o_data      <= RIS (7 downto 0);
                            o_MEM_ADDR  <= "0000000000000000";
                            MT_WORK     <= '1';
                        when '1' =>
                            o_data   	<= RIS (15 downto 8);
                            o_MEM_ADDR  <= "0000000000000001";
                            o_state_end <= '1';
                        when others =>
                            null;
                    end case;

                when others =>
	-- MODULO DISATTIVATO / RESET
                    MT_WEN      <= '0';
                    MT_WORK     <= '0';
                    O_WE        <= '0';
                    O_EN        <= '0';
                    o_state_end <= '0';
                    o_MEM_ADDR  <= "0000000000000000";
                    o_data      <= "00000000";
            end case;
       	end if;
   	end process;
end multiplying;
