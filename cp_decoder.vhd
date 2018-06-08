----------------------------------------------------------------------------------
--! @file
--! @author W. Fedorko, UBC and P. Plucinski, SU.
--! 
--! @date    December 2 2014
--!
--! @brief CMX data decoder based on sort, based on the jet decoder code.
--! This logic implements a Bacher odd-even merge sort and takes upper 30 positions
--! of the result to be loaded onto the Topo encoder and TX.
--!
--!  
--!
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.CONV_STD_LOGIC_VECTOR;
Library work;
use work.CMXpackage.all;
use work.CMX_flavor_package.all;
use work.CMX_VME_defs.all;


library UNISIM;
use UNISIM.VComponents.all;


entity decoder is
  
  port(
    clk40MHz         : in  std_logic;                                               -- clk40MHz clock
    clk40MHz_m90o    : in  std_logic;
    clk40MHz_90o     : in  std_logic;
    clk40MHz_m180o   : in  std_logic;
    pll_locked       : in  std_logic;                                               -- reset
    datai            : in  arr_4Xword(max_cps-1 downto 0);                         -- input data
    datai_first_half : in  arr_2Xword(max_cps-1 downto 0); 
    Tobs_to_TOPO     : out copy_arr_TOB;       -- TOB arrays to load onto
                                               -- encoder; copied x4
    overflow         : out std_logic_vector(num_copies-1 downto 0);
    BCID_in          : in std_logic_vector(11 downto 0);
    BCID_delayed     : out std_logic_vector(11 downto 0); --BCID id delayed to
                                                          --match the latency
                                                          --of the processing
    --tob rate counter contol
    counter_inhibit          : in  std_logic;
    counter_reset            : in  std_logic;
    --VME control:
    ncs           : in    std_logic;
    rd_nwr        : in    std_logic;
    ds            : in    std_logic;
    addr_vme      : in    std_logic_vector (15 downto 0);
    data_vme_out  : out   std_logic_vector (15 downto 0);
    bus_drive     : out   std_logic);  --overflow copied x4
  
end decoder;

architecture RTL of decoder is


  signal data_vme_out_local : arr_16((4*max_cps)+4 + (max_cps*num_presence_bits_pcp)*2-1 downto 0);
  signal bus_drive_local : std_logic_vector((4*max_cps)+4 + (max_cps*num_presence_bits_pcp)*2-1  downto 0);

  component vme_local_switch is
    port (
      data_vme_up          : out std_logic_vector (15 downto 0);
      data_vme_from_below  : in  arr_16;
      bus_drive_up         : out std_logic;
      bus_drive_from_below : in  std_logic_vector);
  end component vme_local_switch;
  
  component vme_outreg_notri_async is
    generic (
      ia_vme : integer;
      width  : integer);
    port (
      ncs         : in  std_logic;
      rd_nwr      : in  std_logic;
      ds          : in  std_logic;
      addr_vme    : in  std_logic_vector (15 downto 0);
      data_vme    : out std_logic_vector (15 downto 0);
      bus_drive   : out std_logic;
      data_to_vme : in  std_logic_vector (width-1 downto 0));
  end component vme_outreg_notri_async;
  

  component compExch is
    port (
      A, B : in EmTauTOB;
      H, L : out EmTauTOB);
  end component compExch;

--
-- RoI Position
--
  component roiposA
    PORT (
      clka  : in std_logic;
      addra : in std_logic_vector(7 downto 0);
      douta : out std_logic_vector(23 downto 0)
      );
  end component;

  component roiposB
    PORT (
      clka  : in std_logic;
      addra : in std_logic_vector(7 downto 0);
      douta : out std_logic_vector(23 downto 0)
      );
  end component;


  component or_all is
    generic (
      numbits : integer);
    port (
      DATA   : in  std_logic_vector(numbits - 1 downto 0);
      or_all : out std_logic);
  end component or_all;

--
-- Decoder signals
--
  
  signal roi_posA_full        : pos_type_ext;  
  signal roi_posA             : pos_type;
  signal roi_posB_full        : pos_type_ext;  
  signal roi_posB             : pos_type;

  --'collapsed position'
  signal roi_pos             : pos_type;

  --signal overflw,
  --  overflw_del0               : std_logic_vector(max_cps-1 downto 0);

  signal overflow_local           : std_logic_vector(max_cps-1 downto 0);
  signal or_all_ov_local : std_logic;
  signal overflow_local_any : std_logic;
  signal overflow_local_any_r,overflow_local_any_rr : std_logic;
  signal overflow_sig : std_logic_vector(num_copies-1 downto 0);
  signal CLE_overflowing_TOB : arr_8(num_copies-1 downto 0);  --energy of the
                                                              --overflowing cluster
  

  signal ntobsA : num_tobs_half;
  signal ntobsB : num_tobs_half;
  signal ntobs  : num_tobs;
  
  signal TOBs_input : arr_TOB(max_tobs_tot-1 downto 0);
  --signal TOBs_output : arr_TOB(max_tobs_tot-1 downto 0);

  
  signal TobLayerIn : type_TOB_sort_layers;
  signal TobLayerOut : type_TOB_sort_layers;

  --component chipscope_ila_cp_decoder is
  --  port (
  --    CONTROL : inout std_logic_vector(35 downto 0);
  --    CLK     : in    std_logic;
  --    DATA    : in    std_logic_vector(377 downto 0);
  --    TRIG0   : in    std_logic_vector(0 to 0));
  --end component chipscope_ila_cp_decoder;
  --
  --signal CONTROL : std_logic_vector(35 downto 0);
  --signal DATA_ila_cp_decoder    : std_logic_vector(377 downto 0);
  --signal TRIG0_ila_cp_decoder   : std_logic_vector(0 to 0);
  --
  --component chipscope_icon_u2_c1 is
  --  port (
  --    CONTROL0 : inout std_logic_vector(35 downto 0));
  --end component chipscope_icon_u2_c1;

  signal ntobs_counter : arr_ctr_33bit(max_cps-1 downto 0);
  signal presence_bit_counter : arr_ctr_32bit(max_cps*16 -1 downto 0); --16 presence bits/CPM
                                                                       
  signal local_backplane_overflow_counter : arr_ctr_33bit(max_cps-1 downto 0);

  signal global_backplane_overflow_counter : unsigned(32 downto 0);
  signal total_overflow_counter : unsigned(32 downto 0);

  signal counter_inhibit_r_local : std_logic;
  signal counter_reset_r_local : std_logic;

  signal all_one_thirtythree : std_logic_vector(32 downto 0);
  
  signal BCID_r, BCID_rr, BCID_rrr : std_logic_vector(11 downto 0);

  --component chipscope_icon_u1_c14 is
  --  port (
  --    CONTROL0  : inout std_logic_vector(35 downto 0);
  --    CONTROL1  : inout std_logic_vector(35 downto 0);
  --    CONTROL2  : inout std_logic_vector(35 downto 0);
  --    CONTROL3  : inout std_logic_vector(35 downto 0);
  --    CONTROL4  : inout std_logic_vector(35 downto 0);
  --    CONTROL5  : inout std_logic_vector(35 downto 0);
  --    CONTROL6  : inout std_logic_vector(35 downto 0);
  --    CONTROL7  : inout std_logic_vector(35 downto 0);
  --    CONTROL8  : inout std_logic_vector(35 downto 0);
  --    CONTROL9  : inout std_logic_vector(35 downto 0);
  --    CONTROL10 : inout std_logic_vector(35 downto 0);
  --    CONTROL11 : inout std_logic_vector(35 downto 0);
  --    CONTROL12 : inout std_logic_vector(35 downto 0);
  --    CONTROL13 : inout std_logic_vector(35 downto 0));
  --end component chipscope_icon_u1_c14;
  --
  --signal CONTROL_bus  : arr_36(13 downto 0);
  --
  --component chipscope_ila_presence_bits is
  --  port (
  --    CONTROL  : inout std_logic_vector(35 downto 0);
  --    CLK      : in    std_logic;
  --    TRIG0    : in    std_logic_vector(15 downto 0);
  --    TRIG_OUT : out   std_logic);
  --end component chipscope_ila_presence_bits;
  --
  --
  --signal TRIG0_ila_presence_bits    : arr_16(6 downto 0);
  --signal TRIG_OUT_ila_presence_bits : std_logic_vector(6 downto 0);
  --
  --component chipscope_ila_TOB_data is
  --  port (
  --    CONTROL : inout std_logic_vector(35 downto 0);
  --    CLK     : in    std_logic;
  --    DATA    : in    std_logic_vector(170 downto 0);
  --    TRIG0   : in    std_logic_vector(16 downto 0));
  --end component chipscope_ila_TOB_data;
  --
  --
  --signal DATA_TOB_data    : arr_171(6 downto 0);
  --signal TRIG0_TOB_data   : arr_17(6 downto 0);
  
begin

  all_one_thirtythree<=(others=>'1');


  --chipscope_icon_u1_c14_inst: entity work.chipscope_icon_u1_c14
  --  port map (
  --    CONTROL0  => CONTROL_bus(0),
  --    CONTROL1  => CONTROL_bus(1),
  --    CONTROL2  => CONTROL_bus(2),
  --    CONTROL3  => CONTROL_bus(3),
  --    CONTROL4  => CONTROL_bus(4),
  --    CONTROL5  => CONTROL_bus(5),
  --    CONTROL6  => CONTROL_bus(6),
  --    CONTROL7  => CONTROL_bus(7),
  --    CONTROL8  => CONTROL_bus(8),
  --    CONTROL9  => CONTROL_bus(9),
  --    CONTROL10 => CONTROL_bus(10),
  --    CONTROL11 => CONTROL_bus(11),
  --    CONTROL12 => CONTROL_bus(12),
  --    CONTROL13 => CONTROL_bus(13));
  --
  --
  --cs_gen: for i_cs in 0 to 6 generate
  --
  --  constant i_cp :integer := i_cs*2+1;
  --
  --begin
  --  
  --  chipscope_ila_presence_bits_inst: entity work.chipscope_ila_presence_bits
  --    port map (
  --      CONTROL  => CONTROL_bus(i_cs*2),
  --      CLK      => clk40MHz_m90o,
  --      TRIG0    => TRIG0_ila_presence_bits(i_cs),
  --      TRIG_OUT => TRIG_OUT_ila_presence_bits(i_cs));
  --
  --  chipscope_ila_TOB_data_inst: entity work.chipscope_ila_TOB_data
  --    port map (
  --      CONTROL => CONTROL_bus(i_cs*2+1),
  --      CLK     => clk40MHz_90o,
  --      DATA    => DATA_TOB_data(i_cs),
  --      TRIG0   => TRIG0_TOB_data(i_cs));
  --
  --  TRIG0_ila_presence_bits(i_cs)<=datai_first_half(i_cp)(15 downto 0);
  --
  --  TRIG0_TOB_data(i_cs)(15 downto 0)<=datai(i_cp)(15 downto 0);
  --  TRIG0_TOB_data(i_cs)(16)<=TRIG_OUT_ila_presence_bits(i_cs);
  --
  --  DATA_TOB_data(i_cs)(15 downto 0)<=datai(i_cp)(15 downto 0);
  --  DATA_TOB_data(i_cs)(16)<=TRIG_OUT_ila_presence_bits(i_cs);
  --  DATA_TOB_data(i_cs)(36 downto 17)<=roi_posA(i_cp);
  --  DATA_TOB_data(i_cs)(56 downto 37)<=roi_posB(i_cp);
  --  DATA_TOB_data(i_cs)(60 downto 57)<=ntobsA(i_cp);
  --  DATA_TOB_data(i_cs)(80 downto 61)<=roi_pos(i_cp);
  --  DATA_TOB_data(i_cs)(90 downto 81)  <=TOBs_input(i_cp*max_tobs_pcp+0).POS;
  --  DATA_TOB_data(i_cs)(100 downto 91) <=TOBs_input(i_cp*max_tobs_pcp+1).POS;
  --  DATA_TOB_data(i_cs)(110 downto 101)<=TOBs_input(i_cp*max_tobs_pcp+2).POS;
  --  DATA_TOB_data(i_cs)(120 downto 111)<=TOBs_input(i_cp*max_tobs_pcp+3).POS; 
  --  DATA_TOB_data(i_cs)(130 downto 121)<=TOBs_input(i_cp*max_tobs_pcp+4).POS;
  --  DATA_TOB_data(i_cs)(138 downto 131)  <=TOBs_input(i_cp*max_tobs_pcp+0).CLE;
  --  DATA_TOB_data(i_cs)(146 downto 139) <=TOBs_input(i_cp*max_tobs_pcp+1).CLE;
  --  DATA_TOB_data(i_cs)(154 downto 147)<=TOBs_input(i_cp*max_tobs_pcp+2).CLE;
  --  DATA_TOB_data(i_cs)(162 downto 155)<=TOBs_input(i_cp*max_tobs_pcp+3).CLE; 
  --  DATA_TOB_data(i_cs)(170 downto 163)<=TOBs_input(i_cp*max_tobs_pcp+4).CLE;
  --  
  --  
  --end generate cs_gen;

  vme_local_switch_inst: entity work.vme_local_switch
    port map (
      data_vme_up          => data_vme_out,
      data_vme_from_below  => data_vme_out_local,
      bus_drive_up         => bus_drive,
      bus_drive_from_below => bus_drive_local);
  
  process(clk40MHz)
  begin
    if rising_edge(clk40MHz) then
      counter_inhibit_r_local<=counter_inhibit;
      counter_reset_r_local<=counter_reset;
    end if;
  end process;


  data_parser_0: for i_cp in 0 to (max_cps-1) generate
    data_parser_1: for j_tob in 0 to (max_tobs_pcp-1) generate

      process(clk40MHz_90o)
      begin
        if rising_edge(clk40MHz_90o) then
          TOBs_input(i_cp*max_tobs_pcp+j_tob).CLE<=datai(i_cp)(arr_addr_CLE_Lo(j_tob) + 7 downto arr_addr_CLE_Lo(j_tob));
          TOBs_input(i_cp*max_tobs_pcp+j_tob).ISO<=datai(i_cp)(arr_addr_ISO_Lo(j_tob)+4 downto arr_addr_ISO_Lo(j_tob));
          TOBs_input(i_cp*max_tobs_pcp+j_tob).POS(1 downto 0)<=datai(i_cp)(arr_addr_POS_Lo(j_tob)+1 downto arr_addr_POS_Lo(j_tob));
          TOBs_input(i_cp*max_tobs_pcp+j_tob).POS(5 downto 2)<=roi_pos(i_cp)(3+j_tob*4 downto j_tob*4);          
        end if;
      end process;

      TOBs_input(i_cp*max_tobs_pcp+j_tob).POS(9 downto 6)<=std_logic_vector(to_unsigned(i_cp+1,4));

    end generate data_parser_1;
  end generate data_parser_0;


  roipos_gen: for i in 0 to (max_cps-1) generate

    roipos_A: roiposA
      port map (
        clka  => clk40MHz_m90o,
        addra => datai_first_half(i)(7 downto 0),
        douta => roi_posA_full(i));
    
    roipos_B: roiposB
      port map (
        clka  => clk40MHz_m90o,
        addra => datai_first_half(i)(15 downto 8),
        douta => roi_posB_full(i));
    
    roi_posA(i)<=roi_posA_full(i)(19 downto 0);
    roi_posB(i)<=roi_posB_full(i)(19 downto 0);

    

    ntobsA(i)<=roi_posA_full(i)(23 downto 20);
    ntobsB(i)<=roi_posB_full(i)(23 downto 20);

    ntobs(i)<=resize(unsigned(ntobsA(i)),5) + resize(unsigned(ntobsB(i)),5) ;

    with ntobsA(i) select roi_pos(i) <=
      roi_posB(i) when "0000",
      (roi_posB(i)(15 downto 0) & roi_posA(i)(3 downto 0)) when "0001",
      (roi_posB(i)(11 downto 0) & roi_posA(i)(7 downto 0)) when "0010",
      (roi_posB(i)(7 downto 0) & roi_posA(i)(11 downto 0)) when "0011",
      (roi_posB(i)(3 downto 0) & roi_posA(i)(15 downto 0)) when "0100",
      roi_posA(i) when others;  -- five or more in first half


    process(clk40MHz)
    begin
      if rising_edge(clk40MHz) then
        if counter_reset_r_local='1' then
          ntobs_counter(i)<=to_unsigned(0,33);
        else
          if ntobs_counter(i)(32)='1' then
            ntobs_counter(i)<=unsigned(all_one_thirtythree);
          else
            if counter_inhibit_r_local /= '1' then
              ntobs_counter(i)<=ntobs_counter(i)+unsigned(ntobs(i));
            else
              ntobs_counter(i)<=ntobs_counter(i);
            end if;
          end if;
        end if;
      end if;
    end process;

    vme_outreg_notri_async_REG_RO_TOB_COUNTER_0: entity work.vme_outreg_notri_async
      generic map (
        ia_vme => ADDR_REG_RO_TOB_COUNTER+4*i,
        width  => 16)
      port map (
        ncs         => ncs,
        rd_nwr      => rd_nwr,
        ds          => ds,
        addr_vme    => addr_vme,
        data_vme    => data_vme_out_local(2*i),
        bus_drive   => bus_drive_local(2*i),
        data_to_vme => std_logic_vector(ntobs_counter(i)(15 downto 0)));

    vme_outreg_notri_async_REG_RO_TOB_COUNTER_1: entity work.vme_outreg_notri_async
      generic map (
        ia_vme => ADDR_REG_RO_TOB_COUNTER+4*i+2,
        width  => 16)
      port map (
        ncs         => ncs,
        rd_nwr      => rd_nwr,
        ds          => ds,
        addr_vme    => addr_vme,
        data_vme    => data_vme_out_local(2*i+1),
        bus_drive   => bus_drive_local(2*i+1),
        data_to_vme => std_logic_vector(ntobs_counter(i)(31 downto 16)));


    
    gen_presence_counter: for i_pres_bit in 0 to (num_presence_bits_pcp-1) generate
      constant i_counter : integer:=i*num_presence_bits_pcp+i_pres_bit;
    begin
    
      process(clk40MHz)
      begin
        if rising_edge(clk40MHz) then
          if counter_reset_r_local='1' then
            presence_bit_counter(i_counter)<=to_unsigned(0,32);
          else
            if counter_inhibit_r_local/='1' and presence_bit_counter(i_counter)/=unsigned(all_one_thirtythree(31 downto 0)) and datai(i)(i_pres_bit)='1' then
              presence_bit_counter(i_counter)<=presence_bit_counter(i_counter)+1;
            else
              presence_bit_counter(i_counter)<=presence_bit_counter(i_counter);
            end if;
          end if;
        end if;
      end process;

      
    vme_outreg_notri_async_REG_RO_PRESENCE_COUNTER_0: entity work.vme_outreg_notri_async
      generic map (
        ia_vme => ADDR_REG_RO_PRESENCE_COUNTER+4*i_counter,
        width  => 16)
      port map (
        ncs         => ncs,
        rd_nwr      => rd_nwr,
        ds          => ds,
        addr_vme    => addr_vme,
        data_vme    => data_vme_out_local((4*max_cps)+4 + i_counter*2),
        bus_drive   => bus_drive_local((4*max_cps)+4 + i_counter*2),
        data_to_vme => std_logic_vector(presence_bit_counter(i_counter)(15 downto 0)));

    vme_outreg_notri_async_REG_RO_PRESENCE_COUNTER_1: entity work.vme_outreg_notri_async
      generic map (
        ia_vme => ADDR_REG_RO_PRESENCE_COUNTER+4*i_counter+2,
        width  => 16)
      port map (
        ncs         => ncs,
        rd_nwr      => rd_nwr,
        ds          => ds,
        addr_vme    => addr_vme,
        data_vme    => data_vme_out_local((4*max_cps)+4 + i_counter*2+1),
        bus_drive   => bus_drive_local((4*max_cps)+4 + i_counter*2+1),
        data_to_vme => std_logic_vector(presence_bit_counter(i_counter)(31 downto 16)));

    end generate gen_presence_counter;
    
  end generate roipos_gen;

  

  ov_local_gen: for i_cp in 0 to (max_cps-1) generate
    
    process(clk40MHz_m90o)
    begin
      if rising_edge(clk40MHz_m90o) then
        if unsigned(ntobs(i_cp))>to_unsigned(5,5) then
          overflow_local(i_cp)<='1';
        else
          overflow_local(i_cp)<='0';
        end if;
      end if;
    end process;

    
    process(clk40MHz)
    begin
      if rising_edge(clk40MHz) then
        if counter_reset_r_local='1' then
          local_backplane_overflow_counter(i_cp)<=to_unsigned(0,33);
        else
          if local_backplane_overflow_counter(i_cp)(32)='1' then
            local_backplane_overflow_counter(i_cp)<=unsigned(all_one_thirtythree);
          else
            if counter_inhibit_r_local /= '1' and overflow_local(i_cp)='1' then
              local_backplane_overflow_counter(i_cp)<=local_backplane_overflow_counter(i_cp)+1;
            else
              local_backplane_overflow_counter(i_cp)<=local_backplane_overflow_counter(i_cp);
            end if;
          end if;
        end if;
      end if;
    end process;


    vme_outreg_notri_async_REG_RO_LOCAL_BACKPLANE_OVERFLOW_COUNTER_0: entity work.vme_outreg_notri_async
      generic map (
        ia_vme => ADDR_REG_RO_LOCAL_BACKPLANE_OVERFLOW_COUNTER+4*i_cp,
        width  => 16)
      port map (
        ncs         => ncs,
        rd_nwr      => rd_nwr,
        ds          => ds,
        addr_vme    => addr_vme,
        data_vme    => data_vme_out_local((2*max_cps)+2*i_cp),
        bus_drive   => bus_drive_local((2*max_cps)+2*i_cp),
        data_to_vme => std_logic_vector(local_backplane_overflow_counter(i_cp)(15 downto 0)));

    vme_outreg_notri_async_REG_RO_LOCAL_BACKPLANE_OVERFLOW_COUNTER_1: entity work.vme_outreg_notri_async
      generic map (
        ia_vme => ADDR_REG_RO_LOCAL_BACKPLANE_OVERFLOW_COUNTER+4*i_cp+2,
        width  => 16)
      port map (
        ncs         => ncs,
        rd_nwr      => rd_nwr,
        ds          => ds,
        addr_vme    => addr_vme,
        data_vme    => data_vme_out_local((2*max_cps)+2*i_cp+1),
        bus_drive   => bus_drive_local((2*max_cps)+2*i_cp+1),
        data_to_vme => std_logic_vector(local_backplane_overflow_counter(i_cp)(31 downto 16)));
        
  end generate ov_local_gen;


  or_all_ov_local_inst: entity work.or_all
    generic map (
      numbits => max_cps)
    port map (
      DATA   => overflow_local,
      or_all => or_all_ov_local);
  
  process(clk40MHz_90o)
  begin
    if rising_edge(clk40MHz_90o) then
      overflow_local_any<=or_all_ov_local;
    end if;
  end process;

  process(clk40MHz_m180o)
  begin
    if rising_edge(clk40MHz_m180o) then
      overflow_local_any_rr<=overflow_local_any_r;
      overflow_local_any_r<=overflow_local_any;
    end if;
  end process;


  
  process(clk40MHz)
  begin
    if rising_edge(clk40MHz) then
      if counter_reset_r_local='1' then
        global_backplane_overflow_counter<=to_unsigned(0,33);
      else
        if global_backplane_overflow_counter(32)='1' then
          global_backplane_overflow_counter<=unsigned(all_one_thirtythree);
        else
          if counter_inhibit_r_local /= '1' and overflow_local_any_rr='1' then
            global_backplane_overflow_counter<=global_backplane_overflow_counter+1;
          else
            global_backplane_overflow_counter<=global_backplane_overflow_counter;
          end if;
        end if;
      end if;
    end if;
  end process;

  vme_outreg_notri_async_REG_RO_GLOBAL_BACKPLANE_OVERFLOW_COUNTER_0: entity work.vme_outreg_notri_async
    generic map (
      ia_vme => ADDR_REG_RO_GLOBAL_BACKPLANE_OVERFLOW_COUNTER,
      width  => 16)
    port map (
      ncs         => ncs,
      rd_nwr      => rd_nwr,
      ds          => ds,
      addr_vme    => addr_vme,
      data_vme    => data_vme_out_local((4*max_cps)),
      bus_drive   => bus_drive_local((4*max_cps)),
      data_to_vme => std_logic_vector(global_backplane_overflow_counter(15 downto 0)));
  
  vme_outreg_notri_async_REG_RO_GLOBAL_BACKPLANE_OVERFLOW_COUNTER_1: entity work.vme_outreg_notri_async
    generic map (
      ia_vme => ADDR_REG_RO_GLOBAL_BACKPLANE_OVERFLOW_COUNTER+2,
      width  => 16)
    port map (
      ncs         => ncs,
      rd_nwr      => rd_nwr,
      ds          => ds,
      addr_vme    => addr_vme,
      data_vme    => data_vme_out_local((4*max_cps)+1),
      bus_drive   => bus_drive_local((4*max_cps)+1),
      data_to_vme => std_logic_vector(global_backplane_overflow_counter(31 downto 16)));
    

  
  TobLayerIn(0)<=TOBs_input;

  gen_layers: for i_layer in 0 to num_sort_layers-2 generate

    gen_layer_connection_no_reg: if (
      (i_layer/=1) and
      (i_layer/=3) and
      (i_layer/=5) and
      (i_layer/=7) and
      (i_layer/=9) and
      (i_layer/=11) and
      (i_layer/=13) and
      (i_layer/=15) and
      (i_layer/=17) and
      (i_layer/=19) and
      (i_layer/=21) and
      (i_layer/=23) and
      (i_layer/=25)
      ) generate
      TobLayerIn(i_layer+1)<=TobLayerOut(i_layer);
    end generate gen_layer_connection_no_reg;
    
    gen_layer_connection_L1_to_L2_reg: if i_layer=1 generate
      process(clk40MHz_m180o)
      begin
        if rising_edge(clk40MHz_m180o) then
          TobLayerIn(i_layer+1)<=TobLayerOut(i_layer);
        end if;
      end process;
    end generate gen_layer_connection_L1_to_L2_reg;

    gen_layer_connection_L3_to_L4_reg: if i_layer=3 generate
      process(clk40MHz_m90o)
      begin
        if rising_edge(clk40MHz_m90o) then
          TobLayerIn(i_layer+1)<=TobLayerOut(i_layer);
        end if;
      end process;
    end generate gen_layer_connection_L3_to_L4_reg;

    gen_layer_connection_L5_to_L6_reg: if i_layer=5 generate
      process(clk40MHz)
      begin
        if rising_edge(clk40MHz) then
          TobLayerIn(i_layer+1)<=TobLayerOut(i_layer);
        end if;
      end process;
    end generate gen_layer_connection_L5_to_L6_reg;
    
    gen_layer_connection_L7_to_L8_reg: if i_layer=7 generate
      process(clk40MHz_90o)
      begin
        if rising_edge(clk40MHz_90o) then
          TobLayerIn(i_layer+1)<=TobLayerOut(i_layer);
        end if;
      end process;
    end generate gen_layer_connection_L7_to_L8_reg;

    gen_layer_connection_L9_to_L10_reg: if i_layer=9 generate
      process(clk40MHz_m180o)
      begin
        if rising_edge(clk40MHz_m180o) then
          TobLayerIn(i_layer+1)<=TobLayerOut(i_layer);
        end if;
      end process;
    end generate gen_layer_connection_L9_to_L10_reg;

    gen_layer_connection_L11_to_L12_reg: if i_layer=11 generate
      process(clk40MHz_m90o)
      begin
        if rising_edge(clk40MHz_m90o) then
          TobLayerIn(i_layer+1)<=TobLayerOut(i_layer);
        end if;
      end process;
    end generate gen_layer_connection_L11_to_L12_reg;

    gen_layer_connection_L13_to_L14_reg: if i_layer=13 generate
      process(clk40MHz)
      begin
        if rising_edge(clk40MHz) then
          TobLayerIn(i_layer+1)<=TobLayerOut(i_layer);
        end if;
      end process;
    end generate gen_layer_connection_L13_to_L14_reg;

    gen_layer_connection_L15_to_L16_reg: if i_layer=15 generate
      process(clk40MHz_90o)
      begin
        if rising_edge(clk40MHz_90o) then
          TobLayerIn(i_layer+1)<=TobLayerOut(i_layer);
        end if;
      end process;
    end generate gen_layer_connection_L15_to_L16_reg;

    gen_layer_connection_L17_to_L18_reg: if i_layer=17 generate
      process(clk40MHz_m180o)
      begin
        if rising_edge(clk40MHz_m180o) then
          TobLayerIn(i_layer+1)<=TobLayerOut(i_layer);
        end if;
      end process;
    end generate gen_layer_connection_L17_to_L18_reg;

    gen_layer_connection_L19_to_L20_reg: if i_layer=19 generate
      process(clk40MHz_m90o)
      begin
        if rising_edge(clk40MHz_m90o) then
          TobLayerIn(i_layer+1)<=TobLayerOut(i_layer);
        end if;
      end process;
    end generate gen_layer_connection_L19_to_L20_reg;

    gen_layer_connection_L21_to_L22_reg: if i_layer=21 generate
      process(clk40MHz)
      begin
        if rising_edge(clk40MHz) then
          TobLayerIn(i_layer+1)<=TobLayerOut(i_layer);
        end if;
      end process;
    end generate gen_layer_connection_L21_to_L22_reg;

    gen_layer_connection_L23_to_L24_reg: if i_layer=23 generate
      process(clk40MHz_90o)
      begin
        if rising_edge(clk40MHz_90o) then
          TobLayerIn(i_layer+1)<=TobLayerOut(i_layer);
        end if;
      end process;
    end generate gen_layer_connection_L23_to_L24_reg;

    
    gen_layer_connection_L25_to_L26_reg: if i_layer=25 generate
      process(clk40MHz_m180o)
      begin
        if rising_edge(clk40MHz_m180o) then
          TobLayerIn(i_layer+1)<=TobLayerOut(i_layer);
        end if;
      end process;
    end generate gen_layer_connection_L25_to_L26_reg;

    
  end generate gen_layers;

  process(clk40MHz_m90o)
  begin
    if rising_edge(clk40MHz_m90o) then
      BCID_delayed<=BCID_rrr;
      BCID_rrr<=BCID_rr;
      BCID_rr<=BCID_r;
      BCID_r<=BCID_in;
    end if;
  end process;

  --duplicate registers to ease timing.
  output_copy_gen: for i_copy in 0 to num_copies-1 generate
    process(clk40MHz_m90o)
    begin
      if rising_edge(clk40MHz_m90o) then
        Tobs_to_TOPO(i_copy)<=TobLayerOut(27)(max_tobs_topo-1 downto 0);
        CLE_overflowing_TOB(i_copy)<=TobLayerOut(27)(max_tobs_topo).CLE;
      end if;
    end process;
  end generate output_copy_gen;

  
  overflow_copy_gen: for i_copy in 0 to num_copies-1 generate
    process(clk40MHz)
    begin
      if rising_edge(clk40MHz) then
        if CLE_overflowing_TOB(i_copy) /= "00000000" or overflow_local_any_rr/='0' then
          overflow_sig(i_copy)<='1';
        else
          overflow_sig(i_copy)<='0';
        end if;
      end if;
    end process;
  end generate overflow_copy_gen;
  overflow<=overflow_sig;


  process(clk40MHz)
  begin
    if rising_edge(clk40MHz) then
      if counter_reset_r_local='1' then
        total_overflow_counter<=to_unsigned(0,33);
      else
        if total_overflow_counter(32)='1' then
          total_overflow_counter<=unsigned(all_one_thirtythree);
        else
          if counter_inhibit_r_local /= '1' and overflow_sig(0)='1' then
            total_overflow_counter<=total_overflow_counter+1;
          else
            total_overflow_counter<=total_overflow_counter;
          end if;
        end if;
      end if;
    end if;
  end process;

  vme_outreg_notri_async_REG_RO_TOTAL_OVERFLOW_COUNTER_0: entity work.vme_outreg_notri_async
    generic map (
      ia_vme => ADDR_REG_RO_TOTAL_OVERFLOW_COUNTER,
      width  => 16)
    port map (
      ncs         => ncs,
      rd_nwr      => rd_nwr,
      ds          => ds,
      addr_vme    => addr_vme,
      data_vme    => data_vme_out_local((4*max_cps)+2),
      bus_drive   => bus_drive_local((4*max_cps)+2),
      data_to_vme => std_logic_vector(total_overflow_counter(15 downto 0)));
  
  vme_outreg_notri_async_REG_RO_TOTAL_OVERFLOW_COUNTER_1: entity work.vme_outreg_notri_async
    generic map (
      ia_vme => ADDR_REG_RO_TOTAL_OVERFLOW_COUNTER+2,
      width  => 16)
    port map (
      ncs         => ncs,
      rd_nwr      => rd_nwr,
      ds          => ds,
      addr_vme    => addr_vme,
      data_vme    => data_vme_out_local((4*max_cps)+3),
      bus_drive   => bus_drive_local((4*max_cps)+3),
      data_to_vme => std_logic_vector(total_overflow_counter(31 downto 16)));
    
  

  --chipscope_icon_u2_c1_inst: entity work.chipscope_icon_u2_c1
  --  port map (
  --    CONTROL0 => CONTROL);
  --
  --
  --chipscope_ila_cp_decoder_inst: entity work.chipscope_ila_cp_decoder
  --  port map (
  --    CONTROL => CONTROL,
  --    CLK     => clk40MHz,
  --    DATA    => DATA_ila_cp_decoder,
  --    TRIG0   => TRIG0_ila_cp_decoder);
  --
  --
  --TRIG0_ila_cp_decoder(0)<=overflow_local_any;
  --
  --
  --DATA_ila_cp_decoder(95 downto 0)<=datai(0);
  --DATA_ila_cp_decoder(191 downto 96)<=datai(1);
  --
  --DATA_ila_cp_decoder(195 downto 192)<=ntobs(0);       
  --DATA_ila_cp_decoder(199 downto 196)<=ntobs(1);
  --
  --DATA_ila_cp_decoder(215 downto 200)<=overflow_local;
  --
  --DATA_ila_cp_decoder(216)<=overflow_local_any;
  --
  --DATA_ila_cp_decoder(217)<='0';
  --
  --cs_tob_gen: for i_tob in 0 to 15 generate
  --  DATA_ila_cp_decoder(218 + (10*(i_tob+1)-1) downto 218 + 10*i_tob)<=TOBs_output(i_tob).Et2;
  --end generate cs_tob_gen;
  --  
  
  --paste from auto code generation

  compExch_Layer_00_to_01_sites_00_01: compExch port map(A=>TobLayerIn(0)(0), B=>TobLayerIn(0)(1), H=>TobLayerOut(0)(0), L=>TobLayerOut(0)(1));


  compExch_Layer_00_to_01_sites_02_03: compExch port map(A=>TobLayerIn(0)(2), B=>TobLayerIn(0)(3), H=>TobLayerOut(0)(2), L=>TobLayerOut(0)(3));


  compExch_Layer_00_to_01_sites_04_05: compExch port map(A=>TobLayerIn(0)(4), B=>TobLayerIn(0)(5), H=>TobLayerOut(0)(4), L=>TobLayerOut(0)(5));


  compExch_Layer_00_to_01_sites_06_07: compExch port map(A=>TobLayerIn(0)(6), B=>TobLayerIn(0)(7), H=>TobLayerOut(0)(6), L=>TobLayerOut(0)(7));


  compExch_Layer_00_to_01_sites_08_09: compExch port map(A=>TobLayerIn(0)(8), B=>TobLayerIn(0)(9), H=>TobLayerOut(0)(8), L=>TobLayerOut(0)(9));


  compExch_Layer_00_to_01_sites_10_11: compExch port map(A=>TobLayerIn(0)(10), B=>TobLayerIn(0)(11), H=>TobLayerOut(0)(10), L=>TobLayerOut(0)(11));


  compExch_Layer_00_to_01_sites_12_13: compExch port map(A=>TobLayerIn(0)(12), B=>TobLayerIn(0)(13), H=>TobLayerOut(0)(12), L=>TobLayerOut(0)(13));


  compExch_Layer_00_to_01_sites_14_15: compExch port map(A=>TobLayerIn(0)(14), B=>TobLayerIn(0)(15), H=>TobLayerOut(0)(14), L=>TobLayerOut(0)(15));


  compExch_Layer_00_to_01_sites_16_17: compExch port map(A=>TobLayerIn(0)(16), B=>TobLayerIn(0)(17), H=>TobLayerOut(0)(16), L=>TobLayerOut(0)(17));


  compExch_Layer_00_to_01_sites_18_19: compExch port map(A=>TobLayerIn(0)(18), B=>TobLayerIn(0)(19), H=>TobLayerOut(0)(18), L=>TobLayerOut(0)(19));


  compExch_Layer_00_to_01_sites_20_21: compExch port map(A=>TobLayerIn(0)(20), B=>TobLayerIn(0)(21), H=>TobLayerOut(0)(20), L=>TobLayerOut(0)(21));


  compExch_Layer_00_to_01_sites_22_23: compExch port map(A=>TobLayerIn(0)(22), B=>TobLayerIn(0)(23), H=>TobLayerOut(0)(22), L=>TobLayerOut(0)(23));


  compExch_Layer_00_to_01_sites_24_25: compExch port map(A=>TobLayerIn(0)(24), B=>TobLayerIn(0)(25), H=>TobLayerOut(0)(24), L=>TobLayerOut(0)(25));


  compExch_Layer_00_to_01_sites_26_27: compExch port map(A=>TobLayerIn(0)(26), B=>TobLayerIn(0)(27), H=>TobLayerOut(0)(26), L=>TobLayerOut(0)(27));


  compExch_Layer_00_to_01_sites_28_29: compExch port map(A=>TobLayerIn(0)(28), B=>TobLayerIn(0)(29), H=>TobLayerOut(0)(28), L=>TobLayerOut(0)(29));


  compExch_Layer_00_to_01_sites_30_31: compExch port map(A=>TobLayerIn(0)(30), B=>TobLayerIn(0)(31), H=>TobLayerOut(0)(30), L=>TobLayerOut(0)(31));


  compExch_Layer_00_to_01_sites_32_33: compExch port map(A=>TobLayerIn(0)(32), B=>TobLayerIn(0)(33), H=>TobLayerOut(0)(32), L=>TobLayerOut(0)(33));


  compExch_Layer_00_to_01_sites_34_35: compExch port map(A=>TobLayerIn(0)(34), B=>TobLayerIn(0)(35), H=>TobLayerOut(0)(34), L=>TobLayerOut(0)(35));


  compExch_Layer_00_to_01_sites_36_37: compExch port map(A=>TobLayerIn(0)(36), B=>TobLayerIn(0)(37), H=>TobLayerOut(0)(36), L=>TobLayerOut(0)(37));


  compExch_Layer_00_to_01_sites_38_39: compExch port map(A=>TobLayerIn(0)(38), B=>TobLayerIn(0)(39), H=>TobLayerOut(0)(38), L=>TobLayerOut(0)(39));


  compExch_Layer_00_to_01_sites_40_41: compExch port map(A=>TobLayerIn(0)(40), B=>TobLayerIn(0)(41), H=>TobLayerOut(0)(40), L=>TobLayerOut(0)(41));


  compExch_Layer_00_to_01_sites_42_43: compExch port map(A=>TobLayerIn(0)(42), B=>TobLayerIn(0)(43), H=>TobLayerOut(0)(42), L=>TobLayerOut(0)(43));


  compExch_Layer_00_to_01_sites_44_45: compExch port map(A=>TobLayerIn(0)(44), B=>TobLayerIn(0)(45), H=>TobLayerOut(0)(44), L=>TobLayerOut(0)(45));


  compExch_Layer_00_to_01_sites_46_47: compExch port map(A=>TobLayerIn(0)(46), B=>TobLayerIn(0)(47), H=>TobLayerOut(0)(46), L=>TobLayerOut(0)(47));


  compExch_Layer_00_to_01_sites_48_49: compExch port map(A=>TobLayerIn(0)(48), B=>TobLayerIn(0)(49), H=>TobLayerOut(0)(48), L=>TobLayerOut(0)(49));


  compExch_Layer_00_to_01_sites_50_51: compExch port map(A=>TobLayerIn(0)(50), B=>TobLayerIn(0)(51), H=>TobLayerOut(0)(50), L=>TobLayerOut(0)(51));


  compExch_Layer_00_to_01_sites_52_53: compExch port map(A=>TobLayerIn(0)(52), B=>TobLayerIn(0)(53), H=>TobLayerOut(0)(52), L=>TobLayerOut(0)(53));


  compExch_Layer_00_to_01_sites_54_55: compExch port map(A=>TobLayerIn(0)(54), B=>TobLayerIn(0)(55), H=>TobLayerOut(0)(54), L=>TobLayerOut(0)(55));


  compExch_Layer_00_to_01_sites_56_57: compExch port map(A=>TobLayerIn(0)(56), B=>TobLayerIn(0)(57), H=>TobLayerOut(0)(56), L=>TobLayerOut(0)(57));


  compExch_Layer_00_to_01_sites_58_59: compExch port map(A=>TobLayerIn(0)(58), B=>TobLayerIn(0)(59), H=>TobLayerOut(0)(58), L=>TobLayerOut(0)(59));


  compExch_Layer_00_to_01_sites_60_61: compExch port map(A=>TobLayerIn(0)(60), B=>TobLayerIn(0)(61), H=>TobLayerOut(0)(60), L=>TobLayerOut(0)(61));


  compExch_Layer_00_to_01_sites_62_63: compExch port map(A=>TobLayerIn(0)(62), B=>TobLayerIn(0)(63), H=>TobLayerOut(0)(62), L=>TobLayerOut(0)(63));


  compExch_Layer_00_to_01_sites_64_65: compExch port map(A=>TobLayerIn(0)(64), B=>TobLayerIn(0)(65), H=>TobLayerOut(0)(64), L=>TobLayerOut(0)(65));


  compExch_Layer_00_to_01_sites_66_67: compExch port map(A=>TobLayerIn(0)(66), B=>TobLayerIn(0)(67), H=>TobLayerOut(0)(66), L=>TobLayerOut(0)(67));


  compExch_Layer_00_to_01_sites_68_69: compExch port map(A=>TobLayerIn(0)(68), B=>TobLayerIn(0)(69), H=>TobLayerOut(0)(68), L=>TobLayerOut(0)(69));































  compExch_Layer_01_to_02_sites_00_02: compExch port map(A=>TobLayerIn(1)(0), B=>TobLayerIn(1)(2), H=>TobLayerOut(1)(0), L=>TobLayerOut(1)(2));
  compExch_Layer_01_to_02_sites_01_03: compExch port map(A=>TobLayerIn(1)(1), B=>TobLayerIn(1)(3), H=>TobLayerOut(1)(1), L=>TobLayerOut(1)(3));


  compExch_Layer_02_to_03_sites_01_02: compExch port map(A=>TobLayerIn(2)(1), B=>TobLayerIn(2)(2), H=>TobLayerOut(2)(1), L=>TobLayerOut(2)(2));
  TobLayerOut(2)(0)<=TobLayerIn(2)(0);
  TobLayerOut(2)(3)<=TobLayerIn(2)(3);


  compExch_Layer_01_to_02_sites_04_06: compExch port map(A=>TobLayerIn(1)(4), B=>TobLayerIn(1)(6), H=>TobLayerOut(1)(4), L=>TobLayerOut(1)(6));
  compExch_Layer_01_to_02_sites_05_07: compExch port map(A=>TobLayerIn(1)(5), B=>TobLayerIn(1)(7), H=>TobLayerOut(1)(5), L=>TobLayerOut(1)(7));


  compExch_Layer_02_to_03_sites_05_06: compExch port map(A=>TobLayerIn(2)(5), B=>TobLayerIn(2)(6), H=>TobLayerOut(2)(5), L=>TobLayerOut(2)(6));
  TobLayerOut(2)(4)<=TobLayerIn(2)(4);
  TobLayerOut(2)(7)<=TobLayerIn(2)(7);


  compExch_Layer_01_to_02_sites_08_10: compExch port map(A=>TobLayerIn(1)(8), B=>TobLayerIn(1)(10), H=>TobLayerOut(1)(8), L=>TobLayerOut(1)(10));
  compExch_Layer_01_to_02_sites_09_11: compExch port map(A=>TobLayerIn(1)(9), B=>TobLayerIn(1)(11), H=>TobLayerOut(1)(9), L=>TobLayerOut(1)(11));


  compExch_Layer_02_to_03_sites_09_10: compExch port map(A=>TobLayerIn(2)(9), B=>TobLayerIn(2)(10), H=>TobLayerOut(2)(9), L=>TobLayerOut(2)(10));
  TobLayerOut(2)(8)<=TobLayerIn(2)(8);
  TobLayerOut(2)(11)<=TobLayerIn(2)(11);


  compExch_Layer_01_to_02_sites_12_14: compExch port map(A=>TobLayerIn(1)(12), B=>TobLayerIn(1)(14), H=>TobLayerOut(1)(12), L=>TobLayerOut(1)(14));
  compExch_Layer_01_to_02_sites_13_15: compExch port map(A=>TobLayerIn(1)(13), B=>TobLayerIn(1)(15), H=>TobLayerOut(1)(13), L=>TobLayerOut(1)(15));


  compExch_Layer_02_to_03_sites_13_14: compExch port map(A=>TobLayerIn(2)(13), B=>TobLayerIn(2)(14), H=>TobLayerOut(2)(13), L=>TobLayerOut(2)(14));
  TobLayerOut(2)(12)<=TobLayerIn(2)(12);
  TobLayerOut(2)(15)<=TobLayerIn(2)(15);


  compExch_Layer_01_to_02_sites_16_18: compExch port map(A=>TobLayerIn(1)(16), B=>TobLayerIn(1)(18), H=>TobLayerOut(1)(16), L=>TobLayerOut(1)(18));
  compExch_Layer_01_to_02_sites_17_19: compExch port map(A=>TobLayerIn(1)(17), B=>TobLayerIn(1)(19), H=>TobLayerOut(1)(17), L=>TobLayerOut(1)(19));


  compExch_Layer_02_to_03_sites_17_18: compExch port map(A=>TobLayerIn(2)(17), B=>TobLayerIn(2)(18), H=>TobLayerOut(2)(17), L=>TobLayerOut(2)(18));
  TobLayerOut(2)(16)<=TobLayerIn(2)(16);
  TobLayerOut(2)(19)<=TobLayerIn(2)(19);


  compExch_Layer_01_to_02_sites_20_22: compExch port map(A=>TobLayerIn(1)(20), B=>TobLayerIn(1)(22), H=>TobLayerOut(1)(20), L=>TobLayerOut(1)(22));
  compExch_Layer_01_to_02_sites_21_23: compExch port map(A=>TobLayerIn(1)(21), B=>TobLayerIn(1)(23), H=>TobLayerOut(1)(21), L=>TobLayerOut(1)(23));


  compExch_Layer_02_to_03_sites_21_22: compExch port map(A=>TobLayerIn(2)(21), B=>TobLayerIn(2)(22), H=>TobLayerOut(2)(21), L=>TobLayerOut(2)(22));
  TobLayerOut(2)(20)<=TobLayerIn(2)(20);
  TobLayerOut(2)(23)<=TobLayerIn(2)(23);


  compExch_Layer_01_to_02_sites_24_26: compExch port map(A=>TobLayerIn(1)(24), B=>TobLayerIn(1)(26), H=>TobLayerOut(1)(24), L=>TobLayerOut(1)(26));
  compExch_Layer_01_to_02_sites_25_27: compExch port map(A=>TobLayerIn(1)(25), B=>TobLayerIn(1)(27), H=>TobLayerOut(1)(25), L=>TobLayerOut(1)(27));


  compExch_Layer_02_to_03_sites_25_26: compExch port map(A=>TobLayerIn(2)(25), B=>TobLayerIn(2)(26), H=>TobLayerOut(2)(25), L=>TobLayerOut(2)(26));
  TobLayerOut(2)(24)<=TobLayerIn(2)(24);
  TobLayerOut(2)(27)<=TobLayerIn(2)(27);


  compExch_Layer_01_to_02_sites_28_30: compExch port map(A=>TobLayerIn(1)(28), B=>TobLayerIn(1)(30), H=>TobLayerOut(1)(28), L=>TobLayerOut(1)(30));
  compExch_Layer_01_to_02_sites_29_31: compExch port map(A=>TobLayerIn(1)(29), B=>TobLayerIn(1)(31), H=>TobLayerOut(1)(29), L=>TobLayerOut(1)(31));


  compExch_Layer_02_to_03_sites_29_30: compExch port map(A=>TobLayerIn(2)(29), B=>TobLayerIn(2)(30), H=>TobLayerOut(2)(29), L=>TobLayerOut(2)(30));
  TobLayerOut(2)(28)<=TobLayerIn(2)(28);
  TobLayerOut(2)(31)<=TobLayerIn(2)(31);


  compExch_Layer_01_to_02_sites_32_34: compExch port map(A=>TobLayerIn(1)(32), B=>TobLayerIn(1)(34), H=>TobLayerOut(1)(32), L=>TobLayerOut(1)(34));
  compExch_Layer_01_to_02_sites_33_35: compExch port map(A=>TobLayerIn(1)(33), B=>TobLayerIn(1)(35), H=>TobLayerOut(1)(33), L=>TobLayerOut(1)(35));


  compExch_Layer_02_to_03_sites_33_34: compExch port map(A=>TobLayerIn(2)(33), B=>TobLayerIn(2)(34), H=>TobLayerOut(2)(33), L=>TobLayerOut(2)(34));
  TobLayerOut(2)(32)<=TobLayerIn(2)(32);
  TobLayerOut(2)(35)<=TobLayerIn(2)(35);


  compExch_Layer_01_to_02_sites_36_38: compExch port map(A=>TobLayerIn(1)(36), B=>TobLayerIn(1)(38), H=>TobLayerOut(1)(36), L=>TobLayerOut(1)(38));
  compExch_Layer_01_to_02_sites_37_39: compExch port map(A=>TobLayerIn(1)(37), B=>TobLayerIn(1)(39), H=>TobLayerOut(1)(37), L=>TobLayerOut(1)(39));


  compExch_Layer_02_to_03_sites_37_38: compExch port map(A=>TobLayerIn(2)(37), B=>TobLayerIn(2)(38), H=>TobLayerOut(2)(37), L=>TobLayerOut(2)(38));
  TobLayerOut(2)(36)<=TobLayerIn(2)(36);
  TobLayerOut(2)(39)<=TobLayerIn(2)(39);


  compExch_Layer_01_to_02_sites_40_42: compExch port map(A=>TobLayerIn(1)(40), B=>TobLayerIn(1)(42), H=>TobLayerOut(1)(40), L=>TobLayerOut(1)(42));
  compExch_Layer_01_to_02_sites_41_43: compExch port map(A=>TobLayerIn(1)(41), B=>TobLayerIn(1)(43), H=>TobLayerOut(1)(41), L=>TobLayerOut(1)(43));


  compExch_Layer_02_to_03_sites_41_42: compExch port map(A=>TobLayerIn(2)(41), B=>TobLayerIn(2)(42), H=>TobLayerOut(2)(41), L=>TobLayerOut(2)(42));
  TobLayerOut(2)(40)<=TobLayerIn(2)(40);
  TobLayerOut(2)(43)<=TobLayerIn(2)(43);


  compExch_Layer_01_to_02_sites_44_46: compExch port map(A=>TobLayerIn(1)(44), B=>TobLayerIn(1)(46), H=>TobLayerOut(1)(44), L=>TobLayerOut(1)(46));
  compExch_Layer_01_to_02_sites_45_47: compExch port map(A=>TobLayerIn(1)(45), B=>TobLayerIn(1)(47), H=>TobLayerOut(1)(45), L=>TobLayerOut(1)(47));


  compExch_Layer_02_to_03_sites_45_46: compExch port map(A=>TobLayerIn(2)(45), B=>TobLayerIn(2)(46), H=>TobLayerOut(2)(45), L=>TobLayerOut(2)(46));
  TobLayerOut(2)(44)<=TobLayerIn(2)(44);
  TobLayerOut(2)(47)<=TobLayerIn(2)(47);


  compExch_Layer_01_to_02_sites_48_50: compExch port map(A=>TobLayerIn(1)(48), B=>TobLayerIn(1)(50), H=>TobLayerOut(1)(48), L=>TobLayerOut(1)(50));
  compExch_Layer_01_to_02_sites_49_51: compExch port map(A=>TobLayerIn(1)(49), B=>TobLayerIn(1)(51), H=>TobLayerOut(1)(49), L=>TobLayerOut(1)(51));


  compExch_Layer_02_to_03_sites_49_50: compExch port map(A=>TobLayerIn(2)(49), B=>TobLayerIn(2)(50), H=>TobLayerOut(2)(49), L=>TobLayerOut(2)(50));
  TobLayerOut(2)(48)<=TobLayerIn(2)(48);
  TobLayerOut(2)(51)<=TobLayerIn(2)(51);


  compExch_Layer_01_to_02_sites_52_54: compExch port map(A=>TobLayerIn(1)(52), B=>TobLayerIn(1)(54), H=>TobLayerOut(1)(52), L=>TobLayerOut(1)(54));
  compExch_Layer_01_to_02_sites_53_55: compExch port map(A=>TobLayerIn(1)(53), B=>TobLayerIn(1)(55), H=>TobLayerOut(1)(53), L=>TobLayerOut(1)(55));


  compExch_Layer_02_to_03_sites_53_54: compExch port map(A=>TobLayerIn(2)(53), B=>TobLayerIn(2)(54), H=>TobLayerOut(2)(53), L=>TobLayerOut(2)(54));
  TobLayerOut(2)(52)<=TobLayerIn(2)(52);
  TobLayerOut(2)(55)<=TobLayerIn(2)(55);


  compExch_Layer_01_to_02_sites_56_58: compExch port map(A=>TobLayerIn(1)(56), B=>TobLayerIn(1)(58), H=>TobLayerOut(1)(56), L=>TobLayerOut(1)(58));
  compExch_Layer_01_to_02_sites_57_59: compExch port map(A=>TobLayerIn(1)(57), B=>TobLayerIn(1)(59), H=>TobLayerOut(1)(57), L=>TobLayerOut(1)(59));


  compExch_Layer_02_to_03_sites_57_58: compExch port map(A=>TobLayerIn(2)(57), B=>TobLayerIn(2)(58), H=>TobLayerOut(2)(57), L=>TobLayerOut(2)(58));
  TobLayerOut(2)(56)<=TobLayerIn(2)(56);
  TobLayerOut(2)(59)<=TobLayerIn(2)(59);


  compExch_Layer_01_to_02_sites_60_62: compExch port map(A=>TobLayerIn(1)(60), B=>TobLayerIn(1)(62), H=>TobLayerOut(1)(60), L=>TobLayerOut(1)(62));
  compExch_Layer_01_to_02_sites_61_63: compExch port map(A=>TobLayerIn(1)(61), B=>TobLayerIn(1)(63), H=>TobLayerOut(1)(61), L=>TobLayerOut(1)(63));


  compExch_Layer_02_to_03_sites_61_62: compExch port map(A=>TobLayerIn(2)(61), B=>TobLayerIn(2)(62), H=>TobLayerOut(2)(61), L=>TobLayerOut(2)(62));
  TobLayerOut(2)(60)<=TobLayerIn(2)(60);
  TobLayerOut(2)(63)<=TobLayerIn(2)(63);


  compExch_Layer_01_to_02_sites_64_66: compExch port map(A=>TobLayerIn(1)(64), B=>TobLayerIn(1)(66), H=>TobLayerOut(1)(64), L=>TobLayerOut(1)(66));
  compExch_Layer_01_to_02_sites_65_67: compExch port map(A=>TobLayerIn(1)(65), B=>TobLayerIn(1)(67), H=>TobLayerOut(1)(65), L=>TobLayerOut(1)(67));


  compExch_Layer_02_to_03_sites_65_66: compExch port map(A=>TobLayerIn(2)(65), B=>TobLayerIn(2)(66), H=>TobLayerOut(2)(65), L=>TobLayerOut(2)(66));
  TobLayerOut(2)(64)<=TobLayerIn(2)(64);
  TobLayerOut(2)(67)<=TobLayerIn(2)(67);


  TobLayerOut(1)(68)<=TobLayerIn(1)(68);
  TobLayerOut(1)(69)<=TobLayerIn(1)(69);

  TobLayerOut(2)(68)<=TobLayerIn(2)(68);
  TobLayerOut(2)(69)<=TobLayerIn(2)(69);





























  compExch_Layer_03_to_04_sites_00_04: compExch port map(A=>TobLayerIn(3)(0), B=>TobLayerIn(3)(4), H=>TobLayerOut(3)(0), L=>TobLayerOut(3)(4));
  compExch_Layer_03_to_04_sites_01_05: compExch port map(A=>TobLayerIn(3)(1), B=>TobLayerIn(3)(5), H=>TobLayerOut(3)(1), L=>TobLayerOut(3)(5));
  compExch_Layer_03_to_04_sites_02_06: compExch port map(A=>TobLayerIn(3)(2), B=>TobLayerIn(3)(6), H=>TobLayerOut(3)(2), L=>TobLayerOut(3)(6));
  compExch_Layer_03_to_04_sites_03_07: compExch port map(A=>TobLayerIn(3)(3), B=>TobLayerIn(3)(7), H=>TobLayerOut(3)(3), L=>TobLayerOut(3)(7));


  compExch_Layer_04_to_05_sites_02_04: compExch port map(A=>TobLayerIn(4)(2), B=>TobLayerIn(4)(4), H=>TobLayerOut(4)(2), L=>TobLayerOut(4)(4));
  compExch_Layer_04_to_05_sites_03_05: compExch port map(A=>TobLayerIn(4)(3), B=>TobLayerIn(4)(5), H=>TobLayerOut(4)(3), L=>TobLayerOut(4)(5));
  TobLayerOut(4)(0)<=TobLayerIn(4)(0);
  TobLayerOut(4)(1)<=TobLayerIn(4)(1);
  TobLayerOut(4)(6)<=TobLayerIn(4)(6);
  TobLayerOut(4)(7)<=TobLayerIn(4)(7);


  compExch_Layer_05_to_06_sites_01_02: compExch port map(A=>TobLayerIn(5)(1), B=>TobLayerIn(5)(2), H=>TobLayerOut(5)(1), L=>TobLayerOut(5)(2));
  compExch_Layer_05_to_06_sites_03_04: compExch port map(A=>TobLayerIn(5)(3), B=>TobLayerIn(5)(4), H=>TobLayerOut(5)(3), L=>TobLayerOut(5)(4));
  compExch_Layer_05_to_06_sites_05_06: compExch port map(A=>TobLayerIn(5)(5), B=>TobLayerIn(5)(6), H=>TobLayerOut(5)(5), L=>TobLayerOut(5)(6));
  TobLayerOut(5)(0)<=TobLayerIn(5)(0);
  TobLayerOut(5)(7)<=TobLayerIn(5)(7);


  compExch_Layer_03_to_04_sites_08_12: compExch port map(A=>TobLayerIn(3)(8), B=>TobLayerIn(3)(12), H=>TobLayerOut(3)(8), L=>TobLayerOut(3)(12));
  compExch_Layer_03_to_04_sites_09_13: compExch port map(A=>TobLayerIn(3)(9), B=>TobLayerIn(3)(13), H=>TobLayerOut(3)(9), L=>TobLayerOut(3)(13));
  compExch_Layer_03_to_04_sites_10_14: compExch port map(A=>TobLayerIn(3)(10), B=>TobLayerIn(3)(14), H=>TobLayerOut(3)(10), L=>TobLayerOut(3)(14));
  compExch_Layer_03_to_04_sites_11_15: compExch port map(A=>TobLayerIn(3)(11), B=>TobLayerIn(3)(15), H=>TobLayerOut(3)(11), L=>TobLayerOut(3)(15));


  compExch_Layer_04_to_05_sites_10_12: compExch port map(A=>TobLayerIn(4)(10), B=>TobLayerIn(4)(12), H=>TobLayerOut(4)(10), L=>TobLayerOut(4)(12));
  compExch_Layer_04_to_05_sites_11_13: compExch port map(A=>TobLayerIn(4)(11), B=>TobLayerIn(4)(13), H=>TobLayerOut(4)(11), L=>TobLayerOut(4)(13));
  TobLayerOut(4)(8)<=TobLayerIn(4)(8);
  TobLayerOut(4)(9)<=TobLayerIn(4)(9);
  TobLayerOut(4)(14)<=TobLayerIn(4)(14);
  TobLayerOut(4)(15)<=TobLayerIn(4)(15);


  compExch_Layer_05_to_06_sites_09_10: compExch port map(A=>TobLayerIn(5)(9), B=>TobLayerIn(5)(10), H=>TobLayerOut(5)(9), L=>TobLayerOut(5)(10));
  compExch_Layer_05_to_06_sites_11_12: compExch port map(A=>TobLayerIn(5)(11), B=>TobLayerIn(5)(12), H=>TobLayerOut(5)(11), L=>TobLayerOut(5)(12));
  compExch_Layer_05_to_06_sites_13_14: compExch port map(A=>TobLayerIn(5)(13), B=>TobLayerIn(5)(14), H=>TobLayerOut(5)(13), L=>TobLayerOut(5)(14));
  TobLayerOut(5)(8)<=TobLayerIn(5)(8);
  TobLayerOut(5)(15)<=TobLayerIn(5)(15);


  compExch_Layer_03_to_04_sites_16_20: compExch port map(A=>TobLayerIn(3)(16), B=>TobLayerIn(3)(20), H=>TobLayerOut(3)(16), L=>TobLayerOut(3)(20));
  compExch_Layer_03_to_04_sites_17_21: compExch port map(A=>TobLayerIn(3)(17), B=>TobLayerIn(3)(21), H=>TobLayerOut(3)(17), L=>TobLayerOut(3)(21));
  compExch_Layer_03_to_04_sites_18_22: compExch port map(A=>TobLayerIn(3)(18), B=>TobLayerIn(3)(22), H=>TobLayerOut(3)(18), L=>TobLayerOut(3)(22));
  compExch_Layer_03_to_04_sites_19_23: compExch port map(A=>TobLayerIn(3)(19), B=>TobLayerIn(3)(23), H=>TobLayerOut(3)(19), L=>TobLayerOut(3)(23));


  compExch_Layer_04_to_05_sites_18_20: compExch port map(A=>TobLayerIn(4)(18), B=>TobLayerIn(4)(20), H=>TobLayerOut(4)(18), L=>TobLayerOut(4)(20));
  compExch_Layer_04_to_05_sites_19_21: compExch port map(A=>TobLayerIn(4)(19), B=>TobLayerIn(4)(21), H=>TobLayerOut(4)(19), L=>TobLayerOut(4)(21));
  TobLayerOut(4)(16)<=TobLayerIn(4)(16);
  TobLayerOut(4)(17)<=TobLayerIn(4)(17);
  TobLayerOut(4)(22)<=TobLayerIn(4)(22);
  TobLayerOut(4)(23)<=TobLayerIn(4)(23);


  compExch_Layer_05_to_06_sites_17_18: compExch port map(A=>TobLayerIn(5)(17), B=>TobLayerIn(5)(18), H=>TobLayerOut(5)(17), L=>TobLayerOut(5)(18));
  compExch_Layer_05_to_06_sites_19_20: compExch port map(A=>TobLayerIn(5)(19), B=>TobLayerIn(5)(20), H=>TobLayerOut(5)(19), L=>TobLayerOut(5)(20));
  compExch_Layer_05_to_06_sites_21_22: compExch port map(A=>TobLayerIn(5)(21), B=>TobLayerIn(5)(22), H=>TobLayerOut(5)(21), L=>TobLayerOut(5)(22));
  TobLayerOut(5)(16)<=TobLayerIn(5)(16);
  TobLayerOut(5)(23)<=TobLayerIn(5)(23);


  compExch_Layer_03_to_04_sites_24_28: compExch port map(A=>TobLayerIn(3)(24), B=>TobLayerIn(3)(28), H=>TobLayerOut(3)(24), L=>TobLayerOut(3)(28));
  compExch_Layer_03_to_04_sites_25_29: compExch port map(A=>TobLayerIn(3)(25), B=>TobLayerIn(3)(29), H=>TobLayerOut(3)(25), L=>TobLayerOut(3)(29));
  compExch_Layer_03_to_04_sites_26_30: compExch port map(A=>TobLayerIn(3)(26), B=>TobLayerIn(3)(30), H=>TobLayerOut(3)(26), L=>TobLayerOut(3)(30));
  compExch_Layer_03_to_04_sites_27_31: compExch port map(A=>TobLayerIn(3)(27), B=>TobLayerIn(3)(31), H=>TobLayerOut(3)(27), L=>TobLayerOut(3)(31));


  compExch_Layer_04_to_05_sites_26_28: compExch port map(A=>TobLayerIn(4)(26), B=>TobLayerIn(4)(28), H=>TobLayerOut(4)(26), L=>TobLayerOut(4)(28));
  compExch_Layer_04_to_05_sites_27_29: compExch port map(A=>TobLayerIn(4)(27), B=>TobLayerIn(4)(29), H=>TobLayerOut(4)(27), L=>TobLayerOut(4)(29));
  TobLayerOut(4)(24)<=TobLayerIn(4)(24);
  TobLayerOut(4)(25)<=TobLayerIn(4)(25);
  TobLayerOut(4)(30)<=TobLayerIn(4)(30);
  TobLayerOut(4)(31)<=TobLayerIn(4)(31);


  compExch_Layer_05_to_06_sites_25_26: compExch port map(A=>TobLayerIn(5)(25), B=>TobLayerIn(5)(26), H=>TobLayerOut(5)(25), L=>TobLayerOut(5)(26));
  compExch_Layer_05_to_06_sites_27_28: compExch port map(A=>TobLayerIn(5)(27), B=>TobLayerIn(5)(28), H=>TobLayerOut(5)(27), L=>TobLayerOut(5)(28));
  compExch_Layer_05_to_06_sites_29_30: compExch port map(A=>TobLayerIn(5)(29), B=>TobLayerIn(5)(30), H=>TobLayerOut(5)(29), L=>TobLayerOut(5)(30));
  TobLayerOut(5)(24)<=TobLayerIn(5)(24);
  TobLayerOut(5)(31)<=TobLayerIn(5)(31);


  compExch_Layer_03_to_04_sites_32_36: compExch port map(A=>TobLayerIn(3)(32), B=>TobLayerIn(3)(36), H=>TobLayerOut(3)(32), L=>TobLayerOut(3)(36));
  compExch_Layer_03_to_04_sites_33_37: compExch port map(A=>TobLayerIn(3)(33), B=>TobLayerIn(3)(37), H=>TobLayerOut(3)(33), L=>TobLayerOut(3)(37));
  compExch_Layer_03_to_04_sites_34_38: compExch port map(A=>TobLayerIn(3)(34), B=>TobLayerIn(3)(38), H=>TobLayerOut(3)(34), L=>TobLayerOut(3)(38));
  compExch_Layer_03_to_04_sites_35_39: compExch port map(A=>TobLayerIn(3)(35), B=>TobLayerIn(3)(39), H=>TobLayerOut(3)(35), L=>TobLayerOut(3)(39));


  compExch_Layer_04_to_05_sites_34_36: compExch port map(A=>TobLayerIn(4)(34), B=>TobLayerIn(4)(36), H=>TobLayerOut(4)(34), L=>TobLayerOut(4)(36));
  compExch_Layer_04_to_05_sites_35_37: compExch port map(A=>TobLayerIn(4)(35), B=>TobLayerIn(4)(37), H=>TobLayerOut(4)(35), L=>TobLayerOut(4)(37));
  TobLayerOut(4)(32)<=TobLayerIn(4)(32);
  TobLayerOut(4)(33)<=TobLayerIn(4)(33);
  TobLayerOut(4)(38)<=TobLayerIn(4)(38);
  TobLayerOut(4)(39)<=TobLayerIn(4)(39);


  compExch_Layer_05_to_06_sites_33_34: compExch port map(A=>TobLayerIn(5)(33), B=>TobLayerIn(5)(34), H=>TobLayerOut(5)(33), L=>TobLayerOut(5)(34));
  compExch_Layer_05_to_06_sites_35_36: compExch port map(A=>TobLayerIn(5)(35), B=>TobLayerIn(5)(36), H=>TobLayerOut(5)(35), L=>TobLayerOut(5)(36));
  compExch_Layer_05_to_06_sites_37_38: compExch port map(A=>TobLayerIn(5)(37), B=>TobLayerIn(5)(38), H=>TobLayerOut(5)(37), L=>TobLayerOut(5)(38));
  TobLayerOut(5)(32)<=TobLayerIn(5)(32);
  TobLayerOut(5)(39)<=TobLayerIn(5)(39);


  compExch_Layer_03_to_04_sites_40_44: compExch port map(A=>TobLayerIn(3)(40), B=>TobLayerIn(3)(44), H=>TobLayerOut(3)(40), L=>TobLayerOut(3)(44));
  compExch_Layer_03_to_04_sites_41_45: compExch port map(A=>TobLayerIn(3)(41), B=>TobLayerIn(3)(45), H=>TobLayerOut(3)(41), L=>TobLayerOut(3)(45));
  compExch_Layer_03_to_04_sites_42_46: compExch port map(A=>TobLayerIn(3)(42), B=>TobLayerIn(3)(46), H=>TobLayerOut(3)(42), L=>TobLayerOut(3)(46));
  compExch_Layer_03_to_04_sites_43_47: compExch port map(A=>TobLayerIn(3)(43), B=>TobLayerIn(3)(47), H=>TobLayerOut(3)(43), L=>TobLayerOut(3)(47));


  compExch_Layer_04_to_05_sites_42_44: compExch port map(A=>TobLayerIn(4)(42), B=>TobLayerIn(4)(44), H=>TobLayerOut(4)(42), L=>TobLayerOut(4)(44));
  compExch_Layer_04_to_05_sites_43_45: compExch port map(A=>TobLayerIn(4)(43), B=>TobLayerIn(4)(45), H=>TobLayerOut(4)(43), L=>TobLayerOut(4)(45));
  TobLayerOut(4)(40)<=TobLayerIn(4)(40);
  TobLayerOut(4)(41)<=TobLayerIn(4)(41);
  TobLayerOut(4)(46)<=TobLayerIn(4)(46);
  TobLayerOut(4)(47)<=TobLayerIn(4)(47);


  compExch_Layer_05_to_06_sites_41_42: compExch port map(A=>TobLayerIn(5)(41), B=>TobLayerIn(5)(42), H=>TobLayerOut(5)(41), L=>TobLayerOut(5)(42));
  compExch_Layer_05_to_06_sites_43_44: compExch port map(A=>TobLayerIn(5)(43), B=>TobLayerIn(5)(44), H=>TobLayerOut(5)(43), L=>TobLayerOut(5)(44));
  compExch_Layer_05_to_06_sites_45_46: compExch port map(A=>TobLayerIn(5)(45), B=>TobLayerIn(5)(46), H=>TobLayerOut(5)(45), L=>TobLayerOut(5)(46));
  TobLayerOut(5)(40)<=TobLayerIn(5)(40);
  TobLayerOut(5)(47)<=TobLayerIn(5)(47);


  compExch_Layer_03_to_04_sites_48_52: compExch port map(A=>TobLayerIn(3)(48), B=>TobLayerIn(3)(52), H=>TobLayerOut(3)(48), L=>TobLayerOut(3)(52));
  compExch_Layer_03_to_04_sites_49_53: compExch port map(A=>TobLayerIn(3)(49), B=>TobLayerIn(3)(53), H=>TobLayerOut(3)(49), L=>TobLayerOut(3)(53));
  compExch_Layer_03_to_04_sites_50_54: compExch port map(A=>TobLayerIn(3)(50), B=>TobLayerIn(3)(54), H=>TobLayerOut(3)(50), L=>TobLayerOut(3)(54));
  compExch_Layer_03_to_04_sites_51_55: compExch port map(A=>TobLayerIn(3)(51), B=>TobLayerIn(3)(55), H=>TobLayerOut(3)(51), L=>TobLayerOut(3)(55));


  compExch_Layer_04_to_05_sites_50_52: compExch port map(A=>TobLayerIn(4)(50), B=>TobLayerIn(4)(52), H=>TobLayerOut(4)(50), L=>TobLayerOut(4)(52));
  compExch_Layer_04_to_05_sites_51_53: compExch port map(A=>TobLayerIn(4)(51), B=>TobLayerIn(4)(53), H=>TobLayerOut(4)(51), L=>TobLayerOut(4)(53));
  TobLayerOut(4)(48)<=TobLayerIn(4)(48);
  TobLayerOut(4)(49)<=TobLayerIn(4)(49);
  TobLayerOut(4)(54)<=TobLayerIn(4)(54);
  TobLayerOut(4)(55)<=TobLayerIn(4)(55);


  compExch_Layer_05_to_06_sites_49_50: compExch port map(A=>TobLayerIn(5)(49), B=>TobLayerIn(5)(50), H=>TobLayerOut(5)(49), L=>TobLayerOut(5)(50));
  compExch_Layer_05_to_06_sites_51_52: compExch port map(A=>TobLayerIn(5)(51), B=>TobLayerIn(5)(52), H=>TobLayerOut(5)(51), L=>TobLayerOut(5)(52));
  compExch_Layer_05_to_06_sites_53_54: compExch port map(A=>TobLayerIn(5)(53), B=>TobLayerIn(5)(54), H=>TobLayerOut(5)(53), L=>TobLayerOut(5)(54));
  TobLayerOut(5)(48)<=TobLayerIn(5)(48);
  TobLayerOut(5)(55)<=TobLayerIn(5)(55);


  compExch_Layer_03_to_04_sites_56_60: compExch port map(A=>TobLayerIn(3)(56), B=>TobLayerIn(3)(60), H=>TobLayerOut(3)(56), L=>TobLayerOut(3)(60));
  compExch_Layer_03_to_04_sites_57_61: compExch port map(A=>TobLayerIn(3)(57), B=>TobLayerIn(3)(61), H=>TobLayerOut(3)(57), L=>TobLayerOut(3)(61));
  compExch_Layer_03_to_04_sites_58_62: compExch port map(A=>TobLayerIn(3)(58), B=>TobLayerIn(3)(62), H=>TobLayerOut(3)(58), L=>TobLayerOut(3)(62));
  compExch_Layer_03_to_04_sites_59_63: compExch port map(A=>TobLayerIn(3)(59), B=>TobLayerIn(3)(63), H=>TobLayerOut(3)(59), L=>TobLayerOut(3)(63));


  compExch_Layer_04_to_05_sites_58_60: compExch port map(A=>TobLayerIn(4)(58), B=>TobLayerIn(4)(60), H=>TobLayerOut(4)(58), L=>TobLayerOut(4)(60));
  compExch_Layer_04_to_05_sites_59_61: compExch port map(A=>TobLayerIn(4)(59), B=>TobLayerIn(4)(61), H=>TobLayerOut(4)(59), L=>TobLayerOut(4)(61));
  TobLayerOut(4)(56)<=TobLayerIn(4)(56);
  TobLayerOut(4)(57)<=TobLayerIn(4)(57);
  TobLayerOut(4)(62)<=TobLayerIn(4)(62);
  TobLayerOut(4)(63)<=TobLayerIn(4)(63);


  compExch_Layer_05_to_06_sites_57_58: compExch port map(A=>TobLayerIn(5)(57), B=>TobLayerIn(5)(58), H=>TobLayerOut(5)(57), L=>TobLayerOut(5)(58));
  compExch_Layer_05_to_06_sites_59_60: compExch port map(A=>TobLayerIn(5)(59), B=>TobLayerIn(5)(60), H=>TobLayerOut(5)(59), L=>TobLayerOut(5)(60));
  compExch_Layer_05_to_06_sites_61_62: compExch port map(A=>TobLayerIn(5)(61), B=>TobLayerIn(5)(62), H=>TobLayerOut(5)(61), L=>TobLayerOut(5)(62));
  TobLayerOut(5)(56)<=TobLayerIn(5)(56);
  TobLayerOut(5)(63)<=TobLayerIn(5)(63);


  compExch_Layer_03_to_04_sites_64_68: compExch port map(A=>TobLayerIn(3)(64), B=>TobLayerIn(3)(68), H=>TobLayerOut(3)(64), L=>TobLayerOut(3)(68));
  compExch_Layer_03_to_04_sites_65_69: compExch port map(A=>TobLayerIn(3)(65), B=>TobLayerIn(3)(69), H=>TobLayerOut(3)(65), L=>TobLayerOut(3)(69));
  TobLayerOut(3)(66)<=TobLayerIn(3)(66);
  TobLayerOut(3)(67)<=TobLayerIn(3)(67);


  compExch_Layer_04_to_05_sites_66_68: compExch port map(A=>TobLayerIn(4)(66), B=>TobLayerIn(4)(68), H=>TobLayerOut(4)(66), L=>TobLayerOut(4)(68));
  compExch_Layer_04_to_05_sites_67_69: compExch port map(A=>TobLayerIn(4)(67), B=>TobLayerIn(4)(69), H=>TobLayerOut(4)(67), L=>TobLayerOut(4)(69));
  TobLayerOut(4)(64)<=TobLayerIn(4)(64);
  TobLayerOut(4)(65)<=TobLayerIn(4)(65);


  compExch_Layer_05_to_06_sites_65_66: compExch port map(A=>TobLayerIn(5)(65), B=>TobLayerIn(5)(66), H=>TobLayerOut(5)(65), L=>TobLayerOut(5)(66));
  compExch_Layer_05_to_06_sites_67_68: compExch port map(A=>TobLayerIn(5)(67), B=>TobLayerIn(5)(68), H=>TobLayerOut(5)(67), L=>TobLayerOut(5)(68));
  TobLayerOut(5)(64)<=TobLayerIn(5)(64);
  TobLayerOut(5)(69)<=TobLayerIn(5)(69);























  compExch_Layer_06_to_07_sites_00_08: compExch port map(A=>TobLayerIn(6)(0), B=>TobLayerIn(6)(8), H=>TobLayerOut(6)(0), L=>TobLayerOut(6)(8));
  compExch_Layer_06_to_07_sites_01_09: compExch port map(A=>TobLayerIn(6)(1), B=>TobLayerIn(6)(9), H=>TobLayerOut(6)(1), L=>TobLayerOut(6)(9));
  compExch_Layer_06_to_07_sites_02_10: compExch port map(A=>TobLayerIn(6)(2), B=>TobLayerIn(6)(10), H=>TobLayerOut(6)(2), L=>TobLayerOut(6)(10));
  compExch_Layer_06_to_07_sites_03_11: compExch port map(A=>TobLayerIn(6)(3), B=>TobLayerIn(6)(11), H=>TobLayerOut(6)(3), L=>TobLayerOut(6)(11));
  compExch_Layer_06_to_07_sites_04_12: compExch port map(A=>TobLayerIn(6)(4), B=>TobLayerIn(6)(12), H=>TobLayerOut(6)(4), L=>TobLayerOut(6)(12));
  compExch_Layer_06_to_07_sites_05_13: compExch port map(A=>TobLayerIn(6)(5), B=>TobLayerIn(6)(13), H=>TobLayerOut(6)(5), L=>TobLayerOut(6)(13));
  compExch_Layer_06_to_07_sites_06_14: compExch port map(A=>TobLayerIn(6)(6), B=>TobLayerIn(6)(14), H=>TobLayerOut(6)(6), L=>TobLayerOut(6)(14));
  compExch_Layer_06_to_07_sites_07_15: compExch port map(A=>TobLayerIn(6)(7), B=>TobLayerIn(6)(15), H=>TobLayerOut(6)(7), L=>TobLayerOut(6)(15));


  compExch_Layer_07_to_08_sites_04_08: compExch port map(A=>TobLayerIn(7)(4), B=>TobLayerIn(7)(8), H=>TobLayerOut(7)(4), L=>TobLayerOut(7)(8));
  compExch_Layer_07_to_08_sites_05_09: compExch port map(A=>TobLayerIn(7)(5), B=>TobLayerIn(7)(9), H=>TobLayerOut(7)(5), L=>TobLayerOut(7)(9));
  compExch_Layer_07_to_08_sites_06_10: compExch port map(A=>TobLayerIn(7)(6), B=>TobLayerIn(7)(10), H=>TobLayerOut(7)(6), L=>TobLayerOut(7)(10));
  compExch_Layer_07_to_08_sites_07_11: compExch port map(A=>TobLayerIn(7)(7), B=>TobLayerIn(7)(11), H=>TobLayerOut(7)(7), L=>TobLayerOut(7)(11));
  TobLayerOut(7)(0)<=TobLayerIn(7)(0);
  TobLayerOut(7)(1)<=TobLayerIn(7)(1);
  TobLayerOut(7)(2)<=TobLayerIn(7)(2);
  TobLayerOut(7)(3)<=TobLayerIn(7)(3);
  TobLayerOut(7)(12)<=TobLayerIn(7)(12);
  TobLayerOut(7)(13)<=TobLayerIn(7)(13);
  TobLayerOut(7)(14)<=TobLayerIn(7)(14);
  TobLayerOut(7)(15)<=TobLayerIn(7)(15);


  compExch_Layer_08_to_09_sites_02_04: compExch port map(A=>TobLayerIn(8)(2), B=>TobLayerIn(8)(4), H=>TobLayerOut(8)(2), L=>TobLayerOut(8)(4));
  compExch_Layer_08_to_09_sites_03_05: compExch port map(A=>TobLayerIn(8)(3), B=>TobLayerIn(8)(5), H=>TobLayerOut(8)(3), L=>TobLayerOut(8)(5));
  compExch_Layer_08_to_09_sites_06_08: compExch port map(A=>TobLayerIn(8)(6), B=>TobLayerIn(8)(8), H=>TobLayerOut(8)(6), L=>TobLayerOut(8)(8));
  compExch_Layer_08_to_09_sites_07_09: compExch port map(A=>TobLayerIn(8)(7), B=>TobLayerIn(8)(9), H=>TobLayerOut(8)(7), L=>TobLayerOut(8)(9));
  compExch_Layer_08_to_09_sites_10_12: compExch port map(A=>TobLayerIn(8)(10), B=>TobLayerIn(8)(12), H=>TobLayerOut(8)(10), L=>TobLayerOut(8)(12));
  compExch_Layer_08_to_09_sites_11_13: compExch port map(A=>TobLayerIn(8)(11), B=>TobLayerIn(8)(13), H=>TobLayerOut(8)(11), L=>TobLayerOut(8)(13));
  TobLayerOut(8)(0)<=TobLayerIn(8)(0);
  TobLayerOut(8)(1)<=TobLayerIn(8)(1);
  TobLayerOut(8)(14)<=TobLayerIn(8)(14);
  TobLayerOut(8)(15)<=TobLayerIn(8)(15);


  compExch_Layer_09_to_10_sites_01_02: compExch port map(A=>TobLayerIn(9)(1), B=>TobLayerIn(9)(2), H=>TobLayerOut(9)(1), L=>TobLayerOut(9)(2));
  compExch_Layer_09_to_10_sites_03_04: compExch port map(A=>TobLayerIn(9)(3), B=>TobLayerIn(9)(4), H=>TobLayerOut(9)(3), L=>TobLayerOut(9)(4));
  compExch_Layer_09_to_10_sites_05_06: compExch port map(A=>TobLayerIn(9)(5), B=>TobLayerIn(9)(6), H=>TobLayerOut(9)(5), L=>TobLayerOut(9)(6));
  compExch_Layer_09_to_10_sites_07_08: compExch port map(A=>TobLayerIn(9)(7), B=>TobLayerIn(9)(8), H=>TobLayerOut(9)(7), L=>TobLayerOut(9)(8));
  compExch_Layer_09_to_10_sites_09_10: compExch port map(A=>TobLayerIn(9)(9), B=>TobLayerIn(9)(10), H=>TobLayerOut(9)(9), L=>TobLayerOut(9)(10));
  compExch_Layer_09_to_10_sites_11_12: compExch port map(A=>TobLayerIn(9)(11), B=>TobLayerIn(9)(12), H=>TobLayerOut(9)(11), L=>TobLayerOut(9)(12));
  compExch_Layer_09_to_10_sites_13_14: compExch port map(A=>TobLayerIn(9)(13), B=>TobLayerIn(9)(14), H=>TobLayerOut(9)(13), L=>TobLayerOut(9)(14));
  TobLayerOut(9)(0)<=TobLayerIn(9)(0);
  TobLayerOut(9)(15)<=TobLayerIn(9)(15);


  compExch_Layer_06_to_07_sites_16_24: compExch port map(A=>TobLayerIn(6)(16), B=>TobLayerIn(6)(24), H=>TobLayerOut(6)(16), L=>TobLayerOut(6)(24));
  compExch_Layer_06_to_07_sites_17_25: compExch port map(A=>TobLayerIn(6)(17), B=>TobLayerIn(6)(25), H=>TobLayerOut(6)(17), L=>TobLayerOut(6)(25));
  compExch_Layer_06_to_07_sites_18_26: compExch port map(A=>TobLayerIn(6)(18), B=>TobLayerIn(6)(26), H=>TobLayerOut(6)(18), L=>TobLayerOut(6)(26));
  compExch_Layer_06_to_07_sites_19_27: compExch port map(A=>TobLayerIn(6)(19), B=>TobLayerIn(6)(27), H=>TobLayerOut(6)(19), L=>TobLayerOut(6)(27));
  compExch_Layer_06_to_07_sites_20_28: compExch port map(A=>TobLayerIn(6)(20), B=>TobLayerIn(6)(28), H=>TobLayerOut(6)(20), L=>TobLayerOut(6)(28));
  compExch_Layer_06_to_07_sites_21_29: compExch port map(A=>TobLayerIn(6)(21), B=>TobLayerIn(6)(29), H=>TobLayerOut(6)(21), L=>TobLayerOut(6)(29));
  compExch_Layer_06_to_07_sites_22_30: compExch port map(A=>TobLayerIn(6)(22), B=>TobLayerIn(6)(30), H=>TobLayerOut(6)(22), L=>TobLayerOut(6)(30));
  compExch_Layer_06_to_07_sites_23_31: compExch port map(A=>TobLayerIn(6)(23), B=>TobLayerIn(6)(31), H=>TobLayerOut(6)(23), L=>TobLayerOut(6)(31));


  compExch_Layer_07_to_08_sites_20_24: compExch port map(A=>TobLayerIn(7)(20), B=>TobLayerIn(7)(24), H=>TobLayerOut(7)(20), L=>TobLayerOut(7)(24));
  compExch_Layer_07_to_08_sites_21_25: compExch port map(A=>TobLayerIn(7)(21), B=>TobLayerIn(7)(25), H=>TobLayerOut(7)(21), L=>TobLayerOut(7)(25));
  compExch_Layer_07_to_08_sites_22_26: compExch port map(A=>TobLayerIn(7)(22), B=>TobLayerIn(7)(26), H=>TobLayerOut(7)(22), L=>TobLayerOut(7)(26));
  compExch_Layer_07_to_08_sites_23_27: compExch port map(A=>TobLayerIn(7)(23), B=>TobLayerIn(7)(27), H=>TobLayerOut(7)(23), L=>TobLayerOut(7)(27));
  TobLayerOut(7)(16)<=TobLayerIn(7)(16);
  TobLayerOut(7)(17)<=TobLayerIn(7)(17);
  TobLayerOut(7)(18)<=TobLayerIn(7)(18);
  TobLayerOut(7)(19)<=TobLayerIn(7)(19);
  TobLayerOut(7)(28)<=TobLayerIn(7)(28);
  TobLayerOut(7)(29)<=TobLayerIn(7)(29);
  TobLayerOut(7)(30)<=TobLayerIn(7)(30);
  TobLayerOut(7)(31)<=TobLayerIn(7)(31);


  compExch_Layer_08_to_09_sites_18_20: compExch port map(A=>TobLayerIn(8)(18), B=>TobLayerIn(8)(20), H=>TobLayerOut(8)(18), L=>TobLayerOut(8)(20));
  compExch_Layer_08_to_09_sites_19_21: compExch port map(A=>TobLayerIn(8)(19), B=>TobLayerIn(8)(21), H=>TobLayerOut(8)(19), L=>TobLayerOut(8)(21));
  compExch_Layer_08_to_09_sites_22_24: compExch port map(A=>TobLayerIn(8)(22), B=>TobLayerIn(8)(24), H=>TobLayerOut(8)(22), L=>TobLayerOut(8)(24));
  compExch_Layer_08_to_09_sites_23_25: compExch port map(A=>TobLayerIn(8)(23), B=>TobLayerIn(8)(25), H=>TobLayerOut(8)(23), L=>TobLayerOut(8)(25));
  compExch_Layer_08_to_09_sites_26_28: compExch port map(A=>TobLayerIn(8)(26), B=>TobLayerIn(8)(28), H=>TobLayerOut(8)(26), L=>TobLayerOut(8)(28));
  compExch_Layer_08_to_09_sites_27_29: compExch port map(A=>TobLayerIn(8)(27), B=>TobLayerIn(8)(29), H=>TobLayerOut(8)(27), L=>TobLayerOut(8)(29));
  TobLayerOut(8)(16)<=TobLayerIn(8)(16);
  TobLayerOut(8)(17)<=TobLayerIn(8)(17);
  TobLayerOut(8)(30)<=TobLayerIn(8)(30);
  TobLayerOut(8)(31)<=TobLayerIn(8)(31);


  compExch_Layer_09_to_10_sites_17_18: compExch port map(A=>TobLayerIn(9)(17), B=>TobLayerIn(9)(18), H=>TobLayerOut(9)(17), L=>TobLayerOut(9)(18));
  compExch_Layer_09_to_10_sites_19_20: compExch port map(A=>TobLayerIn(9)(19), B=>TobLayerIn(9)(20), H=>TobLayerOut(9)(19), L=>TobLayerOut(9)(20));
  compExch_Layer_09_to_10_sites_21_22: compExch port map(A=>TobLayerIn(9)(21), B=>TobLayerIn(9)(22), H=>TobLayerOut(9)(21), L=>TobLayerOut(9)(22));
  compExch_Layer_09_to_10_sites_23_24: compExch port map(A=>TobLayerIn(9)(23), B=>TobLayerIn(9)(24), H=>TobLayerOut(9)(23), L=>TobLayerOut(9)(24));
  compExch_Layer_09_to_10_sites_25_26: compExch port map(A=>TobLayerIn(9)(25), B=>TobLayerIn(9)(26), H=>TobLayerOut(9)(25), L=>TobLayerOut(9)(26));
  compExch_Layer_09_to_10_sites_27_28: compExch port map(A=>TobLayerIn(9)(27), B=>TobLayerIn(9)(28), H=>TobLayerOut(9)(27), L=>TobLayerOut(9)(28));
  compExch_Layer_09_to_10_sites_29_30: compExch port map(A=>TobLayerIn(9)(29), B=>TobLayerIn(9)(30), H=>TobLayerOut(9)(29), L=>TobLayerOut(9)(30));
  TobLayerOut(9)(16)<=TobLayerIn(9)(16);
  TobLayerOut(9)(31)<=TobLayerIn(9)(31);


  compExch_Layer_06_to_07_sites_32_40: compExch port map(A=>TobLayerIn(6)(32), B=>TobLayerIn(6)(40), H=>TobLayerOut(6)(32), L=>TobLayerOut(6)(40));
  compExch_Layer_06_to_07_sites_33_41: compExch port map(A=>TobLayerIn(6)(33), B=>TobLayerIn(6)(41), H=>TobLayerOut(6)(33), L=>TobLayerOut(6)(41));
  compExch_Layer_06_to_07_sites_34_42: compExch port map(A=>TobLayerIn(6)(34), B=>TobLayerIn(6)(42), H=>TobLayerOut(6)(34), L=>TobLayerOut(6)(42));
  compExch_Layer_06_to_07_sites_35_43: compExch port map(A=>TobLayerIn(6)(35), B=>TobLayerIn(6)(43), H=>TobLayerOut(6)(35), L=>TobLayerOut(6)(43));
  compExch_Layer_06_to_07_sites_36_44: compExch port map(A=>TobLayerIn(6)(36), B=>TobLayerIn(6)(44), H=>TobLayerOut(6)(36), L=>TobLayerOut(6)(44));
  compExch_Layer_06_to_07_sites_37_45: compExch port map(A=>TobLayerIn(6)(37), B=>TobLayerIn(6)(45), H=>TobLayerOut(6)(37), L=>TobLayerOut(6)(45));
  compExch_Layer_06_to_07_sites_38_46: compExch port map(A=>TobLayerIn(6)(38), B=>TobLayerIn(6)(46), H=>TobLayerOut(6)(38), L=>TobLayerOut(6)(46));
  compExch_Layer_06_to_07_sites_39_47: compExch port map(A=>TobLayerIn(6)(39), B=>TobLayerIn(6)(47), H=>TobLayerOut(6)(39), L=>TobLayerOut(6)(47));


  compExch_Layer_07_to_08_sites_36_40: compExch port map(A=>TobLayerIn(7)(36), B=>TobLayerIn(7)(40), H=>TobLayerOut(7)(36), L=>TobLayerOut(7)(40));
  compExch_Layer_07_to_08_sites_37_41: compExch port map(A=>TobLayerIn(7)(37), B=>TobLayerIn(7)(41), H=>TobLayerOut(7)(37), L=>TobLayerOut(7)(41));
  compExch_Layer_07_to_08_sites_38_42: compExch port map(A=>TobLayerIn(7)(38), B=>TobLayerIn(7)(42), H=>TobLayerOut(7)(38), L=>TobLayerOut(7)(42));
  compExch_Layer_07_to_08_sites_39_43: compExch port map(A=>TobLayerIn(7)(39), B=>TobLayerIn(7)(43), H=>TobLayerOut(7)(39), L=>TobLayerOut(7)(43));
  TobLayerOut(7)(32)<=TobLayerIn(7)(32);
  TobLayerOut(7)(33)<=TobLayerIn(7)(33);
  TobLayerOut(7)(34)<=TobLayerIn(7)(34);
  TobLayerOut(7)(35)<=TobLayerIn(7)(35);
  TobLayerOut(7)(44)<=TobLayerIn(7)(44);
  TobLayerOut(7)(45)<=TobLayerIn(7)(45);
  TobLayerOut(7)(46)<=TobLayerIn(7)(46);
  TobLayerOut(7)(47)<=TobLayerIn(7)(47);


  compExch_Layer_08_to_09_sites_34_36: compExch port map(A=>TobLayerIn(8)(34), B=>TobLayerIn(8)(36), H=>TobLayerOut(8)(34), L=>TobLayerOut(8)(36));
  compExch_Layer_08_to_09_sites_35_37: compExch port map(A=>TobLayerIn(8)(35), B=>TobLayerIn(8)(37), H=>TobLayerOut(8)(35), L=>TobLayerOut(8)(37));
  compExch_Layer_08_to_09_sites_38_40: compExch port map(A=>TobLayerIn(8)(38), B=>TobLayerIn(8)(40), H=>TobLayerOut(8)(38), L=>TobLayerOut(8)(40));
  compExch_Layer_08_to_09_sites_39_41: compExch port map(A=>TobLayerIn(8)(39), B=>TobLayerIn(8)(41), H=>TobLayerOut(8)(39), L=>TobLayerOut(8)(41));
  compExch_Layer_08_to_09_sites_42_44: compExch port map(A=>TobLayerIn(8)(42), B=>TobLayerIn(8)(44), H=>TobLayerOut(8)(42), L=>TobLayerOut(8)(44));
  compExch_Layer_08_to_09_sites_43_45: compExch port map(A=>TobLayerIn(8)(43), B=>TobLayerIn(8)(45), H=>TobLayerOut(8)(43), L=>TobLayerOut(8)(45));
  TobLayerOut(8)(32)<=TobLayerIn(8)(32);
  TobLayerOut(8)(33)<=TobLayerIn(8)(33);
  TobLayerOut(8)(46)<=TobLayerIn(8)(46);
  TobLayerOut(8)(47)<=TobLayerIn(8)(47);


  compExch_Layer_09_to_10_sites_33_34: compExch port map(A=>TobLayerIn(9)(33), B=>TobLayerIn(9)(34), H=>TobLayerOut(9)(33), L=>TobLayerOut(9)(34));
  compExch_Layer_09_to_10_sites_35_36: compExch port map(A=>TobLayerIn(9)(35), B=>TobLayerIn(9)(36), H=>TobLayerOut(9)(35), L=>TobLayerOut(9)(36));
  compExch_Layer_09_to_10_sites_37_38: compExch port map(A=>TobLayerIn(9)(37), B=>TobLayerIn(9)(38), H=>TobLayerOut(9)(37), L=>TobLayerOut(9)(38));
  compExch_Layer_09_to_10_sites_39_40: compExch port map(A=>TobLayerIn(9)(39), B=>TobLayerIn(9)(40), H=>TobLayerOut(9)(39), L=>TobLayerOut(9)(40));
  compExch_Layer_09_to_10_sites_41_42: compExch port map(A=>TobLayerIn(9)(41), B=>TobLayerIn(9)(42), H=>TobLayerOut(9)(41), L=>TobLayerOut(9)(42));
  compExch_Layer_09_to_10_sites_43_44: compExch port map(A=>TobLayerIn(9)(43), B=>TobLayerIn(9)(44), H=>TobLayerOut(9)(43), L=>TobLayerOut(9)(44));
  compExch_Layer_09_to_10_sites_45_46: compExch port map(A=>TobLayerIn(9)(45), B=>TobLayerIn(9)(46), H=>TobLayerOut(9)(45), L=>TobLayerOut(9)(46));
  TobLayerOut(9)(32)<=TobLayerIn(9)(32);
  TobLayerOut(9)(47)<=TobLayerIn(9)(47);


  compExch_Layer_06_to_07_sites_48_56: compExch port map(A=>TobLayerIn(6)(48), B=>TobLayerIn(6)(56), H=>TobLayerOut(6)(48), L=>TobLayerOut(6)(56));
  compExch_Layer_06_to_07_sites_49_57: compExch port map(A=>TobLayerIn(6)(49), B=>TobLayerIn(6)(57), H=>TobLayerOut(6)(49), L=>TobLayerOut(6)(57));
  compExch_Layer_06_to_07_sites_50_58: compExch port map(A=>TobLayerIn(6)(50), B=>TobLayerIn(6)(58), H=>TobLayerOut(6)(50), L=>TobLayerOut(6)(58));
  compExch_Layer_06_to_07_sites_51_59: compExch port map(A=>TobLayerIn(6)(51), B=>TobLayerIn(6)(59), H=>TobLayerOut(6)(51), L=>TobLayerOut(6)(59));
  compExch_Layer_06_to_07_sites_52_60: compExch port map(A=>TobLayerIn(6)(52), B=>TobLayerIn(6)(60), H=>TobLayerOut(6)(52), L=>TobLayerOut(6)(60));
  compExch_Layer_06_to_07_sites_53_61: compExch port map(A=>TobLayerIn(6)(53), B=>TobLayerIn(6)(61), H=>TobLayerOut(6)(53), L=>TobLayerOut(6)(61));
  compExch_Layer_06_to_07_sites_54_62: compExch port map(A=>TobLayerIn(6)(54), B=>TobLayerIn(6)(62), H=>TobLayerOut(6)(54), L=>TobLayerOut(6)(62));
  compExch_Layer_06_to_07_sites_55_63: compExch port map(A=>TobLayerIn(6)(55), B=>TobLayerIn(6)(63), H=>TobLayerOut(6)(55), L=>TobLayerOut(6)(63));


  compExch_Layer_07_to_08_sites_52_56: compExch port map(A=>TobLayerIn(7)(52), B=>TobLayerIn(7)(56), H=>TobLayerOut(7)(52), L=>TobLayerOut(7)(56));
  compExch_Layer_07_to_08_sites_53_57: compExch port map(A=>TobLayerIn(7)(53), B=>TobLayerIn(7)(57), H=>TobLayerOut(7)(53), L=>TobLayerOut(7)(57));
  compExch_Layer_07_to_08_sites_54_58: compExch port map(A=>TobLayerIn(7)(54), B=>TobLayerIn(7)(58), H=>TobLayerOut(7)(54), L=>TobLayerOut(7)(58));
  compExch_Layer_07_to_08_sites_55_59: compExch port map(A=>TobLayerIn(7)(55), B=>TobLayerIn(7)(59), H=>TobLayerOut(7)(55), L=>TobLayerOut(7)(59));
  TobLayerOut(7)(48)<=TobLayerIn(7)(48);
  TobLayerOut(7)(49)<=TobLayerIn(7)(49);
  TobLayerOut(7)(50)<=TobLayerIn(7)(50);
  TobLayerOut(7)(51)<=TobLayerIn(7)(51);
  TobLayerOut(7)(60)<=TobLayerIn(7)(60);
  TobLayerOut(7)(61)<=TobLayerIn(7)(61);
  TobLayerOut(7)(62)<=TobLayerIn(7)(62);
  TobLayerOut(7)(63)<=TobLayerIn(7)(63);


  compExch_Layer_08_to_09_sites_50_52: compExch port map(A=>TobLayerIn(8)(50), B=>TobLayerIn(8)(52), H=>TobLayerOut(8)(50), L=>TobLayerOut(8)(52));
  compExch_Layer_08_to_09_sites_51_53: compExch port map(A=>TobLayerIn(8)(51), B=>TobLayerIn(8)(53), H=>TobLayerOut(8)(51), L=>TobLayerOut(8)(53));
  compExch_Layer_08_to_09_sites_54_56: compExch port map(A=>TobLayerIn(8)(54), B=>TobLayerIn(8)(56), H=>TobLayerOut(8)(54), L=>TobLayerOut(8)(56));
  compExch_Layer_08_to_09_sites_55_57: compExch port map(A=>TobLayerIn(8)(55), B=>TobLayerIn(8)(57), H=>TobLayerOut(8)(55), L=>TobLayerOut(8)(57));
  compExch_Layer_08_to_09_sites_58_60: compExch port map(A=>TobLayerIn(8)(58), B=>TobLayerIn(8)(60), H=>TobLayerOut(8)(58), L=>TobLayerOut(8)(60));
  compExch_Layer_08_to_09_sites_59_61: compExch port map(A=>TobLayerIn(8)(59), B=>TobLayerIn(8)(61), H=>TobLayerOut(8)(59), L=>TobLayerOut(8)(61));
  TobLayerOut(8)(48)<=TobLayerIn(8)(48);
  TobLayerOut(8)(49)<=TobLayerIn(8)(49);
  TobLayerOut(8)(62)<=TobLayerIn(8)(62);
  TobLayerOut(8)(63)<=TobLayerIn(8)(63);


  compExch_Layer_09_to_10_sites_49_50: compExch port map(A=>TobLayerIn(9)(49), B=>TobLayerIn(9)(50), H=>TobLayerOut(9)(49), L=>TobLayerOut(9)(50));
  compExch_Layer_09_to_10_sites_51_52: compExch port map(A=>TobLayerIn(9)(51), B=>TobLayerIn(9)(52), H=>TobLayerOut(9)(51), L=>TobLayerOut(9)(52));
  compExch_Layer_09_to_10_sites_53_54: compExch port map(A=>TobLayerIn(9)(53), B=>TobLayerIn(9)(54), H=>TobLayerOut(9)(53), L=>TobLayerOut(9)(54));
  compExch_Layer_09_to_10_sites_55_56: compExch port map(A=>TobLayerIn(9)(55), B=>TobLayerIn(9)(56), H=>TobLayerOut(9)(55), L=>TobLayerOut(9)(56));
  compExch_Layer_09_to_10_sites_57_58: compExch port map(A=>TobLayerIn(9)(57), B=>TobLayerIn(9)(58), H=>TobLayerOut(9)(57), L=>TobLayerOut(9)(58));
  compExch_Layer_09_to_10_sites_59_60: compExch port map(A=>TobLayerIn(9)(59), B=>TobLayerIn(9)(60), H=>TobLayerOut(9)(59), L=>TobLayerOut(9)(60));
  compExch_Layer_09_to_10_sites_61_62: compExch port map(A=>TobLayerIn(9)(61), B=>TobLayerIn(9)(62), H=>TobLayerOut(9)(61), L=>TobLayerOut(9)(62));
  TobLayerOut(9)(48)<=TobLayerIn(9)(48);
  TobLayerOut(9)(63)<=TobLayerIn(9)(63);


  TobLayerOut(6)(64)<=TobLayerIn(6)(64);
  TobLayerOut(6)(65)<=TobLayerIn(6)(65);
  TobLayerOut(6)(66)<=TobLayerIn(6)(66);
  TobLayerOut(6)(67)<=TobLayerIn(6)(67);
  TobLayerOut(6)(68)<=TobLayerIn(6)(68);
  TobLayerOut(6)(69)<=TobLayerIn(6)(69);

  TobLayerOut(7)(64)<=TobLayerIn(7)(64);
  TobLayerOut(7)(65)<=TobLayerIn(7)(65);
  TobLayerOut(7)(66)<=TobLayerIn(7)(66);
  TobLayerOut(7)(67)<=TobLayerIn(7)(67);
  TobLayerOut(7)(68)<=TobLayerIn(7)(68);
  TobLayerOut(7)(69)<=TobLayerIn(7)(69);

  compExch_Layer_08_to_09_sites_66_68: compExch port map(A=>TobLayerIn(8)(66), B=>TobLayerIn(8)(68), H=>TobLayerOut(8)(66), L=>TobLayerOut(8)(68));
  compExch_Layer_08_to_09_sites_67_69: compExch port map(A=>TobLayerIn(8)(67), B=>TobLayerIn(8)(69), H=>TobLayerOut(8)(67), L=>TobLayerOut(8)(69));
  TobLayerOut(8)(64)<=TobLayerIn(8)(64);
  TobLayerOut(8)(65)<=TobLayerIn(8)(65);


  compExch_Layer_09_to_10_sites_65_66: compExch port map(A=>TobLayerIn(9)(65), B=>TobLayerIn(9)(66), H=>TobLayerOut(9)(65), L=>TobLayerOut(9)(66));
  compExch_Layer_09_to_10_sites_67_68: compExch port map(A=>TobLayerIn(9)(67), B=>TobLayerIn(9)(68), H=>TobLayerOut(9)(67), L=>TobLayerOut(9)(68));
  TobLayerOut(9)(64)<=TobLayerIn(9)(64);
  TobLayerOut(9)(69)<=TobLayerIn(9)(69);














  compExch_Layer_10_to_11_sites_00_16: compExch port map(A=>TobLayerIn(10)(0), B=>TobLayerIn(10)(16), H=>TobLayerOut(10)(0), L=>TobLayerOut(10)(16));
  compExch_Layer_10_to_11_sites_01_17: compExch port map(A=>TobLayerIn(10)(1), B=>TobLayerIn(10)(17), H=>TobLayerOut(10)(1), L=>TobLayerOut(10)(17));
  compExch_Layer_10_to_11_sites_02_18: compExch port map(A=>TobLayerIn(10)(2), B=>TobLayerIn(10)(18), H=>TobLayerOut(10)(2), L=>TobLayerOut(10)(18));
  compExch_Layer_10_to_11_sites_03_19: compExch port map(A=>TobLayerIn(10)(3), B=>TobLayerIn(10)(19), H=>TobLayerOut(10)(3), L=>TobLayerOut(10)(19));
  compExch_Layer_10_to_11_sites_04_20: compExch port map(A=>TobLayerIn(10)(4), B=>TobLayerIn(10)(20), H=>TobLayerOut(10)(4), L=>TobLayerOut(10)(20));
  compExch_Layer_10_to_11_sites_05_21: compExch port map(A=>TobLayerIn(10)(5), B=>TobLayerIn(10)(21), H=>TobLayerOut(10)(5), L=>TobLayerOut(10)(21));
  compExch_Layer_10_to_11_sites_06_22: compExch port map(A=>TobLayerIn(10)(6), B=>TobLayerIn(10)(22), H=>TobLayerOut(10)(6), L=>TobLayerOut(10)(22));
  compExch_Layer_10_to_11_sites_07_23: compExch port map(A=>TobLayerIn(10)(7), B=>TobLayerIn(10)(23), H=>TobLayerOut(10)(7), L=>TobLayerOut(10)(23));
  compExch_Layer_10_to_11_sites_08_24: compExch port map(A=>TobLayerIn(10)(8), B=>TobLayerIn(10)(24), H=>TobLayerOut(10)(8), L=>TobLayerOut(10)(24));
  compExch_Layer_10_to_11_sites_09_25: compExch port map(A=>TobLayerIn(10)(9), B=>TobLayerIn(10)(25), H=>TobLayerOut(10)(9), L=>TobLayerOut(10)(25));
  compExch_Layer_10_to_11_sites_10_26: compExch port map(A=>TobLayerIn(10)(10), B=>TobLayerIn(10)(26), H=>TobLayerOut(10)(10), L=>TobLayerOut(10)(26));
  compExch_Layer_10_to_11_sites_11_27: compExch port map(A=>TobLayerIn(10)(11), B=>TobLayerIn(10)(27), H=>TobLayerOut(10)(11), L=>TobLayerOut(10)(27));
  compExch_Layer_10_to_11_sites_12_28: compExch port map(A=>TobLayerIn(10)(12), B=>TobLayerIn(10)(28), H=>TobLayerOut(10)(12), L=>TobLayerOut(10)(28));
  compExch_Layer_10_to_11_sites_13_29: compExch port map(A=>TobLayerIn(10)(13), B=>TobLayerIn(10)(29), H=>TobLayerOut(10)(13), L=>TobLayerOut(10)(29));
  compExch_Layer_10_to_11_sites_14_30: compExch port map(A=>TobLayerIn(10)(14), B=>TobLayerIn(10)(30), H=>TobLayerOut(10)(14), L=>TobLayerOut(10)(30));
  compExch_Layer_10_to_11_sites_15_31: compExch port map(A=>TobLayerIn(10)(15), B=>TobLayerIn(10)(31), H=>TobLayerOut(10)(15), L=>TobLayerOut(10)(31));


  compExch_Layer_11_to_12_sites_08_16: compExch port map(A=>TobLayerIn(11)(8), B=>TobLayerIn(11)(16), H=>TobLayerOut(11)(8), L=>TobLayerOut(11)(16));
  compExch_Layer_11_to_12_sites_09_17: compExch port map(A=>TobLayerIn(11)(9), B=>TobLayerIn(11)(17), H=>TobLayerOut(11)(9), L=>TobLayerOut(11)(17));
  compExch_Layer_11_to_12_sites_10_18: compExch port map(A=>TobLayerIn(11)(10), B=>TobLayerIn(11)(18), H=>TobLayerOut(11)(10), L=>TobLayerOut(11)(18));
  compExch_Layer_11_to_12_sites_11_19: compExch port map(A=>TobLayerIn(11)(11), B=>TobLayerIn(11)(19), H=>TobLayerOut(11)(11), L=>TobLayerOut(11)(19));
  compExch_Layer_11_to_12_sites_12_20: compExch port map(A=>TobLayerIn(11)(12), B=>TobLayerIn(11)(20), H=>TobLayerOut(11)(12), L=>TobLayerOut(11)(20));
  compExch_Layer_11_to_12_sites_13_21: compExch port map(A=>TobLayerIn(11)(13), B=>TobLayerIn(11)(21), H=>TobLayerOut(11)(13), L=>TobLayerOut(11)(21));
  compExch_Layer_11_to_12_sites_14_22: compExch port map(A=>TobLayerIn(11)(14), B=>TobLayerIn(11)(22), H=>TobLayerOut(11)(14), L=>TobLayerOut(11)(22));
  compExch_Layer_11_to_12_sites_15_23: compExch port map(A=>TobLayerIn(11)(15), B=>TobLayerIn(11)(23), H=>TobLayerOut(11)(15), L=>TobLayerOut(11)(23));
  TobLayerOut(11)(0)<=TobLayerIn(11)(0);
  TobLayerOut(11)(1)<=TobLayerIn(11)(1);
  TobLayerOut(11)(2)<=TobLayerIn(11)(2);
  TobLayerOut(11)(3)<=TobLayerIn(11)(3);
  TobLayerOut(11)(4)<=TobLayerIn(11)(4);
  TobLayerOut(11)(5)<=TobLayerIn(11)(5);
  TobLayerOut(11)(6)<=TobLayerIn(11)(6);
  TobLayerOut(11)(7)<=TobLayerIn(11)(7);
  TobLayerOut(11)(24)<=TobLayerIn(11)(24);
  TobLayerOut(11)(25)<=TobLayerIn(11)(25);
  TobLayerOut(11)(26)<=TobLayerIn(11)(26);
  TobLayerOut(11)(27)<=TobLayerIn(11)(27);
  TobLayerOut(11)(28)<=TobLayerIn(11)(28);
  TobLayerOut(11)(29)<=TobLayerIn(11)(29);
  TobLayerOut(11)(30)<=TobLayerIn(11)(30);
  TobLayerOut(11)(31)<=TobLayerIn(11)(31);


  compExch_Layer_12_to_13_sites_04_08: compExch port map(A=>TobLayerIn(12)(4), B=>TobLayerIn(12)(8), H=>TobLayerOut(12)(4), L=>TobLayerOut(12)(8));
  compExch_Layer_12_to_13_sites_05_09: compExch port map(A=>TobLayerIn(12)(5), B=>TobLayerIn(12)(9), H=>TobLayerOut(12)(5), L=>TobLayerOut(12)(9));
  compExch_Layer_12_to_13_sites_06_10: compExch port map(A=>TobLayerIn(12)(6), B=>TobLayerIn(12)(10), H=>TobLayerOut(12)(6), L=>TobLayerOut(12)(10));
  compExch_Layer_12_to_13_sites_07_11: compExch port map(A=>TobLayerIn(12)(7), B=>TobLayerIn(12)(11), H=>TobLayerOut(12)(7), L=>TobLayerOut(12)(11));
  compExch_Layer_12_to_13_sites_12_16: compExch port map(A=>TobLayerIn(12)(12), B=>TobLayerIn(12)(16), H=>TobLayerOut(12)(12), L=>TobLayerOut(12)(16));
  compExch_Layer_12_to_13_sites_13_17: compExch port map(A=>TobLayerIn(12)(13), B=>TobLayerIn(12)(17), H=>TobLayerOut(12)(13), L=>TobLayerOut(12)(17));
  compExch_Layer_12_to_13_sites_14_18: compExch port map(A=>TobLayerIn(12)(14), B=>TobLayerIn(12)(18), H=>TobLayerOut(12)(14), L=>TobLayerOut(12)(18));
  compExch_Layer_12_to_13_sites_15_19: compExch port map(A=>TobLayerIn(12)(15), B=>TobLayerIn(12)(19), H=>TobLayerOut(12)(15), L=>TobLayerOut(12)(19));
  compExch_Layer_12_to_13_sites_20_24: compExch port map(A=>TobLayerIn(12)(20), B=>TobLayerIn(12)(24), H=>TobLayerOut(12)(20), L=>TobLayerOut(12)(24));
  compExch_Layer_12_to_13_sites_21_25: compExch port map(A=>TobLayerIn(12)(21), B=>TobLayerIn(12)(25), H=>TobLayerOut(12)(21), L=>TobLayerOut(12)(25));
  compExch_Layer_12_to_13_sites_22_26: compExch port map(A=>TobLayerIn(12)(22), B=>TobLayerIn(12)(26), H=>TobLayerOut(12)(22), L=>TobLayerOut(12)(26));
  compExch_Layer_12_to_13_sites_23_27: compExch port map(A=>TobLayerIn(12)(23), B=>TobLayerIn(12)(27), H=>TobLayerOut(12)(23), L=>TobLayerOut(12)(27));
  TobLayerOut(12)(0)<=TobLayerIn(12)(0);
  TobLayerOut(12)(1)<=TobLayerIn(12)(1);
  TobLayerOut(12)(2)<=TobLayerIn(12)(2);
  TobLayerOut(12)(3)<=TobLayerIn(12)(3);
  TobLayerOut(12)(28)<=TobLayerIn(12)(28);
  TobLayerOut(12)(29)<=TobLayerIn(12)(29);
  TobLayerOut(12)(30)<=TobLayerIn(12)(30);
  TobLayerOut(12)(31)<=TobLayerIn(12)(31);


  compExch_Layer_13_to_14_sites_02_04: compExch port map(A=>TobLayerIn(13)(2), B=>TobLayerIn(13)(4), H=>TobLayerOut(13)(2), L=>TobLayerOut(13)(4));
  compExch_Layer_13_to_14_sites_03_05: compExch port map(A=>TobLayerIn(13)(3), B=>TobLayerIn(13)(5), H=>TobLayerOut(13)(3), L=>TobLayerOut(13)(5));
  compExch_Layer_13_to_14_sites_06_08: compExch port map(A=>TobLayerIn(13)(6), B=>TobLayerIn(13)(8), H=>TobLayerOut(13)(6), L=>TobLayerOut(13)(8));
  compExch_Layer_13_to_14_sites_07_09: compExch port map(A=>TobLayerIn(13)(7), B=>TobLayerIn(13)(9), H=>TobLayerOut(13)(7), L=>TobLayerOut(13)(9));
  compExch_Layer_13_to_14_sites_10_12: compExch port map(A=>TobLayerIn(13)(10), B=>TobLayerIn(13)(12), H=>TobLayerOut(13)(10), L=>TobLayerOut(13)(12));
  compExch_Layer_13_to_14_sites_11_13: compExch port map(A=>TobLayerIn(13)(11), B=>TobLayerIn(13)(13), H=>TobLayerOut(13)(11), L=>TobLayerOut(13)(13));
  compExch_Layer_13_to_14_sites_14_16: compExch port map(A=>TobLayerIn(13)(14), B=>TobLayerIn(13)(16), H=>TobLayerOut(13)(14), L=>TobLayerOut(13)(16));
  compExch_Layer_13_to_14_sites_15_17: compExch port map(A=>TobLayerIn(13)(15), B=>TobLayerIn(13)(17), H=>TobLayerOut(13)(15), L=>TobLayerOut(13)(17));
  compExch_Layer_13_to_14_sites_18_20: compExch port map(A=>TobLayerIn(13)(18), B=>TobLayerIn(13)(20), H=>TobLayerOut(13)(18), L=>TobLayerOut(13)(20));
  compExch_Layer_13_to_14_sites_19_21: compExch port map(A=>TobLayerIn(13)(19), B=>TobLayerIn(13)(21), H=>TobLayerOut(13)(19), L=>TobLayerOut(13)(21));
  compExch_Layer_13_to_14_sites_22_24: compExch port map(A=>TobLayerIn(13)(22), B=>TobLayerIn(13)(24), H=>TobLayerOut(13)(22), L=>TobLayerOut(13)(24));
  compExch_Layer_13_to_14_sites_23_25: compExch port map(A=>TobLayerIn(13)(23), B=>TobLayerIn(13)(25), H=>TobLayerOut(13)(23), L=>TobLayerOut(13)(25));
  compExch_Layer_13_to_14_sites_26_28: compExch port map(A=>TobLayerIn(13)(26), B=>TobLayerIn(13)(28), H=>TobLayerOut(13)(26), L=>TobLayerOut(13)(28));
  compExch_Layer_13_to_14_sites_27_29: compExch port map(A=>TobLayerIn(13)(27), B=>TobLayerIn(13)(29), H=>TobLayerOut(13)(27), L=>TobLayerOut(13)(29));
  TobLayerOut(13)(0)<=TobLayerIn(13)(0);
  TobLayerOut(13)(1)<=TobLayerIn(13)(1);
  TobLayerOut(13)(30)<=TobLayerIn(13)(30);
  TobLayerOut(13)(31)<=TobLayerIn(13)(31);


  compExch_Layer_14_to_15_sites_01_02: compExch port map(A=>TobLayerIn(14)(1), B=>TobLayerIn(14)(2), H=>TobLayerOut(14)(1), L=>TobLayerOut(14)(2));
  compExch_Layer_14_to_15_sites_03_04: compExch port map(A=>TobLayerIn(14)(3), B=>TobLayerIn(14)(4), H=>TobLayerOut(14)(3), L=>TobLayerOut(14)(4));
  compExch_Layer_14_to_15_sites_05_06: compExch port map(A=>TobLayerIn(14)(5), B=>TobLayerIn(14)(6), H=>TobLayerOut(14)(5), L=>TobLayerOut(14)(6));
  compExch_Layer_14_to_15_sites_07_08: compExch port map(A=>TobLayerIn(14)(7), B=>TobLayerIn(14)(8), H=>TobLayerOut(14)(7), L=>TobLayerOut(14)(8));
  compExch_Layer_14_to_15_sites_09_10: compExch port map(A=>TobLayerIn(14)(9), B=>TobLayerIn(14)(10), H=>TobLayerOut(14)(9), L=>TobLayerOut(14)(10));
  compExch_Layer_14_to_15_sites_11_12: compExch port map(A=>TobLayerIn(14)(11), B=>TobLayerIn(14)(12), H=>TobLayerOut(14)(11), L=>TobLayerOut(14)(12));
  compExch_Layer_14_to_15_sites_13_14: compExch port map(A=>TobLayerIn(14)(13), B=>TobLayerIn(14)(14), H=>TobLayerOut(14)(13), L=>TobLayerOut(14)(14));
  compExch_Layer_14_to_15_sites_15_16: compExch port map(A=>TobLayerIn(14)(15), B=>TobLayerIn(14)(16), H=>TobLayerOut(14)(15), L=>TobLayerOut(14)(16));
  compExch_Layer_14_to_15_sites_17_18: compExch port map(A=>TobLayerIn(14)(17), B=>TobLayerIn(14)(18), H=>TobLayerOut(14)(17), L=>TobLayerOut(14)(18));
  compExch_Layer_14_to_15_sites_19_20: compExch port map(A=>TobLayerIn(14)(19), B=>TobLayerIn(14)(20), H=>TobLayerOut(14)(19), L=>TobLayerOut(14)(20));
  compExch_Layer_14_to_15_sites_21_22: compExch port map(A=>TobLayerIn(14)(21), B=>TobLayerIn(14)(22), H=>TobLayerOut(14)(21), L=>TobLayerOut(14)(22));
  compExch_Layer_14_to_15_sites_23_24: compExch port map(A=>TobLayerIn(14)(23), B=>TobLayerIn(14)(24), H=>TobLayerOut(14)(23), L=>TobLayerOut(14)(24));
  compExch_Layer_14_to_15_sites_25_26: compExch port map(A=>TobLayerIn(14)(25), B=>TobLayerIn(14)(26), H=>TobLayerOut(14)(25), L=>TobLayerOut(14)(26));
  compExch_Layer_14_to_15_sites_27_28: compExch port map(A=>TobLayerIn(14)(27), B=>TobLayerIn(14)(28), H=>TobLayerOut(14)(27), L=>TobLayerOut(14)(28));
  compExch_Layer_14_to_15_sites_29_30: compExch port map(A=>TobLayerIn(14)(29), B=>TobLayerIn(14)(30), H=>TobLayerOut(14)(29), L=>TobLayerOut(14)(30));
  TobLayerOut(14)(0)<=TobLayerIn(14)(0);
  TobLayerOut(14)(31)<=TobLayerIn(14)(31);


  compExch_Layer_10_to_11_sites_32_48: compExch port map(A=>TobLayerIn(10)(32), B=>TobLayerIn(10)(48), H=>TobLayerOut(10)(32), L=>TobLayerOut(10)(48));
  compExch_Layer_10_to_11_sites_33_49: compExch port map(A=>TobLayerIn(10)(33), B=>TobLayerIn(10)(49), H=>TobLayerOut(10)(33), L=>TobLayerOut(10)(49));
  compExch_Layer_10_to_11_sites_34_50: compExch port map(A=>TobLayerIn(10)(34), B=>TobLayerIn(10)(50), H=>TobLayerOut(10)(34), L=>TobLayerOut(10)(50));
  compExch_Layer_10_to_11_sites_35_51: compExch port map(A=>TobLayerIn(10)(35), B=>TobLayerIn(10)(51), H=>TobLayerOut(10)(35), L=>TobLayerOut(10)(51));
  compExch_Layer_10_to_11_sites_36_52: compExch port map(A=>TobLayerIn(10)(36), B=>TobLayerIn(10)(52), H=>TobLayerOut(10)(36), L=>TobLayerOut(10)(52));
  compExch_Layer_10_to_11_sites_37_53: compExch port map(A=>TobLayerIn(10)(37), B=>TobLayerIn(10)(53), H=>TobLayerOut(10)(37), L=>TobLayerOut(10)(53));
  compExch_Layer_10_to_11_sites_38_54: compExch port map(A=>TobLayerIn(10)(38), B=>TobLayerIn(10)(54), H=>TobLayerOut(10)(38), L=>TobLayerOut(10)(54));
  compExch_Layer_10_to_11_sites_39_55: compExch port map(A=>TobLayerIn(10)(39), B=>TobLayerIn(10)(55), H=>TobLayerOut(10)(39), L=>TobLayerOut(10)(55));
  compExch_Layer_10_to_11_sites_40_56: compExch port map(A=>TobLayerIn(10)(40), B=>TobLayerIn(10)(56), H=>TobLayerOut(10)(40), L=>TobLayerOut(10)(56));
  compExch_Layer_10_to_11_sites_41_57: compExch port map(A=>TobLayerIn(10)(41), B=>TobLayerIn(10)(57), H=>TobLayerOut(10)(41), L=>TobLayerOut(10)(57));
  compExch_Layer_10_to_11_sites_42_58: compExch port map(A=>TobLayerIn(10)(42), B=>TobLayerIn(10)(58), H=>TobLayerOut(10)(42), L=>TobLayerOut(10)(58));
  compExch_Layer_10_to_11_sites_43_59: compExch port map(A=>TobLayerIn(10)(43), B=>TobLayerIn(10)(59), H=>TobLayerOut(10)(43), L=>TobLayerOut(10)(59));
  compExch_Layer_10_to_11_sites_44_60: compExch port map(A=>TobLayerIn(10)(44), B=>TobLayerIn(10)(60), H=>TobLayerOut(10)(44), L=>TobLayerOut(10)(60));
  compExch_Layer_10_to_11_sites_45_61: compExch port map(A=>TobLayerIn(10)(45), B=>TobLayerIn(10)(61), H=>TobLayerOut(10)(45), L=>TobLayerOut(10)(61));
  compExch_Layer_10_to_11_sites_46_62: compExch port map(A=>TobLayerIn(10)(46), B=>TobLayerIn(10)(62), H=>TobLayerOut(10)(46), L=>TobLayerOut(10)(62));
  compExch_Layer_10_to_11_sites_47_63: compExch port map(A=>TobLayerIn(10)(47), B=>TobLayerIn(10)(63), H=>TobLayerOut(10)(47), L=>TobLayerOut(10)(63));


  compExch_Layer_11_to_12_sites_40_48: compExch port map(A=>TobLayerIn(11)(40), B=>TobLayerIn(11)(48), H=>TobLayerOut(11)(40), L=>TobLayerOut(11)(48));
  compExch_Layer_11_to_12_sites_41_49: compExch port map(A=>TobLayerIn(11)(41), B=>TobLayerIn(11)(49), H=>TobLayerOut(11)(41), L=>TobLayerOut(11)(49));
  compExch_Layer_11_to_12_sites_42_50: compExch port map(A=>TobLayerIn(11)(42), B=>TobLayerIn(11)(50), H=>TobLayerOut(11)(42), L=>TobLayerOut(11)(50));
  compExch_Layer_11_to_12_sites_43_51: compExch port map(A=>TobLayerIn(11)(43), B=>TobLayerIn(11)(51), H=>TobLayerOut(11)(43), L=>TobLayerOut(11)(51));
  compExch_Layer_11_to_12_sites_44_52: compExch port map(A=>TobLayerIn(11)(44), B=>TobLayerIn(11)(52), H=>TobLayerOut(11)(44), L=>TobLayerOut(11)(52));
  compExch_Layer_11_to_12_sites_45_53: compExch port map(A=>TobLayerIn(11)(45), B=>TobLayerIn(11)(53), H=>TobLayerOut(11)(45), L=>TobLayerOut(11)(53));
  compExch_Layer_11_to_12_sites_46_54: compExch port map(A=>TobLayerIn(11)(46), B=>TobLayerIn(11)(54), H=>TobLayerOut(11)(46), L=>TobLayerOut(11)(54));
  compExch_Layer_11_to_12_sites_47_55: compExch port map(A=>TobLayerIn(11)(47), B=>TobLayerIn(11)(55), H=>TobLayerOut(11)(47), L=>TobLayerOut(11)(55));
  TobLayerOut(11)(32)<=TobLayerIn(11)(32);
  TobLayerOut(11)(33)<=TobLayerIn(11)(33);
  TobLayerOut(11)(34)<=TobLayerIn(11)(34);
  TobLayerOut(11)(35)<=TobLayerIn(11)(35);
  TobLayerOut(11)(36)<=TobLayerIn(11)(36);
  TobLayerOut(11)(37)<=TobLayerIn(11)(37);
  TobLayerOut(11)(38)<=TobLayerIn(11)(38);
  TobLayerOut(11)(39)<=TobLayerIn(11)(39);
  TobLayerOut(11)(56)<=TobLayerIn(11)(56);
  TobLayerOut(11)(57)<=TobLayerIn(11)(57);
  TobLayerOut(11)(58)<=TobLayerIn(11)(58);
  TobLayerOut(11)(59)<=TobLayerIn(11)(59);
  TobLayerOut(11)(60)<=TobLayerIn(11)(60);
  TobLayerOut(11)(61)<=TobLayerIn(11)(61);
  TobLayerOut(11)(62)<=TobLayerIn(11)(62);
  TobLayerOut(11)(63)<=TobLayerIn(11)(63);


  compExch_Layer_12_to_13_sites_36_40: compExch port map(A=>TobLayerIn(12)(36), B=>TobLayerIn(12)(40), H=>TobLayerOut(12)(36), L=>TobLayerOut(12)(40));
  compExch_Layer_12_to_13_sites_37_41: compExch port map(A=>TobLayerIn(12)(37), B=>TobLayerIn(12)(41), H=>TobLayerOut(12)(37), L=>TobLayerOut(12)(41));
  compExch_Layer_12_to_13_sites_38_42: compExch port map(A=>TobLayerIn(12)(38), B=>TobLayerIn(12)(42), H=>TobLayerOut(12)(38), L=>TobLayerOut(12)(42));
  compExch_Layer_12_to_13_sites_39_43: compExch port map(A=>TobLayerIn(12)(39), B=>TobLayerIn(12)(43), H=>TobLayerOut(12)(39), L=>TobLayerOut(12)(43));
  compExch_Layer_12_to_13_sites_44_48: compExch port map(A=>TobLayerIn(12)(44), B=>TobLayerIn(12)(48), H=>TobLayerOut(12)(44), L=>TobLayerOut(12)(48));
  compExch_Layer_12_to_13_sites_45_49: compExch port map(A=>TobLayerIn(12)(45), B=>TobLayerIn(12)(49), H=>TobLayerOut(12)(45), L=>TobLayerOut(12)(49));
  compExch_Layer_12_to_13_sites_46_50: compExch port map(A=>TobLayerIn(12)(46), B=>TobLayerIn(12)(50), H=>TobLayerOut(12)(46), L=>TobLayerOut(12)(50));
  compExch_Layer_12_to_13_sites_47_51: compExch port map(A=>TobLayerIn(12)(47), B=>TobLayerIn(12)(51), H=>TobLayerOut(12)(47), L=>TobLayerOut(12)(51));
  compExch_Layer_12_to_13_sites_52_56: compExch port map(A=>TobLayerIn(12)(52), B=>TobLayerIn(12)(56), H=>TobLayerOut(12)(52), L=>TobLayerOut(12)(56));
  compExch_Layer_12_to_13_sites_53_57: compExch port map(A=>TobLayerIn(12)(53), B=>TobLayerIn(12)(57), H=>TobLayerOut(12)(53), L=>TobLayerOut(12)(57));
  compExch_Layer_12_to_13_sites_54_58: compExch port map(A=>TobLayerIn(12)(54), B=>TobLayerIn(12)(58), H=>TobLayerOut(12)(54), L=>TobLayerOut(12)(58));
  compExch_Layer_12_to_13_sites_55_59: compExch port map(A=>TobLayerIn(12)(55), B=>TobLayerIn(12)(59), H=>TobLayerOut(12)(55), L=>TobLayerOut(12)(59));
  TobLayerOut(12)(32)<=TobLayerIn(12)(32);
  TobLayerOut(12)(33)<=TobLayerIn(12)(33);
  TobLayerOut(12)(34)<=TobLayerIn(12)(34);
  TobLayerOut(12)(35)<=TobLayerIn(12)(35);
  TobLayerOut(12)(60)<=TobLayerIn(12)(60);
  TobLayerOut(12)(61)<=TobLayerIn(12)(61);
  TobLayerOut(12)(62)<=TobLayerIn(12)(62);
  TobLayerOut(12)(63)<=TobLayerIn(12)(63);


  compExch_Layer_13_to_14_sites_34_36: compExch port map(A=>TobLayerIn(13)(34), B=>TobLayerIn(13)(36), H=>TobLayerOut(13)(34), L=>TobLayerOut(13)(36));
  compExch_Layer_13_to_14_sites_35_37: compExch port map(A=>TobLayerIn(13)(35), B=>TobLayerIn(13)(37), H=>TobLayerOut(13)(35), L=>TobLayerOut(13)(37));
  compExch_Layer_13_to_14_sites_38_40: compExch port map(A=>TobLayerIn(13)(38), B=>TobLayerIn(13)(40), H=>TobLayerOut(13)(38), L=>TobLayerOut(13)(40));
  compExch_Layer_13_to_14_sites_39_41: compExch port map(A=>TobLayerIn(13)(39), B=>TobLayerIn(13)(41), H=>TobLayerOut(13)(39), L=>TobLayerOut(13)(41));
  compExch_Layer_13_to_14_sites_42_44: compExch port map(A=>TobLayerIn(13)(42), B=>TobLayerIn(13)(44), H=>TobLayerOut(13)(42), L=>TobLayerOut(13)(44));
  compExch_Layer_13_to_14_sites_43_45: compExch port map(A=>TobLayerIn(13)(43), B=>TobLayerIn(13)(45), H=>TobLayerOut(13)(43), L=>TobLayerOut(13)(45));
  compExch_Layer_13_to_14_sites_46_48: compExch port map(A=>TobLayerIn(13)(46), B=>TobLayerIn(13)(48), H=>TobLayerOut(13)(46), L=>TobLayerOut(13)(48));
  compExch_Layer_13_to_14_sites_47_49: compExch port map(A=>TobLayerIn(13)(47), B=>TobLayerIn(13)(49), H=>TobLayerOut(13)(47), L=>TobLayerOut(13)(49));
  compExch_Layer_13_to_14_sites_50_52: compExch port map(A=>TobLayerIn(13)(50), B=>TobLayerIn(13)(52), H=>TobLayerOut(13)(50), L=>TobLayerOut(13)(52));
  compExch_Layer_13_to_14_sites_51_53: compExch port map(A=>TobLayerIn(13)(51), B=>TobLayerIn(13)(53), H=>TobLayerOut(13)(51), L=>TobLayerOut(13)(53));
  compExch_Layer_13_to_14_sites_54_56: compExch port map(A=>TobLayerIn(13)(54), B=>TobLayerIn(13)(56), H=>TobLayerOut(13)(54), L=>TobLayerOut(13)(56));
  compExch_Layer_13_to_14_sites_55_57: compExch port map(A=>TobLayerIn(13)(55), B=>TobLayerIn(13)(57), H=>TobLayerOut(13)(55), L=>TobLayerOut(13)(57));
  compExch_Layer_13_to_14_sites_58_60: compExch port map(A=>TobLayerIn(13)(58), B=>TobLayerIn(13)(60), H=>TobLayerOut(13)(58), L=>TobLayerOut(13)(60));
  compExch_Layer_13_to_14_sites_59_61: compExch port map(A=>TobLayerIn(13)(59), B=>TobLayerIn(13)(61), H=>TobLayerOut(13)(59), L=>TobLayerOut(13)(61));
  TobLayerOut(13)(32)<=TobLayerIn(13)(32);
  TobLayerOut(13)(33)<=TobLayerIn(13)(33);
  TobLayerOut(13)(62)<=TobLayerIn(13)(62);
  TobLayerOut(13)(63)<=TobLayerIn(13)(63);


  compExch_Layer_14_to_15_sites_33_34: compExch port map(A=>TobLayerIn(14)(33), B=>TobLayerIn(14)(34), H=>TobLayerOut(14)(33), L=>TobLayerOut(14)(34));
  compExch_Layer_14_to_15_sites_35_36: compExch port map(A=>TobLayerIn(14)(35), B=>TobLayerIn(14)(36), H=>TobLayerOut(14)(35), L=>TobLayerOut(14)(36));
  compExch_Layer_14_to_15_sites_37_38: compExch port map(A=>TobLayerIn(14)(37), B=>TobLayerIn(14)(38), H=>TobLayerOut(14)(37), L=>TobLayerOut(14)(38));
  compExch_Layer_14_to_15_sites_39_40: compExch port map(A=>TobLayerIn(14)(39), B=>TobLayerIn(14)(40), H=>TobLayerOut(14)(39), L=>TobLayerOut(14)(40));
  compExch_Layer_14_to_15_sites_41_42: compExch port map(A=>TobLayerIn(14)(41), B=>TobLayerIn(14)(42), H=>TobLayerOut(14)(41), L=>TobLayerOut(14)(42));
  compExch_Layer_14_to_15_sites_43_44: compExch port map(A=>TobLayerIn(14)(43), B=>TobLayerIn(14)(44), H=>TobLayerOut(14)(43), L=>TobLayerOut(14)(44));
  compExch_Layer_14_to_15_sites_45_46: compExch port map(A=>TobLayerIn(14)(45), B=>TobLayerIn(14)(46), H=>TobLayerOut(14)(45), L=>TobLayerOut(14)(46));
  compExch_Layer_14_to_15_sites_47_48: compExch port map(A=>TobLayerIn(14)(47), B=>TobLayerIn(14)(48), H=>TobLayerOut(14)(47), L=>TobLayerOut(14)(48));
  compExch_Layer_14_to_15_sites_49_50: compExch port map(A=>TobLayerIn(14)(49), B=>TobLayerIn(14)(50), H=>TobLayerOut(14)(49), L=>TobLayerOut(14)(50));
  compExch_Layer_14_to_15_sites_51_52: compExch port map(A=>TobLayerIn(14)(51), B=>TobLayerIn(14)(52), H=>TobLayerOut(14)(51), L=>TobLayerOut(14)(52));
  compExch_Layer_14_to_15_sites_53_54: compExch port map(A=>TobLayerIn(14)(53), B=>TobLayerIn(14)(54), H=>TobLayerOut(14)(53), L=>TobLayerOut(14)(54));
  compExch_Layer_14_to_15_sites_55_56: compExch port map(A=>TobLayerIn(14)(55), B=>TobLayerIn(14)(56), H=>TobLayerOut(14)(55), L=>TobLayerOut(14)(56));
  compExch_Layer_14_to_15_sites_57_58: compExch port map(A=>TobLayerIn(14)(57), B=>TobLayerIn(14)(58), H=>TobLayerOut(14)(57), L=>TobLayerOut(14)(58));
  compExch_Layer_14_to_15_sites_59_60: compExch port map(A=>TobLayerIn(14)(59), B=>TobLayerIn(14)(60), H=>TobLayerOut(14)(59), L=>TobLayerOut(14)(60));
  compExch_Layer_14_to_15_sites_61_62: compExch port map(A=>TobLayerIn(14)(61), B=>TobLayerIn(14)(62), H=>TobLayerOut(14)(61), L=>TobLayerOut(14)(62));
  TobLayerOut(14)(32)<=TobLayerIn(14)(32);
  TobLayerOut(14)(63)<=TobLayerIn(14)(63);


  TobLayerOut(10)(64)<=TobLayerIn(10)(64);
  TobLayerOut(10)(65)<=TobLayerIn(10)(65);
  TobLayerOut(10)(66)<=TobLayerIn(10)(66);
  TobLayerOut(10)(67)<=TobLayerIn(10)(67);
  TobLayerOut(10)(68)<=TobLayerIn(10)(68);
  TobLayerOut(10)(69)<=TobLayerIn(10)(69);

  TobLayerOut(11)(64)<=TobLayerIn(11)(64);
  TobLayerOut(11)(65)<=TobLayerIn(11)(65);
  TobLayerOut(11)(66)<=TobLayerIn(11)(66);
  TobLayerOut(11)(67)<=TobLayerIn(11)(67);
  TobLayerOut(11)(68)<=TobLayerIn(11)(68);
  TobLayerOut(11)(69)<=TobLayerIn(11)(69);

  TobLayerOut(12)(64)<=TobLayerIn(12)(64);
  TobLayerOut(12)(65)<=TobLayerIn(12)(65);
  TobLayerOut(12)(66)<=TobLayerIn(12)(66);
  TobLayerOut(12)(67)<=TobLayerIn(12)(67);
  TobLayerOut(12)(68)<=TobLayerIn(12)(68);
  TobLayerOut(12)(69)<=TobLayerIn(12)(69);

  compExch_Layer_13_to_14_sites_66_68: compExch port map(A=>TobLayerIn(13)(66), B=>TobLayerIn(13)(68), H=>TobLayerOut(13)(66), L=>TobLayerOut(13)(68));
  compExch_Layer_13_to_14_sites_67_69: compExch port map(A=>TobLayerIn(13)(67), B=>TobLayerIn(13)(69), H=>TobLayerOut(13)(67), L=>TobLayerOut(13)(69));
  TobLayerOut(13)(64)<=TobLayerIn(13)(64);
  TobLayerOut(13)(65)<=TobLayerIn(13)(65);


  compExch_Layer_14_to_15_sites_65_66: compExch port map(A=>TobLayerIn(14)(65), B=>TobLayerIn(14)(66), H=>TobLayerOut(14)(65), L=>TobLayerOut(14)(66));
  compExch_Layer_14_to_15_sites_67_68: compExch port map(A=>TobLayerIn(14)(67), B=>TobLayerIn(14)(68), H=>TobLayerOut(14)(67), L=>TobLayerOut(14)(68));
  TobLayerOut(14)(64)<=TobLayerIn(14)(64);
  TobLayerOut(14)(69)<=TobLayerIn(14)(69);







  compExch_Layer_15_to_16_sites_00_32: compExch port map(A=>TobLayerIn(15)(0), B=>TobLayerIn(15)(32), H=>TobLayerOut(15)(0), L=>TobLayerOut(15)(32));
  compExch_Layer_15_to_16_sites_01_33: compExch port map(A=>TobLayerIn(15)(1), B=>TobLayerIn(15)(33), H=>TobLayerOut(15)(1), L=>TobLayerOut(15)(33));
  compExch_Layer_15_to_16_sites_02_34: compExch port map(A=>TobLayerIn(15)(2), B=>TobLayerIn(15)(34), H=>TobLayerOut(15)(2), L=>TobLayerOut(15)(34));
  compExch_Layer_15_to_16_sites_03_35: compExch port map(A=>TobLayerIn(15)(3), B=>TobLayerIn(15)(35), H=>TobLayerOut(15)(3), L=>TobLayerOut(15)(35));
  compExch_Layer_15_to_16_sites_04_36: compExch port map(A=>TobLayerIn(15)(4), B=>TobLayerIn(15)(36), H=>TobLayerOut(15)(4), L=>TobLayerOut(15)(36));
  compExch_Layer_15_to_16_sites_05_37: compExch port map(A=>TobLayerIn(15)(5), B=>TobLayerIn(15)(37), H=>TobLayerOut(15)(5), L=>TobLayerOut(15)(37));
  compExch_Layer_15_to_16_sites_06_38: compExch port map(A=>TobLayerIn(15)(6), B=>TobLayerIn(15)(38), H=>TobLayerOut(15)(6), L=>TobLayerOut(15)(38));
  compExch_Layer_15_to_16_sites_07_39: compExch port map(A=>TobLayerIn(15)(7), B=>TobLayerIn(15)(39), H=>TobLayerOut(15)(7), L=>TobLayerOut(15)(39));
  compExch_Layer_15_to_16_sites_08_40: compExch port map(A=>TobLayerIn(15)(8), B=>TobLayerIn(15)(40), H=>TobLayerOut(15)(8), L=>TobLayerOut(15)(40));
  compExch_Layer_15_to_16_sites_09_41: compExch port map(A=>TobLayerIn(15)(9), B=>TobLayerIn(15)(41), H=>TobLayerOut(15)(9), L=>TobLayerOut(15)(41));
  compExch_Layer_15_to_16_sites_10_42: compExch port map(A=>TobLayerIn(15)(10), B=>TobLayerIn(15)(42), H=>TobLayerOut(15)(10), L=>TobLayerOut(15)(42));
  compExch_Layer_15_to_16_sites_11_43: compExch port map(A=>TobLayerIn(15)(11), B=>TobLayerIn(15)(43), H=>TobLayerOut(15)(11), L=>TobLayerOut(15)(43));
  compExch_Layer_15_to_16_sites_12_44: compExch port map(A=>TobLayerIn(15)(12), B=>TobLayerIn(15)(44), H=>TobLayerOut(15)(12), L=>TobLayerOut(15)(44));
  compExch_Layer_15_to_16_sites_13_45: compExch port map(A=>TobLayerIn(15)(13), B=>TobLayerIn(15)(45), H=>TobLayerOut(15)(13), L=>TobLayerOut(15)(45));
  compExch_Layer_15_to_16_sites_14_46: compExch port map(A=>TobLayerIn(15)(14), B=>TobLayerIn(15)(46), H=>TobLayerOut(15)(14), L=>TobLayerOut(15)(46));
  compExch_Layer_15_to_16_sites_15_47: compExch port map(A=>TobLayerIn(15)(15), B=>TobLayerIn(15)(47), H=>TobLayerOut(15)(15), L=>TobLayerOut(15)(47));
  compExch_Layer_15_to_16_sites_16_48: compExch port map(A=>TobLayerIn(15)(16), B=>TobLayerIn(15)(48), H=>TobLayerOut(15)(16), L=>TobLayerOut(15)(48));
  compExch_Layer_15_to_16_sites_17_49: compExch port map(A=>TobLayerIn(15)(17), B=>TobLayerIn(15)(49), H=>TobLayerOut(15)(17), L=>TobLayerOut(15)(49));
  compExch_Layer_15_to_16_sites_18_50: compExch port map(A=>TobLayerIn(15)(18), B=>TobLayerIn(15)(50), H=>TobLayerOut(15)(18), L=>TobLayerOut(15)(50));
  compExch_Layer_15_to_16_sites_19_51: compExch port map(A=>TobLayerIn(15)(19), B=>TobLayerIn(15)(51), H=>TobLayerOut(15)(19), L=>TobLayerOut(15)(51));
  compExch_Layer_15_to_16_sites_20_52: compExch port map(A=>TobLayerIn(15)(20), B=>TobLayerIn(15)(52), H=>TobLayerOut(15)(20), L=>TobLayerOut(15)(52));
  compExch_Layer_15_to_16_sites_21_53: compExch port map(A=>TobLayerIn(15)(21), B=>TobLayerIn(15)(53), H=>TobLayerOut(15)(21), L=>TobLayerOut(15)(53));
  compExch_Layer_15_to_16_sites_22_54: compExch port map(A=>TobLayerIn(15)(22), B=>TobLayerIn(15)(54), H=>TobLayerOut(15)(22), L=>TobLayerOut(15)(54));
  compExch_Layer_15_to_16_sites_23_55: compExch port map(A=>TobLayerIn(15)(23), B=>TobLayerIn(15)(55), H=>TobLayerOut(15)(23), L=>TobLayerOut(15)(55));
  compExch_Layer_15_to_16_sites_24_56: compExch port map(A=>TobLayerIn(15)(24), B=>TobLayerIn(15)(56), H=>TobLayerOut(15)(24), L=>TobLayerOut(15)(56));
  compExch_Layer_15_to_16_sites_25_57: compExch port map(A=>TobLayerIn(15)(25), B=>TobLayerIn(15)(57), H=>TobLayerOut(15)(25), L=>TobLayerOut(15)(57));
  compExch_Layer_15_to_16_sites_26_58: compExch port map(A=>TobLayerIn(15)(26), B=>TobLayerIn(15)(58), H=>TobLayerOut(15)(26), L=>TobLayerOut(15)(58));
  compExch_Layer_15_to_16_sites_27_59: compExch port map(A=>TobLayerIn(15)(27), B=>TobLayerIn(15)(59), H=>TobLayerOut(15)(27), L=>TobLayerOut(15)(59));
  compExch_Layer_15_to_16_sites_28_60: compExch port map(A=>TobLayerIn(15)(28), B=>TobLayerIn(15)(60), H=>TobLayerOut(15)(28), L=>TobLayerOut(15)(60));
  compExch_Layer_15_to_16_sites_29_61: compExch port map(A=>TobLayerIn(15)(29), B=>TobLayerIn(15)(61), H=>TobLayerOut(15)(29), L=>TobLayerOut(15)(61));
  compExch_Layer_15_to_16_sites_30_62: compExch port map(A=>TobLayerIn(15)(30), B=>TobLayerIn(15)(62), H=>TobLayerOut(15)(30), L=>TobLayerOut(15)(62));
  compExch_Layer_15_to_16_sites_31_63: compExch port map(A=>TobLayerIn(15)(31), B=>TobLayerIn(15)(63), H=>TobLayerOut(15)(31), L=>TobLayerOut(15)(63));


  compExch_Layer_16_to_17_sites_16_32: compExch port map(A=>TobLayerIn(16)(16), B=>TobLayerIn(16)(32), H=>TobLayerOut(16)(16), L=>TobLayerOut(16)(32));
  compExch_Layer_16_to_17_sites_17_33: compExch port map(A=>TobLayerIn(16)(17), B=>TobLayerIn(16)(33), H=>TobLayerOut(16)(17), L=>TobLayerOut(16)(33));
  compExch_Layer_16_to_17_sites_18_34: compExch port map(A=>TobLayerIn(16)(18), B=>TobLayerIn(16)(34), H=>TobLayerOut(16)(18), L=>TobLayerOut(16)(34));
  compExch_Layer_16_to_17_sites_19_35: compExch port map(A=>TobLayerIn(16)(19), B=>TobLayerIn(16)(35), H=>TobLayerOut(16)(19), L=>TobLayerOut(16)(35));
  compExch_Layer_16_to_17_sites_20_36: compExch port map(A=>TobLayerIn(16)(20), B=>TobLayerIn(16)(36), H=>TobLayerOut(16)(20), L=>TobLayerOut(16)(36));
  compExch_Layer_16_to_17_sites_21_37: compExch port map(A=>TobLayerIn(16)(21), B=>TobLayerIn(16)(37), H=>TobLayerOut(16)(21), L=>TobLayerOut(16)(37));
  compExch_Layer_16_to_17_sites_22_38: compExch port map(A=>TobLayerIn(16)(22), B=>TobLayerIn(16)(38), H=>TobLayerOut(16)(22), L=>TobLayerOut(16)(38));
  compExch_Layer_16_to_17_sites_23_39: compExch port map(A=>TobLayerIn(16)(23), B=>TobLayerIn(16)(39), H=>TobLayerOut(16)(23), L=>TobLayerOut(16)(39));
  compExch_Layer_16_to_17_sites_24_40: compExch port map(A=>TobLayerIn(16)(24), B=>TobLayerIn(16)(40), H=>TobLayerOut(16)(24), L=>TobLayerOut(16)(40));
  compExch_Layer_16_to_17_sites_25_41: compExch port map(A=>TobLayerIn(16)(25), B=>TobLayerIn(16)(41), H=>TobLayerOut(16)(25), L=>TobLayerOut(16)(41));
  compExch_Layer_16_to_17_sites_26_42: compExch port map(A=>TobLayerIn(16)(26), B=>TobLayerIn(16)(42), H=>TobLayerOut(16)(26), L=>TobLayerOut(16)(42));
  compExch_Layer_16_to_17_sites_27_43: compExch port map(A=>TobLayerIn(16)(27), B=>TobLayerIn(16)(43), H=>TobLayerOut(16)(27), L=>TobLayerOut(16)(43));
  compExch_Layer_16_to_17_sites_28_44: compExch port map(A=>TobLayerIn(16)(28), B=>TobLayerIn(16)(44), H=>TobLayerOut(16)(28), L=>TobLayerOut(16)(44));
  compExch_Layer_16_to_17_sites_29_45: compExch port map(A=>TobLayerIn(16)(29), B=>TobLayerIn(16)(45), H=>TobLayerOut(16)(29), L=>TobLayerOut(16)(45));
  compExch_Layer_16_to_17_sites_30_46: compExch port map(A=>TobLayerIn(16)(30), B=>TobLayerIn(16)(46), H=>TobLayerOut(16)(30), L=>TobLayerOut(16)(46));
  compExch_Layer_16_to_17_sites_31_47: compExch port map(A=>TobLayerIn(16)(31), B=>TobLayerIn(16)(47), H=>TobLayerOut(16)(31), L=>TobLayerOut(16)(47));
  TobLayerOut(16)(0)<=TobLayerIn(16)(0);
  TobLayerOut(16)(1)<=TobLayerIn(16)(1);
  TobLayerOut(16)(2)<=TobLayerIn(16)(2);
  TobLayerOut(16)(3)<=TobLayerIn(16)(3);
  TobLayerOut(16)(4)<=TobLayerIn(16)(4);
  TobLayerOut(16)(5)<=TobLayerIn(16)(5);
  TobLayerOut(16)(6)<=TobLayerIn(16)(6);
  TobLayerOut(16)(7)<=TobLayerIn(16)(7);
  TobLayerOut(16)(8)<=TobLayerIn(16)(8);
  TobLayerOut(16)(9)<=TobLayerIn(16)(9);
  TobLayerOut(16)(10)<=TobLayerIn(16)(10);
  TobLayerOut(16)(11)<=TobLayerIn(16)(11);
  TobLayerOut(16)(12)<=TobLayerIn(16)(12);
  TobLayerOut(16)(13)<=TobLayerIn(16)(13);
  TobLayerOut(16)(14)<=TobLayerIn(16)(14);
  TobLayerOut(16)(15)<=TobLayerIn(16)(15);
  TobLayerOut(16)(48)<=TobLayerIn(16)(48);
  TobLayerOut(16)(49)<=TobLayerIn(16)(49);
  TobLayerOut(16)(50)<=TobLayerIn(16)(50);
  TobLayerOut(16)(51)<=TobLayerIn(16)(51);
  TobLayerOut(16)(52)<=TobLayerIn(16)(52);
  TobLayerOut(16)(53)<=TobLayerIn(16)(53);
  TobLayerOut(16)(54)<=TobLayerIn(16)(54);
  TobLayerOut(16)(55)<=TobLayerIn(16)(55);
  TobLayerOut(16)(56)<=TobLayerIn(16)(56);
  TobLayerOut(16)(57)<=TobLayerIn(16)(57);
  TobLayerOut(16)(58)<=TobLayerIn(16)(58);
  TobLayerOut(16)(59)<=TobLayerIn(16)(59);
  TobLayerOut(16)(60)<=TobLayerIn(16)(60);
  TobLayerOut(16)(61)<=TobLayerIn(16)(61);
  TobLayerOut(16)(62)<=TobLayerIn(16)(62);
  TobLayerOut(16)(63)<=TobLayerIn(16)(63);


  compExch_Layer_17_to_18_sites_08_16: compExch port map(A=>TobLayerIn(17)(8), B=>TobLayerIn(17)(16), H=>TobLayerOut(17)(8), L=>TobLayerOut(17)(16));
  compExch_Layer_17_to_18_sites_09_17: compExch port map(A=>TobLayerIn(17)(9), B=>TobLayerIn(17)(17), H=>TobLayerOut(17)(9), L=>TobLayerOut(17)(17));
  compExch_Layer_17_to_18_sites_10_18: compExch port map(A=>TobLayerIn(17)(10), B=>TobLayerIn(17)(18), H=>TobLayerOut(17)(10), L=>TobLayerOut(17)(18));
  compExch_Layer_17_to_18_sites_11_19: compExch port map(A=>TobLayerIn(17)(11), B=>TobLayerIn(17)(19), H=>TobLayerOut(17)(11), L=>TobLayerOut(17)(19));
  compExch_Layer_17_to_18_sites_12_20: compExch port map(A=>TobLayerIn(17)(12), B=>TobLayerIn(17)(20), H=>TobLayerOut(17)(12), L=>TobLayerOut(17)(20));
  compExch_Layer_17_to_18_sites_13_21: compExch port map(A=>TobLayerIn(17)(13), B=>TobLayerIn(17)(21), H=>TobLayerOut(17)(13), L=>TobLayerOut(17)(21));
  compExch_Layer_17_to_18_sites_14_22: compExch port map(A=>TobLayerIn(17)(14), B=>TobLayerIn(17)(22), H=>TobLayerOut(17)(14), L=>TobLayerOut(17)(22));
  compExch_Layer_17_to_18_sites_15_23: compExch port map(A=>TobLayerIn(17)(15), B=>TobLayerIn(17)(23), H=>TobLayerOut(17)(15), L=>TobLayerOut(17)(23));
  compExch_Layer_17_to_18_sites_24_32: compExch port map(A=>TobLayerIn(17)(24), B=>TobLayerIn(17)(32), H=>TobLayerOut(17)(24), L=>TobLayerOut(17)(32));
  compExch_Layer_17_to_18_sites_25_33: compExch port map(A=>TobLayerIn(17)(25), B=>TobLayerIn(17)(33), H=>TobLayerOut(17)(25), L=>TobLayerOut(17)(33));
  compExch_Layer_17_to_18_sites_26_34: compExch port map(A=>TobLayerIn(17)(26), B=>TobLayerIn(17)(34), H=>TobLayerOut(17)(26), L=>TobLayerOut(17)(34));
  compExch_Layer_17_to_18_sites_27_35: compExch port map(A=>TobLayerIn(17)(27), B=>TobLayerIn(17)(35), H=>TobLayerOut(17)(27), L=>TobLayerOut(17)(35));
  compExch_Layer_17_to_18_sites_28_36: compExch port map(A=>TobLayerIn(17)(28), B=>TobLayerIn(17)(36), H=>TobLayerOut(17)(28), L=>TobLayerOut(17)(36));
  compExch_Layer_17_to_18_sites_29_37: compExch port map(A=>TobLayerIn(17)(29), B=>TobLayerIn(17)(37), H=>TobLayerOut(17)(29), L=>TobLayerOut(17)(37));
  compExch_Layer_17_to_18_sites_30_38: compExch port map(A=>TobLayerIn(17)(30), B=>TobLayerIn(17)(38), H=>TobLayerOut(17)(30), L=>TobLayerOut(17)(38));
  compExch_Layer_17_to_18_sites_31_39: compExch port map(A=>TobLayerIn(17)(31), B=>TobLayerIn(17)(39), H=>TobLayerOut(17)(31), L=>TobLayerOut(17)(39));
  compExch_Layer_17_to_18_sites_40_48: compExch port map(A=>TobLayerIn(17)(40), B=>TobLayerIn(17)(48), H=>TobLayerOut(17)(40), L=>TobLayerOut(17)(48));
  compExch_Layer_17_to_18_sites_41_49: compExch port map(A=>TobLayerIn(17)(41), B=>TobLayerIn(17)(49), H=>TobLayerOut(17)(41), L=>TobLayerOut(17)(49));
  compExch_Layer_17_to_18_sites_42_50: compExch port map(A=>TobLayerIn(17)(42), B=>TobLayerIn(17)(50), H=>TobLayerOut(17)(42), L=>TobLayerOut(17)(50));
  compExch_Layer_17_to_18_sites_43_51: compExch port map(A=>TobLayerIn(17)(43), B=>TobLayerIn(17)(51), H=>TobLayerOut(17)(43), L=>TobLayerOut(17)(51));
  compExch_Layer_17_to_18_sites_44_52: compExch port map(A=>TobLayerIn(17)(44), B=>TobLayerIn(17)(52), H=>TobLayerOut(17)(44), L=>TobLayerOut(17)(52));
  compExch_Layer_17_to_18_sites_45_53: compExch port map(A=>TobLayerIn(17)(45), B=>TobLayerIn(17)(53), H=>TobLayerOut(17)(45), L=>TobLayerOut(17)(53));
  compExch_Layer_17_to_18_sites_46_54: compExch port map(A=>TobLayerIn(17)(46), B=>TobLayerIn(17)(54), H=>TobLayerOut(17)(46), L=>TobLayerOut(17)(54));
  compExch_Layer_17_to_18_sites_47_55: compExch port map(A=>TobLayerIn(17)(47), B=>TobLayerIn(17)(55), H=>TobLayerOut(17)(47), L=>TobLayerOut(17)(55));
  TobLayerOut(17)(0)<=TobLayerIn(17)(0);
  TobLayerOut(17)(1)<=TobLayerIn(17)(1);
  TobLayerOut(17)(2)<=TobLayerIn(17)(2);
  TobLayerOut(17)(3)<=TobLayerIn(17)(3);
  TobLayerOut(17)(4)<=TobLayerIn(17)(4);
  TobLayerOut(17)(5)<=TobLayerIn(17)(5);
  TobLayerOut(17)(6)<=TobLayerIn(17)(6);
  TobLayerOut(17)(7)<=TobLayerIn(17)(7);
  TobLayerOut(17)(56)<=TobLayerIn(17)(56);
  TobLayerOut(17)(57)<=TobLayerIn(17)(57);
  TobLayerOut(17)(58)<=TobLayerIn(17)(58);
  TobLayerOut(17)(59)<=TobLayerIn(17)(59);
  TobLayerOut(17)(60)<=TobLayerIn(17)(60);
  TobLayerOut(17)(61)<=TobLayerIn(17)(61);
  TobLayerOut(17)(62)<=TobLayerIn(17)(62);
  TobLayerOut(17)(63)<=TobLayerIn(17)(63);


  compExch_Layer_18_to_19_sites_04_08: compExch port map(A=>TobLayerIn(18)(4), B=>TobLayerIn(18)(8), H=>TobLayerOut(18)(4), L=>TobLayerOut(18)(8));
  compExch_Layer_18_to_19_sites_05_09: compExch port map(A=>TobLayerIn(18)(5), B=>TobLayerIn(18)(9), H=>TobLayerOut(18)(5), L=>TobLayerOut(18)(9));
  compExch_Layer_18_to_19_sites_06_10: compExch port map(A=>TobLayerIn(18)(6), B=>TobLayerIn(18)(10), H=>TobLayerOut(18)(6), L=>TobLayerOut(18)(10));
  compExch_Layer_18_to_19_sites_07_11: compExch port map(A=>TobLayerIn(18)(7), B=>TobLayerIn(18)(11), H=>TobLayerOut(18)(7), L=>TobLayerOut(18)(11));
  compExch_Layer_18_to_19_sites_12_16: compExch port map(A=>TobLayerIn(18)(12), B=>TobLayerIn(18)(16), H=>TobLayerOut(18)(12), L=>TobLayerOut(18)(16));
  compExch_Layer_18_to_19_sites_13_17: compExch port map(A=>TobLayerIn(18)(13), B=>TobLayerIn(18)(17), H=>TobLayerOut(18)(13), L=>TobLayerOut(18)(17));
  compExch_Layer_18_to_19_sites_14_18: compExch port map(A=>TobLayerIn(18)(14), B=>TobLayerIn(18)(18), H=>TobLayerOut(18)(14), L=>TobLayerOut(18)(18));
  compExch_Layer_18_to_19_sites_15_19: compExch port map(A=>TobLayerIn(18)(15), B=>TobLayerIn(18)(19), H=>TobLayerOut(18)(15), L=>TobLayerOut(18)(19));
  compExch_Layer_18_to_19_sites_20_24: compExch port map(A=>TobLayerIn(18)(20), B=>TobLayerIn(18)(24), H=>TobLayerOut(18)(20), L=>TobLayerOut(18)(24));
  compExch_Layer_18_to_19_sites_21_25: compExch port map(A=>TobLayerIn(18)(21), B=>TobLayerIn(18)(25), H=>TobLayerOut(18)(21), L=>TobLayerOut(18)(25));
  compExch_Layer_18_to_19_sites_22_26: compExch port map(A=>TobLayerIn(18)(22), B=>TobLayerIn(18)(26), H=>TobLayerOut(18)(22), L=>TobLayerOut(18)(26));
  compExch_Layer_18_to_19_sites_23_27: compExch port map(A=>TobLayerIn(18)(23), B=>TobLayerIn(18)(27), H=>TobLayerOut(18)(23), L=>TobLayerOut(18)(27));
  compExch_Layer_18_to_19_sites_28_32: compExch port map(A=>TobLayerIn(18)(28), B=>TobLayerIn(18)(32), H=>TobLayerOut(18)(28), L=>TobLayerOut(18)(32));
  compExch_Layer_18_to_19_sites_29_33: compExch port map(A=>TobLayerIn(18)(29), B=>TobLayerIn(18)(33), H=>TobLayerOut(18)(29), L=>TobLayerOut(18)(33));
  compExch_Layer_18_to_19_sites_30_34: compExch port map(A=>TobLayerIn(18)(30), B=>TobLayerIn(18)(34), H=>TobLayerOut(18)(30), L=>TobLayerOut(18)(34));
  compExch_Layer_18_to_19_sites_31_35: compExch port map(A=>TobLayerIn(18)(31), B=>TobLayerIn(18)(35), H=>TobLayerOut(18)(31), L=>TobLayerOut(18)(35));
  compExch_Layer_18_to_19_sites_36_40: compExch port map(A=>TobLayerIn(18)(36), B=>TobLayerIn(18)(40), H=>TobLayerOut(18)(36), L=>TobLayerOut(18)(40));
  compExch_Layer_18_to_19_sites_37_41: compExch port map(A=>TobLayerIn(18)(37), B=>TobLayerIn(18)(41), H=>TobLayerOut(18)(37), L=>TobLayerOut(18)(41));
  compExch_Layer_18_to_19_sites_38_42: compExch port map(A=>TobLayerIn(18)(38), B=>TobLayerIn(18)(42), H=>TobLayerOut(18)(38), L=>TobLayerOut(18)(42));
  compExch_Layer_18_to_19_sites_39_43: compExch port map(A=>TobLayerIn(18)(39), B=>TobLayerIn(18)(43), H=>TobLayerOut(18)(39), L=>TobLayerOut(18)(43));
  compExch_Layer_18_to_19_sites_44_48: compExch port map(A=>TobLayerIn(18)(44), B=>TobLayerIn(18)(48), H=>TobLayerOut(18)(44), L=>TobLayerOut(18)(48));
  compExch_Layer_18_to_19_sites_45_49: compExch port map(A=>TobLayerIn(18)(45), B=>TobLayerIn(18)(49), H=>TobLayerOut(18)(45), L=>TobLayerOut(18)(49));
  compExch_Layer_18_to_19_sites_46_50: compExch port map(A=>TobLayerIn(18)(46), B=>TobLayerIn(18)(50), H=>TobLayerOut(18)(46), L=>TobLayerOut(18)(50));
  compExch_Layer_18_to_19_sites_47_51: compExch port map(A=>TobLayerIn(18)(47), B=>TobLayerIn(18)(51), H=>TobLayerOut(18)(47), L=>TobLayerOut(18)(51));
  compExch_Layer_18_to_19_sites_52_56: compExch port map(A=>TobLayerIn(18)(52), B=>TobLayerIn(18)(56), H=>TobLayerOut(18)(52), L=>TobLayerOut(18)(56));
  compExch_Layer_18_to_19_sites_53_57: compExch port map(A=>TobLayerIn(18)(53), B=>TobLayerIn(18)(57), H=>TobLayerOut(18)(53), L=>TobLayerOut(18)(57));
  compExch_Layer_18_to_19_sites_54_58: compExch port map(A=>TobLayerIn(18)(54), B=>TobLayerIn(18)(58), H=>TobLayerOut(18)(54), L=>TobLayerOut(18)(58));
  compExch_Layer_18_to_19_sites_55_59: compExch port map(A=>TobLayerIn(18)(55), B=>TobLayerIn(18)(59), H=>TobLayerOut(18)(55), L=>TobLayerOut(18)(59));
  TobLayerOut(18)(0)<=TobLayerIn(18)(0);
  TobLayerOut(18)(1)<=TobLayerIn(18)(1);
  TobLayerOut(18)(2)<=TobLayerIn(18)(2);
  TobLayerOut(18)(3)<=TobLayerIn(18)(3);
  TobLayerOut(18)(60)<=TobLayerIn(18)(60);
  TobLayerOut(18)(61)<=TobLayerIn(18)(61);
  TobLayerOut(18)(62)<=TobLayerIn(18)(62);
  TobLayerOut(18)(63)<=TobLayerIn(18)(63);


  compExch_Layer_19_to_20_sites_02_04: compExch port map(A=>TobLayerIn(19)(2), B=>TobLayerIn(19)(4), H=>TobLayerOut(19)(2), L=>TobLayerOut(19)(4));
  compExch_Layer_19_to_20_sites_03_05: compExch port map(A=>TobLayerIn(19)(3), B=>TobLayerIn(19)(5), H=>TobLayerOut(19)(3), L=>TobLayerOut(19)(5));
  compExch_Layer_19_to_20_sites_06_08: compExch port map(A=>TobLayerIn(19)(6), B=>TobLayerIn(19)(8), H=>TobLayerOut(19)(6), L=>TobLayerOut(19)(8));
  compExch_Layer_19_to_20_sites_07_09: compExch port map(A=>TobLayerIn(19)(7), B=>TobLayerIn(19)(9), H=>TobLayerOut(19)(7), L=>TobLayerOut(19)(9));
  compExch_Layer_19_to_20_sites_10_12: compExch port map(A=>TobLayerIn(19)(10), B=>TobLayerIn(19)(12), H=>TobLayerOut(19)(10), L=>TobLayerOut(19)(12));
  compExch_Layer_19_to_20_sites_11_13: compExch port map(A=>TobLayerIn(19)(11), B=>TobLayerIn(19)(13), H=>TobLayerOut(19)(11), L=>TobLayerOut(19)(13));
  compExch_Layer_19_to_20_sites_14_16: compExch port map(A=>TobLayerIn(19)(14), B=>TobLayerIn(19)(16), H=>TobLayerOut(19)(14), L=>TobLayerOut(19)(16));
  compExch_Layer_19_to_20_sites_15_17: compExch port map(A=>TobLayerIn(19)(15), B=>TobLayerIn(19)(17), H=>TobLayerOut(19)(15), L=>TobLayerOut(19)(17));
  compExch_Layer_19_to_20_sites_18_20: compExch port map(A=>TobLayerIn(19)(18), B=>TobLayerIn(19)(20), H=>TobLayerOut(19)(18), L=>TobLayerOut(19)(20));
  compExch_Layer_19_to_20_sites_19_21: compExch port map(A=>TobLayerIn(19)(19), B=>TobLayerIn(19)(21), H=>TobLayerOut(19)(19), L=>TobLayerOut(19)(21));
  compExch_Layer_19_to_20_sites_22_24: compExch port map(A=>TobLayerIn(19)(22), B=>TobLayerIn(19)(24), H=>TobLayerOut(19)(22), L=>TobLayerOut(19)(24));
  compExch_Layer_19_to_20_sites_23_25: compExch port map(A=>TobLayerIn(19)(23), B=>TobLayerIn(19)(25), H=>TobLayerOut(19)(23), L=>TobLayerOut(19)(25));
  compExch_Layer_19_to_20_sites_26_28: compExch port map(A=>TobLayerIn(19)(26), B=>TobLayerIn(19)(28), H=>TobLayerOut(19)(26), L=>TobLayerOut(19)(28));
  compExch_Layer_19_to_20_sites_27_29: compExch port map(A=>TobLayerIn(19)(27), B=>TobLayerIn(19)(29), H=>TobLayerOut(19)(27), L=>TobLayerOut(19)(29));
  compExch_Layer_19_to_20_sites_30_32: compExch port map(A=>TobLayerIn(19)(30), B=>TobLayerIn(19)(32), H=>TobLayerOut(19)(30), L=>TobLayerOut(19)(32));
  compExch_Layer_19_to_20_sites_31_33: compExch port map(A=>TobLayerIn(19)(31), B=>TobLayerIn(19)(33), H=>TobLayerOut(19)(31), L=>TobLayerOut(19)(33));
  compExch_Layer_19_to_20_sites_34_36: compExch port map(A=>TobLayerIn(19)(34), B=>TobLayerIn(19)(36), H=>TobLayerOut(19)(34), L=>TobLayerOut(19)(36));
  compExch_Layer_19_to_20_sites_35_37: compExch port map(A=>TobLayerIn(19)(35), B=>TobLayerIn(19)(37), H=>TobLayerOut(19)(35), L=>TobLayerOut(19)(37));
  compExch_Layer_19_to_20_sites_38_40: compExch port map(A=>TobLayerIn(19)(38), B=>TobLayerIn(19)(40), H=>TobLayerOut(19)(38), L=>TobLayerOut(19)(40));
  compExch_Layer_19_to_20_sites_39_41: compExch port map(A=>TobLayerIn(19)(39), B=>TobLayerIn(19)(41), H=>TobLayerOut(19)(39), L=>TobLayerOut(19)(41));
  compExch_Layer_19_to_20_sites_42_44: compExch port map(A=>TobLayerIn(19)(42), B=>TobLayerIn(19)(44), H=>TobLayerOut(19)(42), L=>TobLayerOut(19)(44));
  compExch_Layer_19_to_20_sites_43_45: compExch port map(A=>TobLayerIn(19)(43), B=>TobLayerIn(19)(45), H=>TobLayerOut(19)(43), L=>TobLayerOut(19)(45));
  compExch_Layer_19_to_20_sites_46_48: compExch port map(A=>TobLayerIn(19)(46), B=>TobLayerIn(19)(48), H=>TobLayerOut(19)(46), L=>TobLayerOut(19)(48));
  compExch_Layer_19_to_20_sites_47_49: compExch port map(A=>TobLayerIn(19)(47), B=>TobLayerIn(19)(49), H=>TobLayerOut(19)(47), L=>TobLayerOut(19)(49));
  compExch_Layer_19_to_20_sites_50_52: compExch port map(A=>TobLayerIn(19)(50), B=>TobLayerIn(19)(52), H=>TobLayerOut(19)(50), L=>TobLayerOut(19)(52));
  compExch_Layer_19_to_20_sites_51_53: compExch port map(A=>TobLayerIn(19)(51), B=>TobLayerIn(19)(53), H=>TobLayerOut(19)(51), L=>TobLayerOut(19)(53));
  compExch_Layer_19_to_20_sites_54_56: compExch port map(A=>TobLayerIn(19)(54), B=>TobLayerIn(19)(56), H=>TobLayerOut(19)(54), L=>TobLayerOut(19)(56));
  compExch_Layer_19_to_20_sites_55_57: compExch port map(A=>TobLayerIn(19)(55), B=>TobLayerIn(19)(57), H=>TobLayerOut(19)(55), L=>TobLayerOut(19)(57));
  compExch_Layer_19_to_20_sites_58_60: compExch port map(A=>TobLayerIn(19)(58), B=>TobLayerIn(19)(60), H=>TobLayerOut(19)(58), L=>TobLayerOut(19)(60));
  compExch_Layer_19_to_20_sites_59_61: compExch port map(A=>TobLayerIn(19)(59), B=>TobLayerIn(19)(61), H=>TobLayerOut(19)(59), L=>TobLayerOut(19)(61));
  TobLayerOut(19)(0)<=TobLayerIn(19)(0);
  TobLayerOut(19)(1)<=TobLayerIn(19)(1);
  TobLayerOut(19)(62)<=TobLayerIn(19)(62);
  TobLayerOut(19)(63)<=TobLayerIn(19)(63);


  compExch_Layer_20_to_21_sites_01_02: compExch port map(A=>TobLayerIn(20)(1), B=>TobLayerIn(20)(2), H=>TobLayerOut(20)(1), L=>TobLayerOut(20)(2));
  compExch_Layer_20_to_21_sites_03_04: compExch port map(A=>TobLayerIn(20)(3), B=>TobLayerIn(20)(4), H=>TobLayerOut(20)(3), L=>TobLayerOut(20)(4));
  compExch_Layer_20_to_21_sites_05_06: compExch port map(A=>TobLayerIn(20)(5), B=>TobLayerIn(20)(6), H=>TobLayerOut(20)(5), L=>TobLayerOut(20)(6));
  compExch_Layer_20_to_21_sites_07_08: compExch port map(A=>TobLayerIn(20)(7), B=>TobLayerIn(20)(8), H=>TobLayerOut(20)(7), L=>TobLayerOut(20)(8));
  compExch_Layer_20_to_21_sites_09_10: compExch port map(A=>TobLayerIn(20)(9), B=>TobLayerIn(20)(10), H=>TobLayerOut(20)(9), L=>TobLayerOut(20)(10));
  compExch_Layer_20_to_21_sites_11_12: compExch port map(A=>TobLayerIn(20)(11), B=>TobLayerIn(20)(12), H=>TobLayerOut(20)(11), L=>TobLayerOut(20)(12));
  compExch_Layer_20_to_21_sites_13_14: compExch port map(A=>TobLayerIn(20)(13), B=>TobLayerIn(20)(14), H=>TobLayerOut(20)(13), L=>TobLayerOut(20)(14));
  compExch_Layer_20_to_21_sites_15_16: compExch port map(A=>TobLayerIn(20)(15), B=>TobLayerIn(20)(16), H=>TobLayerOut(20)(15), L=>TobLayerOut(20)(16));
  compExch_Layer_20_to_21_sites_17_18: compExch port map(A=>TobLayerIn(20)(17), B=>TobLayerIn(20)(18), H=>TobLayerOut(20)(17), L=>TobLayerOut(20)(18));
  compExch_Layer_20_to_21_sites_19_20: compExch port map(A=>TobLayerIn(20)(19), B=>TobLayerIn(20)(20), H=>TobLayerOut(20)(19), L=>TobLayerOut(20)(20));
  compExch_Layer_20_to_21_sites_21_22: compExch port map(A=>TobLayerIn(20)(21), B=>TobLayerIn(20)(22), H=>TobLayerOut(20)(21), L=>TobLayerOut(20)(22));
  compExch_Layer_20_to_21_sites_23_24: compExch port map(A=>TobLayerIn(20)(23), B=>TobLayerIn(20)(24), H=>TobLayerOut(20)(23), L=>TobLayerOut(20)(24));
  compExch_Layer_20_to_21_sites_25_26: compExch port map(A=>TobLayerIn(20)(25), B=>TobLayerIn(20)(26), H=>TobLayerOut(20)(25), L=>TobLayerOut(20)(26));
  compExch_Layer_20_to_21_sites_27_28: compExch port map(A=>TobLayerIn(20)(27), B=>TobLayerIn(20)(28), H=>TobLayerOut(20)(27), L=>TobLayerOut(20)(28));
  compExch_Layer_20_to_21_sites_29_30: compExch port map(A=>TobLayerIn(20)(29), B=>TobLayerIn(20)(30), H=>TobLayerOut(20)(29), L=>TobLayerOut(20)(30));
  compExch_Layer_20_to_21_sites_31_32: compExch port map(A=>TobLayerIn(20)(31), B=>TobLayerIn(20)(32), H=>TobLayerOut(20)(31), L=>TobLayerOut(20)(32));
  compExch_Layer_20_to_21_sites_33_34: compExch port map(A=>TobLayerIn(20)(33), B=>TobLayerIn(20)(34), H=>TobLayerOut(20)(33), L=>TobLayerOut(20)(34));
  compExch_Layer_20_to_21_sites_35_36: compExch port map(A=>TobLayerIn(20)(35), B=>TobLayerIn(20)(36), H=>TobLayerOut(20)(35), L=>TobLayerOut(20)(36));
  compExch_Layer_20_to_21_sites_37_38: compExch port map(A=>TobLayerIn(20)(37), B=>TobLayerIn(20)(38), H=>TobLayerOut(20)(37), L=>TobLayerOut(20)(38));
  compExch_Layer_20_to_21_sites_39_40: compExch port map(A=>TobLayerIn(20)(39), B=>TobLayerIn(20)(40), H=>TobLayerOut(20)(39), L=>TobLayerOut(20)(40));
  compExch_Layer_20_to_21_sites_41_42: compExch port map(A=>TobLayerIn(20)(41), B=>TobLayerIn(20)(42), H=>TobLayerOut(20)(41), L=>TobLayerOut(20)(42));
  compExch_Layer_20_to_21_sites_43_44: compExch port map(A=>TobLayerIn(20)(43), B=>TobLayerIn(20)(44), H=>TobLayerOut(20)(43), L=>TobLayerOut(20)(44));
  compExch_Layer_20_to_21_sites_45_46: compExch port map(A=>TobLayerIn(20)(45), B=>TobLayerIn(20)(46), H=>TobLayerOut(20)(45), L=>TobLayerOut(20)(46));
  compExch_Layer_20_to_21_sites_47_48: compExch port map(A=>TobLayerIn(20)(47), B=>TobLayerIn(20)(48), H=>TobLayerOut(20)(47), L=>TobLayerOut(20)(48));
  compExch_Layer_20_to_21_sites_49_50: compExch port map(A=>TobLayerIn(20)(49), B=>TobLayerIn(20)(50), H=>TobLayerOut(20)(49), L=>TobLayerOut(20)(50));
  compExch_Layer_20_to_21_sites_51_52: compExch port map(A=>TobLayerIn(20)(51), B=>TobLayerIn(20)(52), H=>TobLayerOut(20)(51), L=>TobLayerOut(20)(52));
  compExch_Layer_20_to_21_sites_53_54: compExch port map(A=>TobLayerIn(20)(53), B=>TobLayerIn(20)(54), H=>TobLayerOut(20)(53), L=>TobLayerOut(20)(54));
  compExch_Layer_20_to_21_sites_55_56: compExch port map(A=>TobLayerIn(20)(55), B=>TobLayerIn(20)(56), H=>TobLayerOut(20)(55), L=>TobLayerOut(20)(56));
  compExch_Layer_20_to_21_sites_57_58: compExch port map(A=>TobLayerIn(20)(57), B=>TobLayerIn(20)(58), H=>TobLayerOut(20)(57), L=>TobLayerOut(20)(58));
  compExch_Layer_20_to_21_sites_59_60: compExch port map(A=>TobLayerIn(20)(59), B=>TobLayerIn(20)(60), H=>TobLayerOut(20)(59), L=>TobLayerOut(20)(60));
  compExch_Layer_20_to_21_sites_61_62: compExch port map(A=>TobLayerIn(20)(61), B=>TobLayerIn(20)(62), H=>TobLayerOut(20)(61), L=>TobLayerOut(20)(62));
  TobLayerOut(20)(0)<=TobLayerIn(20)(0);
  TobLayerOut(20)(63)<=TobLayerIn(20)(63);


  TobLayerOut(15)(64)<=TobLayerIn(15)(64);
  TobLayerOut(15)(65)<=TobLayerIn(15)(65);
  TobLayerOut(15)(66)<=TobLayerIn(15)(66);
  TobLayerOut(15)(67)<=TobLayerIn(15)(67);
  TobLayerOut(15)(68)<=TobLayerIn(15)(68);
  TobLayerOut(15)(69)<=TobLayerIn(15)(69);

  TobLayerOut(16)(64)<=TobLayerIn(16)(64);
  TobLayerOut(16)(65)<=TobLayerIn(16)(65);
  TobLayerOut(16)(66)<=TobLayerIn(16)(66);
  TobLayerOut(16)(67)<=TobLayerIn(16)(67);
  TobLayerOut(16)(68)<=TobLayerIn(16)(68);
  TobLayerOut(16)(69)<=TobLayerIn(16)(69);

  TobLayerOut(17)(64)<=TobLayerIn(17)(64);
  TobLayerOut(17)(65)<=TobLayerIn(17)(65);
  TobLayerOut(17)(66)<=TobLayerIn(17)(66);
  TobLayerOut(17)(67)<=TobLayerIn(17)(67);
  TobLayerOut(17)(68)<=TobLayerIn(17)(68);
  TobLayerOut(17)(69)<=TobLayerIn(17)(69);

  TobLayerOut(18)(64)<=TobLayerIn(18)(64);
  TobLayerOut(18)(65)<=TobLayerIn(18)(65);
  TobLayerOut(18)(66)<=TobLayerIn(18)(66);
  TobLayerOut(18)(67)<=TobLayerIn(18)(67);
  TobLayerOut(18)(68)<=TobLayerIn(18)(68);
  TobLayerOut(18)(69)<=TobLayerIn(18)(69);

  compExch_Layer_19_to_20_sites_66_68: compExch port map(A=>TobLayerIn(19)(66), B=>TobLayerIn(19)(68), H=>TobLayerOut(19)(66), L=>TobLayerOut(19)(68));
  compExch_Layer_19_to_20_sites_67_69: compExch port map(A=>TobLayerIn(19)(67), B=>TobLayerIn(19)(69), H=>TobLayerOut(19)(67), L=>TobLayerOut(19)(69));
  TobLayerOut(19)(64)<=TobLayerIn(19)(64);
  TobLayerOut(19)(65)<=TobLayerIn(19)(65);


  compExch_Layer_20_to_21_sites_65_66: compExch port map(A=>TobLayerIn(20)(65), B=>TobLayerIn(20)(66), H=>TobLayerOut(20)(65), L=>TobLayerOut(20)(66));
  compExch_Layer_20_to_21_sites_67_68: compExch port map(A=>TobLayerIn(20)(67), B=>TobLayerIn(20)(68), H=>TobLayerOut(20)(67), L=>TobLayerOut(20)(68));
  TobLayerOut(20)(64)<=TobLayerIn(20)(64);
  TobLayerOut(20)(69)<=TobLayerIn(20)(69);


  compExch_Layer_21_to_22_sites_00_64: compExch port map(A=>TobLayerIn(21)(0), B=>TobLayerIn(21)(64), H=>TobLayerOut(21)(0), L=>TobLayerOut(21)(64));
  compExch_Layer_21_to_22_sites_01_65: compExch port map(A=>TobLayerIn(21)(1), B=>TobLayerIn(21)(65), H=>TobLayerOut(21)(1), L=>TobLayerOut(21)(65));
  compExch_Layer_21_to_22_sites_02_66: compExch port map(A=>TobLayerIn(21)(2), B=>TobLayerIn(21)(66), H=>TobLayerOut(21)(2), L=>TobLayerOut(21)(66));
  compExch_Layer_21_to_22_sites_03_67: compExch port map(A=>TobLayerIn(21)(3), B=>TobLayerIn(21)(67), H=>TobLayerOut(21)(3), L=>TobLayerOut(21)(67));
  compExch_Layer_21_to_22_sites_04_68: compExch port map(A=>TobLayerIn(21)(4), B=>TobLayerIn(21)(68), H=>TobLayerOut(21)(4), L=>TobLayerOut(21)(68));
  compExch_Layer_21_to_22_sites_05_69: compExch port map(A=>TobLayerIn(21)(5), B=>TobLayerIn(21)(69), H=>TobLayerOut(21)(5), L=>TobLayerOut(21)(69));
  TobLayerOut(21)(6)<=TobLayerIn(21)(6);
  TobLayerOut(21)(7)<=TobLayerIn(21)(7);
  TobLayerOut(21)(8)<=TobLayerIn(21)(8);
  TobLayerOut(21)(9)<=TobLayerIn(21)(9);
  TobLayerOut(21)(10)<=TobLayerIn(21)(10);
  TobLayerOut(21)(11)<=TobLayerIn(21)(11);
  TobLayerOut(21)(12)<=TobLayerIn(21)(12);
  TobLayerOut(21)(13)<=TobLayerIn(21)(13);
  TobLayerOut(21)(14)<=TobLayerIn(21)(14);
  TobLayerOut(21)(15)<=TobLayerIn(21)(15);
  TobLayerOut(21)(16)<=TobLayerIn(21)(16);
  TobLayerOut(21)(17)<=TobLayerIn(21)(17);
  TobLayerOut(21)(18)<=TobLayerIn(21)(18);
  TobLayerOut(21)(19)<=TobLayerIn(21)(19);
  TobLayerOut(21)(20)<=TobLayerIn(21)(20);
  TobLayerOut(21)(21)<=TobLayerIn(21)(21);
  TobLayerOut(21)(22)<=TobLayerIn(21)(22);
  TobLayerOut(21)(23)<=TobLayerIn(21)(23);
  TobLayerOut(21)(24)<=TobLayerIn(21)(24);
  TobLayerOut(21)(25)<=TobLayerIn(21)(25);
  TobLayerOut(21)(26)<=TobLayerIn(21)(26);
  TobLayerOut(21)(27)<=TobLayerIn(21)(27);
  TobLayerOut(21)(28)<=TobLayerIn(21)(28);
  TobLayerOut(21)(29)<=TobLayerIn(21)(29);
  TobLayerOut(21)(30)<=TobLayerIn(21)(30);
  TobLayerOut(21)(31)<=TobLayerIn(21)(31);
  TobLayerOut(21)(32)<=TobLayerIn(21)(32);
  TobLayerOut(21)(33)<=TobLayerIn(21)(33);
  TobLayerOut(21)(34)<=TobLayerIn(21)(34);
  TobLayerOut(21)(35)<=TobLayerIn(21)(35);
  TobLayerOut(21)(36)<=TobLayerIn(21)(36);
  TobLayerOut(21)(37)<=TobLayerIn(21)(37);
  TobLayerOut(21)(38)<=TobLayerIn(21)(38);
  TobLayerOut(21)(39)<=TobLayerIn(21)(39);
  TobLayerOut(21)(40)<=TobLayerIn(21)(40);
  TobLayerOut(21)(41)<=TobLayerIn(21)(41);
  TobLayerOut(21)(42)<=TobLayerIn(21)(42);
  TobLayerOut(21)(43)<=TobLayerIn(21)(43);
  TobLayerOut(21)(44)<=TobLayerIn(21)(44);
  TobLayerOut(21)(45)<=TobLayerIn(21)(45);
  TobLayerOut(21)(46)<=TobLayerIn(21)(46);
  TobLayerOut(21)(47)<=TobLayerIn(21)(47);
  TobLayerOut(21)(48)<=TobLayerIn(21)(48);
  TobLayerOut(21)(49)<=TobLayerIn(21)(49);
  TobLayerOut(21)(50)<=TobLayerIn(21)(50);
  TobLayerOut(21)(51)<=TobLayerIn(21)(51);
  TobLayerOut(21)(52)<=TobLayerIn(21)(52);
  TobLayerOut(21)(53)<=TobLayerIn(21)(53);
  TobLayerOut(21)(54)<=TobLayerIn(21)(54);
  TobLayerOut(21)(55)<=TobLayerIn(21)(55);
  TobLayerOut(21)(56)<=TobLayerIn(21)(56);
  TobLayerOut(21)(57)<=TobLayerIn(21)(57);
  TobLayerOut(21)(58)<=TobLayerIn(21)(58);
  TobLayerOut(21)(59)<=TobLayerIn(21)(59);
  TobLayerOut(21)(60)<=TobLayerIn(21)(60);
  TobLayerOut(21)(61)<=TobLayerIn(21)(61);
  TobLayerOut(21)(62)<=TobLayerIn(21)(62);
  TobLayerOut(21)(63)<=TobLayerIn(21)(63);


  compExch_Layer_22_to_23_sites_32_64: compExch port map(A=>TobLayerIn(22)(32), B=>TobLayerIn(22)(64), H=>TobLayerOut(22)(32), L=>TobLayerOut(22)(64));
  compExch_Layer_22_to_23_sites_33_65: compExch port map(A=>TobLayerIn(22)(33), B=>TobLayerIn(22)(65), H=>TobLayerOut(22)(33), L=>TobLayerOut(22)(65));
  compExch_Layer_22_to_23_sites_34_66: compExch port map(A=>TobLayerIn(22)(34), B=>TobLayerIn(22)(66), H=>TobLayerOut(22)(34), L=>TobLayerOut(22)(66));
  compExch_Layer_22_to_23_sites_35_67: compExch port map(A=>TobLayerIn(22)(35), B=>TobLayerIn(22)(67), H=>TobLayerOut(22)(35), L=>TobLayerOut(22)(67));
  compExch_Layer_22_to_23_sites_36_68: compExch port map(A=>TobLayerIn(22)(36), B=>TobLayerIn(22)(68), H=>TobLayerOut(22)(36), L=>TobLayerOut(22)(68));
  compExch_Layer_22_to_23_sites_37_69: compExch port map(A=>TobLayerIn(22)(37), B=>TobLayerIn(22)(69), H=>TobLayerOut(22)(37), L=>TobLayerOut(22)(69));
  TobLayerOut(22)(0)<=TobLayerIn(22)(0);
  TobLayerOut(22)(1)<=TobLayerIn(22)(1);
  TobLayerOut(22)(2)<=TobLayerIn(22)(2);
  TobLayerOut(22)(3)<=TobLayerIn(22)(3);
  TobLayerOut(22)(4)<=TobLayerIn(22)(4);
  TobLayerOut(22)(5)<=TobLayerIn(22)(5);
  TobLayerOut(22)(6)<=TobLayerIn(22)(6);
  TobLayerOut(22)(7)<=TobLayerIn(22)(7);
  TobLayerOut(22)(8)<=TobLayerIn(22)(8);
  TobLayerOut(22)(9)<=TobLayerIn(22)(9);
  TobLayerOut(22)(10)<=TobLayerIn(22)(10);
  TobLayerOut(22)(11)<=TobLayerIn(22)(11);
  TobLayerOut(22)(12)<=TobLayerIn(22)(12);
  TobLayerOut(22)(13)<=TobLayerIn(22)(13);
  TobLayerOut(22)(14)<=TobLayerIn(22)(14);
  TobLayerOut(22)(15)<=TobLayerIn(22)(15);
  TobLayerOut(22)(16)<=TobLayerIn(22)(16);
  TobLayerOut(22)(17)<=TobLayerIn(22)(17);
  TobLayerOut(22)(18)<=TobLayerIn(22)(18);
  TobLayerOut(22)(19)<=TobLayerIn(22)(19);
  TobLayerOut(22)(20)<=TobLayerIn(22)(20);
  TobLayerOut(22)(21)<=TobLayerIn(22)(21);
  TobLayerOut(22)(22)<=TobLayerIn(22)(22);
  TobLayerOut(22)(23)<=TobLayerIn(22)(23);
  TobLayerOut(22)(24)<=TobLayerIn(22)(24);
  TobLayerOut(22)(25)<=TobLayerIn(22)(25);
  TobLayerOut(22)(26)<=TobLayerIn(22)(26);
  TobLayerOut(22)(27)<=TobLayerIn(22)(27);
  TobLayerOut(22)(28)<=TobLayerIn(22)(28);
  TobLayerOut(22)(29)<=TobLayerIn(22)(29);
  TobLayerOut(22)(30)<=TobLayerIn(22)(30);
  TobLayerOut(22)(31)<=TobLayerIn(22)(31);
  TobLayerOut(22)(38)<=TobLayerIn(22)(38);
  TobLayerOut(22)(39)<=TobLayerIn(22)(39);
  TobLayerOut(22)(40)<=TobLayerIn(22)(40);
  TobLayerOut(22)(41)<=TobLayerIn(22)(41);
  TobLayerOut(22)(42)<=TobLayerIn(22)(42);
  TobLayerOut(22)(43)<=TobLayerIn(22)(43);
  TobLayerOut(22)(44)<=TobLayerIn(22)(44);
  TobLayerOut(22)(45)<=TobLayerIn(22)(45);
  TobLayerOut(22)(46)<=TobLayerIn(22)(46);
  TobLayerOut(22)(47)<=TobLayerIn(22)(47);
  TobLayerOut(22)(48)<=TobLayerIn(22)(48);
  TobLayerOut(22)(49)<=TobLayerIn(22)(49);
  TobLayerOut(22)(50)<=TobLayerIn(22)(50);
  TobLayerOut(22)(51)<=TobLayerIn(22)(51);
  TobLayerOut(22)(52)<=TobLayerIn(22)(52);
  TobLayerOut(22)(53)<=TobLayerIn(22)(53);
  TobLayerOut(22)(54)<=TobLayerIn(22)(54);
  TobLayerOut(22)(55)<=TobLayerIn(22)(55);
  TobLayerOut(22)(56)<=TobLayerIn(22)(56);
  TobLayerOut(22)(57)<=TobLayerIn(22)(57);
  TobLayerOut(22)(58)<=TobLayerIn(22)(58);
  TobLayerOut(22)(59)<=TobLayerIn(22)(59);
  TobLayerOut(22)(60)<=TobLayerIn(22)(60);
  TobLayerOut(22)(61)<=TobLayerIn(22)(61);
  TobLayerOut(22)(62)<=TobLayerIn(22)(62);
  TobLayerOut(22)(63)<=TobLayerIn(22)(63);


  compExch_Layer_23_to_24_sites_16_32: compExch port map(A=>TobLayerIn(23)(16), B=>TobLayerIn(23)(32), H=>TobLayerOut(23)(16), L=>TobLayerOut(23)(32));
  compExch_Layer_23_to_24_sites_17_33: compExch port map(A=>TobLayerIn(23)(17), B=>TobLayerIn(23)(33), H=>TobLayerOut(23)(17), L=>TobLayerOut(23)(33));
  compExch_Layer_23_to_24_sites_18_34: compExch port map(A=>TobLayerIn(23)(18), B=>TobLayerIn(23)(34), H=>TobLayerOut(23)(18), L=>TobLayerOut(23)(34));
  compExch_Layer_23_to_24_sites_19_35: compExch port map(A=>TobLayerIn(23)(19), B=>TobLayerIn(23)(35), H=>TobLayerOut(23)(19), L=>TobLayerOut(23)(35));
  compExch_Layer_23_to_24_sites_20_36: compExch port map(A=>TobLayerIn(23)(20), B=>TobLayerIn(23)(36), H=>TobLayerOut(23)(20), L=>TobLayerOut(23)(36));
  compExch_Layer_23_to_24_sites_21_37: compExch port map(A=>TobLayerIn(23)(21), B=>TobLayerIn(23)(37), H=>TobLayerOut(23)(21), L=>TobLayerOut(23)(37));
  compExch_Layer_23_to_24_sites_22_38: compExch port map(A=>TobLayerIn(23)(22), B=>TobLayerIn(23)(38), H=>TobLayerOut(23)(22), L=>TobLayerOut(23)(38));
  compExch_Layer_23_to_24_sites_23_39: compExch port map(A=>TobLayerIn(23)(23), B=>TobLayerIn(23)(39), H=>TobLayerOut(23)(23), L=>TobLayerOut(23)(39));
  compExch_Layer_23_to_24_sites_24_40: compExch port map(A=>TobLayerIn(23)(24), B=>TobLayerIn(23)(40), H=>TobLayerOut(23)(24), L=>TobLayerOut(23)(40));
  compExch_Layer_23_to_24_sites_25_41: compExch port map(A=>TobLayerIn(23)(25), B=>TobLayerIn(23)(41), H=>TobLayerOut(23)(25), L=>TobLayerOut(23)(41));
  compExch_Layer_23_to_24_sites_26_42: compExch port map(A=>TobLayerIn(23)(26), B=>TobLayerIn(23)(42), H=>TobLayerOut(23)(26), L=>TobLayerOut(23)(42));
  compExch_Layer_23_to_24_sites_27_43: compExch port map(A=>TobLayerIn(23)(27), B=>TobLayerIn(23)(43), H=>TobLayerOut(23)(27), L=>TobLayerOut(23)(43));
  compExch_Layer_23_to_24_sites_28_44: compExch port map(A=>TobLayerIn(23)(28), B=>TobLayerIn(23)(44), H=>TobLayerOut(23)(28), L=>TobLayerOut(23)(44));
  compExch_Layer_23_to_24_sites_29_45: compExch port map(A=>TobLayerIn(23)(29), B=>TobLayerIn(23)(45), H=>TobLayerOut(23)(29), L=>TobLayerOut(23)(45));
  compExch_Layer_23_to_24_sites_30_46: compExch port map(A=>TobLayerIn(23)(30), B=>TobLayerIn(23)(46), H=>TobLayerOut(23)(30), L=>TobLayerOut(23)(46));
  compExch_Layer_23_to_24_sites_31_47: compExch port map(A=>TobLayerIn(23)(31), B=>TobLayerIn(23)(47), H=>TobLayerOut(23)(31), L=>TobLayerOut(23)(47));
  compExch_Layer_23_to_24_sites_48_64: compExch port map(A=>TobLayerIn(23)(48), B=>TobLayerIn(23)(64), H=>TobLayerOut(23)(48), L=>TobLayerOut(23)(64));
  compExch_Layer_23_to_24_sites_49_65: compExch port map(A=>TobLayerIn(23)(49), B=>TobLayerIn(23)(65), H=>TobLayerOut(23)(49), L=>TobLayerOut(23)(65));
  compExch_Layer_23_to_24_sites_50_66: compExch port map(A=>TobLayerIn(23)(50), B=>TobLayerIn(23)(66), H=>TobLayerOut(23)(50), L=>TobLayerOut(23)(66));
  compExch_Layer_23_to_24_sites_51_67: compExch port map(A=>TobLayerIn(23)(51), B=>TobLayerIn(23)(67), H=>TobLayerOut(23)(51), L=>TobLayerOut(23)(67));
  compExch_Layer_23_to_24_sites_52_68: compExch port map(A=>TobLayerIn(23)(52), B=>TobLayerIn(23)(68), H=>TobLayerOut(23)(52), L=>TobLayerOut(23)(68));
  compExch_Layer_23_to_24_sites_53_69: compExch port map(A=>TobLayerIn(23)(53), B=>TobLayerIn(23)(69), H=>TobLayerOut(23)(53), L=>TobLayerOut(23)(69));
  TobLayerOut(23)(0)<=TobLayerIn(23)(0);
  TobLayerOut(23)(1)<=TobLayerIn(23)(1);
  TobLayerOut(23)(2)<=TobLayerIn(23)(2);
  TobLayerOut(23)(3)<=TobLayerIn(23)(3);
  TobLayerOut(23)(4)<=TobLayerIn(23)(4);
  TobLayerOut(23)(5)<=TobLayerIn(23)(5);
  TobLayerOut(23)(6)<=TobLayerIn(23)(6);
  TobLayerOut(23)(7)<=TobLayerIn(23)(7);
  TobLayerOut(23)(8)<=TobLayerIn(23)(8);
  TobLayerOut(23)(9)<=TobLayerIn(23)(9);
  TobLayerOut(23)(10)<=TobLayerIn(23)(10);
  TobLayerOut(23)(11)<=TobLayerIn(23)(11);
  TobLayerOut(23)(12)<=TobLayerIn(23)(12);
  TobLayerOut(23)(13)<=TobLayerIn(23)(13);
  TobLayerOut(23)(14)<=TobLayerIn(23)(14);
  TobLayerOut(23)(15)<=TobLayerIn(23)(15);
  TobLayerOut(23)(54)<=TobLayerIn(23)(54);
  TobLayerOut(23)(55)<=TobLayerIn(23)(55);
  TobLayerOut(23)(56)<=TobLayerIn(23)(56);
  TobLayerOut(23)(57)<=TobLayerIn(23)(57);
  TobLayerOut(23)(58)<=TobLayerIn(23)(58);
  TobLayerOut(23)(59)<=TobLayerIn(23)(59);
  TobLayerOut(23)(60)<=TobLayerIn(23)(60);
  TobLayerOut(23)(61)<=TobLayerIn(23)(61);
  TobLayerOut(23)(62)<=TobLayerIn(23)(62);
  TobLayerOut(23)(63)<=TobLayerIn(23)(63);


  compExch_Layer_24_to_25_sites_08_16: compExch port map(A=>TobLayerIn(24)(8), B=>TobLayerIn(24)(16), H=>TobLayerOut(24)(8), L=>TobLayerOut(24)(16));
  compExch_Layer_24_to_25_sites_09_17: compExch port map(A=>TobLayerIn(24)(9), B=>TobLayerIn(24)(17), H=>TobLayerOut(24)(9), L=>TobLayerOut(24)(17));
  compExch_Layer_24_to_25_sites_10_18: compExch port map(A=>TobLayerIn(24)(10), B=>TobLayerIn(24)(18), H=>TobLayerOut(24)(10), L=>TobLayerOut(24)(18));
  compExch_Layer_24_to_25_sites_11_19: compExch port map(A=>TobLayerIn(24)(11), B=>TobLayerIn(24)(19), H=>TobLayerOut(24)(11), L=>TobLayerOut(24)(19));
  compExch_Layer_24_to_25_sites_12_20: compExch port map(A=>TobLayerIn(24)(12), B=>TobLayerIn(24)(20), H=>TobLayerOut(24)(12), L=>TobLayerOut(24)(20));
  compExch_Layer_24_to_25_sites_13_21: compExch port map(A=>TobLayerIn(24)(13), B=>TobLayerIn(24)(21), H=>TobLayerOut(24)(13), L=>TobLayerOut(24)(21));
  compExch_Layer_24_to_25_sites_14_22: compExch port map(A=>TobLayerIn(24)(14), B=>TobLayerIn(24)(22), H=>TobLayerOut(24)(14), L=>TobLayerOut(24)(22));
  compExch_Layer_24_to_25_sites_15_23: compExch port map(A=>TobLayerIn(24)(15), B=>TobLayerIn(24)(23), H=>TobLayerOut(24)(15), L=>TobLayerOut(24)(23));
  compExch_Layer_24_to_25_sites_24_32: compExch port map(A=>TobLayerIn(24)(24), B=>TobLayerIn(24)(32), H=>TobLayerOut(24)(24), L=>TobLayerOut(24)(32));
  compExch_Layer_24_to_25_sites_25_33: compExch port map(A=>TobLayerIn(24)(25), B=>TobLayerIn(24)(33), H=>TobLayerOut(24)(25), L=>TobLayerOut(24)(33));
  compExch_Layer_24_to_25_sites_26_34: compExch port map(A=>TobLayerIn(24)(26), B=>TobLayerIn(24)(34), H=>TobLayerOut(24)(26), L=>TobLayerOut(24)(34));
  compExch_Layer_24_to_25_sites_27_35: compExch port map(A=>TobLayerIn(24)(27), B=>TobLayerIn(24)(35), H=>TobLayerOut(24)(27), L=>TobLayerOut(24)(35));
  compExch_Layer_24_to_25_sites_28_36: compExch port map(A=>TobLayerIn(24)(28), B=>TobLayerIn(24)(36), H=>TobLayerOut(24)(28), L=>TobLayerOut(24)(36));
  compExch_Layer_24_to_25_sites_29_37: compExch port map(A=>TobLayerIn(24)(29), B=>TobLayerIn(24)(37), H=>TobLayerOut(24)(29), L=>TobLayerOut(24)(37));
  compExch_Layer_24_to_25_sites_30_38: compExch port map(A=>TobLayerIn(24)(30), B=>TobLayerIn(24)(38), H=>TobLayerOut(24)(30), L=>TobLayerOut(24)(38));
  compExch_Layer_24_to_25_sites_31_39: compExch port map(A=>TobLayerIn(24)(31), B=>TobLayerIn(24)(39), H=>TobLayerOut(24)(31), L=>TobLayerOut(24)(39));
  compExch_Layer_24_to_25_sites_40_48: compExch port map(A=>TobLayerIn(24)(40), B=>TobLayerIn(24)(48), H=>TobLayerOut(24)(40), L=>TobLayerOut(24)(48));
  compExch_Layer_24_to_25_sites_41_49: compExch port map(A=>TobLayerIn(24)(41), B=>TobLayerIn(24)(49), H=>TobLayerOut(24)(41), L=>TobLayerOut(24)(49));
  compExch_Layer_24_to_25_sites_42_50: compExch port map(A=>TobLayerIn(24)(42), B=>TobLayerIn(24)(50), H=>TobLayerOut(24)(42), L=>TobLayerOut(24)(50));
  compExch_Layer_24_to_25_sites_43_51: compExch port map(A=>TobLayerIn(24)(43), B=>TobLayerIn(24)(51), H=>TobLayerOut(24)(43), L=>TobLayerOut(24)(51));
  compExch_Layer_24_to_25_sites_44_52: compExch port map(A=>TobLayerIn(24)(44), B=>TobLayerIn(24)(52), H=>TobLayerOut(24)(44), L=>TobLayerOut(24)(52));
  compExch_Layer_24_to_25_sites_45_53: compExch port map(A=>TobLayerIn(24)(45), B=>TobLayerIn(24)(53), H=>TobLayerOut(24)(45), L=>TobLayerOut(24)(53));
  compExch_Layer_24_to_25_sites_46_54: compExch port map(A=>TobLayerIn(24)(46), B=>TobLayerIn(24)(54), H=>TobLayerOut(24)(46), L=>TobLayerOut(24)(54));
  compExch_Layer_24_to_25_sites_47_55: compExch port map(A=>TobLayerIn(24)(47), B=>TobLayerIn(24)(55), H=>TobLayerOut(24)(47), L=>TobLayerOut(24)(55));
  compExch_Layer_24_to_25_sites_56_64: compExch port map(A=>TobLayerIn(24)(56), B=>TobLayerIn(24)(64), H=>TobLayerOut(24)(56), L=>TobLayerOut(24)(64));
  compExch_Layer_24_to_25_sites_57_65: compExch port map(A=>TobLayerIn(24)(57), B=>TobLayerIn(24)(65), H=>TobLayerOut(24)(57), L=>TobLayerOut(24)(65));
  compExch_Layer_24_to_25_sites_58_66: compExch port map(A=>TobLayerIn(24)(58), B=>TobLayerIn(24)(66), H=>TobLayerOut(24)(58), L=>TobLayerOut(24)(66));
  compExch_Layer_24_to_25_sites_59_67: compExch port map(A=>TobLayerIn(24)(59), B=>TobLayerIn(24)(67), H=>TobLayerOut(24)(59), L=>TobLayerOut(24)(67));
  compExch_Layer_24_to_25_sites_60_68: compExch port map(A=>TobLayerIn(24)(60), B=>TobLayerIn(24)(68), H=>TobLayerOut(24)(60), L=>TobLayerOut(24)(68));
  compExch_Layer_24_to_25_sites_61_69: compExch port map(A=>TobLayerIn(24)(61), B=>TobLayerIn(24)(69), H=>TobLayerOut(24)(61), L=>TobLayerOut(24)(69));
  TobLayerOut(24)(0)<=TobLayerIn(24)(0);
  TobLayerOut(24)(1)<=TobLayerIn(24)(1);
  TobLayerOut(24)(2)<=TobLayerIn(24)(2);
  TobLayerOut(24)(3)<=TobLayerIn(24)(3);
  TobLayerOut(24)(4)<=TobLayerIn(24)(4);
  TobLayerOut(24)(5)<=TobLayerIn(24)(5);
  TobLayerOut(24)(6)<=TobLayerIn(24)(6);
  TobLayerOut(24)(7)<=TobLayerIn(24)(7);
  TobLayerOut(24)(62)<=TobLayerIn(24)(62);
  TobLayerOut(24)(63)<=TobLayerIn(24)(63);


  compExch_Layer_25_to_26_sites_04_08: compExch port map(A=>TobLayerIn(25)(4), B=>TobLayerIn(25)(8), H=>TobLayerOut(25)(4), L=>TobLayerOut(25)(8));
  compExch_Layer_25_to_26_sites_05_09: compExch port map(A=>TobLayerIn(25)(5), B=>TobLayerIn(25)(9), H=>TobLayerOut(25)(5), L=>TobLayerOut(25)(9));
  compExch_Layer_25_to_26_sites_06_10: compExch port map(A=>TobLayerIn(25)(6), B=>TobLayerIn(25)(10), H=>TobLayerOut(25)(6), L=>TobLayerOut(25)(10));
  compExch_Layer_25_to_26_sites_07_11: compExch port map(A=>TobLayerIn(25)(7), B=>TobLayerIn(25)(11), H=>TobLayerOut(25)(7), L=>TobLayerOut(25)(11));
  compExch_Layer_25_to_26_sites_12_16: compExch port map(A=>TobLayerIn(25)(12), B=>TobLayerIn(25)(16), H=>TobLayerOut(25)(12), L=>TobLayerOut(25)(16));
  compExch_Layer_25_to_26_sites_13_17: compExch port map(A=>TobLayerIn(25)(13), B=>TobLayerIn(25)(17), H=>TobLayerOut(25)(13), L=>TobLayerOut(25)(17));
  compExch_Layer_25_to_26_sites_14_18: compExch port map(A=>TobLayerIn(25)(14), B=>TobLayerIn(25)(18), H=>TobLayerOut(25)(14), L=>TobLayerOut(25)(18));
  compExch_Layer_25_to_26_sites_15_19: compExch port map(A=>TobLayerIn(25)(15), B=>TobLayerIn(25)(19), H=>TobLayerOut(25)(15), L=>TobLayerOut(25)(19));
  compExch_Layer_25_to_26_sites_20_24: compExch port map(A=>TobLayerIn(25)(20), B=>TobLayerIn(25)(24), H=>TobLayerOut(25)(20), L=>TobLayerOut(25)(24));
  compExch_Layer_25_to_26_sites_21_25: compExch port map(A=>TobLayerIn(25)(21), B=>TobLayerIn(25)(25), H=>TobLayerOut(25)(21), L=>TobLayerOut(25)(25));
  compExch_Layer_25_to_26_sites_22_26: compExch port map(A=>TobLayerIn(25)(22), B=>TobLayerIn(25)(26), H=>TobLayerOut(25)(22), L=>TobLayerOut(25)(26));
  compExch_Layer_25_to_26_sites_23_27: compExch port map(A=>TobLayerIn(25)(23), B=>TobLayerIn(25)(27), H=>TobLayerOut(25)(23), L=>TobLayerOut(25)(27));
  compExch_Layer_25_to_26_sites_28_32: compExch port map(A=>TobLayerIn(25)(28), B=>TobLayerIn(25)(32), H=>TobLayerOut(25)(28), L=>TobLayerOut(25)(32));
  compExch_Layer_25_to_26_sites_29_33: compExch port map(A=>TobLayerIn(25)(29), B=>TobLayerIn(25)(33), H=>TobLayerOut(25)(29), L=>TobLayerOut(25)(33));
  compExch_Layer_25_to_26_sites_30_34: compExch port map(A=>TobLayerIn(25)(30), B=>TobLayerIn(25)(34), H=>TobLayerOut(25)(30), L=>TobLayerOut(25)(34));
  compExch_Layer_25_to_26_sites_31_35: compExch port map(A=>TobLayerIn(25)(31), B=>TobLayerIn(25)(35), H=>TobLayerOut(25)(31), L=>TobLayerOut(25)(35));
  compExch_Layer_25_to_26_sites_36_40: compExch port map(A=>TobLayerIn(25)(36), B=>TobLayerIn(25)(40), H=>TobLayerOut(25)(36), L=>TobLayerOut(25)(40));
  compExch_Layer_25_to_26_sites_37_41: compExch port map(A=>TobLayerIn(25)(37), B=>TobLayerIn(25)(41), H=>TobLayerOut(25)(37), L=>TobLayerOut(25)(41));
  compExch_Layer_25_to_26_sites_38_42: compExch port map(A=>TobLayerIn(25)(38), B=>TobLayerIn(25)(42), H=>TobLayerOut(25)(38), L=>TobLayerOut(25)(42));
  compExch_Layer_25_to_26_sites_39_43: compExch port map(A=>TobLayerIn(25)(39), B=>TobLayerIn(25)(43), H=>TobLayerOut(25)(39), L=>TobLayerOut(25)(43));
  compExch_Layer_25_to_26_sites_44_48: compExch port map(A=>TobLayerIn(25)(44), B=>TobLayerIn(25)(48), H=>TobLayerOut(25)(44), L=>TobLayerOut(25)(48));
  compExch_Layer_25_to_26_sites_45_49: compExch port map(A=>TobLayerIn(25)(45), B=>TobLayerIn(25)(49), H=>TobLayerOut(25)(45), L=>TobLayerOut(25)(49));
  compExch_Layer_25_to_26_sites_46_50: compExch port map(A=>TobLayerIn(25)(46), B=>TobLayerIn(25)(50), H=>TobLayerOut(25)(46), L=>TobLayerOut(25)(50));
  compExch_Layer_25_to_26_sites_47_51: compExch port map(A=>TobLayerIn(25)(47), B=>TobLayerIn(25)(51), H=>TobLayerOut(25)(47), L=>TobLayerOut(25)(51));
  compExch_Layer_25_to_26_sites_52_56: compExch port map(A=>TobLayerIn(25)(52), B=>TobLayerIn(25)(56), H=>TobLayerOut(25)(52), L=>TobLayerOut(25)(56));
  compExch_Layer_25_to_26_sites_53_57: compExch port map(A=>TobLayerIn(25)(53), B=>TobLayerIn(25)(57), H=>TobLayerOut(25)(53), L=>TobLayerOut(25)(57));
  compExch_Layer_25_to_26_sites_54_58: compExch port map(A=>TobLayerIn(25)(54), B=>TobLayerIn(25)(58), H=>TobLayerOut(25)(54), L=>TobLayerOut(25)(58));
  compExch_Layer_25_to_26_sites_55_59: compExch port map(A=>TobLayerIn(25)(55), B=>TobLayerIn(25)(59), H=>TobLayerOut(25)(55), L=>TobLayerOut(25)(59));
  compExch_Layer_25_to_26_sites_60_64: compExch port map(A=>TobLayerIn(25)(60), B=>TobLayerIn(25)(64), H=>TobLayerOut(25)(60), L=>TobLayerOut(25)(64));
  compExch_Layer_25_to_26_sites_61_65: compExch port map(A=>TobLayerIn(25)(61), B=>TobLayerIn(25)(65), H=>TobLayerOut(25)(61), L=>TobLayerOut(25)(65));
  compExch_Layer_25_to_26_sites_62_66: compExch port map(A=>TobLayerIn(25)(62), B=>TobLayerIn(25)(66), H=>TobLayerOut(25)(62), L=>TobLayerOut(25)(66));
  compExch_Layer_25_to_26_sites_63_67: compExch port map(A=>TobLayerIn(25)(63), B=>TobLayerIn(25)(67), H=>TobLayerOut(25)(63), L=>TobLayerOut(25)(67));
  TobLayerOut(25)(0)<=TobLayerIn(25)(0);
  TobLayerOut(25)(1)<=TobLayerIn(25)(1);
  TobLayerOut(25)(2)<=TobLayerIn(25)(2);
  TobLayerOut(25)(3)<=TobLayerIn(25)(3);
  TobLayerOut(25)(68)<=TobLayerIn(25)(68);
  TobLayerOut(25)(69)<=TobLayerIn(25)(69);


  compExch_Layer_26_to_27_sites_02_04: compExch port map(A=>TobLayerIn(26)(2), B=>TobLayerIn(26)(4), H=>TobLayerOut(26)(2), L=>TobLayerOut(26)(4));
  compExch_Layer_26_to_27_sites_03_05: compExch port map(A=>TobLayerIn(26)(3), B=>TobLayerIn(26)(5), H=>TobLayerOut(26)(3), L=>TobLayerOut(26)(5));
  compExch_Layer_26_to_27_sites_06_08: compExch port map(A=>TobLayerIn(26)(6), B=>TobLayerIn(26)(8), H=>TobLayerOut(26)(6), L=>TobLayerOut(26)(8));
  compExch_Layer_26_to_27_sites_07_09: compExch port map(A=>TobLayerIn(26)(7), B=>TobLayerIn(26)(9), H=>TobLayerOut(26)(7), L=>TobLayerOut(26)(9));
  compExch_Layer_26_to_27_sites_10_12: compExch port map(A=>TobLayerIn(26)(10), B=>TobLayerIn(26)(12), H=>TobLayerOut(26)(10), L=>TobLayerOut(26)(12));
  compExch_Layer_26_to_27_sites_11_13: compExch port map(A=>TobLayerIn(26)(11), B=>TobLayerIn(26)(13), H=>TobLayerOut(26)(11), L=>TobLayerOut(26)(13));
  compExch_Layer_26_to_27_sites_14_16: compExch port map(A=>TobLayerIn(26)(14), B=>TobLayerIn(26)(16), H=>TobLayerOut(26)(14), L=>TobLayerOut(26)(16));
  compExch_Layer_26_to_27_sites_15_17: compExch port map(A=>TobLayerIn(26)(15), B=>TobLayerIn(26)(17), H=>TobLayerOut(26)(15), L=>TobLayerOut(26)(17));
  compExch_Layer_26_to_27_sites_18_20: compExch port map(A=>TobLayerIn(26)(18), B=>TobLayerIn(26)(20), H=>TobLayerOut(26)(18), L=>TobLayerOut(26)(20));
  compExch_Layer_26_to_27_sites_19_21: compExch port map(A=>TobLayerIn(26)(19), B=>TobLayerIn(26)(21), H=>TobLayerOut(26)(19), L=>TobLayerOut(26)(21));
  compExch_Layer_26_to_27_sites_22_24: compExch port map(A=>TobLayerIn(26)(22), B=>TobLayerIn(26)(24), H=>TobLayerOut(26)(22), L=>TobLayerOut(26)(24));
  compExch_Layer_26_to_27_sites_23_25: compExch port map(A=>TobLayerIn(26)(23), B=>TobLayerIn(26)(25), H=>TobLayerOut(26)(23), L=>TobLayerOut(26)(25));
  compExch_Layer_26_to_27_sites_26_28: compExch port map(A=>TobLayerIn(26)(26), B=>TobLayerIn(26)(28), H=>TobLayerOut(26)(26), L=>TobLayerOut(26)(28));
  compExch_Layer_26_to_27_sites_27_29: compExch port map(A=>TobLayerIn(26)(27), B=>TobLayerIn(26)(29), H=>TobLayerOut(26)(27), L=>TobLayerOut(26)(29));
  compExch_Layer_26_to_27_sites_30_32: compExch port map(A=>TobLayerIn(26)(30), B=>TobLayerIn(26)(32), H=>TobLayerOut(26)(30), L=>TobLayerOut(26)(32));
  compExch_Layer_26_to_27_sites_31_33: compExch port map(A=>TobLayerIn(26)(31), B=>TobLayerIn(26)(33), H=>TobLayerOut(26)(31), L=>TobLayerOut(26)(33));
  compExch_Layer_26_to_27_sites_34_36: compExch port map(A=>TobLayerIn(26)(34), B=>TobLayerIn(26)(36), H=>TobLayerOut(26)(34), L=>TobLayerOut(26)(36));
  compExch_Layer_26_to_27_sites_35_37: compExch port map(A=>TobLayerIn(26)(35), B=>TobLayerIn(26)(37), H=>TobLayerOut(26)(35), L=>TobLayerOut(26)(37));
  compExch_Layer_26_to_27_sites_38_40: compExch port map(A=>TobLayerIn(26)(38), B=>TobLayerIn(26)(40), H=>TobLayerOut(26)(38), L=>TobLayerOut(26)(40));
  compExch_Layer_26_to_27_sites_39_41: compExch port map(A=>TobLayerIn(26)(39), B=>TobLayerIn(26)(41), H=>TobLayerOut(26)(39), L=>TobLayerOut(26)(41));
  compExch_Layer_26_to_27_sites_42_44: compExch port map(A=>TobLayerIn(26)(42), B=>TobLayerIn(26)(44), H=>TobLayerOut(26)(42), L=>TobLayerOut(26)(44));
  compExch_Layer_26_to_27_sites_43_45: compExch port map(A=>TobLayerIn(26)(43), B=>TobLayerIn(26)(45), H=>TobLayerOut(26)(43), L=>TobLayerOut(26)(45));
  compExch_Layer_26_to_27_sites_46_48: compExch port map(A=>TobLayerIn(26)(46), B=>TobLayerIn(26)(48), H=>TobLayerOut(26)(46), L=>TobLayerOut(26)(48));
  compExch_Layer_26_to_27_sites_47_49: compExch port map(A=>TobLayerIn(26)(47), B=>TobLayerIn(26)(49), H=>TobLayerOut(26)(47), L=>TobLayerOut(26)(49));
  compExch_Layer_26_to_27_sites_50_52: compExch port map(A=>TobLayerIn(26)(50), B=>TobLayerIn(26)(52), H=>TobLayerOut(26)(50), L=>TobLayerOut(26)(52));
  compExch_Layer_26_to_27_sites_51_53: compExch port map(A=>TobLayerIn(26)(51), B=>TobLayerIn(26)(53), H=>TobLayerOut(26)(51), L=>TobLayerOut(26)(53));
  compExch_Layer_26_to_27_sites_54_56: compExch port map(A=>TobLayerIn(26)(54), B=>TobLayerIn(26)(56), H=>TobLayerOut(26)(54), L=>TobLayerOut(26)(56));
  compExch_Layer_26_to_27_sites_55_57: compExch port map(A=>TobLayerIn(26)(55), B=>TobLayerIn(26)(57), H=>TobLayerOut(26)(55), L=>TobLayerOut(26)(57));
  compExch_Layer_26_to_27_sites_58_60: compExch port map(A=>TobLayerIn(26)(58), B=>TobLayerIn(26)(60), H=>TobLayerOut(26)(58), L=>TobLayerOut(26)(60));
  compExch_Layer_26_to_27_sites_59_61: compExch port map(A=>TobLayerIn(26)(59), B=>TobLayerIn(26)(61), H=>TobLayerOut(26)(59), L=>TobLayerOut(26)(61));
  compExch_Layer_26_to_27_sites_62_64: compExch port map(A=>TobLayerIn(26)(62), B=>TobLayerIn(26)(64), H=>TobLayerOut(26)(62), L=>TobLayerOut(26)(64));
  compExch_Layer_26_to_27_sites_63_65: compExch port map(A=>TobLayerIn(26)(63), B=>TobLayerIn(26)(65), H=>TobLayerOut(26)(63), L=>TobLayerOut(26)(65));
  compExch_Layer_26_to_27_sites_66_68: compExch port map(A=>TobLayerIn(26)(66), B=>TobLayerIn(26)(68), H=>TobLayerOut(26)(66), L=>TobLayerOut(26)(68));
  compExch_Layer_26_to_27_sites_67_69: compExch port map(A=>TobLayerIn(26)(67), B=>TobLayerIn(26)(69), H=>TobLayerOut(26)(67), L=>TobLayerOut(26)(69));
  TobLayerOut(26)(0)<=TobLayerIn(26)(0);
  TobLayerOut(26)(1)<=TobLayerIn(26)(1);


  compExch_Layer_27_to_28_sites_01_02: compExch port map(A=>TobLayerIn(27)(1), B=>TobLayerIn(27)(2), H=>TobLayerOut(27)(1), L=>TobLayerOut(27)(2));
  compExch_Layer_27_to_28_sites_03_04: compExch port map(A=>TobLayerIn(27)(3), B=>TobLayerIn(27)(4), H=>TobLayerOut(27)(3), L=>TobLayerOut(27)(4));
  compExch_Layer_27_to_28_sites_05_06: compExch port map(A=>TobLayerIn(27)(5), B=>TobLayerIn(27)(6), H=>TobLayerOut(27)(5), L=>TobLayerOut(27)(6));
  compExch_Layer_27_to_28_sites_07_08: compExch port map(A=>TobLayerIn(27)(7), B=>TobLayerIn(27)(8), H=>TobLayerOut(27)(7), L=>TobLayerOut(27)(8));
  compExch_Layer_27_to_28_sites_09_10: compExch port map(A=>TobLayerIn(27)(9), B=>TobLayerIn(27)(10), H=>TobLayerOut(27)(9), L=>TobLayerOut(27)(10));
  compExch_Layer_27_to_28_sites_11_12: compExch port map(A=>TobLayerIn(27)(11), B=>TobLayerIn(27)(12), H=>TobLayerOut(27)(11), L=>TobLayerOut(27)(12));
  compExch_Layer_27_to_28_sites_13_14: compExch port map(A=>TobLayerIn(27)(13), B=>TobLayerIn(27)(14), H=>TobLayerOut(27)(13), L=>TobLayerOut(27)(14));
  compExch_Layer_27_to_28_sites_15_16: compExch port map(A=>TobLayerIn(27)(15), B=>TobLayerIn(27)(16), H=>TobLayerOut(27)(15), L=>TobLayerOut(27)(16));
  compExch_Layer_27_to_28_sites_17_18: compExch port map(A=>TobLayerIn(27)(17), B=>TobLayerIn(27)(18), H=>TobLayerOut(27)(17), L=>TobLayerOut(27)(18));
  compExch_Layer_27_to_28_sites_19_20: compExch port map(A=>TobLayerIn(27)(19), B=>TobLayerIn(27)(20), H=>TobLayerOut(27)(19), L=>TobLayerOut(27)(20));
  compExch_Layer_27_to_28_sites_21_22: compExch port map(A=>TobLayerIn(27)(21), B=>TobLayerIn(27)(22), H=>TobLayerOut(27)(21), L=>TobLayerOut(27)(22));
  compExch_Layer_27_to_28_sites_23_24: compExch port map(A=>TobLayerIn(27)(23), B=>TobLayerIn(27)(24), H=>TobLayerOut(27)(23), L=>TobLayerOut(27)(24));
  compExch_Layer_27_to_28_sites_25_26: compExch port map(A=>TobLayerIn(27)(25), B=>TobLayerIn(27)(26), H=>TobLayerOut(27)(25), L=>TobLayerOut(27)(26));
  compExch_Layer_27_to_28_sites_27_28: compExch port map(A=>TobLayerIn(27)(27), B=>TobLayerIn(27)(28), H=>TobLayerOut(27)(27), L=>TobLayerOut(27)(28));
  compExch_Layer_27_to_28_sites_29_30: compExch port map(A=>TobLayerIn(27)(29), B=>TobLayerIn(27)(30), H=>TobLayerOut(27)(29), L=>TobLayerOut(27)(30));
  compExch_Layer_27_to_28_sites_31_32: compExch port map(A=>TobLayerIn(27)(31), B=>TobLayerIn(27)(32), H=>TobLayerOut(27)(31), L=>TobLayerOut(27)(32));
  compExch_Layer_27_to_28_sites_33_34: compExch port map(A=>TobLayerIn(27)(33), B=>TobLayerIn(27)(34), H=>TobLayerOut(27)(33), L=>TobLayerOut(27)(34));
  compExch_Layer_27_to_28_sites_35_36: compExch port map(A=>TobLayerIn(27)(35), B=>TobLayerIn(27)(36), H=>TobLayerOut(27)(35), L=>TobLayerOut(27)(36));
  compExch_Layer_27_to_28_sites_37_38: compExch port map(A=>TobLayerIn(27)(37), B=>TobLayerIn(27)(38), H=>TobLayerOut(27)(37), L=>TobLayerOut(27)(38));
  compExch_Layer_27_to_28_sites_39_40: compExch port map(A=>TobLayerIn(27)(39), B=>TobLayerIn(27)(40), H=>TobLayerOut(27)(39), L=>TobLayerOut(27)(40));
  compExch_Layer_27_to_28_sites_41_42: compExch port map(A=>TobLayerIn(27)(41), B=>TobLayerIn(27)(42), H=>TobLayerOut(27)(41), L=>TobLayerOut(27)(42));
  compExch_Layer_27_to_28_sites_43_44: compExch port map(A=>TobLayerIn(27)(43), B=>TobLayerIn(27)(44), H=>TobLayerOut(27)(43), L=>TobLayerOut(27)(44));
  compExch_Layer_27_to_28_sites_45_46: compExch port map(A=>TobLayerIn(27)(45), B=>TobLayerIn(27)(46), H=>TobLayerOut(27)(45), L=>TobLayerOut(27)(46));
  compExch_Layer_27_to_28_sites_47_48: compExch port map(A=>TobLayerIn(27)(47), B=>TobLayerIn(27)(48), H=>TobLayerOut(27)(47), L=>TobLayerOut(27)(48));
  compExch_Layer_27_to_28_sites_49_50: compExch port map(A=>TobLayerIn(27)(49), B=>TobLayerIn(27)(50), H=>TobLayerOut(27)(49), L=>TobLayerOut(27)(50));
  compExch_Layer_27_to_28_sites_51_52: compExch port map(A=>TobLayerIn(27)(51), B=>TobLayerIn(27)(52), H=>TobLayerOut(27)(51), L=>TobLayerOut(27)(52));
  compExch_Layer_27_to_28_sites_53_54: compExch port map(A=>TobLayerIn(27)(53), B=>TobLayerIn(27)(54), H=>TobLayerOut(27)(53), L=>TobLayerOut(27)(54));
  compExch_Layer_27_to_28_sites_55_56: compExch port map(A=>TobLayerIn(27)(55), B=>TobLayerIn(27)(56), H=>TobLayerOut(27)(55), L=>TobLayerOut(27)(56));
  compExch_Layer_27_to_28_sites_57_58: compExch port map(A=>TobLayerIn(27)(57), B=>TobLayerIn(27)(58), H=>TobLayerOut(27)(57), L=>TobLayerOut(27)(58));
  compExch_Layer_27_to_28_sites_59_60: compExch port map(A=>TobLayerIn(27)(59), B=>TobLayerIn(27)(60), H=>TobLayerOut(27)(59), L=>TobLayerOut(27)(60));
  compExch_Layer_27_to_28_sites_61_62: compExch port map(A=>TobLayerIn(27)(61), B=>TobLayerIn(27)(62), H=>TobLayerOut(27)(61), L=>TobLayerOut(27)(62));
  compExch_Layer_27_to_28_sites_63_64: compExch port map(A=>TobLayerIn(27)(63), B=>TobLayerIn(27)(64), H=>TobLayerOut(27)(63), L=>TobLayerOut(27)(64));
  compExch_Layer_27_to_28_sites_65_66: compExch port map(A=>TobLayerIn(27)(65), B=>TobLayerIn(27)(66), H=>TobLayerOut(27)(65), L=>TobLayerOut(27)(66));
  compExch_Layer_27_to_28_sites_67_68: compExch port map(A=>TobLayerIn(27)(67), B=>TobLayerIn(27)(68), H=>TobLayerOut(27)(67), L=>TobLayerOut(27)(68));
  TobLayerOut(27)(0)<=TobLayerIn(27)(0);
  TobLayerOut(27)(69)<=TobLayerIn(27)(69);


-- number of layers: 28

  
end RTL;
