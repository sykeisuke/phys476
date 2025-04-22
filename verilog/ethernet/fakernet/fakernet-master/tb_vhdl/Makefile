
QUIET ?= @

DIR=sim
all: $(DIR)/tb_fakernet

DIR_FNET_MDIO=sim_mdio
all: $(DIR_FNET_MDIO)/tb_fnet_mdio
DIR_FNET_ASYNC_FIFO=sim_async_fifo
all: $(DIR_FNET_ASYNC_FIFO)/tb_fnet_async_fifo
DIR_EFNET_UART=sim_efnet_uart
all: $(DIR_EFNET_UART)/tb_efnet_uart
DIR_EFB_NMEA=sim_efb_nmea
all: $(DIR_EFB_NMEA)/tb_efb_nmea
DIR_EFB_TRACK_PPS_TS=sim_efb_track_pps_ts
all: $(DIR_EFB_TRACK_PPS_TS)/tb_efb_track_pps_ts
DIR_EFB_SPI_READ=sim_efb_spi_read
all: $(DIR_EFB_SPI_READ)/tb_efb_spi_read
DIR_EFNET_W2O=sim_efnet_w2o
all: $(DIR_EFNET_W2O)/tb_efnet_w2o
DIR_EFNET_RSS=sim_efnet_rss
all: $(DIR_EFNET_RSS)/tb_efnet_rss
DIR_EFB_CFG_PARSE=sim_efb_cfg_parse
all: $(DIR_EFB_CFG_PARSE)/tb_efb_cfg_parse
DIR_EFB_LMD_EVENT=sim_efb_lmd_event
all: $(DIR_EFB_LMD_EVENT)/tb_efb_lmd_event

###

VHDL_SRC= \
	fnet_in_state.vhd \
	fnet_out_state.vhd \
	fnet_out_pipeline.vhd \
	fnet_regaccess.vhd \
	fnet_records.vhd \
	fnet_util_pkg.vhd \
	fnet_ram_block_a11d16.vhd \
	fnet_ram_block_stat.vhd \
	fnet_regacc_aux_stat.vhd \
	fnet_crc32.vhd \
	fnet_tcp_state.vhd \
	fnet_tcp_buffer.vhd \
	fnet_tcp_control.vhd \
	fnet_tcp_prepare.vhd \
	fnet_dyn_control.vhd \
	fnet_ntp_query_control.vhd \
	fnet_mon_pkt_control.vhd \
	fnet_packet_gen.vhd \
	fnet_local_reg.vhd \
	fnet_mdio.vhd \
	fnet_test_datagen.vhd \
	fnet_ram_block_data.vhd \
	fnet_async_fifo.vhd \
	fakernet_module.vhd \
	../extra_vhdl/efnet_slip_rx.vhd \
	../extra_vhdl/efnet_slip_tx.vhd \
# Intentionally empty

VHDL_TB_FAKERNET_SRC=tb_fakernet.vhd

ALL_TB_FAKERNET_VHDL_SRCS=\
	$(addprefix ../vhdl/,$(VHDL_SRC)) \
	$(addprefix ../tb_vhdl/,$(VHDL_TB_FAKERNET_SRC))

###

VHDL_FNET_MDIO_SRC= \
	fnet_records.vhd \
	fnet_util_pkg.vhd \
	fnet_mdio.vhd \
# Intentionally empty

VHDL_TB_FNET_MDIO_SRC=tb_fnet_mdio.vhd

ALL_TB_FNET_MDIO_VHDL_SRCS=\
	$(addprefix ../vhdl/,$(VHDL_FNET_MDIO_SRC)) \
	$(addprefix ../tb_vhdl/,$(VHDL_TB_FNET_MDIO_SRC))

###

VHDL_FNET_ASYNC_FIFO_SRC= \
	fnet_records.vhd \
	fnet_util_pkg.vhd \
	fnet_async_fifo.vhd \
# Intentionally empty

VHDL_TB_FNET_ASYNC_FIFO_SRC=tb_fnet_async_fifo.vhd

ALL_TB_FNET_ASYNC_FIFO_VHDL_SRCS=\
	$(addprefix ../vhdl/,$(VHDL_FNET_ASYNC_FIFO_SRC)) \
	$(addprefix ../tb_vhdl/,$(VHDL_TB_FNET_ASYNC_FIFO_SRC))

###

VHDL_EFNET_UART_SRC= \
	../extra_vhdl/efnet_uart_rx.vhd \
	../extra_vhdl/efnet_uart_tx.vhd \
# Intentionally empty

VHDL_TB_EFNET_UART_SRC=tb_efnet_uart.vhd

ALL_TB_EFNET_UART_VHDL_SRCS=\
	$(addprefix ../vhdl/,$(VHDL_EFNET_UART_SRC)) \
	$(addprefix ../tb_vhdl/,$(VHDL_TB_EFNET_UART_SRC))

###

VHDL_EFB_NMEA_SRC= \
	efb_nmea_parse.vhd \
	efb_ublox_parse.vhd \
# Intentionally empty

VHDL_TB_EFB_NMEA_SRC=tb_efb_nmea.vhd

ALL_TB_EFB_NMEA_VHDL_SRCS=\
	$(addprefix ../boards/common/,$(VHDL_EFB_NMEA_SRC)) \
	$(addprefix ../tb_vhdl/,$(VHDL_TB_EFB_NMEA_SRC))

###

VHDL_EFB_TRACK_PPS_TS_SRC= \
	../../vhdl/fnet_util_pkg.vhd \
	efb_track_pps_ts.vhd \
# Intentionally empty

VHDL_TB_EFB_TRACK_PPS_TS_SRC=tb_efb_track_pps_ts.vhd

ALL_TB_EFB_TRACK_PPS_TS_VHDL_SRCS=\
	$(addprefix ../boards/common/,$(VHDL_EFB_TRACK_PPS_TS_SRC)) \
	$(addprefix ../tb_vhdl/,$(VHDL_TB_EFB_TRACK_PPS_TS_SRC))

###

VHDL_EFB_SPI_READ_SRC= \
	../../vhdl/fnet_util_pkg.vhd \
	efb_spi_flash_read.vhd \
# Intentionally empty

VHDL_TB_EFB_SPI_READ_SRC=tb_efb_spi_read.vhd

ALL_TB_EFB_SPI_READ_VHDL_SRCS=\
	$(addprefix ../boards/common/,$(VHDL_EFB_SPI_READ_SRC)) \
	$(addprefix ../tb_vhdl/,$(VHDL_TB_EFB_SPI_READ_SRC))

###

VHDL_EFNET_W2O_SRC= \
	fnet_records.vhd \
	fnet_util_pkg.vhd \
	fnet_async_fifo.vhd \
	../extra_vhdl/efnet_word_to_octet.vhd \
	../extra_vhdl/efnet_word_out_fifo.vhd \
	../extra_vhdl/efnet_uart_tx.vhd \
# Intentionally empty

VHDL_TB_EFNET_W2O_SRC=tb_efnet_w2o.vhd

ALL_TB_EFNET_W2O_VHDL_SRCS=\
	$(addprefix ../vhdl/,$(VHDL_EFNET_W2O_SRC)) \
	$(addprefix ../tb_vhdl/,$(VHDL_TB_EFNET_W2O_SRC))

###

VHDL_EFNET_RSS_SRC= \
	fnet_util_pkg.vhd \
	../extra_vhdl/efnet_rx_speed_sense.vhd \
# Intentionally empty

VHDL_TB_EFNET_RSS_SRC=tb_efnet_rss.vhd

ALL_TB_EFNET_RSS_VHDL_SRCS=\
	$(addprefix ../vhdl/,$(VHDL_EFNET_RSS_SRC)) \
	$(addprefix ../tb_vhdl/,$(VHDL_TB_EFNET_RSS_SRC))

###

VHDL_EFB_CFG_PARSE_SRC= \
	fnet_util_pkg.vhd \
	../boards/common/efb_text_config_parse.vhd \
# Intentionally empty

VHDL_TB_EFB_CFG_PARSE_SRC=tb_efb_cfg_parse.vhd

ALL_TB_EFB_CFG_PARSE_VHDL_SRCS=\
	$(addprefix ../vhdl/,$(VHDL_EFB_CFG_PARSE_SRC)) \
	$(addprefix ../tb_vhdl/,$(VHDL_TB_EFB_CFG_PARSE_SRC)) \
	sim_efb_cfg_parse/text_config_data.vhd

###

VHDL_EFB_LMD_EVENT_SRC= \
	fnet_records.vhd \
	fnet_util_pkg.vhd \
	fnet_ram_block_data.vhd \
	../boards/common/efb_lmd_format_events.vhd \
	../boards/common/efb_lmd_buffer_events.vhd \
	../boards/common/efb_sim_events.vhd \
# Intentionally empty

VHDL_TB_EFB_LMD_EVENT_SRC=tb_efb_lmd_event.vhd

ALL_TB_EFB_LMD_EVENT_VHDL_SRCS=\
	$(addprefix ../vhdl/,$(VHDL_EFB_LMD_EVENT_SRC)) \
	$(addprefix ../tb_vhdl/,$(VHDL_TB_EFB_LMD_EVENT_SRC))

###

GHDL_WARNINGS=--vital-checks --warn-binding --warn-reserved --warn-library \
	--warn-vital-generic --warn-delayed-checks --warn-body --warn-specs \
	--warn-unused

# -O2 / -O3 seems to have no execution time effect
GHDL_FLAGS=--no-vital-checks --ieee=synopsys --std=93c $(GHDL_WARNINGS) -O3

$(DIR)/tb_fakernet: $(ALL_TB_FAKERNET_VHDL_SRCS)
	@echo "  VHDL   $@"
	$(QUIET)mkdir -p $(DIR)
	$(QUIET)rm -rf $(DIR)/simulation
	$(QUIET)mkdir -p $(DIR)/simulation
	$(QUIET)(cd $(DIR) ; \
	  ghdl -i -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work \
	  $(addprefix ../,$(ALL_TB_FAKERNET_VHDL_SRCS)))
	$(QUIET)(cd $(DIR) ; \
	  ghdl -m -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work tb_fakernet)

$(DIR_FNET_MDIO)/tb_fnet_mdio: $(ALL_TB_FNET_MDIO_VHDL_SRCS)
	@echo "  VHDL   $@"
	$(QUIET)mkdir -p $(DIR_FNET_MDIO)
	$(QUIET)rm -rf $(DIR_FNET_MDIO)/simulation
	$(QUIET)mkdir -p $(DIR_FNET_MDIO)/simulation
	$(QUIET)(cd $(DIR_FNET_MDIO) ; \
	  ghdl -i -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work \
	  $(addprefix ../,$(ALL_TB_FNET_MDIO_VHDL_SRCS)))
	$(QUIET)(cd $(DIR_FNET_MDIO) ; \
	  ghdl -m -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work tb_fnet_mdio)

$(DIR_FNET_ASYNC_FIFO)/tb_fnet_async_fifo: $(ALL_TB_FNET_ASYNC_FIFO_VHDL_SRCS)
	@echo "  VHDL   $@"
	$(QUIET)mkdir -p $(DIR_FNET_ASYNC_FIFO)
	$(QUIET)rm -rf $(DIR_FNET_ASYNC_FIFO)/simulation
	$(QUIET)mkdir -p $(DIR_FNET_ASYNC_FIFO)/simulation
	$(QUIET)(cd $(DIR_FNET_ASYNC_FIFO) ; \
	  ghdl -i -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work \
	  $(addprefix ../,$(ALL_TB_FNET_ASYNC_FIFO_VHDL_SRCS)))
	$(QUIET)(cd $(DIR_FNET_ASYNC_FIFO) ; \
	  ghdl -m -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work tb_fnet_async_fifo)

$(DIR_EFNET_UART)/tb_efnet_uart: $(ALL_TB_EFNET_UART_VHDL_SRCS)
	@echo "  VHDL   $@"
	$(QUIET)mkdir -p $(DIR_EFNET_UART)
	$(QUIET)rm -rf $(DIR_EFNET_UART)/simulation
	$(QUIET)mkdir -p $(DIR_EFNET_UART)/simulation
	$(QUIET)(cd $(DIR_EFNET_UART) ; \
	  ghdl -i -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work \
	  $(addprefix ../,$(ALL_TB_EFNET_UART_VHDL_SRCS)))
	$(QUIET)(cd $(DIR_EFNET_UART) ; \
	  ghdl -m -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work tb_efnet_uart)

$(DIR_EFB_NMEA)/tb_efb_nmea: $(ALL_TB_EFB_NMEA_VHDL_SRCS)
	@echo "  VHDL   $@"
	$(QUIET)mkdir -p $(DIR_EFB_NMEA)
	$(QUIET)rm -rf $(DIR_EFB_NMEA)/simulation
	$(QUIET)mkdir -p $(DIR_EFB_NMEA)/simulation
	$(QUIET)(cd $(DIR_EFB_NMEA) ; \
	  ghdl -i -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work \
	  $(addprefix ../,$(ALL_TB_EFB_NMEA_VHDL_SRCS)))
	$(QUIET)(cd $(DIR_EFB_NMEA) ; \
	  ghdl -m -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work tb_efb_nmea)

$(DIR_EFB_TRACK_PPS_TS)/tb_efb_track_pps_ts: $(ALL_TB_EFB_TRACK_PPS_TS_VHDL_SRCS)
	@echo "  VHDL   $@"
	$(QUIET)mkdir -p $(DIR_EFB_TRACK_PPS_TS)
	$(QUIET)rm -rf $(DIR_EFB_TRACK_PPS_TS)/simulation
	$(QUIET)mkdir -p $(DIR_EFB_TRACK_PPS_TS)/simulation
	$(QUIET)(cd $(DIR_EFB_TRACK_PPS_TS) ; \
	  ghdl -i -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work \
	  $(addprefix ../,$(ALL_TB_EFB_TRACK_PPS_TS_VHDL_SRCS)))
	$(QUIET)(cd $(DIR_EFB_TRACK_PPS_TS) ; \
	  ghdl -m -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work tb_efb_track_pps_ts)

$(DIR_EFB_SPI_READ)/tb_efb_spi_read: $(ALL_TB_EFB_SPI_READ_VHDL_SRCS)
	@echo "  VHDL   $@"
	$(QUIET)mkdir -p $(DIR_EFB_SPI_READ)
	$(QUIET)rm -rf $(DIR_EFB_SPI_READ)/simulation
	$(QUIET)mkdir -p $(DIR_EFB_SPI_READ)/simulation
	$(QUIET)(cd $(DIR_EFB_SPI_READ) ; \
	  ghdl -i -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work \
	  $(addprefix ../,$(ALL_TB_EFB_SPI_READ_VHDL_SRCS)))
	$(QUIET)(cd $(DIR_EFB_SPI_READ) ; \
	  ghdl -m -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work tb_efb_spi_read)

$(DIR_EFNET_W2O)/tb_efnet_w2o: $(ALL_TB_EFNET_W2O_VHDL_SRCS)
	@echo "  VHDL   $@"
	$(QUIET)mkdir -p $(DIR_EFNET_W2O)
	$(QUIET)rm -rf $(DIR_EFNET_W2O)/simulation
	$(QUIET)mkdir -p $(DIR_EFNET_W2O)/simulation
	$(QUIET)(cd $(DIR_EFNET_W2O) ; \
	  ghdl -i -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work \
	  $(addprefix ../,$(ALL_TB_EFNET_W2O_VHDL_SRCS)))
	$(QUIET)(cd $(DIR_EFNET_W2O) ; \
	  ghdl -m -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work tb_efnet_w2o)

$(DIR_EFNET_RSS)/tb_efnet_rss: $(ALL_TB_EFNET_RSS_VHDL_SRCS)
	@echo "  VHDL   $@"
	$(QUIET)mkdir -p $(DIR_EFNET_RSS)
	$(QUIET)rm -rf $(DIR_EFNET_RSS)/simulation
	$(QUIET)mkdir -p $(DIR_EFNET_RSS)/simulation
	$(QUIET)(cd $(DIR_EFNET_RSS) ; \
	  ghdl -i -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work \
	  $(addprefix ../,$(ALL_TB_EFNET_RSS_VHDL_SRCS)))
	$(QUIET)(cd $(DIR_EFNET_RSS) ; \
	  ghdl -m -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work tb_efnet_rss)

GEN_TEXT_CONFIG_PL = ../boards/common/gen_text_config.pl

$(DIR_EFB_CFG_PARSE)/text_config_data.vhd: text_cfg_setup1.txt \
	    $(GEN_TEXT_CONFIG_PL)
	@echo "   GEN   $@"
	$(QUIET)mkdir -p $(DIR_EFB_CFG_PARSE)
	$(QUIET)$(GEN_TEXT_CONFIG_PL) < $< > $@

$(DIR_EFB_CFG_PARSE)/tb_efb_cfg_parse: $(ALL_TB_EFB_CFG_PARSE_VHDL_SRCS)
	@echo "  VHDL   $@"
	$(QUIET)mkdir -p $(DIR_EFB_CFG_PARSE)
	$(QUIET)rm -rf $(DIR_EFB_CFG_PARSE)/simulation
	$(QUIET)mkdir -p $(DIR_EFB_CFG_PARSE)/simulation
	$(QUIET)(cd $(DIR_EFB_CFG_PARSE) ; \
	  ghdl -i -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work \
	  $(addprefix ../,$(ALL_TB_EFB_CFG_PARSE_VHDL_SRCS)))
	$(QUIET)(cd $(DIR_EFB_CFG_PARSE) ; \
	  ghdl -m -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work tb_efb_cfg_parse)

$(DIR_EFB_LMD_EVENT)/tb_efb_lmd_event: $(ALL_TB_EFB_LMD_EVENT_VHDL_SRCS)
	@echo "  VHDL   $@"
	$(QUIET)mkdir -p $(DIR_EFB_LMD_EVENT)
	$(QUIET)rm -rf $(DIR_EFB_LMD_EVENT)/simulation
	$(QUIET)mkdir -p $(DIR_EFB_LMD_EVENT)/simulation
	$(QUIET)(cd $(DIR_EFB_LMD_EVENT) ; \
	  ghdl -i -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work \
	  $(addprefix ../,$(ALL_TB_EFB_LMD_EVENT_VHDL_SRCS)))
	$(QUIET)(cd $(DIR_EFB_LMD_EVENT) ; \
	  ghdl -m -g $(GHDL_FLAGS) --workdir=simulation \
	    -fexplicit --work=work tb_efb_lmd_event)

clean:
	rm -rf sim*/
