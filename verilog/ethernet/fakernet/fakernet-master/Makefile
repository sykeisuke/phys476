
QUIET ?= @

all: client short

# Available targets:
#
# client           - client library / test program
# short            - test VHDL packet handling (ghdl)
# timing           - synthesis timings

###

.PHONY: client
client:
	$(MAKE) -C client

###

.PHONY: time_%
time_%:
	echo Timing $*
	cd tb_timing ; TARGET=$* ./build.sh

.PHONY: timing
timing: time_virtex4 time_virtex6 time_spartan6 \
	time_max10 time_cyclonev time_cycloneiv time_cyclone10
	find  tb_timing/ | grep "/result.txt" | sort | \
	  xargs cat | tb_timing/getspeeds.pl

###

.PHONY: simbuild
simbuild:
	(cd tb_vhdl ; ./build.sh)

.PHONY: simexec
simexec: simbuild
	(cd tb_vhdl ; ./runsim.sh)

.PHONY: sim_mdio
sim_mdio: simbuild
	(cd tb_vhdl ; ./run_mdio.sh mdio)

.PHONY: sim_async_fifo
sim_async_fifo: simbuild
	(cd tb_vhdl ; ./run_async_fifo.sh async_fifo)

.PHONY: sim_efnet_uart
sim_efnet_uart: simbuild
	(cd tb_vhdl ; ./run_efnet_uart.sh efnet_uart)

.PHONY: sim_efnet_w2o
sim_efnet_w2o: simbuild
	(cd tb_vhdl ; ./run_efnet_w2o.sh efnet_w2o)

.PHONY: sim_efnet_rss
sim_efnet_rss: simbuild
	(cd tb_vhdl ; ./run_efnet_rss.sh efnet_rss)

.PHONY: sim_efb_cfg_parse
sim_efb_cfg_parse: simbuild
	(cd tb_vhdl ; \
	 ./run_efb_cfg_parse.sh text_cfg_input1 ; \
	 diff -u text_cfg_input1.good text_cfg_input1.out )

.PHONY: sim_efb_lmd_event
sim_efb_lmd_event: simbuild
	(cd tb_vhdl ; ./run_efb_lmd_event.sh efb_lmd_event)

.PHONY: sim_efb_nmea
sim_efb_nmea: simbuild
	(cd tb_vhdl ; \
	 ./run_efb_nmea.sh nmea_input1 ; \
	 ./run_efb_nmea.sh nmea_input2 ; \
	 ./run_efb_nmea.sh nmea_input3 ; \
	 ./run_efb_nmea.sh nmea_input4 ; \
	 diff -u nmea_input1.good nmea_input1.out ; \
	 diff -u nmea_input2.good nmea_input2.out ; \
	 diff -u nmea_input3.good nmea_input3.out ; \
	 diff -u nmea_input4.good nmea_input4.out )

.PHONY: sim_efb_track_pps_ts
sim_efb_track_pps_ts: simbuild
	(cd tb_vhdl ; ./run_efb_track_pps_ts.sh track_pps_ts)

.PHONY: sim_efb_spi_read
sim_efb_spi_read: simbuild
	(cd tb_vhdl ; ./run_efb_spi_read.sh spi_read)

.PHONY: runsim
runsim: simexec sim_mdio sim_async_fifo \
	  sim_efnet_uart sim_efnet_w2o sim_efnet_rss \
	  sim_efb_nmea sim_efb_track_pps_ts \
	  sim_efb_cfg_parse sim_efb_spi_read sim_efb_lmd_event
        @echo "Success: $@"

###

.PHONY: clean_sim
clean_sim:
	$(MAKE) -C tb_vhdl clean

clean: clean_sim

clean:
	make -C client clean
