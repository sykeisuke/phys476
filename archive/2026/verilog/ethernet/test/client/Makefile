
all: fnetctrl

CFLAGS += -g -std=gnu99 -Wall -W -Wshadow -Wwrite-strings \
        -Wno-unused-function -Wno-unused-label -Wstrict-prototypes

LDLIBS += -lm

fnetctrl: fnetctrl.o fnetctrl_mdio.o fnetctrl_stat.o fnetctrl_udp.o \
	fnetctrl_tcp.o fnetctrl_gps.o fnetctrl_sampler.o  fnetctrl_spi.o \
	fnetctrl_readout.o fnetctrl_sendwaveform.o\
	fnetctrl_util.o fnet_client.o

fnetctrl.o fnet_client.o: fakernet.h fnetctrl.h fnet_client.h

clean:
	rm -f *.o
	rm -f fnetctrl
