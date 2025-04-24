
/* Copyright (c) 2020, Haakan T. Johansson */
/* All rights reserved. */

/* Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the authors nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include "fakernet.h"
#include "fnet_client.h"
#include "fnetctrl.h"

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <errno.h>
#include <sys/time.h>
#include <time.h>
#include <inttypes.h>
#include <math.h>
#include <ctype.h>
#include <assert.h>

/*************************************************************************/

void fnet_ctrl_usage(char *cmdname)
{
  printf ("Fakernet control program.\n");
  printf ("\n");
  printf ("Usage: %s HOSTNAME(=IP-ADDR) <options>\n", cmdname);
  printf ("\n");
  printf ("  --stat           Query status counters once per second.\n");
  printf ("  --tcp-reset      Reset TCP connection.\n");
  printf ("  --tcp[=dump|hex|func|local]  Reset TCP, and read data (indefinitely).\n");
  printf ("                   (dump: dump data, hex: hexdump data, func: monitor GPS,\n");
  printf ("                   local: verify local-generated data).\n");
  printf ("  --btn2led        Test buttons and LEDs on ARTY board.\n");
  printf ("  --tcp-payload=N  Restrict TCP packet payload size (0 = no restr.).\n");
  printf ("  --tcp-window=N   Restrict TCP window size (0 = no restr.).\n");
  printf ("  --udp-flood=N    Flood UDP requests, with N regacc per request.\n");
  printf ("  --read=ADDR      Read UDP control register at address ADDR.\n");
  printf ("  --write=ADDR:VAL Write UDP control register at address ADDR, value VAL.\n");
  printf ("  --send-waveform=FILE  Send waveform binary file to FPGA.\n");
  printf ("  --mdio=PHY[,help]  Decode MDIO status, or perform action.\n");
  printf ("  --mdio-read=PHY  Read and print MDIO registers.\n");
  printf ("  --mdio-write=PHY:ADDR=0xHEX  Write MDIO register.\n");
  printf ("  --mdio-mmd-write=PHY:DEV:ADDR=0xHEX  Write MDIO MMD register.\n");
  printf ("  --readout        Query status about readout (LMD).\n");
  printf ("  --spi-read       Read SPI flash memory.\n");
  printf ("  --debug          Print debug messages to stderr.\n");
  printf ("  --[no]idempotent  Use the first (no-sequence, multi-client) port.\n");
  printf ("  --ntpq=MAC,IP    Send NTP query to IP with given MAC address.\n");
  printf ("  --help           Show this message.\n");
  printf ("\n");
}

int main(int argc, char **argv)
{
  struct fnet_ctrl_client *client = NULL;
  const char *error_ptr;
  char *end;

  char *hostname;
  int do_stat = 0;
  int do_tcp_reset = 0;
  int do_tcp_read = 0;
  int do_btn2led = 0;
  int do_udp_flood = 0;

  char *send_waveform_filename = NULL;
  int do_send_waveform = 0;

  int do_mdio = 0;
  int do_mdio_read = 0;
  int do_mdio_write = 0;
  int do_mdio_mmd_write = 0;

  int do_ntpq = 0;
  char *ntpq_cmd = NULL;

  int phy_addr = 0;
  int phy_reg = 0;
  int phy_value = 0;
  int mmd_dev = 0;
  int mmd_reg = 0;
  const char *mdio_action = NULL;

  int items_per_packet;

  int do_testreg = 0;
  uint32_t testreg_addr = 0;
  uint32_t testreg_value = 0;

  int do_reg_read = 0;
  int do_reg_write = 0;
  uint32_t reg_addr  = (uint32_t) -1;
  uint32_t reg_value = 0;

  int do_spi_read = 0;

  int do_readout_stat = 0;

  int reliable = -1;
  int debug = 0;
  int i;

  int tcp_sockfd = -1;
  tcp_handler_func_t tcp_handler = NULL;
  void *tcp_handler_pinfo = NULL;
  int tcp_show_progess = 1;

  fnet_tcp_local_verify_info local_verify_info;
  fnet_tcp_lcl_func_mon_info lcl_func_mon_info;

  memset(&local_verify_info, 0, sizeof (local_verify_info));
  memset(&lcl_func_mon_info, 0, sizeof (lcl_func_mon_info));

  lcl_func_mon_info._xadc._prev_max_ch = (uint32_t) -1;

  for (i = 1; i < argc; i++)
    {
      if (strcmp(argv[i],"--help") == 0)
	{
	  fnet_ctrl_usage(argv[0]);
	  exit(0);
	}

      if (strcmp(argv[i],"--debug") == 0)
	debug = 1;
      else if (strcmp(argv[i],"--noidempotent") == 0)
	reliable = 1;
      else if (strcmp(argv[i],"--idempotent") == 0)
	reliable = 0;
      else if (strcmp(argv[i],"--stat") == 0)
	do_stat = 1;
      else if (strcmp(argv[i],"--readout") == 0)
	do_readout_stat = 1;
      else if (strncmp(argv[i],"--mdio=",7) == 0)
	{
	  char *comma = strchr(argv[i]+7,',');
	  do_mdio = 1;
	  phy_addr = atoi(argv[i]+7);
	  if (comma)
	    mdio_action = comma+1;
	}
      else if (strncmp(argv[i],"--mdio-read=",12) == 0)
	{
	  do_mdio_read = 1;
	  phy_addr = atoi(argv[i]+12);
	}
      else if (strncmp(argv[i],"--mdio-write=",13) == 0)
	{
	  char *colon  = strchr(argv[i]+13,':');
	  char *equals = strchr(argv[i]+13,'=');

	  if (!colon || !equals)
	    {
	      fprintf (stderr, "Bad --mdio-write=.\n");
	      fnet_ctrl_usage(argv[0]);
	      exit(1);
	    }
	  do_mdio_write = 1;
	  phy_addr = atoi(argv[i]+13);
	  phy_reg  = atoi(colon+1);
	  phy_value = strtol(equals+1,NULL,16);
	  /*printf ("%d %d %04x\n", phy_addr, phy_reg, phy_value);
	    exit(0);*/
	}
      else if (strncmp(argv[i],"--mdio-mmd-write=",17) == 0)
	{
	  char *colon  = strchr(argv[i]+17,':');
	  char *colon2 = strchr(colon ? colon+1 : "",':');
	  char *equals = strchr(argv[i]+17,'=');

	  if (!colon || !colon2 || !equals)
	    {
	      fprintf (stderr, "Bad --mdio-mmd-write=.\n");
	      fnet_ctrl_usage(argv[0]);
	      exit(1);
	    }
	  do_mdio_mmd_write = 1;
	  phy_addr = atoi(argv[i]+13);
	  mmd_dev  = atoi(colon+1);
	  mmd_reg  = atoi(colon2+1);
	  phy_value = strtol(equals+1,NULL,16);
	  /*printf ("%d %d %04x\n", phy_addr, phy_reg, phy_value);
	    exit(0);*/
	}
      else if (strcmp(argv[i],"--spi-read") == 0)
	do_spi_read = 1;
      else if (strncmp(argv[i],"--send-waveform=",17) == 0)
        {
          do_send_waveform = 1;
          send_waveform_filename = argv[i] + 17;
        }
      else if (strncmp(argv[i],"--ntpq=",7) == 0)
	{
	  do_ntpq = 1;
	  ntpq_cmd = argv[i] + 7;
	}
      else if (strcmp(argv[i],"--tcp-reset") == 0)
	do_tcp_reset = 1;
      else if (strcmp(argv[i],"--tcp") == 0)
	do_tcp_read = 1;
      else if (strncmp(argv[i],"--tcp=",6) == 0)
	{
	  do_tcp_read = 1;
	  if (strcmp(argv[i]+6,"dump") == 0)
	    {
	      tcp_handler = fnet_tcp_dump;
	      tcp_show_progess = 0;
	    }
	  else if (strcmp(argv[i]+6,"hex") == 0)
	    {
	      tcp_handler = fnet_tcp_hexdump;
	      tcp_show_progess = 0;
	    }
	  else if (strcmp(argv[i]+6,"local") == 0)
	    {
	      fnet_tcp_local_verify_info_init(&local_verify_info);
	      tcp_handler = fnet_tcp_local_verify;
	      tcp_handler_pinfo = &local_verify_info;
	    }
	  else if (strcmp(argv[i]+6,"gps") == 0 ||
		   strcmp(argv[i]+6,"func") == 0)
	    {
	      tcp_handler = fnet_tcp_lcl_func_mon;
	      tcp_handler_pinfo = &lcl_func_mon_info;
	      tcp_show_progess = 0;
	    }
	  else
	    {
	      fprintf (stderr, "Unknown TCP read style '%s'.\n", argv[i]+6);
	      exit(1);
	    }
	}
      else if (strcmp(argv[i],"--btn2led") == 0)
	do_btn2led = 1;
      else if (strncmp(argv[i],"--udp-flood=",12) == 0)
	{
	  do_udp_flood = 1;
	  items_per_packet = atoi(argv[i]+12);
	}
      else if (strncmp(argv[i],"--tcp-payload=",14) == 0)
	{
	  do_testreg = 1;
	  testreg_addr = FAKERNET_REG_ACCESS_ADDR_INT_TEST_CTRL_REG | 0;
	  testreg_value = atoi(argv[i]+14);
	}
      else if (strncmp(argv[i],"--tcp-window=",13) == 0)
	{
	  do_testreg = 1;
	  testreg_addr = FAKERNET_REG_ACCESS_ADDR_INT_TEST_CTRL_REG | 1;
	  testreg_value = atoi(argv[i]+13);
	}
      else if (strncmp(argv[i],"--tcp-lcl-datagen=",18) == 0)
	{
	  do_testreg = 1;
	  testreg_addr = FAKERNET_REG_ACCESS_ADDR_INT_TEST_CTRL_REG | 2;
	  testreg_value = atoi(argv[i]+18);
	}
      else if (strncmp(argv[i],"--tcp-lcl-data-chance=",22) == 0)
	{
	  do_testreg = 1;
	  testreg_addr = FAKERNET_REG_ACCESS_ADDR_INT_TEST_CTRL_REG | 3;
	  testreg_value = strtoul(argv[i]+22, NULL, 0);
	}
      else if (strncmp(argv[i],"--tcp-lcl-data-valmask=",23) == 0)
	{
	  do_testreg = 1;
	  testreg_addr = FAKERNET_REG_ACCESS_ADDR_INT_TEST_CTRL_REG | 4;
	  testreg_value = strtoul(argv[i]+23, NULL, 0);
	}
      else if (strncmp(argv[i],"--tcp-lcl-data-mark=",20) == 0)
	{
	  do_testreg = 1;
	  testreg_addr = FAKERNET_REG_ACCESS_ADDR_INT_TEST_CTRL_REG | 5;
	  testreg_value = atoi(argv[i]+20);
	}
      else if (strncmp(argv[i],"--read=",7) == 0)
	{
	  do_reg_read = 1;
	  reg_addr = strtoul(argv[i]+7, &end, 0);
	  if (*end != '\0')
	    {
	      fprintf (stderr, "Bad --read address: %s\n", argv[i]+7);
	      exit(1);
	    }
	}
      else if (strncmp(argv[i],"--write=",8) == 0)
	{
	  const char *colon;

	  do_reg_write = 1;
	  reg_addr = strtoul(argv[i]+8, &end, 0);
	  if (*end != ':')
	    {
	      fprintf (stderr, "Bad --write address: %s\n", argv[i]+8);
	      exit(1);
	    }
	  colon = end;
	  reg_value = strtoul(colon+1, &end, 0);
	  if (*end != '\0')
	    {
	      fprintf (stderr, "Bad --write value: %s\n", argv[i]+8);
	      exit(1);
	    }
	}
      else
	{
	  hostname = argv[i];
	  /*
	    fprintf (stderr, "Unhandled argument '%s'.\n", argv[i]);
	    exit(1);
	  */
	}
    }

  if (!hostname)
    {
      fnet_ctrl_usage(argv[0]);
      exit(1);
    }

  if (reliable == -1)
    {
      /* It was not specified if we should use a reliable or
       * the idempotent access channel.
       *
       * We only need the reliable channel for control access.
       * Also use it as default for UDP flood tests.
       */

      if (do_tcp_reset ||
	  do_tcp_read ||
	  do_testreg ||
	  do_udp_flood)
	reliable = 1;
      else
	reliable = 0;
    }

  if (0)
    {
      char debug_filename[256];

      sprintf (debug_filename,"udp_%ld_%d.dbg", (long) time(NULL), getpid());

      _fnet_debug_fid = fopen(debug_filename, "w");

      if (!_fnet_debug_fid)
	{
	  printf ("Failed to open %s for debug output.\n", debug_filename);
	  exit(1);
	}
      printf ("Debug output: %s\n", debug_filename);
    }

  if (strcmp(hostname, "-") == 0)
    {
      if (!do_tcp_read)
	{
	  printf ("Hostname '-' for local read "
		  "only makes sense with TCP input.\n");
	  exit(1);
	}

      tcp_sockfd = STDIN_FILENO;

      /* Bypass other client users, as client is NULL. */
      goto tcp_read;
    }
  else
    {
      client = fnet_ctrl_connect(hostname, reliable,
				 &error_ptr,
				 debug ? stderr : NULL);

      if (!client)
	{
	  fprintf (stderr,
		   "Failed to establish control connection "
		   "to host %s: %s\n",
		   hostname, error_ptr);
	  exit(1);
	}
    }

  if (do_reg_read)
    fnet_reg_rw(client, reg_addr, 0, 0);
  if (do_reg_write)
    fnet_reg_rw(client, reg_addr, reg_value, 1);

  /* The following sets registers once, and can then do another action. */

  if (do_testreg)
    fnet_set_test_reg(client, testreg_addr, testreg_value);

  if (do_tcp_reset)
    fnet_tcp_reset(client);

  /* The following routines never return. */

  if (do_tcp_read)
    {
     tcp_read:
      fnet_tcp_read(client, tcp_sockfd,
		    tcp_handler, tcp_handler_pinfo, tcp_show_progess);

      if (!client)
	goto end;
    }

  if (do_stat)
    fnet_ctrl_stat(client);

  if (do_mdio)
    fnet_mdio_action(client, phy_addr, mdio_action);

  if (do_mdio_write)
    fnet_mdio_do_write(client, phy_addr, phy_reg, phy_value);

  if (do_mdio_mmd_write)
    fnet_mdio_mmd_do_write(client, phy_addr, mmd_dev, mmd_reg, phy_value);

  if (do_mdio_read)
    fnet_mdio_dump(client, phy_addr);

  if (do_spi_read)
    fnet_spi_read(client);

  if (do_send_waveform)
    fnet_send_waveform(client, send_waveform_filename);

  if (do_readout_stat)
    fnet_ctrl_readout_stat(client);

  if (do_ntpq)
    fnet_ntpq_fire(client, ntpq_cmd);

  if (do_btn2led)
    fnet_btn2led(client);

  if (do_udp_flood)
    fnet_udp_flood(client, items_per_packet, reg_addr);

  fnet_ctrl_close(client);

 end:
  return 0;
}
