
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
#include "fnetctrl.h"

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <assert.h>
#include <ctype.h>

/*************************************************************************/

#define NUM_VER_REGISTERS     16
#define NUM_DEBUG_REGISTERS   32
#define NUM_DEBUG_COUNTERS    80 /* 80 used */

void fnet_ctrl_stat_print_diff(const stat_index_name *idx_names,
			       const uint32_t *vals,
			       const uint32_t *prev_vals,
			       int flags_all,
			       int print_flags, int name_chars,
			       double slow_tick_time)
{
  int k;

  for (k = 0; idx_names[k].index != -1; k++)
    {
      int index = idx_names[k].index;
      int flags = idx_names[k].flags | flags_all;
      uint32_t val = vals[index];
      uint32_t diff = val - prev_vals[index];

      if (print_flags & STAT_PRINT_IDX)
	printf ("[%2d] ", index);

      printf("%-*s ",
	     name_chars, idx_names[k].name);

      if (print_flags & STAT_PRINT_HEX)
	printf("0x%8x = ", val);

      if (flags & FLAG_IS_SLOW_TICKS)
	{
	  printf("%10u = %.1f us\n",
		 val,
		 val *
		 slow_tick_time * 1.e6);
	}
      else if (flags & FLAG_IS_DIFFABLE)
	{
	  printf("%10u ; %10u\n",
		 val, diff);
	}
      else if (flags & FLAG_IS_IP)
	{
	  printf("%d.%d.%d.%d\n",
		 (val >> 24) & 0xff,
		 (val >> 16) & 0xff,
		 (val >>  8) & 0xff,
		 (val      ) & 0xff);
	}
      else
	{
	  printf("%10u\n", val);
	}

      if (flags & FLAG_WORDS_DIV_64)
	{
	  uint64_t val64 = val;
	  uint64_t diff64 = diff;

	  /* Prescaled by factor 32, *2 for words -> bytes. */
	  val64 *= 64;
	  diff64 *= 64;

	  if (print_flags & STAT_PRINT_IDX)
	    printf ("     ");
	  printf("%-*s "
		 "%13" PRIu64 " ; %10" PRIu64"\n",
		 name_chars - 3, "bytes",
		 val64, diff64);
	}
    }
}

void fnet_ctrl_stat_print_raw_vals(const uint32_t *vals, int n,
				   uint32_t base_offset)
{
  int k;

  for (k = 0; k < n; k++)
    {
      if (k % 8 == 0)
	printf("%03x:", base_offset + k);
      printf(" %08x", vals[k]);
      if (k % 8 == 7)
	printf("\n");
    }
  if (n % 8 != 0)
    printf("\n");
}

void fnet_ctrl_stat_print_descr(struct fnet_ctrl_client *client,
				uint32_t *ver)
{
  int i, j;
  char description[48+1];

  (void) client;

  for (i = 0; i < 12; i++)
    for (j = 0; j < 4; j++)
      description[i*4+j] = (ver[i+4] >> (j * 8)) & 0xff;

  printf ("Descr: ");
  for (i = 0; i < 48; i++)
    {
      char c = description[i];

      if (!c)
	break;
      if (isprint(c))
	printf ("%c", c);
      else
	printf ("\\x%02x", c);
    }
  printf ("\n");
}

void fnet_ctrl_stat_print_conn_status(struct fnet_ctrl_client *client,
				      uint32_t compile_time)
{
  uint32_t status_udp_channels;
  uint32_t status_tcp;
  const char *tcp_conn_str = "?";
  time_t ct;
  char ct_date[64];

  fnet_ctrl_get_last_udp_status(client,
				&status_udp_channels, &status_tcp);

  switch (status_tcp & FAKERNET_STATUS2_TCP_CONN_MASK)
    {
    case FAKERNET_STATUS2_TCP_IDLE:
      tcp_conn_str = "idle";
      break;
    case FAKERNET_STATUS2_TCP_GOT_SYN:
      tcp_conn_str = "syn";
      break;
    case FAKERNET_STATUS2_TCP_CONNECTED:
      tcp_conn_str = "connected";
      break;
    }

  ct = (time_t) compile_time;
  strftime(ct_date,sizeof(ct_date),"%Y-%m-%d %H:%M:%S UTC",gmtime(&ct));

  printf("UDP: connected %02x, active %02x               "
	 "Compiled: %s\n",
	 status_udp_channels & 0xff,
	 (status_udp_channels >> 8) & 0xff,
	 ct_date);
  printf("TCP: %s%s%s%s%s\n",
	 tcp_conn_str,
	 status_tcp &
	 FAKERNET_STATUS2_TCP_COMMIT_OVERFLOW ?
	 " COMMIT-OVERFLOW" : "",
	 status_tcp &
	 FAKERNET_STATUS2_TCP_WRITE_OVERFLOW ?
	 " WRITE-OVERFLOW" : "",
	 status_tcp & FAKERNET_STATUS2_TCP_RAM_BITFLIP ?
	 " TCP-RAM-BITFLIP" : "",
	 status_tcp & FAKERNET_STATUS2_ANY_RAM_BITFLIP ?
	 " ANY-RAM-BITFLIP" : "");
  printf("\n");
}

void fnet_ctrl_stat(struct fnet_ctrl_client *client)
{
  uint32_t prev_cnt[NUM_DEBUG_COUNTERS];
  uint32_t prev_val[NUM_DEBUG_REGISTERS];
  struct timeval t_prev, t_now, t_after;
  double dt_prev = 0.0;
  int packets;
  int ret;

  /* Colourise the output, if we are printing to a tty. */
  fnet_fork_colouriser(STDOUT_FILENO, "fnet_stat_colour.pl");

  memset(prev_cnt, 0, sizeof (prev_cnt));
  memset(prev_val, 0, sizeof (prev_val));
  timerclear(&t_prev);

  for (packets = 0; ; packets++)
    {
      fakernet_reg_acc_item *send;
      fakernet_reg_acc_item *recv;
      int num_send;

      uint32_t c_ver[NUM_VER_REGISTERS];

      printf ("========================================"
	      "=======================================\n");

      fnet_ctrl_get_send_recv_bufs(client, &send, &recv);

      {
	int k;
	int kk = 0;

	if (NUM_VER_REGISTERS >
	    FAKERNET_REG_ACCESS_MAX_ITEMS)
	  {
	    fprintf (stderr, "Too many items for version query access packet "
		     "(%d > %d).\n",
		     NUM_VER_REGISTERS,
		     FAKERNET_REG_ACCESS_MAX_ITEMS);
	    exit(1);
	  }

	for (k = 0; k < NUM_VER_REGISTERS; k++, kk++)
	  {
	    send[kk].addr =
	      htonl(FAKERNET_REG_ACCESS_ADDR_READ |
		    (FAKERNET_REG_ACCESS_ADDR_VERSION + k));
	    send[kk].data = htonl(0);
	  }

	num_send = kk;
      }

      ret = fnet_ctrl_send_recv_regacc(client, num_send);

      if (ret != 1)
	{
	  fprintf (stderr, "Failed: %s\n",
		   fnet_ctrl_last_error(client));
	  continue;
	}

      {
	int k;
	int kk = 0;

	for (k = 0; k < NUM_VER_REGISTERS; k++, kk++)
	  c_ver[k] = ntohl(recv[kk].data);

	fnet_ctrl_stat_print_raw_vals(c_ver, NUM_VER_REGISTERS,
				      0x10);
      }

      fnet_ctrl_get_send_recv_bufs(client, &send, &recv);

      {
	int k;
	int kk = 0;

	if (NUM_DEBUG_REGISTERS +
	    NUM_DEBUG_COUNTERS >
	    FAKERNET_REG_ACCESS_MAX_ITEMS)
	  {
	    fprintf (stderr, "Too many items for register access packet "
		     "(%d > %d).\n",
		     NUM_DEBUG_REGISTERS +
		     NUM_DEBUG_COUNTERS,
		     FAKERNET_REG_ACCESS_MAX_ITEMS);
	    exit(1);
	  }

	for (k = 0; k < NUM_DEBUG_REGISTERS; k++, kk++)
	  {
	    send[kk].addr =
	      htonl(FAKERNET_REG_ACCESS_ADDR_READ |
		    (FAKERNET_REG_ACCESS_ADDR_INT_STAT + k));
	    send[kk].data = htonl(0);
	  }

	for (k = 0; k < NUM_DEBUG_COUNTERS; k++, kk++)
	  {
	    send[kk].addr =
	      htonl(FAKERNET_REG_ACCESS_ADDR_READ |
		    (FAKERNET_REG_ACCESS_ADDR_INT_COUNT + k));
	    send[kk].data = htonl(0);
	  }

	num_send = kk;
      }

      t_prev = t_now;
      gettimeofday(&t_now, NULL);

      ret = fnet_ctrl_send_recv_regacc(client, num_send);

      if (ret != 1)
	{
	  fprintf (stderr, "Failed: %s\n",
		   fnet_ctrl_last_error(client));
	  continue;
	}

      gettimeofday(&t_after, NULL);

      {
	int k;
	int kk = 0;
	uint32_t sum_counts = 0;
	uint32_t slow_tick_diff = 0;

	uint32_t c_cnt[NUM_DEBUG_COUNTERS];
	uint32_t c_val[NUM_DEBUG_REGISTERS];

	double elapsed, dt;

	elapsed = (t_now.tv_sec - t_prev.tv_sec) +
	  0.000001 * (t_now.tv_usec - t_prev.tv_usec);
	dt = (t_after.tv_sec - t_now.tv_sec) +
	  0.000001 * (t_after.tv_usec - t_now.tv_usec);

	double slow_tick_time = 0;

	for (k = 0; k < NUM_DEBUG_REGISTERS; k++, kk++)
	  c_val[k] = ntohl(recv[kk].data);

	for (k = 0; k < NUM_DEBUG_COUNTERS; k++, kk++)
	  {
	    uint32_t diff;

	    c_cnt[k] = ntohl(recv[kk].data);
	    diff = (c_cnt[k] - prev_cnt[k]);

	    sum_counts += diff;
	    if (k == 26)
	      slow_tick_diff = diff;
	  }

	fnet_ctrl_stat_print_raw_vals(c_val, NUM_DEBUG_REGISTERS,
				      0x20);
	fnet_ctrl_stat_print_raw_vals(c_val, NUM_DEBUG_COUNTERS,
				      0x200);

	fnet_ctrl_stat_print_descr(client, c_ver);

	fnet_ctrl_stat_print_conn_status(client, c_ver[0]);

	if (timerisset(&t_prev))
	  slow_tick_time = elapsed / slow_tick_diff;
	printf("Total counts: %10u (%7.3f MHz +/- %5.3f)\n",
	       sum_counts,
	       sum_counts * 4 / elapsed * 1e-6,
	       sum_counts * 4 / elapsed * 1e-6 *
	       (dt + dt_prev) / elapsed);
	printf("Slow ticks:   %10u (%d) (%.2f us)\n",
	       slow_tick_diff,
	       sum_counts / slow_tick_diff,
	       slow_tick_time * 1.e6);
	printf("\n");

	dt_prev = dt;

	{
	  const stat_index_name lcl_reg_names[] = {
	    {  0, 0, "" },
	    {  1, FLAG_IS_DIFFABLE,
	       /**/  "tcp_stat.base_seqno" },
	    {  2, 0, "tcp_stat.max_sent" },
	    {  3, 0, "tcp_stat.filled" },
	    {  4, 0, "tcp_stat.unsent" },
	    {  5, 0, "tcp_stat.unfilled" },
	    {  6, 0, "tcp_stat.window_sz" },
	    {  7, 0, "tcp_stat.rtt_trip" },
	    {  8, 0, "tcp_astat.cur_off" },
	    {  9, 0, "tcp_stat.same_ack" },
	    { 10, FLAG_IS_SLOW_TICKS,
	      /**/   "tcp_stat.rtt_est" },
	    { 11, FLAG_IS_IP,
	      /**/   "in_stat.dyn_rarp_ip" },
	    { -1, 0, 0 },
	  };

	  fnet_ctrl_stat_print_diff(lcl_reg_names + 1, c_val, prev_val,
				    0, STAT_PRINT_HEX, 20, slow_tick_time);

	  memcpy (prev_val, c_val,
		  NUM_DEBUG_REGISTERS * sizeof (prev_val[0]));
	}
	printf ("\n");
	{
	  const stat_index_name lcl_count_names[] = {
	    {  0, 0, "idle_cnt" },
	    { 26, 0, "slow_tick" },
	    { 27, 0, "timeout_tick" },
	    { 58, FLAG_WORDS_DIV_64,
	      /* */  "in_words_div_64" },
	    {  1, 0, "+-in_info.start_packet" },
	    {  3, 0, "| `-in_info.start_arp" },
	    {  4, 0, "|   in_info.arp_our_ip" },
	    {  8, 0, "|   in_info.good_arp" },
	    { 68, 0, "|   in_info.good_rarp" },
	    {  2, 0, "`-in_info.mac_for_us" },
	    {  5, 0, "  in_info.start_ipv4" },
	    {  6, 0, "  in_info.ip_hdr_ok" },
	    {  7, 0, "  in_info.ip_cfg_for_us" },
	    { 69, 0, "  in_info.ip_dyn_for_us" },
	    {  9, 0, "  +-in_info.start_icmp" },
	    { 12, 0, "  | in_info.good_icmp" },
	    { 10, 0, "  +-in_info.start_udp" },
	    { 15, 0, "  | in_info.udp_arm" },
	    { 16, 0, "  | in_info.udp_badactivearm" },
	    { 17, 0, "  | in_info.udp_reset" },
	    { 18, 0, "  | in_info.udp_badreset" },
	    { 19, 0, "  | in_info.udp_disconnect" },
	    { 20, 0, "  | in_info.udp_baddisconnect" },
	    { 21, 0, "  | in_info.udp_regaccess" },
	    { 22, 0, "  | +-in_info.udp_ra_otherip" },
	    { 23, 0, "  |   +-in_info.udp_ra_seqplus1" },
	    { 24, 0, "  |   +-in_info.udp_ra_repeat" },
	    { 25, 0, "  |   `-in_info.udp_ra_busy" },
	    { 60, 0, "  | in_info.udp_regaccess_idp" },
	    { 61, 0, "  | `-in_info.udp_ra_idp_busy" },
	    { 13, 0, "  | in_info.good_udp" },
	    { 73, 0, "  +-in_info.start_bootp" },
	    { 74, 0, "  | in_info.good_bootp" },
	    { 77, 0, "  | in_info.good_dhcp_offer" },
	    { 78, 0, "  | in_info.good_dhcp_ack" },
	    { 71, 0, "  +-in_info.start_ntp" },
	    { 72, 0, "  | in_info.good_ntp" },
	    { 11, 0, "  `-in_info.start_tcp" },
	    { 14, 0, "    in_info.good_tcp" },
	    { 63, 0, "+-in_info.bad_cksum" },
	    { 64, 0, "+-in_info.bad_crc" },
	    { 65, 0, "+-in_info.incomplete" },
	    { 66, 0, "+-in_info.spurious" },
	    { 67, 0, "`-in_info.stop_parse" },

	    { 75, 0, "out_info.block_bug" },
	    { 59, FLAG_WORDS_DIV_64,
	      /* */  "out_words_div_64" },
	    { 44, 0, "out_info.packets" },
	    { 33, 0, "+-out_info.arp_icmp" },
	    { 76, 0, "+-out_info.drop_ntp" },
	    { 70, 0, "+-out_info.pkt_gen" },
	    { 62, 0, "+-out_info.udp_idp" },
	    { 34, 0, "+-out_info.udp[0]" },
	    { 35, 0, "+-out_info.udp[1]" },
	    { 36, 0, "+-out_info.udp[2]" },
	    { 37, 0, "+-out_info.udp[3]" },
	    /* 38 39 40 41 42 */
	    { 43, 0, "`-out_info.tcp" },

	    { 79, 0, "tcp_state.reset" },
	    { 55, 0, "tcp_state.got_syn" },
	    { 54, 0, "tcp_state.connected" },

	    { 49, 0, "tcp_state.got_ack" },
	    { 51, 0, "`-tcp_state.same_ack" },
	    { 52, 0, "  `-tcp_state.twice_same_ack" },

	    { 48, 0, "tcp_state.did_repeat" },
	    { 53, 0, "tcp_state.abort_repeat" },
	    { 57, 0, "tcp_state.did_keepalive" },

	    { 47, 0, "`-tcp_state.start_meas_rtt" },
	    { 50, 0, "  `-tcp_state.got_meas_rtt" },
	    { 56, 0, "    `-tcp_state.new_rtt_est" },

	    { -1, 0, 0 },
	  };

	  fnet_ctrl_stat_print_diff(lcl_count_names, c_cnt, prev_cnt,
				    FLAG_IS_DIFFABLE,
				    STAT_PRINT_IDX, 33, -0.);

	  memcpy (prev_cnt, c_cnt,
		  NUM_DEBUG_COUNTERS * sizeof (prev_cnt[0]));
	}
	printf("\n");
      }
      /* Dump buffer to colouriser. */
      fflush(stdout);

      usleep(1000000);
    }
}
