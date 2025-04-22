
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

/*************************************************************************/

void fnet_udp_flood(struct fnet_ctrl_client *client,
		    int items_per_packet,
		    uint32_t reg_addr)
{
  struct timeval t_prev, t_now;
  int packets;
  int ret;

  int prev_packets   = 0;
  int prev_timeout   = 0;
  int prev_malformed = 0;

  fnet_ctrl_client_stats *stats = fnet_ctrl_get_stats(client);

  if (reg_addr == (uint32_t) -1)
    reg_addr = FAKERNET_REG_ACCESS_ADDR_VERSION;

  timerclear(&t_prev);

  for (packets = 0; ; packets++)
    {
      fakernet_reg_acc_item *send;
      fakernet_reg_acc_item *recv;
      int num_send;
      int k;

      fnet_ctrl_get_send_recv_bufs(client, &send, &recv);

      for (k = 0; k < items_per_packet; k++)
	{
	  /* Do flood access to the internal registers. */
	  send[k].addr =
	    htonl(FAKERNET_REG_ACCESS_ADDR_READ | reg_addr);
	  send[k].data = htonl(0);
	}

      num_send = items_per_packet;

      ret = fnet_ctrl_send_recv_regacc(client, num_send);

      if (ret != 1)
	{
	  fprintf (stderr, "Failed: %s\n",
		   fnet_ctrl_last_error(client));
	  continue;
	}

      gettimeofday(&t_now, NULL);

      if (t_now.tv_sec <  t_prev.tv_sec ||
	  t_now.tv_sec >= t_prev.tv_sec + 10)
	{
	  double elapsed;
	  int diff_packets = packets - prev_packets;

	  int diff_timeout =
	    stats->recv_timeout - prev_timeout;
	  int diff_malformed =
	    stats->malformed_packet - prev_malformed;

	  elapsed = (t_now.tv_sec - t_prev.tv_sec) +
	    0.000001 * (t_now.tv_usec - t_prev.tv_usec);

	  printf("packets: %d, "
		 "incr: %d (%.0f/s ; %.1f us/pkt) "
		 "%.2f MB/s, "
		 "(to: %d, mf: %d, to_us: %d)"
		 "\n",
		 packets,
		 diff_packets, diff_packets/elapsed,
		 elapsed / diff_packets * 1.e6,
		 items_per_packet*4*diff_packets/elapsed/1.e6,
		 diff_timeout, diff_malformed,
		 fnet_ctrl_get_udp_timeout_usec(client));
	  t_prev = t_now;
	  prev_packets   = packets;
	  prev_timeout   = stats->recv_timeout;
	  prev_malformed = stats->malformed_packet;
	}
    }
}

void fnet_btn2led(struct fnet_ctrl_client *client)
{
  uint32_t rgb_out = 0;
  uint32_t rgb_cnt = 0;
  int ret;

  for ( ; ; )
    {
      fakernet_reg_acc_item *send;
      fakernet_reg_acc_item *recv;
      int num_send;

      uint32_t btn;
      static uint32_t prevbtn = (uint32_t) -1;
      int nbtn = 0;

      fnet_ctrl_get_send_recv_bufs(client, &send, &recv);

      send[0].addr =
	htonl(FAKERNET_REG_ACCESS_ADDR_READ | (1));
      send[0].data = htonl(0);

      send[1].addr =
	htonl(FAKERNET_REG_ACCESS_ADDR_WRITE | (2));
      send[1].data = htonl(rgb_out);
      send[2].addr =
	htonl(FAKERNET_REG_ACCESS_ADDR_WRITE | (3));
      send[2].data = htonl(0x0800);

      num_send = 3;

      ret = fnet_ctrl_send_recv_regacc(client, num_send);

      if (ret != 1)
	{
	  fprintf (stderr, "Failed: %s\n",
		   fnet_ctrl_last_error(client));
	  continue;
	}

      btn = ntohl(recv[0].data);

      if (btn != prevbtn)
	{
	  printf("BTN: %08x = %d%d%d%d%d%d%d%d\n",
		 btn,
		 !!(btn & 0x01), !!(btn & 0x02),
		 !!(btn & 0x04), !!(btn & 0x08),
		 !!(btn & 0x10), !!(btn & 0x20),
		 !!(btn & 0x40), !!(btn & 0x80));
	  prevbtn = btn;
	}

      nbtn =
	!!(btn & 0x01) + !!(btn & 0x02) + !!(btn & 0x04) + !!(btn & 0x08) +
	!!(btn & 0x10) + !!(btn & 0x20) + !!(btn & 0x40) + !!(btn & 0x80);

      rgb_cnt++;

      rgb_out =
	((rgb_cnt & 0x10) ? 0x008 : 0) |
	((rgb_cnt & 0x20) ? 0x080 : 0) |
	((rgb_cnt & 0x40) ? 0x800 : 0) |
	((nbtn >= 1) ? 0x001 : 0) |
	((nbtn >= 2) ? 0x002 : 0) |
	((nbtn >= 3) ? 0x004 : 0) |
	((nbtn >= 4) ? 0x010 : 0) |
	((nbtn >= 5) ? 0x020 : 0) |
	((nbtn >= 6) ? 0x040 : 0) |
	((nbtn >= 7) ? 0x100 : 0) |
	((nbtn >= 8) ? 0x200 : 0);

      usleep(10000);
    }
}

#define NUM_TEST_REGISTERS 6

void fnet_set_test_reg(struct fnet_ctrl_client *client,
		       uint32_t testreg_addr,
		       uint32_t testreg_value)
{
  fakernet_reg_acc_item *send;
  fakernet_reg_acc_item *recv;
  int num_send;
  int ret;

  uint32_t testreg[NUM_TEST_REGISTERS];
  int k;

  uint32_t commit_chance;
  uint32_t commit_lenmask;
  uint32_t commit_lenmax;

  fnet_ctrl_get_send_recv_bufs(client, &send, &recv);

  send[0].addr =
    htonl(FAKERNET_REG_ACCESS_ADDR_WRITE | testreg_addr);
  send[0].data = htonl(testreg_value);

  for (k = 0; k < NUM_TEST_REGISTERS; k++)
    {
      send[1+k].addr =
	htonl(FAKERNET_REG_ACCESS_ADDR_READ |
	      (FAKERNET_REG_ACCESS_ADDR_INT_TEST_CTRL_REG + k));
      send[1+k].data = htonl(0);
    }

  num_send = (1+NUM_TEST_REGISTERS);

  ret = fnet_ctrl_send_recv_regacc(client, num_send);

  if (ret != 1)
    {
      fprintf (stderr, "Failed: %s\n",
	       fnet_ctrl_last_error(client));
      exit(1);
    }

  for (k = 0; k < NUM_TEST_REGISTERS; k++)
    testreg[k] = ntohl(recv[1+k].data);

  commit_chance  = testreg[3];
  commit_lenmask = (testreg[4] >> 8) & 0xff;
  commit_lenmax  = testreg[4] & 0xff;

  double commit_prob;
  double commit_avg_len;
  uint32_t i;

  /* Probability that a commit is attempted. */
  commit_prob = (commit_chance / (double) 0xffffffff);

  commit_avg_len = 0.;
  /* Simply evaluate each possibility. */
  for (i = 0; i <= 0x3f; i++)
    {
      uint32_t try_len = i & commit_lenmask;

      if (try_len &&
	  try_len <= commit_lenmax)
	{
	  commit_avg_len += try_len;
	}
    }
  commit_avg_len /= (0x3f+1);

  printf ("max-payload: %d  max-window: %d\n",
	  testreg[0], testreg[1]);
  printf ("do_local: %d m:%x "
	  "commit-chance: %" PRIu32 "  commit-max/mask: 0x%02x/0x%02x  "
	  "%.3f kB/s\n",
	  testreg[2],
	  testreg[5],
	  commit_chance,
	  commit_lenmax, commit_lenmask,
	  commit_prob *
	  commit_avg_len *
	  4 /* units of 4 bytes */ * 100.e6 /* operating freq */ / 1e3 /* kB*/);
}

void fnet_reg_rw(struct fnet_ctrl_client *client,
		 uint32_t reg_addr,
		 uint32_t reg_value,
		 int write)
{
  fakernet_reg_acc_item *send;
  fakernet_reg_acc_item *recv;
  int num_send;
  int ret;

  fnet_ctrl_get_send_recv_bufs(client, &send, &recv);

  if (write)
    {
      send[0].addr =
	htonl(FAKERNET_REG_ACCESS_ADDR_WRITE | reg_addr);
      send[0].data = htonl(reg_value);
    }
  else
    {
      send[0].addr =
	htonl(FAKERNET_REG_ACCESS_ADDR_READ | reg_addr);
      send[0].data = htonl(0);
    }

  num_send = 1;

  ret = fnet_ctrl_send_recv_regacc(client, num_send);

  if (ret != 1)
    {
      fprintf (stderr, "Failed: %s\n",
	       fnet_ctrl_last_error(client));
      exit(1);
    }

  if (write)
    printf ("Wrote 0x%08x -> 0x%08x\n", reg_addr, reg_value);
  else
    printf ("Read  0x%08x -> 0x%08x\n", reg_addr, ntohl(recv[0].data));
}
