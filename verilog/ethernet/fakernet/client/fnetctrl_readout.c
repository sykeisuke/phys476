
/* Copyright (c) 2023, Haakan T. Johansson */
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

#define NUM_VER_REGISTERS             16
#define NUM_READOUT_DEBUG_REGISTERS    2
#define NUM_READOUT_DEBUG_COUNTERS     5

void fnet_ctrl_readout_stat(struct fnet_ctrl_client *client)
{
  uint32_t prev_cnt[NUM_READOUT_DEBUG_COUNTERS];
  uint32_t prev_val[NUM_READOUT_DEBUG_REGISTERS];
  struct timeval t_prev, t_now, t_after;
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
      }

      fnet_ctrl_get_send_recv_bufs(client, &send, &recv);

      {
	int k;
	int kk = 0;

	if (NUM_READOUT_DEBUG_REGISTERS +
	    NUM_READOUT_DEBUG_COUNTERS >
	    FAKERNET_REG_ACCESS_MAX_ITEMS)
	  {
	    fprintf (stderr, "Too many items for register access packet "
		     "(%d > %d).\n",
		     NUM_READOUT_DEBUG_REGISTERS +
		     NUM_READOUT_DEBUG_COUNTERS,
		     FAKERNET_REG_ACCESS_MAX_ITEMS);
	    exit(1);
	  }

	for (k = 0; k < NUM_READOUT_DEBUG_REGISTERS; k++, kk++)
	  {
	    send[kk].addr =
	      htonl(FAKERNET_REG_ACCESS_ADDR_READ |
		    (FAKERNET_REG_ACCESS_ADDR_INT_STAT + k));
	    send[kk].data = htonl(0);
	  }

	for (k = 0; k < NUM_READOUT_DEBUG_COUNTERS; k++, kk++)
	  {
	    send[kk].addr =
	      htonl(FAKERNET_REG_ACCESS_ADDR_READ |
		    (0x1100 + k));
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

	uint32_t c_cnt[NUM_READOUT_DEBUG_COUNTERS];
	uint32_t c_val[NUM_READOUT_DEBUG_REGISTERS];
	/*
	double elapsed;

	elapsed = (t_now.tv_sec - t_prev.tv_sec) +
	  0.000001 * (t_now.tv_usec - t_prev.tv_usec);
	*/
	for (k = 0; k < NUM_READOUT_DEBUG_REGISTERS; k++, kk++)
	    c_val[k] = ntohl(recv[kk].data);
	for (k = 0; k < NUM_READOUT_DEBUG_COUNTERS; k++, kk++)
	  {
	    uint32_t diff;

	    c_cnt[k] = ntohl(recv[kk].data);
	    diff = (c_cnt[k] - prev_cnt[k]);

	    sum_counts += diff;
	  }

	(void) c_val;

	fnet_ctrl_stat_print_descr(client, c_ver);

	fnet_ctrl_stat_print_conn_status(client, c_ver[0]);

	printf ("TS: %04x:%04x:%04x:%04x, sync: %d%d%d\n\n",
		(c_cnt[3] >> 16), (c_cnt[3] & 0xffff),
		(c_cnt[2] >> 16), (c_cnt[2] & 0xffff),
		!!(c_cnt[4] & 4),
		!!(c_cnt[4] & 2),
		!!(c_cnt[4] & 1));

	{
	  const stat_index_name lcl_count_names[] = {
	    { 0, 0, "buffer_no" },
	    { 1, 0, "event_no" },

	    { -1, 0, 0 },
	  };

	  fnet_ctrl_stat_print_diff(lcl_count_names, c_cnt, prev_cnt,
				    FLAG_IS_DIFFABLE,
				    STAT_PRINT_IDX, 33, -0.);

	  memcpy (prev_cnt, c_cnt,
		  NUM_READOUT_DEBUG_COUNTERS * sizeof (prev_cnt[0]));
	}
	printf("\n");
      }
      /* Dump buffer to colouriser. */
      fflush(stdout);

      usleep(1000000);
    }
}
