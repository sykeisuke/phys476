
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

/*************************************************************************/


void fnet_spi_read(struct fnet_ctrl_client *client)
{
  fakernet_reg_acc_item *send;
  fakernet_reg_acc_item *recv;
  int num_send;
  int ret;
  int first = 1;

  uint32_t addr;
  uint32_t prev_addr = (uint32_t) -1;

  fnet_ctrl_get_send_recv_bufs(client, &send, &recv);

  /* Note: since the PSI reading takes more time than the UDP register
   * access processing allows, first do the read of the previusly.
   *
   * Read duration ~32 * 8 * 10 ns = 2.5 us.
   *
   * The network turnaround time is large enough for the access to
   * complete.
   */

  for (addr = 0; addr < 0x1000000 + 1; addr++)
    {
      /* First read the result of the previous access. */
      send[0].addr =
	htonl(FAKERNET_REG_ACCESS_ADDR_READ | 7);
      send[0].data = htonl(0);

      /* Request the next address. */
      send[1].addr =
	htonl(FAKERNET_REG_ACCESS_ADDR_WRITE | 7);
      send[1].data = htonl(addr);

      num_send = 2;

      ret = fnet_ctrl_send_recv_regacc(client, num_send);

      if (ret != 1)
	{
	  fprintf (stderr, "Failed: %s\n",
		   fnet_ctrl_last_error(client));
	  exit(1);
	}

      if (!first)
	{
	  uint32_t data;

	  data = ntohl(recv[0].data);

	  if ((data & 0xffffff00) != 0x80000100)
	    {
	      fprintf (stderr, "SPI read marker wrong: addr 0x%08x => 0x%08x\n",
		       prev_addr, data);
	      exit(1);
	    }

	  printf ("%c",
		  data & 0xff);
	}

      prev_addr = addr;

      first = 0;
    }
}
