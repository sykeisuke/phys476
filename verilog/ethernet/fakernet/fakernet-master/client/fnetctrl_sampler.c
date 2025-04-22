
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
#include <inttypes.h>

/*************************************************************************/

size_t fnet_tcp_sampler_mon(char *buffer, size_t n, uint64_t handled,
			    void *pinfo, uint32_t *pcur)
{
  uint32_t *p = (uint32_t *) buffer;
  uint32_t *ok = p;

  (void) pinfo;
  (void) handled;
  (void) pcur;

  if (n >= sizeof (uint32_t))
    {
      if (n < 2*sizeof (uint32_t))
	goto incomplete_message;

      {
	uint32_t w1, w2;
	uint32_t ch;
	uint64_t t;
	uint32_t sub_pattern;
	uint32_t flip_sub;
	uint32_t sub_t;
	int32_t ssub_t;

	static uint64_t prev_edge[16][2];
	int edge = 0;

	w1 = htonl(*p); p++; n -= sizeof (uint32_t);
	w2 = htonl(*p); p++; n -= sizeof (uint32_t);

	ch = (w1 >> 20) & 0x0f;

	sub_pattern = w1 & 0x0003ffff;
	flip_sub = sub_pattern ^ (sub_pattern >> 1);

	for (sub_t = 0; sub_t < 15; sub_t++)
	  if (flip_sub & (1 << sub_t))
	    break;

	t = (((uint64_t) w2) << 4) + (15 - sub_t);

	if (w1 & 0x00020000) /* Falling. */
	  edge = 1;

	printf ("Sampler-%d: %08x %08x %010" PRIx64 ""
		" (- %08" PRIx64 " = %" PRId64 " ; "
		"- %08" PRIx64 " = %" PRId64 ")\n",
		ch, w1, w2, t,
		prev_edge[ch][edge],
		(t - prev_edge[ch][edge]) & (uint64_t) 0xfffffffffll,
		prev_edge[1][edge],
		(t - prev_edge[1][edge]) & (uint64_t) 0xfffffffffll);

	prev_edge[ch][edge] = t;

	/* For .vcd output. */

	for (ssub_t = 15; ssub_t >= 0; ssub_t--)
	  if (flip_sub & (1 << ssub_t))
	    {
	      t = (((uint64_t) w2) << 4) + (15 - sub_t);

	      printf ("VCD:#%012" PRId64 "\n", t);
	      printf ("VCD:%d%c\n", !!(w1 & (1 << ssub_t)), 'a' + ch);
	    }
      }

      ok = p;
    }

 incomplete_message:
  /* We have not gotten the full message yet, will try again next round: */

  return ((char *) ok) - buffer;
}
