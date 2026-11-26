
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
#include <ctype.h>
#include <math.h>

/*************************************************************************/

void fnet_tcp_reset(struct fnet_ctrl_client *client)
{
  int ret;

  ret = fnet_ctrl_reset_tcp(client);

  if (ret < 0)
    {
      fprintf (stderr, "Failed: %s\n",
	       fnet_ctrl_last_error(client));
      exit(1);
    }
}

size_t fnet_tcp_dump(char *buffer, size_t n, uint64_t handled,
		     void *pinfo, uint32_t *pcur)
{
  char *ok = buffer;

  (void) handled;
  (void) pinfo;
  (void) pcur;

  /* The buffer is in network order.  We print in network order. */

  while (n)
    {
      putchar(*ok);

      ok++;
      n--;
    }

  return ((char *) ok) - buffer;
}

size_t fnet_tcp_hexdump(char *buffer, size_t n, uint64_t handled,
			void *pinfo, uint32_t *pcur)
{
  uint32_t *p = (uint32_t *) buffer;
  uint32_t *ok = p;

  (void) pinfo;

  while (n > 4 * sizeof (uint32_t))
    {
      printf ("%08" PRIx64 " ", handled + ((char *) ok) - buffer);

      for (size_t j = 0; j < 4; j++)
	{
	  printf (" %08x",
		  ntohl(*(ok+j)));
	}

      printf ("  |");

      for (size_t j = 0; j < 4; j++)
	{
	  uint32_t val = ntohl(*(ok+j));

	  for (size_t k = 0; k < 4; k++)
	    {
	      uint8_t c = (uint8_t) (val >> ((3-k) * 8));

	      printf ("%c",
		      isprint(c) ? c : '.');
	    }
	}

      printf ("|\n");

      *pcur = ntohl(*(ok + 7));

      ok += 4;
      n  -= 4 * sizeof (uint32_t);
    }

  fflush(stdout);

  return ((char *) ok) - buffer;
}


void fnet_tcp_local_verify_info_init(fnet_tcp_local_verify_info *info)
{
  info->last_seq = (uint32_t) -1;
  info->last_words = 0x3f;
  info->last_header = (uint32_t) -1;
}

size_t fnet_tcp_local_verify(char *buffer, size_t n, uint64_t handled,
			     void *pinfo, uint32_t *pcur)
{
  fnet_tcp_local_verify_info *info = (fnet_tcp_local_verify_info *) pinfo;

  uint32_t *p = (uint32_t *) buffer;
  uint32_t *ok = p;

  (void) handled;

  for ( ; ; )
    {
      uint32_t header;
      uint32_t words;
      uint32_t seq;
      uint32_t i;

      /* Do we have the full header word? */
      if (n < sizeof (uint32_t))
	break;

      header = ntohl(*p);

      if ((header & 0xff000000) != 0xa5000000)
	{
	  fprintf (stderr,
		   "Header word (0x%08x) has wrong marker.\n",
		   header);
	  goto error_dump_buffer;
	}

      words = (header >> 16) & 0xff;

      if (words == 0)
	{
	  fprintf (stderr,
		   "Header word (0x%08x) has wrong length.\n",
		   header);
	  goto error_dump_buffer;
	}

      if (((header >> 8) & 0xff) != info->last_words)
	{
	  fprintf (stderr,
		   "Header word (0x%08x) has "
		   "wrong prev length (0x%02x).\n",
		   header, info->last_words);
	  goto error_dump_buffer;
	}

      /* Do we have all the data for this commit group? */
      if (n < words * sizeof (uint32_t))
	break;

      seq = header & 0xff;

      if (seq != ((info->last_seq + 1) & 0xff))
	{
	  fprintf (stderr,
		   "Header word (0x%08x) has "
		   "wrong seq (last=0x%04x).\n",
		   header, info->last_seq);
	  goto error_dump_buffer;
	}

      p++;
      info->word_count++;

      /* Check the data words. */
      for (i = 1; i < words; i++)
	{
	  uint32_t raw = ntohl(*(p++));
	  uint32_t expect =
	    ((((~info->word_count) & 0x00ff) << 16)) |
	    (    info->word_count  & 0xffff);

	  if ((raw & 0x00ffffff) != expect)
	    {
	      fprintf (stderr,
		       "Data word (0x%08x) wrong "
		       "count (expect 0x%08x, count 0x%04x).\n",
		       raw, expect, info->word_count);
	      goto error_dump_buffer;
	    }

	  if (pcur)
	    *pcur = raw >> 24;

	  info->word_count++;
	}

      n -= words * sizeof (uint32_t);
      ok = p;

      info->total_checked += words * sizeof (uint32_t);

      info->last_seq = seq;
      info->last_words = words;
      info->last_header = header;

      continue;
    error_dump_buffer:

      fprintf (stderr,
	       "last header: 0x%08x, last seq: 0x%02x, last words: 0x%08x\n"
	       "total_checked: 0x%" PRIx64 "\n",
	       info->last_header,
	       info->last_seq,
	       info->last_words,
	       info->total_checked);

      for (size_t j = 0; j < 32 && j < n; j++)
	{
	  printf ("%08x %2d: %08x\n",
		  (uint32_t) (ok-(uint32_t*)buffer),
		  (int) j, ntohl(*(ok+j)));
	}
      exit(1);
    }

  /* Move whatever data is left such that it can be checked
   * next time.
   */

  return ((char *) ok) - buffer;
}

void fnet_tcp_xadc_show(fnet_tcp_xadc_mon_info *info)
{
  int ch;

  if (!info->_set)
    return;

  printf ("XADC:");

  for (ch = 0; ch < 64; ch++)
    {
      uint64_t ch_bit;

      ch_bit = (((uint64_t) 1) << ch);

      if (info->_set & ch_bit)
	{
	  uint16_t raw = info->_raw[ch];

	  if (ch == 0)
	    {
	      double temp_C;

	      temp_C = (raw >> 4) / 4096. * 503.975 - 273.15;

	      printf ("  temp = %.1f C", temp_C);
	    }
	  if (ch == 1 || ch == 2 || ch == 6)
	    {
	      double pot_V;
	      const char *chname = "";

	      if (ch == 1)
		chname = "ccint";
	      else if (ch == 2)
		chname = "ccaux";
	      else if (ch == 6)
		chname = "ccbram";

	      pot_V = (raw >> 4) / 4096. * 3.;

	      printf ("  V_%s = %.2f V", chname, pot_V);
	    }
	  if (ch == 16+1 || ch == 16+2 || ch == 16+9 || ch == 16+10)
	    {
	      double meas_V, meas_I;
	      const char *chname = "";

	      meas_V = (raw >> 4) / 4096.;

	      if (ch == 16+1) {
		chname = "5V";
		meas_V *= 5.99;
	      } else if (ch == 16+2) {
		chname = "VU";
		meas_V *= 16;
	      } else if (ch == 16+9) {
		chname = "5V";
		meas_I = meas_V / 0.250; /* V / A */
	      } else if (ch == 16+10) {
		chname = "core";
		meas_I = meas_V / 0.500; /* V / A */
	      }

	      if (ch == 16+1 || ch == 16+2)
		printf ("  V_%s = %.2f V", chname, meas_V);
	      else
		printf ("  I_%s = %.3f A", chname, meas_I);
	    }
	}
    }

  printf ("\n");
  fflush(stdout);

  info->_set = 0;
}

size_t fnet_tcp_xadc_mon(char *buffer, size_t n, uint64_t handled,
			 fnet_tcp_xadc_mon_info *info, uint32_t *pcur)
{
  uint32_t *p = (uint32_t *) buffer;
  uint32_t *ok = p;

  (void) handled;
  (void) pcur;

  if (n >= sizeof (uint32_t))
    {
      if (n < 1*sizeof (uint32_t))
	goto incomplete_message;

      {
	uint32_t w1;
	uint32_t ch;
	uint32_t raw;
	uint64_t ch_bit;

	w1 = htonl(*p); p++; n -= sizeof (uint32_t);

	ch  = (w1 >> 16) & 0x3f;
	raw =  w1        & 0xffff;

	/*printf ("XADC %d [%016" PRIu64 "]\n", ch, info->_set);*/

	ch_bit = (((uint64_t) 1) << ch);

	if (ch < info->_last_ch ||
	    (info->_set & ch_bit))
	  {
	    /* Unexpected wrap.  Or duplicate!
	     * Show what we had so far.
	     */
	    fnet_tcp_xadc_show(info);
	  }

	info->_raw[ch] = raw;
	info->_set |= ch_bit;

	if (ch == info->_prev_max_ch)
	  {
	    /* Heuristic - this is likely the highest channel we will reach.
	     * Show what we had so far.
	     */
	    fnet_tcp_xadc_show(info);
	  }

	/*printf ("XADC %d %d %d\n", ch, info->_last_ch, info->_prev_max_ch);*/

	if (ch < info->_last_ch)
	  info->_prev_max_ch = info->_last_ch;

	info->_last_ch = ch;

	/*printf ("XADC %d %d %d\n", ch, info->_last_ch, info->_prev_max_ch);*/
     }

      ok = p;
    }

 incomplete_message:
  /* We have not gotten the full message yet, will try again next round: */

  return ((char *) ok) - buffer;
}

size_t fnet_tcp_uart_mon(char *buffer, size_t n, uint64_t handled,
			 void *pinfo, uint32_t *pcur)
{
  uint32_t *p = (uint32_t *) buffer;
  uint32_t *ok = p;

  (void) handled;
  (void) pinfo;
  (void) pcur;

  if (n >= sizeof (uint32_t))
    {
      if (n < 1*sizeof (uint32_t))
	goto incomplete_message;

      {
	uint32_t w1;
	uint32_t rx_data;
	uint32_t tx_data;
	uint32_t rx_has;
	uint32_t tx_has;

	w1 = htonl(*p); p++; n -= sizeof (uint32_t);

	rx_data = (w1 >>  0) & 0xff;
	tx_data = (w1 >>  8) & 0xff;
	rx_has  = (w1 >> 16) &    1;
	tx_has  = (w1 >> 17) &    1;

	printf ("UART:");

	if (rx_has)
	  printf (" rx:%02x '%c'", rx_data, isprint(rx_data) ? rx_data : '.');
	else
	  printf ("          ");
	if (tx_has)
	  printf (" tx:%02x '%c'", tx_data, isprint(tx_data) ? tx_data : '.');
	else
	  printf ("          ");

	printf ("\n");
     }

      ok = p;
    }

 incomplete_message:
  /* We have not gotten the full message yet, will try again next round: */

  return ((char *) ok) - buffer;
}

size_t fnet_tcp_lcl_func_mon(char *buffer, size_t n, uint64_t handled,
			     void *pinfo, uint32_t *pcur)
{
  fnet_tcp_lcl_func_mon_info *info = (fnet_tcp_lcl_func_mon_info *) pinfo;

  char *start = buffer;
  char *ok = buffer;

  (void) handled;
  (void) pcur;

  while (n >= sizeof (uint32_t))
    {
      uint32_t *p = (uint32_t *) buffer;
      uint32_t word  = htonl(*p);
      uint32_t func = (word >> 24);

      size_t used = 0;

      switch (func)
	{
	case 1:
	  {
	    used = fnet_tcp_gps_mon(buffer, n, handled,
				    &(info->_gps), pcur);
	  }
	  break;
	case 2:
	  {
	    used = fnet_tcp_sampler_mon(buffer, n, handled,
					NULL, pcur);
	  }
	  break;
	case 3:
	  {
	    used = fnet_tcp_xadc_mon(buffer, n, handled,
				     &(info->_xadc), pcur);
	  }
	  break;
	case 4:
	  {
	    used = fnet_tcp_uart_mon(buffer, n, handled,
				     NULL, pcur);
	  }
	  break;
	default:
	  {
	    size_t i;

	    fprintf (stderr, "Unexpected function monitor word %08x", word);
	    for (i = 1; (i+1)*sizeof (uint32_t) <= n && i < 10; i++)
	      fprintf (stderr, ", %08x", htonl(p[i]));
	    fprintf (stderr, ".\n");
	    exit(1);
	  }
	}

      if (!used)
	goto incomplete_message;

      buffer += used;
      ok = buffer;
      n -= used;
    }

  incomplete_message:
  /* We have not gotten the full message yet, will try again next round: */

  return ok - start;
}

void fnet_tcp_read(struct fnet_ctrl_client *client, int tcp_sockfd,
		   tcp_handler_func_t handler, void *pinfo,
		   int show_progress)
{
  if (tcp_sockfd == -1)
    {
      tcp_sockfd = fnet_ctrl_open_tcp(client);

      if (tcp_sockfd < 0)
	{
	  fprintf (stderr, "Failed: %s\n",
		   fnet_ctrl_last_error(client));
	  exit(1);
	}

      fnet_ctrl_close(client);
    }

  /* Colourise the output, if we are printing to a tty. */
  fnet_fork_colouriser(STDOUT_FILENO, "fnet_tcp_colour.pl");

  /*
    {
    int window = 1000;
    setsockopt(tcp_sockfd, IPPROTO_TCP, TCP_WINDOW_CLAMP,
    &window, sizeof(window));
    }
  */

#define TCP_BUF_SIZE 0x20000

  {
    char buffer[TCP_BUF_SIZE];
    size_t fill = 0;
    uint64_t total = 0;
    uint64_t total_prev = 0;
    uint64_t handled = 0;
    struct timeval a, b;
    double infinite_response_avg = 0;
    /* For local-gen data verification: */
    uint32_t cur_mark = -1;

    gettimeofday(&a, NULL);

    for ( ; ; )
      {
	size_t todo = TCP_BUF_SIZE - fill;

	ssize_t n = read(tcp_sockfd, buffer+fill, todo);

	if (n == -1)
	  {
	    perror("read");
	    return;
	  }

	if (n == 0)
	  {
	    fprintf(stderr,"read got 0\n");
	    return;
	  }

	fill += n;
	total += n;

	if (handler)
	  {
	    size_t used, remain;

	    used = handler(buffer, fill, handled, pinfo, &cur_mark);

	    if (used > fill)
	      {
		fprintf (stderr,
			 "Internal error, tcp read handler used (%zd) "
			 "more than available (%zd).",
			 used, fill);
		exit(1);
	      }

	    handled += used;

	    remain = fill - used;

	    memmove(buffer, buffer + used, remain);

	    fill = remain;
	  }
	else
	  fill = 0;

	if (show_progress)
	{
	  struct timeval t_diff;
	  double elapsed;
	  double avg;

	  gettimeofday(&b, NULL);

	  timersub(&b, &a, &t_diff);

	  elapsed = t_diff.tv_sec + 1.e-6 * t_diff.tv_usec;

	  avg = ((double) (total - total_prev)) / elapsed;

	  if (elapsed > 1 || elapsed < 0)
	    {
	      double fact = exp(-0.25 * elapsed);

	      if (infinite_response_avg == 0.0)
		infinite_response_avg = avg;
	      else
		infinite_response_avg =
		  (1 - fact) * avg +
		  (    fact) * infinite_response_avg;

	      fprintf(stderr,
		      "read: %" PRId64 " (%.1f MB)  "
		      "%.1f kB/s [slow avg %.1f kB/s] mark:%02x\r",
		      total, ((double) total) * 1.e-6,
		      avg / 1000,
		      infinite_response_avg / 1000,
		      cur_mark);
	      fflush(stderr);

	      a = b;
	      total_prev = total;
	    }

	  /* After having done one TCP_BUF_SIZE of data,
	   * to emulate restricted bandwidth.
	   */
	  /* usleep(TCP_BUF_SIZE); */
	}

	/*
	  {
	  int one = 1;
	  setsockopt(tcp_sockfd, IPPROTO_TCP, TCP_QUICKACK,
	  &one, sizeof(one));
	  }
	*/
      }
  }
}
