
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

void fnet_ntpq_fire(struct fnet_ctrl_client *client, const char *cmd)
{
  fakernet_reg_acc_item *send;
  fakernet_reg_acc_item *recv;
  int num_send;
  int ret;

  uint32_t ntpq_ip     = 0xc0a801c0;
  uint32_t ntpq_mac_hi = 0x0000ee99;
  uint32_t ntpq_mac_lo = 0x0c772800;
  uint32_t ntpq_seq    = (((uint32_t) time(NULL)) << 16) | (rand() & 0xffff);

  const char *comma = strchr(cmd, ',');
  const char *mac;
  const char *ip;

  if (comma)
    {
      mac = cmd;
      ip = comma + 1;
    }
  else
    {
      mac = NULL;
      ip = cmd;
    }

  if (mac)
    {
      uint32_t a, b, c, d, e, f;
      uint64_t x;

      if (sscanf (mac,
		  "%" SCNx32 ":%" SCNx32 ":%" SCNx32 ":"
		  "%" SCNx32 ":%" SCNx32 ":%" SCNx32 "",
		  &a, &b, &c, &d, &e, &f) == 6)
	{
	  ntpq_mac_hi =
	    ((a & 0xff) <<  8) |
	    ((b & 0xff)      );
	  ntpq_mac_lo =
	    ((c & 0xff) << 24) |
	    ((d & 0xff) << 16) |
	    ((e & 0xff) <<  8) |
	    ((f & 0xff)      );
	}
      else if (sscanf (mac,
		       "%" SCNx64 "",
		       &x) == 1)
	{
	  ntpq_mac_hi = (uint32_t) (x >> 32);
	  ntpq_mac_lo = (uint32_t)  x;
	}
      else
	{
	  fprintf (stderr, "Could not parse MAC (%s).\n", mac);
	  exit(1);
	}
    }
  if (ip)
    {
      uint32_t a, b, c, d;
      uint64_t x;

      if (sscanf (ip,
		  "%" SCNd32 ".%" SCNd32 "."
		  "%" SCNd32 ".%" SCNd32 "",
		  &a, &b, &c, &d) == 4)
	{
	  ntpq_ip =
	    ((a & 0xff) << 24) |
	    ((b & 0xff) << 16) |
	    ((c & 0xff) <<  8) |
	    ((d & 0xff)      );
	}
      else if (sscanf (ip,
		       "%" SCNx64 "",
		       &x) == 1)
	{
	  ntpq_ip = (uint32_t) x;
	}
      else
	{
	  fprintf (stderr, "Could not parse IP (%s).\n", ip);
	  exit(1);
	}
    }

  fnet_ctrl_get_send_recv_bufs(client, &send, &recv);

  send[0].addr =
    htonl(FAKERNET_REG_ACCESS_ADDR_WRITE | 17);
  send[0].data = htonl(ntpq_ip);
  send[1].addr =
    htonl(FAKERNET_REG_ACCESS_ADDR_WRITE | 18);
  send[1].data = htonl(ntpq_seq);
  send[2].addr =
    htonl(FAKERNET_REG_ACCESS_ADDR_WRITE | 20);
  send[2].data = htonl(ntpq_mac_hi);
  send[3].addr =
    htonl(FAKERNET_REG_ACCESS_ADDR_WRITE | 21);
  send[3].data = htonl(ntpq_mac_lo);
  send[4].addr =
    htonl(FAKERNET_REG_ACCESS_ADDR_WRITE | 16);
  send[4].data = htonl(1); /* Value does not matter. */
  num_send = 5;

  ret = fnet_ctrl_send_recv_regacc(client, num_send);

  if (ret != 1)
    {
      fprintf (stderr, "Failed: %s\n",
	       fnet_ctrl_last_error(client));
      exit(1);
    }

  printf ("Requested NTP query to (%04x%08x) %08x.\n",
	  ntpq_mac_hi, ntpq_mac_lo, ntpq_ip);
}

typedef struct fnet_ntp_query_t
{
  uint32_t _ip;
  uint64_t _send_tm;
  uint64_t _send_ts;
} fnet_ntp_query;

typedef struct fnet_ntp_reply_t
{
  uint32_t _ip;
  uint64_t _recv_tm;
  uint64_t _recv_ts;
  uint64_t _remote_rx_ts;
  uint64_t _remote_tx_ts;
} fnet_ntp_reply;

typedef struct fnet_ntp_pingpong_t
{
  fnet_ntp_query _query;
  fnet_ntp_reply _reply;
  int            _first;
} fnet_ntp_pingpong;

#define NUM_NTP_QUERY_HIST  16

fnet_ntp_pingpong _fnet_ntp_query_hist[NUM_NTP_QUERY_HIST];
int               _fnet_ntp_query_i = 0;

void fnet_show_ntp_result(fnet_ntp_pingpong *pp_tx,
			  fnet_ntp_pingpong *pp_rx,
			  const char *postfix)
{
  int64_t tx_leg, rx_leg;

  int64_t sum_delay, offset;

  uint64_t mid_ts;

  tx_leg = pp_tx->_reply._remote_rx_ts - pp_tx->_query._send_ts;
  rx_leg = pp_rx->_reply._recv_ts      - pp_rx->_reply._remote_tx_ts;

  sum_delay = tx_leg + rx_leg;

  offset =
    ((int64_t) ((tx_leg - rx_leg))) / 2;

  mid_ts = pp_tx->_query._send_ts + (sum_delay / 2);

  printf ("NTP%s: %d.%d.%d.%d at: %9" PRIu32 ".%09" PRIu32 " (tx %7d rx %7d) dly %7d off %d\n",
	  postfix,
	  (pp_rx->_reply._ip >> 24) & 0xff,
	  (pp_rx->_reply._ip >> 16) & 0xff,
	  (pp_rx->_reply._ip >>  8) & 0xff,
	  (pp_rx->_reply._ip      ) & 0xff,
	  (uint32_t) (mid_ts >> 32),
	  (uint32_t) ((((uint32_t) mid_ts) * 1000000000ll) >> 32),
	  (uint32_t) ((tx_leg * 1000000000ll) >> 32),
	  (uint32_t) ((rx_leg * 1000000000ll) >> 32),
	  (uint32_t) ((sum_delay * 1000000000ll) >> 32),
	  (uint32_t) ((offset * 1000000000ll) >> 32));
}

void fnet_add_ntp_query(fnet_ntp_query *query)
{
  fnet_ntp_pingpong *pp_add;
  fnet_ntp_pingpong *pp_prev;
  int first = 0;

  pp_prev = &_fnet_ntp_query_hist[(  _fnet_ntp_query_i) % NUM_NTP_QUERY_HIST];
  pp_add  = &_fnet_ntp_query_hist[(++_fnet_ntp_query_i) % NUM_NTP_QUERY_HIST];  

  /* Before we add the new query, we evaluate the best of the previous
   * if either the IP number changed, or it is too long ago.
   * 0x01000000 = 1/256 s = ~4 ms.
   */
  /*
  printf ("TRY* %08x %08x, %016llx %016llx\n",
	  pp_prev->_query._ip, query->_ip,
	  pp_prev->_query._send_ts, query->_send_ts);
  */
  if (pp_prev->_query._ip != query->_ip ||
      pp_prev->_query._send_ts < query->_send_ts - 0x01000000)
    {
      fnet_ntp_pingpong *pp_tx = NULL;
      fnet_ntp_pingpong *pp_rx = NULL;
      int i;
      int64_t best_tx_leg = INT64_MAX, best_rx_leg = INT64_MAX;
      /* int64_t roundtrip; */
      uint64_t earliest_send;

      /*
      if (roundtrip < 0)
	roundtrip = 0;

      earliest_send = reply->_recv_ts - 3 * roundtrip;
      */
      earliest_send = pp_prev->_query._send_ts - 0x01000000;

      for (i = 0; i < NUM_NTP_QUERY_HIST; i++)
	{
	  fnet_ntp_pingpong *pp =
	    &_fnet_ntp_query_hist[i];

	  if (pp->_reply._ip == pp_prev->_query._ip &&
	      pp->_reply._ip == pp->_query._ip &&
	      pp->_query._send_ts >= earliest_send &&
	      pp->_query._send_ts <= pp_prev->_query._send_ts)
	    {
	      int64_t tx_leg, rx_leg;

	      tx_leg = pp->_reply._remote_rx_ts - pp->_query._send_ts;
	      rx_leg = pp->_reply._recv_ts      - pp->_reply._remote_tx_ts;

	      if (tx_leg < best_tx_leg)
		{
		  best_tx_leg = tx_leg;
		  pp_tx = pp;
		}
	      if (rx_leg < best_rx_leg)
		{
		  best_rx_leg = rx_leg;
		  pp_rx = pp;
		}
	    }
	}

      if (pp_tx && pp_rx)
	fnet_show_ntp_result(pp_tx, pp_rx, "-best");

      first = 1;
    }

  /* Add the new query. */

  pp_add->_query = *query;
  pp_add->_reply._ip = 0; /* No reply set for this query. */
  pp_add->_first = first;
}

void fnet_add_ntp_reply(fnet_ntp_reply *reply)
{
  int i;
  
  for (i = 0; i < NUM_NTP_QUERY_HIST; i++)
    {
      fnet_ntp_pingpong *pp = &_fnet_ntp_query_hist[i];

      if (reply->_ip      == pp->_query._ip &&
	  reply->_recv_tm == pp->_query._send_tm)
	{
	  pp->_reply = *reply;

	  fnet_show_ntp_result(pp, pp,
			       pp->_first ? "-first" : "-burst");
	}
    }
}

#define FNET_TCP_GPS_UART_STATE_NONE   0
#define FNET_TCP_GPS_UART_STATE_NMEA   1
#define FNET_TCP_GPS_UART_STATE_UBX    2

static uint32_t fnet_tcp_get_le32(uint8_t *p)
{
  return
    (((uint32_t) p[0])      ) |
    (((uint32_t) p[1]) <<  8) |
    (((uint32_t) p[2]) << 16) |
    (((uint32_t) p[3]) << 24);
}

static uint16_t fnet_tcp_get_le16(uint8_t *p)
{
  return
    (((uint32_t) p[0])      ) |
    (((uint32_t) p[1]) <<  8);
}

void fnet_tcp_gps_uart(fnet_tcp_gps_mon_msg *msg, uint32_t gpsno, char c)
{
  int emit_until = 0;
  const char *emit = "UNKNOWN";
  int ubx_class_id = 0;

  msg->_msg[msg->_len++] = c;

  switch (msg->_state)
    {
    case FNET_TCP_GPS_UART_STATE_NONE:
      /* If we find a '$', we go into NMEA state. */
      if (msg->_msg[msg->_len-1] == '$')
	{
	  msg->_state = FNET_TCP_GPS_UART_STATE_NMEA;
	  emit_until = msg->_len-1;
	  goto emit_text;
	}
      /* If we have six chars, and they start with UBX sync chars, we
       * go into UBX state.
       */
      if (msg->_len >= 6 &&
	  msg->_msg[msg->_len-6] == 0xb5 &&
	  msg->_msg[msg->_len-5] == 0x62)
	{
	  msg->_state = FNET_TCP_GPS_UART_STATE_UBX;
	  /* Note msg->_end will not be shifted by emit. */
	  msg->_end = ((((int) msg->_msg[msg->_len-1]) << 8) |
		       (((int) msg->_msg[msg->_len-2])     )) + 6 + 2;
	  emit_until = msg->_len-6;
	  goto emit_text;
	}
      if (msg->_len >= (int) sizeof (msg->_msg))
	{
	  /* Get rid of the characters so far. */
	  emit = "OVERLONG_UNIDENTIFIED";
	  emit_until = msg->_len / 2;
	  goto emit_binary;
	}
      return;
    case FNET_TCP_GPS_UART_STATE_NMEA:
      if (msg->_msg[msg->_len-1] == '$')
	{
	  /* Stay in NMEA state. */
	  emit = "INCOMPLETE_NMEA";
	  emit_until = msg->_len-1;
	  goto emit_text;
	}
      if (msg->_msg[msg->_len-1] == '\n')
	{
	  msg->_state = FNET_TCP_GPS_UART_STATE_NONE;
	  if (msg->_len >= 2 &&
	      msg->_msg[msg->_len-2] == '\r')
	    {
	      /* msg->_msg[msg->_len-2] = '\0'; */
	    }
	  /* We abuse the emit function to dump the text. */
	  emit = NULL;
	  emit_until = msg->_len;
	  goto emit_text;
	}
      if (msg->_len >= (int) sizeof (msg->_msg))
	{
	  /* Stay in NMEA state. */
	  emit = "OVERLONG_NMEA";
	  emit_until = msg->_len;
	  goto emit_text;
	}
      break;
    case FNET_TCP_GPS_UART_STATE_UBX:
      if (msg->_len >= msg->_end)
	{
	  msg->_state = FNET_TCP_GPS_UART_STATE_NONE;
	  emit = "UBX";
	  emit_until = msg->_len;
	  goto emit_binary_ubx;
	}
      if (msg->_len >= (int) sizeof (msg->_msg))
	{
	  /* Stay in NMEA state. */
	  emit = "OVERLONG_UBLOX";
	  emit_until = msg->_len;
	  goto emit_binary;
	}

      break;
    }
  return;

 emit_text:
  if (emit_until > 0)
    {
      int i;

      printf ("UART-%d: %s%s",
	      gpsno,
	      emit ? emit : "",
	      emit ? ": " : "");

      for (i = 0; i < emit_until; i++)
	{
	  if (isprint(msg->_msg[i]))
	    printf ("%c",msg->_msg[i]);
	  else if (msg->_msg[i] == '\r' || msg->_msg[i] == '\n')
	    ; /* just swallow */
	  else
	    printf ("\\%02x", msg->_msg[i]);
	}
    goto emit_done;
  }
  return;

 emit_binary_ubx:
  if      (msg->_msg[2] == 0x05 &&
	   msg->_msg[3] == 0x01 &&
	   emit_until == 6+2+2)
    {
      emit = "UBX-ACK-ACK";
      ubx_class_id = 0x0501;
    }
  else if (msg->_msg[2] == 0x05 &&
	   msg->_msg[3] == 0x00 &&
	   emit_until == 6+2+2)
    {
      emit = "UBX-ACK-NAK";
      ubx_class_id = 0x0500;
    }
  else if (msg->_msg[2] == 0x0d &&
	   msg->_msg[3] == 0x01 &&
	   emit_until == 6+16+2)
    {
      emit = "UBX-TIM-TP";
      ubx_class_id = 0x0d01;
    }
  else if (msg->_msg[2] == 0x0d &&
	   msg->_msg[3] == 0x04 &&
	   emit_until == 6+28+2)
    {
      emit = "UBX-TIM-SVIN";
      ubx_class_id = 0x0d04;
    }
  else if (msg->_msg[2] == 0x01 &&
	   msg->_msg[3] == 0x26 &&
	   emit_until == 6+24+2)
    {
      emit = "UBX-NAV-TIMELS";
      ubx_class_id = 0x0126;
    }

 emit_binary:
  printf ("UART-%d: %s%s",
	  gpsno,
	  emit ? emit : "",
	  emit ? ": " : "");

  if      (ubx_class_id == 0x0501 || ubx_class_id == 0x0500)
    {
      printf ("%02x %02x",
	      msg->_msg[6+ 0],
	      msg->_msg[6+ 1]);
    }
  else if (ubx_class_id == 0x0d01)
    {
      printf ("%08x %08x %08x %04x %02x %02x",
	      fnet_tcp_get_le32(msg->_msg+6+ 0),
	      fnet_tcp_get_le32(msg->_msg+6+ 4),
	      fnet_tcp_get_le32(msg->_msg+6+ 8),
	      fnet_tcp_get_le16(msg->_msg+6+ 12),
	      msg->_msg[6+ 14],
	      msg->_msg[6+ 15]);

      printf (" qerr: %d", (int32_t) fnet_tcp_get_le32(msg->_msg+6+ 8));
    }
  else if (ubx_class_id == 0x0d04)
    {
      printf ("%08x=%ds %08x %08x %08x %08x=%.1fm %08x=#%d %02x%s %02x%s %02x %02x",
	      fnet_tcp_get_le32(msg->_msg+6+ 0),  /* duration [s] */
	      fnet_tcp_get_le32(msg->_msg+6+ 0),
	      fnet_tcp_get_le32(msg->_msg+6+ 4),  /* meanX [cm] */
	      fnet_tcp_get_le32(msg->_msg+6+ 8),  /* meanY [cm] */
	      fnet_tcp_get_le32(msg->_msg+6+ 12), /* meanZ [cm] */
	      fnet_tcp_get_le32(msg->_msg+6+ 16), /* 3d variance [mm^2] */
	      sqrt(fnet_tcp_get_le32(msg->_msg+6+ 16)) / 1000.,
	      fnet_tcp_get_le32(msg->_msg+6+ 20), /* # observations */
	      fnet_tcp_get_le32(msg->_msg+6+ 20),
	      msg->_msg[6+ 24],                   /* valid */
	      msg->_msg[6+ 24] == 1 ? "=hold" : "",
	      msg->_msg[6+ 25],                   /* in progress */
	      msg->_msg[6+ 25] == 1 ? "=survey" : "",
	      msg->_msg[6+ 26],                   /* reserved */
	      msg->_msg[6+ 27]);                  /* reserved */
    }
  else if (ubx_class_id == 0x0126)
    {
      int32_t time_to_ls_change =
	fnet_tcp_get_le32(msg->_msg+6+12) /* time to ls change */;
      int32_t abs_time_to_ls_change = abs(time_to_ls_change);

      printf ("%08x %02x %02x %02x %02x %02x %02x %02x %02x "
	      "%08x=%c%dd%dh%dm%ds %04x %04x %02x %02x %02x %02x",
	      fnet_tcp_get_le32(msg->_msg+6+ 0), /* iTOW */
	      msg->_msg[6+ 4],                   /* message version */
	      msg->_msg[6+ 5],                   /* reserved */
	      msg->_msg[6+ 6],                   /* reserved */
	      msg->_msg[6+ 7],                   /* reserved */
	      msg->_msg[6+ 8],                   /* src curr ls*/
	      msg->_msg[6+ 9],                   /*     curr ls */
	      msg->_msg[6+10],                   /* src change ls */
	      msg->_msg[6+11],                   /*     change ls */
	      fnet_tcp_get_le32(msg->_msg+6+12), /* time to ls change */
	      time_to_ls_change < 0 ? '-' : '+',
	      abs_time_to_ls_change / (24 * 60 * 60),
	      abs_time_to_ls_change / (     60 * 60) % 24,
	      abs_time_to_ls_change / (          60) % 60,
	      abs_time_to_ls_change                  % 60,
	      fnet_tcp_get_le16(msg->_msg+6+16), /* date of LS ch. week */
	      fnet_tcp_get_le16(msg->_msg+6+18), /* date of LS ch. day */
	      msg->_msg[6+ 20],                  /* reserved */
	      msg->_msg[6+ 21],                  /* reserved */
	      msg->_msg[6+ 22],                  /* reserved */
	      msg->_msg[6+ 23]                   /* valid */
	      );
    }
  else
    {
      int i;

      for (i = 0; i < emit_until; i++)
	printf ("\\%02x", (uint8_t) msg->_msg[i]);
    }
  goto emit_done;
  return;

 emit_done:
  printf ("\n");
  fflush(stdout);

  memmove(msg->_msg, msg->_msg+emit_until, msg->_len-emit_until);
  msg->_len -= emit_until;
}

size_t fnet_tcp_gps_mon(char *buffer, size_t n, uint64_t handled,
			fnet_tcp_gps_mon_info *info, uint32_t *pcur)
{
  uint32_t *p = (uint32_t *) buffer;
  uint32_t *ok = p;

  (void) handled;
  (void) pcur;

  if (n >= sizeof (uint32_t))
    {
      uint32_t word  = htonl(*p);
      uint32_t gpsno = (word >> 20) & 0x0f;
      uint32_t next_seconds = 0;

      if (gpsno == 0)
	{
	  switch ((word >> 16) & 0x0f)
	    {
	    case 1: /* Six words: NTP query report. */
	      if (n < 6*sizeof (uint32_t))
		goto incomplete_message;

	      {
		uint32_t send_ip;
		uint32_t send_tm_hi, send_tm_lo;
		uint32_t send_ts_hi, send_ts_lo;
		fnet_ntp_query query;

		p++; n -= sizeof (uint32_t); send_ip    = htonl(*p);
		p++; n -= sizeof (uint32_t); send_tm_hi = htonl(*p);
		p++; n -= sizeof (uint32_t); send_tm_lo = htonl(*p);
		p++; n -= sizeof (uint32_t); send_ts_hi = htonl(*p);
		p++; n -= sizeof (uint32_t); send_ts_lo = htonl(*p);

		printf ("NTP-send: %d.%d.%d.%d (%08x) %08x:%08x at: "
			"%9" PRIu32 ".%09" PRIu32 "\n",
			(send_ip >> 24) & 0xff,
			(send_ip >> 16) & 0xff,
			(send_ip >>  8) & 0xff,
			(send_ip      ) & 0xff,
			send_ip,
			send_tm_hi, send_tm_lo,
			send_ts_hi,
			(uint32_t) ((send_ts_lo * 1000000000ll) >> 32));

		query._ip = send_ip;
		query._send_ts = (((uint64_t) send_ts_hi) << 32) | send_ts_lo;
		query._send_tm = (((uint64_t) send_tm_hi) << 32) | send_tm_lo;

		fnet_add_ntp_query(&query);

		fflush(stdout);
	      }
	      break;

	    case 2: /* Six words: NTP query report. */
	      if (n < 16*sizeof (uint32_t))
		goto incomplete_message;

	      {
		uint32_t recv_ip;
		uint32_t recv_ts_hi, recv_ts_lo;
		uint32_t recv_data[12];
		fnet_ntp_reply reply;
		int i;

		p++; n -= sizeof (uint32_t); recv_ip     = htonl(*p);
		p++; n -= sizeof (uint32_t); recv_ts_hi  = htonl(*p);
		p++; n -= sizeof (uint32_t); recv_ts_lo  = htonl(*p);
		for (i = 0; i < 12; i++) {
		  p++; n -= sizeof (uint32_t); recv_data[i]  = htonl(*p);
		}

		printf ("NTP-recv: %d.%d.%d.%d (%08x) %08x:%08x at: "
			"%9" PRIu32 ".%09" PRIu32 "    "
			"l: %d v: %d m: %d st: %3d p: %3d pr: %3d "
			"dly: %08x disp: %08x ID: %08x  "
			"ref: %9" PRIu32 ".%09" PRIu32 " "
			"rx: %9" PRIu32 ".%09" PRIu32 " "
			"tx: %9" PRIu32 ".%09" PRIu32 "\n",
			(recv_ip >> 24) & 0xff,
			(recv_ip >> 16) & 0xff,
			(recv_ip >>  8) & 0xff,
			(recv_ip      ) & 0xff,
			recv_ip,
			recv_data[ 6], recv_data[ 7],  /* Orig TS = tm above.*/
			recv_ts_hi,
			(uint32_t) ((recv_ts_lo * 1000000000ll) >> 32),
			(recv_data[ 0] >> 30) & 0x3,
			(recv_data[ 0] >> 27) & 0x7,
			(recv_data[ 0] >> 24) & 0x7,
 			(recv_data[ 0] >> 16) & 0xff,
			(recv_data[ 0] >>  8) & 0xff,
			(int8_t) ((recv_data[ 0]) & 0xff),
			recv_data[ 1], /* Root delay. */
			recv_data[ 2], /* Root disp. */
			recv_data[ 3], /* Reference ID. */
			recv_data[ 4], /* Reference ts. */
			(uint32_t) ((recv_data[ 5] * 1000000000ll) >> 32),
			/* printed above */
			recv_data[ 8], /* Remote RX ts. */
			(uint32_t) ((recv_data[ 9] * 1000000000ll) >> 32),
			recv_data[10], /* Remote TX ts. */
			(uint32_t) ((recv_data[11] * 1000000000ll) >> 32));

		if (((recv_data[ 0] >> 30) & 0x3) != 3) /* leap 3 = no sync */
		  {
		    reply._ip = recv_ip;
		    reply._recv_ts =
		      (((uint64_t) recv_ts_hi) << 32) | recv_ts_lo;
		    reply._recv_tm =
		      (((uint64_t) recv_data[ 6]) << 32) | recv_data[ 7];
		    reply._remote_rx_ts =
		      (((uint64_t) recv_data[ 8]) << 32) | recv_data[ 9];
		    reply._remote_tx_ts =
		      (((uint64_t) recv_data[10]) << 32) | recv_data[11];

		    fnet_add_ntp_reply(&reply);
		  }

		fflush(stdout);
	      }
	      break;

	    default:
	      fprintf (stderr, "Unexpected NTP monitor word %08x.\n", word);
	      exit(1);
	    }
	}
      else
	{
	  switch ((word >> 16) & 0x0f)
	    {
	    case 1: /* One byte from the UART. */
	      {
		char c = word & 0xff;
		fnet_tcp_gps_mon_msg *msg = &(info->_uart_msg[gpsno]);

		/* Could use the byte count to detect missing bytes?
		 * Though: that would only be due to use reading the
		 * TCP stream too slowly...
		 */

		fnet_tcp_gps_uart(msg, gpsno, c);

		/*
		  printf ("UART-%d: [%2d] %02x\n",
		          gpsno, (msg >> 8) & 0xf, msg & 0xff);
		*/
		break;
	      }

	    case 2: /* Two words: PPS pulse time (32 bits). */
	      {
		uint32_t pps_cnt;
		uint32_t clkcnt, clksincelast;
		uint32_t subclkfrac256;
		uint32_t word_qperiod;
		int16_t  track_quant_err;
		uint16_t period_ps;
		double   subclk, subclksincelast;

		if (n < 3*sizeof (uint32_t))
		  goto incomplete_message;

		pps_cnt = word & 0x0f;
		subclkfrac256 = (word >> 8) & 0xff;
		p++; n -= sizeof (uint32_t); clkcnt = htonl(*p);
		p++; n -= sizeof (uint32_t); word_qperiod = htonl(*p);

		track_quant_err   =
		  (int16_t) ((word_qperiod >> 16) & 0xffff);
		period_ps = (uint16_t) word_qperiod;

		subclk = (-(double) subclkfrac256) / 256 * period_ps * 0.001 +
		  track_quant_err * 0.001;

		clksincelast = clkcnt - info->_clkcnt[gpsno];
		subclksincelast = subclk - info->_subclk[gpsno];

		if (abs((int32_t) (clkcnt - info->_lastdiffclk)) > 1000)
		  {
		    int i;

		    printf ("PPS-diff:");
		    if (info->_hasdiffclk & (1 << 1))
		      for (i = 2; i < 16; i++)
			{
			  if (info->_hasdiffclk & (1 << i))
			    {
			      double diff =
				((double) (int32_t) (info->_clkcnt[i] -
						     info->_clkcnt[1])) *
				period_ps * 0.001 +
				(info->_subclk[i] - info->_subclk[1]);

			      printf (" %6.2f", diff);
			    }
			  else if (info->_hasdiffclk > (uint32_t) (1 << i))
			    {
			      printf (" nan");
			    }
			}
		    printf ("\n");

		    info->_hasdiffclk = 0;
		  }

		info->_clkcnt[gpsno] = clkcnt;
		info->_subclk[gpsno] = subclk;

		info->_lastdiffclk = clkcnt;
		info->_hasdiffclk |= (1 << gpsno);

		printf ("PPS-%d: %" PRIu32 " %+6.2f "
			"#%-2d %3d %6d %5d (%" PRId32 " %10.2f)\n",
			gpsno, clkcnt, subclk,
			pps_cnt,
			subclkfrac256, track_quant_err, period_ps,
			clksincelast,
			((double) clksincelast) *
			period_ps * 0.001 + (subclksincelast));
		fflush(stdout);
		break;
	      }

	    case 3: /* Two words: seconds next PPS (32 bits). */
	      if (n < 2*sizeof (uint32_t))
		goto incomplete_message;

	      p++;
	      n -= sizeof (uint32_t);
	      next_seconds = htonl(*p);

	      printf ("NEXT-%d: %" PRIu32 "\n", gpsno, next_seconds);
	      fflush(stdout);
	      break;

	    case 4: /* Eight words: time-track information. */
	      if (n < 8*sizeof (uint32_t))
		goto incomplete_message;

	      {
		uint32_t frac_add, track_diff, ravg_abs_diff;
		uint32_t cur_ts_hi, cur_ts_lo;
		int8_t precision;
		uint32_t track_cnt;
		uint32_t synced;
		uint32_t track_corr_full;
		uint32_t word_qpps;
		int16_t track_quant_err;
		uint32_t track_pps_subtime;

		precision = (int8_t) ((word >> 8) & 0xff);
		track_cnt = ((word >> 4) & 0xf);
		synced    = ((word     ) & 0x1);
		p++; n -= sizeof (uint32_t); cur_ts_hi  = htonl(*p);
		p++; n -= sizeof (uint32_t); cur_ts_lo  = htonl(*p);
		p++; n -= sizeof (uint32_t); frac_add   = htonl(*p);
		p++; n -= sizeof (uint32_t); track_diff = htonl(*p);
		p++; n -= sizeof (uint32_t); ravg_abs_diff = htonl(*p);
		p++; n -= sizeof (uint32_t); track_corr_full = htonl(*p);
		p++; n -= sizeof (uint32_t); word_qpps  = htonl(*p);

		track_pps_subtime = (word_qpps     ) &   0xff;
		track_quant_err   =
		  (((int16_t) ((word_qpps >> 8) & 0xffff)) << 21) >> 21;

		printf ("TRACK-%d: "
			"%9" PRIu32 ".%09" PRIu32 " (:%09" PRIu32 ") "
			"a:%9" PRIu32 " c:%6" PRId32 " "
			"s:%2" PRIu32 " q:%6" PRId32 " "
			"d: %4" PRId32 " ra: %4" PRId32 " "
			"#%-2d %c p:%3d\n",
			gpsno,
			cur_ts_hi,
			(uint32_t) ((cur_ts_lo * 1000000000ll) >> 32),
			cur_ts_lo,
			frac_add, track_corr_full,
			track_pps_subtime, track_quant_err,
			track_diff, ravg_abs_diff,
			track_cnt, synced ? 'S' : 'u', precision);
		fflush(stdout);
	      }
	      break;

	    default:
	      fprintf (stderr, "Unexpected GPS monitor word %08x.\n", word);
	      exit(1);
	    }
	}

      p++;
      ok = p;
      n -= sizeof (uint32_t);
    }

 incomplete_message:
  /* We have not gotten the full message yet, will try again next round: */

  return ((char *) ok) - buffer;
}
