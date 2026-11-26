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

#ifndef __FNETCTRL_H__
#define __FNETCTRL_H__

#include "fnet_client.h"

#include <stdint.h>

/*************************************************************************/

void fnet_udp_flood(struct fnet_ctrl_client *client,
		    int items_per_packet,
		    uint32_t reg_addr);

void fnet_btn2led(struct fnet_ctrl_client *client);

void fnet_set_test_reg(struct fnet_ctrl_client *client,
		       uint32_t testreg_addr,
		       uint32_t testreg_value);

void fnet_reg_rw(struct fnet_ctrl_client *client,
		 uint32_t reg_addr,
		 uint32_t reg_value,
		 int write);

/*************************************************************************/

void fnet_ctrl_stat_print_conn_status(struct fnet_ctrl_client *client,
				      uint32_t compile_time);

#define FLAG_WORDS_DIV_64   0x01
#define FLAG_IS_SLOW_TICKS  0x02
#define FLAG_IS_DIFFABLE    0x04
#define FLAG_IS_IP          0x08

typedef struct stat_index_name_t
{
  int          index;
  int          flags;
  const char  *name;
} stat_index_name;

#define STAT_PRINT_IDX      0x01
#define STAT_PRINT_HEX      0x02

void fnet_ctrl_stat_print_diff(const stat_index_name *idx_names,
			       const uint32_t *vals,
			       const uint32_t *prev_vals,
			       int flags_all,
			       int print_flags, int name_chars,
			       double slow_tick_time);

void fnet_ctrl_stat_print_descr(struct fnet_ctrl_client *client,
				uint32_t *ver);

void fnet_ctrl_stat_print_conn_status(struct fnet_ctrl_client *client,
				      uint32_t compile_time);

void fnet_ctrl_stat(struct fnet_ctrl_client *client);

/*************************************************************************/

void fnet_mdio_action(struct fnet_ctrl_client *client,
		      int phy_addr, const char *cmd);

void fnet_mdio_do_write(struct fnet_ctrl_client *client,
			int phy_addr, int reg, int value);

void fnet_mdio_mmd_do_write(struct fnet_ctrl_client *client,
			    int phy_addr, int devaddr, int reg, int value);

void fnet_mdio_dump(struct fnet_ctrl_client *client,
		    int phy_addr);

/*************************************************************************/

void fnet_spi_read(struct fnet_ctrl_client *client);

/*************************************************************************/

void fnet_ctrl_readout_stat(struct fnet_ctrl_client *client);

/*************************************************************************/

void fnet_tcp_reset(struct fnet_ctrl_client *client);

size_t fnet_tcp_dump(char *buffer, size_t n, uint64_t handled,
		     void *pinfo, uint32_t *pcur);

size_t fnet_tcp_hexdump(char *buffer, size_t n, uint64_t handled,
			void *pinfo, uint32_t *pcur);

/**/

typedef struct fnet_tcp_local_verify_info_t
{
  uint32_t word_count;
  uint64_t total_checked;

  uint32_t last_seq;
  uint32_t last_words;
  uint32_t last_header;

} fnet_tcp_local_verify_info;

void fnet_tcp_local_verify_info_init(fnet_tcp_local_verify_info *info);

size_t fnet_tcp_local_verify(char *buffer, size_t n, uint64_t handled,
			     void *pinfo, uint32_t *pcur);

/**/

typedef struct fnet_tcp_gps_mon_msg_t
{
  uint8_t _msg[128];
  int     _len, _end;
  int     _state;

} fnet_tcp_gps_mon_msg;

typedef struct fnet_tcp_gps_mon_info_t
{
  fnet_tcp_gps_mon_msg _uart_msg[16];
  uint32_t             _clkcnt[16];
  double               _subclk[16];

  uint32_t             _lastdiffclk;
  uint32_t             _hasdiffclk;

} fnet_tcp_gps_mon_info;

typedef struct fnet_tcp_xadc_mon_info_t
{
  uint64_t  _set;
  uint16_t  _raw[64];

  uint32_t _last_ch;
  uint32_t _prev_max_ch;
} fnet_tcp_xadc_mon_info;

typedef struct fnet_tcp_lcl_func_mon_info_t
{
  fnet_tcp_gps_mon_info _gps;

  fnet_tcp_xadc_mon_info _xadc;

} fnet_tcp_lcl_func_mon_info;

size_t fnet_tcp_lcl_func_mon(char *buffer, size_t n, uint64_t handled,
			     void *pinfo, uint32_t *pcur);

/**/

void fnet_ntpq_fire(struct fnet_ctrl_client *client, const char *cmd);

typedef size_t (*tcp_handler_func_t)(char *buffer, size_t n, uint64_t handled,
				     void *pinfo, uint32_t *pcur);

size_t fnet_tcp_gps_mon(char *buffer, size_t n, uint64_t handled,
			fnet_tcp_gps_mon_info *info, uint32_t *pcur);

size_t fnet_tcp_sampler_mon(char *buffer, size_t n, uint64_t handled,
			    void *pinfo, uint32_t *pcur);

void fnet_tcp_read(struct fnet_ctrl_client *client, int tcp_sockfd,
		   tcp_handler_func_t handler, void *pinfo,
		   int show_progress);

/*************************************************************************/

void fnet_fork_colouriser(int fd, const char *colouriser);

/*************************************************************************/

#endif/*__FNET_CLIENT_H__*/
