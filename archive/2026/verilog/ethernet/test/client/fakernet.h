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

#ifndef __FAKERNET_H__
#define __FAKERNET_H__

#include <stdint.h>

/***********************************************************************/

#define FAKERNET_DEFAULT_CTRL_PORT_BASE  15

/* UDP register access packet, send to port 51208+i, where i is the
 * client channel number.
 */

#define FAKERNET_REG_ACCESS_ADDR_WRITE      0x80000000 /* Write request. */
#define FAKERNET_REG_ACCESS_ADDR_WRITTEN    0x40000000 /* Write response. */
#define FAKERNET_REG_ACCESS_ADDR_READ       0x20000000 /* Read request. */
#define FAKERNET_REG_ACCESS_ADDR_READ_RET   0x10000000 /* Read response. */

/* To access the internal registers. */
#define FAKERNET_REG_ACCESS_ADDR_INTERNAL             0x08000000

/* The internal version (compile time) registers. */
#define FAKERNET_REG_ACCESS_ADDR_VERSION              0x08000010  /* 0x0f */

/* The internal status registers. */
#define FAKERNET_REG_ACCESS_ADDR_INT_STAT             0x08000020  /* 0x1f */

/* To access the MDIO interface. */
#define FAKERNET_REG_ACCESS_ADDR_MDIO                 0x08000100  /* 0x01 */

/* The internal event counters. */
#define FAKERNET_REG_ACCESS_ADDR_INT_COUNT            0x08000200  /* 0x1ff */

/* The internal test control registers. */
#define FAKERNET_REG_ACCESS_ADDR_INT_TEST_CTRL_REG    0x08000400  /* 0x7 */

/* The internal test control pulse registers. */
#define FAKERNET_REG_ACCESS_ADDR_INT_TEST_CTRL_PULSE  0x08000500  /* 0x7 */

/* To reset the ongoing TCP connection. */
#define FAKERNET_REG_ACCESS_ADDR_RESET_TCP            0x08000800  /* 0x1 */

typedef struct fakernet_reg_acc_item_t
{
  /* Register address.  High bits contain operation to be performed. */
  uint32_t addr;
  /* Data field.  Empty for reads, will be filled on return. */
  uint32_t data;

} fakernet_reg_acc_item;

#define FAKERNET_SEQ_REQ_RESET_ARM            0x8000
#define FAKERNET_SEQ_REQ_RESET                0x4000

#define FAKERNET_SEQ_REQ_DISCONNECT           0x2000
#define FAKERNET_SEQ_REQ_CONNECTED            0x1000

#define FAKERNET_SEQ_REQ_ARM_USER_CODE_MASK   0x0f00
#define FAKERNET_SEQ_REQ_ARM_USER_CODE_SHIFT  8

#define FAKERNET_SEQ_SEQUENCE_MASK            0x00ff
#define FAKERNET_SEQ_SEQUENCE_MASK_1ST_XOR    0x00f0

/* Cannot be a power of 2 to nicely keep the total packet size below
 * the next power of 2.
 */
#define FAKERNET_REG_ACCESS_MAX_ITEMS 116

/* */

#define FAKERBET_STATUS1_UDP_CONNECTED(i)     (0x0001 << i)
#define FAKERBET_STATUS1_UDP_ACTIVE(i)        (0x0100 << i)

#define FAKERNET_STATUS2_TCP_CONN_MASK        0x0007
#define FAKERNET_STATUS2_TCP_IDLE             0x0001
#define FAKERNET_STATUS2_TCP_GOT_SYN          0x0002
#define FAKERNET_STATUS2_TCP_CONNECTED        0x0003

#define FAKERNET_STATUS2_TCP_COMMIT_OVERFLOW  0x0010 /* FPGA user code bug. */
#define FAKERNET_STATUS2_TCP_WRITE_OVERFLOW   0x0020 /* FPGA user code bug. */
#define FAKERNET_STATUS2_TCP_RAM_BITFLIP      0x0040 /* FPGA malfunction. */
#define FAKERNET_STATUS2_ANY_RAM_BITFLIP      0x0080 /* FPGA malfunction. */

typedef struct fakernet_reg_access_t
{
  uint16_t status_udp_channels; /* Low octet: in use. High: recent in use. */
  uint16_t status_tcp;

  /* Sequence number, must increment by one for each packet.
   * The accesses are only performed once, so repeating a packet
   * is harmless.
   *
   * When an invalid number is given, the last response (with correct
   * sequence number) is returned.  (For the time being with a zeroed
   * UDP checksum.)
   */
  uint16_t sequence_request;
  uint16_t sequence_response;

  /* The requested access items (remainder of packet). */
  fakernet_reg_acc_item items[0 /* At most FAKERNET_REG_ACCESS_MAX_ITEMS. */ ];

} fakernet_reg_access;

/* The MDIO interface will initiate another access when written to.
 * The 'op' bits (29..28) tells if it is a read or a write.
 * Reading the MDIO register will report the current access, where
 * the 'ta' bits (17..16) are used to tell if the access is done (17)
 * and if it was successful (16 is 1).
 *
 * If a new access is requested while the previous is being performed
 * it will be ignored (and the MDIO write will fail).
 */

#define FAKERNET_MDIO_START       0x40000000  /* 31..30: 01        */
#define FAKERNET_MDIO_START_MASK  0xc0000000  /* 31..30: 11        */
#define FAKERNET_MDIO_OP_WRITE    0x10000000  /* 29..28:   01      */
#define FAKERNET_MDIO_OP_READ     0x20000000  /* 29..28:   10      */
#define FAKERNET_MDIO_OP_MASK     0x30000000  /* 29..28:   11      */
#define FAKERNET_MDIO_ADDR_MASK   0x0f800000  /* 27..23:     a     */
#define FAKERNET_MDIO_REG_MASK    0x007c0000  /* 22..18:      r    */
#define FAKERNET_MDIO_TA_WRITE    0x00020000  /* 17..16:       10  */
#define FAKERNET_MDIO_TA_READ     0x00000000  /* 17..16:       00  */
#define FAKERNET_MDIO_TA_MASK     0x00030000  /* 17..16:       11  */
#define FAKERNET_MDIO_DATA_MASK   0x0000ffff  /* 15.. 0:         d */

#define FAKERNET_MDIO_PHY_ADDR_SHIFT      23
#define FAKERNET_MDIO_REG_ADDR_SHIFT      18
#define FAKERNET_MDIO_DATA_SHIFT           0

#define FAKERNET_MDIO_ST_OP_ADDR_TA_MASK  0xfffc0000

#define FAKERNET_MDIO_DONE        0x00020000
#define FAKERNET_MDIO_SUCCESS     0x00010000

/***********************************************************************/

#endif/*__FAKERNET_H__*/
