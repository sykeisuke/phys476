
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

/*************************************************************************/

#define MDIO_REG_BMCR     0
#define MDIO_REG_BMSR     1
#define MDIO_REG_PHYIDR1  2
#define MDIO_REG_PHYIDR2  3
#define MDIO_REG_ANAR     4
#define MDIO_REG_ANLPAR   5
#define MDIO_REG_GBCR     9
#define MDIO_REG_GBSR    10
#define MDIO_REG_MACR    13
#define MDIO_REG_MAADR   14
#define MDIO_REG_GBESR   15

/* Reg 0 */
#define MDIO_BIT_BMCR_SPEED_100                 0x2000 /* 13 */
#define MDIO_BIT_BMCR_AUTONEG_ENABLE            0x1000 /* 12 */
#define MDIO_BIT_BMCR_AUTONEG_RESTART           0x0200 /*  9 */
#define MDIO_BIT_BMCR_DUPLEX                    0x0100 /*  8 */

/* Reg 1 */
#define MDIO_BIT_BMSR_ABILITY_100baseTX_FULL    0x4000 /* 14 */
#define MDIO_BIT_BMSR_ABILITY_100baseTX_HALF    0x2000 /* 13 */
#define MDIO_BIT_BMSR_ABILITY_10baseT_FULL      0x1000 /* 12 */
#define MDIO_BIT_BMSR_ABILITY_10baseT_HALF      0x0800 /* 11 */
#define MDIO_BIT_BMSR_DEV_SUPP_GBESR            0x0100 /*  8 */

/* Reg 2 */
#define MDIO_BITS_PHYIDR1_OUI_MSB               0xffff
#define MDIO_SHFT_PHYIDR1_OUI_MSB                    0
/* Reg 3 */
#define MDIO_BITS_PHYIDR2_OUI_LSB               0xfc00
#define MDIO_SHFT_PHYIDR2_OUI_LSB                   10
#define MDIO_BITS_PHYIDR2_VENDOR_MODEL          0x03f0
#define MDIO_SHFT_PHYIDR2_VENDOR_MODEL               4
#define MDIO_BITS_PHYIDR2_MODEL_REV             0x000f
#define MDIO_SHFT_PHYIDR2_MODEL_REV                  0

/* Reg 4, 5 */
#define MDIO_BIT_ANAR_ADVERT_100baseTX_FULL     0x0100 /*  8 */
#define MDIO_BIT_ANAR_ADVERT_100baseTX_HALF     0x0080 /*  7 */
#define MDIO_BIT_ANAR_ADVERT_10baseT_FULL       0x0040 /*  6 */
#define MDIO_BIT_ANAR_ADVERT_10baseT_HALF       0x0020 /*  5 */

/* Reg 9 */
#define MDIO_BIT_GBCR_ADVERT_1000baseT_FULL    0x0200 /*  9 */

/* Reg 10 */
#define MDIO_BIT_GBSR_LP_ADVERT_1000baseT_FULL 0x0800 /* 11 */
#define MDIO_BIT_GBSR_LP_ADVERT_1000baseT_HALF 0x0400 /* 10 */

/* Reg 13 */
#define MDIO_BIT_MACR_FUNC_ADDR                0x0000 /* 15:14 */
#define MDIO_BIT_MACR_FUNC_DATA_NO_INCR        0x4000 /* 15:14 */
#define MDIO_BIT_MACR_FUNC_DEVADDR_MASK        0x001f /* 4:0 */
#define MDIO_BIT_MACR_FUNC_DEVADDR_SHIFT       0      /* 4:0 */

/* Reg 15 */
#define MDIO_BIT_GBESR_ABILITY_1000baseT_FULL  0x2000 /* 13 */
#define MDIO_BIT_GBESR_ABILITY_1000baseT_HALF  0x1000 /* 12 */

#define MDIO_PHYIDR1_RTL8211E                  0x001c
#define MDIO_PHYIDR2_RTL8211E                  0xc915

/* RTL8211E has PHYSR (PHY specific status register 0x11=17) which
 * gives speed in bits 15:14: 00=10 Mbps, 01=100 Mbps, 10=1000 Mbps,
 * 11=reserved.  Bit 11: speed and duplex resolved.
 */

/* Internal flags. */
#define FNET_MDIO_MODE_10baseT_HALF         0x00000001
#define FNET_MDIO_MODE_10baseT_FULL         0x00000002
#define FNET_MDIO_MODE_100baseTX_HALF       0x00000004
#define FNET_MDIO_MODE_100baseTX_FULL       0x00000008
#define FNET_MDIO_MODE_1000baseT_HALF       0x00000010
#define FNET_MDIO_MODE_1000baseT_FULL       0x00000020

/*************************************************************************/

uint32_t fnet_mdio_perform(struct fnet_ctrl_client *client,
			   uint32_t mdio_cmd)
{
  fakernet_reg_acc_item *send;
  fakernet_reg_acc_item *recv;
  int num_send;
  int ret;
  uint32_t mdio_result;

  fnet_ctrl_get_send_recv_bufs(client, &send, &recv);

  send[0].addr =
    htonl(FAKERNET_REG_ACCESS_ADDR_WRITE |
	  FAKERNET_REG_ACCESS_ADDR_MDIO);
  send[0].data = htonl(mdio_cmd);

  num_send = 1;

  ret = fnet_ctrl_send_recv_regacc(client, num_send);

  if (ret != 1)
    {
      fprintf (stderr, "Failed MDIO request: %s\n",
	       fnet_ctrl_last_error(client));
      exit(1);
    }

  usleep(100);

  send[0].addr =
    htonl(FAKERNET_REG_ACCESS_ADDR_READ |
	  FAKERNET_REG_ACCESS_ADDR_MDIO);
  send[0].data = htonl(0);

  num_send = 1;

  ret = fnet_ctrl_send_recv_regacc(client, num_send);

  if (ret != 1)
    {
      fprintf (stderr, "Failed MDIO response: %s\n",
	       fnet_ctrl_last_error(client));
      exit(1);
    }

  mdio_result = ntohl(recv[0].data);

  if (!(mdio_result & FAKERNET_MDIO_DONE))
    {
      fprintf (stderr, "MDIO access reported not done: %08x\n",
	       mdio_result);
      exit(1);
    }

  if (!(mdio_result & FAKERNET_MDIO_SUCCESS))
    {
      fprintf (stderr, "MDIO access reported not success: %08x\n",
	       mdio_result);
      exit(1);
    }

  return mdio_result;
}

typedef struct fnet_mdio_mmd_devaddr_reg_t
{
  int devaddr;
  int reg;
} fnet_mdio_mmd_devaddr_reg;

void fnet_mdio_print_result(uint32_t mdio_result,
			    uint32_t write_data,
			    fnet_mdio_mmd_devaddr_reg *mmd_addr)
{
  int j;

  const char *names[] = { "BMCR", "BMSR", "PHYIDR1", "PHYIDR2",
			  "ANAR", "ANALPAR", "", "",
			  };

  uint32_t op    =  mdio_result & FAKERNET_MDIO_OP_MASK;
  uint32_t addr  = (mdio_result & FAKERNET_MDIO_ADDR_MASK) >>
    FAKERNET_MDIO_PHY_ADDR_SHIFT;
  uint32_t reg   = (mdio_result & FAKERNET_MDIO_REG_MASK)
    >> FAKERNET_MDIO_REG_ADDR_SHIFT;
  uint32_t data  =  mdio_result & FAKERNET_MDIO_DATA_MASK;

  if (op == FAKERNET_MDIO_OP_WRITE)
    data = write_data;

  printf ("%08x %s ",
	  mdio_result,
	  op == FAKERNET_MDIO_OP_WRITE ? "wr" :
	  op == FAKERNET_MDIO_OP_READ  ? "rd" : "??");

  if (reg == MDIO_REG_MAADR)
    printf ("%2d:%2d:%2d       ",
	    addr,
	    mmd_addr->devaddr,
	    mmd_addr->reg);
  else
    printf ("%2d:%2d %-8s ",
	    addr,
	    reg,
	    ((size_t) reg) < sizeof(names)/sizeof(names[0]) ? names[reg] : "");

  printf ("0x%04x = ",
	  data);

  for (j = 0; j < 16; j++)
    printf ("%c %s", (data >> (15 - j)) & 1 ? '1' : '0',
	    (j % 4 == 3) ? " " : "");
  printf ("\n");
}

uint32_t fnet_mdio_read(struct fnet_ctrl_client *client,
			int phy_addr, int reg)
{
  uint32_t mdio_read_cmd;
  uint32_t mdio_result;

  phy_addr &= 0x1f;
  reg      &= 0x1f;

  mdio_read_cmd =
    FAKERNET_MDIO_START |
    FAKERNET_MDIO_OP_READ |
    FAKERNET_MDIO_TA_READ |
    ((phy_addr) << FAKERNET_MDIO_PHY_ADDR_SHIFT) |
    ((reg)      << FAKERNET_MDIO_REG_ADDR_SHIFT);

  mdio_result =
    fnet_mdio_perform(client, mdio_read_cmd);

  return mdio_result;
}

uint32_t fnet_mdio_write(struct fnet_ctrl_client *client,
			 int phy_addr, int reg, int value)
{
  uint32_t mdio_write_cmd;
  uint32_t mdio_result;

  phy_addr &= 0x1f;
  reg      &= 0x1f;
  value    &= 0xffff;

  mdio_write_cmd =
    FAKERNET_MDIO_START |
    FAKERNET_MDIO_OP_WRITE |
    FAKERNET_MDIO_TA_WRITE |
    ((phy_addr) << FAKERNET_MDIO_PHY_ADDR_SHIFT) |
    ((reg)      << FAKERNET_MDIO_REG_ADDR_SHIFT) |
    ((value)    << FAKERNET_MDIO_DATA_SHIFT);

  mdio_result =
    fnet_mdio_perform(client, mdio_write_cmd);

  return mdio_result;
}

uint32_t fnet_mdio_mmd_read_write(struct fnet_ctrl_client *client,
				  int phy_addr,
				  int dev_addr, int reg,
				  int write, int wr_value)
{
  uint32_t mdio_mmd_result;

  dev_addr &= 0x1f;

  /* Tell we will give address. */
  fnet_mdio_write(client, phy_addr,
		  MDIO_REG_MACR,
		  MDIO_BIT_MACR_FUNC_ADDR |
		  ((dev_addr) << MDIO_BIT_MACR_FUNC_DEVADDR_SHIFT));

  /* Write the address. */
  fnet_mdio_write(client, phy_addr,
		  MDIO_REG_MAADR,
		  reg);

  /* Tell we will do data transfer. */
  fnet_mdio_write(client, phy_addr,
		  MDIO_REG_MACR,
		  MDIO_BIT_MACR_FUNC_DATA_NO_INCR |
		  ((dev_addr) << MDIO_BIT_MACR_FUNC_DEVADDR_SHIFT));

  if (write)
    {
      /* Write the data. */
      mdio_mmd_result = fnet_mdio_write(client, phy_addr,
					MDIO_REG_MAADR, wr_value);
    }
  else
    {
      /* Read the data. */
      mdio_mmd_result = fnet_mdio_read(client, phy_addr,
				       MDIO_REG_MAADR);
    }

  return mdio_mmd_result;
}

typedef struct fnet_mdio_action_item_t
{
  const char *name;
  const char *descr;
  int onoff;
  int reg;
  uint16_t set;
  uint16_t unset;
} fnet_mdio_action_item;

fnet_mdio_action_item _fnet_mdio_actions[] =
  {
   { "autoneg_restart", "Enable and restart autonegotiation.", 0,
     MDIO_REG_BMCR, (MDIO_BIT_BMCR_AUTONEG_ENABLE |
		     MDIO_BIT_BMCR_AUTONEG_RESTART), 0 },
   { "autoneg_on", "Enable autonegotiation.", 0,
     MDIO_REG_BMCR, MDIO_BIT_BMCR_AUTONEG_ENABLE, 0 },
   { "autoneg_off", "Disable autonegotiation.", 0,
     MDIO_REG_BMCR, 0, MDIO_BIT_BMCR_AUTONEG_ENABLE },

   /* Specific autonegotiated modes. */
   { "adv10T/F",    "Advertise 10baseT/full.", 1,
     MDIO_REG_ANAR, MDIO_BIT_ANAR_ADVERT_10baseT_FULL, 0 },
   { "adv10T/H",    "Advertise 10baseT/half.", 1,
     MDIO_REG_ANAR, MDIO_BIT_ANAR_ADVERT_10baseT_HALF, 0 },
   { "adv100T/F",   "Advertise 100baseT/full.", 1,
     MDIO_REG_ANAR, MDIO_BIT_ANAR_ADVERT_100baseTX_FULL, 0 },
   { "adv100T/H",   "Advertise 100baseT/half.", 1,
     MDIO_REG_ANAR, MDIO_BIT_ANAR_ADVERT_100baseTX_HALF, 0 },

   /* Combined autonegotiated modes. */
   { "adv10",       "Advertise only 10baseT.", 1,
     MDIO_REG_ANAR, (MDIO_BIT_ANAR_ADVERT_10baseT_FULL |
		     MDIO_BIT_ANAR_ADVERT_10baseT_HALF),
     /**/           (MDIO_BIT_ANAR_ADVERT_100baseTX_FULL |
		     MDIO_BIT_ANAR_ADVERT_100baseTX_HALF)  },
   { "adv100",      "Advertise only 100baseT.", 1,
     MDIO_REG_ANAR, (MDIO_BIT_ANAR_ADVERT_100baseTX_FULL |
		     MDIO_BIT_ANAR_ADVERT_100baseTX_HALF),
     /**/           (MDIO_BIT_ANAR_ADVERT_10baseT_FULL |
		     MDIO_BIT_ANAR_ADVERT_10baseT_HALF) },
   { "adv10/100",   "Advertise 10baseT/100baseT.", 0,
     MDIO_REG_ANAR, (MDIO_BIT_ANAR_ADVERT_10baseT_FULL |
		     MDIO_BIT_ANAR_ADVERT_10baseT_HALF |
		     MDIO_BIT_ANAR_ADVERT_100baseTX_FULL |
		     MDIO_BIT_ANAR_ADVERT_100baseTX_HALF), 0 },

   /* Fixed speed if not autonegotiated. */
   { "fix100",      "Fixed speed 100 if not autonegotiated.", 1,
     MDIO_REG_BMCR, MDIO_BIT_BMCR_SPEED_100, 0 },
   { "fixduplex",   "Fixed duplex if not autonegotiated.", 1,
     MDIO_REG_BMCR, MDIO_BIT_BMCR_DUPLEX, 0 },
   { NULL, NULL, 0, 0, 0, 0 }
  };

void fnet_mdio_action_usage(void)
{
  fnet_mdio_action_item *action;

  printf ("MDIO actions:\n");
  printf ("\n");
  for (action = _fnet_mdio_actions; action->name; action++)
    {
      int n = 16;
      const char *pre = "";

      if (action->onoff)
	{
	  pre = "[-]";
	  n = 13;
	}

      printf ("  %s%-*s %s\n",
	      pre, n, action->name, action->descr);
    }
  printf ("\n");
}

void fnet_mdio_action(struct fnet_ctrl_client *client,
		      int phy_addr, const char *cmd)
{
  (void) client;
  (void) phy_addr;

  if (cmd)
    {
      fnet_mdio_action_item *action;
      char onoff = ' ';

      if (strcmp(cmd,"help") == 0)
	{
	  fnet_mdio_action_usage();
	  exit(0);
	}
      for (action = _fnet_mdio_actions; action->name; action++)
	{
	  if (strcmp(cmd, action->name) == 0)
	    break;
	  if (action->onoff &&
	      (cmd[0] == '+' || cmd[0] == '-') &&
	      strcmp(cmd+1, action->name) == 0)
	    {
	      onoff = cmd[0];
	      break;
	    }
	}
      if (action->name == NULL)
	{
	  fprintf (stderr, "Bad --mdio= action '%s'.\n", cmd);
	  fnet_mdio_action_usage();
	  exit(1);
	}

      {
	uint32_t prev;
	uint32_t value;

	prev = fnet_mdio_read(client, phy_addr, action->reg);

	value = prev & 0xffff; /* Get the value itself. */
	if (onoff != '-')
	  {
	    value |=  action->set;
	    value &= ~action->unset;
	  }
	else
	  {
	    value |=  action->unset;
	    value &= ~action->set;
	  }

	printf ("reg %d: %04x -> %04x\n", action->reg, prev, value);

	fnet_mdio_write(client, phy_addr, action->reg, value);
      }
    }

  {
    uint32_t bmcr = fnet_mdio_read(client, phy_addr, MDIO_REG_BMCR);
    uint32_t bmsr = fnet_mdio_read(client, phy_addr, MDIO_REG_BMSR);
    uint32_t phyidr1 = fnet_mdio_read(client, phy_addr, MDIO_REG_PHYIDR1);
    uint32_t phyidr2 = fnet_mdio_read(client, phy_addr, MDIO_REG_PHYIDR2);
    uint32_t anar = fnet_mdio_read(client, phy_addr, MDIO_REG_ANAR);
    uint32_t anlpar = fnet_mdio_read(client, phy_addr, MDIO_REG_ANLPAR);
    uint32_t gbcr = fnet_mdio_read(client, phy_addr, MDIO_REG_GBCR);
    uint32_t gbsr = fnet_mdio_read(client, phy_addr, MDIO_REG_GBSR);
    uint32_t gbesr = fnet_mdio_read(client, phy_addr, MDIO_REG_GBESR);


    uint32_t oui =
      (((phyidr1 & MDIO_BITS_PHYIDR1_OUI_MSB) >>
	MDIO_SHFT_PHYIDR1_OUI_MSB) << 6) |
      (((phyidr2 & MDIO_BITS_PHYIDR2_OUI_LSB) >>
	MDIO_SHFT_PHYIDR2_OUI_LSB));
    uint32_t vendor_model =
      (phyidr2 & MDIO_BITS_PHYIDR2_VENDOR_MODEL) >>
      MDIO_SHFT_PHYIDR2_VENDOR_MODEL;
    uint32_t model_rev =
      (phyidr2 & MDIO_BITS_PHYIDR2_MODEL_REV) >>
      MDIO_SHFT_PHYIDR2_MODEL_REV;

    uint32_t autoneg = !!(bmcr & MDIO_BIT_BMCR_AUTONEG_ENABLE);

    uint32_t mode_use = 0;
    uint32_t mode_cap = 0;
    uint32_t mode_adv = 0;
    uint32_t mode_lp  = 0;

#define MODE_SET(dest,src,pre,mode) do {			\
      if (src & pre##mode) dest |= FNET_MDIO_MODE_##mode;	\
    } while (0)

    MODE_SET(mode_cap, bmsr, MDIO_BIT_BMSR_ABILITY_, 100baseTX_FULL);
    MODE_SET(mode_cap, bmsr, MDIO_BIT_BMSR_ABILITY_, 100baseTX_HALF);
    MODE_SET(mode_cap, bmsr, MDIO_BIT_BMSR_ABILITY_, 10baseT_FULL);
    MODE_SET(mode_cap, bmsr, MDIO_BIT_BMSR_ABILITY_, 10baseT_HALF);
    if (bmsr & MDIO_BIT_BMSR_DEV_SUPP_GBESR) {
      MODE_SET(mode_cap, gbesr, MDIO_BIT_GBESR_ABILITY_, 1000baseT_FULL);
      MODE_SET(mode_cap, gbesr, MDIO_BIT_GBESR_ABILITY_, 1000baseT_HALF);
    }

    MODE_SET(mode_adv, anar, MDIO_BIT_ANAR_ADVERT_, 100baseTX_FULL);
    MODE_SET(mode_adv, anar, MDIO_BIT_ANAR_ADVERT_, 100baseTX_HALF);
    MODE_SET(mode_adv, anar, MDIO_BIT_ANAR_ADVERT_, 10baseT_FULL);
    MODE_SET(mode_adv, anar, MDIO_BIT_ANAR_ADVERT_, 10baseT_HALF);
    MODE_SET(mode_adv, gbcr, MDIO_BIT_GBCR_ADVERT_, 1000baseT_FULL);

    MODE_SET(mode_lp,  anlpar, MDIO_BIT_ANAR_ADVERT_, 100baseTX_FULL);
    MODE_SET(mode_lp,  anlpar, MDIO_BIT_ANAR_ADVERT_, 100baseTX_HALF);
    MODE_SET(mode_lp,  anlpar, MDIO_BIT_ANAR_ADVERT_, 10baseT_FULL);
    MODE_SET(mode_lp,  anlpar, MDIO_BIT_ANAR_ADVERT_, 10baseT_HALF);
    MODE_SET(mode_lp,  gbsr, MDIO_BIT_GBSR_LP_ADVERT_, 1000baseT_FULL);
    MODE_SET(mode_lp,  gbsr, MDIO_BIT_GBSR_LP_ADVERT_, 1000baseT_HALF);

    if (autoneg)
      {
	uint32_t mode_aneg = mode_adv & mode_lp;

#define MODE_COMMON(dest,src,mode)				\
	if (src & FNET_MDIO_MODE_##mode)			\
	  dest = FNET_MDIO_MODE_##mode;				\
	else

	/* Heuristic: modes negotiated will be highest common. */
	if (mode_aneg      & FNET_MDIO_MODE_1000baseT_FULL)
	  mode_use         = FNET_MDIO_MODE_1000baseT_FULL;
	else if (mode_aneg & FNET_MDIO_MODE_1000baseT_HALF)
	  mode_use         = FNET_MDIO_MODE_1000baseT_HALF;
	else if (mode_aneg & FNET_MDIO_MODE_100baseTX_FULL)
	  mode_use         = FNET_MDIO_MODE_100baseTX_FULL;
	else if (mode_aneg & FNET_MDIO_MODE_100baseTX_HALF)
	  mode_use         = FNET_MDIO_MODE_100baseTX_HALF;
	else if (mode_aneg & FNET_MDIO_MODE_10baseT_FULL)
	  mode_use         = FNET_MDIO_MODE_10baseT_FULL;
	else if (mode_aneg & FNET_MDIO_MODE_10baseT_HALF)
	  mode_use         = FNET_MDIO_MODE_10baseT_HALF;
      }
    else
      {
	if (bmcr & MDIO_BIT_BMCR_SPEED_100)
	  {
	    if (bmcr & MDIO_BIT_BMCR_DUPLEX)
	      mode_use     = FNET_MDIO_MODE_100baseTX_FULL;
	    else
	      mode_use     = FNET_MDIO_MODE_100baseTX_HALF;
	  }
	else
	  {
	    if (bmcr & MDIO_BIT_BMCR_DUPLEX)
	      mode_use     = FNET_MDIO_MODE_10baseT_FULL;
	    else
	      mode_use     = FNET_MDIO_MODE_10baseT_HALF;
	  }
      }

    printf ("PHY:          OUI %02x:%02x:%02x  v/model: %02x rev: %01x\n",
	    (oui >> 16) & 0xff,
	    (oui >>  8) & 0xff,
	    (oui      ) & 0xff,
	    vendor_model,
	    model_rev);

    if (((phyidr1 & 0xffff) == 0xffff &&
	 (phyidr2 & 0xffff) == 0xffff) ||
	((phyidr1 & 0xffff) == 0 &&
	 (phyidr2 & 0xffff) == 0))
      {
	fprintf (stderr, "Does not look like a valid PHY (all 0 or 1).\n");
	exit(1);
      }

    printf ("autoneg:      %-3s  %s%s%s%s%s%s%s\n",
	    autoneg ? "on" : "off",
	    mode_use ? "" : "?",
	    mode_use & FNET_MDIO_MODE_1000baseT_FULL ? " 1000baseT/F" : "",
	    mode_use & FNET_MDIO_MODE_1000baseT_HALF ? " 1000baseT/H" : "",
	    mode_use & FNET_MDIO_MODE_100baseTX_FULL ? " 100baseT/F" : "",
	    mode_use & FNET_MDIO_MODE_100baseTX_HALF ? " 100baseT/H" : "",
	    mode_use & FNET_MDIO_MODE_10baseT_FULL   ? " 10baseT/F" : "",
	    mode_use & FNET_MDIO_MODE_10baseT_HALF   ? " 10baseT/H" : "");

    printf ("ability:     %s%s%s%s%s%s\n",
	    mode_cap & FNET_MDIO_MODE_1000baseT_FULL ? " 1000baseT/F" : "",
	    mode_cap & FNET_MDIO_MODE_1000baseT_HALF ? " 1000baseT/H" : "",
	    mode_cap & FNET_MDIO_MODE_100baseTX_FULL ? " 100baseT/F" : "",
	    mode_cap & FNET_MDIO_MODE_100baseTX_HALF ? " 100baseT/H" : "",
	    mode_cap & FNET_MDIO_MODE_10baseT_FULL   ? " 10baseT/F" : "",
	    mode_cap & FNET_MDIO_MODE_10baseT_HALF   ? " 10baseT/H" : "");

    printf ("advertise:   %s%s%s%s%s%s\n",
	    mode_adv & FNET_MDIO_MODE_1000baseT_FULL ? " 1000baseT/F" : "",
	    mode_adv & FNET_MDIO_MODE_1000baseT_HALF ? " 1000baseT/H" : "",
	    mode_adv & FNET_MDIO_MODE_100baseTX_FULL ? " 100baseT/F" : "",
	    mode_adv & FNET_MDIO_MODE_100baseTX_HALF ? " 100baseT/H" : "",
	    mode_adv & FNET_MDIO_MODE_10baseT_FULL   ? " 10baseT/F" : "",
	    mode_adv & FNET_MDIO_MODE_10baseT_HALF   ? " 10baseT/H" : "");

    printf ("lp-advertise:%s%s%s%s%s%s\n",
	    mode_lp & FNET_MDIO_MODE_1000baseT_FULL ? " 1000baseT/F" : "",
	    mode_lp & FNET_MDIO_MODE_1000baseT_HALF ? " 1000baseT/H" : "",
	    mode_lp & FNET_MDIO_MODE_100baseTX_FULL ? " 100baseT/F" : "",
	    mode_lp & FNET_MDIO_MODE_100baseTX_HALF ? " 100baseT/H" : "",
	    mode_lp & FNET_MDIO_MODE_10baseT_FULL   ? " 10baseT/F" : "",
	    mode_lp & FNET_MDIO_MODE_10baseT_HALF   ? " 10baseT/H" : "");

  }
}

void fnet_mdio_do_write(struct fnet_ctrl_client *client,
			int phy_addr, int reg, int value)
{
  uint32_t mdio_result;

  mdio_result =
    fnet_mdio_write(client, phy_addr, reg, value);

  fnet_mdio_print_result(mdio_result, value, NULL);

  mdio_result =
    fnet_mdio_read(client, phy_addr, reg);

  fnet_mdio_print_result(mdio_result, 0, NULL);
}

void fnet_mdio_mmd_do_write(struct fnet_ctrl_client *client,
			    int phy_addr, int devaddr, int reg, int value)
{
  uint32_t mdio_result;

  fnet_mdio_mmd_devaddr_reg mmd_addr;

  mmd_addr.devaddr = devaddr;
  mmd_addr.reg     = reg;

  mdio_result =
    fnet_mdio_mmd_read_write(client, phy_addr, devaddr, reg, 1, value);

  fnet_mdio_print_result(mdio_result, value, &mmd_addr);

  mdio_result =
    fnet_mdio_mmd_read_write(client, phy_addr, devaddr, reg, 0, 0);

  fnet_mdio_print_result(mdio_result, 0, &mmd_addr);
}

void fnet_mdio_dump(struct fnet_ctrl_client *client,
		    int phy_addr)
{
  uint32_t mdio_result[32];
  int i;

  phy_addr &= 0x1f;

  for (i = 0; i < 32; i++)
    if (i != MDIO_REG_MACR &&
	i != MDIO_REG_MAADR)
      {
	mdio_result[i] =
	  fnet_mdio_read(client, phy_addr, i);

	fnet_mdio_print_result(mdio_result[i], 0, NULL);
      }

  if ((mdio_result[MDIO_REG_PHYIDR1] & 0xffff) == MDIO_PHYIDR1_RTL8211E &&
      (mdio_result[MDIO_REG_PHYIDR2] & 0xffff) == MDIO_PHYIDR2_RTL8211E)
    {
      uint32_t mdio_mmd;

      fnet_mdio_mmd_devaddr_reg regs[6] =
	{ { 3, 0, },
	  { 3, 1, },
	  { 3, 20, },
	  { 3, 22, },
	  { 7, 60, },
	  { 7, 61, },
	};

      for (i = 0; i < 6; i++)
	{
	  fnet_mdio_mmd_devaddr_reg *mmd_addr = &regs[i];

	  mdio_mmd =
	    fnet_mdio_mmd_read_write(client, phy_addr,
				     mmd_addr->devaddr, mmd_addr->reg, 0, 0);

	  fnet_mdio_print_result(mdio_mmd, 0,
				 mmd_addr);
	}
    }
}
