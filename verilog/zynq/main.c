#include "lwip/init.h"
#include "lwip/netif.h"
#include "lwip/tcp.h"
#include "lwip/etharp.h"
#include "lwip/timeouts.h"
#include "netif/xadapter.h"

#include "xil_printf.h"
#include "xil_cache.h"
#include "xiltimer.h"  // xiltimer library

// Timer intervals for lwIP
#define TCP_TMR_INTERVAL      250    // [ms] TCP timer interval
#define MY_ARP_TMR_INTERVAL   10000  // [ms] ARP timer interval

// Global network interface instance
struct netif server_netif;
// xiltimer instance (type XTimer defined in xiltimer.h)
XTimer my_timer;

/* Time function required by lwIP (returns milliseconds) */
u32 sys_now(void)
{
    XTime now;
    XTime_GetTime(&now);  // Get time in microseconds
#ifndef COUNTS_PER_SECOND
    // CPU clock frequency of PS7 on PYNQ-Z2 (example: 666666687 Hz)
    #define COUNTS_PER_SECOND 666666687ULL
#endif
    return (u32)(now / (COUNTS_PER_SECOND / 1000));
}

/* TCP receive callback (echoes back received data) */
static err_t recv_callback(void *arg, struct tcp_pcb *tpcb, struct pbuf *p, err_t err)
{
    if (!p) {
        xil_printf("Connection closed.\n");
        tcp_close(tpcb);
        return ERR_OK;
    }
    xil_printf("Received %d bytes: %s\n", p->len, (char *)p->payload);
    tcp_write(tpcb, p->payload, p->len, 1);
    pbuf_free(p);
    return ERR_OK;
}

/* TCP accept callback */
static err_t accept_callback(void *arg, struct tcp_pcb *tpcb, err_t err)
{
    xil_printf("Connection accepted.\n");
    tcp_recv(tpcb, recv_callback);
    return ERR_OK;
}

/* Start TCP Echo Server on port 7 */
void start_tcp_echo_server(void)
{
    struct tcp_pcb *pcb;
    err_t err;

    pcb = tcp_new_ip_type(IPADDR_TYPE_ANY);
    if (!pcb) {
        xil_printf("Error creating PCB.\n");
        return;
    }
    err = tcp_bind(pcb, IP_ANY_TYPE, 7);
    if (err != ERR_OK) {
        xil_printf("Unable to bind to port 7: %d\n", err);
        tcp_close(pcb);
        return;
    }
    pcb = tcp_listen(pcb);
    tcp_accept(pcb, accept_callback);
    xil_printf("TCP Echo Server is up.\n");
}

/* Main function */
int main(void)
{
    int status;
    ip_addr_t ipaddr, netmask, gw;
    u32 tcp_timer, arp_timer;

    xil_printf("\n\r--- lwIP TCP Echo Server using xiltimer ---\n\r");

    /* Enable instruction and data caches */
    Xil_ICacheEnable();
    Xil_DCacheEnable();

    /* Initialize xiltimer (BSP must include xiltimer support) */
    status = XilTickTimer_Init(&my_timer);
    if (status != XST_SUCCESS) {
        xil_printf("XilTickTimer_Init failed.\n\r");
        return -1;
    }
    // Note: XilTickTimer_Start() may not exist; assume auto-start after init

    /* Initialize lwIP stack */
    lwip_init();

    /* Set static IP address */
    IP4_ADDR(&ipaddr, 192, 168, 1, 10);
    IP4_ADDR(&netmask, 255, 255, 255, 0);
    IP4_ADDR(&gw, 192, 168, 1, 1);

    /* Add network interface
       - xemacpsif_init() is BSP-specific init function
       - etharp_input() is the lwIP Layer 2 input function (cast if needed) */
    extern err_t xemacpsif_init(struct netif *netif);
    netif_add(&server_netif, &ipaddr, &netmask, &gw, NULL,
              xemacpsif_init, (netif_input_fn)etharp_input);
    netif_set_default(&server_netif);
    netif_set_up(&server_netif);

    xil_printf("Network interface up. IP: %s\n\r", ipaddr_ntoa(&ipaddr));

    /* Start the TCP Echo Server */
    start_tcp_echo_server();

    /* Get initial timer values */
    tcp_timer = sys_now();
    arp_timer = sys_now();

    /* Main loop */
    while (1) {
        xemacif_input(&server_netif);

        if ((sys_now() - tcp_timer) >= TCP_TMR_INTERVAL) {
            tcp_tmr();
            tcp_timer = sys_now();
        }
        if ((sys_now() - arp_timer) >= MY_ARP_TMR_INTERVAL) {
            etharp_tmr();
            arp_timer = sys_now();
        }
    }

    /* Normally never reached */
    Xil_DCacheDisable();
    Xil_ICacheDisable();

    return 0;
}
