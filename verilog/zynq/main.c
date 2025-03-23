#include "lwip/init.h"
#include "lwip/netif.h"
#include "lwip/tcp.h"
#include "lwip/etharp.h"
#include "lwip/timeouts.h"
#include "netif/xadapter.h"

#include "xil_printf.h"
#include "xil_cache.h"
#include "xiltimer.h"  // xiltimer ライブラリ

// lwIP のタイマー間隔定義
#define TCP_TMR_INTERVAL      250    // [ms] TCP タイマー間隔
#define MY_ARP_TMR_INTERVAL   10000  // [ms] ARP タイマー間隔

// グローバルネットワークインターフェース
struct netif server_netif;
// xiltimer のインスタンス（型は xiltimer.h で定義される XTimer）
XTimer my_timer;

/ lwIP が利用する現在時刻取得関数 (1ms 単位) /
u32 sys_now(void)
{
    XTime now;
    XTime_GetTime(&now);  // マイクロ秒単位を取得
#ifndef COUNTS_PER_SECOND
    // ここは PYNQ-Z2 の PS7 の CPU クロック値（例: 666666687 Hz）
    #define COUNTS_PER_SECOND 666666687ULL
#endif
    return (u32)(now / (COUNTS_PER_SECOND / 1000));
}

/ TCP 受信時のコールバック（エコーバック） /
static err_t recv_callback(void arg, struct tcp_pcb tpcb, struct pbuf *p, err_t err)
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

/ TCP 接続受付コールバック /
static err_t accept_callback(void arg, struct tcp_pcb tpcb, err_t err)
{
    xil_printf("Connection accepted.\n");
    tcp_recv(tpcb, recv_callback);
    return ERR_OK;
}

/ TCP Echo サーバの起動（ポート 7 を使用） /
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

/ メイン関数 /
int main(void)
{
    int status;
    ip_addr_t ipaddr, netmask, gw;
    u32 tcp_timer, arp_timer;

    xil_printf("\n\r--- lwIP TCP Echo Server using xiltimer ---\n\r");

    / キャッシュ有効化 /
    Xil_ICacheEnable();
    Xil_DCacheEnable();

    / xiltimer 初期化（xiltimer BSP が有効な状態である前提） /
    status = XilTickTimer_Init(&my_timer);
    if (status != XST_SUCCESS) {
        xil_printf("XilTickTimer_Init failed.\n\r");
        return -1;
    }
    // ※ XilTickTimer_Start() は API に存在しない場合が多いため呼ばず、初期化時に自動スタートする前提とします

    / lwIP スタック初期化 /
    lwip_init();

    / 固定 IP の設定 /
    IP4_ADDR(&ipaddr, 192, 168, 1, 10);
    IP4_ADDR(&netmask, 255, 255, 255, 0);
    IP4_ADDR(&gw, 192, 168, 1, 1);

    /* ネットワークインターフェース追加
       - xemacpsif_init() は BSP が提供する初期化関数
       - etharp_input() は lwIP のレイヤ2入力関数。型が合わない場合はキャスト */
    extern err_t xemacpsif_init(struct netif *netif);
    netif_add(&server_netif, &ipaddr, &netmask, &gw, NULL,
              xemacpsif_init, (netif_input_fn)etharp_input);
    netif_set_default(&server_netif);
    netif_set_up(&server_netif);

    xil_printf("Network interface up. IP: %s\n\r", ipaddr_ntoa(&ipaddr));

    / TCP Echo サーバ起動 /
    start_tcp_echo_server();

    / タイマー初期値取得 /
    tcp_timer = sys_now();
    arp_timer = sys_now();

    / メインループ /
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

    / 通常はここに到達しない /
    Xil_DCacheDisable();
    Xil_ICacheDisable();

    return 0;
}
