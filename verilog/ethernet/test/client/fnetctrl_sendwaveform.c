#include "fakernet.h"
#include "fnet_client.h"
#include "fnetctrl.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <arpa/inet.h>
#include <errno.h>

// Function to send waveform data stored in a binary file to the FPGA via UDP register access
void fnet_send_waveform(struct fnet_ctrl_client *client, const char *filename) {
    FILE *fp = fopen(filename, "rb");
    if (!fp) {
        perror("Failed to open waveform file");
        exit(1);
    }

    float buffer[100];  // One event = 100 float values
    int index = 0;

    // Read and send each event
    while (fread(buffer, sizeof(float), 100, fp) == 100) {
        for (int i = 0; i < 100; i++) {
            uint32_t value;
            memcpy(&value, &buffer[i], sizeof(uint32_t));  // Convert float to raw uint32_t bits

            // Send each value to FPGA register address 0x1000 + i
            fnet_ctrl_write_register(client, 0x1000 + i, value);
        }

        // Optionally write a trigger register to indicate event completion
        fnet_ctrl_write_register(client, 0x1FFF, index++);  // Dummy write to indicate event
    }

    fclose(fp);
    printf("Finished sending waveform data.\n");
}

void fnet_ctrl_write_register(struct fnet_ctrl_client *client,
                              uint32_t reg_addr,
                              uint32_t reg_value) {
    fakernet_reg_acc_item *send, *recv;
    int num_send = 1;
    fnet_ctrl_get_send_recv_bufs(client, &send, &recv);

    send[0].addr = htonl(FAKERNET_REG_ACCESS_ADDR_WRITE | reg_addr);
    send[0].data = htonl(reg_value);

    int ret = fnet_ctrl_send_recv_regacc(client, num_send);
    if (ret != 1) {
        fprintf(stderr, "Write failed: %s\n", fnet_ctrl_last_error(client));
        exit(1);
    }
}
