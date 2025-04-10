module top (
    input wire clk,         // 100 MHz system clock
    input wire rst,         // Reset button

    // RGMII transmit interface
    output wire [3:0] rgmii_txd,
    output wire rgmii_tx_ctl,
    output wire rgmii_txc,

    // MDIO interface (for PHY configuration)
    output wire mdc,
    inout wire mdio
);

    // Internal signals
    wire mdio_ready;
    wire [7:0] eth_data, ip_data, udp_data;
    wire eth_valid, ip_valid, udp_valid;
    wire tx_clk;
    wire locked;

    // Clocking Wizard instance (converts 100 MHz to 125 MHz)
    clk_wiz_0 clk_gen (
        .clk_out1(tx_clk),  // Generated 125 MHz clock
        .reset(rst),
        .locked(locked),
        .clk_in1(clk)       // 100 MHz system clock from FPGA
    );

    //------------------------------------------------
    // Start signals for header generation modules
    //------------------------------------------------
    reg eth_start;
    reg ip_start;
    reg udp_start;

    //------------------------------------------------
    // UDP packet generator
    //------------------------------------------------
    udp_sender udp_inst (
        .clk(tx_clk),      // Unified clock
        .start(udp_start), // Improved start control
        .udp_data(udp_data),
        .udp_valid(udp_valid)
    );

    //------------------------------------------------
    // IP header generator
    //------------------------------------------------
    ip_header ip_inst (
        .clk(tx_clk),      // Unified clock
        .start(ip_start),
        .ip_data(ip_data),
        .ip_valid(ip_valid)
    );

    //------------------------------------------------
    // Ethernet frame generator (MAC + EtherType)
    //------------------------------------------------
    ethernet_frame eth_inst (
        .clk(tx_clk),      // Unified clock
        .start(eth_start),
        .eth_data(eth_data),
        .eth_valid(eth_valid)
    );

    //------------------------------------------------
    // PHY initialization (MDIO configuration)
    //------------------------------------------------
    mdio_ctrl mdio_inst (
        .clk(tx_clk), // Unified clock
        .rst(rst),
        .mdc(mdc),
        .mdio(mdio),
        .ready(mdio_ready)
    );

    //------------------------------------------------
    // Signals for CRC32 calculation
    //------------------------------------------------
    wire [31:0] crc_value;
    wire crc_ready;

    reg crc_init;
    reg crc_calc;
    reg crc_finish;

    crc32_ethernet crc_inst (
        .clk(tx_clk),
        .rst(rst),
        .data_valid(rgmii_tx_valid),
        .data_in(rgmii_tx_data),
        .crc_init(crc_init),
        .crc_calc(crc_calc),
        .crc_finish(crc_finish),
        .crc_out(crc_value),
        .crc_valid(crc_ready)
    );

    //------------------------------------------------
    // Transmit data control (synchronized with RGMII clock)
    //------------------------------------------------
    reg [7:0] rgmii_tx_data;
    reg rgmii_tx_valid;
    reg [3:0] tx_state;

    always @(posedge tx_clk or posedge rst) begin
        if (rst) begin
            tx_state        <= 4'b0000;
            rgmii_tx_data   <= 8'h00;
            rgmii_tx_valid  <= 1'b0;
            eth_start       <= 1'b0;
            ip_start        <= 1'b0;
            udp_start       <= 1'b0;
            crc_init        <= 1'b0;
            crc_calc        <= 1'b0;
            crc_finish      <= 1'b0;
        end else begin
            case (tx_state)
                //------------------------------------------------
                // Wait for PHY ready → start sending Ethernet header
                //------------------------------------------------
                4'b0000: begin
                    if (mdio_ready) begin
                        eth_start       <= 1'b1; // Trigger start
                        crc_init        <= 1'b1; // Initialize CRC
                        crc_calc        <= 1'b0;
                        crc_finish      <= 1'b0;
                        tx_state        <= 4'b0001;
                    end
                end
                //------------------------------------------------
                // Send Ethernet header
                //------------------------------------------------
                4'b0001: begin
                    crc_init <= 1'b0;
                    if (eth_valid) begin
                        rgmii_tx_data  <= eth_data;
                        rgmii_tx_valid <= 1'b1;
                        crc_calc       <= 1'b1;
                    end else begin
                        rgmii_tx_valid <= 1'b0;
                        eth_start      <= 1'b0; // Stop after done
                        ip_start       <= 1'b1; // Start IP header
                        tx_state       <= 4'b0010;
                    end
                end
                //------------------------------------------------
                // Send IP header
                //------------------------------------------------
                4'b0010: begin
                    if (ip_valid) begin
                        rgmii_tx_data  <= ip_data;
                        rgmii_tx_valid <= 1'b1;
                        crc_calc       <= 1'b1;
                    end else begin
                        rgmii_tx_valid <= 1'b0;
                        ip_start       <= 1'b0; // Done
                        udp_start      <= 1'b1; // Start UDP
                        tx_state       <= 4'b0011;
                    end
                end
                //------------------------------------------------
                // Send UDP header + payload
                //------------------------------------------------
                4'b0011: begin
                    if (udp_valid) begin
                        rgmii_tx_data  <= udp_data;
                        rgmii_tx_valid <= 1'b1;
                        crc_calc       <= 1'b1;
                    end else begin
                        rgmii_tx_valid <= 1'b0;
                        udp_start      <= 1'b0; // Done
                        crc_finish     <= 1'b1; // Finalize CRC
                        tx_state       <= 4'b0100;
                    end
                end
                //------------------------------------------------
                // Wait to send CRC (until calculation completes)
                //------------------------------------------------
                4'b0100: begin
                    crc_finish <= 1'b0;
                    crc_calc   <= 1'b0;
                    if (crc_ready) begin
                        rgmii_tx_data  <= crc_value[7:0];
                        rgmii_tx_valid <= 1'b1;
                        tx_state       <= 4'b0101;
                    end else begin
                        rgmii_tx_valid <= 1'b0;
                    end
                end
                //------------------------------------------------
                // Send remaining CRC bytes
                //------------------------------------------------
                4'b0101: begin
                    rgmii_tx_data <= crc_value[15:8];
                    tx_state      <= 4'b0110;
                end
                4'b0110: begin
                    rgmii_tx_data <= crc_value[23:16];
                    tx_state      <= 4'b0111;
                end
                4'b0111: begin
                    rgmii_tx_data <= crc_value[31:24];
                    rgmii_tx_valid <= 1'b0;
                    tx_state      <= 4'b0000; // Return to initial state
                end
                default: begin
                    tx_state      <= 4'b0000;
                end
            endcase
        end
    end

    //------------------------------------------------
    // RGMII transmit module
    //------------------------------------------------
    rgmii_tx rgmii_inst (
        .tx_clk(tx_clk),
        .tx_data(rgmii_tx_data),
        .tx_valid(rgmii_tx_valid),
        .tx_ctl(rgmii_tx_ctl),
        .txd(rgmii_txd)
    );

    // RGMII transmit clock output
    assign rgmii_txc = tx_clk;

endmodule
