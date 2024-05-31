module Top(
           input  wire CLK100MHZ,
           input  wire CPU_RESETN,
           input  wire UART_TXD_IN,
           output wire UART_RXD_OUT
           );

   topEntity u_topEntity
     (.CLK_100MHZ(CLK100MHZ),
      .RESET(~CPU_RESETN),
      .RX(UART_TXD_IN),
      .TX(UART_RXD_OUT)
      );

endmodule
