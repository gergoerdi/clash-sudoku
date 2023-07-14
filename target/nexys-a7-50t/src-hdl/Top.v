module Top(
           input  CLK100MHZ,
           input  CPU_RESETN,
           input  UART_TXD_IN,
           output UART_RXD_OUT
           );

   topEntity u_topEntity
     (.CLK_100MHZ(CLK100MHZ),
      .RESET(~CPU_RESETN),
      .RX(UART_TXD_IN),
      .TX(UART_RXD_OUT)
      );

endmodule
