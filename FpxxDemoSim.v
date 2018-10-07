
module tb;

    reg             osc_clk;
    wire [31:0]     op_a;
    wire [31:0]     op_b;
    wire [31:0]     op_a_p_op_b;
    reg  [62:0]     lz_in;
    wire [5:0]      lz;

    FpxxDemo u_dut (
      .osc_clk(osc_clk),
      .op_a(op_a),
      .op_b(op_b),
      .op_a_p_op_b(op_a_p_op_b),
      .lz_in(lz_in),
      .lz(lz));

    initial begin
        osc_clk     = 0;
        forever begin
            #5 osc_clk  = ~osc_clk;
        end
    end

    initial begin
        $display("%t: Start of simulation!", $time);
        $dumpfile("waves.vcd");
        $dumpvars(0, tb);

        repeat(1000) @(posedge osc_clk);
        $display("%t: Simulation complete...", $time);
        $finish;
    end

    initial begin
        lz_in = 63'h00000000_00000000;
        @(posedge osc_clk);
        $display(lz);
        @(posedge osc_clk);

        lz_in = 63'h40000000_00000000;
        @(posedge osc_clk);
        $display(lz);
        @(posedge osc_clk);

        lz_in = 63'h00000000_80000000;
        @(posedge osc_clk);
        $display(lz);
        @(posedge osc_clk);

    end



endmodule

