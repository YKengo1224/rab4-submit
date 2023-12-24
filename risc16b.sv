`default_nettype none

module risc16b#(
  parameter int SERACH_IMAGE_SIZE = 16,
  parameter int TARGET_IMAGE_SIZE = 128
)
    (
   input wire 	 clk,
   input wire 	 rst,
   output logic [15:0] i_addr,
   output logic 	 i_oe,
   input wire [15:0] 	 i_din,
   output logic [15:0] d_addr,
   output logic 	 d_oe,
   input wire [15:0] 	 d_din,
   output logic [15:0] d_dout,
   output logic [1:0]  d_we
   );




  localparam [2:0]  STATE_I_SDATA = 3'b001;  //  state : input find search image data
  localparam [2:0]  STATE_I_TDATA = 3'b010;  //  state : input find target image data
  localparam [2:0]  STATE_FINISH  = 3'b011;

  logic [15:0]      if_pc;

  logic [2:0]       state;
  logic [2:0]       state_prev;
  
  logic [15:0]      fisrt_addr;
  logic [15:0]      second_addr;
  logic [15:0]      fisrt_data;
  logic [15:0]      second_data;

  logic[15:0]       w_addr;


  localparam int    SERACH_IMAGE_PNUM = SERACH_IMAGE_SIZE*SERACH_IMAGE_SIZE;
  localparam int    SERACH_WADDR_BITWIDTH = $clog2(SERACH_IMAGE_PNUM);
  logic [SERACH_IMAGE_PNUM-1:0][7:0]                             serach_image;
  logic [SERACH_WADDR_BITWIDTH:0]                                search_waddr;




  localparam int COUNT_WIDTH  = 8;

  logic [COUNT_WIDTH-1:0]  t_height_count;
  logic [COUNT_WIDTH-1:0]  t_width_count;

  localparam  int START_COUNT_WIDTH = 32;
  logic [START_COUNT_WIDTH-1:0]  calc_start_count;   //count up while  come to (0,0) pixel 
  logic                    calc_start_flag;
  logic                    stencil_end_flag;
  
  localparam int INPUT_STENCIL_WIDTH = COUNT_WIDTH + COUNT_WIDTH+ 8;

  logic [3:0][INPUT_STENCIL_WIDTH-1:0]  i_stencil_data;

  localparam int STENCIL_TOTAL_BITWIDTH     = SERACH_IMAGE_SIZE * 20 * INPUT_STENCIL_WIDTH;
  localparam int STENCIL_PATCH_COLUMN       = ($clog2(SERACH_IMAGE_SIZE)*4 + 4);
  logic [SERACH_IMAGE_SIZE-1:0][STENCIL_PATCH_COLUMN-1:0][INPUT_STENCIL_WIDTH-1:0]  stencil_reg;

  localparam int LINE_BUFFER_SIZE           = (TARGET_IMAGE_SIZE - STENCIL_PATCH_COLUMN); 
  localparam int LINE_BUFFER_TOTAL_BITWIDTH = (SERACH_IMAGE_SIZE-1) * LINE_BUFFER_SIZE * INPUT_STENCIL_WIDTH;
  


  logic [SERACH_IMAGE_SIZE-2:0][LINE_BUFFER_SIZE-1:0][INPUT_STENCIL_WIDTH-1:0]      line_buffers;

  logic [4:0][SERACH_IMAGE_PNUM-1:0][7:0]                          stencil_patches;
  logic [4:0][COUNT_WIDTH+COUNT_WIDTH-1:0]                         coordinates;    //座標
  logic [4:0]                                                      calc_eval_en;
  logic [4:0][15:0]                          eval_data;
  logic [4:0]                                eval_data_valid;
  logic [4:0][COUNT_WIDTH+COUNT_WIDTH-1:0]   eval_coordinates;

  logic [2:0][2:0][15:0]		     max_calc_reg;
  logic [2:0][2:0][15:0]                      max_coordinates_reg;

  logic [15:0]                           max_eval_value;
  logic [15:0]                           max_coordinates;

//######################################################
//input pixel  data
//######################################################
  /*
  input 4 pixel data per clk

  i_addr: fisrt addr
  d_addr: second  addr

  i_oe  : fist data enable
  d_oe  : second data enable 

  i_din: fisrt data
  d_din: second data


  */
  assign if_pc  = fisrt_addr;
  assign i_addr = fisrt_addr;
  assign d_addr = (state != STATE_FINISH) ? second_addr: w_addr;
  assign i_oe   = 1'b1; 
  assign d_oe   = 1'b1; 
  assign fisrt_data = i_din;
  assign second_data  = d_din;

  always_ff @(posedge clk) begin : state_ff
    if(rst) begin
      state <= STATE_I_SDATA;
    end
    else begin
      case (state)

        STATE_I_SDATA: begin
          if(fisrt_addr == 16'hb0fc) begin
            state <= STATE_I_TDATA;
          end
          else begin
            state <= STATE_I_SDATA;
          end
        end 

        STATE_I_TDATA: begin
          //if(fisrt_addr == 16'hfffc) begin
          if(fisrt_addr == 16'h0000) begin            
            state <= STATE_FINISH;
          end
          else begin
            state <= STATE_I_TDATA;
          end
        end

        default: state <= STATE_FINISH;
      endcase
    end
  end

  always_ff @(posedge  clk) begin : fisrt_addr_block
    if(rst) begin
      fisrt_addr <= 16'hb000;
    end    
    else if(fisrt_addr == 16'hb0fc) begin  //finish input serch image
      fisrt_addr <= 16'hc000;
    end
    else if(fisrt_addr == 16'hfffc) begin //finish input target image
      fisrt_addr <= 16'h0000;
    end
    else begin
      fisrt_addr <= fisrt_addr + 16'h0004;
    end
  end

  always_ff @(posedge clk) begin : d_addr_block
    if(rst) begin
      second_addr <= 16'hb002;
    end    
    else if(second_addr == 16'hb0fe) begin  //finish input serch image
      second_addr <= 16'hc002;
    end
    else if(second_addr == 16'hfffe) begin //finish input target image
      second_addr <= 16'h0002;
    end
    else begin
      second_addr <= second_addr + 16'h0004;
    end

  end

//######################################################
//######################################################


//######################################################
//state : output max data
//######################################################
  always_ff @(posedge clk) begin
    if(rst) begin
      w_addr <= 16'd0;
    end
    else if(fisrt_addr == 16'h003c)begin
      w_addr <= 16'hbf00;
    end
    else begin
      w_addr <= w_addr + 16'd0002;
    end
  end

  always_ff @(posedge clk) begin
    if(rst) begin
      d_dout <= 16'd0;
    end
    else if(fisrt_addr == 16'h003c)begin
      d_dout <= max_coordinates;
    end
    else if(fisrt_addr == 16'h0040)begin
      d_dout <= max_eval_value;
    end
    else begin
      d_dout <= 16'd0;
    end
  end
  
  always_ff @(posedge clk) begin
    if(rst) begin
      d_we <= 2'b00;
    end
    else if(fisrt_addr == 16'h003c)begin
      d_we <= 2'b11;
    end
    else if(fisrt_addr == 16'h0040)begin
      d_we <= 2'b11;
    end
    else begin
      d_we <= 2'b00;
    end
  end

//######################################################
//######################################################

//######################################################
//state : input serach image data
//######################################################

  always_ff @(posedge clk) begin :serach_image_block
    if(rst) begin
        serach_image <= (SERACH_IMAGE_PNUM*8)'('b0);
    end
    else if(state == STATE_I_SDATA) begin
        serach_image[3] <= fisrt_data[15:8];
        serach_image[2] <= fisrt_data[7:0];
        serach_image[1] <= second_data[15:8];
        serach_image[0] <= second_data[7:0];
        for(int i = 4; (i+3) < SERACH_IMAGE_PNUM;i=i+4) begin
           serach_image[i+3] <= serach_image[(i-4)+3];
          serach_image[i+2] <= serach_image[(i-4)+2];
          serach_image[i+1] <= serach_image[(i-4)+1];
          serach_image[i+0] <= serach_image[(i-4)+0];
        end
    end
    else begin
      serach_image <= serach_image;
    end
  end
//######################################################
//######################################################


//######################################################
//state : input target image dataa
//######################################################

  //------------------target count-------------------
  always_ff @(posedge clk) begin
    if(rst) begin
      t_width_count <= COUNT_WIDTH'('d0);
    end
    else if(state == STATE_I_TDATA)begin
      if(t_width_count+4 == TARGET_IMAGE_SIZE) begin
        t_width_count <= COUNT_WIDTH'('d0);
      end
      else begin
        t_width_count <= t_width_count + COUNT_WIDTH'('d4);
      end
    end
    else begin
      t_width_count <= COUNT_WIDTH'('d0);
    end
  end


  always_ff @(posedge clk) begin
    if(rst) begin
      t_height_count <= COUNT_WIDTH'('d0);
    end
    else if(state == STATE_I_TDATA)begin
      if(t_width_count+4 == TARGET_IMAGE_SIZE) begin
        if(t_height_count == TARGET_IMAGE_SIZE) begin
          t_height_count <= COUNT_WIDTH'('d0);
        end
        else begin
          t_height_count <= t_height_count + COUNT_WIDTH'('d1);
        end
      end
      else begin
        t_height_count <= t_height_count;
      end
    end
    else begin
      t_height_count <= COUNT_WIDTH'('d0);
    end
  end

  always_ff @(posedge clk) begin
    if(rst) begin
      calc_start_count <= START_COUNT_WIDTH'('d0);
    end
    else if(calc_start_count==0 & state==STATE_I_TDATA )begin
      calc_start_count <= START_COUNT_WIDTH'('d4);
    end
    else if(calc_start_flag) begin
      calc_start_count <= calc_start_count;
    end
    else if(calc_start_count != START_COUNT_WIDTH'('d0)) begin
      calc_start_count <= calc_start_count + START_COUNT_WIDTH'('d4 );
    end
    else begin
      calc_start_count <= START_COUNT_WIDTH'('d0);
    end
  end

  assign calc_start_flag = (calc_start_count == (SERACH_IMAGE_SIZE-1)*LINE_BUFFER_SIZE + SERACH_IMAGE_SIZE*STENCIL_PATCH_COLUMN);


  //-------------------------------------------------





  //--------------input stencil data-----------------
  always_comb begin :  i_stencil_data_block
    i_stencil_data[3] = {t_height_count,t_width_count+COUNT_WIDTH'('d0),fisrt_data[15:8]};
    i_stencil_data[2] = {t_height_count,t_width_count+COUNT_WIDTH'('d1),fisrt_data[7:0]};
    i_stencil_data[1] = {t_height_count,t_width_count+COUNT_WIDTH'('d2),second_data[15:8]};
    i_stencil_data[0] = {t_height_count,t_width_count+COUNT_WIDTH'('d3),second_data[7:0]};
  end
  //-------------------------------------------------

  
  //--------------generate line buffer---------------
  always_ff @(posedge clk) begin
    if(rst) begin
      line_buffers <= LINE_BUFFER_TOTAL_BITWIDTH'('d0);
    end
    else if(state == STATE_I_TDATA)begin
      for(int i = 0;i<SERACH_IMAGE_SIZE-1;i++) begin
        line_buffers[i][3] <= stencil_reg[i][STENCIL_PATCH_COLUMN-1];
        line_buffers[i][2] <= stencil_reg[i][STENCIL_PATCH_COLUMN-2];
        line_buffers[i][1] <= stencil_reg[i][STENCIL_PATCH_COLUMN-3];
        line_buffers[i][0] <= stencil_reg[i][STENCIL_PATCH_COLUMN-4];
        for(int j=4;j<LINE_BUFFER_SIZE;j+=4) begin  : shift
          line_buffers[i][j+3] <=  line_buffers[i][j-1];
          line_buffers[i][j+2] <=  line_buffers[i][j-2];
          line_buffers[i][j+1] <=  line_buffers[i][j-3];
          line_buffers[i][j+0] <=  line_buffers[i][j-4];
        end
      end
    end
    else begin
      line_buffers <= line_buffers; 
    end
  end


  //--------------------------------------------------


  //-----------------stencil patch--------------------
  genvar i;
  generate  
    for(i = 0;i<SERACH_IMAGE_SIZE;i++) begin : stencil_reg_gen
      if(i == 0) begin
        always_ff @(posedge clk) begin
          if(rst) begin
            stencil_reg[i] <= (STENCIL_PATCH_COLUMN*INPUT_STENCIL_WIDTH)'('d0);
          end
          else if(state == STATE_I_TDATA) begin
            stencil_reg[i][3] <= i_stencil_data[3];
            stencil_reg[i][2] <= i_stencil_data[2];
            stencil_reg[i][1] <= i_stencil_data[1];
            stencil_reg[i][0] <= i_stencil_data[0];
            for(int j = 4;j<STENCIL_PATCH_COLUMN;j=j+4) begin
              stencil_reg[i][j+3] <= stencil_reg[i][j-1];
              stencil_reg[i][j+2] <= stencil_reg[i][j-2];
              stencil_reg[i][j+1] <= stencil_reg[i][j-3];
              stencil_reg[i][j+0] <= stencil_reg[i][j-4];
            end
          end
        end
      end
      else begin
        always_ff @(posedge clk) begin
          if(rst) begin
            stencil_reg[i] <= (STENCIL_PATCH_COLUMN*INPUT_STENCIL_WIDTH)'('d0);
          end
          else if(state == STATE_I_TDATA) begin
            stencil_reg[i][3] <= line_buffers[i-1][LINE_BUFFER_SIZE-1];
            stencil_reg[i][2] <= line_buffers[i-1][LINE_BUFFER_SIZE-2];
            stencil_reg[i][1] <= line_buffers[i-1][LINE_BUFFER_SIZE-3];
            stencil_reg[i][0] <= line_buffers[i-1][LINE_BUFFER_SIZE-4];  
            for(int j = 4;j<STENCIL_PATCH_COLUMN;j=j+4) begin
              stencil_reg[i][j+3] <= stencil_reg[i][j-1];
              stencil_reg[i][j+2] <= stencil_reg[i][j-2];
              stencil_reg[i][j+1] <= stencil_reg[i][j-3];
              stencil_reg[i][j+0] <= stencil_reg[i][j-4];
            end
          end
        end

      end
    end
  endgenerate
  //--------------------------------------------------

  //----------------stencil patches-------------------
  always_comb begin
    for(int i = 0;i<5;i++) begin
      for(int j = 0;j<SERACH_IMAGE_SIZE;j++) begin
        for(int k = 0;k<SERACH_IMAGE_SIZE;k++) begin
            stencil_patches[i][j*SERACH_IMAGE_SIZE+k] = stencil_reg[j][(k+(STENCIL_PATCH_COLUMN-16-i))][7:0];  
        end
      end
    end
  end

  //--------------------------------------------------

  //------------------coordinates---------------------
  always_comb begin
    coordinates[0] = stencil_reg[SERACH_IMAGE_SIZE-1][STENCIL_PATCH_COLUMN-1][INPUT_STENCIL_WIDTH-1: 8];
    coordinates[1] = stencil_reg[SERACH_IMAGE_SIZE-1][STENCIL_PATCH_COLUMN-2][INPUT_STENCIL_WIDTH-1: 8];
    coordinates[2] = stencil_reg[SERACH_IMAGE_SIZE-1][STENCIL_PATCH_COLUMN-3][INPUT_STENCIL_WIDTH-1: 8];
    coordinates[3] = stencil_reg[SERACH_IMAGE_SIZE-1][STENCIL_PATCH_COLUMN-4][INPUT_STENCIL_WIDTH-1: 8];
    coordinates[4] = stencil_reg[SERACH_IMAGE_SIZE-1][STENCIL_PATCH_COLUMN-5][INPUT_STENCIL_WIDTH-1: 8];
  end
  //--------------------------------------------------
  //-----------------calc eval_en---------------------
  always_comb begin
    //if(calc_start_flag & t_width_count<=COUNT_WIDTH'('d112) & t_height_count<=COUNT_WIDTH'('d112) ) begin
    if (calc_start_flag & ((coordinates[0][COUNT_WIDTH+COUNT_WIDTH-1 -: COUNT_WIDTH])<=(COUNT_WIDTH)'('d112)) ) begin
      if(coordinates[0][COUNT_WIDTH-1 -: COUNT_WIDTH]<(COUNT_WIDTH)'('d112)) begin
        calc_eval_en[3:0] = 4'b1111;
      end
      else begin
        calc_eval_en[3:0] = 4'b0000;
      end
    end
    else begin
      calc_eval_en[3:0] = 4'b0000;
    end
  end

  always_comb begin
    calc_eval_en[4] = (state == STATE_I_TDATA) & calc_start_flag & (coordinates[0][COUNT_WIDTH-1 -: COUNT_WIDTH] == COUNT_WIDTH'('d108));
  end
  //--------------------------------------------------
  
  genvar gi;
  generate   
    for(gi= 0;gi<5;gi++) begin  : calc_eval_gen
      calc_eval#(
        .SERACH_IMAGE_SIZE(SERACH_IMAGE_SIZE),
        .SERACH_IMAGE_PNUM(SERACH_IMAGE_PNUM),
        .COORDI_BITWIDTH(COUNT_WIDTH+COUNT_WIDTH),
        .BIT_WIDTH(8)
      )
      calc_eval_inst(
        .clk(clk),
        .rst(rst),
        .coordinates(coordinates[gi]),
        .tdata(stencil_patches[gi]),
        .sdata(serach_image),
        .calc_en(calc_eval_en[gi]),
        .o_data(eval_data[gi]),
        .o_valid(eval_data_valid[gi]),
        .o_coordinates(eval_coordinates[gi])
      );
    end

  endgenerate
  //--------------------------------------------------



  always_ff @(posedge clk) begin
    if(rst) begin
      max_calc_reg <= (3*3*16)'('d0);
    end
    else begin
      max_calc_reg[0][0] <= (eval_data_valid[0]) ?((eval_data[0] >= eval_data[1])? eval_data[0] : eval_data[1]) :16'd0;
      max_calc_reg[0][1] <= (eval_data_valid[1]) ?((eval_data[2] >= eval_data[3])? eval_data[2] : eval_data[3]) :16'd0;
      max_calc_reg[0][2] <= (eval_data_valid[4]) ? eval_data[4]: 16'd0;
      max_calc_reg[1][0] <= (max_calc_reg[0][0] >= max_calc_reg[0][1]) ? max_calc_reg[0][0] : max_calc_reg[0][1];
      max_calc_reg[1][1] <= max_calc_reg[0][2];
      max_calc_reg[1][2] <= 16'd0;
      max_calc_reg[2][0] <= (max_calc_reg[1][0] >= max_calc_reg[1][1]) ? max_calc_reg[1][0] : max_calc_reg[1][1];
      max_calc_reg[2][1] <= 16'd0;
      max_calc_reg[2][2] <= 16'd0;
    end
  end

  always_ff @(posedge clk) begin
    if(rst) begin
      max_coordinates_reg <= (3*3*16)'('d0);
    end
    else begin
      max_coordinates_reg[0][0] <= (eval_data_valid[0]) ?((eval_data[0] >= eval_data[1])? eval_coordinates[0] : eval_coordinates[1]) :16'd0;
      max_coordinates_reg[0][1] <= (eval_data_valid[1]) ?((eval_data[2] >= eval_data[3])? eval_coordinates[2] : eval_coordinates[3]) :16'd0;
      max_coordinates_reg[0][2] <= (eval_data_valid[4]) ? eval_coordinates[4]: 16'd0;
      max_coordinates_reg[1][0] <= (max_calc_reg[0][0] >= max_calc_reg[0][1]) ? max_coordinates_reg[0][0] : max_coordinates_reg[0][1];
      max_coordinates_reg[1][1] <= max_coordinates_reg[0][2];
      max_coordinates_reg[1][2] <= 16'd0;
      max_coordinates_reg[2][0] <= (max_calc_reg[1][0] >= max_calc_reg[1][1]) ? max_coordinates_reg[1][0] : max_coordinates_reg[1][1];
      max_coordinates_reg[2][1] <= 16'd0;
      max_coordinates_reg[2][2] <= 16'd0;
    end
  end

  always_ff @(posedge clk) begin
    if(rst) begin
      max_eval_value <= 16'd0;
    end
    else if(max_eval_value <= max_calc_reg[2][0]) begin
      max_eval_value <= max_calc_reg[2][0];
    end
    else begin
      max_eval_value <= max_eval_value;
    end
  end

  always_ff @(posedge clk) begin
    if(rst) begin
      max_coordinates <= 16'd0;
    end
    else if(max_eval_value <= max_calc_reg[2][0]) begin
      max_coordinates <= max_coordinates_reg[2][0];
    end
    else begin
      max_coordinates <= max_coordinates;
    end
  end




//######################################################
//######################################################

endmodule










module calc_eval#(
  parameter int SERACH_IMAGE_SIZE = -1,
  parameter int SERACH_IMAGE_PNUM = -1,
  parameter int COORDI_BITWIDTH = -1,
  parameter int BIT_WIDTH = -1
)
(
  input wire clk,
  input wire rst,
  input wire [SERACH_IMAGE_PNUM-1:0][BIT_WIDTH-1:0] tdata,
  input wire [SERACH_IMAGE_PNUM-1:0][BIT_WIDTH-1:0] sdata,
  input wire [COORDI_BITWIDTH-1:0]                   coordinates,
  input wire                                        calc_en,
  output logic [15:0]     o_data,
  output logic            o_valid,
  output logic [COORDI_BITWIDTH-1:0]                 o_coordinates
);


  localparam int  TREE_DEPTH = $clog2(SERACH_IMAGE_PNUM)+1;         //Addtreeのdelay$clog2(SERACH_IMAGE_PNUM)+1  +  calc_evalのdelay1
  localparam ADD_WIDTH = (BIT_WIDTH+1) + $clog2(SERACH_IMAGE_PNUM);

  logic  [2:0]                                            calc_en_reg;
  //logic  [SERACH_IMAGE_PNUM-1:0][BIT_WIDTH:0]             sub_reg;
  //logic  [SERACH_IMAGE_PNUM-1:0][BIT_WIDTH:0]             abs_reg;
  logic  [SERACH_IMAGE_PNUM-1:0][BIT_WIDTH:0]             sub255_reg;
  logic  [TREE_DEPTH-1:0][COORDI_BITWIDTH-1:0]            coordinates_reg;
  logic  [ADD_WIDTH-1:0]  add_data;


  always_ff @( posedge clk ) begin : blockName
    if(rst) begin
      calc_en_reg <= 3'b0;
    end
    else begin
      calc_en_reg <= {calc_en_reg[1:0],calc_en};
    end
  end

  // always_ff @(posedge clk) begin
  //   if(rst) begin
  //     sub_reg     <= (SERACH_IMAGE_PNUM*(BIT_WIDTH+1))'('d0);      
  //   end
  //   else begin
  //     for(int i = 0;i<(SERACH_IMAGE_PNUM);i++) begin      
  //       sub_reg[i]  <= {1'b0 , sdata[i]} - {1'b0 , tdata[i]};;
  //     end
  //   end
  // end
  
  // always_comb begin 
  // for(int i = 0;i<(SERACH_IMAGE_PNUM);i++) begin      
  //   sub_reg[i]  <= {1'b0 , sdata[i]} - {1'b0 , tdata[i]};;
  // end  
  // end

  // always_ff @(posedge clk) begin
  //   if(rst) begin
  //     abs_reg     <= (SERACH_IMAGE_PNUM*(BIT_WIDTH+1))'('d0);      
  //   end
  //   else begin
  //     for(int i = 0;i<(SERACH_IMAGE_PNUM);i++) begin      
  //       if(sub_reg[i][BIT_WIDTH]) begin
  //         abs_reg[i] <= ~sub_reg[i]+(BIT_WIDTH+1)'('b1);
  //       end
  //       else begin
  //         abs_reg[i]  <= sub_reg[i];          
  //       end
  //     end
  //   end
  // end


  always_ff @(posedge clk) begin
    if(rst) begin
      sub255_reg     <= (SERACH_IMAGE_PNUM*(BIT_WIDTH+1))'('d0);
    end    
    else begin
      for(int i = 0;i<(SERACH_IMAGE_PNUM);i++) begin      
        //sub255_reg[i]  <= (BIT_WIDTH+1)'('d255) - abs_reg[i];
        sub255_reg[i] <= (BIT_WIDTH+1)'('d255) - abs(tdata[i],sdata[i]);
      end
    end
  end

  adder_tree#(
		.ADD_NUM(SERACH_IMAGE_PNUM),
		.BIT_WIDTH(BIT_WIDTH+1)
  )
  adder_tree_inst(
    .clk(clk),
    .n_rst(~rst),
    .add_data(sub255_reg),
    .en(calc_en_reg[2]),
    .dout(add_data),
    .d_valid(o_valid)
  );


  assign o_data = 16'(add_data >> 1);

  always_ff@(posedge clk) begin
    if(rst) begin
      coordinates_reg <= (COORDI_BITWIDTH*TREE_DEPTH)'('d0);
    end
    else begin
      coordinates_reg[0] <= coordinates;
      for(int i = 1;i<TREE_DEPTH;i++) begin
        coordinates_reg[i] <= coordinates_reg[i-1];
      end
    end
  end

  assign o_coordinates = coordinates_reg[TREE_DEPTH-1];

  function [BIT_WIDTH:0] abs(
    input [BIT_WIDTH-1:0] tdata ,
    input [BIT_WIDTH-1:0] sdata
  );
    logic [BIT_WIDTH:0] tmp;   
    logic [BIT_WIDTH:0] tmp2;
  
    tmp = {1'b0 , sdata} - {1'b0 , tdata};
  


    if (tmp[BIT_WIDTH]) begin
      tmp2 =  ~(tmp-BIT_WIDTH'('d1));
    end
    else begin
      tmp2 = tmp;
    end

    abs = tmp2;
  
  endfunction

endmodule

module adder_tree#(
parameter integer ADD_NUM = 256,   //only 256
parameter integer BIT_WIDTH = 8,
parameter integer TREE_MAX_WIDTH = 2 ** $clog2(ADD_NUM),
parameter integer TREE_DEPTH = $clog2(ADD_NUM),
parameter integer O_BIT_WIDTH = BIT_WIDTH + TREE_DEPTH 
)
  (
    input wire				    clk,
    input wire				    n_rst,
    input wire [ADD_NUM-1:0][BIT_WIDTH-1:0] add_data,
    input wire            en,
    output wire [O_BIT_WIDTH-1:0]	    dout,
    output wire                       d_valid
  );

    logic [7:0]   en_reg;
    
    always_ff @(posedge clk) begin
      if(!n_rst) begin
        en_reg <= 4'd0;
      end
      else begin
        en_reg[0] <= en;
        for(int i = 1;i<8;i++) begin
          en_reg[i] <= en_reg[i-1];
        end
      end
    end


    logic [127:0][BIT_WIDTH+0:0] tree_1;
    logic [63:0][BIT_WIDTH+1:0]  tree_2;
    logic [31:0][BIT_WIDTH+2:0]  tree_3;
    logic [15:0][BIT_WIDTH+3:0]  tree_4;
    logic [7:0][BIT_WIDTH+4:0]   tree_5;
    logic [3:0][BIT_WIDTH+5:0]   tree_6;        
    logic [1:0][BIT_WIDTH+6:0]   tree_7;
    logic      [BIT_WIDTH+7:0]   tree_8;

    always_ff @(posedge clk ) begin : blockName
      if(!n_rst) begin
        tree_1 <= (128*BIT_WIDTH+1)'('d0);
        tree_2 <= (64*BIT_WIDTH+2)'('d0);
        tree_3 <= (32*BIT_WIDTH+3)'('d0);
        tree_4 <= (16*BIT_WIDTH+3)'('d0);
        tree_5 <= (8*BIT_WIDTH+4)'('d0);
        tree_6 <= (4*BIT_WIDTH+5)'('d0);
        tree_7 <= (2*BIT_WIDTH+6)'('d0);
        tree_8 <= (1*BIT_WIDTH+7)'('d0);
      end  
      else begin
        for(int i = 0;i<128;i++) begin
          tree_1[i] <= add_data[i*2] + add_data[i*2+1];
        end
        for(int i = 0;i<64;i++) begin
          tree_2[i] <= tree_1[i*2] + tree_1[i*2+1];
        end
        for(int i = 0;i<32;i++) begin
          tree_3[i] <= tree_2[i*2] + tree_2[i*2+1];
        end
        for(int i = 0;i<16;i++) begin
          tree_4[i] <= tree_3[i*2] + tree_3[i*2+1];
        end
        for(int i = 0;i<8;i++) begin
          tree_5[i] <= tree_4[i*2] + tree_4[i*2+1];
        end
        for(int i = 0;i<4;i++) begin
          tree_6[i] <= tree_5[i*2] + tree_5[i*2+1];
        end
        for(int i = 0;i<2;i++) begin
          tree_7[i] <= tree_6[i*2] + tree_6[i*2+1];
        end
        tree_8 = tree_7[0] + tree_7[1];
      end
    end
    
    assign dout    = tree_8;
    assign d_valid = en_reg[TREE_DEPTH-1];

  // logic [TREE_DEPTH-1:0]   en_reg;
  // logic [TREE_DEPTH:0][TREE_MAX_WIDTH-1:0][O_BIT_WIDTH-1:0] tree;

  //   genvar						     depth,i;
  //   generate
  //     for(depth=0;depth<=TREE_DEPTH;depth=depth+1) begin  :tree_gen
  //       localparam integer WIDTH_DEPTH = TREE_MAX_WIDTH >> depth;
  //       localparam integer DEPTH_BIT_WIDTH = BIT_WIDTH + depth;

  //       if(depth == 0) begin
  //         for(i=0;i<WIDTH_DEPTH;i=i+1) begin  :depth_0_gen

  //           if(i < ADD_NUM) begin
  //             // always_ff @(posedge clk) begin : blockName
  //             //   if(!n_rst) begin
  //             //     tree[depth][i] <= O_BIT_WIDTH'('d0);
  //             //   end
  //             always_comb begin
  //                 tree[depth][i][DEPTH_BIT_WIDTH-1:0] <= add_data[i];
  //                 tree[depth][i][O_BIT_WIDTH-1:DEPTH_BIT_WIDTH] <= '0;                    
  //             end
  //           end

  //           else begin
  //               always_comb begin
  //                 tree[depth][i] <= O_BIT_WIDTH'('d0);
  //               end
  //           end

  //         end
	//      end

	//      else begin
	//        for(i=0;i<TREE_MAX_WIDTH;i=i+1) begin  : depth_i_gen
  //           if(i<WIDTH_DEPTH) begin

  //             always_ff @(posedge clk) begin
  //               if(!n_rst) begin
  //                 tree[depth][i] <= O_BIT_WIDTH'('d0);
  //               end
  //               else begin
  //                 tree[depth][i]
  //                       <= tree[depth-1][i*2] +
  //                         tree[depth-1][i*2+1];
  //               end
  //             end
             
  //           end

  //           else begin
  //             always_ff @(posedge clk) begin
  //               if(!n_rst) begin
  //                 tree[depth][i] <= O_BIT_WIDTH'('d0);
  //               end
  //               else begin
  //                 tree[depth][i] <= O_BIT_WIDTH'('d0);
  //               end
  //             end                  
  //           end

	//        end
	//      end // else: !if(depth == 0)

  //     end // for (depth=0;depth<TREE_DEPTH;depth=depth+1)
  // endgenerate

  // always_ff @(posedge clk) begin
  //   if(!n_rst) begin
  //     en_reg <= (TREE_DEPTH+1)'('d0);
  //   end
  //   else begin
  //     en_reg[0] <= en;
  //     for(int i = 1;i<TREE_DEPTH;i++) begin
  //       en_reg[i] <= en_reg[i-1];
  //     end
  //   end
  // end

  // assign dout    = tree[TREE_DEPTH][0];
  // assign d_valid = en_reg[TREE_DEPTH-1];


endmodule   
		       
`default_nettype wire
