#ifndef DEFINES_H_
#define DEFINES_H_

#include "ap_fixed.h"
#include "ap_int.h"
#include "nnet_utils/nnet_types.h"
#include <cstddef>
#include <cstdio>

// hls-fpga-machine-learning insert numbers
#define N_INPUT_1_1 3
#define N_LAYER_2 16
#define N_LAYER_2 16
#define N_LAYER_5 32
#define N_LAYER_5 32
#define N_LAYER_8 32
#define N_LAYER_8 32
#define N_LAYER_11 1
#define N_LAYER_11 1

// hls-fpga-machine-learning insert layer-precision
typedef ap_fixed<16,6> input_t;
typedef ap_fixed<16,6> model_default_t;
typedef ap_fixed<16,6> layer2_t;
typedef ap_fixed<16,9> weight2_t;
typedef ap_fixed<16,9> bias2_t;
typedef ap_uint<1> layer2_index;
typedef ap_ufixed<16,0,AP_RND_CONV,AP_SAT> layer4_t;
typedef ap_fixed<18,8> relu1_table_t;
typedef ap_fixed<16,6> layer5_t;
typedef ap_fixed<16,9> weight5_t;
typedef ap_fixed<16,9> bias5_t;
typedef ap_uint<1> layer5_index;
typedef ap_ufixed<16,0,AP_RND_CONV,AP_SAT> layer7_t;
typedef ap_fixed<18,8> relu2_table_t;
typedef ap_fixed<16,6> layer8_t;
typedef ap_fixed<16,9> weight8_t;
typedef ap_fixed<16,9> bias8_t;
typedef ap_uint<1> layer8_index;
typedef ap_ufixed<16,0,AP_RND_CONV,AP_SAT> layer10_t;
typedef ap_fixed<18,8> relu3_table_t;
typedef ap_fixed<16,6> layer11_t;
typedef ap_fixed<18,9> weight11_t;
typedef ap_fixed<18,9> bias11_t;
typedef ap_uint<1> layer11_index;
typedef ap_fixed<16,6> sigmoid_exp_table_t;
typedef ap_fixed<16,6> sigmoid_inv_table_t;
typedef ap_fixed<16,6> result_t;
typedef ap_fixed<18,8> sigmoid_table_t;

#endif
