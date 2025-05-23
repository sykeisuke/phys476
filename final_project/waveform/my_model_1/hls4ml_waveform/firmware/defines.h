#ifndef DEFINES_H_
#define DEFINES_H_

#include "ap_fixed.h"
#include "ap_int.h"
#include "nnet_utils/nnet_types.h"
#include <cstddef>
#include <cstdio>

// hls-fpga-machine-learning insert numbers
#define N_INPUT_1_1 100
#define N_LAYER_2 16
#define N_LAYER_2 16
#define N_LAYER_5 32
#define N_LAYER_5 32
#define N_LAYER_8 32
#define N_LAYER_8 32
#define N_LAYER_11 4

// hls-fpga-machine-learning insert layer-precision
typedef ap_fixed<16,6> input_t;
typedef ap_fixed<16,6> model_default_t;
typedef ap_fixed<16,6> layer2_t;
typedef ap_uint<1> layer2_index;
typedef ap_fixed<16,6> layer4_t;
typedef ap_fixed<18,8> activation_table_t;
typedef ap_fixed<16,6> layer5_t;
typedef ap_uint<1> layer5_index;
typedef ap_fixed<16,6> layer7_t;
typedef ap_fixed<18,8> activation_1_table_t;
typedef ap_fixed<16,6> layer8_t;
typedef ap_uint<1> layer8_index;
typedef ap_fixed<16,6> layer10_t;
typedef ap_fixed<18,8> activation_2_table_t;
typedef ap_fixed<16,6> result_t;
typedef ap_uint<1> layer11_index;

#endif
