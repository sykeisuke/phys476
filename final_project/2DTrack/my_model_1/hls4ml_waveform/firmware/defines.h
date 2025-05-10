#ifndef DEFINES_H_
#define DEFINES_H_

#include "ap_fixed.h"
#include "ap_int.h"
#include "nnet_utils/nnet_types.h"
#include <cstddef>
#include <cstdio>

// hls-fpga-machine-learning insert numbers
#define N_INPUT_1_1 30
#define N_INPUT_2_1 30
#define N_INPUT_3_1 1
#define OUT_HEIGHT_2 30
#define OUT_WIDTH_2 30
#define N_FILT_2 32
#define OUT_HEIGHT_2 30
#define OUT_WIDTH_2 30
#define N_FILT_2 32
#define OUT_HEIGHT_4 30
#define OUT_WIDTH_4 30
#define N_FILT_4 64
#define OUT_HEIGHT_4 30
#define OUT_WIDTH_4 30
#define N_FILT_4 64
#define OUT_HEIGHT_6 15
#define OUT_WIDTH_6 15
#define N_FILT_6 64
#define OUT_HEIGHT_7 30
#define OUT_WIDTH_7 30
#define N_CHAN_7 64
#define OUT_HEIGHT_8 30
#define OUT_WIDTH_8 30
#define N_FILT_8 32
#define OUT_HEIGHT_8 30
#define OUT_WIDTH_8 30
#define N_FILT_8 32
#define OUT_HEIGHT_12 30
#define OUT_WIDTH_12 30
#define N_FILT_12 3

// hls-fpga-machine-learning insert layer-precision
typedef ap_fixed<16,6> input_t;
typedef ap_fixed<16,6> model_default_t;
typedef ap_fixed<16,6> layer2_t;
typedef ap_fixed<16,6> layer3_t;
typedef ap_fixed<18,8> conv2d_8_relu_table_t;
typedef ap_fixed<16,6> layer4_t;
typedef ap_fixed<16,6> layer5_t;
typedef ap_fixed<18,8> conv2d_9_relu_table_t;
typedef ap_fixed<16,6> layer6_t;
typedef ap_fixed<16,6> layer7_t;
typedef ap_fixed<16,6> layer8_t;
typedef ap_fixed<16,6> layer9_t;
typedef ap_fixed<18,8> conv2d_10_relu_table_t;
typedef ap_fixed<16,6> result_t;

#endif
