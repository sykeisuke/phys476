#include <iostream>

#include "myproject.h"
#include "parameters.h"

void myproject(
    input_t input_3[N_INPUT_1_1*N_INPUT_2_1*N_INPUT_3_1],
    result_t layer12_out[OUT_HEIGHT_12*OUT_WIDTH_12*N_FILT_12]
) {

    // hls-fpga-machine-learning insert IO
    #pragma HLS ARRAY_RESHAPE variable=input_3 complete dim=0
    #pragma HLS ARRAY_PARTITION variable=layer12_out complete dim=0
    #pragma HLS INTERFACE ap_vld port=input_3,layer12_out 
    #pragma HLS DATAFLOW 

#ifndef __SYNTHESIS__
    static bool loaded_weights = false;
    if (!loaded_weights) {
        // hls-fpga-machine-learning insert load weights
        nnet::load_weights_from_txt<model_default_t, 288>(w2, "w2.txt");
        nnet::load_weights_from_txt<model_default_t, 32>(b2, "b2.txt");
        nnet::load_weights_from_txt<model_default_t, 18432>(w4, "w4.txt");
        nnet::load_weights_from_txt<model_default_t, 64>(b4, "b4.txt");
        nnet::load_weights_from_txt<model_default_t, 18432>(w8, "w8.txt");
        nnet::load_weights_from_txt<model_default_t, 32>(b8, "b8.txt");
        nnet::load_weights_from_txt<model_default_t, 96>(w12, "w12.txt");
        nnet::load_weights_from_txt<model_default_t, 3>(b12, "b12.txt");
        loaded_weights = true;
    }
#endif

    // ****************************************
    // NETWORK INSTANTIATION
    // ****************************************

    // hls-fpga-machine-learning insert layers

    layer2_t layer2_out[OUT_HEIGHT_2*OUT_WIDTH_2*N_FILT_2];
    #pragma HLS ARRAY_PARTITION variable=layer2_out complete dim=0
    nnet::conv_2d_cl<input_t, layer2_t, config2>(input_3, layer2_out, w2, b2); // conv2d_8

    layer3_t layer3_out[OUT_HEIGHT_2*OUT_WIDTH_2*N_FILT_2];
    #pragma HLS ARRAY_PARTITION variable=layer3_out complete dim=0
    nnet::relu<layer2_t, layer3_t, relu_config3>(layer2_out, layer3_out); // conv2d_8_relu

    layer4_t layer4_out[OUT_HEIGHT_4*OUT_WIDTH_4*N_FILT_4];
    #pragma HLS ARRAY_PARTITION variable=layer4_out complete dim=0
    nnet::conv_2d_cl<layer3_t, layer4_t, config4>(layer3_out, layer4_out, w4, b4); // conv2d_9

    layer5_t layer5_out[OUT_HEIGHT_4*OUT_WIDTH_4*N_FILT_4];
    #pragma HLS ARRAY_PARTITION variable=layer5_out complete dim=0
    nnet::relu<layer4_t, layer5_t, relu_config5>(layer4_out, layer5_out); // conv2d_9_relu

    layer6_t layer6_out[OUT_HEIGHT_6*OUT_WIDTH_6*N_FILT_6];
    #pragma HLS ARRAY_PARTITION variable=layer6_out complete dim=0
    nnet::pooling2d_cl<layer5_t, layer6_t, config6>(layer5_out, layer6_out); // max_pooling2d_2

    layer7_t layer7_out[OUT_HEIGHT_7*OUT_WIDTH_7*N_CHAN_7];
    #pragma HLS ARRAY_PARTITION variable=layer7_out complete dim=0
    nnet::resize_nearest<layer6_t, config7>(layer6_out, layer7_out); // up_sampling2d_2

    layer8_t layer8_out[OUT_HEIGHT_8*OUT_WIDTH_8*N_FILT_8];
    #pragma HLS ARRAY_PARTITION variable=layer8_out complete dim=0
    nnet::conv_2d_cl<layer7_t, layer8_t, config8>(layer7_out, layer8_out, w8, b8); // conv2d_10

    layer9_t layer9_out[OUT_HEIGHT_8*OUT_WIDTH_8*N_FILT_8];
    #pragma HLS ARRAY_PARTITION variable=layer9_out complete dim=0
    nnet::relu<layer8_t, layer9_t, relu_config9>(layer8_out, layer9_out); // conv2d_10_relu

    nnet::pointwise_conv_2d_cl<layer9_t, result_t, config12>(layer9_out, layer12_out, w12, b12); // conv2d_11

}
