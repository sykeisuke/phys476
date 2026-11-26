"""Pruning (tfmot) and QKeras quantization as used in the 2026 notebooks (Chapter 27, backup).

UNTESTED, and it needs a Keras 2 environment (TensorFlow 2.x with tf.keras): the TensorFlow
model-optimization toolkit and QKeras do not work with Keras 3. For new work use HGQ or the
quantization support of hls4ml itself (check the current documentation).
"""
from tensorflow.keras.models import Sequential
from tensorflow.keras.regularizers import l1
from tensorflow_model_optimization.sparsity.keras import (
    ConstantSparsity, UpdatePruningStep, prune_low_magnitude, strip_pruning)
from qkeras.qlayers import QDense, QActivation
from qkeras.quantizers import quantized_bits, quantized_relu


def prune(model):
    """Force 10 % of the weights to zero, starting after 2000 steps, every 100 steps."""
    params = {"pruning_schedule": ConstantSparsity(0.1, begin_step=2000, frequency=100)}
    return prune_low_magnitude(model, **params)   # train with callbacks=[UpdatePruningStep()]
    # after training: model = strip_pruning(model)


def qkeras_model():
    """16-bit weights with 8 integer bits (QKeras counts the sign separately; check docs)."""
    m = Sequential()
    m.add(QDense(16, input_shape=(3,), name="fc1",
                 kernel_quantizer=quantized_bits(16, 8, alpha=1),
                 bias_quantizer=quantized_bits(16, 8, alpha=1),
                 kernel_regularizer=l1(0.0001)))
    m.add(QActivation(activation=quantized_relu(16), name="relu1"))
    return m
