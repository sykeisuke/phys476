"""The four optimizers in Keras (Chapter 24). Runs in the course hls4ml
environment (Keras 2.11); Keras 3 compatible? check.
Same model each time; only the optimizer line changes."""
import keras

sgd = keras.optimizers.SGD(learning_rate=0.01)
momentum = keras.optimizers.SGD(learning_rate=0.01, momentum=0.9)
adagrad = keras.optimizers.Adagrad(learning_rate=0.01)
rmsprop = keras.optimizers.RMSprop(learning_rate=0.001, rho=0.9)
adam = keras.optimizers.Adam(learning_rate=0.001, beta_1=0.9, beta_2=0.999)
